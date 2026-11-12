# Author: Daniele Buono
# 
# d.buono@bio.lmu.de
# 
# LMU Munich


if (!requireNamespace("tools")){
	install.packages("tools", dependencies = T)
}
library(tools) # to extract file names

if (!requireNamespace("googleCloudVisionR")){
	install.packages("googleCloudVisionR", dependencies = T)
}
library(googleCloudVisionR) # for google OCR

if (!requireNamespace("googleAnalyticsR")){
	install.packages("googleAnalyticsR", dependencies = T)
}
library(googleAnalyticsR)
if (!requireNamespace("googleAuthR")){
	install.packages("googleAuthR", dependencies = T)
}
library(googleAuthR) # to autenticate with non-interactive script

if (!requireNamespace("stringr")){
	install.packages("stringr", dependencies = T)
}
library(stringr) 
if (!requireNamespace("httr")){
	install.packages("httr", dependencies = T)
}
library(httr) # to send prompts to chatGPT

if (!requireNamespace("jsonlite")){
	install.packages("jsonlite", dependencies = T)
}
library(jsonlite) # to extract info from JSON objects from chatGPT replies

if (!requireNamespace("plyr")){
	install.packages("plyr", dependencies = T)
}
library(plyr) # to compose table with chatGPT final transcriptions

if (!requireNamespace("parallel")){
	install.packages("parallel", dependencies = T)
}
library(parallel) # to parallelise compression images
if (!requireNamespace("base64enc")){
	install.packages("base64enc", dependencies = T)
}
library(base64enc) # to convert images for chatGPT

WAITING_TIME_GPT <- 4 # seconds to wait between two chatGPT calls to avoid empty answers in chatGPT v4 

# 0. resize input images when very big ####
# functions ----
resize_imgs <- function(img_folder, list_jpg_files, dir_img_resized, max_img_size, quality, quiet = F, cores = 1){
  resized_imgs <- c()
  n_img = 1
  #list_jpg_files <- list.files(img_folder, pattern = "\\.(jpg|jpeg|JPG|JPEG)")
  n_tot = length(list_jpg_files)
  if(cores == 1){
    for (file_i in list_jpg_files){
      file_i_info <- file.info(paste0(img_folder, file_i))
      cat(paste0("\n\t[",n_img,"/",n_tot,"] ",paste0("size of image ", file_i," = ",file_i_info$size/1e+06," Mb. ")))
      if (file_i_info$size > max_img_size*1e+06){
        
        if (!quiet) cat(paste0("Reducing to target ",max_img_size," Mb..."))
        system(paste0("convert ", img_folder, file_i, " -define jpeg:extent=", max_img_size, "MB ", dir_img_resized, basename(file_i)))
        file_dest <- file.info(paste0(dir_img_resized, basename(file_i)))
        cat (paste0(" DONE. New size = ",file_dest$size/1e+06," Mb"))
        
      } else {
        if (!quiet) cat(paste0("We will use original image."))
        file.copy(paste0(img_folder, file_i), dir_img_resized)
      }
      n_img = n_img + 1
      resized_imgs <- c(resized_imgs, paste0(dir_img_resized,basename(file_i)))
    }
  } else { # compress in parallel
    
    file_infos <- file.info(paste0(img_folder,list_jpg_files))
    large_files <- file_infos[file_infos$size > max_img_size*1e+06,]
    compress = function(file_name){
      system(paste0("convert ", file_name, " -define jpeg:extent=", max_img_size,"MB ", dir_img_resized, basename(file_name)))
    }
    res <- mclapply(rownames(large_files), compress, mc.cores = cores)
    
    small_files <- file_infos[file_infos$size <= max_img_size*1e+06,]
    res <- file.copy(rownames(small_files), dir_img_resized)
    resized_imgs <- paste0(dir_img_resized, basename(rownames(file_infos)))
  }
  
  return(resized_imgs)
}


## 0. crop rulers from images ####
# requires linux installation of imagemagick

crop_ruler_from_imgs <- function (exp_path, path_images, list_images){
  
  path_images_cropped <- paste0(exp_path,"0_ruler_cropped/")
  dir.create(path_images_cropped, showWarnings = FALSE)
  
  #list_images <- list.files(path = path_images, pattern = "*.jpg")
  height_ruler = 480 # measured by cropping manually a picture
  # copy all pictures in destination folder, they will be overwritten
  for (image_i in list_images){
    system(paste0("cp ", path_images, image_i," ", path_images_cropped))
  }
  #setwd(path_images_cropped)
  # see https://imagemagick.org/script/mogrify.php
  for (image_i in list.files(path = path_images_cropped, pattern = "*.jpg")){
    cat("\n image_i = ", image_i)
    width_i <- as.numeric(system(paste0("identify -format '%w' ",path_images_cropped, image_i), intern = T))
    height_i <- as.numeric(system(paste0("identify -format '%h' ",path_images_cropped, image_i), intern = T))
    system(paste0("mogrify -crop ",width_i,"x",height_i-height_ruler,"+1+1 ",path_images_cropped, image_i))
  }
  return(path_images_cropped)
}


## run yolo ----
run_yolo <- function(path_labels, weights, path_images, project_name, conf_thres = 0.7){
  system(paste0("python3 yolov7/detect.py --weights ", weights, " --source ", path_images,
                " --save-txt --save-conf --project ", exp_path, " --nosave",# do not save images
                " --name ", project_name, " --exist-ok ",
                "--no-trace",
                " --conf-thres ", conf_thres))
  # moving labels one folder up and deleting empty folder
  system(paste0("mv ", path_labels,"labels/*.txt ",path_labels))
  system(paste0("rm -r ",path_labels,"labels"))
}

check_multiple_picts <- function (dir_imgs, dir_out){
  # looks for pictures that belongs to multiple barcodes, eg M-0000001_a M-0000001_b
  barcides_with_multi_picts <- c()
  image_files <- list.files(dir_imgs, pattern = "*.jpg", recursive = F)
  barcodes <- sub("_.*", "", image_files)
  barcode_counts <- table(barcodes)
  # find barcodes that appear more than once
  dup_barcodes <- names(barcode_counts[barcode_counts > 1])
  # if none found, return empty
  if (length(dup_barcodes) == 0) {
    return(character(0))
  }
  # collapse into one regex pattern
  pattern <- paste0("^(", paste(dup_barcodes, collapse = "|"), ")")
  # get matching files
  duplicate_files <- image_files[grepl(pattern, image_files)]
  
  file_txt_out <- paste0(dir_out,"barcodes_with_multiple_pictures.txt")
  fileConn <- file(file_txt_out)
  writeLines(paste0(duplicate_files, collapse = "\n"), fileConn)
  close(fileConn)
  
  return(duplicate_files)
}

check_multiple_barcodes <- function (dir_imgs, dir_labels, dir_out, class_names){
  
  imgs_with_multi_barcode <- c()
  images_in_folder <- list.files(dir_imgs, pattern = "*.jpg", recursive = F)
  classes = readLines(class_names)
  for (image_i in images_in_folder) {
    # read .txt file in folder label
    label_lines <- readLines(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))
    img_i_barcodes = 0
    for (line_i in label_lines){
      values <- strsplit(line_i, " ")[[1]]
      if(classes[as.numeric(values[1])+1] == "Barcode"){
        img_i_barcodes = img_i_barcodes + 1
      }
    }
    if(img_i_barcodes > 1) {
      imgs_with_multi_barcode <- c(imgs_with_multi_barcode, image_i)
      file_txt_out <- paste0(dir_out,"pictures_with_multiple_barcodes.txt")
      fileConn <- file(file_txt_out)
      writeLines(paste0(imgs_with_multi_barcode, collapse = "\n"), fileConn)
      close(fileConn)
    }
  }
  return(imgs_with_multi_barcode)
}

check_missing_labels <- function (dir_imgs, dir_labels, dir_out, class_names){
  
  imgs_with_no_labels <- c()
  images_in_folder <- list.files(dir_imgs, pattern = "\\.(jpg|jpeg|JPG|JPEG)", recursive = F)
  classes = readLines(class_names)
  for (image_i in images_in_folder) {
    # read .txt file in folder label
    label_lines <- readLines(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))
    img_i_text_labels = F
    for (line_i in label_lines){ #check each line of labeled object for their class, verify if it is "Text_label"
      values <- strsplit(line_i, " ")[[1]]
      if(classes[as.numeric(values[1])+1] == "Text_label"){
        img_i_text_labels = T
        break # stop checking as soon as one text label is found
      }
    }
    if(!img_i_text_labels) {
      imgs_with_no_labels <- c(imgs_with_no_labels, image_i)
      file_txt_out <- paste0(dir_out,"pictures_with_no_text_label.txt")
      fileConn <- file(file_txt_out)
      writeLines(paste0(imgs_with_no_labels, collapse = "\n"), fileConn)
      close(fileConn)
    }
  }
  return(imgs_with_no_labels)
}


## crop text labels from images recognized by yolov7 ----
crop_img <- function(image, xcenter, ycenter, width, height, img_out_label, dir_cropped, img_nr) {
  # yolo squares:
  # class xcenter ycenter width height 
  # e.g.
  # 1 0.884604 0.0460538 0.12119 0.0455672
  
  width_img <- as.numeric(system(paste0("identify -format '%w' ", image), intern = T))
  height_img <- as.numeric(system(paste0("identify -format '%h' ", image), intern = T))
  file_out <- paste0(dir_cropped, sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image)),"_",img_nr,"_",img_out_label,".", file_ext(image))
  
  w = as.integer(width_img*width)
  h = as.integer(height_img*height)
  x = as.integer((xcenter*width_img)-(width*width_img)/2)
  y = as.integer((ycenter*height_img)-(height*height_img)/2)
  
  system(paste0("convert ", image," -crop ", w,"x", h, "+", x, "+", y, " ", file_out))
  return(file_out)
}

crop_imgs <- function (image_folder, dir_cropped, dir_labels, only_lab="F", class_names, cores = 1){
  images_in_folder <- list.files(image_folder, pattern = "\\.(jpg|jpeg|JPG|JPEG)", recursive = F)
  classes = readLines(class_names)
  labels_in_folder <- list.files(dir_labels, pattern = ".txt", recursive = F)
  if (identical(labels_in_folder, character(0))) {
    cat("\nthere seems to be no label detection files!\n")
    return(0)
  }
  dir.create(file.path(dir_cropped), showWarnings = FALSE)
  if (cores == 1){
    for (image_i in images_in_folder) {
      # read .txt file in folder label
      if (!file.exists(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))) {
        cat (paste0("\nlabel file for ",basename(image_i)," does not exists! (probably no object was identified)"))
        break
      }
      cat(paste0("\n    cropping ", image_i,":"))
      label_lines <- readLines(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))
      img_i_nr = 0
      for (line_i in label_lines){
        values <- strsplit(line_i, " ")[[1]]
        img_i_nr = img_i_nr + 1
        if(only_lab == "F") { # crop every label if only_lab is not "T"
          cropped_img <- crop_img(image = paste0(image_folder,image_i),  
                                  xcenter = as.numeric(values[2]), 
                                  ycenter = as.numeric(values[3]), 
                                  width = as.numeric(values[4]), 
                                  height = as.numeric(values[5]), 
                                  img_out_label = classes[as.numeric(values[1])+1], 
                                  img_nr = img_i_nr, 
                                  dir_cropped = dir_cropped)
          cat(paste0("\n\t", cropped_img))
        } else if (values[1] == "0"){ # if only_lab == "T", then check that class object is 0 (corresponds to text_label)
          cropped_img <- crop_img(image = paste0(image_folder, image_i),  
                                  xcenter = as.numeric(values[2]), 
                                  ycenter = as.numeric(values[3]), 
                                  width = as.numeric(values[4]), 
                                  height = as.numeric(values[5]), 
                                  img_out_label = classes[as.numeric(values[1])+1], 
                                  img_nr = img_i_nr, 
                                  dir_cropped = dir_cropped)
          cat(paste0("\n\t", cropped_img))
        }
      }
    }
  } else { #multicore
    crop_imgs_multi = function(image_i){
      # read .txt file in folder label
      if (! file.exists(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))) {
        cat (paste0("\nlabel file for ",basename(image_i)," does not exists! (probably no object was identified)"))
        return(0)
      } else {
        label_lines <- readLines(paste0(dir_labels,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(image_i)),".txt"))
        img_i_nr = 0
        for (line_i in label_lines){
          values <- strsplit(line_i, " ")[[1]]
          img_i_nr = img_i_nr + 1
          if(only_lab == "F") { # crop every label if only_lab is not "T"
            cropped_img <- crop_img(image = paste0(image_folder,image_i),  
                                    xcenter = as.numeric(values[2]), 
                                    ycenter = as.numeric(values[3]), 
                                    width = as.numeric(values[4]), 
                                    height = as.numeric(values[5]), 
                                    img_out_label = classes[as.numeric(values[1])+1], 
                                    img_nr = img_i_nr, 
                                    dir_cropped = dir_cropped)
          } else if (values[1] == "0"){ # if only_lab == "T", then check that class object is 0 (corresponds to text_label)
            cropped_img <- crop_img(image = paste0(image_folder,image_i),  
                                    xcenter = as.numeric(values[2]), 
                                    ycenter = as.numeric(values[3]), 
                                    width = as.numeric(values[4]), 
                                    height = as.numeric(values[5]), 
                                    img_out_label = classes[as.numeric(values[1])+1], 
                                    img_nr = img_i_nr, 
                                    dir_cropped = dir_cropped)
          }
        }
      }
    }
    res <- mclapply(images_in_folder, crop_imgs_multi, mc.cores = cores)
  }
}                  

# paste all label images in one collage to use directly in chatGPT v 4o ----
create_label_collages <- function (label_path, exp_path, collages_path, resize=T, res_width=800){
  single_label_imgs = c()
  
  list_label_images <- list.files(path = label_path, pattern = "\\.(jpg|jpeg|JPG|JPEG)")
  barcodes <- unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(list_label_images))))
  for (barcode_i in barcodes){
    # create a single collage per barcode_i
    filenames_barcode_i <- list_label_images[grepl(barcode_i, list_label_images)]
    if(length(filenames_barcode_i)==1) single_label_imgs <- c(single_label_imgs,filenames_barcode_i)
    file_collage_i <- paste0(barcode_i,"_labels.jpg")
    label_files <- paste0(label_path,filenames_barcode_i, collapse = " ")
    # create collage, first try one column, if it fails (can happen with many labels) it try with two and three columns

    if(system(paste0("montage ", label_files," -tile 1x",length(filenames_barcode_i)," -geometry +1+1 ",collages_path,file_collage_i))!=0){
      if(system(paste0("montage ", label_files," -tile 2x", length(filenames_barcode_i), " -geometry +1+1 ", collages_path, file_collage_i))!=0){
        system(paste0("montage ", label_files," -tile 3x", length(filenames_barcode_i), " -geometry +1+1 ", collages_path, file_collage_i))
      }
    }
    if (resize) system(paste0("mogrify -resize ",res_width,"x ",collages_path,file_collage_i))
  }
  

  
  return(single_label_imgs)
}

## transcribe labels with google OCR ----
set_google_ocr_credentials <- function(google_credentials){
  Sys.setenv("GOOGLE_APPLICATION_CREDENTIALS" = google_credentials)
  gar_auth(email = fromJSON(google_credentials)$client_email)
}

transcribe_labels_googleOCR <- function(path_imgs, path_out, quiet = F, feature = "DOCUMENT_TEXT_DETECTION", use_format_txt = F){
  
  # look only for "label" images
  img_paths = list.files(path_imgs, pattern = "*_Text_label.jpg", full.names = T)
  dir.create(paste0(path_out,"responses"), showWarnings = F)
  for (img_i in img_paths){
    if(use_format_txt){
      # obtain formatted text (new lines, spaces)
      gcv_response <- gcv_get_raw_response(imagePaths = img_i, feature = feature)
      text_i <- gcv_response$content$responses$textAnnotations[[1]]$description[1]
      if(is.null(text_i)) text_i = "" #formatted text answer can give NULL reply when label has no text
    } else {
      # obtain unformatted text (no new lines, space between each word)
      gcv_response <- gcv_get_image_annotations(imagePaths = img_i, feature = feature)
      text_i <- paste(gcv_response$description, collapse = " ")
      # save complete answer from gcv
      file_resp_out <- paste0(path_out,"responses/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(img_i)),"_response.txt")
      write.csv(as.matrix(gcv_response), file_resp_out)
    }
    
    if (!quiet) cat(paste0("\n\t",basename(img_i),": ",gsub("[\n]", " ", text_i)))
    
    # save transcription as txt file
    file_txt_out <- paste0(path_out,sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(img_i)),".txt")
    fileConn <- file(file_txt_out)
    writeLines(text_i, fileConn)
    close(fileConn)
    
    
  }
}

remove_empty_labels <- function (dir_label_files){
  txt_files <- list.files(dir_label_files, pattern = "*.txt", full.names = T)
  for(txt_file_i in txt_files){
    text_content_j <- readLines(txt_file_i)
    if (text_content_j == ""){
      cat("\nLabel", txt_file_i, "empty! Text file will be now removed.")
      file.remove(txt_file_i)
    }
  }
}

# 3. text classification ####

# Calls the ChatGPT API with the given prompt and returns the answer
ask_chatgpt <- function(prompt, api_key, version = "gpt-3.5-turbo") {
  
  body = list(
    model = version,
    # to implement ----
    response_format = list(
      type = "json_object"
    ),
    messages = list(list(
      role = "user", 
      content = prompt
    ))
  )
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = body
  )
  return(str_trim(content(response)$choices[[1]]$message$content))
}

# classify text content directly from images using chatGPT ####
# # see https://www.listendata.com/2023/05/chatgpt-in-r.html

base64_image <- function(image_path) {
  image_data <- readBin(image_path, "raw", file.info(image_path)$size)
  encoded_image <- base64enc::base64encode(image_data)
  return(encoded_image)
}

chatGPT_img <- function(prompt, 
                        image_path,
                        modelName = "gpt-4o-2024-08-06",
                        detail = "high",
                        temperature = 0, # 0 less creative, 2 more creative
                        apiKey) {
  
  base64_img = base64_image(image_path)
  base64_img = paste0("data:image/jpeg;base64,",base64_img)
  
  
  response <- tryCatch({
    
    res <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = modelName,
        response_format = list(type = "json_object"),
        temperature = temperature,
        messages = list(
          list(
            role = "system",
            content = "You are a very meticulous scientist"
          ),
          list(
            role = "user",
            content = list(
              list(type = "text", text = prompt),
              list(
                type = "image_url",
                image_url = list(
                  url = base64_img,
                  detail = detail
                )
              )
            )
          )
        ) #messages
      ) #body
    ) #POST function
    
    # check HTTP status
    if (status_code(res) >= 300) {
      warning("API returned error: ", status_code(res))
      warning(content(res, "text", encoding = "UTF-8"))
      return(NULL)
    }
    
    return(res)
    
  }, error = function(e) {
    message("Request failed: ", e$message)
    return(NULL)
  })
  
  # response <- POST(
  #   url = "https://api.openai.com/v1/chat/completions", 
  #   add_headers(Authorization = paste("Bearer", apiKey)),
  #   content_type_json(),
  #   encode = "json",
  #   body = list(
  #     model = modelName,
  #     response_format = list(
  #       type = "json_object"
  #     ),
  #     temperature = temperature,
  #     messages = list(
  #       list(
  #         "role" = "system",
  #         "content" = "You are a very meticulous scientist"
  #       ),
  #       list(
  #         role = "user",
  #         content = list(
  #           list(type = "text", 
  #                text = prompt),
  #           list(type = "image_url", 
  #                image_url = list(url = base64_img, 
  #                                 detail = detail)
  #           )
  #         )
  #       )
  #     ) #messages
  #   ) #body
  # ) #POST function
  # 
  # if(status_code(response)>200) {
  #   stop(content(response))
  # }
  # 
  return(response)
}

# to implement: Structured Outputs ####
ask_chatgpt_2 <- function(prompt, api_key, version = "gpt-3.5-turbo", tools = NULL) {
  
  # Basic message structure
  messages <- list(list(
    role = "user", 
    content = prompt
  ))
  
  # Body structure for the POST request
  body <- list(
    model = version,
    messages = messages
  )
  
  # Add tools to the body if provided
  if (!is.null(tools)) {
    body$tools <- list(tools)
  }
  
  # Make the POST request
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = body
  )
  
  # Extract and return the content of the response
  result <- content(response, as = "parsed", type = "application/json")
  return(str_trim(result$choices[[1]]$message$content))
}
# 
# # Example usage with structured output tools:
# tools <- list(
#   type = "function",
#   function = list(
#     name = "query",
#     description = "Execute a query.",
#     strict = TRUE,
#     parameters = list(
#       type = "object",
#       properties = list(
#         table_name = list(
#           type = "string",
#           enum = c("orders")
#         ),
#         columns = list(
#           type = "array",
#           items = list(
#             type = "string",
#             enum = c(
#               "id",
#               "status",
#               "expected_delivery_date",
#               "delivered_at",
#               "shipped_at",
#               "ordered_at",
#               "canceled_at"
#             )
#           )
#         ),
#         conditions = list(
#           type = "array",
#           items = list(
#             type = "object",
#             properties = list(
#               column = list(
#                 type = "string"
#               ),
#               operator = list(
#                 type = "string",
#                 enum = c("=", ">", "<", ">=", "<=", "!=")
#               ),
#               value = list(
#                 anyOf = list(
#                   list(type = "string"),
#                   list(type = "number"),
#                   list(type = "object", properties = list(
#                     column_name = list(type = "string")
#                   ), required = list("column_name"), additionalProperties = FALSE)
#                 )
#               )
#             ),
#             required = c("column", "operator", "value"),
#             additionalProperties = FALSE
#           )
#         ),
#         order_by = list(
#           type = "string",
#           enum = c("asc", "desc")
#         )
#       ),
#       required = c("table_name", "columns", "conditions", "order_by"),
#       additionalProperties = FALSE
#     )
#   )
# )

# # Example call to the function
# result <- ask_chatgpt(
#   prompt = "look up all my orders in may of last year that were fulfilled but not delivered on time",
#   api_key = "your_api_key_here",
#   tools = tools
# )
# 
# cat(result)



multiple_ask_chatGPT <- function(prompts, version = "gpt-3.5-turbo"){
  replies <- c()
  for (prompt_i in prompts){
    prompt_lines = readLines(prompt_i)
    prompt_text <- paste(prompt_lines, collapse = "\n")
    reply = ask_chatgpt(prompt = prompt_text, version = version)
    replies = c(replies, reply)
    Sys.sleep(WAITING_TIME_GPT)
  }
  return (replies)
}

# classify_labels <- function(transcribed_label_file, prompts){
# 
#   replies <- multiple_ask_chatGPT(prompts)
#   class <- replies
#   classes <- data.frame(transcribed_label_file, class)
#   
#   return(classes)
# }

classify_multi_labels <- function(prompts_folder, class_folder, max_attempts, path_transcribed_labels, single_label_specs,
                                  file_class_names = "label_class_names.txt", api_key, version = "gpt-3.5-turbo", quiet = F){
  
  prompts = list.files(substr(prompts_folder,1, nchar(prompts_folder)-1), pattern = "*.txt", full.names = T)
  text_files <- list.files(path_transcribed_labels, pattern = "*.txt", full.names = T)
  class_names = readLines(file_class_names)
  
  badly_classified <- c()
  
  n_i = 0
  for (prompt_i in prompts){
    n_i = n_i + 1
    cat(paste0("\n\t[",n_i,"/",length(prompts),"] "))
    try_n = 0 # counter of tries
    valid_reply = F # valid reply
    while((valid_reply != T) & (try_n < max_attempts )){ # try up to 5 times until we get a correct reply 
      try_n = try_n+1
      reply_i <- ask_chatgpt(paste(readLines(prompt_i), collapse = "\n"), api_key, version)
      if(validate(reply_i)){ # check if string is in correct JSON format
        json_data <- fromJSON(reply_i)
        matrix_json_i <- cbind(names(json_data),t(as.data.frame(json_data))) #transform JSON into a matrix
        
        barcode_i <- gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(prompt_i))) # get current barcode
        filenames_barcode_i <- text_files[grepl(barcode_i, text_files)] # get number of labels for that barcode
        if (length(filenames_barcode_i) == nrow(matrix_json_i)){ # check that we have one classification for each label
          if(setequal(matrix_json_i[,1], basename(filenames_barcode_i))){ # check that names of labels correspond to file names
            if(identical(setdiff(matrix_json_i[,2], class_names), character(0))){# check that classes belongs to allowed ones
              if(("Main_label" %in% matrix_json_i[,2]) & (sum(matrix_json_i[,2] == "Main_label") == 1)){ # check if there is one and only one class Main_label
                if(!quiet) cat(paste0("valid reply for ", basename(prompt_i),"  ✅️"))
                valid_reply = T
                file_reply_out <- paste0(class_folder, gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(prompt_i))),".tsv")
                write.table(matrix_json_i , row.names = F, col.names = c("label","class"), sep="\t", quote = T, file = file_reply_out)
                #matrix_classes_all = rbind(matrix_classes_all, matrix_json_i)
              } else {
                cat(paste0(" ⚠️  zero or more than one Main_label classes in ", basename(prompt_i), "..."))
                if (try_n == max_attempts) {
                  badly_classified <- c(badly_classified, prompt_i)
                  cat(" ❌  no success after",try_n,"attempts, saved in badly classified list")
                } else cat("try again...")
              }
              
            } else {
              cat(paste0(" ⚠️️  one or few classes in ", basename(prompt_i)," are wrong... "))
              if (try_n == max_attempts) {
                badly_classified <- c(badly_classified, prompt_i)
                cat(" ❌  no success after",try_n,"attempts, saved in badly classified list")
              } else cat("try again...")
            }
          } else {
            cat(paste0(" ⚠️   one or few label names in json object for ", basename(prompt_i), " do not match file names... "))
            if (try_n == max_attempts) {
              badly_classified <- c(badly_classified, prompt_i)
              cat(" ❌  no ouccess after",try_n,"attempts, saved in badly classified list")
            } else cat("try again...")
          }
        } else {
          cat(" ⚠️   number of labels in json object for ", basename(prompt_i), "(",nrow(matrix_json_i),") does not match number of text labels (",length(filenames_barcode_i),") ")
          if (try_n == max_attempts) {
            badly_classified <- c(badly_classified, prompt_i)
            cat(" ❌  no success after",try_n,"attempts, saved in badly classified list")
          } else cat("try again...")
        }
      } else {
        cat(" ⚠️️  error while reading json object for ", basename(prompt_i), ", ")
        if (try_n == max_attempts) {
          badly_classified <- c(badly_classified, prompt_i)
          cat(" ❌  no success after",try_n,"attempts, saved in badly classified list")
        } else cat("try again...")
      }
    }
    
    
    if(version == "gpt-4" & n_i%%5 == 0) {
      cat("\tlet's rest a bit...")
      Sys.sleep(WAITING_TIME_GPT) # wait a bit between one call to the other to avoid empty replies using chatgpt v4 
      #(there is a limit on the number of calls per minutes much lower than with version 3.5-turbo)
      if(n_i%%15 == 0) Sys.sleep(5*WAITING_TIME_GPT)
    }
  }
  
  # create a large table with all classified labels and their texts so it is easier to check if classification is correct
  matrix_classes_all <- matrix(ncol = 2)
  colnames(matrix_classes_all) <- c("label","class")
  
  for (matrix_files_i in list.files(path = class_folder, pattern = "\\.tsv")){
    matrix_i = read.table(paste0(class_folder,matrix_files_i), header = T)
    matrix_classes_all = rbind(matrix_classes_all, matrix_i)
  }
  
  matrix_classes_all <- na.omit(matrix_classes_all)
  texts = c()
  for (i in 1 : nrow(matrix_classes_all)){
    text_i = paste(readLines(paste0(path_transcribed_labels,matrix_classes_all[i,1])), collapse = " ")
    texts <-c(texts, text_i)
  }
  matrix_classes_all <- cbind(matrix_classes_all, texts)
  write.table(matrix_classes_all , row.names = F, col.names = c("label","class","text"), sep="\t", quote = T, file = paste0(class_folder,"all_classes.tsv"))
  return(badly_classified)
}

library(jsonlite)

# Function to check if two JSON objects have the same structure
json_structure_equal <- function(json1, json2) {
  # Convert JSON strings to lists
  obj1 <- fromJSON(json1)
  obj2 <- fromJSON(json2)
  
  if (is.list(obj1) && is.list(obj2)) { # Check if both objects are lists
    if (length(obj1) != length(obj2)) { # Check if they have the same number of elements
      return(FALSE)
    } else if (!identical(names(obj1), names(obj2))) { # Check if keys are the same
      return(FALSE)
    } else return(TRUE)
  } else return(FALSE)
}

extract_infos_chatGPT <- function (path_prompts, path_extracted_info, api_key,max_attempts, version = "gpt-3.5-turbo", valid_JSON_dictionary){
  
  prompts = list.files(substr(path_prompts, 1, nchar(path_prompts)-1), pattern = "*.txt", full.names = T)
  n_i = 0
  
  for (prompt_i in prompts){
    n_i = n_i+1
    if(file.exists(paste0(path_extracted_info, gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(prompt_i))),".json"))){ #if json object already in folder, the information was already extracted, skipping the current one
      cat("\n  ℹ️  info from", gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(prompt_i))),"already extracted, skipping.")
    } else {
      cat(paste0("\n\t[",n_i,"/",length(prompts),"] ✍️  transcribing ", basename(prompt_i), ". "))
      n_attempts=0
      repeat {
        n_attempts = n_attempts+1
        cat(paste0("Attempt ",n_attempts,": "))
        reply_i <- ask_chatgpt(prompt = paste(readLines(prompt_i), collapse = "\n"), api_key =  api_key, version = version)
        if (validate(reply_i)){
          if(json_structure_equal(reply_i, paste(readLines(valid_JSON_dictionary), collapse = "\n"))){ # check if reply has identical structure to the valid JSON dictionary
            file_reply_out <- paste0(path_extracted_info, gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(prompt_i))),".json")
            fileConn <- file(file_reply_out)
            writeLines(reply_i, fileConn)
            close(fileConn)
            cat("valid reply, saved  ✅️")
            break
          } else {
            cat(" ❌️  different json structure than valid ",valid_JSON_dictionary,". ")
            if(n_attempts == max_attempts) {
              cat("Max attempts (",max_attempts,") reached, skipping.")
              break
            } else {cat("Retrying...")}
          }
        } else {
          cat(" ❌️  wrong json format. ")
          if(n_attempts == max_attempts) {
            cat("Max attempts (",max_attempts,") reached, skipping.")
            break} else {cat("Retrying...")}
        }
      }
      
      
      if(version == "gpt-4") {
        Sys.sleep(WAITING_TIME_GPT) # wait a bit between one call to the other to avoid empty replies using chatgpt v4
        if(n_i%%15 == 0) Sys.sleep(5*WAITING_TIME_GPT)
      }
    }
  }
}

extract_infos_chatGPT_imgs <- function (prompt, path_label_collages, path_extracted_info, api_key, 
                                        max_attempts = 1, version = "gpt-4o-2024-08-06", temperature = 0, 
                                        valid_JSON_dictionary){
  token_in = 0
  token_out = 0
  list_collage_imgs = list.files(path_label_collages, pattern = "*.jpg")
  n_i = 0
  
  for (img_i in list_collage_imgs){
    n_i = n_i+1
    barcode_i = gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", img_i))
    if(file.exists(paste0(path_extracted_info, barcode_i,".json"))){ #if json object already in folder, the information was already extracted, skipping the current one
      cat("\n  ℹ️  info from", barcode_i,"already extracted, skipping.")
    } else {
      cat(paste0("\n\t[",n_i,"/",length(list_collage_imgs),"]  ✍️  transcribing ", barcode_i, ". "))
      n_attempts = 0
      repeat {
        n_attempts = n_attempts+1
        cat(paste0("Attempt ",n_attempts,": "))
        resp <- chatGPT_img(prompt = paste(readLines(prompt), collapse="\n"), image_path = paste0(path_label_collages,img_i),modelName = version, temperature = temperature, apiKey = api_key)
        reply_i <- str_trim(content(resp)$choices[[1]]$message$content)
        token_in <- token_in+content(resp)$usage$prompt_tokens
        token_out <- token_out+content(resp)$usage$completion_tokens
        if (validate(reply_i)){
          if(json_structure_equal(reply_i, paste(readLines(valid_JSON_dictionary), collapse = "\n"))){ # check if reply has identical structure to the valid JSON dictionary
            file_reply_out <- paste0(path_extracted_info, barcode_i,".json")
            fileConn <- file(file_reply_out)
            writeLines(reply_i, fileConn)
            close(fileConn)
            cat(" valid reply, saved  ✅️")
            break
          } else {
            cat(" ❌️  different json structure than valid ",valid_JSON_dictionary,". ")
            if(n_attempts == max_attempts) {
              cat("Max attempts (",max_attempts,") reached, skipping.")
              break
            } else {cat("Retrying...")}
          }
        } else {
          cat(" ❌️  wrong json format. ")
          if(n_attempts == max_attempts) {
            cat("Max attempts (",max_attempts,") reached, skipping.")
            break} else {cat("Retrying...")}
        }
      }
      
      
      if(version == "gpt-4") {
        Sys.sleep(WAITING_TIME_GPT) # wait a bit between one call to the other to avoid empty replies using chatgpt v4
        if(n_i%%15 == 0) Sys.sleep(5*WAITING_TIME_GPT)
      }
    }

  }
  return (c(token_in, token_out))
}

extract_infos_chatGPT_whole_imgs <- function (prompt, path_images, path_extracted_info, api_key, 
                                        max_attempts = 1, version = "gpt-4o-2024-08-06", temperature = 0, 
                                        valid_JSON_dictionary){
  token_in = 0
  token_out = 0
  list_collage_imgs = list.files(path_images, pattern = "*.jpg")
  n_i = 0
  
  for (img_i in list_collage_imgs){
    n_i = n_i+1
    barcode_i = gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", img_i))
    if(file.exists(paste0(path_extracted_info, barcode_i,".json"))){ #if json object already in folder, the information was already extracted, skipping the current one
      cat("\n  ℹ️  info from", barcode_i,"already extracted, skipping.")
    } else {
      cat(paste0("\n\t[",n_i,"/",length(list_collage_imgs),"]  ✍️  transcribing ", barcode_i, ". "))
      n_attempts = 0
      repeat {
        n_attempts = n_attempts+1
        cat(paste0("Attempt ",n_attempts,": "))
        #cat(paste0(paste(readLines(prompt), collapse="\n"),basename(barcode_i),"\n"))
        resp <- chatGPT_img(prompt = paste0(paste(readLines(prompt), collapse="\n"),basename(barcode_i),"\n"),
                             image_path = paste0(path_images,img_i),
                             modelName = version, 
                             temperature = temperature, 
                             apiKey = api_key)
        reply_i <- str_trim(content(resp)$choices[[1]]$message$content)
        token_in <- token_in+content(resp)$usage$prompt_tokens
        token_out <- token_out+content(resp)$usage$completion_tokens
        if (validate(reply_i)){
          if(json_structure_equal(reply_i, paste(readLines(valid_JSON_dictionary), collapse = "\n"))){ # check if reply has identical structure to the valid JSON dictionary
            file_reply_out <- paste0(path_extracted_info, barcode_i,".json")
            fileConn <- file(file_reply_out)
            writeLines(reply_i, fileConn)
            close(fileConn)
            cat(" valid reply, saved  ✅️")
            break
          } else {
            cat(" ❌️  different json structure than valid ",valid_JSON_dictionary,". ")
            if(n_attempts == max_attempts) {
              cat("Max attempts (",max_attempts,") reached, skipping.")
              break
            } else {cat("Retrying...")}
          }
        } else {
          cat(" ❌️  wrong json format. ")
          if(n_attempts == max_attempts) {
            cat("Max attempts (",max_attempts,") reached, skipping.")
            break} else {cat("Retrying...")}
        }
      }
      
      
      if(version == "gpt-4") {
        Sys.sleep(WAITING_TIME_GPT) # wait a bit between one call to the other to avoid empty replies using chatgpt v4
        if(n_i%%15 == 0) Sys.sleep(5*WAITING_TIME_GPT)
      }
    }
    
  }
  return (c(token_in, token_out))
}

escape_xml_characters <- function(string){
  # to change those characters in:
  # "   &quot;
  # '   &apos;
  # <   &lt;
  # >   &gt;
  # &   &amp;
  string_out <- gsub("&", "&amp;", string)
  string_out <- gsub("\"", "&quot;", string_out)
  string_out <- gsub("'", "&apos;", string_out)
  string_out <- gsub("<", "&lt;", string_out)
  string_out <- gsub(">", "&gt;", string_out)
  return(string_out)
}

create_classification_prompts <- function(path_transcribed_labels, skip_barcodes, prompt_fixed_p1, prompt_fixed_p2, prompts_out_folder, quiet=T){
  
  fixed_text_1 <- readLines(prompt_fixed_p1)
  fixed_text_2 <- readLines(prompt_fixed_p2)
  text_files <- list.files(path_transcribed_labels, pattern = "*.txt", full.names = T)
  
  barcodes = unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(text_files))))
  n_i = 0
  n_skip = 0
  n_prompt = 0
  # create one prompt for each barcode (herbarium specimen)
  for (barcode_i in barcodes){
    n_i = n_i+1
    if (barcode_i %in% skip_barcodes){
      if(!quiet) cat(paste0("\n\t[",n_i,"/",length(barcodes),"] skipping barcode ", barcode_i))
      n_skip = n_skip + 1
    } else {
      filenames_barcode_i <- text_files[grepl(barcode_i, text_files)]
      prompt_i = fixed_text_1
      for (filename_j in filenames_barcode_i){
        text_content_j <- readLines(filename_j)
        # write first file name
        # label_nr = sub(paste0(barcode_i,"_(\\d+).*"), "\\1", basename(filename_j))
        # prompt_i <- c(prompt_i, paste0("Text number ",label_nr,":"), paste0("\t",text_content_j))
        prompt_i <- c(prompt_i, paste0("\t<text_label id=\"", basename(filename_j), "\">\n\t\t<![CDATA[\n\t\t", paste(text_content_j, collapse = "\n\t\t"),"\n\t\t]]>\n\t</text_label>"))
      }
      prompt_i <- c(prompt_i, fixed_text_2)
      # save as txt file
      
      if(!quiet) cat(paste0("\n\t[",n_i,"/",length(barcodes),"] prompt for classification of labels of ", barcode_i," ready, it includes ", length(filenames_barcode_i), " labels"))
      n_prompt = n_prompt + 1
      file_txt_out <- paste0(prompts_out_folder,"/",barcode_i,"_catPrompt.txt")
      fileConn <- file(file_txt_out)
      writeLines(prompt_i, fileConn)
      close(fileConn)
    }
  }
  return(c(n_i, n_skip, n_prompt))
}

single_label_classify <- function(path_transcribed_labels,
                                  path_class_labels, quiet=T){
  
  # list all transcription files
  text_files <- list.files(path_transcribed_labels, pattern = "*.txt", full.names = T)
  # obtain a list of unique barcodes
  barcodes = unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(text_files))))
  
  barcodes_with_single_label = c()
  
  for (barcode_i in barcodes){
    # obtain a list of files belonging to barcode_i
    filenames_barcode_i <- text_files[grepl(barcode_i, text_files)]
    if (length(filenames_barcode_i) == 1){
      barcodes_with_single_label = c(barcodes_with_single_label, barcode_i)
      if(!quiet) cat(paste0("\n\tbarcode ",barcode_i," includes only one label, it will be assigned to class Main_label"))
      label_name = basename(filenames_barcode_i[1])
      class_i = paste0("label\tclass\n",label_name,"\tMain_label")
      file_txt_out <- paste0(path_class_labels,"/",barcode_i,".tsv")
      fileConn <- file(file_txt_out)
      writeLines(class_i, fileConn)
      close(fileConn)
    }
  }
  return(barcodes_with_single_label)
}

find_single_labels <- function(dir_labels){
  text_files <- list.files(dir_labels, pattern = "*.txt", full.names = T)
  barcodes = unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(text_files))))
  specimens_with_single_label = c()
  for (barcode_i in barcodes){
    filenames_barcode_i <- text_files[grepl(barcode_i, text_files)]
    if (length(filenames_barcode_i) == 1){
      specimens_with_single_label = c(specimens_with_single_label, barcode_i)
    }
  }
  return(specimens_with_single_label)
}

create_single_classification_prompts <- function(text_files, prompt_fixed_part, prompts_out_folder){
  # create one prompt for each text label
  fixed_text <- readLines(prompt_fixed_part)
  prompt_list = c()
  
  for (text_file_i in text_files){
    text_i = readLines(text_file_i)
    prompt_i = c(fixed_text, text_i)
    # save as txt file
    file_txt_out <- paste0(prompts_out_folder,"/",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(text_file_i)),"_catPrompt.txt")
    fileConn <- file(file_txt_out)
    writeLines(prompt_i, fileConn)
    close(fileConn)
    prompt_list = c(prompt_list, file_txt_out)
  }
  
  return(prompt_list)
}

create_multi_transcription_prompts <- function(path_transcribed_labels, 
                                               prompt_fixed_part, 
                                               prompts_out_folder, 
                                               path_class_labels,
                                               quiet = F){
  
  fixed_text <- readLines(prompt_fixed_part)
  
  transcribed_files = list.files(substr(path_transcribed_labels,1,nchar(path_transcribed_labels)-1), pattern = "*.txt", full.names = T)
  
  class_files = list.files(substr(path_class_labels,1,nchar(path_class_labels)-1), pattern = "*.tsv", full.names = T)
  
  barcodes = unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(transcribed_files))))
  bad_classes <- c()
  n_i = 0
  for (barcode_i in barcodes){
    n_i = n_i+1
    if(!quiet) cat(paste0("\n\t[", n_i, "/",length(barcodes),"] "))
    # paste fixed prompt part [+ class + OCR text]n_labels
    transcriptions_barcode_i <- transcribed_files[grepl(barcode_i, transcribed_files)]
    
    skip_next <- F
    tryCatch({
      classes_barcode_i <- read.table(class_files[grepl(barcode_i, class_files)], header = T)
    }, error = function(e){
      skip_next <<- T
      cat(" ❌️  error while reading table of classification for ", barcode_i)
    }
    )
    if(nrow(classes_barcode_i)!= length(transcriptions_barcode_i)){
      if(!quiet) cat(paste0(" ❌️  number of classes in ", class_files[grepl(barcode_i, class_files)]," (",nrow(classes_barcode_i),
                            ") different from number of labels for ", barcode_i," (",length(transcriptions_barcode_i),")"))
      bad_classes <- c(bad_classes,barcode_i)
      skip_next <- T
    }
    if(!skip_next){
      prompt_i = fixed_text
      for (j in 1 : length(transcriptions_barcode_i)){
        label_name = basename(transcriptions_barcode_i[j])
        label_class = classes_barcode_i[classes_barcode_i$label==label_name,]$class
        text_content_j <- readLines(transcriptions_barcode_i[j])
        prompt_i <- c(prompt_i, paste0("<",label_class,">"), paste0("\t",text_content_j),paste0("</",label_class,">"))
      }
      
      if(!quiet) cat(paste0("prompt for ", barcode_i," ready"))
      # save final prompt as txt file
      file_txt_out <- paste0(prompts_out_folder, barcode_i, ".txt")
      fileConn <- file(file_txt_out)
      writeLines(prompt_i, fileConn)
      close(fileConn)
    } else cat(" Skipping")
    
    
  }
  return(bad_classes)
}

# old function
# create_transcription_prompts <- function(text_files, prompt_fixed_part, prompts_out_folder, classes){
#   
#   fixed_text <- readLines(prompt_fixed_part)
#   
#   prompt_list = c()
#   
#   barcodes = unique(gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(text_files))))
#   
#   for (barcode_i in barcodes){
#     # paste fixed prompt part [+ class + OCR text]n_labels
#     filenames_barcode_i <- text_files[grepl(barcode_i, text_files)]
#     classes_barcode_i <- classes[grepl(barcode_i, classes$transcribed_label),]$class
#     prompt_i = c(fixed_text)
#     for (j in 1 : length(filenames_barcode_i)){
#       text_content_j <- readLines(filenames_barcode_i[j])
#       prompt_i <- c(prompt_i,paste0(classes_barcode_i[j],":"), text_content_j)
#     }
#     
#     # save as txt file
#     file_txt_out <- paste0(prompts_out_folder,"/",barcode_i,"_prompt.txt")
#     fileConn <- file(file_txt_out)
#     writeLines(prompt_i, fileConn)
#     close(fileConn)
#     prompt_list = c(prompt_list, file_txt_out)
#   }
#   return(prompt_list)
# }
# 

# create_single_table_from_multiple_tables <- function (path_tables, out_dir){
#   
#   tables = list.files(path_tables, pattern = "*.txt", full.names = T)
#   
#   final_table <- matrix()
#   
#   for(file_table_i in tables){
#     table_i <- read.table(file_table_i, sep = "\t", header = T)
#     
#     final_table <- plyr::rbind.fill(final_table, 
#                                     cbind(barcode = gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(json_object_i))),
#                                           table_i))
#   }
#   
#   file_out = paste0(out_dir,"table_extracted_info.tsv")
#   write.table(final_table , row.names = F,sep="\t",quote = T, file = file_out)
#   return(file_out)
# }

create_table_from_multiple_JSON <- function (path_JSON_objects, path_classes = NULL, out_dir, quiet = F){
  
  json_objects = list.files(substr(path_JSON_objects, 1, nchar(path_JSON_objects)-1), pattern = "*.json", full.names = T)
  matrix_json <- data.frame()
  bad_jsons <- c()
  if (!is.null(path_classes)){
    OCR_texts <- read.table(paste0(path_classes,"all_classes.tsv"), header=T)
    
    for(json_object_i in json_objects){
      if(!quiet) cat("\n\ttrying to add", basename(json_object_i), "...")
      tryCatch(
        {
          json_data <- fromJSON(json_object_i)
          barcode_i = gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(json_object_i)))
          OCR_text_i = OCR_texts[grepl(barcode_i, OCR_texts$label),]
          matrix_json <- rbind.fill(matrix_json, 
                                    cbind(barcode = barcode_i,
                                          OCR_Main = OCR_text_i[OCR_text_i$class == "Main_label", 3],
                                          OCR_RevDet = ifelse (identical(OCR_text_i[OCR_text_i$class == "Rev_Det_label", 3], character(0)),"",
                                                               paste(OCR_text_i[OCR_text_i$class == "Rev_Det_label", 3], collapse = "<sep_label>")),
                                          OCR_NoteLabel = ifelse (identical(OCR_text_i[OCR_text_i$class == "Note_label", 3], character(0)),"", 
                                                                  paste(OCR_text_i[OCR_text_i$class == "Note_label", 3], collapse = "<sep_label>")),
                                          OCR_OtherLabel = ifelse(identical(OCR_text_i[OCR_text_i$class == "Other_label", 3],character(0)),"",
                                                                  paste(OCR_text_i[OCR_text_i$class == "Other_label", 3], collapse = "<sep_label>")),
                                          as.data.frame(json_data)))
          
          if(!quiet) cat(" ✅️")
        }, error = function(e){
          message(" ❌️  error while adding json object ", json_object_i, ", skipping...")
          bad_jsons <- c(bad_jsons, json_object_i)
        }
      )
    }
  } else {
    for(json_object_i in json_objects){
      if(!quiet) cat("\n\ttrying to add", basename(json_object_i), "...")
      tryCatch(
        {
          json_data <- fromJSON(json_object_i)
          barcode_i = gsub("_.*", "", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(json_object_i)))
          matrix_json <- rbind.fill(matrix_json, 
                                    cbind(barcode = barcode_i,
                                          as.data.frame(json_data)))
          
          if(!quiet) cat(" ✅️")
        }, error = function(e){
          message(" ❌️  error while adding json object ", json_object_i, ", skipping...")
          bad_jsons <- c(bad_jsons, json_object_i)
        }
      )
    }
  }
  
  #View(matrix_json)
  file_out = paste0(out_dir,paste0(basename(out_dir),"_table_extracted_info.tsv"))
  write.table(matrix_json , row.names = F, sep="\t", quote = T, file = file_out)
  return(bad_jsons)
}
