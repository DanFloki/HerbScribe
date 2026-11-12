# Author: Daniele Buono
# 
# d.buono@bio.lmu.de
# 
# LMU Munich

if (!requireNamespace("hms")){
	install.packages("hms", dependencies = T)
}
library(hms) # to calculate execution time
if (!requireNamespace("yaml")){
	install.packages("yaml", dependencies = T)
}
library(yaml) # to read yaml settings file
if (!requireNamespace("filesstrings")){
	install.packages("filesstrings", dependencies = T)
}
library(filesstrings) # to move files

args = commandArgs(T)

if (length(args) < 1) stop("Usage: \n> Rscript transcribe_labels_v2.R settings.yaml 
                           <optional: path_original_images. If not specified, it will look for the folder specified in settings.yaml in EXP path>
                           <optional: output_path. If not specified, output files will be created in EXP folder in settings.yaml>")
file_yaml = args[1]
#file_yaml = "settings.yaml"
settings <- read_yaml(file_yaml)

cat("\n\nWelcome! This is an R script to transcribe information from herbarium labels. \nVersion 3")

cat("\n\n ⚙️  settings", paste0("(",file_yaml,")"),"successfully loaded!\n")
cat("______________________________________________________________________\n")
for(i in readLines(file_yaml)) cat("\n",i)
cat("______________________________________________________________________\n")



time_start <- Sys.time()
# assign folders
if (length(args) == 1){ # one argument, paths inside settings
  exp_path = settings$directories$EXP
  if((substr(exp_path, nchar(exp_path), nchar(exp_path))!="/")) exp_path = paste0(exp_path, "/")
  path_working_images <- paste0(exp_path, settings$directories$ORIGINAL_IMAGES)
} else if (length(args) == 2) { # two arguments, path exp inside settings, path working images specified
  exp_path = settings$directories$EXP
  if((substr(exp_path, nchar(exp_path), nchar(exp_path))!="/")) exp_path = paste0(exp_path, "/")
  path_working_images <- args[2]
} else if (length(args) == 3){ # three arguments, paths working images and experiment specified
  path_working_images <- args[2]
  exp_path = args[3]
}

if (substr(path_working_images, nchar(path_working_images), nchar(path_working_images))!="/") path_working_images = paste0(path_working_images, "/")
if (substr(exp_path,nchar(exp_path),nchar(exp_path)) != "/") exp_path=paste0(exp_path,"/")

# run all functions ----
 
source("functions_v3.R")

##
## 0.1 check images
check_subdirs = FALSE
if(settings$look_sub_dirs == "T") check_subdirs = TRUE
list_working_images <- list.files(path = path_working_images, pattern = "\\.(jpg|jpeg)", recursive = check_subdirs)
n_images = length(list_working_images)
if(length(list_working_images) == 0) {
  stop(paste0("\n 🛑️  no jpeg files in ", path_working_images, ". Please check path images to analyse in yaml configuration file or in specified path."))
} else cat(paste0("\n ℹ️  ",length(list_working_images)),"images to work on:",list_working_images)

dir.create(exp_path, showWarnings = FALSE)

## 0. crop rulers if settings ----
if(settings$crop_ruler == "T"){
  cat("\n ✂  option to crop ruler set to \"T\", cropping rulers... ")
  path_working_images <- crop_ruler_from_imgs(exp_path, path_working_images, list_working_images)
  list_working_images =  list.files(path_working_images, pattern = "\\.(jpg|jpeg)")
  cat("done.")
} else cat("\n ✂  option to crop ruler set to \"F\", we will use original non cropped images")

## 0. resize images if settings ----
if(!settings$skip_step_resize == "T"){
  if(settings$resize_imgs == "T"){
    cat("\n 📐️  option to resize images set to \"T\", resizing images... ")
    dir_original_img_resized = paste0(exp_path,settings$directories$RESIZED_IMAGES)
    dir.create(file.path(dir_original_img_resized), showWarnings = F)
    resized_imgs <- resize_imgs(img_folder = path_working_images, 
                                list_jpg_files = list_working_images,
                                dir_img_resized = dir_original_img_resized,
                                max_img_size = as.numeric(settings$images_max_size_Mb),
                                cores = settings$cores_resizing_img,
                                quality = 80)
    cat("done.")
    path_working_images = dir_original_img_resized
  } else cat("\n 📐️  option to resize images set to \"F\", we will use images in their original size")
} else cat("\n\n 🚀  skipping image resizing step, already done?")

## skips steps 1 and 2: 1. identify labels with python script detect.py 
## 2. crop labels identified by yolov7 from images

# check if option to use images in chatGPT is TRUE ----
if(settings$imgs_ChatGPT == "T"){

  ## 3b. whole images instead collage ----
  


  ## 4b. transcribe info from whole pictures ----
  cat("\n\n 🧠️  extracting info from whole herbarium images using ChatGPT...")
  
  chatGPT_extracted_info = paste0(exp_path, settings$directories$CHATGPT_EXTRACTED_INFO)
  dir.create(chatGPT_extracted_info, showWarnings = F)
  
  token_used <- extract_infos_chatGPT_whole_imgs(prompt = settings$prompt_transcription_whole_img, 
                                           path_images = path_working_images,
                                           path_extracted_info = chatGPT_extracted_info,
                                           max_attempts = settings$max_attempts_gpt,
                                           api_key = settings$openAI_API_key,
                                           version = settings$chatGPTversion,
                                           temperature = settings$temperature,
                                           valid_JSON_dictionary = settings$json_dictionary_whole_img)
  
  n_used_images = length(list.files(path = path_working_images, pattern = "\\.(jpg|jpeg)"))
  
  cat(paste0("\n\n     Done.\n\n     tokens in: ",token_used[1]," (",round(token_used[1]*(settings$price_1M_input_tokens/1e6),2)," $); tokens out: ",token_used[2]," (",round(token_used[2]*(settings$price_1M_output_tokens/1e6),2)," $). ",
             "Cost of this run: ",round(token_used[1]*(settings$price_1M_input_tokens/1e6)+token_used[2]*(settings$price_1M_output_tokens/1e6),2), " $ (",round((token_used[1]*(settings$price_1M_input_tokens/1e6)+token_used[2]*(settings$price_1M_output_tokens/1e6))/n_used_images,5)," $/image)"))
  # save results in a table ----
  cat("\n\n 📄️  creating a table with all the information extracted...")
  
  bad_jsons <- create_table_from_multiple_JSON(path_JSON_objects = chatGPT_extracted_info,
                                               out_dir = exp_path,
                                               quiet = T)
} else { # use OCR for extracting text and feed LLM only text 
  ## 3a. run OCR on each separated label and produce a .txt file for each ----
  dir_transcribed_labels <- paste0(exp_path,settings$directories$OCR_TRANSCRIBED_LABELS)
  
  if(!settings$skip_step_googleOCR == "T"){
    cat("\n\n ✍️  runnig GoogleOCR to extract text from labels...\n ")
    set_google_ocr_credentials(settings$google_credentials)
    
    dir.create(dir_transcribed_labels, showWarnings = F)
    
    transcribe_labels_googleOCR(path_imgs = dir_crop, # function will filter for "labels" only
                                path_out = dir_transcribed_labels,
                                use_format_txt = settings$use_formatted_transcribed_text)
  } else cat("\n\n 🚀  skipping GoogleOCR, already done?")
  # if some labels contains no text, removing them
  #remove_empty_labels(dir_transcribed_labels)
  
  # 4a. use ChatGPT to interpret labels ----
  ## 4a.1 classify labels ----
  ### promtps preparation ----
  prompts_class_folder <- paste0(exp_path,settings$directories$PROMPTS_CLASSIFY)
  path_class_labels <- paste0(exp_path, settings$directories$CLASS_LABELS)
  
  if(!settings$skip_step_classifyLabels == "T"){
    dir.create(prompts_class_folder, showWarnings = F)
    dir.create(path_class_labels, showWarnings = F)
    
    cat("\n\n 📜️  creating prompts to use for ChatGPT classification of labels...")
    single_label_specs <- single_label_classify(path_transcribed_labels = dir_transcribed_labels,
                                                path_class_labels = path_class_labels)
    # create only one prompt to classify labels, explain that one has to be the main one
    count_prompts <- create_classification_prompts(path_transcribed_labels = dir_transcribed_labels,
                                                   skip_barcodes = single_label_specs,
                                                   prompt_fixed_p1 = settings$prompt_class_fixed_p1,
                                                   prompt_fixed_p2 = settings$prompt_class_fixed_p2,
                                                   prompts_out_folder = prompts_class_folder,
                                                   quiet = T)
    cat(paste0(" ",count_prompts[1]," processed: ",count_prompts[2]," with only one label (will be classified as Main label), ",
               count_prompts[3], " with more than one label which will be classified"))

    #### classifying ----  
    
    cat("\n\n 📑️  classifying labels based on their content in: Main_label, Rev_Det_label, Note_label, Other_label...")
    
    
    bad_classes <- classify_multi_labels(prompts_folder = prompts_class_folder,
                                         class_folder = path_class_labels,
                                         file_class_names = "label_class_names.txt",
                                         max_attempts = settings$max_attempts_gpt,
                                         path_transcribed_labels = dir_transcribed_labels, 
                                         api_key = settings$openAI_API_key,
                                         single_label_specs = single_label_specs,
                                         version = settings$chatGPTversion)
    if(!is.null(bad_classes)) {
      cat("\n\n ⚠️  Those barcode labels were wrongly classified:", paste0(bad_classes, collapse = ", "))
      fileConn <- file(paste0(exp_path,"badly_classified_barcodes.txt"))
      writeLines(paste0(bad_classes, collapse = "\n"), fileConn)
      close(fileConn)
    } else cat("\n\n 🥳️  classification of labels complete, apparently no error.")
  } else cat("\n\n 🚀  skipping classification of labels using ChatGPT, already done?")
  
  ## 4a.2 transcript labels ----
  ### prompts preparation ----
  cat("\n\n 📜️  creating prompts to use for ChatGPT information extraction from labels...")
  
  dir_prompt_trans = paste0(exp_path,settings$directories$PROMPTS_TRANSCRIPTION)
  dir.create(dir_prompt_trans, showWarnings = F)
  
  bad_classes <- create_multi_transcription_prompts(path_transcribed_labels = dir_transcribed_labels, 
                                                    prompt_fixed_part = settings$prompt_transcription, 
                                                    prompts_out_folder = dir_prompt_trans, 
                                                    path_class_labels = path_class_labels,
                                                    quiet=T)
  if(!is.null(bad_classes)) {
    cat("\n\n ⚠️  Those barcode labels were wrongly classified:", paste0(bad_classes, collapse = ", "))
    fileConn <- file(paste0(exp_path,"badly_classified_barcodes.txt"))
    writeLines(paste0(bad_classes, collapse = "\n"), fileConn)
    close(fileConn)
  } else cat("\n\n 🥳️ prompt construction from classified labels complete, apparently no error.")
  ### transcription ----
  cat("\n\n 🧠️  extracting info from labels using ChatGPT...")
  
  chatGPT_extracted_info = paste0(exp_path, settings$directories$CHATGPT_EXTRACTED_INFO)
  dir.create(chatGPT_extracted_info, showWarnings = F)
  
  extract_infos_chatGPT(path_prompts = dir_prompt_trans,
                        path_extracted_info = chatGPT_extracted_info,
                        max_attempts = settings$max_attempts_gpt,
                        api_key = settings$openAI_API_key,
                        version = settings$chatGPTversion,
                        valid_JSON_dictionary = settings$json_dictionary_whole_img)
  # save results in a table ----
  cat("\n\n 📄️  creating a table with all the information extracted...")
  
  bad_jsons <- create_table_from_multiple_JSON(path_JSON_objects = chatGPT_extracted_info,
                                               path_classes = path_class_labels,
                                               out_dir = exp_path,
                                               quiet = T)
}

cat(" done.\n\n 👽️  Final transcriptions are stored in", exp_path)
if(!is.null(bad_jsons)){
  cat("\n ⚠   Remember to check badly formatted JSON", paste0(bad_jsons, collapse = ", "))
  fileConn <- file(paste0(exp_path,"badly_formatted_JSON.txt"))
  writeLines(paste0(bad_jsons, collapse = "\n"), fileConn)
  close(fileConn)
} else cat(". No apparent error in JSON files.")

time_finish <- Sys.time()
time_elapsed <- round(as.numeric(difftime(time_finish, time_start, unit = "secs")), digits = 0)
time_hms <- as.character(as_hms(time_elapsed))

# remove intermediate files ----
if(settings$clean_intermediate_files){
  cat("\n\n ℹ️  removing intermediate files... ")
  #unlink(paste0(exp_path,settings$directories$RESIZED_IMAGES), recursive = T) #those are already removed above
  #unlink(paste0(exp_path,settings$directories$YOLO_LABELS), recursive = T) #those are already removed above
  unlink(paste0(exp_path,settings$directories$YOLO_CROPPED_IMAGES), recursive = T)
  unlink(paste0(exp_path,settings$directories$OCR_TRANSCRIBED_LABELS), recursive = T)
  unlink(paste0(exp_path,settings$directories$PROMPTS_CLASSIFY), recursive = T)
  unlink(paste0(exp_path,settings$directories$CLASS_LABELS), recursive = T)
  unlink(paste0(exp_path,settings$directories$PROMPTS_TRANSCRIPTION), recursive = T)
  #unlink(paste0(exp_path,settings$directories$CHATGPT_EXTRACTED_INFO), recursive = T)
  unlink(paste0(exp_path,settings$directories$TEXT_LABEL_COLLAGES), recursive = T)
  cat("done.")
}

cat("\n\n")
cat(paste0(" ℹ️  ",n_images," transcribed images in (hh:mm:ss) ",time_hms,". In average each image took ", round(time_elapsed/n_images, digits = 1)," seconds."))
cat("\n\n\n 😎️  done.\n")


