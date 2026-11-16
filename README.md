This script run only on linux OS.

You need:
- The command line tool imagemagick (sudo apt install imagemagick).
- dependencies necessary to run yolo v7 via conda: conda env create -f transcription_environment.yml
- before starting the transcription, activate the conda environment: conda activate transcription

You need to obtain your personal OpenAI API key (https://platform.openai.com/), and include it into the settings.yaml file.


To run the script:

Rscript herbScribe_v3.1.R settings.yaml example_images transcriptions_example_images


The settings.yaml file includes our current settings, there you can see which prompt are we using right now. If you want to use the LLM to transcribe the text from images, leave the setting imgs_ChatGPT: T. If you do so, the prompt that will be used is in the field prompt_transcription_img_labels. With this prompt, were are extracting the field specified both in the prompt and in the json reference file specified in the field json_dictionary. 
IMPORTANT: If you want to extract different fields, you should modify the json structure that is specidied in the **prompt** and and in the **json_dictionary** to have them **matching exactly**. json_dictionary is used to very the structure of the output of the LLM; if the structure in the two files do not match, the reply from the LLM will not be accepted.



More detailed instruction on how to use the pipeline are coming soon.
