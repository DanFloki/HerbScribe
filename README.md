This script run only on linux OS.

You need:
- The command line tool imagemagick (sudo apt install imagemagick).
- dependencies necessary to run yolo v7 via conda:
    conda env create -f transcription_environment.yml
    before starting the transcription, activate the conda environment:
    conda activate transcription

You need to obtain your personal OpenAI API key (https://platform.openai.com/), and include it into the settings.yaml file.


To run the script:

Rscript herbScribe_v3.1.R settings.yaml example_images transcriptions_example_images




More detailed instruction on how to use the pipeline are coming soon.
