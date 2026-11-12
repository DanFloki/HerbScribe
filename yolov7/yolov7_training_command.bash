#!/bin/bash

python .\train.py --img-size 640 --cfg .\cfg\training\yolov7.yaml --hyp .\training_data\hyp.scratch.custom.yaml --batch 20 --epochs 1000 --data .\training_data\data_config_file.yaml --weights .\yolov7.pt --workers 24 --name train_323_herb_picts_plus_55_lichens --device 0

