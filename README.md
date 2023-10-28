# Physical-Acticity-Type-Detection
The course project physical activity type (PAT) detection is a group project with three group members. This project developed machine learning models to classify PATs based on data from GPS sensors and accelerometers. Artificial neural network (ANN) was employed to detect six PATs (Lying, Sitting, Standing, Non-level Walking, Walking, Running) of younger and older adults separately using PyTorch. The transferability of PTA detection models across different age groups was also examined.

## Data
Three uTrail devices were mounted on the left and right hips. The uTrail is a small wearable device that includes an audio sensor, an accelerometer, and a GPS sensor [[1]](#1). This project used data collected by GPS sensors and accelerometers.

- Positions of uTrail devices on the participants' body.
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/884854e0-6491-4f5d-bd8a-f1e492453701" height="350">

- Information recorded by the uTrail device.
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/aa8d548b-588c-418f-a875-1e405f5401c2" height="250">

## Method
- The flow chart of data preprocessing and classification model development.
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/6452947b-893b-481b-bd25-b6769af8f4f4" height="730">

- 80 features were extracted from GPS and accelerometer data.
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/0d04af4d-78d7-4625-97ea-f9688cf6cd2e" height="460">

- Models with training dataset and validation method and testing dataset for evaluating the classification performance differences between the two age groups.
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/01652d5f-c218-4160-881a-1b5290f24c6f" height="280">


## Results
### Example results of YandO-trained-btw-age model
- Average accuracy and loss per epoch for training and validation sets for YandO-trained-btw-age model (ratio=1:1).
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/d7ef6439-01f5-4c6d-9dca-8767ae6c55a3" height="320">

- Evaluation matrix of YandO-trained-btw-age model (ratio=1:1).
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/94a25ae5-cf2b-4de4-a336-d3cff56ac8d2" height="320">

- Confustion matrix of YandO-trained-btw-age model (ratio=1:1).
<img src="https://github.com/leyixu21/Physical-Acticity-Type-Detection/assets/96665869/49e98358-3caf-40c6-a6ee-c4fb0cbd4f96" height="200">

## Reference
<a id="1">[1]</a> 
Allahbakhshi, H., Röcke, C., & Weibel, R. (2021). Assessing the Transferability of Physical Activity Type Detection Models: Influence of Age Group Is Underappreciated. Frontiers in Physiology, 12, 738939.
