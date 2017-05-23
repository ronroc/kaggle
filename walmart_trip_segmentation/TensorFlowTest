**Build a Traffic Sign Recognition Project**

The goals / steps of this project are the following:
* Load the data set (see below for links to the project data set)
* Explore, summarize and visualize the data set
* Design, train and test a model architecture
* Use the model to make predictions on new images
* Analyze the softmax probabilities of the new images
* Summarize the results with a written report


[//]: # (Image References)

[image1]: ./plot1.png "Visualization"
[image2]: ./plot2.png "Grayscaling"
[image3]: ./plot3.png "Random Noise"
[image4]: ./img6.jpg "Traffic Sign 1"
[image5]: ./img7.jpg "Traffic Sign 2"
[image6]: ./img8.jpg "Traffic Sign 3"
[image7]: ./img9.jpg "Traffic Sign 4"
[image8]: ./img10.jpg "Traffic Sign 5"



Data Set Summary & Exploration

1. Basic summary of the dataset

I used the pandas library to calculate summary statistics of the traffic
signs data set.

* The size of training set is 34799 Images
* The size of the validation set is 4410 Images
* The size of test set is 12630 Images
* The shape of a traffic sign image is 32 by 32
* The number of unique classes/labels in the data set is 43

2. Exploratory visualization of the dataset.

* Distribution of number of samples  in training set per different sign type

![alt text][image1]

* Distribution of number of samples in testing set per different sign type

![alt text][image2]

* Distribution of number of samples in validation set per different sign type

![alt text][image3]

Other visualization can be found in the Ipython notebook

Design and Test a Model Architecture

As a first step, I converted the image to greyscale and tested on the pipeline . In comparison to other pre processing methods it did not produce the same results , hence I did not include it

The other preprocessing step I tried was normalising the image. This step gave a decent lift in comparison to other pre processing methods

My focus was on learning and getting the implementation of the Network right and on availability of time iterate further, hence I used the LeNet-5 implementation described in the classroom

My final model consisted of the following layers:

| Layer         		|     Description	        					|
|:---------------------:|:---------------------------------------------:|
| Input         		| 32x32x3 RGB image   							|
| Layer 1 - Convolutional     	| Input = 32x32x1. Output = 28x28x6. 	|
| RELU					|												|
| Pooling	      	| 28x28x6. Output = 14x14x6 				|
| Layer 2 - Convolutional	    | Output = 10x10x16      									|
| RELU		|        									|
| Pooling				| Input = 10x10x16. Output = 5x5x16.        									|
|Flatten |Input = 5x5x16. Output = 400.|
|Layer 3 - Fully connected. |Input = 400. Output = 120.|
|RELU |                     |
|Layer 4 - Fully connected. |Input = 120. Output = 84.|
|RELU                       |
|Layer 5 - Fully connected. |Input = 84. Output = 10. |

Model Parameters
Epochs: 101
Batch size: 128
Learning rate: 0.001
Truncated normal mean: 0.0
Truncated normal standard deviation: 0.1
Loss optimization algorithm: Adam

These values were chosen based on recommendations given in the coursework. I ran the pipeline with different preprocessing steps and chose the preprocessing step which
performed better. Since my focus for this project was to get a clear understanding of the various components described in the LeNet and have a correct implementation , I  chose the parameters mentioned in the coursework

Performance numbers
My final model results were:

* validation set accuracy of
* test set accuracy of

Test a Model on New Images

Here are five German traffic signs that I found on the web:

![alt text][image5] ![alt text][image6] ![alt text][image7]
![alt text][image8] ![alt text][image4]


Here are the results of the prediction:

| Image			        |     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| Keep left     			| Keep left 										|
| Roundabout mandatory					| Roundabout mandatory											|
| Go straight or left	      		| Go straight or left					 				|
| End of all speed and passing limits			| Keep right      							|
| Bumpy Road      		| No passing for vehicles over 3.5 metric tons sign   									|


The model was able to correctly guess 3 of the 5 traffic signs, which gives an accuracy of 80%.
The top classes predicted by the network is shown in the Ipython notebook

For the first image, the model is wrongly sure that this is a - No passing for vehicles over 3.5 metric tons sign (probability of 0.9).

| Probability         	|     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| .99         			|Vehicles over 3.5 metric tons prohibited   									|
| .01     				| No passing 										|
| .0					| Slippery road											|
| .0	      			| Right-of-way at the next intersection					 				|
| .0				    | No entry      							|


For the second image, the model is  sure that this is a - Keep left sign.

| Probability         	|     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| 1.0         			| Keep left   									|
| .0     				| Turn right ahead 										|
| .0					| Go straight or left											|
| .0	      			| Road work					 				|
| .0				    | Speed limit (20km/h)      							|


For the third image, the model is sure that this is a - Roundabout mandatory sign.

| Probability         	|     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| 1.0         			| Roundabout mandatory   									|
| .0     				| Vehicles over 3.5 metric tons prohibited 										|
| .0					| End of no passing											|
| .0	      			| Speed limit (80km/h)					 				|
| .0				    | Ahead only      							|

For the fourth image, the model is  sure that this is a - Go straight or left sign.

| Probability         	|     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| 1.0         			| Go straight or left   									|
| .0     				| Keep left 										|
| .0					| Ahead only											|
| .0	      			| Speed limit (50km/h)					 				|
| .0				    | Roundabout mandatory      							|


For the fifth image, the model is  sure that this is a - End of all speed and passing limits

| Probability         	|     Prediction	        					|
|:---------------------:|:---------------------------------------------:|
| .6         			| End of all speed and passing limits   									|
| .4     				| Priority road 										|
| .0					| End of speed limit (80km/h)											|
| .0	      			| Speed limit (60km/h)					 				|
| .0				    | Right-of-way at the next intersection      							|
