require(readr); require(deepnet)

# x is a matrix of multiple predictions coming from different learners

# y is a vector of all output flags

#x_test is a matrix of multiple predictions on an unseen sample

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09242015_1.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09192015_4.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09192015_3.csv")

fourth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09152015_1.csv")

fifth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09192015_2.csv")

sixth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_4.csv")

seventh <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_3.csv")

eigth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_1.csv")

ninth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/eight_2.csv")

tenth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ten.csv")

eleven <- read_csv("D:/kaggle/Springleaf/SUBMISSION/seventh.csv")

twelve <- read_csv("D:/kaggle/Springleaf/SUBMISSION/eight.csv")

x=cbind(first$target[1:145231], second$target[1:145231], third$target[1:145231], 
        
        fourth$target[1:145231], fifth$target[1:145231], sixth$target[1:145231], 
        
        seventh$target[1:145231], eigth$target[1:145231], ninth$target[1:145231], 
        
        tenth$target[1:145231], eleven$target[1:145231], twelve$target[1:145231] 
        
        )


y = train$target

x <- as.matrix(x)

y <- as.numeric(y)


X=cbind(first$target, second$target, third$target, fourth$target, 
        
        fifth$target, sixth$target, seventh$target, eigth$target, 
        
        ninth$target, tenth$target, eleven$target, twelve$target)

X <- as.matrix(X)

nn <- dbn.dnn.train(x ,y , hidden = c(1),
                    
                    activationfun = "sigm",learningrate = 0.2,momentum = 0.8)

nn_predict <- nn.predict(nn,X)

#nn_predict_test <- n.predict(nn,x_test)

sub <- data.frame(ID=test_ID)

sub$target <- nn_predict

write_csv(sub, "D:/kaggle/Springleaf/SUBMISSION/1002015.csv")

