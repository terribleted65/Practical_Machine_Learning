#Load Libraries
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(corrgram)
library(rattle)
#
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to 
collect a large amount of data about personal activity relatively inexpensively. 
These type of devices are part of the quantified self movement - a group of 
enthusiasts who take measurements about themselves regularly to improve their 
health. One thing that people regularly do is quantify how much of a particular 
activity they do, but they rarely quantify how well they do it. In this project, 
I will use data from accelerometers on the belt, forearm, arm, and dumbell of 
6 participants.  

The participants were asked to perform one set of 10 repetitions of the
Unilateral Dumbbell Biceps Curl in five different fashions: exactly according 
to the specification (Class A), throwing the elbows to the front (Class B), 
lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway 
(Class D) and throwing the hips to the front (Class E).  Class A corresponds to 
the specified execution of the exercise, while the other 4 classes correspond to 
common mistakes. 

The data was previously downloaded.  I read the data and found that both datasets
had 160 columns/variables and the training set had 19,622 rows where the testing set
had 20 rows. Since many columns had missing data (null, blank, or NA), I normalized
the missing values to be NA while reading the files for easier data cleaning.
#
testing_data <- read.csv("pml-testing.csv",na.strings=c(""," ","NA"))
dim(testing_data)
#
training_data <- read.csv("pml-training.csv",na.strings=c(""," ","NA"))
dim(training_data)
#
As it is important when building a model to have only data in your dataframe that
corresponds to the outcome (variable=casse).  I removed the columns that had data 
but had no correlation to calculating the outcome variable.
#
testing_data <- testing_data[,-c(1,3:7)]
training_data <- training_data[,-c(1,3:7)]
#
I ran a random forest model against this data and was only able to gain an accuracy
of 79.1%. Even though many columns have no data or NA, they must be impacting the 
accuracy of the model.  Because of this possibility, I identified and only kept 
the remaining columns that have data.  Now the testing and training dataframes 
only have 54 columns each.
#
test_data_clean <- testing_data[, colSums(is.na(testing_data)) == 0] 
dim(test_data_clean)
#
train_data_clean <- training_data[, colSums(is.na(training_data)) == 0] 
dim(train_data_clean)
#
I then split the remaining training dataframe into a training set (70%) and 
a testing or valdation dataset (30%). 
#
set.seed(888) 
inTrain <- createDataPartition(train_data_clean$classe, p=0.70, list=FALSE)
train_data <- train_data_clean[inTrain, ]
test_data <- train_data_clean[-inTrain, ]
#
I decided to use the Random Forests algorithm to develop the predictive model 
because it is one of the best among classification algorithms and is able to
to classify large amounts of data with accuracy and is relatively efficient in
doing so.

In testing different random forest model options, I increased the folds and was 
able to produce small improvements in the accuracy.  As the improvements were small,
I settled on 10 folds (gained +0.3% going from 5 to 10 folds).  Because the random
forest model takes so long to execute and that the number of trees default is 500, 
I started with 250 trees and I kept reducing the number of trees down to
gain performance.  The accuracy loss from 250 trees to 50 was quite small (less than
(0.09%).  The model with 10 folds and only 50 trees provided an accuracy of 
99.42% with a significant p-value of 2.2e-16 and a very small OOB error rate of 0.86%.
#
random_forest_mod <- train(classe ~ ., data=train_data, method="rf",ntree=50,
          trControl=trainControl(method = "cv",number = 10,allowParallel=TRUE))
#
random_forest_mod$finalModel
random_forest_pred <- predict(random_forest_mod, test_data)
confusionMatrix(test_data$classe, random_forest_pred)
#
I will now take and apply our cross validated model against the provided test
test dataset.
#
test_forest_mod <- predict(random_forest_mod, test_data_clean)
test_forest_mod

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(test_forest_mod)


#Appendix: 

Correlation Matrix Visualization:
I used the corrgram function to provide a what I feel is an easier to 
interpret plot of the correlation between variables.  On the left of the diagnol
the correlations are strongest when the result is dark blue and weakest when it 
is dark red and varies in the range between.  To the right of the diagnol the pies
that have the majority filled in with blue are the strongest correlations and the 
weakest correlations are the full pies of red with the correlation varying between 
these two extremes.
#
corrgram(train_data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Training Data") 
#
Simplified Decision Tree Visualizaiton
fan_dec_tree_viz <- train(classe ~ ., data=train_data, method="rpart")
fancyRpartPlot(fan_dec_tree_viz$finalModel)
#
Detailed Decision Tree Visualization
dec_tree_viz <- rpart(classe ~ ., data=train_data, method="class")
prp(dec_tree_viz,type=3,tweak=1.2,main="Training Data") 
#