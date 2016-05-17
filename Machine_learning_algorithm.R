#Author:Jerry
#E-mail:jerry@mail.ntust.edu.tw


########################################################
#Supervised Learning
########################################################
#######################################
#Linear Regression
#######################################
x_train <- c(2,2.6,5.4,4.2,3.2,6.6,1.5,3.8)
y_train <- c(3.4,4.8,6.2,5,4.2,6.5,4.6,5.3)
lm_test<-as.data.frame(c(1,2.6,1.4,1.2,1.2,2.6,2.5,3.8))
train_data<-as.data.frame(cbind(x_train,y_train))


#Check Score 
(lm_model<-lm(y_train~x_train, data=train_data))
summary(lm_model)
plot(x_train,y_train) #Plot x,y
lm_abline<-lm_model$coefficients
abline(a=lm_abline[1],b=lm_abline[2])

#Predict Output
predict(lm_model, newdata=lm_test)
plot(x_train,predict(lm_model, newdata=lm_test))

#######################################
#Linear regression for iris datasets
#######################################

lm_iris<-iris
ind <- sample(2, nrow(lm_iris), replace=TRUE, prob=c(0.7, 0.3))
lm_trainingData <- lm_iris[ind==1,]
lm_testingData <- lm_iris[ind==2,] 

#Pairs Scatter Plos 
pairs(iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
#Correlation Coefficient
cor(lm_trainingData$Petal.Length,lm_trainingData$Petal.Width)

(lm_iris_model<-lm(Petal.Length~., data=lm_trainingData))
summary(lm_iris_model)
lm_iris_model_plot<-lm(Petal.Length ~ Petal.Width, data=lm_trainingData)
#plot(lm_iris_model)

#Predict Output
Prd_iris<-predict(lm_iris_model, newdata=lm_testingData)
cbind(lm_testingData,Prd_iris)

#plot
plot(Petal.Length ~ Petal.Width, data = iris)
abline(lm_iris_model_plot)

library(ggplot2)
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#######################################
#Logistic Regression for iris datasets
#######################################
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
lr_trainData <- iris[ind==1,]
lr_testData <- iris[ind==2,]

newcol = data.frame(isSetosa=(lr_trainData$Species == 'setosa'))
lr_trainData <- cbind(lr_trainData, newcol)

(lr_model <- glm(isSetosa ~ ., family = binomial(logit), data=lr_trainData))
lr_model_plot <- glm(isSetosa ~ Petal.Width, family = binomial(logit), data=lr_trainData)
#plot(lr_model)

#Predict Output
Prd_iris_lr <- predict(lr_model, newdata=lr_testData, type='response')
pred_lris_out<-round(Prd_iris_lr, 3)

lr_result<-cbind(lr_testData,pred_lris_out)
lr_result

#plot
plot(lr_trainData$Petal.Width, lr_trainData$isSetosa)
curve(predict(lr_model_plot, data.frame(Petal.Width=x), type="response"), add=TRUE)   


#######################################
#Decision Tree with CHAID(Ctree)
#Example Available for MRO
#######################################

library(party)
library(partykit)

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

iris_ctree <- ctree(Species ~ ., data=trainData)

#Check Score 
table(predict(iris_ctree), trainData$Species)
iris_ctree
#Plot Tree
plot(iris_ctree)

#Validation model for trainingdata
at1<-trainData[which(ind==1),"Species"]
d1<-predict(iris_ctree,newdata=trainData)
#Confusion matrix
table(predict(iris_ctree,newdata=trainData), d1)
#Overall accuracy
(iris_ctree_acc_tr <- sum(d1 == at1, na.rm=TRUE)/length(at1))
#Overall error
(iris_ctree_err_tr <- sum(d1 != at1, na.rm=TRUE)/length(at1))


#Validation model for testingdata
at2<-testData$Species
d2<-predict(iris_ctree,newdata=testData)
#Confusion matrix
table(predict(iris_ctree,newdata=testData), d2)
#Overall accuracy
(iris_ctree_acc_te <- sum(d2 == at2, na.rm=TRUE)/length(at2)) * 100
#Overall error
(iris_ctree_err_te <- sum(d2 != at2, na.rm=TRUE)/length(at2)) * 100

#######################################
#Random Forest for iris datasets
#######################################
library(randomForest)
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
rf_trainData <- iris[ind==1,]
rf_testData <- iris[ind==2,]

rf_model <- randomForest(Species~.,data=rf_trainData,ntree=1000)
table(predict(rf_model),rf_trainData$Species)

rf_model
plot(rf_model)
#MeanDecreaseGini
importance(rf_model)
#Visualization of Gini
varImpPlot(rf_model)

#Predict Output
at2_rf<-rf_testData$Species
Pred_rf<-predict(rf_model,newdata=rf_testData)
table(Pred_rf, rf_testData$Species)
cbind(rf_testData,Pred_rf)

(iris_rf_acc <- sum(Pred_rf == at2_rf, na.rm=TRUE)/length(at2_rf)) * 100
(iris_ctree_err_te <- sum(Pred_rf != at2_rf, na.rm=TRUE)/length(at2_rf)) * 100

#######################################
#K-nearest neighbors for iris datasets
#######################################
install.packages("kknn")
library(kknn)

ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
knn_trainData <- iris[ind==1,]
knn_testData <- iris[ind==2,]

knn_model <- kknn(formula = formula(Species~.), train = knn_trainData, test = knn_testData, k = 7, distance = 1)

(fit <- fitted(knn_model))
table(knn_testData$Species, fit)



########################################################
#Unsupervised Learning
########################################################
#######################################
#Apriori  Algorithm
#######################################
library("arules")
library("arulesViz")

require(arules)
data("Adult")
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
inspect(head(rules))
#AdultUCI[1:2,]

#######################################
#K-means neighbors for iris datasets
#######################################
kmeans_iris<-iris
#kmeans(numeric matrix of data ,either the number of clusters)
kmeans_iris_model <- kmeans(kmeans_iris[,1:4], 4) 

#Compare the Species label with the clustering result
table(kmeans_iris$Species, kmeans_iris_model$cluster)

#Plot
#http://www.statmethods.net/advgraphs/parameters.html
plot(kmeans_iris[c("Sepal.Length", "Sepal.Width")], col=kmeans_iris_model$cluster)
plot(kmeans_iris[c("Sepal.Length", "Sepal.Width")], col=kmeans_iris_model$cluster,cex=1,pch=18)
points(kmeans_iris_model$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, cex=5, pch=8)

#output 
kmeans_iris_model $cluster
(kmeans_iris_result<-cbind(1:150,kmeans_iris$Species))


#######################################
#Hierarchical Clustering for iris datasets
#######################################
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL

#average-linkage agglomerative algorithm
hc_model <- hclust(dist(irisSample), method="ave")
plot(hc_model, hang = -1, labels=iris$Species[idx])


########################################################
#Semi-Supervised Learning
########################################################
#######################################
#Self-training for iris datasets
#######################################
#intall.packages("DMwR")
#library(DMwR)
## Dividing the data set into train and test sets
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]

## Learn a tree with the full train set and test it
stdTree <- rpartXse(Species~ .,tr,se=0.5)
table(predict(stdTree,ts,type='class'),ts$Species)

## Now let us create another training set with most of the target
## variable values unknown
trSelfT <- tr
nas <- sample(100,70)
trSelfT[nas,'Species'] <- NA

## Learn a tree using only the labelled cases and test it
baseTree <- rpartXse(Species~ .,trSelfT[-nas,],se=0.5)
table(predict(baseTree,ts,type='class'),ts$Species)

## The user-defined function that will be used in the self-training process
f <- function(m,d) { 
  l <- predict(m,d,type='class')
  c <- apply(predict(m,d),1,max)
  data.frame(cl=l,p=c)
}

## Self train the same model using the semi-superside data and test the
## resulting model
treeSelfT <- SelfTrain(Species~ .,trSelfT,learner('rpartXse',list(se=0.5)),'f')
table(predict(treeSelfT,ts,type='class'),ts$Species)

########################################################
#Reinforcement Learning)
########################################################
#######################################
#MDPtoolbox
#######################################
install.packages("MDPtoolbox")
library(MDPtoolbox)

# Generates a random MDP problem
set.seed(0)
mdp_example_rand(2, 2)
mdp_example_rand(2, 2, FALSE)
mdp_example_rand(2, 2, TRUE)
mdp_example_rand(2, 2, FALSE, matrix(c(1,0,1,1),2,2))

# Generates a MDP for a simple forest management problem
MDP <- mdp_example_forest()

# Find an optimal policy
results <- mdp_policy_iteration(MDP$P, MDP$R, 0.9)

# Visualise the policy
results$policy

#######################################
#iqLearn
#######################################
install.packages("iqLearn")
library(iqLearn)

## load in two-stage BMI data
data (bmiData)
bmiData$A1[which (bmiData$A1=="MR")] = 1
bmiData$A1[which (bmiData$A1=="CD")] = -1
bmiData$A2[which (bmiData$A2=="MR")] = 1
bmiData$A2[which (bmiData$A2=="CD")] = -1
bmiData$A1 = as.numeric (bmiData$A1)
bmiData$A2 = as.numeric (bmiData$A2)
s1vars = bmiData[,1:4]
s2vars = bmiData[,c (1, 3, 5)]
a1 = bmiData[,7]
a2 = bmiData[,8]
## define response y to be the negative 12 month change in BMI from
## baseline
y = -(bmiData[,6] - bmiData[,4])/bmiData[,4]
## second-stage regression
fitIQ2 = learnIQ2 (y ~ gender + parent_BMI + month4_BMI +
                     A2*(parent_BMI + month4_BMI), data=bmiData, "A2", c("parent_BMI",
                                                                         "month4_BMI"))
summary (fitIQ2)
## model conditional expected value of main effect term
fitIQ1main = learnIQ1main (~ gender + race + parent_BMI + baseline_BMI
                           + A1*(gender + parent_BMI), data=bmiData, "A1", c ("gender",
                                                                              "parent_BMI"), fitIQ2)
## model conditional mean of contrast function
fitIQ1cm = learnIQ1cm (~ gender + race + parent_BMI + baseline_BMI +
                         A1*(gender + parent_BMI + baseline_BMI), data=bmiData, "A1", c
                       ("gender", "parent_BMI", "baseline_BMI"), fitIQ2)
## variance modeling
fitIQ1var = learnIQ1var (~ gender + race + parent_BMI + baseline_BMI +
                           A1*(parent_BMI), data=bmiData, "A1", c ("parent_BMI"), "hetero",
                         fitIQ1cm)
## new patient
h1 = c (1, 1, 30, 35)
optIQ1 = IQ1 (fitIQ1main, fitIQ1cm, fitIQ1var, "nonpar", h1, h1, h1)
optIQ1$q1opt


########################################################
#Deep Learning
########################################################
#######################################
#Neural Network
#######################################

#install.packages("neuralnet")
library("neuralnet")
#計算平方根

#Generate 50 random numbers uniformly distributed between 0 and 100
traininginput <-  as.data.frame(runif(50, min=0, max=100))
# sqrt(x) computes the (principal) square root of x
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Run the neural network
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)

#######################################
#Deep Learning
#Deep Neural Network
#######################################
# 1. split data into test/train
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

# dimension: 2X2
input <- matrix(1:4, nrow=2, ncol=2)
# dimension: 2x3
weights <- matrix(1:6, nrow=2, ncol=3)
# dimension: 1*3
bias <- matrix(1:3, nrow=1, ncol=3)

s1 <- input %*% weights + matrix(rep(bias, each=2), ncol=3) 
max(0, s1)
pmax(0, s1)

#Example with iris dataset
library(nnet)
ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                  species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
ir.nn2 <- nnet(species ~ ., data = ird, subset = samp, size = 6, rang = 0.1,
               decay = 1e-2, maxit = 2000)

labels.nnet <- predict(ir.nn2, ird[1:20,])
table(ird$species[-samp], labels.nnet)

