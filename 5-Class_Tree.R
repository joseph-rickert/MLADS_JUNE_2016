# title: 5 - Classification Tree
# author: Joseph Rickert
# date: "May 13, 2016"

# This script presents a binary classification problem using a cell segmentation data 
# set that is included in the package. The data is described in the paper: 
# Hill et al "Impact of image segmentation on high-content screening 
# data quality for SK-BR-3 cells" BMC fioinformatics (2007) vol 8 (1) pp. 340
# 

### Background
# "Well-segmented"" cells are cells for which location and size may be 
# accurrately detremined through optical measurements. Cells that are 
# not Well-segmented (WS) are said to be "Poorly-segmented"" (PS). 
# Given a set of optical measurements can we predict which cells will be PS? 


## Load required packages
library(caret)        # just for confusion matrix
library(corrplot)			# plot correlations
library(rattle)       # Plot Tree
library(rpart)				# CART Decision Trees
library(ROCR)         # Plot the ROC Curve

#
# Function to divide data into training, and test sets 
splitData <- function(data=dF,pctTrain=0.7)
{
  # fcn to create indices to divide data into random 
  # training, validation and testing data sets
  N <- nrow(data)										
  trainInd <- sample(N, pctTrain*N)								
  testInd <- setdiff(seq_len(N),trainInd)	
  indexList <- list(trainInd,testInd)
  return(indexList)
} 

## Read the Data and Prepare the Training and Test Sets
# Get the weather data and select the subset for modeling

set.seed(42)									 # Set seed for simulation repeatability

data(segmentationData)  	# Load the segmentation data set
head(segmentationData)
dF <- segmentationData[,-c(1,2)]  #Remove non-predictive variables
head(dF,2)
rm(segmentationData)     # Free up some memory space

indices <- splitData(dF)       # Generate indidices to split data
trainInd <- unlist(indices[1]) # Training data
testInd <- unlist(indices[2])  # Test data

dim(dF[trainInd,])
dim(dF[testInd,])
#
# Explore the data
sapply(dF,summary) 
sapply(dF,class)

# Look at the correlations for the independent variables
corrplot(cor(dF[,-1]), 
         method="ellipse",
         tl.pos="n",
         title="Correlation of segmentationData Predictor Variables")

## Build a Tree Model with rpart   

# The rpart algorithm based on recursive partitioning       
# (See section 11.2 of Data Mining with Rattle and R by williams)   
# The rpart Algorithm:  
#   1. -   Partition the data set according to some criterion of "best" partition   
#   2. -   Do the same for each of the two new subsets   
#   3. -  Once a partition is made, stick with it (greedy approach)   
# 
# Measures of "best" partition:   
#   1. -    information gain (the default)   
#   2. -    Gini   
# 
# Information Gain Algorithm:   
#   For all possible splits (partitions)   
#    1. -    Split data, D, into to subsets S1 and S2 where D = S1 U S2   
#    2. -    Calculate information I1 and I2 associated with S1 and S2   
#    3. -    Compute total information of split: Info(D,S1,S2) = (|D1|/D)*I1 + (|D2|/|D|)*I2   
#    4. -    Compute the information gain of the split: info(D) - info(D,S1,S2)   
#    5. -    Select split with greatest information gain

### Build a classification tree model   
# Set up the control function for the tree
ctrl <- rpart.control(minsplit = 20,      # Min obs at node for splitting
                      cp = 0.05,          # Complexity parameter
                      maxcompete = 4,     # Num. competetitor splits retained
                      maxsurrogate = 5,   # Num. surrogate splits retained
                      usesurrogate = 2,   # How to use surrogate splits
                      xval = 10,          # Num. of cross validations
                      surrogatestyle = 0, # How to select surrogates
                      maxdepth = 30       # Max depth of tree
)


form <- formula(Class~ .)				# Describe the model to R
tree_fit <- rpart(formula=form,
              data=dF[trainInd,],
              control = ctrl)	        # Build the model
tree_fit	

### Interpreting the Model Results     

# Every line of the output the follows will have   
#  1. node: a node number   
#  2. split: the logic for how the node splits the data   
#  3. n: the number of observations considered at that split    
#  4. loss: the number of incorrectly classified observations   
#  5. the majority class at that node   
#  6. yprob: the distribution of classes at that node   
# 
# So for the second line above: Pressure3pm>=1011.9 204 16 No (0.92156863 0.07843137)   
#  1. node: 2)      
#  2. split: if ToralIntenCh2 < 47253.5 go left down tree   
#  3. n: 659 obversations went down this branch      
#  4. loss: 57 misclassified observations   
#  5. Most observations were PS   
#  6. 91% of obs have target var PS, 9% are WS   

### Examine the results    
summary(tree_fit)             # Summary of rpart tree model


### Plot the Tree
# First a basic tree plot and then a fancy one
fancyRpartPlot(tree_fit)         # a fancy plot


### Evaluate model performance on the test set 
# Run the tree model on the test set 
pr <- predict(tree_fit, dF[testInd, ], type="class")

response <- dF[testInd,1] # Actual responses
# Calculate the Confusion Matrix
confusionMatrix(data=pr,reference=response)      


### Draw the ROC Curve
#Refit
prob <- predict(tree_fit, dF[testInd, ], type="prob")

pred <- prediction(prob[,1],response,label.ordering=c("WS","PS"))
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10),
     xlim = c(0,1),
     ylim = c(0,1),
     xlab="False Positive Rate",
     ylab="True Positive Rate",
     main="ROC Curve")


### Explore an unpruned tree
# The complexity parameter sets the minimum benefit that must be 
# gained at each split of the decision tree. (default = .01) Typical behavior with cp=0 is to see the error reate decrease at first and then begin to increase.


ctrl2 <- rpart.control(minsplit = 0,  # Min obs at node for splitting
                       cp = 0,     # Complexity parameter
                       maxcompete = 4, # No. competor splits in retained
                       maxsurrogate = 5, # No. surrogate splits retained
                       usesurrogate = 2, # How to use surrogate splits
                       xval = 10,        # No. of cross validations
                       surrogatestyle = 0, # How to select surrogates
                       maxdepth = 30       # Max depth of tree
)


big_tree <- rpart(formula=form,control=ctrl2,data=dF[trainInd,])
print(big_tree$cptable)

plotcp(big_tree)


fancyRpartPlot(big_tree)         # a fancy plot


