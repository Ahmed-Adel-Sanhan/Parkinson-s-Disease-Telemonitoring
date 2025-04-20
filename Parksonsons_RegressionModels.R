# Import Libraries
library(caret)
library(rpart)
library(caTools)
library(Metrics)
library(pscl) 
library(ROCR) # Gives Prediction

#1. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}

#2. R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

# Parameters
input_filename   <- "Input/parkinsons-1.csv"
run_seed         <- 123

train_method_linear <- "lm"      # Linear Model (lm)
train_method_cart   <- "rpart"   # Classification and Regression Trees (CART)

tc_method = "repeatedcv" # Repeated Cross Validation
tc_method_number = 10  # For repeated cv, this is the number of folds

# Features
dysphonia_parameters <- c("total_UPDRS",
                          "age",
                          "Jitter...",
                          "Jitter.Abs.",
                          "Jitter.RAP",
                          "Jitter.PPQ5",
                          "Jitter.DDP",
                          "Shimmer",
                          "Shimmer.dB.", 
                          "Shimmer.APQ3", 
                          "Shimmer.APQ11", 
                          "Shimmer.DDA", 
                          "NHR", 
                          "HNR", 
                          "RPDE", 
                          "DFA", 
                          "PPE")

# Import Data
pd <- read.csv(input_filename)

# Clean Data
pd <- subset(pd, test_time >= 0)
pd$NHR[pd$subject. == "36"] <- median(pd$NHR, na.rm = TRUE)

# Downselect audio parameters
pd.dysphonia <- pd[dysphonia_parameters]

# Train & Test Models no Cross Validation

# Split data
set.seed(run_seed)
sample <- sample.split(pd.dysphonia$total_UPDRS, SplitRatio = 0.7)
train  <- subset(pd.dysphonia, sample == TRUE)
test   <- subset(pd.dysphonia, sample == FALSE)

# Train Models
regressor_lm    <- lm(   formula = total_UPDRS ~ ., data = train)
regressor_rpart <- rpart(formula = total_UPDRS ~ ., data = train)

# Test Model
lm_predict    <- predict(regressor_lm,    newdata = test)
rpart_predict <- predict(regressor_rpart, newdata = test)

# Get Results
summary(regressor_lm)$r.squared
summary(regressor_rpart)$r.squared

# Calculate MAE and R2
lm_MAE <- mae(test$total_UPDRS,lm_predict)     # Calculate MAE
lm_R2   = RSQUARE(test$total_UPDRS,lm_predict) # Using R2

rpart_MAE <- mae(test$total_UPDRS,rpart_predict)     # Calculate MAE
rpart_R2  <- RSQUARE(test$total_UPDRS,rpart_predict) # Using R2

# Create Data Table
data_table <- as.table(matrix(c(lm_MAE, lm_R2, rpart_MAE, rpart_R2), ncol = 2, byrow=TRUE))
colnames(data_table) <- c("MAE", "R")
rownames(data_table) <- c("Linear Model", "CART")
print(data_table)


# cross Validation BELOW
set.seed(run_seed)
tc <- trainControl(method = tc_method,
                   number = tc_method_number)

# Train the Model
model_lm <- train(total_UPDRS ~., 
                  data      = pd.dysphonia, 
                  trControl = tc,
                  method    = train_method_linear) 
model_cart <- train(total_UPDRS ~., 
                  data      = pd.dysphonia, 
                  trControl = tc, 
                  method    = train_method_cart)

print(model_lm)
print(model_cart)





