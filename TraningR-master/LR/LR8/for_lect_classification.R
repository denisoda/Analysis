library(ggplot2) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(magrittr)
library(h2o)

localH2O = h2o.init(nthreads=-1) # -1 means use all CPUs on the host (Default)

# h2o.removeAll() # Clean slate - just in case the cluster was already running

# Load data
train <- read.csv("data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", stringsAsFactors=FALSE)
full  <- bind_rows(train, test) # bind training & test data

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "Survived")
  fea <- data[,features]
  factors <- c("Pclass",
               "Sex",
               "Embarked",
               "Survived")
  fea %<>% mutate_at(factors, funs(as.factor))
  return(fea)
}

full <- extractFeatures(full)

# Show number of missing values for features
(y <- as.data.frame(lapply(full[1:7], function(x) sum(is.na(x)|x==""))))

# Show observation with missing Fare
full[is.na(full$Fare),]

# Impute missing Fare
imp_Pclass <- full$Pclass[is.na(full$Fare)]
imp_Embarked <- full$Embarked[is.na(full$Fare)]

full$Fare[is.na(full$Fare)] <- 
  median(full[full$Pclass == imp_Pclass & full$Embarked == imp_Embarked, ]$Fare, na.rm = TRUE)

# Impute missing Embarked
(embark_fare <- full %>% filter(Embarked == ""|is.na(Embarked)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

full$Embarked[full$Embarked == ""] <- 
  getmode(full[full$Pclass == '1' & ((full$Fare > 79) & (full$Fare < 80)), ]$Embarked)

# Impute missing Age
set.seed(2017) # Set a random seed

# Perform mice imputation, excluding Survived variable:
mice_mod <- mice(full[, !names(full) %in% 
                        c('Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model
full$Age <- mice_output$Age

# Feature engeneering
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_classic()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

full$FsizeD <- as.factor(full$FsizeD)
full %<>% select(-Fsize)


# Split the data back into a train set and a test set
train <- full[1:891,]
# test <- full[892:1309,]
hex_df <- as.h2o(train)

# Splits data into 2 parts
hex_split <- h2o.splitFrame(hex_df, ratios = c(0.8), 
                            destination_frames = c("train", "test"), seed = 2017)

nfolds <- 5

myX <- c("Pclass","Age","Sex","Parch","SibSp","Fare","Embarked", "FsizeD")

# Train & Cross-validate a GLM
my_glm <- h2o.glm(x = myX, y = "Survived",
               training_frame = hex_split[[1]],
               model_id = "my_glm",
               nfolds = nfolds,
               fold_assignment = "Modulo",
               family = "binomial",
               keep_cross_validation_predictions = TRUE,
               seed = 2017)

# h2o.saveModel(my_glm, "models", force = TRUE)
# glm <- h2o.loadModel("models/my_glm")
# glmmodel.path <- h2o.saveModel(my_glm, "lect_glm")
# glm <- h2o.loadModel(glmmodel.path)

perf_glm_test <- h2o.performance(my_glm, newdata = hex_split[[2]])
glm_auc_test <- h2o.auc(perf_glm_test)
print(sprintf("GLM Test AUC:  %s",glm_auc_test))
pred <- predict(my_glm, df2.hex)


# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = myX,
                          y = "Survived",
                          training_frame = hex_split[[1]],
                          model_id = "my_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 2017)
# h2o.saveModel(my_rf, "models", force = TRUE)
# my_rf <- h2o.loadModel("models/my_rf")

perf_rf_test <- h2o.performance(my_rf, newdata = hex_split[[2]])

rf_auc_test <- h2o.auc(perf_rf_test)

print(sprintf("RF Test AUC:  %s",rf_auc_test))

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = myX,
                  y = "Survived",
                  training_frame = hex_split[[1]],
                  model_id = "my_gbm",
                  distribution = "bernoulli",
                  ntrees = 10,
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 2017)

# h2o.saveModel(my_gbm, "models", force = TRUE)
# my_gbm <- h2o.loadModel("models/my_gbm")

perf_gbm_test <- h2o.performance(my_gbm, newdata = hex_split[[2]])
gbm_auc_test <- h2o.auc(perf_gbm_test)
print(sprintf("GBM Test AUC:  %s",gbm_auc_test))

# Train a stacked ensemble using the GLM, GBM and RF above
my_ensemble <- h2o.stackedEnsemble(x = myX,
                                y = "Survived",
                                training_frame = hex_split[[1]],
                                model_id = "my_ens_bin",
                                base_models = list("my_gbm", "my_rf", "my_glm"))


# Eval ensemble performance on a test set
perf <- h2o.performance(my_ensemble, newdata = hex_split[[2]])
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

