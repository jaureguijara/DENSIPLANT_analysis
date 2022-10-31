library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(car)
library(Hmisc)
library(stringr)
library(corrplot)
library(randomForest)
library(rpart)
library(ggfortify)
library(partykit)
library(caret)
library(rattle)

if(Sys.info()["user"] == "Jara"){
  in.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/outputs/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs/figures")
}

setwd(in.dir)
df <- read.csv("combined_densiplant_dataset.csv", header = T)
df <- df[,-1]
df <- arrange(df, dens)
df$dens <- as.character(df$dens)

######## 1. RANDOM FORESTS ########

## 1.1 USING PC ## nice results #####  per date & altitude data subset, not including variety or flight altitude

densorder <- c("35","70","140", "280", "560")

set.seed(1289073891) 
rf_results <- NULL

conf_matrices <- list()
references <- list()

k <- 1
for(i in unique(df$Altitude)){
  data <- df[df$Altitude == i, ]
  print(i)
  
  for(j in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[j]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$dens <- factor(subset_data$dens, levels = densorder)
    
    # Train/test split
    
    split1<- sample(c(rep(0, 0.70 * nrow(subset_data)), rep(1, 0.30 * nrow(subset_data))))
    train <- subset_data[split1 == 0, ]
    
    train_pca <- train[,8:ncol(train)]
    
    test <- subset_data[split1 == 1, ]
    test_pca <- test[,8:ncol(test)]
    
    # PCA
    pca <- prcomp(train_pca, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    train.data <- data.frame(density = train$dens, pca$x[,1:PC_selected])
    
    test.data <- predict(pca, newdata = test_pca)
    test.data <- data.frame(test.data[,1:PC_selected])
    
    # RANDOM FOREST
    
    rf <- randomForest(density~., data = train.data, proximity=TRUE, ntree = 10000)
    
    # print(rf)
    
    # Just to check that values are classified correctly, accuracy should be = 1
    # p1 <- predict(rf, train.data)
    # cm1 <- confusionMatrix(p1, train.data$density)
    # print(cm1$overall[c(1,6)]) # accuracy should be 1 for the training set
    
    p2 <- predict(rf, newdata = test.data)
    cm2 <- confusionMatrix(p2, test$dens, mode = "everything")
    
    
    conf_matrices[[k]] <- cm2
    names(conf_matrices)[k] <- paste(i, current_date, sep = "_")
    
    rf_result <- c(cm2$overall[c(1,6)], current_date, i)
    
    rf_results <- rbind(rf_results, rf_result)
    
    k <- k +1
    
  }
  
}

setwd(paste(out.dir, "Exploration/RandomForests", sep = "/"))
write.csv(rf_results, "Accuracy_RF_density_prediction.csv")

## 1.2 USING PC 2 ## IN THIS CASE FLIGHT ALTITUDE AND VARIETY ARE INCLUDED IN THE PCA

densorder <- c("35","70","140", "280", "560")

set.seed(1289073891) 
rf_results <- NULL

conf_matrices <- list()
references <- list()
data <- df
k <- 1

for(j in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[j]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$Altitude <- as.numeric(as.factor(subset_data$Altitude))
  subset_data$dens <- factor(subset_data$dens, levels = densorder)
  subset_data$dens <- as.factor(subset_data$dens)
  subset_data$var <- as.numeric(as.factor(subset_data$var))
  
  # PCA
  
  split1<- sample(c(rep(0, 0.70 * nrow(subset_data)), rep(1, 0.30 * nrow(subset_data))))
  train <- subset_data[split1 == 0, ]
  
  test <- subset_data[split1 == 1, ]
  
  
  if(length(unique(subset_data$Altitude))>1){
    train_pca <- train[,c(3,4,8:ncol(train))]
    test_pca <- test[,c(3,4,8:ncol(test))]
  }
  
  else{
    train_pca <- train[,c(4,8:ncol(train))]
    test_pca <- test[,c(4,8:ncol(test))]
  }
  
  
  pca <- prcomp(train_pca, center = T, scale. = T)
  eigenval <- pca$sdev ^ 2
  
  PC_selected <- length(eigenval[eigenval > 1])
  
  train.data <- data.frame(density = train$dens, pca$x[,1:PC_selected])
  
  test.data <- predict(pca, newdata = test_pca)
  test.data <- data.frame(test.data[,1:PC_selected])
  
  # RANDOM FOREST
  
  rf <- randomForest(density~., data = train.data, proximity=TRUE, ntree = 10000)
  
  # print(rf)
  
  # Just to check that values are classified correctly, accuracy should be = 1
  # p1 <- predict(rf, train.data)
  # cm1 <- confusionMatrix(p1, train.data$density)
  # print(cm1$overall[c(1,6)]) # accuracy should be 1 for the training set
  
  p2 <- predict(rf, newdata = test.data)
  cm2 <- confusionMatrix(p2, test$dens, mode = "everything")
  
  
  conf_matrices[[k]] <- cm2
  names(conf_matrices)[k] <- current_date
  
  rf_result <- c(cm2$overall[c(1,6)], current_date)
  
  rf_results <- rbind(rf_results, rf_result)
  
  k <- k +1
  
}

# we don't see an increase in accuracy when including altitude and variety in the PCA for:
# 03/09 and 03/22, but we do see increase in accuracy for 04/05 and 04/19

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 2.  DECISSION (CLASSIFICATION) TREES ########

## 2.1 Separation per date only, including variety and flight height as predictor variables ###

densorder <- c("35","70","140", "280", "560")
data <- df
set.seed(123)

results_df <- NULL

for(j in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[j]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$dens <- as.factor(as.character(subset_data$dens))
  subset_data$var <- as.factor(subset_data$var)
  subset_data$Altitude <- as.factor(subset_data$Altitude)
  
  if(length(unique(subset_data$Altitude))>1){
    tree_data <- subset_data[,-c(1,2,6,7)]
    
  }
  
  else{
    tree_data <- subset_data[,-c(1,2,6,7,3)]
  }
    
  tree_data$dens <- factor(subset_data$dens, levels = densorder)

  
  # Create a trainControl object to control how the train function creates the model
  train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                number = 10,             # Use 10 partitions
                                repeats = 10)            # Repeat 10 times
  
  #Set required parameters for the model type we are using**
  tune_grid = expand.grid(cp=c(0.001))
  
  
  # Use the train() function to create the model
  validated_tree <- train(dens ~. ,
                          data= tree_data,                  # Data set
                          method="rpart",                     # Model type(decision tree)
                          trControl= train_control,           # Model control options
                          tuneGrid = tune_grid,               # Required model parameters
                          maxdepth = 5,                       # Additional parameters***
                          minbucket= 5)

  alt <- unique(subset_data$Altitude)
  
  if(length(alt) == 2){
    alt <- "15-30m"
  }
  
  if(length(alt) == 3){
    alt <- "15-30-50m"
  }
  
  results <- data.frame(validated_tree$results)
  results$date <- current_date
  results$alt <- alt
  
  results_df <- rbind(results_df, results)
  
  fancyRpartPlot(validated_tree$finalModel, 
                 main = paste(current_date, alt, sep = " "))
}


for(j in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[j]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$dens <- factor(subset_data$dens, levels = densorder)
  subset_data$Altitude <- as.numeric(substring(subset_data$Altitude,1, nchar(subset_data$Altitude)-1))
  
  split1<- sample(c(rep(0, 0.70 * nrow(subset_data)), rep(1, 0.30 * nrow(subset_data))))
  train <- subset_data[split1 == 0, ]
  test <- subset_data[split1 == 1, ]
  
  train.data <- train[,c("dens", "var", "Altitude", "Hue", "GGA", "NGRDI", "TGI")]
  test.data <- test[,c("var", "Altitude", "Hue", "GGA", "NGRDI", "TGI")]
  
  rpart.model <- rpart(as.character(dens) ~ .,data = train.data, method = "class")
  
  rpart.prediction <- predict(rpart.model, newdata = test.data, method = "class")
  pred <- apply(rpart.prediction ,1,function(xx)head(names(sort(xx, decreasing=T)),1))
  
  plot(rpart.model, uniform=TRUE,
       main=paste("Classification Tree for Density", current_date, sep = "_"))
  text(rpart.model, use.n=TRUE, all=TRUE, cex=.8)
  
  table_mat <- table(test$dens, pred)
  print(table_mat)
  
  table_df <- data.frame(table_mat)
  table_df$Var1 <- as.numeric(as.character(table_df$Var1))
  table_df$pred <- as.numeric(as.character(table_df$pred))
  
  bullseye <- sum(table_df[table_df$Var1 == table_df$pred, "Freq"])
  
  accuracy_test <- bullseye / sum(table_mat)
  print(accuracy_test)
  
}
# results are not good  

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 3. CONDITIONAL INFERENCE TREES  ########

## 3.1  Tried including variety and flight height as factors but did not result in any improvement

densorder <- c("35","70","140", "280", "560")
data <- df
for(i in unique(df$Altitude)){
  data <- df[df$Altitude == i,]
  
  for(j in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[j]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$Altitude <- as.factor(subset_data$Altitude)
    subset_data$dens <- factor(subset_data$dens, levels = densorder)
    
    
    split1<- sample(c(rep(0, 0.70 * nrow(subset_data)), rep(1, 0.30 * nrow(subset_data))))
    train <- subset_data[split1 == 0, ]
    train.data <- train[,c(5,8:ncol(train))]
    test <- subset_data[split1 == 1, ]
    test.data <- test[,8:ncol(test)]
    
    train.model <- ctree(dens ~ ., data = train.data)
    prediction <- predict(train.model, newdata = test.data)
    
    print(confusionMatrix(prediction,test$dens, mode = "everything"))
    plot(ConInfTree, main = paste(current_date, i, sep = "_"))
  }
  
  
}

## 3.2 CONDITIONAL INFERENCE TREES WITH PC  including flight height and variety for the PCA#

densorder <- c("35","70","140", "280", "560")
data <- df

for(j in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[j]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$Altitude <- as.numeric(as.factor(subset_data$Altitude))
  subset_data$dens <- factor(subset_data$dens, levels = densorder)
  subset_data$dens <- as.factor(subset_data$dens)
  subset_data$var <- as.numeric(as.factor(subset_data$var))
  
  
  if(length(unique(subset_data$Altitude))>1){
    pca_data <- subset_data[,c(3,4,8:ncol(subset_data))]
  }
  else{
    pca_data <- subset_data[,c(4,8:ncol(subset_data))]
  }
  
  pca <- prcomp(pca_data, center = T, scale. = T)
  eigenval <- pca$sdev ^ 2
  
  PC_selected <- length(eigenval[eigenval > 1])
  
  tree.data <- data.frame(density = as.factor(subset_data$dens), pca$x[,1:PC_selected])  
  
  ConInfTree <- ctree(density ~ ., data = tree.data)
  plot(ConInfTree, main = current_date)
}

# 3.3  CONDITIONAL INFERENCE TREES WITH PC NOT including flight height and variety for the PCA#

densorder <- c("35","70","140", "280", "560")

for(i in unique(df$Altitude)){
  data <- df[df$Altitude == i,]
  
  for(j in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[j]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$Altitude <- as.numeric(as.factor(subset_data$Altitude))
    subset_data$dens <- factor(subset_data$dens, levels = densorder)
    
    pca_data <- subset_data[,c(8:ncol(subset_data))]
    pca <- prcomp(pca_data, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    tree.data <- data.frame(density = as.factor(subset_data$dens), pca$x[,1:PC_selected])  
    
    ConInfTree <- ctree(density ~ ., data = tree.data)
    plot(ConInfTree, main = paste(current_date, i, sep = "_"))
  }
  
}


