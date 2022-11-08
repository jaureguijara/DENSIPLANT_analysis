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


######## 1.  DECISSION (CLASSIFICATION) CART TREES ########

densorder <- c("35","70","140", "280", "560")

set.seed(12345) 

data <- df


results_df <- NULL
round <- 1

# while(round < 11){
#   
#   print(paste("Round: ", round))
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$dens <- as.factor(as.character(subset_data$dens))
    subset_data$var <- as.numeric(as.factor(subset_data$var))
    subset_data$Altitude <- as.factor(subset_data$Altitude)
    
    ## 1.1 Separation per date only, including flight height as predictor variable ###
    if(length(unique(subset_data$Altitude))>1){
      tree_data <- subset_data[,-c(1,2,6,7)]
      tree_data$Altitude <- as.numeric(tree_data$Altitude)
      
    }
    
    else{
      tree_data <- subset_data[,-c(1,2,3,6,7)]
    }
    
    #Train/test split
    split1<- sample(c(rep(0, 0.80 * nrow(tree_data)), rep(1, 0.20 * nrow(tree_data))))
    
    train <- tree_data[split1 == 0, ]
    train_nodens <- dplyr::select(train, -dens)
    
    test <- tree_data[split1 == 1, ]
    test_nodens <- dplyr::select(test, -dens)
    
    train$dens <- factor(train$dens, levels = densorder)
    test$dens <- factor(test$dens, levels = densorder)
    
    # PCA
    pca <- prcomp(train_nodens, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
    
    test_pca_data <- predict(pca, newdata = test_nodens)
    test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
    
    train$dens <- factor(train$dens, levels = densorder)
    train_pca_data$dens <- factor(train_pca_data$dens, levels = densorder)
    
    # Create a trainControl object to control how the train function creates the model
    train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                  number = 10,             
                                  repeats = 10)            
    
    #Set required parameters for the model type we are using**
    tune_grid = expand.grid(cp=c(0.001))
    
    
    # Use the train() function to create the model
    validated_tree <- train(dens ~. ,
                            data= train,                        # Data set
                            method="rpart",                     # Model type(decision tree)
                            trControl= train_control,           # Model control options
                            tuneGrid = tune_grid,               # Required model parameters
                            maxdepth = 5,                       # Additional parameters***
                            minbucket= 5)
    
    validated_tree_pc <- train(dens ~. ,
                               data= train_pca_data,               # Data set
                               method="rpart",                     # Model type(decision tree)
                               trControl= train_control,           # Model control options
                               tuneGrid = tune_grid,               # Required model parameters
                               maxdepth = 5,                       # Additional parameters***
                               minbucket= 5)
    
    # Prediction
    
    pred <- predict(validated_tree, newdata = test_nodens)
    pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
    
    cm <- confusionMatrix(pred, test$dens, mode = "everything")
    cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
    
    
    # plot and save 
    
    alt <- unique(subset_data$Altitude)
    
    if(length(alt) == 2){
      alt <- "15-30m"
    }
    
    if(length(alt) == 3){
      alt <- "15-30-50m"
    }
    
    train_result <- data.frame(validated_tree$results[2:5])
    colnames(train_result) <- paste(colnames(train_result), "train", sep = "_")
    
    train_result_pc <- data.frame(validated_tree_pc$results[2:5])
    colnames(train_result_pc) <- paste(colnames(train_result_pc), "train", sep = "_")
    
    test_result <- data.frame(t(cm$overall[c(1,6,2)]))
    colnames(test_result) <- paste(colnames(test_result), "test", sep = "_")
    
    test_result_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
    colnames(test_result_pc) <- paste(colnames(test_result_pc), "test", sep = "_")
    
    result <- cbind(train_result, test_result)
    result_pc <- cbind(train_result_pc, test_result_pc)
    
    result$PC <- 0
    result_pc$PC <- PC_selected
    
    results <- rbind(result, result_pc)
    results$date <- current_date
    results$alt <- alt

    results_df <- rbind(results_df, results)
    
    
    setwd(paste(fig.dir, "Trees/CART/train-test", sep = "/"))

    png(paste(current_date, alt, "CART", ".png", sep="_"), width = 6.5, height = 5.5, units = 'in', res = 300)

    fancyRpartPlot(validated_tree$finalModel,
                   main = paste(current_date, alt, sep = " "))
    dev.off()

    png(paste(current_date, alt, "CART_PC", ".png", sep="_"), width = 6.5, height = 5.5, units = 'in', res = 300)

    fancyRpartPlot(validated_tree_pc$finalModel,
                   main = paste(current_date, alt, sep = " "))
    dev.off()

    
    ## 1.2 Separation per date and height, flight height not included in the model ###
    
    if(length(unique(subset_data$Altitude)) >1 ){
      
      for(j in unique(subset_data$Altitude)){
        
        subset_data_2 <-  subset_data[subset_data$Altitude == j, ]
        tree_data <- subset_data_2[,-c(1,2,3,6,7)]
        tree_data$var <- as.numeric(as.factor(tree_data$var))
        
        print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
        
        # Train/test
        
        split2<- sample(c(rep(0, 0.80 * nrow(tree_data)), rep(1, 0.20 * nrow(tree_data))))
        
        train <- tree_data[split2 == 0, ]
        train_nodens <- dplyr::select(train, -dens)
        
        test <- tree_data[split2 == 1, ]
        test_nodens <- dplyr::select(test, -dens)
        
        train$dens <- factor(train$dens, levels = densorder)
        test$dens <- factor(test$dens, levels = densorder)
        
        # PCA
        pca <- prcomp(train_nodens, center = T, scale. = T)
        eigenval <- pca$sdev ^ 2
        
        PC_selected <- length(eigenval[eigenval > 1])
        
        train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
        
        test_pca_data <- predict(pca, newdata = test_nodens)
        test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
        
        # CART trees
        
        validated_tree <- train(dens ~. ,
                                data= train,                        # Data set
                                method="rpart",                     # Model type(decision tree)
                                trControl= train_control,           # Model control options
                                tuneGrid = tune_grid,               # Required model parameters
                                maxdepth = 5,                       # Additional parameters***
                                minbucket= 5)
        
        validated_tree_pc <- train(dens ~. ,
                                   data= train_pca_data,               # Data set
                                   method="rpart",                     # Model type(decision tree)
                                   trControl= train_control,           # Model control options
                                   tuneGrid = tune_grid,               # Required model parameters
                                   maxdepth = 5,                       # Additional parameters***
                                   minbucket= 5)
        
        # Prediction
        
        pred <- predict(validated_tree, newdata = test_nodens)
        pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
        
        cm <- confusionMatrix(pred, test$dens, mode = "everything")
        cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
        
        
        # Plot and save
        alt <- unique(subset_data_2$Altitude)
        
        train_result <- data.frame(validated_tree$results[2:5])
        colnames(train_result) <- paste(colnames(train_result), "train", sep = "_")
        
        train_result_pc <- data.frame(validated_tree_pc$results[2:5])
        colnames(train_result_pc) <- paste(colnames(train_result_pc), "train", sep = "_")
        
        test_result <- data.frame(t(cm$overall[c(1,6,2)]))
        colnames(test_result) <- paste(colnames(test_result), "test", sep = "_")
        
        test_result_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
        colnames(test_result_pc) <- paste(colnames(test_result_pc), "test", sep = "_")
        
        result <- cbind(train_result, test_result)
        result_pc <- cbind(train_result_pc, test_result_pc)
        
        result$PC <- 0
        result_pc$PC <- PC_selected
        
        results <- rbind(result, result_pc)
        results$date <- current_date
        results$alt <- alt
        
        results_df <- rbind(results_df, results)
        
        
        png(paste(current_date, alt, "CART", ".png", sep="_"), width = 6.5, height = 5.5, units = 'in', res = 300)

        fancyRpartPlot(validated_tree$finalModel,
                       main = paste(current_date, alt, sep = " "))
        dev.off()

        png(paste(current_date, alt, "CART_PC", ".png", sep="_"), width = 6.5, height = 5.5, units = 'in', res = 300)

        fancyRpartPlot(validated_tree_pc$finalModel,
                       main = paste(current_date, alt, sep = " "))
        dev.off()
        
      }
    }
  }
  
#   round <- round +1
# }

results_df$k <- 10
results_df$repetitions <- 10

# to take a quick look
aggregate(results_df$Accuracy_test, list(results_df$date), FUN=mean) 

setwd(paste(out.dir, "CART", sep = "/"))
write.csv(results_df, "CART_cv_10_10.csv")

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 2. CONDITIONAL INFERENCE TREES  ########

densorder <- c("35","70","140", "280", "560")
data <- df

results_df <- NULL
round <- 1
while(round < 11){

  print(paste("Round: ", round))

  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$dens <- as.factor(as.character(subset_data$dens))
    subset_data$var <- as.numeric(as.factor(subset_data$var))
    subset_data$Altitude <- as.factor(subset_data$Altitude)
    
    ## 1.1 Separation per date only, including flight height as predictor variable ###
    if(length(unique(subset_data$Altitude))>1){
      tree_data <- subset_data[,-c(1,2,6,7)]
      tree_data$Altitude <- as.numeric(tree_data$Altitude)
      
    }
    
    else{
      tree_data <- subset_data[,-c(1,2,3,6,7)]
    }
    
    #Train/test split
    split1<- sample(c(rep(0, 0.80 * nrow(tree_data)), rep(1, 0.20 * nrow(tree_data))))
    
    train <- tree_data[split1 == 0, ]
    train_nodens <- dplyr::select(train, -dens)
    
    test <- tree_data[split1 == 1, ]
    test_nodens <- dplyr::select(test, -dens)
    
    train$dens <- factor(train$dens, levels = densorder)
    test$dens <- factor(test$dens, levels = densorder)
    
    # PCA
    pca <- prcomp(train_nodens, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
    
    test_pca_data <- predict(pca, newdata = test_nodens)
    test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
    
    train$dens <- factor(train$dens, levels = densorder)
    train_pca_data$dens <- factor(train_pca_data$dens, levels = densorder)
    
    #Set required parameters for the model type we are using**
    tune_grid = expand.grid(mincriterion=c(0.05))
    
    
    # Create a trainControl object to control how the train function creates the model
    train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                  number = 10,             # Use 10 partitions
                                  repeats = 10)            # Repeat 10 times
    
    
    # Use the train() function to create the model
    validated_tree <- train(dens ~. ,
                            data= train,                  
                            method="ctree",                     # Model type(ctree)
                            trControl= train_control, 
                            tuneGrid = tune_grid)           
    
    
    validated_tree_pc <- train(dens ~. ,
                            data= train_pca_data,                  
                            method="ctree",                     # Model type(ctree)
                            trControl= train_control, 
                            tuneGrid = tune_grid)           
    
    
    # Prediction
    
    pred <- predict(validated_tree, newdata = test_nodens)
    pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
    
    cm <- confusionMatrix(pred, test$dens, mode = "everything")
    cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
    
  
    # Save results
    
    alt <- unique(subset_data$Altitude)
    
    if(length(alt) == 2){
      alt <- "15-30m"
    }
    
    if(length(alt) == 3){
      alt <- "15-30-50m"
    }
    
    
    train_result <- data.frame(validated_tree$results[2:5])
    colnames(train_result) <- paste(colnames(train_result), "train", sep = "_")
    
    train_result_pc <- data.frame(validated_tree_pc$results[2:5])
    colnames(train_result_pc) <- paste(colnames(train_result_pc), "train", sep = "_")
    
    test_result <- data.frame(t(cm$overall[c(1,6,2)]))
    colnames(test_result) <- paste(colnames(test_result), "test", sep = "_")
    
    test_result_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
    colnames(test_result_pc) <- paste(colnames(test_result_pc), "test", sep = "_")
    
    result <- cbind(train_result, test_result)
    result_pc <- cbind(train_result_pc, test_result_pc)
    
    result$PC <- 0
    result_pc$PC <- PC_selected
    
    results <- rbind(result, result_pc)
    results$date <- current_date
    results$alt <- alt
    
    results_df <- rbind(results_df, results)
    
    setwd(paste(fig.dir, "Trees/ctree/train-test", sep = "/"))
    
    png(paste(current_date, alt, "ctree", ".png", sep="_"), width = 14, height = 8, units = 'in', res = 300)
    
    plot(validated_tree$finalModel, 
         main = paste(current_date, alt, sep = " "))
    dev.off()
    
    png(paste(current_date, alt, "ctree_PC", ".png", sep="_"), width = 14, height = 8, units = 'in', res = 300)
    
    plot(validated_tree_pc$finalModel, 
         main = paste(current_date, alt, sep = " "))
    dev.off()

    
    ## 2.1 Separation per date and height, flight height not included in the model ###
    
    if(length(unique(subset_data$Altitude)) >1 ){
      
      for(j in unique(subset_data$Altitude)){
        
        subset_data_2 <-  subset_data[subset_data$Altitude == j, ]
        tree_data <- subset_data_2[,-c(1,2,3,6,7)]
        tree_data$var <- as.numeric(as.factor(tree_data$var))
        
        print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
        
        # Train/test
        
        split2<- sample(c(rep(0, 0.80 * nrow(tree_data)), rep(1, 0.20 * nrow(tree_data))))
        
        train <- tree_data[split2 == 0, ]
        train_nodens <- dplyr::select(train, -dens)
        
        test <- tree_data[split2 == 1, ]
        test_nodens <- dplyr::select(test, -dens)
        
        train$dens <- factor(train$dens, levels = densorder)
        test$dens <- factor(test$dens, levels = densorder)
        
        # PCA
        pca <- prcomp(train_nodens, center = T, scale. = T)
        eigenval <- pca$sdev ^ 2
        
        PC_selected <- length(eigenval[eigenval > 1])
        
        train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
        
        test_pca_data <- predict(pca, newdata = test_nodens)
        test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
        
        # ctrees
        validated_tree <- train(dens ~. ,
                                data= train,                  
                                method="ctree",                     
                                trControl= train_control, 
                                tuneGrid = tune_grid)           
        
        
        validated_tree_pc <- train(dens ~. ,
                                   data= train_pca_data,                  
                                   method="ctree",                     
                                   trControl= train_control, 
                                   tuneGrid = tune_grid)
        
        # Prediction
        
        pred <- predict(validated_tree, newdata = test_nodens)
        pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
        
        cm <- confusionMatrix(pred, test$dens, mode = "everything")
        cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
        
        
        # Plot and save
        alt <- unique(subset_data_2$Altitude)
        
        train_result <- data.frame(validated_tree$results[2:5])
        colnames(train_result) <- paste(colnames(train_result), "train", sep = "_")
        
        train_result_pc <- data.frame(validated_tree_pc$results[2:5])
        colnames(train_result_pc) <- paste(colnames(train_result_pc), "train", sep = "_")
        
        test_result <- data.frame(t(cm$overall[c(1,6,2)]))
        colnames(test_result) <- paste(colnames(test_result), "test", sep = "_")
        
        test_result_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
        colnames(test_result_pc) <- paste(colnames(test_result_pc), "test", sep = "_")
        
        result <- cbind(train_result, test_result)
        result_pc <- cbind(train_result_pc, test_result_pc)
        
        result$PC <- 0
        result_pc$PC <- PC_selected
        
        results <- rbind(result, result_pc)
        results$date <- current_date
        results$alt <- alt
        
        results_df <- rbind(results_df, results)
        
        
        png(paste(current_date, alt, "ctree", ".png", sep="_"), width = 14, height = 8, units = 'in', res = 300)
        
        plot(validated_tree$finalModel, 
             main = paste(current_date, alt, sep = " "))
        dev.off()
        
        png(paste(current_date, alt, "ctree_PC", ".png", sep="_"), width = 14, height = 8, units = 'in', res = 300)
        
        plot(validated_tree_pc$finalModel, 
             main = paste(current_date, alt, sep = " "))
        dev.off()
      }
    }
  }
  
  round <- round +1
}

results_df$k <- 10
results_df$repetitions <- 10

# to take a quick look
aggregate(results_df$Accuracy_test, list(results_df$date), FUN=mean) 


setwd(paste(out.dir, "ctree", sep = "/"))
write.csv(results_df, "ctree_cv_10_10.csv")

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 3. RANDOM FORESTS ########

densorder <- c("35","70","140", "280", "560")

set.seed(12345) 
results_df <- NULL
error_df <- NULL
importance_df <- NULL
conf_matrices <- list()
k <- 1
data <- df
round <-1

while(round < 11){
  print(paste("Round: ", round))
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    subset_data$dens <- as.factor(as.character(subset_data$dens))
    subset_data$var <- as.numeric(as.factor(subset_data$var))
    subset_data$Altitude <- as.factor(subset_data$Altitude)
    
    ## 2.1 Separation per date only, including flight height as predictor variable ###
    
    if(length(unique(subset_data$Altitude))>1){
      rf_data <- subset_data[,-c(1,2,6,7)]
      rf_data$Altitude <- as.numeric(rf_data$Altitude)
    }
    
    else{
      rf_data <- subset_data[,-c(1,2,6,7,3)]
    }
    
    # Train/test split
    
    split1<- sample(c(rep(0, 0.80 * nrow(rf_data)), rep(1, 0.20 * nrow(rf_data))))
    
    train <- rf_data[split1 == 0, ]
    train_nodens <- dplyr::select(train, -dens)
    
    test <- rf_data[split1 == 1, ]
    test_nodens <- dplyr::select(test, -dens)
    
    train$dens <- factor(train$dens, levels = densorder)
    test$dens <- factor(test$dens, levels = densorder)
    
    # PCA
    pca <- prcomp(train_nodens, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
    
    test_pca_data <- predict(pca, newdata = test_nodens)
    test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
    
    train$dens <- factor(train$dens, levels = densorder)
    train_pca_data$dens <- factor(train_pca_data$dens, levels = densorder)
    
    # RANDOM FOREST
    rf <- randomForest(dens~., data = train, proximity=TRUE, 
                       ntree = 5000, importance = TRUE)
    rf_pc <- randomForest(dens~., data = train_pca_data, proximity=TRUE, 
                          ntree = 5000, importance = TRUE)     
    
    
    # prediction
    
    pred <- predict(rf, newdata = test_nodens)
    pred_pc <- predict(rf_pc, newdata = test_pca_data)
    
    cm <- confusionMatrix(pred, test$dens, mode = "everything")
    cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
    
    # Plot and save
    
    alt <- unique(subset_data$Altitude)
    
    if(length(alt) == 2){
      alt <- "15-30m"
    }
    
    if(length(alt) == 3){
      alt <- "15-30-50m"
    }
    
    
    results <- data.frame(t(cm$overall[c(1,6,2)]))
    results_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
    results$date <- current_date
    results_pc$date <- current_date
    
    results$alt <- alt
    results_pc$alt <- alt
    
    results$PC <- 0
    results_pc$PC <- PC_selected
    
    results$mtry <- rf$mtry
    results_pc$mtry <- rf_pc$mtry
    
    results_df <- rbind(results_df, results)
    results_df <- rbind(results_df, results_pc)
    
    # importance
    
    importance <- data.frame(rf$importance) %>% 
      dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
      mutate(date = current_date,
             alt = alt,
             PC = 0, 
             mtry = rf$mtry) %>% 
      tibble::rownames_to_column("var")
    
    importance_pc <- data.frame(rf_pc$importance) %>% 
      dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
      mutate(date = current_date,
             alt = alt,
             PC = PC_selected, 
             mtry = rf_pc$mtry)%>% 
      tibble::rownames_to_column("var")
    
    importance_results <- rbind(importance, importance_pc)
    importance_df <- rbind(importance_df, importance_results)
    
    
    # error rate 
    
    error <- data.frame(t(rf$err.rate[5000,]))
    error_pc <- data.frame(t(rf_pc$err.rate[5000,]))
    
    error$date <- current_date
    error_pc$date <- current_date
    
    error$alt <- alt
    error_pc$alt <- alt
    
    error$PC <- 0
    error_pc$PC <- PC_selected
    
    error$mtry <- rf$mtry
    error_pc$mtry <- rf_pc$mtry
    
    error_results <- rbind(error,error_pc)
    error_df <- rbind(error_df, error_results)
    

    
    # Plot error
    # setwd(paste(fig.dir, "RandomForests/80-20split", sep = "/"))
    # 
    # png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    # 
    # layout(matrix(c(1,2),nrow=1),
    #        width=c(4,1))
    # par(mar=c(5,4,4,0)) #No margin on the right side
    # plot(rf,
    #      main = paste(current_date, alt, sep = " "))
    # par(mar=c(5,0,4,2)) #No margin on the left side
    # plot(c(0,1),type="n", axes=F, xlab="", ylab="")
    # legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
    # dev.off()
    # 
    # png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    # 
    # layout(matrix(c(1,2),nrow=1),
    #        width=c(4,1))
    # par(mar=c(5,4,4,0)) #No margin on the right side
    # plot(rf_pc,
    #      main = paste(current_date, alt, "PC", PC_selected, sep = " "))
    # par(mar=c(5,0,4,2)) #No margin on the left side
    # plot(c(0,1),type="n", axes=F, xlab="", ylab="")
    # legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
    # 
    # dev.off()
    # 
    # # Plot variable importance
    # png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    # varImpPlot(rf, main = paste(current_date, alt, sep=" "))
    # 
    # dev.off()
    # 
    # png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    # varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
    # 
    # dev.off()
    
    
    
    ## 2.1 Separation per date and height, flight height not included in the model ###
    
    if(length(unique(subset_data$Altitude)) >1 ){
      
      for(j in unique(subset_data$Altitude)){
        
        subset_data_2 <-  subset_data[subset_data$Altitude == j, ]
        rf_data <- subset_data_2[,-c(1,2,3,6,7)]
        rf_data$dens <- factor(rf_data$dens, levels = densorder)
        print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
        
       
        split2<- sample(c(rep(0, 0.80 * nrow(rf_data)), rep(1, 0.20 * nrow(rf_data))))
        
        train <- rf_data[split2 == 0, ]
        train_nodens <- dplyr::select(train, -dens)
        
        test <- rf_data[split2 == 1, ]
        test_nodens <- dplyr::select(test, -dens)
        
        train$dens <- factor(train$dens, levels = densorder)
        test$dens <- factor(test$dens, levels = densorder)
        
        # PCA
        pca <- prcomp(train_nodens, center = T, scale. = T)
        eigenval <- pca$sdev ^ 2
        
        PC_selected <- length(eigenval[eigenval > 1])
        
        train_pca_data <- data.frame(dens = train$dens, pca$x[,1:PC_selected])
        
        test_pca_data <- predict(pca, newdata = test_nodens)
        test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
        
        # RANDOM FOREST
        
        rf <- randomForest(dens~., data = train, proximity=TRUE, 
                           ntree = 5000, importance = TRUE)
        rf_pc <- randomForest(dens~., data = train_pca_data, proximity=TRUE, 
                              ntree = 5000, importance = TRUE)
        

        # prediction
        
        pred <- predict(rf, newdata = test_nodens)
        pred_pc <- predict(rf_pc, newdata = test_pca_data)
        
        cm <- confusionMatrix(pred, test$dens, mode = "everything")
        cm_pc <- confusionMatrix(pred_pc, test$dens, mode = "everything")
        
        # Plot and save
        
        alt <- unique(subset_data_2$Altitude)
        
  
        results <- data.frame(t(cm$overall[c(1,6,2)]))
        results_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
        
        results$date <- current_date
        results_pc$date <- current_date
        
        results$alt <- alt
        results_pc$alt <- alt
        
        results$PC <- 0
        results_pc$PC <- PC_selected
        
        results$mtry <- rf$mtry
        results_pc$mtry <- rf_pc$mtry
        
        results_df <- rbind(results_df, results)
        results_df <- rbind(results_df, results_pc)
        
        # importance
        
        importance <- data.frame(rf$importance) %>% 
          dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
          mutate(date = current_date,
                 alt = alt,
                 PC = 0, 
                 mtry = rf$mtry) %>% 
          tibble::rownames_to_column("var")
        
        importance_pc <- data.frame(rf_pc$importance) %>% 
          dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
          mutate(date = current_date,
                 alt = alt,
                 PC = PC_selected, 
                 mtry = rf_pc$mtry)%>% 
          tibble::rownames_to_column("var")
        
        importance_results <- rbind(importance, importance_pc)
        importance_df <- rbind(importance_df, importance_results)
        
        # error rate 
        
        error <- data.frame(t(rf$err.rate[5000,]))
        error_pc <- data.frame(t(rf_pc$err.rate[5000,]))
        
        error$date <- current_date
        error_pc$date <- current_date
        
        error$alt <- alt
        error_pc$alt <- alt
        
        error$PC <- 0
        error_pc$PC <- PC_selected
        
        error$mtry <- rf$mtry
        error_pc$mtry <- rf_pc$mtry
        
        error_results <- rbind(error,error_pc)
        error_df <- rbind(error_df, error_results)
        
        # Plot error

        # png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        # 
        # layout(matrix(c(1,2),nrow=1),
        #        width=c(4,1))
        # par(mar=c(5,4,4,0)) #No margin on the right side
        # plot(rf,
        #      main = paste(current_date, alt, sep = " "))
        # par(mar=c(5,0,4,2)) #No margin on the left side
        # plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        # legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        # dev.off()
        # 
        # png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        # 
        # layout(matrix(c(1,2),nrow=1),
        #        width=c(4,1))
        # par(mar=c(5,4,4,0)) #No margin on the right side
        # plot(rf_pc,
        #      main = paste(current_date, alt, "PC", PC_selected, sep = " "))
        # par(mar=c(5,0,4,2)) #No margin on the left side
        # plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        # legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        # 
        # dev.off()
        # 
        # # Plot variable importance
        # png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        # varImpPlot(rf, main = paste(current_date, alt, sep=" "))
        # 
        # dev.off()
        # 
        # png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        # varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
        # 
        # dev.off()
        
      }
    }
  }
  round <- round +1
}

# to take a quick look
aggregate(results_df$Accuracy, list(results_df$date), FUN=mean) 

setwd(paste(out.dir, "RandomForests/results/raw", sep = "/"))
write.csv(error_df, "OOB_error_rate_10reps_8020split.csv")
write.csv(results_df, "Accuracy_RF_density_prediction_10reps_8020split.csv")
write.csv(importance_df, "var_importance_8020split")

