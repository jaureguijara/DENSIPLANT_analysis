library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(car)
library(Hmisc)
library(stringr)
library(corrplot)
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
df <- arrange(df, Density)
df$Density<- as.character(df$Density)


######## DECISSION (CLASSIFICATION) CART TREES ########

densorder <- c("35","70","140", "280", "560")

set.seed(12345) 

data <- df

results_df <- NULL
round <- 1

#### With train-test 80-20 split #### 

while(round < 11){
  print(paste("Round: ", round))
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    
    # Subset data per date 
    
    subset_date <- data[data$Date == current_date,]
    subset_date$Genotype <- as.numeric(as.factor(subset_date$Genotype))
    
    alt_combi_subset <- NULL #to merge de dates having different index columns per altitude later
    
    # 1 Separating per date and altitude
    
    for(j in 1:length(unique(subset_date$Altitude))){
      
      current_alt <- unique(subset_date$Altitude)[j]
      
      subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
      
      tree_data <- subset_alt[,-c(2,3,6,7)]
      
      tree_data_alt_tocol <- tree_data 
      colnames(tree_data_alt_tocol)[4:ncol(tree_data_alt_tocol)] <- paste(colnames(tree_data_alt_tocol)[4:ncol(tree_data_alt_tocol)], current_alt, sep = "_")
      alt_combi_subset[[j]] <- tree_data_alt_tocol #to merge de dates having different index columns per altitude later
      
      tree_data$Density <- as.numeric(as.character(tree_data$Density)) # to sort the data
      tree_data <- tree_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      tree_data$Density <- as.factor(as.character(tree_data$Density)) # to use for the model
      
      print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
      
      # Split the data 
      
      split<- sample(c(rep(0, 0.80 * nrow(tree_data)), rep(1, 0.20 * nrow(tree_data))))
      
      train <- tree_data[split == 0, ]
      train_nodens <- dplyr::select(train, -Density)
      
      test <- tree_data[split == 1, ]
      test_nodens <- dplyr::select(test, -Density)
      
      train$Density <- factor(train$Density, levels = densorder)
      test$Density <- factor(test$Density, levels = densorder)
      
      # PCA
      pca <- prcomp(train_nodens, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      train_pca_data <- data.frame(Density = train$Density, pca$x[,1:PC_selected])
      
      test_pca_data <- predict(pca, newdata = test_nodens)
      test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
      
      ### CART tree ###
      
      # Create a trainControl object to control how the train function creates the model
      train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                    number = 10,             
                                    repeats = 10)            
      
      #Set required parameters for the model type we are using**
      tune_grid = expand.grid(cp=c(0.001))
      
      
      # Use the train() function to create the model
      validated_tree <- train(Density~. ,
                              data= train,                        # Data set
                              method="rpart",                     # Model type(decision tree)
                              trControl= train_control,           # Model control options
                              tuneGrid = tune_grid,               # Required model parameters
                              maxdepth = 5,                       # Additional parameters***
                              minbucket= 5)
      
      validated_tree_pc <- train(Density~. ,
                                 data= train_pca_data,               # Data set
                                 method="rpart",                     # Model type(decision tree)
                                 trControl= train_control,           # Model control options
                                 tuneGrid = tune_grid,               # Required model parameters
                                 maxdepth = 5,                       # Additional parameters***
                                 minbucket= 5)
      
      
      
      # Prediction
      
      pred <- predict(validated_tree, newdata = test_nodens)
      pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
      
      cm <- confusionMatrix(pred, test$Density, mode = "everything")
      cm_pc <- confusionMatrix(pred_pc, test$Density, mode = "everything")
        
      # Save and plot
      
      alt <- unique(subset_alt$Altitude)
      
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
      
      if(round == 1){
        
        # Plot tree
        setwd(paste(fig.dir, "Trees/CART/80-20split_altsingle", sep = "/"))
        
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
    
    # 2 Combining altitudes for same dates. 
    
    if(length(unique(subset_date$Altitude)) >1){
      for(i in 1:length(alt_combi_subset)){
        if(i == 1){
          tree_data_combi <-data.frame(alt_combi_subset[i])
        }
        else{
          tree_data_combi <- merge(tree_data_combi,data.frame(alt_combi_subset[i]), 
                                 by = c("Image.Name", "Genotype", "Density"))
        }
      }
      
      tree_data <- tree_data_combi 
      tree_data$Density <- as.numeric(as.character(tree_data$Density)) # to sort the data
      
      tree_data <- tree_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      tree_data$Density <- as.factor(as.character(tree_data$Density)) # to use for the model
      
      # Split the data, we use the same split as before. 
      
      train <- tree_data[split == 0, ]
      train_nodens <- dplyr::select(train, -Density)
      
      test <- tree_data[split == 1, ]
      test_nodens <- dplyr::select(test, -Density)
      
      train$Density <- factor(train$Density, levels = densorder)
      test$Density <- factor(test$Density, levels = densorder)
      
      # PCA
      pca <- prcomp(train_nodens, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      train_pca_data <- data.frame(Density = train$Density, pca$x[,1:PC_selected])
      
      test_pca_data <- predict(pca, newdata = test_nodens)
      test_pca_data <- data.frame(test_pca_data[,1:PC_selected])
      
      ### CART tree ###
      
      # Create a trainControl object to control how the train function creates the model
      train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                    number = 10,             
                                    repeats = 10)            
      
      #Set required parameters for the model type we are using**
      tune_grid = expand.grid(cp=c(0.001))
      
      
      # Use the train() function to create the model
      validated_tree <- train(Density~. ,
                              data= train,                        # Data set
                              method="rpart",                     # Model type(decision tree)
                              trControl= train_control,           # Model control options
                              tuneGrid = tune_grid,               # Required model parameters
                              maxdepth = 5,                       # Additional parameters***
                              minbucket= 5)
      
      validated_tree_pc <- train(Density~. ,
                                 data= train_pca_data,               # Data set
                                 method="rpart",                     # Model type(decision tree)
                                 trControl= train_control,           # Model control options
                                 tuneGrid = tune_grid,               # Required model parameters
                                 maxdepth = 5,                       # Additional parameters***
                                 minbucket= 5)
    
      # Prediction
      
      pred <- predict(validated_tree, newdata = test_nodens)
      pred_pc <- predict(validated_tree_pc, newdata = test_pca_data)
      
      cm <- confusionMatrix(pred, test$Density, mode = "everything")
      cm_pc <- confusionMatrix(pred_pc, test$Density, mode = "everything")
      
      # Save and plot
      
      alt <- unique(subset_date$Altitude)
      if(length(alt) == 2){
        alt <- "15-30m"
      }
      
      else {
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
      
      if(round == 1){
        
        # Plot tree
        setwd(paste(fig.dir, "Trees/CART/80-20split_altcombi", sep = "/"))
        
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
  round <- round +1
}

results_df$k <- 10
results_df$repetitions <- 10

# to take a quick look
aggregate(results_df$Accuracy_test, list(results_df$date), FUN=mean) 

setwd(paste(out.dir, "CART/results", sep = "/"))
write.csv(results_df, "CART_cv_10_10_8020split.csv")



#### With entire dataset, no split, no validation, only train ####

set.seed(12345) 

data <- df

results_df <- NULL
round <- 1


while(round < 11){
  print(paste("Round: ", round))
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    
    # Subset data per date 
    
    subset_date <- data[data$Date == current_date,]
    subset_date$Genotype <- as.numeric(as.factor(subset_date$Genotype))
    
    alt_combi_subset <- NULL #to merge de dates having different index columns per altitude later
    
    # 1 Separating per date and altitude
    
    for(j in 1:length(unique(subset_date$Altitude))){
      
      current_alt <- unique(subset_date$Altitude)[j]
      
      subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
      
      tree_data <- subset_alt[,-c(2,3,6,7)]
      
      tree_data_alt_tocol <- tree_data 
      colnames(tree_data_alt_tocol)[4:ncol(tree_data_alt_tocol)] <- paste(colnames(tree_data_alt_tocol)[4:ncol(tree_data_alt_tocol)], current_alt, sep = "_")
      alt_combi_subset[[j]] <- tree_data_alt_tocol #to merge de dates having different index columns per altitude later
      
      tree_data$Density <- as.numeric(as.character(tree_data$Density)) # to sort the data
      tree_data <- tree_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      tree_data$Density <- as.factor(as.character(tree_data$Density)) # to use for the model
      
      print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
      
      pca_data <- tree_data %>% 
        dplyr::select(-Density)
      
      # PCA
      pca <- prcomp(pca_data, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      tree_data_pc <- data.frame(Density = tree_data$Density, pca$x[,1:PC_selected])
      
      tree_data$Density <- factor(tree_data$Density, levels = densorder)
      tree_data_pc$Density <- factor(tree_data_pc$Density, levels = densorder)
      
      ### CART tree ###
      
      # Create a trainControl object to control how the train function creates the model
      train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                    number = 10,             
                                    repeats = 10)            
      
      #Set required parameters for the model type we are using**
      tune_grid = expand.grid(cp=c(0.001))
      
      
      # Use the train() function to create the model
      validated_tree <- train(Density~. ,
                              data= tree_data,                        # Data set
                              method="rpart",                     # Model type(decision tree)
                              trControl= train_control,           # Model control options
                              tuneGrid = tune_grid,               # Required model parameters
                              maxdepth = 5,                       # Additional parameters***
                              minbucket= 5)
      
      validated_tree_pc <- train(Density~. ,
                                 data= tree_data_pc,               # Data set
                                 method="rpart",                     # Model type(decision tree)
                                 trControl= train_control,           # Model control options
                                 tuneGrid = tune_grid,               # Required model parameters
                                 maxdepth = 5,                       # Additional parameters***
                                 minbucket= 5)
      
      # Save and plot
      
      alt <- unique(subset_alt$Altitude)
      
      results <- data.frame(validated_tree$results)
      results_pc <- data.frame(validated_tree_pc$results)
      
      results$date <- current_date
      results_pc$date <- current_date
      
      results$alt <- alt
      results_pc$alt <- alt
      
      results$PC <- 0
      results_pc$PC <- PC_selected
      
      results_df <- rbind(results_df, results)
      results_df <- rbind(results_df, results_pc)
      
      if(round == 1){
        
        # Plot tree
        setwd(paste(fig.dir, "Trees/CART/nosplit_onlytrain_altsingle", sep = "/"))
        
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
    
    # 2 Combining altitudes for same dates. 
    
    if(length(unique(subset_date$Altitude)) >1){
      for(i in 1:length(alt_combi_subset)){
        if(i == 1){
          tree_data_combi <-data.frame(alt_combi_subset[i])
        }
        else{
          tree_data_combi <- merge(tree_data_combi,data.frame(alt_combi_subset[i]), 
                                   by = c("Image.Name", "Genotype", "Density"))
        }
      }
      
      tree_data <- tree_data_combi 
      tree_data$Density <- as.numeric(as.character(tree_data$Density)) # to sort the data
      
      tree_data <- tree_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      tree_data$Density <- as.factor(as.character(tree_data$Density)) # to use for the model
      
      pca_data <- tree_data %>% 
        dplyr::select(-Density)
      
      # PCA
      pca <- prcomp(pca_data, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      tree_data_pc <- data.frame(Density = tree_data$Density, pca$x[,1:PC_selected])
      
      tree_data$Density <- factor(tree_data$Density, levels = densorder)
      tree_data_pc$Density <- factor(tree_data_pc$Density, levels = densorder)
      
      ### CART tree ###
      
      # Create a trainControl object to control how the train function creates the model
      train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                    number = 10,             
                                    repeats = 10)            
      
      #Set required parameters for the model type we are using**
      tune_grid = expand.grid(cp=c(0.001))
      
      
      # Use the train() function to create the model
      validated_tree <- train(Density~. ,
                              data= tree_data,                        # Data set
                              method="rpart",                     # Model type(decision tree)
                              trControl= train_control,           # Model control options
                              tuneGrid = tune_grid,               # Required model parameters
                              maxdepth = 5,                       # Additional parameters***
                              minbucket= 5)
      
      validated_tree_pc <- train(Density~. ,
                                 data= tree_data_pc,               # Data set
                                 method="rpart",                     # Model type(decision tree)
                                 trControl= train_control,           # Model control options
                                 tuneGrid = tune_grid,               # Required model parameters
                                 maxdepth = 5,                       # Additional parameters***
                                 minbucket= 5)
      
      
      # Save and plot
      
      alt <- unique(subset_date$Altitude)
      if(length(alt) == 2){
        alt <- "15-30m"
      }
      
      else {
        alt <- "15-30-50m"
      }
      
      results <- data.frame(validated_tree$results)
      results <- data.frame(validated_tree_pc$results)
      
      results$date <- current_date
      results_pc$date <- current_date
      
      results$alt <- alt
      results_pc$alt <- alt
      
      results$PC <- 0
      results_pc$PC <- PC_selected
      
      results_df <- rbind(results_df, results)
      results_df <- rbind(results_df, results_pc)
      
      if(round == 1){
        
        # Plot tree
        setwd(paste(fig.dir, "Trees/CART/nosplit_onlytrain_altcombi", sep = "/"))
        
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
  round <- round +1
}

results_df$k <- 10
results_df$repetitions <- 10

# to take a quick look
aggregate(results_df$Accuracy, list(results_df$date), FUN=mean) 

setwd(paste(out.dir, "CART/results", sep = "/"))
write.csv(results_df, "CART_cv_10_10_nosplit_onlytrain.csv")
