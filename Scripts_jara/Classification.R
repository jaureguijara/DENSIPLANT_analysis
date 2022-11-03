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
data <- df

results_df <- NULL

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$dens <- as.factor(as.character(subset_data$dens))
  subset_data$var <- as.factor(subset_data$var)
  subset_data$Altitude <- as.factor(subset_data$Altitude)
  
  ## 1.1 Separation per date only, including flight height as predictor variable ###
  if(length(unique(subset_data$Altitude))>1){
    tree_data <- subset_data[,-c(1,2,6,7)]
    tree_data$Altitude <- as.numeric(tree_data$Altitude)

  }
  
  else{
    tree_data <- subset_data[,-c(1,2,3,6,7)]
  }
  
  tree_data$dens <- factor(tree_data$dens, levels = densorder)
  
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
  
  # With Principal Components instead of variables 
  
  pca_data <- dplyr::select(tree_data, -dens)
  pca_data$var <- as.numeric(pca_data$var)

  
  pca <- prcomp(pca_data, center = T, scale. = T)
  eigenval <- pca$sdev ^ 2
  
  PC_selected <- length(eigenval[eigenval > 1])
  
  tree_data_pc <- data.frame(dens = tree_data$dens, pca$x[,1:PC_selected])
  
  tree_data_pc$dens <- factor(tree_data_pc$dens, levels = densorder)
  
  validated_tree_pc <- train(dens ~. ,
                          data= tree_data_pc,                  # Data set
                          method="rpart",                     # Model type(decision tree)
                          trControl= train_control,           # Model control options
                          tuneGrid = tune_grid,               # Required model parameters
                          maxdepth = 5,                       # Additional parameters***
                          minbucket= 5)
  
  # plot and save 
  
  alt <- unique(subset_data$Altitude)
  
  if(length(alt) == 2){
    alt <- "15-30m"
  }
  
  if(length(alt) == 3){
    alt <- "15-30-50m"
  }
  
  results_1 <- data.frame(validated_tree$results)
  results_1_pc <- data.frame(validated_tree_pc$results)
  
  results_1$date <- current_date
  results_1_pc$date <- current_date
  
  results_1$alt <- alt
  results_1_pc$alt <- alt
  
  results_1$PC <- 0
  results_1_pc$PC <- PC_selected
  
  results_df <- rbind(results_df, results_1)
  results_df <- rbind(results_df, results_1_pc)
  
  
  setwd(paste(fig.dir, "Trees/CART", sep = "/"))
  
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
      tree_data$dens <- factor(tree_data$dens, levels = densorder)
      print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
      
      validated_tree <- train(dens ~. ,
                              data= tree_data,                  # Data set
                              method="rpart",                     # Model type(decision tree)
                              trControl= train_control,           # Model control options
                              tuneGrid = tune_grid,               # Required model parameters
                              maxdepth = 5,                       # Additional parameters***
                              minbucket= 5)
      
      
      # With Principal Components instead of variables 
      
      pca_data <- dplyr::select(tree_data, -dens)
      pca_data$var <- as.numeric(pca_data$var)
      
      pca <- prcomp(pca_data, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      tree_data_pc <- data.frame(dens = tree_data$dens, pca$x[,1:PC_selected])
      
      tree_data_pc$dens <- factor(tree_data_pc$dens, levels = densorder)
      validated_tree_pc <- train(dens ~. ,
                                 data= tree_data_pc,                  # Data set
                                 method="rpart",                     # Model type(decision tree)
                                 trControl= train_control,           # Model control options
                                 tuneGrid = tune_grid,               # Required model parameters
                                 maxdepth = 5,                       # Additional parameters***
                                 minbucket= 5)
      
      # Plot and save
      alt <- unique(subset_data_2$Altitude)
      
      results_2 <- data.frame(validated_tree$results)
      results_2_pc <- data.frame(validated_tree_pc$results)
      
      results_2$date <- current_date
      results_2_pc$date <- current_date
      
      results_2$alt <- alt
      results_2_pc$alt <- alt
      
      results_2$PC <- 0
      results_2_pc$PC <- PC_selected
      
      results_df <- rbind(results_df, results_2)
      results_df <- rbind(results_df, results_2_pc)
      
      
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

setwd(paste(out.dir, "CART", sep = "/"))
write.csv(results_df, "CART_results.csv")

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 2. CONDITIONAL INFERENCE TREES  ########

densorder <- c("35","70","140", "280", "560")
data <- df

results_df <- NULL

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  subset_data$dens <- as.factor(as.character(subset_data$dens))
  subset_data$var <- as.factor(subset_data$var)
  subset_data$Altitude <- as.factor(subset_data$Altitude)

  ## 2.1 Separation per date only, including flight height as predictor variable ###
  
  if(length(unique(subset_data$Altitude))>1){
    tree_data <- subset_data[,-c(1,2,6,7)]
    tree_data$Altitude <- as.numeric(tree_data$Altitude)
  }
  
  else{
    tree_data <- subset_data[,-c(1,2,6,7,3)]
  }
  
  tree_data$dens <- factor(tree_data$dens, levels = densorder)
  
  #Set required parameters for the model type we are using**
  tune_grid = expand.grid(mincriterion=c(0.05))
  
  
  # Create a trainControl object to control how the train function creates the model
  train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                                number = 10,             # Use 10 partitions
                                repeats = 10)            # Repeat 10 times
  
  # Use the train() function to create the model
  validated_tree <- train(dens ~. ,
                          data= tree_data,                  
                          method="ctree",                     # Model type(ctree)
                          trControl= train_control, 
                          tuneGrid = tune_grid)           

  # With Principal Components instead of variables 
  
  pca_data <- dplyr::select(tree_data, -dens)
  pca_data$var <- as.numeric(pca_data$var)
  
  
  pca <- prcomp(pca_data, center = T, scale. = T)
  eigenval <- pca$sdev ^ 2
  
  PC_selected <- length(eigenval[eigenval > 1])
  
  tree_data_pc <- data.frame(dens = tree_data$dens, pca$x[,1:PC_selected])
  
  tree_data_pc$dens <- factor(tree_data_pc$dens, levels = densorder)
  
  validated_tree_pc <- train(dens ~. ,
                             data= tree_data_pc,                 
                             method="ctree",                     
                             trControl= train_control, 
                             tuneGrid = tune_grid)           
  # Plot and save
  
  alt <- unique(subset_data$Altitude)
  
  if(length(alt) == 2){
    alt <- "15-30m"
  }
  
  if(length(alt) == 3){
    alt <- "15-30-50m"
  }
  
  results_1 <- data.frame(validated_tree$results)
  results_1_pc <- data.frame(validated_tree_pc$results)
  
  results_1$date <- current_date
  results_1_pc$date <- current_date
  
  results_1$alt <- alt
  results_1_pc$alt <- alt
  
  results_1$PC <- 0
  results_1_pc$PC <- PC_selected
  
  results_df <- rbind(results_df, results_1)
  results_df <- rbind(results_df, results_1_pc)
  
  setwd(paste(fig.dir, "Trees/ctree", sep = "/"))
  
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
      tree_data$dens <- factor(tree_data$dens, levels = densorder)
      print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
      
      # Use the train() function to create the model
      validated_tree <- train(dens ~. ,
                              data= tree_data,                  
                              method="ctree",                     
                              trControl= train_control, 
                              tuneGrid = tune_grid)     
      
      
      # With Principal Components instead of variables 
      pca_data <- dplyr::select(tree_data, -dens)
      pca_data$var <- as.numeric(pca_data$var)
      
      
      pca <- prcomp(pca_data, center = T, scale. = T)
      eigenval <- pca$sdev ^ 2
      
      PC_selected <- length(eigenval[eigenval > 1])
      
      tree_data_pc <- data.frame(dens = tree_data$dens, pca$x[,1:PC_selected])
      
      tree_data_pc$dens <- factor(tree_data_pc$dens, levels = densorder)
      
      validated_tree_pc <- train(dens ~. ,
                                 data= tree_data_pc,                 
                                 method="ctree",                     
                                 trControl= train_control, 
                                 tuneGrid = tune_grid) 
      
      
      # Plot and save

      alt <- unique(subset_data_2$Altitude)
      
      results_2 <- data.frame(validated_tree$results)
      results_2_pc <- data.frame(validated_tree_pc$results)
      
      results_2$date <- current_date
      results_2_pc$date <- current_date
      
      results_2$alt <- alt
      results_2_pc$alt <- alt
      
      results_2$PC <- 0
      results_2_pc$PC <- PC_selected
      
      results_df <- rbind(results_df, results_2)
      results_df <- rbind(results_df, results_2_pc)
      
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

setwd(paste(out.dir, "ctree", sep = "/"))
write.csv(results_df, "ctree_results.csv")

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

######## 3. RANDOM FORESTS ########

densorder <- c("35","70","140", "280", "560")

set.seed(12345) 
results_df <- NULL
conf_matrices <- list()
k <- 1
data <- df

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
  
  split1<- sample(c(rep(0, 0.70 * nrow(rf_data)), rep(1, 0.30 * nrow(rf_data))))
  
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
  
  
  conf_matrices[[k]] <- cm
  names(conf_matrices)[k] <- paste(alt, current_date, sep = "_")
  k <- k +1
  conf_matrices[[k]] <- cm_pc
  names(conf_matrices)[k] <- paste(alt, current_date, "PC", PC_selected, sep = "_")
  k <- k +1
  
  results <- data.frame(t(cm$overall[c(1,6,2)]))
  results_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
  results$date <- current_date
  results_pc$date <- current_date
  
  results$alt <- alt
  results_pc$alt <- alt
  
  results$PC <- 0
  results_pc$PC <- PC_selected
  
  results_df <- rbind(results_df, results)
  results_df <- rbind(results_df, results_pc)
  
  setwd(paste(fig.dir, "RandomForests", sep = "/"))
  
  # Plot error  
  png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
  
  layout(matrix(c(1,2),nrow=1),
         width=c(4,1)) 
  par(mar=c(5,4,4,0)) #No margin on the right side
  plot(rf, 
       main = paste(current_date, alt, sep = " "))
  par(mar=c(5,0,4,2)) #No margin on the left side
  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
  dev.off()
  
  png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
  
  layout(matrix(c(1,2),nrow=1),
         width=c(4,1)) 
  par(mar=c(5,4,4,0)) #No margin on the right side
  plot(rf_pc, 
       main = paste(current_date, alt, "PC", PC_selected, sep = " "))
  par(mar=c(5,0,4,2)) #No margin on the left side
  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
  
  dev.off()
  
  # Plot variable importance
  png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
  varImpPlot(rf, main = paste(current_date, alt, sep=" "))
  
  dev.off()
  
  png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
  varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
  
  dev.off()
  
  ## 2.1 Separation per date and height, flight height not included in the model ###
  
  if(length(unique(subset_data$Altitude)) >1 ){
    
    for(j in unique(subset_data$Altitude)){
      
      subset_data_2 <-  subset_data[subset_data$Altitude == j, ]
      rf_data <- subset_data_2[,-c(1,2,3,6,7)]
      rf_data$dens <- factor(rf_data$dens, levels = densorder)
      print(paste(current_date, unique(subset_data_2$Altitude), sep ="_"))
      
     
      split2<- sample(c(rep(0, 0.70 * nrow(rf_data)), rep(1, 0.30 * nrow(rf_data))))
      
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
      
      conf_matrices[[k]] <- cm
      names(conf_matrices)[k] <- paste(alt, current_date, sep = "_")
      k <- k +1
      conf_matrices[[k]] <- cm_pc
      names(conf_matrices)[k] <- paste(alt, current_date, "PC", PC_selected, sep = "_")
      k <- k +1
      
      results <- data.frame(t(cm$overall[c(1,6,2)]))
      results_pc <- data.frame(t(cm_pc$overall[c(1,6,2)]))
      results$date <- current_date
      results_pc$date <- current_date
      
      results$alt <- alt
      results_pc$alt <- alt
      
      results$PC <- 0
      results_pc$PC <- PC_selected
      
      results_df <- rbind(results_df, results)
      results_df <- rbind(results_df, results_pc)
      
      # Plot error  
      png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
      
      layout(matrix(c(1,2),nrow=1),
             width=c(4,1)) 
      par(mar=c(5,4,4,0)) #No margin on the right side
      plot(rf, 
           main = paste(current_date, alt, sep = " "))
      par(mar=c(5,0,4,2)) #No margin on the left side
      plot(c(0,1),type="n", axes=F, xlab="", ylab="")
      legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
      dev.off()
      
      png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
      
      layout(matrix(c(1,2),nrow=1),
             width=c(4,1)) 
      par(mar=c(5,4,4,0)) #No margin on the right side
      plot(rf_pc, 
           main = paste(current_date, alt, "PC", PC_selected, sep = " "))
      par(mar=c(5,0,4,2)) #No margin on the left side
      plot(c(0,1),type="n", axes=F, xlab="", ylab="")
      legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
      
      dev.off()
      
      # Plot variable importance
      png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
      varImpPlot(rf, main = paste(current_date, alt, sep=" "))
      
      dev.off()
      
      png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
      varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
      
      dev.off()
      
      
    }
  }
}

setwd(paste(out.dir, "RandomForests", sep = "/"))
write.csv(results_df, "Accuracy_RF_density_prediction.csv")


