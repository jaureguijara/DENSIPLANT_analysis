library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(car)
library(Hmisc)
library(stringr)
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
df <- arrange(df, Density)
df$Density<- as.character(df$Density)

####### RANDOM FORESTS ########

densorder <- c("35","70","140", "280", "560")

set.seed(12345) 
results_df <- NULL
error_df <- NULL
importance_df <- NULL

data <- df
round <-1

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
      
      rf_data <- subset_alt[,-c(2,3,6,7)]
      
      rf_data_alt_tocol <- rf_data 
      colnames(rf_data_alt_tocol)[4:ncol(rf_data_alt_tocol)] <- paste(colnames(rf_data_alt_tocol)[4:ncol(rf_data_alt_tocol)], current_alt, sep = "_")
      alt_combi_subset[[j]] <- rf_data_alt_tocol #to merge de dates having different index columns per altitude later
      
      rf_data$Density <- as.numeric(as.character(rf_data$Density)) # to sort the data
      rf_data <- rf_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      rf_data$Density <- as.factor(as.character(rf_data$Density)) # to use for the model
      
      print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
      
      # Split the data 
      
      split<- sample(c(rep(0, 0.70 * nrow(rf_data)), rep(1, 0.30 * nrow(rf_data))))
      
      train <- rf_data[split == 0, ]
      train_nodens <- dplyr::select(train, -Density)
      
      test <- rf_data[split == 1, ]
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
      
      # RANDOM FOREST
      
      rf <- randomForest(Density~., data = train, proximity=TRUE, 
                         ntree = 5000, importance = TRUE)
      rf_pc <- randomForest(Density~., data = train_pca_data, proximity=TRUE, 
                            ntree = 5000, importance = TRUE)
      
      
      # prediction
      
      pred <- predict(rf, newdata = test_nodens)
      pred_pc <- predict(rf_pc, newdata = test_pca_data)
      
      cm <- confusionMatrix(pred, test$Density, mode = "everything")
      cm_pc <- confusionMatrix(pred_pc, test$Density, mode = "everything")
      
      # Plot and save
      
      alt <- unique(subset_alt$Altitude)
      
      
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
        tibble::rownames_to_column("Genotype")
      
      importance_pc <- data.frame(rf_pc$importance) %>% 
        dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
        mutate(date = current_date,
               alt = alt,
               PC = PC_selected, 
               mtry = rf_pc$mtry)%>% 
        tibble::rownames_to_column("Genotype")
      
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

      setwd(paste(fig.dir, "RandomForests/70-30split_altsingle", sep = "/"))
      
      if(round == 1){
       
         # Plot error
        png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        
        layout(matrix(c(1,2),nrow=1),
               width=c(4,1))
        par(mar=c(5,4,4,0)) #No margin on the right side
        plot(rf,
             main = paste(current_date, alt, sep = " "), ylim = c(0,1))
        par(mar=c(5,0,4,2)) #No margin on the left side
        plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        dev.off()
        
        png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        
        layout(matrix(c(1,2),nrow=1),
               width=c(4,1))
        par(mar=c(5,4,4,0)) #No margin on the right side
        plot(rf_pc,
             main = paste(current_date, alt, "PC", PC_selected, sep = " "), ylim = c(0,1))
        par(mar=c(5,0,4,2)) #No margin on the left side
        plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        
        dev.off()
        
        # Plot Variable importance
        png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 8, units = 'in', res = 300)
        varImpPlot(rf, main = paste(current_date, alt, sep=" "))
        
        dev.off()
        
        png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 8, units = 'in', res = 300)
        varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
        
        dev.off()
        
      }
    }
    
    # 2 Combining altitudes for same dates. 
    
    if(length(unique(subset_date$Altitude)) >1){
      for(i in 1:length(alt_combi_subset)){
       if(i == 1){
         rf_data_combi <-data.frame(alt_combi_subset[i])
       }
        else{
          rf_data_combi <- merge(rf_data_combi,data.frame(alt_combi_subset[i]), 
                                 by = c("Image.Name", "Genotype", "Density"))
        }
      }
      
      rf_data <- rf_data_combi 
      rf_data$Density <- as.numeric(as.character(rf_data$Density)) # to sort the data
    
      rf_data <- rf_data[,-1] %>% 
        arrange(Genotype) %>% 
        arrange(Density)
      
      rf_data$Density <- as.factor(as.character(rf_data$Density)) # to use for the model
      
      # Split the data, we use the same split as before. 
      
      train <- rf_data[split == 0, ]
      train_nodens <- dplyr::select(train, -Density)
      
      test <- rf_data[split == 1, ]
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
      
      # RANDOM FOREST
      
      rf <- randomForest(Density~., data = train, proximity=TRUE, 
                         ntree = 5000, importance = TRUE)
      rf_pc <- randomForest(Density~., data = train_pca_data, proximity=TRUE, 
                            ntree = 5000, importance = TRUE)
      
      
      # prediction
      
      pred <- predict(rf, newdata = test_nodens)
      pred_pc <- predict(rf_pc, newdata = test_pca_data)
      
      cm <- confusionMatrix(pred, test$Density, mode = "everything")
      cm_pc <- confusionMatrix(pred_pc, test$Density, mode = "everything")
      
      # Plot and save
      alt <- unique(subset_date$Altitude)
      if(length(alt) == 2){
        alt <- "15-30m"
      }
      
      else {
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
        tibble::rownames_to_column("Genotype")
      
      importance_pc <- data.frame(rf_pc$importance) %>% 
        dplyr::select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
        mutate(date = current_date,
               alt = alt,
               PC = PC_selected, 
               mtry = rf_pc$mtry)%>% 
        tibble::rownames_to_column("Genotype")
      
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
      
      setwd(paste(fig.dir, "RandomForests/70-30split_altcombi", sep = "/"))

      if(round == 1){
        
        # Plot error
        png(paste(current_date, alt, "RF", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        
        layout(matrix(c(1,2),nrow=1),
               width=c(4,1))
        par(mar=c(5,4,4,0)) #No margin on the right side
        plot(rf,
             main = paste(current_date, alt, sep = " "), ylim = c(0,1))
        par(mar=c(5,0,4,2)) #No margin on the left side
        plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        dev.off()
        
        png(paste(current_date, alt, "RF_PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
        
        layout(matrix(c(1,2),nrow=1),
               width=c(4,1))
        par(mar=c(5,4,4,0)) #No margin on the right side
        plot(rf_pc,
             main = paste(current_date, alt, "PC", PC_selected, sep = " "), ylim = c(0,1))
        par(mar=c(5,0,4,2)) #No margin on the left side
        plot(c(0,1),type="n", axes=F, xlab="", ylab="")
        legend("topright", colnames(rf_pc$err.rate),col=1:6,cex=0.8,fill=1:6)
        
        dev.off()
        
        # Plot Variable importance
        png(paste(current_date, alt, "RF_varimp", ".png", sep="_"), width = 8, height = 8, units = 'in', res = 300)
        varImpPlot(rf, main = paste(current_date, alt, sep=" "))
        
        dev.off()
        
        png(paste(current_date, alt, "RF_varimp_PC", ".png", sep="_"), width = 8, height = 8, units = 'in', res = 300)
        varImpPlot(rf_pc, main = paste(current_date, alt,"PC", PC_selected, sep=" "))
        
        dev.off()
      }
    }  
  }
  round <- round +1
}



# to take a quick look
aggregate(results_df$Accuracy, list(results_df$date), FUN=mean) 

setwd(paste(out.dir, "RandomForests/results/raw", sep = "/"))
write.csv(error_df, "OOB_error_rate_10reps_7030split_alt_single_combi.csv")
write.csv(results_df, "Accuracy_RF_Density_prediction_10reps_7030split_alt_single_combi.csv")
write.csv(importance_df, "var_importance_7030split_alt_single_combi.csv")
  
  