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

in.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/datasets")
out.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/outputs")
fig.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/outputs/figures")


setwd(in.dir)
df <- read.csv("combined_densiplant_dataset.csv", header = T)
df <- df[,-1]
df <- arrange(df, dens)
df$dens <- as.character(df$dens)

df_15 <- df[df$Altitude == "15m",]
df_30 <- df[df$Altitude == "30m",]
df_50 <- df[df$Altitude == "50m",]

#### CORRELATIONS ######

# Most of the variables are non- normally distributed
# Will use spearman correlation (mon- parametric)

for(i in unique(df$Altitude)){
  data <- df[df$Altitude == i, ]
  print(i)
  
  
  for(j in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[j]
    subset_data <- data[data$Date == current_date,]
    
    cor_data <-  subset_data[, c(5, 8:ncol(subset_data) )]
    cor_data$dens <- as.numeric(cor_data$dens)
    cor <- cor(as.matrix(cor_data), method = "spearman")
    cor_pval <- rcorr(as.matrix(cor_data), type = "spearman")
    
    # setwd(paste(out.dir,"Exploration/correlations/corr", sep = "/"))
    # write.csv(data.frame(cor), paste("spearman_correlation",i, current_date, ".csv", sep = "_"))
    #
    # setwd(paste(out.dir,"Exploration/correlations/pval", sep = "/"))
    # write.csv(data.frame(cor_pval$P), paste("spearman_correlation",i, current_date, "pval", ".csv", sep = "_"))
    #

    # setwd(paste(fig.dir,"Exploration/correlations",i, sep = "/"))
    # 
    # png(paste(current_date, i, ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    # corrplot(cor, method="square", title = paste(i, current_date), mar=c(0,0,1,0))
    # dev.off()

  }
    
}



# Create a combined dataset with all density correlations

filePaths <- list.files(path=paste(out.dir,"Exploration/correlations/corr", sep = "/"),
                        pattern="*.csv",include.dirs=T)


cor_df <- NULL

for(filePath in filePaths){
  setwd(paste(out.dir,"Exploration/correlations/corr", sep = "/"))
  print(filePath)
  altitude <- str_extract(filePath, "[1-9][0-9]m")
  date <- str_extract(filePath, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")

  data <- read.csv(filePath, header = T)
  data <- data[, 1:2]
  data$altitude <- altitude
  data$date <- date
  
  setwd(paste(out.dir,"Exploration/correlations/pval", sep = "/"))
  data_pval <- read.csv(paste(str_replace_all(filePath, ".csv", ""), "pval_.csv", sep = ""), header = T)
  
  data$pval <- data_pval$dens
  
  cor_df <- rbind(cor_df, data)
  

}

setwd(paste(out.dir,"Exploration/correlations", sep = "/"))
write.csv(cor_df, "density_vegindex_spearman_cor_pval.csv")



## Date_alt combinations where |r|> 0.7 cor found selected ### 

# setwd(paste(out.dir,"Exploration/correlations", sep = "/"))
# cor_70 <- read.csv("density_vegindex_spearman_cor_pval_0.7.csv")
# 
# cor_70$ID <- paste(cor_70$alt, cor_70$date, sep = "_")
# 
# df$ID <- paste(df$Altitude, df$Date, sep = "_")
# 
# data <- df[df$ID %in% cor_70$ID,] 
# 
# data <- data[, -ncol(data)]
# 
# setwd(in.dir)

#write.csv(data, "combined_densiplant_dataset_cor70.csv")

#### RANDOM FORESTS WITH PC ### nice results #####  :)

cbPalette <- c("#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00", "#CC79A7")
densorder <- c("35","70","140", "280", "560")


# Classification per date & altitude data subset, not including variety
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
    
    ############# PCA & CLASSIFICATION ################
    
    split1<- sample(c(rep(0, 0.70 * nrow(subset_data)), rep(1, 0.30 * nrow(subset_data))))
    train <- subset_data[split1 == 0, ]
    
    train_pca <- train[,8:ncol(train)]
    
    test <- subset_data[split1 == 1, ]
    test_pca <- test[,8:ncol(test)]
    
    pca <- prcomp(train_pca, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    train.data <- data.frame(density = train$dens, pca$x[,1:PC_selected])
    
    test.data <- predict(pca, newdata = test_pca)
    test.data <- data.frame(test.data[,1:PC_selected])
    
    rf <- randomForest(density~., data = train.data, proximity=TRUE, ntree = 10000)
    
    # print(rf)
    
    # Just to check that values are classified correctly
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

#### RANDOM FORESTS WITH PC 2 ### 

densorder <- c("35","70","140", "280", "560")


# Classification per date subset, including altitude and variety as factors
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
  
  ############# PCA & CLASSIFICATION ################
  
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
  
  rf <- randomForest(density~., data = train.data, proximity=TRUE, ntree = 10000)
  
  # print(rf)
  
  # Just to check that values are classified correctly
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


# DECISSION (CLASSIFICATION) TREES: Classification per date only, including variety and flight height as factors
# tried with PC as well and doesn't yield very good results

cbPalette <- c("#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00", "#CC79A7")
densorder <- c("35","70","140", "280", "560")
data <- df
set.seed(123)
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

# CONDITIONAL INFERENCE TREES #
# Tried including variety and flight height as factors but did not result in any improvement

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

    class.data <- subset_data[,c(5,8:ncol(subset_data))]
    
    ConInfTree <- ctree(dens ~ ., data = class.data)
    plot(ConInfTree, main = paste(current_date, i, sep = "_"))
  }
  
  
}

# CONDITIONAL INFERENCE TREES WITH PC  including flight height and variety for the PCA#

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

# CONDITIONAL INFERENCE TREES WITH PC NOT including flight height and variety for the PCA#

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


