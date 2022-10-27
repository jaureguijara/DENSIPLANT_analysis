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



