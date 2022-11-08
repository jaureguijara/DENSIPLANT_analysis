library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(afex)
library(performance) 
library(stringr)
library(plyr)
library(emmeans)

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
df$Date <- as.Date(df$Date)

df_15 <- df[df$Altitude == "15m",]
df_30 <- df[df$Altitude == "30m",]
df_50 <- df[df$Altitude == "50m",]

data <- df[, c(1:7, 9, 17, 19, 21)]

##### 1. Data exploration for ANOVA ####

## CHECK ANOVA ASSUMPTIONS 

assumptions <- NULL

for(i in unique(df$Altitude)){
  data <- df[df$Altitude == i, ]
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    print(current_date)
    subset_data <- data[data$Date == current_date,]
    
    for(i in 8:ncol(subset_data)){
      name <- colnames(subset_data)[i]
      variable <- subset_data[,i]
      
      h <- leveneTest(variable ~ dens*var, subset_data)
      n <- shapiro.test(variable)
      
      ass <- c(name, c(unique(data$Altitude)),current_date, n$p.value, h$`Pr(>F)`[1])
      
      assumptions <- rbind(assumptions, ass)
    }
  }
}

assumptions <- data.frame(assumptions)
rownames(assumptions) <- NULL
colnames(assumptions) <- c("variable", "altitude", "date", "norm_pvalue", "homo_pvalue")
setwd(paste(out.dir, "Exploration", sep ="/"))
write.csv(assumptions,"assumptions_check.csv")



# CHECK ASSUMPTIONS PER RESPONSE VARIABLES

assumptions <- NULL

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]
  
  for(i in 8:ncol(subset_data)){
    name <- colnames(subset_data)[i]
    variable <- subset_data[,i]
    
    if(length(unique(subset_data$Altitude))>1){
      an <- aov(variable ~ var*dens*Altitude + Rows, data = subset_data)
      
      h <- leveneTest(variable ~ dens*var*Altitude, subset_data)
      n <- shapiro.test(an$residuals)
      plot(hist(an$residuals))
    }
    
    else {
      an <- aov(variable ~ var*dens + Rows, data = subset_data)
      
      h <-leveneTest(variable ~ dens*var, subset_data)
      n <- shapiro.test(an$residuals)
      plot(hist(an$residuals))
    }
    
    ass <- c(name,as.character(current_date), n$p.value, h$`Pr(>F)`[1])
    
    assumptions <- rbind(assumptions, ass)
  }
}
assumptions <- data.frame(assumptions)
rownames(assumptions) <- NULL
colnames(assumptions) <- c("variable", "date", "norm_pvalue", "homo_pvalue")
assumptions$date <- as.Date(assumptions$date)
setwd(paste(out.dir, "Exploration", sep ="/"))
write.csv(assumptions,"assumptions_check_2.csv")


# CHECK ASSUMPTIONS FOR RESIDUALS

assumptions <- NULL
for(j in unique(df$Altitude)){
  data <- df[df$Altitude == j,]
  
  for(i in 1:length(unique(data$Date))){
    current_date <- unique(data$Date)[i]
    print(current_date)
    
    # Subset data per date 
    
    subset_data <- data[data$Date == current_date,]
    
    for(i in 8:ncol(subset_data)){
      name <- colnames(subset_data)[i]
      variable <- subset_data[,i]
      
      an <- aov(variable ~ var*dens + Rows, data = subset_data)
      
      h <-leveneTest(variable ~ dens*var, subset_data)
      n <- shapiro.test(an$residuals)
      plot(hist(an$residuals))
      
      
      ass <- c(name,as.character(current_date), unique(data$Altitude), n$p.value, h$`Pr(>F)`[1])
      
      assumptions <- rbind(assumptions, ass)
    }
  }
  
}

assumptions <- data.frame(assumptions)
rownames(assumptions) <- NULL
colnames(assumptions) <- c("variable", "date", "Altitude", "norm_pvalue", "homo_pvalue")
assumptions$date <- as.Date(assumptions$date)
setwd(paste(out.dir, "Exploration", sep ="/"))
write.csv(assumptions,"assumptions_check_3.csv")

variable  <- "TGI"
subset_data <- df[df$Date == "2022-05-03",]
an <- aov(1/(TGI) ~ var*dens + Rows, data = subset_data)

h <-leveneTest(sqrt(TGI) ~ dens*var, subset_data)
n <- shapiro.test(an$residuals)
plot(hist(an$residuals))
plot(an, which =2)
hist(1/(subset_data$TGI))
shapiro.test(1/(subset_data$TGI))

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

##### 2. Analysis of Machine Learning algorithm performance ####


#### 2.1 CART #####

work.dir <- paste(out.dir, "CART", sep ="/")
setwd(paste(work.dir, "results", sep ="/"))

## to combine all datasets ## 

# filePaths <- list.files(path = paste(work.dir, "results", sep ="/"), pattern="*.csv", include.dirs = T)
# 
# df <- NULL
# 
# for(filePath in filePaths){
#   print(filePath)
#   cv <- gsub("_","", gsub("-", "",str_extract(filePath, "_.*-")[]))
# 
#   split <- gsub("-", "", gsub(".csv", "",str_extract(filePath, "-.*")))
# 
# 
#   currentDF <- read.csv(filePath, header = T)
# 
#   firstcolname <- colnames(currentDF)[1]
# 
#   if(firstcolname == "X"){
#     currentDF <- currentDF[,-1]
#   }
# 
#   currentDF$CV <- cv
#   currentDF$split <- split
# 
#   print(ncol(currentDF))
#   #print(head(currentDF))
#   df <- rbind(df, currentDF)
# 
# }

 setwd(work.dir)
# write.csv(df, "CART_results_combination.csv")

df <- read.csv("CART_results_combination.csv", header = T)
df <- df[,-c(1,13,14)]

# Compare training performance

data_train<- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_train, Kappa_train, ID) 

data_mean_train <- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_train, Kappa_train, ID) %>% 
  ddply("ID", summarise, 
        N =length(ID), 
        mean_accuracy = mean(Accuracy_train),
        sd_accuracy = sd(Accuracy_train),
        mean_kappa = mean(Kappa_train),
        sd_kappa = sd(Kappa_train))

data_train <- data_train[data_train$Accuracy_train > 0.5,]

an <- aov(Accuracy_train ~ ID, data = data_train) # p-value  < 2.2e-16
anova(an)

contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "bonferroni")

summary_train <- merge(data_mean_train, contrasts$lsmeans, by  = "ID")
pval_train <- data.frame(contrasts$contrasts)
#significant_test <- significant_test[significant_test$p.value < 0.05,]

write.csv(summary_train, "Training_performance_summary.csv")
write.csv(pval_train, "Training_performance_contrasts.csv")

# Compare testing performance

data_test<- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_test, Kappa_test, ID) %>% 
  na.omit()

data_mean_test <- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_test, Kappa_test, ID) %>% 
  ddply("ID", summarise, 
        N =length(Accuracy_test), 
        mean_accuracy = mean(Accuracy_test),
        sd_accuracy = sd(Accuracy_test),
        mean_kappa = mean(Kappa_test),
        sd_kappa = sd(Kappa_test)) %>% 
  na.omit()

data_test <- data_test[data_test$Accuracy_test > 0.5,]

an <- aov(Accuracy_test ~ ID, data = data_test) #  significant differences found among models with accuracy above 0.5 p-value = 0.0006303
anova(an)

contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "bonferroni")

summary_test <- merge(data_mean_test, contrasts$lsmeans, by  = "ID")
significant_test <- data.frame(contrasts$contrasts) # All p-values are 1

#significant_test <- significant_test[significant_test$p.value < 0.05,]

write.csv(summary_test, "Testing_performance_summary.csv")
write.csv(significant_test, "Testing_performance_contrasts.csv")

# -------------------------------------------------------------------------------------------------------------------

###### 2.2 ctree ####

work.dir <- paste(out.dir, "ctree", sep ="/")

# setwd(paste(work.dir, "results", sep ="/"))
# 
# ## to combine all datasets ## 
# 
# filePaths <- list.files(path = paste(work.dir, "results", sep ="/"), pattern="*.csv", include.dirs = T)
# 
# df <- NULL
# 
# for(filePath in filePaths){
#   print(filePath)
#   cv <- gsub("_","", gsub("-", "",str_extract(filePath, "_.*-")[]))
# 
#   split <- gsub("-", "", gsub(".csv", "",str_extract(filePath, "-.*")))
# 
# 
#   currentDF <- read.csv(filePath, header = T)
# 
#   firstcolname <- colnames(currentDF)[1]
# 
#   if(firstcolname == "X"){
#     currentDF <- currentDF[,-1]
#   }
# 
#   currentDF$CV <- cv
#   currentDF$split <- split
# 
#   print(ncol(currentDF))
#   #print(head(currentDF))
#   df <- rbind(df, currentDF)
# 
# }
# 
setwd(work.dir)
# write.csv(df, "ctree_results_combination.csv")

df <- read.csv("ctree_results_combination.csv", header = T)
df <- df[,-c(1,13,14)]

# Compare training performance

data_train<- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_train, Kappa_train, ID) 

data_mean_train <- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_train, Kappa_train, ID) %>% 
  ddply("ID", summarise, 
        N =length(ID), 
        mean_accuracy = mean(Accuracy_train),
        sd_accuracy = sd(Accuracy_train),
        mean_kappa = mean(Kappa_train),
        sd_kappa = sd(Kappa_train))

data_train <- data_train[data_train$Accuracy_train > 0.5,]

an <- aov(Accuracy_train ~ ID, data = data_train)  # p-value  < 2.2e-16
anova(an)

contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "bonferroni")

summary_train <- merge(data_mean_train, contrasts$lsmeans, by  = "ID")
significant_test <- data.frame(contrasts$contrasts)
#significant_test <- significant_test[significant_test$p.value < 0.05,]

write.csv(summary_train, "Training_performance_summary.csv")
write.csv(significant_test, "Training_performance_contrasts.csv")

# Compare testing performance

data_test<- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_test, Kappa_test, ID) %>% 
  na.omit()

data_mean_test <- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, CV, split, sep = "_")) %>% 
  dplyr::select(Accuracy_test, Kappa_test, ID) %>% 
  ddply("ID", summarise, 
        N =length(Accuracy_test), 
        mean_accuracy = mean(Accuracy_test),
        sd_accuracy = sd(Accuracy_test),
        mean_kappa = mean(Kappa_test),
        sd_kappa = sd(Kappa_test)) %>% 
  na.omit()

data_test <- data_test[data_test$Accuracy_test > 0.5,]

an <- aov(Accuracy_test ~ ID, data = data_test) # no significant differences found among models with accuracy above 0.5 p-value = 0.1735
anova(an)

#contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "bonferroni")

summary_test <- merge(data_mean_test, contrasts$lsmeans, by  = "ID")
#significant_test <- data.frame(contrasts$contrasts)
#significant_test <- significant_test[significant_test$p.value < 0.05,]

write.csv(summary_test, "Testing_performance_summary.csv")
#write.csv(significant_test, "Testing_performance_contrasts.csv")

# -------------------------------------------------------------------------------------------------------------------

###### 2.3 RANDOM FOREST ####


work.dir <- paste(out.dir, "RandomForests", sep ="/")

setwd(paste(work.dir, "results", sep ="/"))

## to combine all datasets ##

filePaths <- list.files(path = paste(work.dir, "results", sep ="/"), pattern="*.csv", include.dirs = T)

df <- NULL

for(filePath in filePaths){
  print(filePath)

  split <- gsub("-", "", gsub(".csv", "",str_extract(filePath, "-.*")))


  currentDF <- read.csv(filePath, header = T)

  firstcolname <- colnames(currentDF)[1]

  if(firstcolname == "X"){
    currentDF <- currentDF[,-1]
  }

  currentDF$split <- split

  #print(ncol(currentDF))
  #print(head(currentDF))
  df <- rbind(df, currentDF)

}

setwd(work.dir)
#write.csv(df, "rf_results_combination.csv")

df <- read.csv("rf_results_combination.csv", header = T)
df <- df[,-c(1,2)]

# Compare  performance

data<- df %>% 
  mutate(PC = ifelse(str_detect(PC, "0") , "var", "PC")) %>% 
  mutate(ID = paste(date, alt, PC, split, sep = "_")) %>% 
  dplyr::select(-date, -alt, -PC, -split,) 

data_mean<- data %>% 
  dplyr::select(OOB, Accuracy_test,AccuracyPValue_test, Kappa_test, ID) %>% 
  ddply("ID", summarise, 
        N =length(ID),
        mean_OOB = mean(OOB),
        sd_OOB = sd(OOB),
        mean_accuracy = mean(Accuracy_test),
        sd_accuracy = sd(Accuracy_test),
        mean_accpval = mean(AccuracyPValue_test),
        sd_accpval = sd(AccuracyPValue_test),
        mean_kappa = mean(Kappa_test),
        sd_kappa = sd(Kappa_test))

#data_train <- data_train[data_train$Accuracy_train > 0.5,]

an <- aov(Accuracy_test ~ ID, data = data)  # p-value  < 2.2e-16
anova(an)

contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "bonferroni")

summary <- merge(data_mean, contrasts$lsmeans, by  = "ID")
significant <- data.frame(contrasts$contrasts)
#significant_test <- significant_test[significant_test$p.value < 0.05,]

write.csv(summary, "performance_summary.csv")
write.csv(significant, "performance_contrasts.csv")


## Tukey comparisons for first 16 groups ##

data_17 <- data[data$ID == "22/03/2022_50m_var_8020split"|
                data$ID =="22/03/2022_50m_PC_8020split"|
                data$ID =="22/03/2022_50m_var_7030split"|
                data$ID =="22/03/2022_50m_PC_7030split"|
                data$ID =="09/03/2022_30m_var_7030split"|
                data$ID =="05/04/2022_50m_var_7030split"|
                data$ID =="09/03/2022_15m_var_7030split"|
                data$ID =="05/04/2022_50m_var_8020split"|
                data$ID =="09/03/2022_30m_var_8020split"|
                data$ID =="09/03/2022_15-30m_var_7030split"|
                data$ID =="09/03/2022_30m_PC_8020split"|
                data$ID =="09/03/2022_15-30m_var_8020split"|
                data$ID =="05/04/2022_50m_PC_7030split"|
                data$ID =="19/04/2022_50m_var_7030split"|
                data$ID =="09/03/2022_30m_PC_7030split"|
                data$ID =="09/03/2022_15m_PC_7030split",]

an <- aov(Accuracy_test ~ ID, data = data_17)  # p-value  3.291e-07
anova(an)

contrasts <- lsmeans::lsmeans(an, pairwise~ID, adjust = "tukey")

summary <- merge(data_mean, contrasts$lsmeans, by  = "ID")
significant <- data.frame(contrasts$contrasts)
#significant <- significant[significant$p.value < 0.05,]
write.csv(significant, "performance_first16_contrasts_tukey.csv")
