library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(afex)
library(performance) 

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

##### CHECK ANOVA ASSUMPTIONS #######

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
