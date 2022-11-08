library(moments)
library(ggpubr)
library(broom)
library(dplyr)
library(ggplot2)

if(Sys.info()["user"] == "Jara"){
  in.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/outputs/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs/figures")
}

cbPalette <- c("#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00", "#CC79A7")
densorder <- c("35","70","140", "280", "560")

setwd(in.dir)

df <- read.csv("combined_densiplant_dataset.csv", header = T)
df <- df[,-1]
df <- arrange(df, dens)
df$dens <- as.character(df$dens)

df_15 <- df[df$Altitude == "15m",]
df_30 <- df[df$Altitude == "30m",]
df_50 <- df[df$Altitude == "50m",]


##### Visualize data separating per altitude #####

data <- df_15 # change depending on the altitude 

# Nested for-loop to visualize data per date and variable (index)

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  print(current_date)
  
  # Subset data per date 
  
  subset_data <- data[data$Date == current_date,]


  for(i in 8:ncol(subset_data)){
    name <- colnames(subset_data)[i]
    variable <- subset_data[,i]
    
    # Data visualization: Boxplot per date & variable
    
    p1 <- ggplot(aes(y = variable, x = dens, fill = var), data = subset_data) + geom_boxplot() +
      scale_x_discrete(limits=densorder) +
      labs(y=name, x="Density") +
      ggtitle(paste(current_date, unique(subset_data$Altitude), name, sep="_"))
    
    setwd(paste(fig.dir, "Exploration/Boxplots", unique(subset_data$Altitude), sep ="/"))
    png(paste(current_date, unique(subset_data$Altitude), name, ".png", sep="_"), width = 6.5, height = 5.5, units = 'in', res = 300)
    plot(p1)
    dev.off()
    
    
  }
  
}

#### Visualize time-dependent data separating per altitude ####

data <- df_15


#### Visualize time-dependent data all both altitudes at the same time ####

data <- df_50

for(i in 8:ncol(data)){
  name <- colnames(data)[i]
  variable <- data[,i]
  
  subset_data <- data[, c("Date", "var", "dens", name)]
  colnames(subset_data)[4] <- "value"
  
  subset_data <- subset_data %>% 
    group_by(Date,var,dens) %>% 
    summarise(mean=mean(value),
              sd=sd(value))
  
  subset_data$Date <- as.Date(subset_data$Date)
  subset_data$dens <- factor(subset_data$dens, levels = densorder)
  
  p2 <-  ggplot(aes(y = mean, x = Date, color = dens), data = subset_data) +
    facet_wrap(.~ var, ncol =3) +
    geom_point()+
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))+
    scale_color_manual(values=cbPalette)+
    labs(y=name, x="Date") +
    ggtitle(paste(name, data$Altitude, sep = "_")) 
  
  
  setwd(paste(fig.dir, "Exploration", "VegIndexes", data$Altitude, sep ="/"))
  png(paste(name, data$Altitude, ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
  plot(p2)
  dev.off()
}
  
#### Visualize time-dependent data all altitudes at the same time ####
data <- df

for(i in 8:ncol(data)){
  name <- colnames(data)[i]
  variable <- data[,i]
  
  subset_data <- data[, c("Date", "var", "dens", "Altitude", name)]
  colnames(subset_data)[5] <- "value"
  
  subset_data <- subset_data %>% 
  group_by(Date,var,dens, Altitude) %>% 
    summarise(mean=mean(value),
              sd=sd(value))
  
  subset_data$Date <- as.Date(subset_data$Date)
  subset_data$dens <- factor(subset_data$dens, levels = densorder)
  
  # change color, facet_wrap and line color depending on separation per dens or var, or only altitude
  
  p3 <-  ggplot(aes(y = mean, x = Date, color = dens), data = subset_data) +
    facet_wrap(.~ Altitude, ncol =3) +
    geom_point()+
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))+
    scale_color_manual(values=cbPalette)+
    geom_line(aes(color = dens, linetype = var), size = 1) +
    scale_linetype_manual(values=c("solid", "dotted", "dashed"))+
    #scale_x_discrete(limits=densorder) +
    labs(y=name, x="Date") +
    ggtitle(name) 
  
  setwd(paste(fig.dir, "Exploration", "VegIndexes", data$Altitude, sep ="/"))
  #    setwd(paste(fig.dir, "per_variety", sep ="/"))
  png(paste(name, ".png", sep="_"), width = 15, height = 10, units = 'in', res = 300)
  plot(p3)
  dev.off()
  

}



## Plot density vs parameter correlations across time ###
setwd(paste(out.dir, "Exploration/correlations", sep = "/"))

df_cor <- read.csv("density_vegindex_spearman_cor_pval.csv", header = T)
df_cor <- df_cor[,-1]

parameters <- unique(df_cor$X)
parameters <- parameters[-1]
colnames(df_cor)[1] <- "VegIndex"

subset_data <- df_cor[df_cor$VegIndex == parameters[2] |
                      df_cor$VegIndex == parameters[5] |
                      df_cor$VegIndex == parameters[9] |
                      df_cor$VegIndex == parameters[11] |
                      df_cor$VegIndex == parameters[12] |
                      df_cor$VegIndex == parameters[14],]


VegIndexorder <- c("Hue", "a.", "GA", "CSI", "NGRDI", "TGI")
subset_data$VegIndex <- factor(subset_data$VegIndex, levels = VegIndexorder)
p4 <- ggplot(aes(y = dens, x = as.Date(date), colour = VegIndex), data = subset_data) + 
  geom_point(data= subset_data[subset_data$pval<0.05,])+
  geom_line()+
  facet_wrap(~altitude)+
  scale_color_manual(values=cbPalette)+
  labs(y="Spearman's ρ", x="Time") +
  geom_hline(yintercept=0, color = "red")+
  ggtitle("Density - VegIndex significant correlations")

setwd(paste(fig.dir,"Exploration/correlations", sep = "/"))
png("Density-VegIndex_significant.png", width = 8, height = 6, units = 'in', res = 300)
plot(p4)
dev.off()





