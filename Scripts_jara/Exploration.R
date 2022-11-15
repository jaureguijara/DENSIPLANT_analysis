library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(car)
library(Hmisc)
library(stringr)
library(corrplot)
library(ggfortify)


if(Sys.info()["user"] == "Jara"){
  in.dir <- file.path("F:/DENSIPLANT/2022_DENSIPLANT_herve_jara/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("F:/DENSIPLANT/2022_DENSIPLANT_herve_jara/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("F:/DENSIPLANT/2022_DENSIPLANT_herve_jara/DENSIPLANT_analysis/outputs/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/datasets")
  out.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/outputs/figures")
}

setwd(in.dir)
df <- read.csv("combined_densiplant_dataset.csv", header = T)
df <- df[,-1]
df <- arrange(df, Density)
df$Density <- as.character(df$Density)

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
    cor_data$Density <- as.numeric(cor_data$Density)
    cor <- cor(as.matrix(cor_data), method = "spearman")
    cor_pval <- rcorr(as.matrix(cor_data), type = "spearman")
    
    setwd(paste(out.dir,"Exploration/correlations/corr", sep = "/"))
    write.csv(data.frame(cor), paste("spearman_correlation",i, current_date, ".csv", sep = "_"))

    setwd(paste(out.dir,"Exploration/correlations/pval", sep = "/"))
    write.csv(data.frame(cor_pval$P), paste("spearman_correlation",i, current_date, "pval", ".csv", sep = "_"))


    setwd(paste(fig.dir,"Exploration/correlations",i, sep = "/"))

    png(paste(current_date, i, ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    corrplot(cor, method="square", title = paste(i, current_date), mar=c(0,0,1,0))
    dev.off()

  }
    
}



# Create a combined dataset with all Density correlations

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
  
  data$pval <- data_pval$Density
  
  cor_df <- rbind(cor_df, data)
  

}

setwd(paste(out.dir,"Exploration/correlations", sep = "/"))
write.csv(cor_df, "Density_vegindex_spearman_cor_pval.csv")


####### PCA #######

library(FactoMineR)
library(factoextra)

cbPalette <- c("#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00", "#CC79A7")
densorder <- c("35","70","140", "280", "560")
data <- df

eigenval_df <- NULL
contrib_df <- NULL

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  
  # Subset data per date 
  
  subset_date <- data[data$Date == current_date,]
  subset_date$Density <- factor(subset_date$Density, levels = densorder)
  alt_combi_subset <- NULL #to merge de dates having different index columns per altitude later
  
  # 1 Separating per date and altitude
  
  for(j in 1:length(unique(subset_date$Altitude))){
    
    current_alt <- unique(subset_date$Altitude)[j]
    
    subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
    
    pca_data <- subset_alt[,-c(2,3,6,7)]
    
    pca_data_alt_tocol <- pca_data 
    colnames(pca_data_alt_tocol)[4:ncol(pca_data_alt_tocol)] <- paste(colnames(pca_data_alt_tocol)[4:ncol(pca_data_alt_tocol)], 
                                                                        current_alt, sep = "_")
    alt_combi_subset[[j]] <- pca_data_alt_tocol #to merge de dates having different index columns per altitude later
    
    print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
    
    pca_data <- pca_data  %>% 
      dplyr::select(-Density, -Image.Name, -Genotype)
    
    
    # PCA
    pca <- prcomp(pca_data,  center = T, scale. = T)
    res.pca <- PCA(pca_data,scale. = T,graph = FALSE)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    var_explained <- eigenval/sum(eigenval)
    var_explained <- paste(as.character(round(sum(var_explained[1:PC_selected])*100,2)), "%", sep = "")
    var_explained <- paste(var_explained, "explained", sep = " ")
    
    # Plot PCA
    setwd(paste(fig.dir, "Exploration/PCA/altsingle", sep ="/"))
    p1 <- autoplot(pca, data = subset_alt, colour = 'Density',
                   shape = "Genotype",
                   main =  paste(current_date, 
                                 current_alt, "PC", PC_selected, var_explained, sep = "_")) +
      scale_color_manual(values=cbPalette)
    

    png(paste(current_date,current_alt, "PCA", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p1)
    dev.off()
    
    p2 <- fviz_contrib(pca, choice = "var", axes = 1:2) + ggtitle(paste("Contributions 2PC ", current_date, 
                                                                        current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions2PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p2)
    dev.off()
    
    p3 <- fviz_pca_var(pca, col.var = "black", repel = TRUE)
    
    png(paste(current_date,current_alt, "Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p3)
    dev.off()
    
    p4 <- fviz_pca_biplot(pca, geom.ind = as.factor(subset_alt$Density),
                    col.ind = as.factor(subset_alt$Density), 
                    fill.ind = as.factor(subset_alt$Density),
                    palette = cbPalette, 
                    addEllipses = TRUE, col.var = "black", repel = TRUE, 
                    legend.title = "Density") +
      geom_point(aes(shape = factor(subset_alt$Genotype), colour = factor(subset_alt$Density)))+ 
      guides(shape = guide_legend(title = "Genotype")) + ggtitle(paste(current_date, 
                                                                       current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "PCA-Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p4)
    dev.off()
    
    p5 <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) + ggtitle(paste(current_date, 
                                                                            current_alt, "PC", PC_selected, var_explained, sep = "_")) 
    png(paste(current_date,current_alt, "Scree_plot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p5)
    dev.off()
    
    # Contributions of variables to PC1
    p6 <- fviz_contrib(res.pca, choice = "var", axes = 1)+ ggtitle(paste("Contributions 1st PC ", current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions_PC1", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p6)
    dev.off() 
    # Contributions of variables to PC2
    p7 <- fviz_contrib(res.pca, choice = "var", axes = 2)+ ggtitle(paste("Contributions 2nd PC ", current_date, current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "Contributions_PC2", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p7)
    dev.off() 
    
    # SAVE
    alt <- unique(subset_alt$Altitude)
    
    eigenval_result <- data.frame(res.pca$eig)
    eigenval_result$Date <- current_date
    eigenval_result$Altitude <- alt
    eigenval_result <- tibble::rownames_to_column(eigenval_result, "Component")
    eigenval_df <- rbind(eigenval_df, eigenval_result)
    
    contrib_result <- data.frame(res.pca$var$contrib)
    contrib_result$Date <- current_date
    contrib_result$Altitude <- alt
    contrib_result <- tibble::rownames_to_column(contrib_result, "Variable")
    contrib_df <- rbind(contrib_df, contrib_result)
    
  }
  
  # 2 Combining altitudes for same dates. 
  
  if(length(unique(subset_date$Altitude)) >1){
    for(i in 1:length(alt_combi_subset)){
      if(i == 1){
        pca_data_combi <-data.frame(alt_combi_subset[i])
      }
      else{
        pca_data_combi <- merge(pca_data_combi,data.frame(alt_combi_subset[i]), 
                                 by = c("Image.Name", "Genotype", "Density"))
      }
    }
    
    pca_data <- pca_data_combi 
    pca_data_combi$Density <- factor(pca_data_combi$Density, levels = densorder) # to sort the data
    
    pca_data <- pca_data%>% 
      dplyr::select(-Density, -Genotype, -Image.Name)
    
    # PCA
    
    pca <- prcomp(pca_data,  center = T, scale. = T)
    res.pca <- PCA(pca_data,scale. = T,graph = FALSE)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    var_explained <- eigenval/sum(eigenval)
    var_explained <- paste(as.character(round(sum(var_explained[1:PC_selected])*100,2)), "%", sep = "")
    var_explained <- paste(var_explained, "explained", sep = " ")
    
    # Plot PCA 
    setwd(paste(fig.dir, "Exploration/PCA/altcombi", sep ="/"))
    
    p1 <- autoplot(pca, data = subset_alt, colour = 'Density',
                   shape = "Genotype",
                   main =  paste(current_date, 
                                 current_alt, "PC", PC_selected, var_explained, sep = "_")) +
      scale_color_manual(values=cbPalette)
    
    
    png(paste(current_date,current_alt, "PCA", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p1)
    dev.off()
    
    p2 <- fviz_contrib(pca, choice = "var", axes = 1:2) + ggtitle(paste("Contributions 2PC ", current_date, 
                                                                        current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions2PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p2)
    dev.off()
    
    p3 <- fviz_pca_var(pca, col.var = "black", repel = TRUE)
    
    png(paste(current_date,current_alt, "Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p3)
    dev.off()
    
    p4 <- fviz_pca_biplot(pca, geom.ind = as.factor(subset_alt$Density),
                          col.ind = as.factor(subset_alt$Density), 
                          fill.ind = as.factor(subset_alt$Density),
                          palette = cbPalette, 
                          addEllipses = TRUE, col.var = "black", repel = TRUE, 
                          legend.title = "Density") +
      geom_point(aes(shape = factor(subset_alt$Genotype), colour = factor(subset_alt$Density)))+ 
      guides(shape = guide_legend(title = "Genotype")) + ggtitle(paste(current_date, 
                                                                       current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "PCA-Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p4)
    dev.off()
    
    p5 <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) + ggtitle(paste(current_date, 
                                                                            current_alt, "PC", PC_selected, var_explained, sep = "_")) 
    png(paste(current_date,current_alt, "Scree_plot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p5)
    dev.off()
    
    # Contributions of variables to PC1
    p6 <- fviz_contrib(res.pca, choice = "var", axes = 1)+ ggtitle(paste("Contributions 1st PC ", current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions_PC1", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p6)
    dev.off() 
    # Contributions of variables to PC2
    p7 <- fviz_contrib(res.pca, choice = "var", axes = 2)+ ggtitle(paste("Contributions 2nd PC ", current_date, current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "Contributions_PC2", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p7)
    dev.off() 
    
    # SAVE
    alt <- unique(subset_date$Altitude)
    if(length(alt) == 2){
      alt <- "15-30m"
    }
    
    else {
      alt <- "15-30-50m"
    }
    
    eigenval_result <- data.frame(res.pca$eig)
    eigenval_result$Date <- current_date
    eigenval_result$Altitude <- alt
    eigenval_result <- tibble::rownames_to_column(eigenval_result, "Component")
    eigenval_df <- rbind(eigenval_df, eigenval_result)
    
    contrib_result <- data.frame(res.pca$var$contrib)
    contrib_result$Date <- current_date
    contrib_result$Altitude <- alt
    contrib_result <- tibble::rownames_to_column(contrib_result, "Variable")
    contrib_df <- rbind(contrib_df, contrib_result)

  }  
}

setwd(paste(out.dir, "Exploration/PCA", sep = "/"))
write.csv(eigenval_df, "Eigenvalues.csv")
write.csv(contrib_df, "VarContributions.csv")
