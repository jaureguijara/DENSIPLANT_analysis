library(stringr)
library(dplyr)
library(stringi)
library(tidyr)

### Merge all Cereal Scanner files into a single file containing all vegetation indexes ###

if(Sys.info()["user"] == "Jara"){
  in.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/datasets/vegindex_csv_files")
  out.dir <- file.path("F:/DENSIPLANT/2022_densiplant_herve_jara/DENSIPLANT_analysis/datasets")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/datasets/vegindex_csv_files")
  out.dir <- file.path("C:/Users/jaure/OneDrive - WageningenUR/Internship/DENSIPLANT/DENSIPLANT_analysis/datasets")
}

filePaths <- list.files(path=in.dir,pattern="*.csv",include.dirs=T)

df <- NULL
for(filePath in filePaths){
  print(filePath)
  altitude <- str_extract(filePath, "[1-9][0-9]m")
  date <- str_extract(filePath, "_[0-9][0-9].[0-9][0-9]_")
  date <- gsub("_", "", date)
  
  setwd(in.dir)
  currentDF <- read.csv(filePath, header = T)
  
  firstcolname <- colnames(currentDF)[1]
  
  if(firstcolname == "X"){
    currentDF <- currentDF[,-1]
  }
  
  currentDF$Date <- as.Date(paste("2022/", gsub("\\.", "/", date), sep =""))
  currentDF$Altitude <- altitude
  colnames(currentDF)[1] <- "Image.Name"
  currentDF$Image.Name <- gsub(".tif","",currentDF$Image.Name)
  #currentDF$Image.Name <- paste(currentDF$Image.Name, date, altitude, sep = "_")
  print(head(currentDF))
  
  df <- bind_rows(df, currentDF)
  
}

df[is.na(df)] <- 0
df <- df %>%
  aggregate(.~ Image.Name + Date + Altitude, sum) %>% 
  mutate(picref = as.character(as.numeric(stri_sub(Image.Name,-3))))

# library(xlsx)
# write.xlsx(df, "EXCEL_combined_densiplant_dataset.xlsx")

### Create experimental design data frame ###

design <- data.frame(var = as.factor(c(rep("MARCOPOLO",5), rep("BOLOGNA", 5), rep("HONDIA", 5))), 
                     dens = as.factor(c(rep(c(35, 70, 140, 280, 560), 3))), 
                     R1 = c(109	, 111,	102,	115,	105,	 113,	104,	108,	110,	103,	112,
                            101,	114,	106,	107), 
                     R2 = c(205,	215,	203,	212,	208,	210,	209,	204,	213,	202,	207,
                            206,	214,	201,	211),
                     R3 = c(314,	305,	308,	312,	301,	310,	302,	313,	306,	309,	303,
                            315,	304,	307,	311),
                     R4 = c(411,	407,	406,	413,	402,	414,	403,	410,	409,	404,	401,
                            412,	408,	405,	415)) %>% 
  pivot_longer(cols=c('R1', 'R2', 'R3', 'R4'),
               names_to='Rows',
               values_to='Plots') 
design <- design[order(design$Plots),]
design$picref <- as.character(c(1:60))

setwd(out.dir)
write.csv(design, "coordinates_var_dens_picref.csv")
#write.xlsx(design, "Excel_coordinates_var_dens_picref.xlsx")


### Merge both data frames to link variety & density to veg index values ### 

finalDF <- df %>% 
  merge(design, by= "picref") %>% 
  mutate(Genotype = var,
         Density =  dens) %>% 
  dplyr::select(Image.Name, Date, Altitude, Genotype, Density, Rows, Plots, everything(), -picref, -var, -dens)


print(head(finalDF))
setwd(out.dir)
write.csv(finalDF, "combined_densiplant_dataset.csv")


