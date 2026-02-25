setwd("C:/Users/nathu/OneDrive/Documents/PhD Thesis/MorphoSource Mammals/Code")


install.packages("ggplot2")
install.packages("vioplot")
install.packages("purrr")
install.packages("ggalt")
install.packages("ggrepel")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("scales")
install.packages("readxl")
install.packages("dplyr")
install.packages("treeio")
install.packages("grDevices")
install.packages("smatr")
install.packages("scales")
install.packages("phytools")
install.packages("egg")
install.packages("gridExtra")



library(ggtree)
library(treeio)
library(ggplot2) 
library(ggrepel)
library(vioplot)
library(tidyverse)
library(reshape2)
library(scales)
library(readxl)
library(dplyr)
library(MASS)
library(egg)


Master_db <- read_excel("Mam_Meas_v2.xlsx")


mycolors2 <- (c("SD" = "#CC0000", 
                "CT" = "#FF6600", 
                "H&P" = "#d3d334", 
                "HR" = "#990066", 
                "LD" = "#f97575", 
                "AQ" = "blue", 
                "SW" = "#15717f",
                "FP" = "#33CCCC",
                "UF" = "#25be92",
                "US" = "#2596be",
                "HP" = "#346bb1",
                "CUR" = "darkgreen", 
                "RIC" = "#00CC00", 
                "QC" = "#6d6d04",
                "QS" = "#813712",
                "BRA" = "#b98c2b"))


#delete unnecessary columns
Master_db$Specimen<- NULL
Master_db$Type_Ref.<- NULL
Master_db$Myology_Ref.<- NULL
Master_db$Habit_type2<- NULL
Master_db$Habit_type3<- NULL


#create shortened species names
name_db <- as.data.frame(Master_db$Species)
colnames(name_db) <- "Speciesname"
name_db <- separate(name_db, Speciesname, into = c("first", "second", "third"), sep = "\\s+")
name_db$first_3 <- substr(name_db$first, start = 1, stop = 3)
name_db$second_3 <- substr(name_db$second, start = 1, stop = 3)
name_db$Species <- Master_db$Species
name_db$combo <- paste0(name_db$first_3, "_", name_db$second_3)
name_db$combo <- make.unique(as.character(name_db$combo), sep = "")


Master_db$Name_ID <- name_db$combo

#create columns with MA and log MA ------
{
  
  Master_db$Skull_Extension_MA <- Master_db$OH / Master_db$SL
  Master_db$Skull_Extension_MA_log <- log(Master_db$OH / Master_db$SL)
  
  Master_db$Shoulder_Flexion_MA <- Master_db$Shoulder_Flexion / Master_db$HL
  Master_db$Shoulder_Flexion_MA_log <- log(Master_db$Shoulder_Flexion_MA)
  
  Master_db$Shoulder_Extension_MA <- Master_db$Shoulder_Extension / Master_db$HL
  Master_db$Shoulder_Extension_MA_log <- log(Master_db$Shoulder_Extension_MA)
  
  Master_db$Shoulder_Adduction_MA <- Master_db$Shoulder_Adduction / Master_db$HL
  Master_db$Shoulder_Adduction_MA_log <- log(Master_db$Shoulder_Adduction_MA)
  
  Master_db$Shoulder_Abduction_MA <- Master_db$Shoulder_Abduction / Master_db$HL
  Master_db$Shoulder_Abduction_MA_log <- log(Master_db$Shoulder_Abduction_MA)
  
  Master_db$Shoulder_Medial_Rotation_MA <- Master_db$Shoulder_Medial_Rotation / Master_db$UL
  Master_db$Shoulder_Medial_Rotation_MA_log <- log(Master_db$Shoulder_Medial_Rotation_MA)
  
  Master_db$Shoulder_Lateral_Rotation_MA <- Master_db$Shoulder_Lateral_Rotation / Master_db$UL
  Master_db$Shoulder_Lateral_Rotation_MA_log <- log(Master_db$Shoulder_Lateral_Rotation_MA)
  
  Master_db$Elbow_Extension_MA <- Master_db$OL / Master_db$UL
  Master_db$Elbow_Extension_MA_log <- log(Master_db$Elbow_Extension_MA)
  
  Master_db$Elbow_Flexion_MA <- Master_db$Elbow_Flexors / Master_db$UL
  Master_db$Elbow_Flexion_MA_log <- log(Master_db$Elbow_Flexion_MA)
  
  Master_db$Wrist_Extension_MA <- Master_db$Wrist_Extension / Master_db$ML
  Master_db$Wrist_Extension_MA_log <- log(Master_db$Wrist_Extension_MA)
  
  Master_db$Wrist_Flexion_MA <- Master_db$Wrist_Flexion / Master_db$ML
  Master_db$Wrist_Flexion_MA_log <- log(Master_db$Wrist_Flexion_MA)
  
  Master_db$Hip_Flexion_MA <- Master_db$PFL / Master_db$FeL
  Master_db$Hip_Flexion_MA_log <- log(Master_db$Hip_Flexion_MA)
  
  Master_db$Ankle_Extension_MA <- Master_db$CL / Master_db$FoL
  Master_db$Ankle_Extension_MA_log <- log(Master_db$Ankle_Extension_MA)
  
  Master_db$PL_log <- log(Master_db$PL)
  
  Master_db <- Master_db[-c(19)]
}


Master_db2 <- Master_db
colnames(Master_db2)[3] = "Locomotor type"
#


