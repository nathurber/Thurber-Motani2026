setwd("C:/Users/nathu/OneDrive/Documents/PhD Thesis/MorphoSource Mammals/Code")


# install.packages("ggplot2")
# install.packages("vioplot")
# install.packages("purrr")
# install.packages("ggalt")
# install.packages("ggrepel")
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("scales")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("treeio")
# install.packages("grDevices")
# install.packages("smatr")
# install.packages("scales")
# install.packages("phytools")
# install.packages("egg")
# install.packages("gridExtra")



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

#For LDAs
Master_db <- read_excel("Thurber-Motani2026_dataset.xlsx")
#with edited Artiodactyla MA for plots
Master_db2 <- read_excel("Thurber-Motani2026_dataset_v2.xlsx")



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



#create shortened species names
name_db <- as.data.frame(Master_db$Species)
colnames(name_db) <- "Speciesname"
name_db <- separate(name_db, Speciesname, into = c("first", "second", "third"), sep = "\\s+")
#error for missing data, ignore
name_db$first_3 <- substr(name_db$first, start = 1, stop = 3)
name_db$second_3 <- substr(name_db$second, start = 1, stop = 3)
name_db$Species <- Master_db$Species
name_db$combo <- paste0(name_db$first_3, "_", name_db$second_3)
name_db$combo <- make.unique(as.character(name_db$combo), sep = "")


Master_db$Name_ID <- name_db$combo

colnames(Master_db2)[3] = "Locomotor type"

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
  
  Master_db$PL_log <- log(Master_db$PL)
  
  Master_db <- Master_db[-c(19)]
  
  Master_db2$Skull_Extension_MA <- Master_db2$OH / Master_db2$SL
  Master_db2$Skull_Extension_MA_log <- log(Master_db2$OH / Master_db2$SL)
  
  Master_db2$Shoulder_Flexion_MA <- Master_db2$Shoulder_Flexion / Master_db2$HL
  Master_db2$Shoulder_Flexion_MA_log <- log(Master_db2$Shoulder_Flexion_MA)
  
  Master_db2$Shoulder_Extension_MA <- Master_db2$Shoulder_Extension / Master_db2$HL
  Master_db2$Shoulder_Extension_MA_log <- log(Master_db2$Shoulder_Extension_MA)
  
  Master_db2$Shoulder_Adduction_MA <- Master_db2$Shoulder_Adduction / Master_db2$HL
  Master_db2$Shoulder_Adduction_MA_log <- log(Master_db2$Shoulder_Adduction_MA)
  
  Master_db2$Shoulder_Abduction_MA <- Master_db2$Shoulder_Abduction / Master_db2$HL
  Master_db2$Shoulder_Abduction_MA_log <- log(Master_db2$Shoulder_Abduction_MA)
  
  Master_db2$Shoulder_Medial_Rotation_MA <- Master_db2$Shoulder_Medial_Rotation / Master_db2$UL
  Master_db2$Shoulder_Medial_Rotation_MA_log <- log(Master_db2$Shoulder_Medial_Rotation_MA)
  
  Master_db2$Shoulder_Lateral_Rotation_MA <- Master_db2$Shoulder_Lateral_Rotation / Master_db2$UL
  Master_db2$Shoulder_Lateral_Rotation_MA_log <- log(Master_db2$Shoulder_Lateral_Rotation_MA)
  
  Master_db2$Elbow_Extension_MA <- Master_db2$OL / Master_db2$UL
  Master_db2$Elbow_Extension_MA_log <- log(Master_db2$Elbow_Extension_MA)
  
  Master_db2$Elbow_Flexion_MA <- Master_db2$Elbow_Flexors / Master_db2$UL
  Master_db2$Elbow_Flexion_MA_log <- log(Master_db2$Elbow_Flexion_MA)
  
  Master_db2$Wrist_Extension_MA <- Master_db2$Wrist_Extension / Master_db2$ML
  Master_db2$Wrist_Extension_MA_log <- log(Master_db2$Wrist_Extension_MA)
  
  Master_db2$Wrist_Flexion_MA <- Master_db2$Wrist_Flexion / Master_db2$ML
  Master_db2$Wrist_Flexion_MA_log <- log(Master_db2$Wrist_Flexion_MA)
  
  Master_db2$PL_log <- log(Master_db2$PL)
  
  Master_db2 <- Master_db2[-c(19)]
  
  
}

#

#ggplot PL/Locomotor Type Boxplots & Order Scatterplots -----

#Scaling boxplots

myorder2 <- c("HR", "CT", "SD", "CUR", "FP", "QC",  "RIC", "H&P", "HP",  "QS", "LD", "BRA", "US", "SW", "UF", "AQ")

PLboxplot <- ggplot(Master_db2, aes(x = `Locomotor type`, y = PL_log, color = `Locomotor type`)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = `Locomotor type`), alpha = 0.75) + 
  ylab("Precaudal length (mm)") + xlab("Locomotor type") +
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  scale_x_discrete(limits = myorder2) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position="none")


PLboxplot

aov_PL_Habit <- aov(PL_log ~ `Locomotor type`, data=Master_db2)

hist(resid(aov_PL_Habit), breaks=50) 

plot(aov_PL_Habit) 
#enter in console
summary(aov_PL_Habit)

ggsave(filename = "Precaudal_Length_Scatter.png", plot = PLboxplot, width = 14, height = 8, 
       units = c("in"), dpi = 250)

library("viridis") 

myorder3 <- c("Eulipotyphla", "Afrosoricida", "Macroscelidea", "Rodentia", "Scandentia", "Cingulata", "Marsupialia", 
              "Lagomorpha",  "Pholidota",  "Hyracoidea", "Pilosa", "Monotremata", "Carnivora", "Primates", 
              "Tubulidentata", "Sirenia", "Artiodactyla")

Orderboxplot <- ggplot(Master_db2, aes(x = Order, y = PL_log, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  ylab("Logged Precaudal Length (mm)") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none")


Orderboxplot

aov_PL_Order <- aov(PL_log ~ Order, data=Master_db2)

summary(aov_PL_Order)

ggsave(filename = "Precaudal_Length_Order_boxplot.png", plot = Orderboxplot, width = 14, height = 8, 
       units = c("in"), dpi = 250)

FinalScalingPlot <- grid.arrange(arrangeGrob(Orderboxplot + annotate("text",x='Afrosoricida',y=7.5,size=15,label=(paste0("A")),fontface = "bold",parse=TRUE),
                                             PLboxplot + annotate("text",x='CT',y=7.5,size=15,label=(paste0("B")),fontface = "bold",parse=TRUE),  nrow = 1))

#Figure 8
ggsave(filename = "ScalingPlotFinal.png", plot =  FinalScalingPlot, width = 14, height = 8, 
       units = c("in"), dpi = 300)


#Order MA Scatter Plots

OrderscatterSkull <- ggplot(Master_db2, aes(x = PL, y = Skull_Extension_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Skull Extension MA") +
  ylab("Skull Extension MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterSkull

OrderscatterShEx <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Extension_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Shoulder Extension MA") +
  ylab("Shoulder Extension MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterShEx

OrderscatterShFl <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Flexion_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Shoulder Flexion MA") +
  ylab("Shoulder Flexion MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterShFl

OrderscatterShAd <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Adduction_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Shoulder Adduction MA") +
  ylab("Shoulder Adduction MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterShAd

OrderscatterShAb <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Abduction_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Shoulder Abduction MA") +
  ylab("Shoulder Abduction MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterShAb

OrderscatterShMR <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Medial_Rotation_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Shoulder Medial Rotation MA") +
  ylab("Shoulder Medial Rotation MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterShMR

OrderscatterShLR <- ggplot(Master_db2, aes(x = PL, y = Shoulder_Lateral_Rotation_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ylab("Shoulder Lateral Rotation MA") + xlab("Precaudal length (mm)") 

OrderscatterShLR

#Figure 7
ggsave(filename = "OrderMAScatter6.png", plot = OrderscatterShLR, width = 11.5, height = 8, 
       units = c("in"), dpi = 250)


OrderscatterElFl <- ggplot(Master_db2, aes(x = PL, y = Elbow_Flexion_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Elbow Flexion MA") +
  ylab("Elbow Flexion MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterElFl

OrderscatterElEx <- ggplot(Master_db2, aes(x = PL, y = Elbow_Extension_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Elbow Extension MA") +
  ylab("Elbow Extension MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterElEx

OrderscatterWrFl <- ggplot(Master_db2, aes(x = PL, y = Wrist_Flexion_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Wrist Flexion MA") +
  ylab("Wrist Flexion MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterWrFl

OrderscatterWrEx <- ggplot(Master_db2, aes(x = PL, y = Wrist_Extension_MA, color = Order, group = Order)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Order), method = "lm", se = FALSE) +
  scale_color_manual(values=rainbow(17)) +
  ggtitle("Order vs Wrist Extension MA") +
  ylab("Wrist Extension MA") + xlab("Precaudal length (mm)") +
  theme(plot.title = element_blank())

OrderscatterWrEx


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend2<-g_legend(OrderscatterElEx)

ScatterOrderMAFinal2x3 <- grid.arrange(arrangeGrob(OrderscatterShAb + theme(legend.position="none", axis.title.x = element_blank()), OrderscatterShAd + theme(legend.position="none", axis.title.x = element_blank()), 
                                                   OrderscatterShEx + theme(legend.position="none", axis.title.x = element_blank()), OrderscatterShFl + theme(legend.position="none", axis.title.x = element_blank()), 
                                                   OrderscatterShMR + theme(legend.position="none", axis.title.x = element_blank()),  OrderscatterShLR + theme(legend.position="none", axis.title.x = element_blank()), nrow =3),
                                       right = mylegend2, bottom="Precaudal length (mm)")

#Figure S4
ggsave(filename = "Scatter_Order_Final_2x3.png", plot =  ScatterOrderMAFinal2x3 , width = 11, height = 7.5, 
       units = c("in"), dpi = 250)

ScatterOrderMAFinal2x3v2 <- grid.arrange(arrangeGrob(OrderscatterSkull + theme(legend.position="none", axis.title.x = element_blank()), OrderscatterElEx + theme(legend.position="none", axis.title.x = element_blank()), 
                                                     OrderscatterElFl + theme(legend.position="none", axis.title.x = element_blank()), OrderscatterWrEx + theme(legend.position="none", axis.title.x = element_blank()), 
                                                     OrderscatterWrFl + theme(legend.position="none", axis.title.x = element_blank()), nrow =3),
                                         right = mylegend2, bottom="Precaudal length (mm)")

#Figure S5
ggsave(filename = "Scatter_Order_Final_2x3_v3.png", plot =  ScatterOrderMAFinal2x3v2 , width = 11, height = 7.5, 
       units = c("in"), dpi = 250)



#Mechanical Advantage Bi-plots and MA vs PL scatterplots -----



library(purrr)
library(ggalt)

library(gridExtra)

colnames(Master_db2)

response = names(Master_db2)[c(31,33,35,37,39,41,43,45,47,49,51)]
response
expl = names(Master_db2)[c(31,33,35,37,39,41,43,45,47,49,51)]
expl
response = set_names(response)
response
expl = set_names(expl)
expl

Master_db2$`Locomotor type` <- factor(Master_db2$`Locomotor type`, levels = c("SD", "CT", "H&P", "HR", "LD", "AQ", "SW", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))


#PL vs MA with hulls
scatter_fun = function(x, y) {
  ggplot(Master_db2, aes(x = .data[[x]], y = .data[[y]], color = `Locomotor type`,fill = `Locomotor type`, group = `Locomotor type`, shape =`Locomotor type`)) + 
    scale_y_continuous(trans = 'log2') +
    scale_y_log10(breaks = log_breaks()) +
    scale_x_continuous(trans = 'log2') +
    scale_x_log10(breaks = log_breaks()) +
    geom_point(size = 1.5) + 
    geom_encircle(aes(fill = `Locomotor type`), s_shape = 1, expand = 0,
                  alpha = 0.2, color = "black", show.legend = FALSE) +
    scale_shape_manual(values=c(0, 1, 2, 5, 6, 15, 19, 7, 9, 12, 14, 17, 18, 8, 3, 4))+
    scale_color_manual(values=mycolors2) +
    scale_fill_manual(values=mycolors2) +
    ggtitle(paste0(y, " vs ", x)) +
    ylab(y) + xlab(x) + 
    theme(plot.title = element_text(hjust = 0.5))
}


ShLaRoElEx <- scatter_fun(x = "Elbow_Extension_MA" , y = "Shoulder_Lateral_Rotation_MA")
ShLaRoElEx <- ShLaRoElEx +  xlab("Logged Elbow Extension MA") + ylab("Logged Shoulder Lateral Rotation MA") + labs(title= NULL)
ShLaRoElEx
#error because of missing MA data, ignore

#Figure 5
ggsave(filename = "Scatterplot_Final_v8.png", plot = ShLaRoElEx, width = 12, height = 8, 
       units = c("in"), dpi = 250)

#pdf with all plots
all_plots2 = map(response, function(resp) {
  map(expl, function(expl) {
    scatter_fun(x = expl, y = resp)
  })
})

pdf("all_MAscatterplotsHulls.pdf", width = 6, height = 4)
all_plots2
dev.off()


#PL vs MA with regression lines 
scatter_fun2 = function(x, y) {
  ggplot(Master_db2, aes(x = .data[[x]], y = .data[[y]], color = `Locomotor type`,fill = `Locomotor type`, group = `Locomotor type`, shape =`Locomotor type`)) + 
    geom_point(alpha = 0.3) +
    geom_line(stat = "smooth", method = lm, aes(group = `Locomotor type`), size = 1, alpha = 0.4) +
    #geom_smooth(aes(group = `Locomotor type`), method = "lm", se = FALSE) +
    scale_shape_manual(values=c(0, 1, 2, 5, 6, 15, 19, 7, 9, 12, 14, 17, 18, 8, 3, 4))+
    scale_color_manual(values=mycolors2) +
    scale_fill_manual(values=mycolors2) +
    ggtitle(paste0(y, " vs ", x)) +
    ylab(y) + xlab(x) +
    theme(plot.title = element_text(hjust = 0.5))
}

LaRoPL <-  scatter_fun2(x = "PL", y = "Shoulder_Lateral_Rotation_MA")
LaRoPL + 
  scale_y_continuous(limits = c(-0.25, 1.25)) +
  annotate("text",x=2000,y=0.85,size=15,label=(paste0("B")),fontface = "bold",parse=TRUE) 

ElExPL <- scatter_fun2(x = "PL", y = "Elbow_Extension_MA")
ElExPL +  
  scale_y_continuous(limits = c(-0.25, 1.25)) +
  annotate("text",x=2000,y=0.85,size=15,label=(paste0("B")),fontface = "bold",parse=TRUE) 




g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


mylegend3 <- g_legend(LaRoPL)

Sh.lm <- lm(Shoulder_Lateral_Rotation_MA ~ PL, Master_db2)
El.lm <- lm(Elbow_Extension_MA ~ PL, Master_db2)

finalPLscatterplot <- grid.arrange(arrangeGrob(LaRoPL +  ylab("Shoulder Lateral Rotation MA") + labs(title= NULL, xlab= "none")+ theme(legend.position="none", axis.title.x=element_blank()) +
                                                 geom_abline(slope = coef(Sh.lm)[["PL"]], intercept = coef(Sh.lm)[["(Intercept)"]], color = "black", linewidth = 2) +
                                                 scale_y_continuous(limits = c(-0.05, 0.8)) +
                                                 annotate("text",x=500,y=0.6,size=5,label=(paste0("slope==",coef(lm(Master_db$Shoulder_Lateral_Rotation_MA~Master_db$PL))[2])),parse=TRUE) +
                                                 annotate("text",x=2000,y=0.5,size=25,label=(paste0("A")),fontface = "bold",parse=TRUE) , 
                                               
                                               
                                               ElExPL +  ylab("Elbow Extension MA") + labs(title= NULL, xlab="none") + theme(legend.position="none", axis.title.x=element_blank()) +
                                                 scale_y_continuous(limits = c(-0.05, 1.25)) +
                                                 geom_abline(slope = coef(El.lm)[["PL"]], intercept = coef(El.lm)[["(Intercept)"]], color = "black", linewidth = 2) +
                                                 annotate("text",x=2000,y=0.8,size=25,label=(paste0("B")),fontface = "bold",parse=TRUE) +
                                                 annotate("text",x=1000,y=1,size=5,label=(paste0("slope==",coef(lm(Master_db$Elbow_Extension_MA~Master_db$PL))[2])),parse=TRUE), right = mylegend3, bottom="Precaudal Length (mm)", nrow = 1) ) 


#Figure 6
ggsave(filename = "Motion_PL_Final.png", plot = finalPLscatterplot, width = 14, height = 8, 
       units = c("in"), dpi = 300)




#Kendall Rank Correlation Test -----

#First test for normal distribution
#low p values indicate not normal distribution
shapiro.test(Master_db2$Shoulder_Lateral_Rotation_MA)
shapiro.test(Master_db2$Elbow_Extension_MA)
shapiro.test(Master_db2$Skull_Extension_MA)
shapiro.test(Master_db2$Shoulder_Flexion_MA)
shapiro.test(Master_db2$Shoulder_Extension_MA)
shapiro.test(Master_db2$Shoulder_Adduction_MA)
shapiro.test(Master_db2$Shoulder_Abduction_MA)
shapiro.test(Master_db2$Shoulder_Medial_Rotation_MA)
shapiro.test(Master_db2$Elbow_Flexion_MA)
shapiro.test(Master_db2$Wrist_Extension_MA)
shapiro.test(Master_db2$Wrist_Flexion_MA)
shapiro.test(Master_db2$PL)

Rodentia <- Master_db2[c(1:85),]
Lagomorpha <- Master_db2[c(86:91),]
Afrosoricida <- Master_db2[c(92:102),]
Hyracoidea <- Master_db2[c(104:109),]
Macroscelidea  <- Master_db2[c(110:118),]
Eulipotyphla <- Master_db2[c(119:131,134:138,140:146),]
Monotremata <- Master_db2[c(147:150),]
Marsupialia <- Master_db2[c(151:182),]
Carnivora <- Master_db2[c(183:236),]
Sirenia <-  Master_db2[c(238:241),]
Cingulata <- Master_db2[c(242:246),]
Pilosa <- Master_db2[c(247:256),]
Primates <- Master_db2[c(257:267),]
Artiodactyla <- Master_db2[c(268:278),]
Scandentia <- Master_db2[c(132:134, 139),]


#correlation test 

#Kendall correlation 
cortestSh2 <- cor.test(Master_db2$Shoulder_Lateral_Rotation_MA, Master_db2$PL, 
                       method = "kendall")
cortestSh2

cortestEl2 <- cor.test(Master_db2$Elbow_Extension_MA, Master_db2$PL, 
                       method = "kendall")
cortestEl2

cortestSkE <- cor.test(Master_db2$Skull_Extension_MA, Master_db2$PL, 
                       method = "kendall")
cortestSkE

cortestShF <- cor.test(Master_db2$Shoulder_Flexion_MA, Master_db2$PL, 
                       method = "kendall")
cortestShF

cortestShE <- cor.test(Master_db2$Shoulder_Extension_MA, Master_db2$PL, 
                       method = "kendall")
cortestShE

cortestShAd <- cor.test(Master_db2$Shoulder_Adduction_MA, Master_db2$PL, 
                        method = "kendall")
cortestShAd

cortestShAb <- cor.test(Master_db2$Shoulder_Abduction_MA, Master_db2$PL, 
                        method = "kendall")
cortestShAb

cortestShM <- cor.test(Master_db2$Shoulder_Medial_Rotation_MA, Master_db2$PL, 
                       method = "kendall")
cortestShM

cortestElF <- cor.test(Master_db2$Elbow_Flexion_MA, Master_db2$PL, 
                       method = "kendall")
cortestElF

cortestWrE <- cor.test(Master_db2$Wrist_Extension_MA, Master_db2$PL, 
                       method = "kendall")
cortestWrE

cortestWrF <- cor.test(Master_db2$Wrist_Flexion_MA, Master_db2$PL, 
                       method = "kendall")
cortestWrF

#Grouped by order
#Kendall correlation 
cortestRod <- cor.test(Rodentia$Shoulder_Lateral_Rotation_MA, Rodentia$PL, 
                       method = "kendall")
cortestRod
#not sig

cortestLag <- cor.test(Lagomorpha$Shoulder_Lateral_Rotation_MA, Lagomorpha$PL, 
                       method = "kendall")
cortestLag
#not sig

cortestAfr <- cor.test(Afrosoricida$Shoulder_Lateral_Rotation_MA, Afrosoricida$PL, 
                       method = "kendall")
cortestAfr
#not sig

cortestHyr <- cor.test(Hyracoidea$Shoulder_Lateral_Rotation_MA, Hyracoidea$PL, 
                       method = "kendall")
cortestHyr
#not sig

cortestMac <- cor.test(Macroscelidea$Shoulder_Lateral_Rotation_MA, Macroscelidea$PL, 
                       method = "kendall")
cortestMac
#not sig


cortestEul <- cor.test(Eulipotyphla$Shoulder_Lateral_Rotation_MA, Eulipotyphla$PL, 
                       method = "kendall")
cortestEul
#not sig

cortestMon <- cor.test(Monotremata$Shoulder_Lateral_Rotation_MA, Monotremata$PL, 
                       method = "kendall")
cortestMon
#not sig

cortestMar <- cor.test(Marsupialia$Shoulder_Lateral_Rotation_MA, Marsupialia$PL, 
                       method = "kendall")
cortestMar
#not sig


cortestCar <- cor.test( Carnivora$Shoulder_Lateral_Rotation_MA,  Carnivora$PL, 
                        method = "kendall")
cortestCar
#p-value = 0.00828

cortestSir <- cor.test( Sirenia$Shoulder_Lateral_Rotation_MA,  Sirenia$PL, 
                        method = "kendall")
cortestSir
#not sig

cortestCin <- cor.test(Cingulata$Shoulder_Lateral_Rotation_MA, Cingulata$PL, 
                       method = "kendall")
cortestCin
#not sig

cortestPil <- cor.test( Pilosa$Shoulder_Lateral_Rotation_MA,  Pilosa$PL, 
                        method = "kendall")
cortestPil
#not sig

cortestPri <- cor.test(Primates$Shoulder_Lateral_Rotation_MA,Primates$PL, 
                       method = "kendall")
cortestPri
#not sig

cortestArt <- cor.test(Artiodactyla$Shoulder_Lateral_Rotation_MA, Artiodactyla$PL, 
                       method = "kendall")
cortestArt
#not sig

cortestScan <- cor.test(Scandentia$Shoulder_Lateral_Rotation_MA, Scandentia$PL, 
                        method = "kendall")
cortestScan
#not sig


#MA vs Locomotor Type/Order Boxplots -----

#MA vs Locomotor Type 
myorder <- c("AQ", "US", "HR", "UF", "SW", "LD", "CT", "SD", "H&P", "FP", "HP", "CUR", "RIC", "QC", "QS", "BRA")

require("gridExtra")

ggplotdf_temp2 <- Master_db2[c(1,3,31,33,35,37,39,41,43,45,47,49,51,53)] 

ggplotdf2 <- melt(ggplotdf_temp2, ID=c(1:3))

ggplotdf2$variable <- gsub("_MA", "", ggplotdf2$variable)
ggplotdf2$variable <- gsub("\\_", "\\ ", ggplotdf2$variable)

ggplotdf2$part <- ggplotdf2$variable
ggplotdf2$part <- sub(" .*", "", ggplotdf2$part)

#change order boxplot label by changing name
ggplotdf2$variable2 <- ggplotdf2$variable
ggplotdf2.5 <- ggplotdf2
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Abduction"] <- "A. Shoulder Abduction"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Adduction"] <- "A. Shoulder Adduction"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Extension"] <- "B. Shoulder Extension"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Flexion"] <- "B. Shoulder Flexion"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Lateral Rotation"] <- "C. Shoulder Lateral Rotation"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Shoulder Medial Rotation"] <- "C. Shoulder Medial Rotation"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Skull Extension"] <- "D. Occipital Joint Extension"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Elbow Extension"] <- "E. Elbow Extension"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Elbow Flexion"] <- "D. Elbow Flexion"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Wrist Flexion"] <- "F. Wrist Flexion"
ggplotdf2.5$variable2[ggplotdf2.5$variable2 == "Wrist Extension"] <- "E. Wrist Extension"


#individual boxplots, errors relate to missing MA values, ignore
Skull_extension <- ggplot( subset(ggplotdf2.5,variable2 %in% c("D. Occipital Joint Extension")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("D. Occipital Joint Extension") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Skull_extension

Shoulder_abduction <- ggplot( subset(ggplotdf2.5,variable2 %in% c("A. Shoulder Abduction")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("A. Shoulder Abduction") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Shoulder_abduction

Shoulder_extension <- ggplot( subset(ggplotdf2.5,variable2 %in% c("B. Shoulder Extension")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("B. Shoulder Extension") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Shoulder_extension

Shoulder_lateralrotation <- ggplot( subset(ggplotdf2.5,variable2 %in% c("C. Shoulder Lateral Rotation")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("C. Shoulder Lateral Rotation") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Shoulder_lateralrotation

Elbow_extension <- ggplot( subset(ggplotdf2.5,variable2 %in% c("E. Elbow Extension")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("E. Elbow Extension") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Elbow_extension

Wrist_flexion <- ggplot( subset(ggplotdf2.5,variable2 %in% c("F. Wrist Flexion")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) +
  ggtitle("F. Wrist Flexion") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)


Wrist_flexion

#Figure 4
#using gridExtra
BoxplotFinal2x3 <- grid.arrange(Shoulder_abduction, Shoulder_extension, Shoulder_lateralrotation, Skull_extension, Elbow_extension, Wrist_flexion, ncol=2, nrow =3)

ggsave(filename = "Boxplot_Final_2x3.png", plot = BoxplotFinal2x3, width = 7.5, height = 12, 
       units = c("in"), dpi = 250)
 

#Supplemental Boxplot, Figure S1

Shoulder_adduction <- ggplot( subset(ggplotdf2.5,variable2 %in% c("A. Shoulder Adduction")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("A. Shoulder Adduction") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)

Shoulder_adduction


Shoulder_flexion <- ggplot( subset(ggplotdf2.5,variable2 %in% c("B. Shoulder Flexion")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("B. Shoulder Flexion") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)

Shoulder_flexion

Shoulder_medialrotation <- ggplot( subset(ggplotdf2.5,variable2 %in% c("C. Shoulder Medial Rotation")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("C. Shoulder Medial Rotation") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)

Shoulder_medialrotation 

Elbow_flexion <- ggplot( subset(ggplotdf2.5,variable2 %in% c("D. Elbow Flexion")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("D. Elbow Flexion") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)

Elbow_flexion 

Wrist_extension <- ggplot( subset(ggplotdf2.5,variable2 %in% c("E. Wrist Extension")), aes(x = `Locomotor type`, y = value, color = `Locomotor type`)) + 
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder) +
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  geom_hline(yintercept = 1, color="#707070") +
  geom_point(aes(group = variable), alpha = 0.75, position = position_jitterdodge(), size = 1) + 
  ggtitle("E. Wrist Extension") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_text(size = 4.5)) +
  scale_colour_manual(values = mycolors2)

Wrist_extension

#gridExtra
#Figure S1
SBoxplotFinal2x3 <- grid.arrange(Shoulder_adduction, Shoulder_flexion, Shoulder_medialrotation, Elbow_flexion, Wrist_extension, ncol=2, nrow =3)

ggsave(filename = "SBoxplot_Final_2x3.png", plot = SBoxplotFinal2x3, width = 7.5, height = 12, 
       units = c("in"), dpi = 250)




#MA vs Order Boxplot
myorder3 <- c("Eulipotyphla", "Afrosoricida", "Macroscelidea", "Rodentia", "Scandentia", "Cingulata", "Marsupialia", 
              "Lagomorpha",  "Pholidota",  "Hyracoidea", "Pilosa", "Monotremata", "Carnivora", "Primates", 
              "Tubulidentata", "Sirenia", "Artiodactyla")

OrderShAbdBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Abduction_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("A. Logged Shoulder Abduction MA") +
  xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))

OrderShAbdBar

OrderShAddBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Adduction_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("B. Logged Shoulder Adduction MA") +
  xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderShAddBar

OrderShExBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Extension_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("C. Logged Shoulder Extension MA") +
  ylab("Logged Shoulder Extension MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderShExBar

OrderShFlBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Flexion_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("D. Logged Shoulder Flexion MA") +
  ylab("Logged Shoulder Extension MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderShFlBar

OrderShLRBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Lateral_Rotation_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("E. Logged Shoulder Lateral Rotation MA") +
  ylab("Logged Shoulder Lateral Rotation MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderShLRBar

OrderShMRBar <- ggplot(Master_db2, aes(x = Order, y = Shoulder_Medial_Rotation_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("F. Logged Shoulder Medial Rotation MA") +
  ylab("Logged Shoulder Lateral Rotation MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderShMRBar

OrderSkExBar <- ggplot(Master_db2, aes(x = Order, y = Skull_Extension_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("A. Logged Occipital Joint Extension MA") +
  ylab("Logged Occipital Joint Extension MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderSkExBar

OrderElExBar <- ggplot(Master_db2, aes(x = Order, y = Elbow_Extension_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("B. Logged Elbow Extension MA") +
  ylab("Logged Elbow Extension MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderElExBar

OrderElFlBar <- ggplot(Master_db2, aes(x = Order, y = Elbow_Flexion_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("C. Logged Elbow Flexion MA") +
  ylab("Logged Elbow Extension MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderElFlBar

OrderWrFlBar <- ggplot(Master_db2, aes(x = Order, y = Wrist_Flexion_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("E. Logged Wrist Flexion MA") +
  ylab("Logged Wrist Flexion MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderWrFlBar

OrderWrExBar <- ggplot(Master_db2, aes(x = Order, y = Wrist_Extension_MA, color = Order)) + 
  geom_point(size = 1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = Order), alpha = 0.75) + 
  scale_y_log10(breaks = seq(0, 2, by = 0.1), limits=c(0.015, 1.2)) +
  ggtitle("D. Logged Wrist Extension MA") +
  ylab("Logged Wrist Flexion MA") + xlab("Order") +
  scale_color_manual(values=rainbow(17)) +
  scale_fill_manual(values=rainbow(17)) +
  guides(fill=guide_legend(title="Order"))+
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myorder3) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title.x=element_blank(),  axis.title.y=element_blank(), axis.text.y = element_text(size = 4.5), axis.text.x = element_text(size = 6))


OrderWrExBar

#Figure S2
BoxplotOrderMAFinal2x3 <- grid.arrange(OrderShAbdBar, OrderShAddBar, OrderShExBar, OrderShFlBar, OrderShLRBar, OrderShMRBar, ncol=2, nrow =3)
ggsave(filename = "Boxplot_Order_Final_2x3.png", plot = BoxplotOrderMAFinal2x3, width = 7.5, height = 12, 
       units = c("in"), dpi = 300)

#Figure S3
BoxplotOrder2MAFinal2x3 <- grid.arrange(OrderSkExBar, OrderElExBar, OrderElFlBar, OrderWrExBar, OrderWrFlBar, ncol=2, nrow =3)
ggsave(filename = "Boxplot_Order_Final2_2x3.png", plot = BoxplotOrder2MAFinal2x3, width = 7.5, height = 12, 
       units = c("in"), dpi = 300)


#ANOVA -----

#MANOVA
my_manova_log <- manova(cbind(Skull_Extension_MA_log, Shoulder_Flexion_MA_log, Shoulder_Extension_MA_log, Shoulder_Adduction_MA_log, Shoulder_Abduction_MA_log,        
                              Shoulder_Medial_Rotation_MA_log, Shoulder_Lateral_Rotation_MA_log, Elbow_Extension_MA_log, Elbow_Flexion_MA_log, Wrist_Extension_MA_log,              
                              Wrist_Flexion_MA_log) ~ `Locomotor type`, data=Master_db2)

summary(my_manova_log, test = "Pillai") 

#ANOVA mechanical advantages vs locomotor types 

my_aov1 <- aov(Skull_Extension_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov2 <- aov(Shoulder_Flexion_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov3 <- aov(Shoulder_Extension_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov4 <- aov(Shoulder_Adduction_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov5 <- aov(Shoulder_Abduction_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov6 <- aov(Shoulder_Medial_Rotation_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov7 <- aov(Shoulder_Lateral_Rotation_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov8 <- aov(Elbow_Extension_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov9 <- aov(Elbow_Flexion_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov10 <- aov(Wrist_Extension_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov11 <- aov(Wrist_Flexion_MA_log ~ `Locomotor type`, data=Master_db2)

my_aov12 <- aov(PL ~ `Locomotor type`, data=Master_db2)

my_aov13 <- aov(PL ~ Order, data=Master_db2)

my_aov14 <- aov(PL ~ Order*`Locomotor type`, data=Master_db2) #two-way ANVOVA, additive 

summary(my_aov1)
summary(my_aov2)
summary(my_aov3)
summary(my_aov4)
summary(my_aov5)
summary(my_aov6)
summary(my_aov7)
summary(my_aov8)
summary(my_aov9)
summary(my_aov10)
summary(my_aov11)
summary(my_aov12)
summary(my_aov13)
summary(my_aov14)


#PL vs MA ANCOVA

my_aov1 <- aov(Skull_Extension_MA  ~ PL, data=Master_db2)

my_aov2 <- aov(Shoulder_Flexion_MA  ~ PL, data=Master_db2)

my_aov3 <- aov(Shoulder_Extension_MA  ~ PL, data=Master_db2)

my_aov4 <- aov(Shoulder_Adduction_MA  ~ PL, data=Master_db2)

my_aov5 <- aov(Shoulder_Abduction_MA  ~ PL, data=Master_db2)

my_aov6 <- aov(Shoulder_Medial_Rotation_MA  ~ PL, data=Master_db2)

my_aov7 <- aov(Shoulder_Lateral_Rotation_MA  ~ PL, data=Master_db2)

my_aov8 <- aov(Elbow_Extension_MA  ~ PL, data=Master_db2)

my_aov9 <- aov(Elbow_Flexion_MA  ~ PL, data=Master_db2)

my_aov10 <- aov(Wrist_Extension_MA  ~ PL, data=Master_db2)

my_aov11 <- aov(Wrist_Flexion_MA  ~ PL, data=Master_db2)

summary(my_aov1)
summary(my_aov2)
summary(my_aov3)
summary(my_aov4)
summary(my_aov5)
summary(my_aov6)
summary(my_aov7)
summary(my_aov8)
summary(my_aov9)
summary(my_aov10)
summary(my_aov11)

#Order vs MA ANOVA

my_aov1 <- aov(Skull_Extension_MA_log ~ Order, data=Master_db2)

my_aov2 <- aov(Shoulder_Flexion_MA_log ~ Order, data=Master_db2)

my_aov3 <- aov(Shoulder_Extension_MA_log ~ Order, data=Master_db2)

my_aov4 <- aov(Shoulder_Adduction_MA_log ~ Order, data=Master_db2)

my_aov5 <- aov(Shoulder_Abduction_MA_log ~ Order, data=Master_db2)

my_aov6 <- aov(Shoulder_Medial_Rotation_MA_log ~ Order, data=Master_db2)

my_aov7 <- aov(Shoulder_Lateral_Rotation_MA_log ~ Order, data=Master_db2)

my_aov8 <- aov(Elbow_Extension_MA_log ~ Order, data=Master_db2)

my_aov9 <- aov(Elbow_Flexion_MA_log ~ Order, data=Master_db2)

my_aov10 <- aov(Wrist_Extension_MA_log ~ Order, data=Master_db2)

my_aov11 <- aov(Wrist_Flexion_MA_log ~ Order, data=Master_db2)

summary(my_aov1)
summary(my_aov2)
summary(my_aov3)
summary(my_aov4)
summary(my_aov5)
summary(my_aov6)
summary(my_aov7)
summary(my_aov8)
summary(my_aov9)
summary(my_aov10)
summary(my_aov11)

#ggplot LDA ------

library(MASS)


  #All Traits------

LDA_df4 <- Master_db[c(3,5:7,9:31)]

LDA_df4_ALT <- LDA_df4[c(92:94,96,121:123,126:130,143:150,194:198,237:241,247:250),]
LDA_df4 <- LDA_df4[-c(92:94,96,121:123,126:130,143:150,194:198,237:241,247:250),]

LDA_df4 <- na.omit(LDA_df4)
LDA_df4 <- bind_rows(LDA_df4,LDA_df4_ALT)

#HR Locomotor type
LDA_df4$Shoulder_Extension[is.na(LDA_df4$Shoulder_Extension)]<-mean(LDA_df4$Shoulder_Extension,na.rm=TRUE)
LDA_df4$Shoulder_Medial_Rotation[is.na(LDA_df4$Shoulder_Medial_Rotation)]<-mean(LDA_df4$Shoulder_Medial_Rotation,na.rm=TRUE)

#H&P & SW Locomotor types
LDA_df4$Wrist_Flexion[is.na(LDA_df4$Wrist_Flexion)]<-mean(LDA_df4$Wrist_Flexion,na.rm=TRUE)
LDA_df4$Wrist_Extension[is.na(LDA_df4$Wrist_Extension)]<-mean(LDA_df4$Wrist_Extension,na.rm=TRUE)

#UF and US 
LDA_df4$HW[is.na(LDA_df4$HW)]<-mean(LDA_df4$HW,na.rm=TRUE)
LDA_df4$FW[is.na(LDA_df4$FW)]<-mean(LDA_df4$FW,na.rm=TRUE)
LDA_df4$TPL[is.na(LDA_df4$TPL)]<-mean(LDA_df4$TPL,na.rm=TRUE)

#SD golden moles 
LDA_df4$Elbow_Flexors[is.na(LDA_df4$Elbow_Flexors)]<-mean(LDA_df4$Elbow_Flexors,na.rm=TRUE)

LDA_df4$Locomotor_type4 <- factor(LDA_df4$Locomotor_type4, levels = c("SD", "CT", "H&P", "HR", "LD", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))




Habit <- LDA_df4[c(1)]
LDA_df4 <- LDA_df4[-c(1)]
Name_ID2 <- LDA_df4[c(26)]
LDA_df4 <- LDA_df4[-c(26)]
LDA_df4 <- log10(LDA_df4)
LDA_df4 <- cbind(Name_ID2, LDA_df4)
LDA_df4 <- cbind(Habit, LDA_df4)

LDA_df4 <- na.omit(LDA_df4)

LDA_df4_num <- LDA_df4[,sapply(LDA_df4,is.numeric)]


LDA_df4_mat <- as.matrix(LDA_df4_num)


#perform LDA
ld2 <- lda(LDA_df4$Locomotor_type4~LDA_df4_mat)

ld2pred <- predict(ld2)

ld2table <- table(ld2pred$class,LDA_df4$Locomotor_type4)
ld2table


#misclassification rates
sum(diag(ld2table))/sum(ld2table) * 100
#  87.27273
ld2table[1,1]/sum(ld2table[,1]) * 100
#SD: 73.68421
ld2table[2,2]/sum(ld2table[,2]) * 100
#CT: 100
ld2table[3,3]/sum(ld2table[,3]) * 100
#H&P: 100
ld2table[4,4]/sum(ld2table[,4]) * 100
#HR: 100
ld2table[5,5]/sum(ld2table[,5]) * 100
#LD: 100
ld2table[6,6]/sum(ld2table[,6]) * 100
#FP: 60
ld2table[7,7]/sum(ld2table[,7]) * 100
#UF: 100
ld2table[8,8]/sum(ld2table[,8]) * 100
#HP: 66.66667
ld2table[9,9]/sum(ld2table[,9]) * 100
#US: 100
ld2table[10,10]/sum(ld2table[,10]) * 100
#TER: 94.44444  
ld2table[11,11]/sum(ld2table[,11]) * 100
#RIC: 100
ld2table[12,12]/sum(ld2table[,12]) * 100
#QC: 83.33333
ld2table[13,13]/sum(ld2table[,13]) * 100
#QS: 100
ld2table[14,14]/sum(ld2table[,14]) * 100
#BRA: 100

aggregate(x=LDA_df4_num,by=list(LDA_df4$Locomotor_type4),FUN=mean)


MamProjection <- cbind(scale(LDA_df4_num,scale=FALSE) %*% ld2$scaling,LDA_df4[,1,drop=FALSE])

AllTraitLDA <- ggplot(data=MamProjection, aes(x=LD1,y=LD2, color = Locomotor_type4, fill = Locomotor_type4, group=Locomotor_type4, shape = Locomotor_type4)) +
  stat_ellipse(aes(fill = Locomotor_type4), geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point() + ylab("All Traits") +
  scale_shape_manual(values=c(0, 1, 2, 5, 6, 7, 9, 12, 14, 17, 18, 8, 3, 4))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  theme(plot.title = element_text(hjust = 0.5, size = 40, face = "bold"), axis.title.x=element_blank(), axis.title.y = element_text(size = 11)) 


print(AllTraitLDA)

#All Traits LDA
ggsave(filename = "LDA_AllTraits_AllNewCharacters_ggplot.png", plot = AllTraitLDA, width = 14, height = 8, 
       units = c("in"), dpi = 250)


### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df4$Locomotor_type4~LDA_df4_mat, CV = TRUE)

ld1table_cv <- table(LDA_df4$Locomotor_type4,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV: 74.54545
sum(diag(ld2table))/sum(ld2table) * 100
#LDA: 87.27273

(sum(diag(ld2table))/sum(ld2table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld2pred$posterior[1,]

#difference
(ld1_cv$posterior - ld2pred$posterior) %>% head()



### Permutation Test ###
# run All Traits LDA before running Permutation Test
{
  
  {pnum <- 1000
  RSLT <- as.data.frame(matrix(,pnum+1,3,dimnames=list(seq(1,pnum+1),c("N","Correct","Type"))))
  RSLT[1,1] <- sum(ld2table)
  RSLT[1,2] <- sum(diag(ld2table))
  RSLT[1,3] <- "Raw_Results"
  }
  # the following section takes a really long time to run
  #install.packages(permute)
  
  library(permute)
  
  msrs <- LDA_df4[,colnames(LDA_df4)!="Locomotor_type4"]
  fnc <- as.character(LDA_df4[,colnames(LDA_df4)=="Locomotor_type4"])
  for(i in 2:(pnum+1)){
    tfnc <- fnc[shuffle(length(fnc))]
    tdat <- cbind(FNC=as.factor(tfnc),msrs)
    tryCatch(ldt <- lda(FNC~.,data=tdat),error=function(e){i=i-1;break()})
    ldpredt <- predict(ldt)
    tbt <- table(predict(ldt)$class,LDA_df4$Locomotor_type4)
    RSLT[i,1] <- sum(tbt)
    RSLT[i,2] <- sum(diag(tbt))
    RSLT[i,3] <- "Randomized"
  }
  #wait while R thinks...
  RSLT <- na.omit(RSLT)
  RSLT$Type <- as.factor(RSLT$Type)
  x11();hist(RSLT$Correct,breaks=50,main = "Random vs. Real") 
  #looking for 50% mean, if in bell curve your data stands out, then it wasn't by chance 
  
  hist.grp <- function(x,grp,brks=20,clist=c("midnightblue","maroon","orchid2",
                                             "royalblue1","seagreen"),legend.pos="topright",xlab="LDA Results",ylab="Frequency",
                       main=NA,overlap=c("translucent","piled"),trans=0.5,lwd=2,lwd.axis=1.5,cex=1,
                       axis1=c("Values","Names","None"),axis2=T)
  {
    overlap <- match.arg(overlap)
    axis1 <- match.arg(axis1)
    if(length(brks)==1){
      binwidth=diff(range(x))/brks
      brks <- seq(min(x,na.rm=T),max(x,na.rm=T),binwidth)
    }
    n=length(brks)-1
    if(!is.factor(grp)) grp <- as.factor(grp)
    ngrp <- nlevels(grp)
    hist0 <- hist(x,breaks=brks,plot=F)
    hists <- vector("list",ngrp)
    for(i in 1:ngrp){
      hists[[i]] <- hist(x[grp==levels(grp)[i]],breaks=brks,plot=F)
    }
    counts <- NULL;for(i in 1:ngrp) counts <- rbind(counts,
                                                    hists[[i]]$counts)
    cs0 <- hist0$counts
    if(overlap=="translucent"){
      cs <- counts[1,]
      barplot(cs,col=alpha(clist[1],trans),space=0,xlim=c(0,n),
              ylim=c(0,max(counts)),xlab=xlab,ylab=ylab,main=main,axes=F,lwd=lwd)
      for(i in 2:ngrp){
        cs <- counts[i,]
        barplot(cs,col=alpha(clist[i],trans),space=0,add=T,axes=F)
      }
    }
    if(overlap=="piled"){
      barplot(cs0,col=clist[1],xlab=xlab,ylab=ylab,space=0,xlim=c(0,n),main=main,
              axes=F,lwd=lwd)
      cs <- cs0
      for(i in 2:ngrp){
        cs <- cs-counts[i-1,]
        barplot(cs,col=clist[i],space=0,add=T,axes=F)
      }
    }
    if(axis1=="Values")
      axis(1,at=c(0:n),labels=round(brks,2),las=3,lwd=lwd.axis)
    span <- dist(range(brks))
    xs <- (x-min(brks))*n/span
    if(axis1=="Names") axis(1,at=xs,labels=names(x),las=3,lwd=lwd.axis)
    if(axis2) axis(2,las=1,lwd=lwd.axis)
    box(lwd=lwd.axis)
    legend(legend.pos,levels(grp),fill=clist[1:ngrp])
    invisible(hists)
  }
  
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA with All Traits",
           brks=c(seq(0,150,5)),xlab="Correctly Classified Species #")
  
  #Figure S6
  pdf(file=paste0("Final_Permutation_all_traits",pnum,".pdf"),w=11,h=6)
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA with All Traits",
           brks=c(seq(0,150,5)),xlab="Correctly Classified Species #")  
  dev.off()
  
  summary(aov(Correct~Type,data=RSLT))
  
  
}






#without HR, LD, (H&P), QS, BRA, US, UF, RIC, CT (100% CR Locomotor Types)
LDA_df44 <- LDA_df4[(LDA_df4$Locomotor_type4 %in% c("SD", "FP", "HP", "CUR", "QC")),]  
LDA_df44$Locomotor_type4 <- factor(LDA_df44$Locomotor_type4, levels = c("SD", "FP", "HP", "CUR", "QC"))

#add ID for labeling outliers
Name_ID3 <- LDA_df44[c(2)]


LDA_df44_num <- LDA_df44[,sapply(LDA_df44,is.numeric)]

LDA_df44_mat <- as.matrix(LDA_df44_num)



ld44 <- lda(LDA_df44$Locomotor_type4~LDA_df44_mat)

ld44pred <- predict(ld44)



ld44table <- table(ld44pred$class,LDA_df44$Locomotor_type4)
ld44table

sum(diag(ld44table))/sum(ld44table) * 100
#  86.61417
ld44table[1,1]/sum(ld44table[,1]) * 100
#SD:  78.94737
ld44table[2,2]/sum(ld44table[,2]) * 100
#FP:  60
ld44table[3,3]/sum(ld44table[,3]) * 100
#HP: 100
ld44table[4,4]/sum(ld44table[,4]) * 100
#CUR:  98.14815
ld44table[5,5]/sum(ld44table[,5]) * 100
#QC:  75


aggregate(x=LDA_df44_num,by=list(LDA_df44$Locomotor_type4),FUN=mean)

MamProjection44 <- cbind(scale(LDA_df44_num,scale=FALSE) %*% ld44$scaling,LDA_df44[,1,drop=FALSE])

#add ID for labeling outliers 
MamProjection44 <- cbind(Name_ID3, MamProjection44)


AllTraitLDANOHRLD <- ggplot(data=MamProjection44, aes(x=LD1,y=LD2, color = Locomotor_type4, fill = Locomotor_type4, group=Locomotor_type4, shape=Locomotor_type4)) + 
  stat_ellipse(aes(fill = Locomotor_type4), geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point() + 
  ylab("LD2") + xlab("LD1") +
  scale_shape_manual(values=c(0, 7, 12, 17, 8))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank()) # +
#code to add outliers 
#geom_text(aes(label = Name_ID), nudge_y = 0.3) #labels avoid other labels -> position = position_jitter(width=1,height=1)


print(AllTraitLDANOHRLD)

#LDA without 100% CR Locomotor types 
ggsave(filename = "LDA4_AllTraits__AllNewCharacters_ggplot_NOHRLD.png", plot = AllTraitLDANOHRLD, width = 14, height = 8,
       units = c("in"), dpi = 250)


### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df44$Locomotor_type4~LDA_df44_mat, CV = TRUE)

ld1table_cv <- table(LDA_df44$Locomotor_type4,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV: 67.71654
sum(diag(ld44table))/sum(ld44table) * 100
#LDA: 86.61417

(sum(diag(ld44table))/sum(ld44table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld44pred$posterior[1,]

#difference
(ld1_cv$posterior - ld44pred$posterior) %>% head()



  #All Head & Forelimb Traits------

LDA_df5 <- Master_db[c(2,3,5:7,9:24)]


LDA_df5_ALT <- LDA_df5[c(92:94,96,121:123,126:130,143:150,194:198,237:241,247:250,270:278),]
LDA_df5 <- LDA_df5[-c(92:94,96,121:123,126:130,143:150,194:198,237:241,247:250,270:278),]

LDA_df5 <- na.omit(LDA_df5)

LDA_df5 <- bind_rows(LDA_df5,LDA_df5_ALT)

#HR Locomotor type 
LDA_df5$Shoulder_Extension[is.na(LDA_df5$Shoulder_Extension)]<-mean(LDA_df5$Shoulder_Extension,na.rm=TRUE)
LDA_df5$Shoulder_Medial_Rotation[is.na(LDA_df5$Shoulder_Medial_Rotation)]<-mean(LDA_df5$Shoulder_Medial_Rotation,na.rm=TRUE)

#change NAs in AQ/ SD golden moles 
LDA_df5$Elbow_Flexors[is.na(LDA_df5$Elbow_Flexors)]<-mean(LDA_df5$Elbow_Flexors,na.rm=TRUE)
LDA_df5$TML[is.na(LDA_df5$TML)]<-mean(LDA_df5$TML,na.rm=TRUE)

#SW UF US Locomotor type
LDA_df5$Wrist_Flexion[is.na(LDA_df5$Wrist_Flexion)]<-mean(LDA_df5$Wrist_Flexion,na.rm=TRUE)
LDA_df5$Wrist_Extension[is.na(LDA_df5$Wrist_Extension)]<-mean(LDA_df5$Wrist_Extension,na.rm=TRUE)

#H&P locomotor type 
LDA_df5$Wrist_Flexion[is.na(LDA_df5$Wrist_Flexion)]<-mean(LDA_df5$Wrist_Flexion,na.rm=TRUE)
LDA_df5$Wrist_Extension[is.na(LDA_df5$Wrist_Extension)]<-mean(LDA_df5$Wrist_Extension,na.rm=TRUE)
LDA_df5$ML[is.na(LDA_df5$ML)]<-mean(LDA_df5$ML,na.rm=TRUE)



LDA_df5 <- na.omit(LDA_df5)

Name_IDcol <- LDA_df5[c(1)]



LDA_df5 <- LDA_df5[-c(1)]

LDA_df5$Locomotor_type4 <- factor(LDA_df5$Locomotor_type4, levels = c("SD", "CT", "H&P", "HR", "LD", "AQ", "SW", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))


Habit <- LDA_df5[c(1)]
LDA_df5 <- LDA_df5[-c(1)]
LDA_df5 <- log10(LDA_df5)
LDA_df5 <- cbind(Habit, LDA_df5)


LDA_df5_num <- LDA_df5[,sapply(LDA_df5,is.numeric)]


LDA_df5_mat <- as.matrix(LDA_df5_num)
ld1 <- lda(LDA_df5$Locomotor_type4~LDA_df5_mat)

ld1pred <- predict(ld1)


ld1table <- table(ld1pred$class,LDA_df5$Locomotor_type4)
ld1table


#classification rate
sum(diag(ld1table))/sum(ld1table) * 100
# 82.7027

ld1table[1,1]/sum(ld1table[,1]) * 100
#SD:  71.05263
ld1table[2,2]/sum(ld1table[,2]) * 100
#CT: 100  
ld1table[3,3]/sum(ld1table[,3]) * 100
#H&P: 100
ld1table[4,4]/sum(ld1table[,4]) * 100
#HR: 100
ld1table[5,5]/sum(ld1table[,5]) * 100
#LD: 100
ld1table[6,6]/sum(ld1table[,6]) * 100
#AQ: 100
ld1table[7,7]/sum(ld1table[,7]) * 100
#SW: 100
ld1table[8,8]/sum(ld1table[,8]) * 100
#FP: 20
ld1table[9,9]/sum(ld1table[,9]) * 100
#UF: 100
ld1table[10,10]/sum(ld1table[,10]) * 100
#HP: 37.5
ld1table[11,11]/sum(ld1table[,11]) * 100
#US: 100
ld1table[12,12]/sum(ld1table[,12]) * 100
#TER: 87.93103
ld1table[13,13]/sum(ld1table[,13]) * 100
#RIC: 100
ld1table[14,14]/sum(ld1table[,14]) * 100
#QC: 80.76923
ld1table[15,15]/sum(ld1table[,15]) * 100
#QS: 100
ld1table[16,16]/sum(ld1table[,16]) * 100
#BRA: 100


MamProjection2 <- cbind(scale(LDA_df5_num,scale=FALSE) %*% ld1$scaling,LDA_df5[,1,drop=FALSE])


AllSpeciesLDA <- ggplot(data=MamProjection2, aes(x=LD1,y=LD2, color = Locomotor_type4, fill = Locomotor_type4, group=Locomotor_type4)) + 
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  stat_ellipse( geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point() + 
  ggtitle("LDA of 185 Species and Head/Forelimb (19) Traits") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) #+

AllSpeciesLDA

#LDA with head/forelimb traits
ggsave(filename = "LDA_AllNewCharacters_ggplot_logged.png", plot = AllSpeciesLDA, width = 14, height = 8, 
       units = c("in"), dpi = 250)

#for final figure 
AllSpeciesLDA2 <- ggplot(data=MamProjection2, aes(x=LD1,y=LD2, color = Locomotor_type4, fill = Locomotor_type4, group=Locomotor_type4, shape = Locomotor_type4)) + 
  stat_ellipse( geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point() + ylab("All Head & Forelimb Traits") +
  scale_shape_manual(values=c(0, 1, 2, 5, 6, 15, 19, 7, 9, 12, 14, 17, 18, 8, 3, 4))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  guides(fill='none') +
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_text(size = 11)) 


AllSpeciesLDA2


### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df5$Locomotor_type4~LDA_df5_mat, CV = TRUE)

ld1table_cv <- table(LDA_df5$Locomotor_type4,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV: 70.81081
sum(diag(ld1table))/sum(ld1table) * 100
#LDA: 82.7027

(sum(diag(ld1table))/sum(ld1table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld1pred$posterior[1,]

#difference
(ld1_cv$posterior - ld1pred$posterior) %>% head()



### Permutation Test ###
# run Head and Forelimb LDA before running Permutation Test
{
  
  {pnum <- 1000
  RSLT <- as.data.frame(matrix(,pnum+1,3,dimnames=list(seq(1,pnum+1),c("N","Correct","Type"))))
  RSLT[1,1] <- sum(ld1table)
  RSLT[1,2] <- sum(diag(ld1table))
  RSLT[1,3] <- "Raw_Results"
  }
  # the following section takes a really long time to run
  #install.packages(permute)
  
  library(permute)
  
  msrs <- LDA_df5[,colnames(LDA_df5)!="Locomotor_type4"]
  fnc <- as.character(LDA_df5[,colnames(LDA_df5)=="Locomotor_type4"])
  for(i in 2:(pnum+1)){
    tfnc <- fnc[shuffle(length(fnc))]
    tdat <- cbind(FNC=as.factor(tfnc),msrs)
    tryCatch(ldt <- lda(FNC~.,data=tdat),error=function(e){i=i-1;break()})
    ldpredt <- predict(ldt)
    tbt <- table(predict(ldt)$class,LDA_df5$Locomotor_type4)
    RSLT[i,1] <- sum(tbt)
    RSLT[i,2] <- sum(diag(tbt))
    RSLT[i,3] <- "Randomized"
  }
  #wait while R thinks...
  RSLT <- na.omit(RSLT)
  RSLT$Type <- as.factor(RSLT$Type)
  x11();hist(RSLT$Correct,breaks=50,include.lowest=F,main = "Random vs. Real")
  #looking for 50% mean, if in bell curve your data stands out, then it wasn't by chance 
  
  hist.grp <- function(x,grp,brks=20,clist=c("midnightblue","maroon","orchid2",
                                             "royalblue1","seagreen"),legend.pos="topright",xlab="LDA Results",ylab="Frequency",
                       main=NA,overlap=c("translucent","piled"),trans=0.5,lwd=2,lwd.axis=1.5,cex=1,
                       axis1=c("Values","Names","None"),axis2=T)
  {
    overlap <- match.arg(overlap)
    axis1 <- match.arg(axis1)
    if(length(brks)==1){
      binwidth=diff(range(x))/brks
      brks <- seq(min(x,na.rm=T),max(x,na.rm=T),binwidth)
    }
    n=length(brks)-1
    if(!is.factor(grp)) grp <- as.factor(grp)
    ngrp <- nlevels(grp)
    hist0 <- hist(x,breaks=brks,plot=F)
    hists <- vector("list",ngrp)
    for(i in 1:ngrp){
      hists[[i]] <- hist(x[grp==levels(grp)[i]],breaks=brks,plot=F)
    }
    counts <- NULL;for(i in 1:ngrp) counts <- rbind(counts,
                                                    hists[[i]]$counts)
    cs0 <- hist0$counts
    if(overlap=="translucent"){
      cs <- counts[1,]
      barplot(cs,col=alpha(clist[1],trans),space=0,xlim=c(0,n),
              ylim=c(0,max(counts)),xlab=xlab,ylab=ylab,main=main,axes=F,lwd=lwd)
      for(i in 2:ngrp){
        cs <- counts[i,]
        barplot(cs,col=alpha(clist[i],trans),space=0,add=T,axes=F)
      }
    }
    if(overlap=="piled"){
      barplot(cs0,col=clist[1],xlab=xlab,ylab=ylab,space=0,xlim=c(0,n),main=main,
              axes=F,lwd=lwd)
      cs <- cs0
      for(i in 2:ngrp){
        cs <- cs-counts[i-1,]
        barplot(cs,col=clist[i],space=0,add=T,axes=F)
      }
    }
    if(axis1=="Values")
      axis(1,at=c(0:n),labels=round(brks,2),las=3,lwd=lwd.axis)
    span <- dist(range(brks))
    xs <- (x-min(brks))*n/span
    if(axis1=="Names") axis(1,at=xs,labels=names(x),las=3,lwd=lwd.axis)
    if(axis2) axis(2,las=1,lwd=lwd.axis)
    box(lwd=lwd.axis)
    legend(legend.pos,levels(grp),fill=clist[1:ngrp])
    invisible(hists)
  }
  
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA with Forebody Traits",
           brks=c(seq(0,160,5)),xlab="Correctly Classified Species #")
  
  #Figure S6
  pdf(file=paste0("Final_Permutation_forebody_traits",pnum,".pdf"),w=11,h=6)
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA with Forebody Traits",
           brks=c(seq(15,160,5)),xlab="Correctly Classified Species #")  
  dev.off()
  
  summary(aov(Correct~Type,data=RSLT))
  
  
  
}




### Posterior Probability Plot - Supplemental Figures

#y.label change to species name
rownames <- Name_IDcol
rownames[c(1),]

postplot <- function(x,col=gray((100:0)/100),main="",usepar=TRUE,yaxis=TRUE,x.tick.nam=colnames(x),cex.axis=1)
{
  groups <- factor(colnames(x))
  ylabel <- rownames(x)
  #  x <- x[rev(row(x)[,1]),]
  if(usepar) oldpar <- par(no.readonly=TRUE)
  if(usepar) par(mar=c(5,17,4,2)+0.1)
  image(t(x),axes=FALSE,col=col,main=main)
  axis(1,at=seq(0,1,1/(nlevels(groups)-1)),labels=x.tick.nam)
  if(yaxis) axis(2,at=seq(0,1,1/(nrow(x)-1)),labels=ylabel,las=1,cex.axis=cex.axis)
  box()
  abline(h=seq(0,1,1/(nrow(x)-1))+0.5/(nrow(x)-1))
  if(usepar) par(oldpar)
}

pred <- ld1pred[["posterior"]]

pred <- cbind(Name_IDcol,pred)
pred <- data.frame(pred, row.names = 1)
ld1pred130 <- pred[c(1:30),]
postplot(ld1pred130)
#save individual plots Figures S10 - S15


ld1pred3160 <- pred[c(31:60),]
postplot(ld1pred3160)


ld1pred6190 <- pred[c(61:90),]
postplot(ld1pred6190)


ld1pred91120 <- pred[c(91:120),]
postplot(ld1pred91120)


ld1pred121150 <- pred[c(121:150),]
postplot(ld1pred121150)


ld1pred151185 <- pred[c(151:185),]
postplot(ld1pred151185)



#LDA of forelimb/head traits (without LD & AQ & HR, CT, RIC, UF, US, QS, BRA)
LDA_df6 <- LDA_df5[(LDA_df5$Locomotor_type4 %in% c("SD", "FP", "HP", "CUR", "QC")),]  


LDA_df6$Locomotor_type4 <- factor(LDA_df6$Locomotor_type4, levels = c("SD", "FP", "HP", "CUR", "QC"))


LDA_df6_num <- LDA_df6[,sapply(LDA_df6,is.numeric)]


LDA_df6_mat <- as.matrix(LDA_df6_num)



ld1 <- lda(LDA_df6$Locomotor_type4~LDA_df6_mat)


for(i in 1:20) print(ld1[i])
ld1pred <- predict(ld1)


ld1table <- table(ld1pred$class,LDA_df6$Locomotor_type4)
ld1table

#misclassification rate
sum(diag(ld1table))/sum(ld1table) * 100
# 78.51852

ld1table[1,1]/sum(ld1table[,1]) * 100
#SD:  73.68421
ld1table[2,2]/sum(ld1table[,2]) * 100
#FP: 20
ld1table[3,3]/sum(ld1table[,3]) * 100
#HP: 37.5
ld1table[4,4]/sum(ld1table[,4]) * 100
#CUR: 91.37931
ld1table[5,5]/sum(ld1table[,5]) * 100
#QC: 80.76923

MamProjection3 <- cbind(scale(LDA_df6_num,scale=FALSE) %*% ld1$scaling,LDA_df6[,1,drop=FALSE])


AllSpeciesNOLDAQHRLDA <- ggplot(data=MamProjection3, aes(x=LD1,y=LD2,color = Locomotor_type4, fill = Locomotor_type4, group=Locomotor_type4, shape=Locomotor_type4)) + 
  stat_ellipse( geom = "polygon", type = "t", level = 0.95, alpha = 0.1) +
  geom_point() + 
  scale_shape_manual(values=c(0, 7, 12, 17,8))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) + 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())


print(AllSpeciesNOLDAQHRLDA)

#LDA with forelimb/head traits and without 100% CR locomotor types 
ggsave(filename = "LDA_AllNewCharacters_ggplot_logged_NOLDAQHR.png", plot = AllSpeciesNOLDAQHRLDA, width = 14, height = 8,
       units = c("in"), dpi = 250)




### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df6$Locomotor_type4~LDA_df6_mat, CV = TRUE)

ld1table_cv <- table(LDA_df6$Locomotor_type4,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV: 66.66667
sum(diag(ld1table))/sum(ld1table) * 100
#LDA: 82.7027

(sum(diag(ld1table))/sum(ld1table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld1pred$posterior[1,]

#difference
(ld1_cv$posterior - ld1pred$posterior) %>% head()



### Permutation Test ###
# run LDA and J-Best before running Permutation Test
{
  
  {pnum <- 1000
  RSLT <- as.data.frame(matrix(,pnum+1,3,dimnames=list(seq(1,pnum+1),c("N","Correct","Type"))))
  RSLT[1,1] <- sum(ld1table)
  RSLT[1,2] <- sum(diag(ld1table))
  RSLT[1,3] <- "Raw_Results"
  }
  # the following section takes a really long time to run
  #install.packages(permute)
  
  library(permute)
  
  msrs <- LDA_df6[,colnames(LDA_df6)!="Locomotor_type4"]
  fnc <- as.character(LDA_df6[,colnames(LDA_df6)=="Locomotor_type4"])
  for(i in 2:(pnum+1)){
    tfnc <- fnc[shuffle(length(fnc))]
    tdat <- cbind(FNC=as.factor(tfnc),msrs)
    tryCatch(ldt <- lda(FNC~.,data=tdat),error=function(e){i=i-1;break()})
    ldpredt <- predict(ldt)
    tbt <- table(predict(ldt)$class,LDA_df6$Locomotor_type4)
    RSLT[i,1] <- sum(tbt)
    RSLT[i,2] <- sum(diag(tbt))
    RSLT[i,3] <- "Randomized"
  }
  RSLT <- na.omit(RSLT)
  RSLT$Type <- as.factor(RSLT$Type)
  x11();hist(RSLT$Correct,breaks=20,include.lowest=F,main = "Random vs. Real")
  #looking for 50% mean, if in bell curve your data stands out, then it wasn't by chance 
  
  #Ryosuke histogram code
  hist.grp <- function(x,grp,bn=20,clist=c("midnightblue","maroon","orchid2",
                                           "royalblue1","seagreen"),legend.pos="topright",xlab="X",ylab="Frequency",
                       main=NA,overlap=c("translucent","piled"),trans=0.5,lwd=2,lwd.axis=1.5,cex=1,
                       axis1=c("Values","Names","None"),axis2=T)
  {
    overlap <- match.arg(overlap)
    axis1 <- match.arg(axis1)
    binwidth=diff(range(x))/bn
    brks <- seq(min(x,na.rm=T),max(x,na.rm=T),binwidth)
    n=length(brks)-1
    mds <- seq(floor(min(x,na.rm=T))+binwidth/2,
               ceiling(max(x,na.rm=T))-binwidth/2,binwidth)
    if(!is.factor(grp)) grp <- as.factor(grp)
    ngrp <- nlevels(grp)
    hist0 <- hist(x,breaks=brks,plot=F)
    hists <- vector("list",ngrp)
    #layout(matrix(seq(1,ngrp),ngrp,1))
    for(i in 1:ngrp){
      hists[[i]] <- hist(x[grp==levels(grp)[i]],breaks=brks,plot=F)
    }
    counts <- NULL;for(i in 1:ngrp) counts <- rbind(counts,
                                                    hists[[i]]$counts)
    cs0 <- hist0$counts
    if(overlap=="translucent"){
      # barplot(cs0,xlab=xlab,ylab=ylab,space=0,xlim=c(0,n),main=main,plot=F)
      cs <- counts[1,]
      barplot(cs,col=alpha(clist[1],trans),space=0,xlim=c(0,n),
              ylim=c(0,max(counts)),xlab=xlab,ylab=ylab,main=main,axes=F,lwd=lwd)
      for(i in 2:ngrp){
        cs <- counts[i,]
        barplot(cs,col=alpha(clist[i],trans),space=0,add=T,axes=F)
      }
    }
    if(overlap=="piled"){
      barplot(cs0,col=clist[1],xlab=xlab,ylab=ylab,space=0,xlim=c(0,n),main=main,
              axes=F,lwd=lwd)
      #barplot(hists[[1]]$counts,col=clist[1],xlab=xlab,ylab=ylab,space=0,xlim=c(0,n))
      cs <- cs0
      for(i in 2:ngrp){
        cs <- cs-counts[i-1,]
        barplot(cs,col=clist[i],space=0,add=T,axes=F)
      }
    }
    if(axis1=="Values")
      axis(1,at=c(0:n),labels=round(brks,2),las=3,lwd=lwd.axis)
    span <- dist(range(brks))
    xs <- (x-min(brks))*n/span
    if(axis1=="Names") axis(1,at=xs,labels=names(x),las=3,lwd=lwd.axis)
    if(axis2) axis(2,las=1,lwd=lwd.axis)
    box(lwd=lwd.axis)
    legend(legend.pos,levels(grp),fill=clist[1:ngrp])
    invisible(hists)
  }
  
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA")
  #hist(RSLT$Correct,main="LDA")
  #RSLT
  abline(v=sum(diag(ld2table)),col=2)
  summary(aov(Correct~Type,data=RSLT))
  
  pdf(file=paste0("Final_Permutation_forebody_lesscat",pnum,".pdf"),w=4,h=4.5)
  hist.grp(RSLT$Correct,RSLT$Type,overlap="piled",main="LDA")
  dev.off()
  
  summary(aov(Correct~Type,data=RSLT))
  
  
}



  #As Many Taxa As Possible------

LDA_df <- Master_db[c(3,5:7,9:18)]
colnames(LDA_df)[1] <- "Locomotor type"

#HR locomotor type
LDA_df$Shoulder_Extension[is.na(LDA_df$Shoulder_Extension)]<-mean(LDA_df$Shoulder_Extension,na.rm=TRUE)
LDA_df$Shoulder_Medial_Rotation[is.na(LDA_df$Shoulder_Medial_Rotation)]<-mean(LDA_df$Shoulder_Medial_Rotation,na.rm=TRUE)


LDA_df <- na.omit(LDA_df)

LDA_df$`Locomotor type` <- factor(LDA_df$`Locomotor type`, levels = c("SD", "CT", "H&P", "HR", "LD", "AQ", "SW", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))

Habit <- LDA_df[c(1)]
LDA_df <- LDA_df[-c(1)]
LDA_df <- log10(LDA_df)
LDA_df <- cbind(Habit, LDA_df)

LDA_df_num <- LDA_df[,sapply(LDA_df,is.numeric)]


LDA_df_mat <- as.matrix(LDA_df_num)

ld3 <- lda(LDA_df$`Locomotor type`~LDA_df_mat)


for(i in 1:20) print(ld3[i]) 
ld3pred <- predict(ld3)

ld3table <- table(ld3pred$class,LDA_df$`Locomotor type`)
ld3table

#classification rates
(sum(diag(ld3table))/sum(ld3table) * 100) %>% round(1)
# 72.7

ld3table[1,1]/sum(ld3table[,1]) * 100
#SD: 58.49057
ld3table[2,2]/sum(ld3table[,2]) * 100
#CT: 100
ld3table[3,3]/sum(ld3table[,3]) * 100
#H&P: 80
ld3table[4,4]/sum(ld3table[,4]) * 100
#HR: 100
ld3table[5,5]/sum(ld3table[,5]) * 100
#LD: 100  
ld3table[6,6]/sum(ld3table[,6]) * 100
#AQ: 100
ld3table[7,7]/sum(ld3table[,7]) * 100
#SW: 100
ld3table[8,8]/sum(ld3table[,8]) * 100
#FP:  0
ld3table[9,9]/sum(ld3table[,9]) * 100
#UF: 100
ld3table[10,10]/sum(ld3table[,10]) * 100
#HP: 30
ld3table[11,11]/sum(ld3table[,11]) * 100
#US: 100
ld3table[12,12]/sum(ld3table[,12]) * 100
#CUR: 79.77528
ld3table[13,13]/sum(ld3table[,13]) * 100
#RIC: 76.92308
ld3table[14,14]/sum(ld3table[,14]) * 100
#QC:  68.57143
ld3table[15,15]/sum(ld3table[,15]) * 100
#QS: 100
ld3table[16,16]/sum(ld3table[,16]) * 100
#BRA: 100 



MamProjection4 <- cbind(scale(LDA_df_num,scale=FALSE) %*% ld3$scaling,LDA_df[,1,drop=FALSE])

AllMamLDA <- ggplot(data=MamProjection4, aes(x=LD1,y=LD2, color = `Locomotor type`, fill = `Locomotor type`, group=`Locomotor type`)) +
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  geom_point() + ggtitle("LDA of 256 Species & 13 Traits") +
  stat_ellipse(aes(fill = `Locomotor type`), geom = "polygon", type = "t", level = 0.95, alpha = 0.1) +
  guides(color=guide_legend(title="Locomotor type"), fill='none') +
  theme(plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))

print(AllMamLDA)

#LDA with as many taxa as possible
ggsave(filename = "LDA_AllMam_ggplot_logged.png", plot = AllMamLDA, width = 14, height = 8, 
       units = c("in"), dpi = 250)

AllMamLDA2 <- ggplot(data=MamProjection4, aes(x=LD1,y=LD2, color = `Locomotor type`, fill = `Locomotor type`, group=`Locomotor type`, shape=`Locomotor type`)) + 
  stat_ellipse( geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point() + ggtitle("All Locomotor Types") + ylab("As Many Taxa As Possible") +
  scale_shape_manual(values=c(0, 1, 2, 5, 6, 15, 19, 7, 9, 12, 14, 17, 18, 8, 3, 4))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), axis.title.y=element_text(size = 11), axis.title.x=element_blank()) 


AllMamLDA2



### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df$`Locomotor type`~LDA_df_mat, CV = TRUE)

ld1table_cv <- table(LDA_df$`Locomotor type`,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV: 66.01562
sum(diag(ld3table))/sum(ld3table) * 100
#LDA: 72.65625

(sum(diag(ld3table))/sum(ld3table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld3pred$posterior[1,]

#difference
(ld1_cv$posterior - ld3pred$posterior) %>% head()



#Without 100% CR Locomotor types AQ,LD,HR, CT...
LDA_df10 <- LDA_df[(LDA_df$`Locomotor type` %in% c("SD", "H&P", "FP", "HP", "CUR", "RIC", "QC")),]  


LDA_df10$`Locomotor type` <- factor(LDA_df10$`Locomotor type`, levels = c("SD", "H&P", "FP", "HP", "CUR", "RIC", "QC"))


LDA_df10_num <- LDA_df10[,sapply(LDA_df10,is.numeric)]

LDA_df10_mat <- as.matrix(LDA_df10_num)

ld1 <- lda(LDA_df10$`Locomotor type`~LDA_df10_mat)

for(i in 1:20) print(ld1[i])
ld1pred <- predict(ld1)

ld1table <- table(ld1pred$class,LDA_df10$`Locomotor type`)
ld1table

#classification rate
sum(diag(ld1table))/sum(ld1table) * 100
# 69.48357

ld1table[1,1]/sum(ld1table[,1]) * 100
#SD: 58.49057
ld1table[2,2]/sum(ld1table[,2]) * 100
#H&P: 80 
ld1table[3,3]/sum(ld1table[,3]) * 100
#FP: 0
ld1table[4,4]/sum(ld1table[,4]) * 100
#HP: 40
ld1table[5,5]/sum(ld1table[,5]) * 100
#CUR: 84.26966
ld1table[6,6]/sum(ld1table[,6]) * 100
#RIC: 76.92308
ld1table[7,7]/sum(ld1table[,7]) * 100
#QC: 68.57143



MamProjection13 <- cbind(scale(LDA_df10_num,scale=FALSE) %*% ld1$scaling,LDA_df10[,1,drop=FALSE])


AllspeciesNOLDAQHRLDA <- ggplot(data=MamProjection13, aes(x=LD1,y=LD2,color = `Locomotor type`, fill = `Locomotor type`, group=`Locomotor type`, shape=`Locomotor type`)) +
  stat_ellipse( geom = "polygon", type = "t", level = 0.90, alpha = 0.1) +
  geom_point() +  ggtitle("Without 100% CR Types") + 
  scale_shape_manual(values=c(0, 2, 7, 12, 17,18, 8))+
  scale_color_manual(values=mycolors2) +
  scale_fill_manual(values=mycolors2) + 
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())

print(AllspeciesNOLDAQHRLDA)


### Robustness against taxon removal-Cross-Validation(CV) test
ld1_cv <- lda(LDA_df10$`Locomotor type`~LDA_df10_mat, CV = TRUE)

ld1table_cv <- table(LDA_df10$`Locomotor type`,ld1_cv$class)
ld1table_cv

#overall classification rate
sum(diag(ld1table_cv))/sum(ld1table_cv) * 100
#CV:  63.38028
sum(diag(ld1table))/sum(ld1table) * 100
#LDA:  69.48357

(sum(diag(ld1table))/sum(ld1table) * 100) - (sum(diag(ld1table_cv))/sum(ld1table_cv) * 100)

ld1_cv$posterior[1,]
ld1pred$posterior[1,]

#difference
(ld1_cv$posterior - ld1pred$posterior) %>% head()



  #Final LDA Figure 9-----

#using gridExtra

require("gridExtra")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(AllMamLDA2)

AllMamLDA2 <-  AllMamLDA2 + annotate('text', label = "A", x = -19, y = 12, fontface = "bold", size = 8)
AllspeciesNOLDAQHRLDA <- AllspeciesNOLDAQHRLDA + annotate('text', label = "B", x = -10, y = 3, fontface = "bold", size = 8)
AllSpeciesLDA2 <- AllSpeciesLDA2  + annotate('text', label = "C", x = -20, y = 5, fontface = "bold", size = 8) # ylim(-5, 12)
AllSpeciesNOLDAQHRLDA <- AllSpeciesNOLDAQHRLDA + annotate('text', label = "D", x = -4.5, y = 3.5, fontface = "bold", size = 8) #x = -2.5, y = 3.75
AllTraitLDA <- AllTraitLDA + annotate('text', label = "E", x = -7, y = 14, fontface = "bold", size = 8)
AllTraitLDANOHRLD <- AllTraitLDANOHRLD + annotate('text', label = "F", x = -4.5, y = 3.5, fontface = "bold", size = 8) #x = -31, y = 5


FinalLDA3 <- grid.arrange(arrangeGrob(AllMamLDA2 + theme(legend.position="none"), AllspeciesNOLDAQHRLDA + theme(legend.position="none"), 
                                      AllSpeciesLDA2 + theme(legend.position="none"), AllSpeciesNOLDAQHRLDA + theme(legend.position="none"), 
                                      AllTraitLDA + theme(legend.position="none"),  AllTraitLDANOHRLD + theme(legend.position="none"), nrow =3),
                          right = mylegend, bottom="LD1", left="LD2")

#Figure 9
ggsave(filename = "FinalLDA.pdf", plot =  FinalLDA3, width = 12, height = 9, 
       units = c("in"), dpi = 300)


  #Contribution scores/Arrow Plot -----

#Run All Trait LDA code first
#Run this code first
{
  {
    require(grDevices)
    require(smatr)
    require(scales)
    require(MASS)
    require(phytools)
    
    ###
    ### Plotting Colors and Symbols
    ###
    
    #pointcolor <- c(442,561,518,386,656,67,588,545,619,497)
    #fillcolor <- c(439,558,514,383,652,63,585,542,616,494)
    
    pointcolor <- rep(c("royalblue4","salmon4","seagreen4","gold4","plum4","steelblue4","turquoise4","mistyrose4","lightgoldenrod4","palevioletred4"),3)
    fillcolor <- rep(c("royalblue1","salmon1","seagreen1","gold1","plum1","steelblue1","turquoise1","mistyrose1","lightgoldenrod1","palevioletred1"),3)
    bgcolor <- "white"
    pointsymbol <- rep(c(15,17,19,4,3,8,0,1,2,5,6,7),3)
    pointsymbol <-rep(c(21:25,3,4,8),5)
    
    ###
    ### Point Functions
    ###
    
    add.point.one <- function(x,y,series=2,colorfill=TRUE,fcolor=fillcolor,
                              pcolor=pointcolor,psymbol=pointsymbol,cex=1)
    {
      points(x,y,pch=psymbol[series],col=pcolor[series],cex=cex,bg=pcolor[series])
    }
    
    add.point.g <- function(x,y,grp,pcolor=pointcolor,fcolor=fillcolor,
                            psymbol=pointsymbol,cex=1)
    {
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      xrange <- range(x)
      yrange <- range(y)
      dset <- cbind(grp,x,y)
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[i],]
        if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
        add.point.one(x1,y1,i,pcolor=pcolor,fcolor=fcolor,psymbol=psymbol)
      }
    }
    
    ###
    ### Residual Functions
    ###
    
    #colorpattern="Sign";linecolor=pointcolor;orgn=F 
    add.resid.one <- function(x,y,series=1,a=0,b=1,method=c("SMA","MA","OLS"),
                              lty=1,lwd=1,linecolor=pointcolor,signcolor=c(4,2),orgn=F,
                              regress=F,colorpattern=c("Single","PerPoint","Sign")){
      method=match.arg(method)
      colorpattern=match.arg(colorpattern)
      xy <- as.data.frame(na.omit(cbind(x=x,y=y)))
      rsd <- xy$y-(b*xy$x+a)
      if(regress){
        reg1 <- sma(y~x,xy,method=method)
        if(orgn) reg1 <- sma(y~x+0,xy,method=method)
        rsd <- residuals(reg1)
      }
      colorsign <- round((sign(rsd)+2)^0.5,0)
      if(colorpattern=="Single"){
        for(i in 1:nrow(xy)){
          segments(xy[i,1],xy[i,2],xy[i,1],xy[i,2]-rsd[i],col=linecolor[series],
                   lwd=lwd,lty=lty)}
      }else if(colorpattern=="PerPoint"){
        for(i in 1:nrow(xy)){
          segments(xy[i,1],xy[i,2],xy[i,1],xy[i,2]-rsd[i],col=linecolor[i],
                   lwd=lwd,lty=lty)
        } 
      }else{
        for(i in 1:nrow(xy)){
          segments(xy[i,1],xy[i,2],xy[i,1],xy[i,2]-rsd[i],col=signcolor[colorsign[i]],
                   lwd=lwd,lty=lty)
        } 
      } 
    }
    
    ###
    ### Bagplot Functions
    ###
    
    add.bag.one <- function(x,y,series=2,colorfill=TRUE,fcolor=fillcolor,
                            pcolor=pointcolor,lwd=lwd,trans=0.5,bagfactor=3)
    {
      bag.one <- compute.bagplot(x,y,bagfactor)
      print("1")
      bag.points <- bag.one$hull.loop
      print(bag.points)
      if(colorfill) {polygon(bag.points,border=pcolor[series],col=scales::alpha(fcolor[series],trans),lwd=lwd)
      }else{polygon(bag.points,border=pcolor[series],lwd=lwd)}
      print("3")
      invisible(bag.points)
    }
    
    add.bag.g <- function(x,y,grp,pval=0.95,num=30,gorder=seq(1,length(x)),
                          colorfill=TRUE,lwd=2,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor,
                          trans=0.5,bagfactor=3)
    {
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      xrange <- range(x)
      yrange <- range(y)
      dset <- cbind(grp,x,y)
      if(colorfill){
        for (i in 1:gnum)
        {
          dset1 <- dset[grp==gnames[gorder[i]],]
          if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
          if(length(x1)>2) add.bag.one(x1,y1,i,colorfill=TRUE,lwd=lwd,pcolor=pcolor,
                                       fcolor=fcolor,trans=trans,bagfactor=bagfactor)
        }
      }
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
        if(length(x1)>2) add.bag.one(x1,y1,i,colorfill=FALSE,lwd=lwd,pcolor=pcolor,
                                     fcolor=fcolor,trans=trans,bagfactor=bagfactor)
      }
    }
    
    ###
    ### Hull Plot Functions
    ###
    
    add.ch.one <- function(x,y,series=2,xlim=range(x),ylim=range(y),main="",
                           xlab="x",ylab="y",colorfill=TRUE,fcolor=fillcolor,pcolor=pointcolor,
                           lwdv=lwd,trans=0.5){
      ch.list <- chull(x,y)
      ch.points <- cbind(x[ch.list],y[ch.list])
      if(colorfill) {polygon(ch.points,border=pcolor[series],col=scales::alpha(fcolor[series],trans),lwd=lwdv)
      }else{polygon(ch.points,border=pcolor[series],lwd=lwdv)}
      invisible(ch.points)
    }
    
    add.ch.g <- function(x,y,grp,pval=0.95,num=30,gorder=seq(1,length(x)),
                         colorfill=TRUE,lwd=2,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor,trans=0.5)
    {
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      xrange <- range(x)
      yrange <- range(y)
      dset <- cbind(grp,x,y)
      if(colorfill){
        for (i in 1:gnum)
        {
          dset1 <- dset[grp==gnames[gorder[i]],]
          if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
          if(length(x1)>2) add.ch.one(x1,y1,i,colorfill=TRUE,lwd=lwd,pcolor=pcolor,
                                      fcolor=fcolor,trans=trans)
          if(length(x1)==2) addregone(x1,y1,series=i,pcolor=pcolor)
        }
      }
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
        if(length(x1)>2) add.ch.one(x1,y1,i,colorfill=FALSE,lwd=lwd,pcolor=pcolor,
                                    fcolor=fcolor,trans=trans)
        if(length(x1)==2) addregone(x1,y1,series=i,pcolor=pcolor)
      }
    }
    
    ###
    ### OLS CI-PI Functions
    ###
    
    cipirange <- function(x,y,series=2,pval=0.95,num=30,sfig=4)
    {
      require(stats)
      new <- data.frame(x = seq(min(x,na.rm=T),max(x,na.rm=T),by=dist(range(x,na.rm=T))/num))
      pred.w.plim <- predict(lm(y ~ x),new,interval="prediction",level=pval)
      pred.w.clim <- predict(lm(y ~ x),new,interval="confidence",level=pval)
      cipipnts <- cbind(new$x,pred.w.clim,pred.w.plim[,-1])
      cipirng <- cbind(range(cipipnts[,1]),range(cipipnts[,-1]))
      cipirng
    }
    
    #series=2;pval=0.95;num=30;sfig=4;colorfill=TRUE;lwd=2;origin=FALSE;pcolor=pointcolor;fcolor=fillcolor;olscipi="CIPI";orgn=FALSE
    # adds OLS,Ci,and Pi with a specified Series color to an existing plot.  Just one group.  Used by OLSCiPiGrp
    addcipione <- function(x,y,series=2,pval=0.95,num=30,sfig=4,colorfill=TRUE,
                           lwd=2,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor,olscipi=c("CIPI","CI","PI"),
                           X=x,trans=0.5)
    {
      require(stats)
      olscipi=match.arg(olscipi)
      new <- data.frame(x = seq(min(c(x,X),na.rm=T),max(c(x,X),na.rm=T),by=dist(range(c(x,X),na.rm=T))/num))
      if(orgn){
        pred.w.plim <- predict(lm(y ~ x-1),new,interval="prediction",level=pval)[,-1]
        pred.w.clim <- predict(lm(y ~ x-1),new,interval="confidence",level=pval)[,-1]
      }else{
        pred.w.plim <- predict(lm(y ~ x),new,interval="prediction",level=pval)[,-1]
        pred.w.clim <- predict(lm(y ~ x),new,interval="confidence",level=pval)[,-1]
      }
      cipipnts <- cbind(new$x,pred.w.clim,pred.w.plim)
      pipnts <- cbind(c(cipipnts[,1],rev(cipipnts[,1])),c(cipipnts[,4],rev(cipipnts[,5])))
      cipnts <- cbind(c(cipipnts[,1],rev(cipipnts[,1])),c(cipipnts[,2],rev(cipipnts[,3])))
      if(olscipi=="CIPI"){
        if(colorfill) {polygon(pipnts[,1],pipnts[,2],col=scales::alpha(fcolor[series],trans),border=NA)}
        matplot(new$x,cbind(pred.w.clim,pred.w.plim[,]),lty=1,lwd=c(lwd/2,lwd/2,lwd/2,lwd/2),type="l",ylab="predicted y",add=TRUE,col=pcolor[series])
      }else if(olscipi=="CI"){
        if(colorfill) {polygon(cipnts[,1],cipnts[,2],col=scales::alpha(fcolor[series],trans),border=NA)}
        matplot(new$x,pred.w.clim,lty=1,lwd=c(lwd/2,lwd/2),type="l",ylab="predicted y",add=TRUE,col=pcolor[series])
      }else{
        if(colorfill) {polygon(pipnts[,1],pipnts[,2],col=scales::alpha(fcolor[series],trans),border=NA)}
        matplot(new$x,pred.w.plim,lty=1,lwd=c(lwd/2,lwd/2),type="l",ylab="predicted y",add=TRUE,col=pcolor[series])
      }
      return(cipipnts)
    }
    
    
    # Adds OLS,Ci,and Pi for each group to an existing plot
    addcipig <- function(x,y,grp,pval=0.95,num=30,gorder=seq(1,length(x)),
                         colorfill=TRUE,lwd=2,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor,X=NA,
                         olscipi=c("CIPI","CI","PI"),min.num.to.cipi=3,trans=0.5,GroupsToInterval=c(1:nlevels(grp)))
    {
      olscipi=match.arg(olscipi)
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      inum <- length(GroupsToInterval)
      xrange <- range(x)
      yrange <- range(y)
      dset <- cbind(grp,x,y)
      if(colorfill){
        for (j in 1:inum)
        {
          i <- GroupsToInterval[j]
          dset1 <- dset[grp==gnames[gorder[i]],]
          if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
          if(length(x1)>=min.num.to.cipi) addcipione(x1,y1,i,pval,num,colorfill=TRUE,
                                                     lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor,olscipi=olscipi,trans=trans,X=X)
        }
      }
      for (j in 1:inum)
      {
        i <- GroupsToInterval[j]
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
        if(length(x1)>=min.num.to.cipi) addcipione(x1,y1,i,pval,num,colorfill=FALSE,
                                                   lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor,olscipi=olscipi,trans=trans,X=X)
      }
    }
    
    
    ###
    ### Ellipse Functions
    ###
    
    ## Returns ellipse points after plotting the ellipse
    erange <- function(x,y,series=2,pval=0.95,num=30)
    {
      acc <- num
      alpha <- 1-pval
      vx <- var(x)
      vy <- var(y)
      vxy <- var(x,y)
      lambda <- eigen(var(cbind(x,y)))$values
      a <- sqrt(vxy^2/((lambda[2]-vx)^2+vxy^2))
      b <- (lambda[2]-vx)*a/vxy
      theta <- atan(a/b)
      k <- sqrt(-2*log(alpha))
      l1 <- sqrt(lambda[1])*k
      l2 <- sqrt(lambda[2])*k
      pvec <- 0:num
      x2right <- sin((pi*pvec)/(num*2))*l1
      x2 <- c(-rev(x2right),x2right )
      tmp <- 1-x2^2/l1^2
      y2 <- l2*sqrt(ifelse(tmp < 0,0,tmp))
      x2 <- c(x2,rev(x2))
      y2 <- c(y2,-rev(y2))
      s0 <- sin(theta)
      c0 <- cos(theta)
      xx <- c0*x2+s0*y2+mean(x)
      yy <- -s0*x2+c0*y2+mean(y)
      erng <- cbind(range(xx),range(yy))
      erng
    }
    
    addellipseone <- function(x,y,series=2,pval=0.95,num=30,colorfill=TRUE,lwd=2,
                              pcolor=pointcolor,fcolor=fillcolor,trans=0.5)
    {
      acc <- num
      trans <- 1-pval
      vx <- var(x,na.rm=T)
      vy <- var(y,na.rm=T)
      vxy <- var(x,y,na.rm=T)
      lambda <- eigen(var(na.omit(cbind(x,y))))$values
      a <- sqrt(vxy^2/((lambda[2]-vx)^2+vxy^2))
      b <- (lambda[2]-vx)*a/vxy
      theta <- atan(a/b)
      k <- sqrt(-2*log(trans))
      l1 <- sqrt(lambda[1])*k
      l2 <- sqrt(lambda[2])*k
      pvec <- 0:num
      x2right <- sin((pi*pvec)/(num*2))*l1
      x2 <- c(-rev(x2right),x2right )
      tmp <- 1-x2^2/l1^2
      y2 <- l2*sqrt(ifelse(tmp < 0,0,tmp))
      x2 <- c(x2,rev(x2))
      y2 <- c(y2,-rev(y2))
      s0 <- sin(theta)
      c0 <- cos(theta)
      xx <- c0*x2+s0*y2+mean(x,na.rm=T)
      yy <- -s0*x2+c0*y2+mean(y,na.rm=T)
      if(colorfill) {polygon(xx,yy,border=pcolor[series],col=scales::alpha(fcolor[series],trans),lwd=lwd,trans=trans)}else{
        polygon(xx,yy,border=pcolor[series],lwd=lwd)}
      epp <- cbind(xx,yy)
      return(epp)
    }
    
    addellipseg <- function(x,y,grp,pval=0.95,num=30,gorder=seq(1,length(x)),colorfill=TRUE,lwd=2,
                            pcolor=pointcolor,fcolor=fillcolor,min.n.to.ellipse=3,trans=0.5,GroupsToInterval=c(1:nlevels(grp)))
    {
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      inum <- length(GroupsToInterval)
      dset <- cbind(grp,x,y)
      if(colorfill){
        for (j in 1:inum)
        {
          i <- GroupsToInterval[j]
          dset1 <- dset[grp==gnames[gorder[i]],]
          if(!is.vector(dset1)){
            if(NROW(dset1)>= min.n.to.ellipse){
              x1 <- dset1[,2]
              y1 <- dset1[,3]
              addellipseone(x1,y1,i,pval,num,colorfill=TRUE,lwd=lwd,pcolor=pcolor,fcolor=fcolor,trans=trans)
            }
          }
        }
      }
      for (j in 1:inum)
      {
        i <- GroupsToInterval[j]
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(!is.vector(dset1)){
          if(NROW(dset1)>= min.n.to.ellipse){
            x1 <- dset1[,2]
            y1 <- dset1[,3]
            addellipseone(x1,y1,i,pval,num,colorfill=FALSE,lwd=lwd,pcolor=pcolor,fcolor=fcolor)
          }
        }
      }
    }
    
    
    ### Legacy Function: Makes a new scater plot and adds ellipses for each group
    #pval=0.95;plotlegend=FALSE;plotpoints=TRUE;plotmeans=TRUE;MinSampleNumToEllipse=3;num=30;xlab="X";ylab="Y";main=paste(pval*100,"% Ellipse",sep="");gorder=seq(1,length(x));isometry=TRUE
    ellipseplot <- cellipse <- function(x,y,grp=as.factor(rep("g1",length(x))),
                                        pval=0.95,lwd=2,trans=0.5,
                                        plotlegend=FALSE,plotpoints=TRUE,plotmeans=TRUE,
                                        MinSampleNumToEllipse=3,num=30,
                                        xlab="X",ylab="Y",main=paste(pval*100,"% Ellipse",sep=""),
                                        gorder=seq(1,nlevels(grp)),isometry=FALSE)
    {
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      dset <- cbind(grp,x,y)
      ellipserange <- array(,list(2,2,gnum))
      dimnames(ellipserange)<-list(c("Min","Max"),c("X","Y"),gnames)
      for (k in 1:gnum)
      {
        dset1 <- dset[grp==gnames[k],]
        if(is.vector(dset1)==TRUE) {ellipserange[,,k] <- cbind(range(dset1[2]),range(dset1[3]))} else{
          if(nrow(dset1)<MinSampleNumToEllipse) {ellipserange[,,k] <- cbind(range(dset1[,2]),range(dset1[,3]))} else{
            ellipserange[,,k] <- erange(dset1[,2],dset1[,3],1,pval,num)
          }
        }
      }
      xrange <- range(x,ellipserange[,1,]);yrange <- range(y,ellipserange[,2,])
      if(isometry) {
        matplot(x,y,xlim=xrange,ylim=yrange,type="n",pch=1,add=FALSE,col=pointcolor[1],cex=1,xlab=xlab,ylab=ylab,main=main,asp=1)
      }else{
        matplot(x,y,xlim=xrange,ylim=yrange,type="n",pch=1,add=FALSE,col=pointcolor[1],cex=1,xlab=xlab,ylab=ylab,main=main)
      }
      addellipseg(x,y,grp,gorder=gorder,pval=pval,colorfill=TRUE,lwd=lwd,trans=trans)
      ellipsepoints <- array(,list((num+1)*4,2,gnum))
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){
          x1 <- dset1[2]
          y1 <- dset1[3]
        }  else{
          x1 <- dset1[,2]
          y1 <- dset1[,3]
          if(nrow(dset1)>=MinSampleNumToEllipse) ellipsepoints[,,i] <- addellipseone(x1,y1,i,pval,num)
        }
        if(plotpoints==TRUE) matplot(x1,y1,xlim=xrange,ylim=yrange,type="p",pch=i,add=TRUE,col=pointcolor[i],cex=1)
        if(plotmeans==TRUE) text(mean(x1),mean(y1),gnames[gorder[i]],adj=c(0.5,0.5),col=pointcolor[i],cex=1.5)
      }
      if(plotlegend==TRUE) legend(x=min(xrange),y=max(yrange),legend=gnames[gorder],pch=1:gnum,col=pointcolor[1:gnum])
      try(write.table(ellipsepoints,"clipboard",sep="\t"),TRUE)
      return(invisible(ellipsepoints))
    }
    
    ###
    ### Regression Plots
    ###
    
    # Regression statistics
    regstat <- function(x,y,method=c("SMA","MA","OLS"),pval=0.95,slptested=1,sfig=4,orgn=F)
    {
      method=match.arg(method)
      coeftable <- matrix(,13,1)
      rnam <- c("n","Slope","  U.conf","  L.conf","Intercept","  conf.U","  conf.L","p-value","r","Slope tested","  Slope p","  r residual", "Method")
      try(stest <- slope.test(y, x, test.value = slptested, method=method, alpha=1-pval,intercept=!orgn))
      try(if(sd(x)*sd(y)==0){regcoef <- matrix(,2,3)}else{regcoef <- line.cis(y,x, method=method,intercept=!orgn)})
      try(coeftable[1,1] <- nrow(cbind(x,y)))
      try(coeftable[2,1] <- signif(stest$b,sfig))
      try(coeftable[3,1] <- signif(stest$ci[1,2],sfig))
      try(coeftable[4,1] <- signif(stest$ci[1,1],sfig))
      try(coeftable[5,1] <- signif(regcoef[1,1],sfig))
      try(coeftable[6,1] <- signif(regcoef[1,3],sfig))
      try(coeftable[7,1] <- signif(regcoef[1,2],sfig))
      try(coeftable[8,1] <- signif(pval,sfig))
      try(coeftable[9,1] <- signif(cor(x,y),sfig))
      try(coeftable[10,1] <- signif(stest$test.value,sfig))
      try(coeftable[11,1] <- signif(stest$p,sfig))
      try(coeftable[12,1] <- signif(stest$r,sfig))
      try(coeftable[13,1] <- method)
      ctbl <- as.data.frame(coeftable)
      rownames(ctbl) <- rnam
      return(ctbl)
    }
    
    # Adds a regression line to an existing plot with a specified series color
    addregone <- function(x,y,series=1,method=c("SMA","MA","OLS"),pval=0.95,num=10,
                          lwd=2,lty=1,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor,X=x)
    {
      method <- match.arg(method)
      require(smatr)
      dset <- cbind(x,y)
      if(nrow(dset)>1){
        xcoord <- data.frame(x = seq(min(X,na.rm=T),max(X,na.rm=T),by=((max(X,na.rm=T)-min(X,na.rm=T))/num)))
        reg1 <- line.cis(y,x,alpha = 1-pval,method=method,intercept=!orgn)
        slope1 <- reg1[2,1]
        intercept1 <- reg1[1,1]
        regline <- slope1*xcoord + intercept1
        matplot(xcoord$x,regline,lty=lty,type="l",ylab="predicted y",add=TRUE,col=pcolor[series],lwd=lwd)
      }
    }
    
    # Adds a regression line of SMA,MA,or OLS for each group to an existing plot
    addreg <- function(x,y,grp,method=c("SMA","MA","OLS"),pval=0.95,num=10,gorder=seq(1,length(x)),lwd=2,orgn=FALSE,pcolor=pointcolor,fcolor=fillcolor)
    {
      method <- match.arg(method)
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      dset <- cbind(grp,x,y)
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){
          x1 <- dset1[2]
          y1 <- dset1[3]
        }
        else{
          x1 <- dset1[,2]
          y1 <- dset1[,3]
          if(sd(x1)*sd(y1)!=0) addregone(x1,y1,i,method,pval,num,lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor)
        }
      }
    }
    
    
    # Main Regression Plot Funtion
    #method="SMA";pval=0.95;isometry=F;lwd=2;slptested=1;orgn=F;plotgraph=T;plotintervals=F;
    #plotlegend=F;plotpoints=T;plotmeans=T;plotlines=T;plotstatistics=F;axes=T;newplot=F;
    #MinSampleNumToInterval=3;MinSampleNumToLine=2;num=30;sfig=4;olscipi="CIPI"
    #xlab="X";ylab="Y";main=NULL;irange=erange;gorder=seq(1,length(x));xlim=range(na.omit(x));ylim=range(na.omit(y));
    #pcolor=pointcolor;fcolor=fillcolor;psymbol=pointsymbol;cex=1;lwd=2;lwd.axis=1.5
    #mtx=NULL;gorder=seq(1,nlevels(grp));colorfill=T;X=NA;trans=0.5;plotresiduals=T
    #GroupNamesToInterval=levels(grp)
    #GroupNamesToInterval="M"
    #GroupNamesToInterval=c("newA_F","newA_M")
    #grp=as.factor(rep("g1",length(x)));
    regplot <- function(x,y,grp=as.factor(rep("g1",length(x))),method=c("SMA","MA","OLS"),
                        pval=0.95,plotgraph=T,orgn=F,axes=T,isometry=F,plotintervals=T,plotlegend=T,
                        plotpoints=T,plotmeans=T,newplot=F,colorfill=T,plotresiduals=F,legend.position="topleft",
                        plotstatistics=F,plotlines=T,plotpvalue=T,MinSampleNumToInterval=3,MinSampleNumToLine=2,
                        num=30,sfig=4,slptested=1,GroupNamesToInterval="All",olscipi=c("CIPI","CI","PI"),
                        lwd=2,lwd.axis=1.5,cex=1,xlab="X",ylab="Y",main=method,
                        pcolor=pointcolor,fcolor=fillcolor,psymbol=pointsymbol,
                        mtx=NULL,irange=erange,gorder=seq(1,nlevels(grp)),X=NA,Y=NA,
                        xlim=range(c(x,X),na.rm=T),ylim=range(c(y,Y),na.rm=T),trans=0.5)
    {
      #print(xlim)
      method <- match.arg(method)
      olscipi <- match.arg(olscipi)
      #if(!is.factor(grp)) grp <- as.factor(grp)
      nas <- which(is.na(x) | is.na(y))
      if(length(nas)!=0) {
        x=x[-nas]
        y=y[-nas]
        grp=grp[-nas]
      }
      if(!is.factor(grp)) grp <- as.factor(grp)
      grp <- factor(as.numeric(grp),labels=levels(grp)[table(grp)>0])   # removing zero-membership factors
      gnum <- nlevels(grp)
      gnames <- levels(grp)
      if(GroupNamesToInterval=="All") GroupNamesToInterval <- levels(grp)
      GroupsToInterval <- NULL
      for(i in 1:length(GroupNamesToInterval)) GroupsToInterval <- c(GroupsToInterval,which(levels(grp)==GroupNamesToInterval[i]))
      inum <- length(GroupsToInterval)
      xrange <- xlim; yrange <- ylim
      dset=cbind(grp,x,y)
      mtx <- NULL
      #Interval Range Calculation
      if(plotintervals==TRUE & method != "SMA"){
        mtx <- paste("Interval p-value = ",pval,sep="")
        if(method=="MA") {irange=erange} else if(method=="OLS") {irange=cipirange}
        intrvlrange <- array(,list(2,2,gnum))
        dimnames(intrvlrange) <- list(c("Min","Max"),c("X","Y"),gnames)
        for (m in 1:inum)
        {
          k <- GroupsToInterval[m]
          dset1 <- dset[grp==gnames[k],]
          if(is.vector(dset1)==TRUE) {intrvlrange[,,k]<-cbind(range(dset1[2]),range(dset1[3]))} else{
            if(nrow(dset1)<MinSampleNumToInterval) {intrvlrange[,,k]<-cbind(range(dset1[,2]),range(dset1[,3]))} else{
              intrvlrange[,,k] <- irange(dset1[,2],dset1[,3],1,pval,num)                                                                            
            }
          }
        }
        #intrvlrange<-intrvlrange[,,-which(intrvlrange[1,1,]==Inf)]
        xrange <- range(x,na.omit(t(intrvlrange[,1,])),xlim);yrange <- range(y,na.omit(t(intrvlrange[,2,])),ylim)
      }
      #print(xrange)
      #Plotting
      if(plotgraph){
        if(newplot) x11()
        if(isometry) {
          plot(x,y,xlim=xrange,ylim=yrange,type="n",pch=1,col=pcolor[1],cex=cex,asp=1,lwd=lwd,axes=FALSE,main="",xlab="",ylab="",bg=fcolor[1])
          box(lwd=lwd.axis);title(xlab=xlab,ylab=ylab,main=main,line=2)
          if(axes) {axis(1,lwd=lwd.axis,padj=0);axis(2,lwd=lwd.axis,padj=0)}
        }else{
          plot(x,y,xlim=xrange,ylim=yrange,type="n",pch=1,col=pcolor[1],cex=cex,lwd=lwd,axes=FALSE,main="",xlab="",ylab="",bg=fcolor[1])
          box(lwd=lwd.axis);title(xlab=xlab,ylab=ylab,main=main,line=2)
          if(axes) {axis(1,lwd=lwd.axis,padj=0);axis(2,lwd=lwd.axis,padj=0)}
        }
        if(plotpvalue==T) if(is.character(mtx)) mtext(mtx,3)
        if(plotintervals==TRUE){
          if(method=="MA")
          {
            addellipseg(x,y,grp,gorder=gorder,pval=pval,colorfill=colorfill,lwd=lwd,pcolor=pcolor,fcolor=fcolor,min.n.to.ellipse=max(MinSampleNumToInterval,3),trans=trans,GroupsToInterval=GroupsToInterval)
          }else if(method=="OLS"){
            addcipig(x,y,grp,pval=pval,num=num,gorder=gorder,colorfill=colorfill,lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor,olscipi=olscipi,min.num.to.cipi=MinSampleNumToInterval,trans=trans,GroupsToInterval=GroupsToInterval,X=X)
          } else {
            add.ch.g(x,y,grp,gorder=gorder,colorfill=colorfill,lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor,trans=trans)
          }
        }
      }
      ctbl=rep(1,13)
      for (i in 1:gnum)
      {
        dset1 <- dset[grp==gnames[gorder[i]],]
        if(is.vector(dset1)==TRUE){
          x1 <- dset1[2]
          y1 <- dset1[3]
          ctbl <- cbind(ctbl,c(1,rep(NA,12)))
        }else if(nrow(dset1)==2){
          x1 <- dset1[,2]
          y1 <- dset1[,3]
          ctbl <- cbind(ctbl,c(2,rep(NA,12)))
        }else if(nrow(dset1)==0){
          x1=NA
          y1=NA
          ctbl <- cbind(ctbl,c(2,rep(NA,12)))
        }else{
          x1 <- dset1[,2]
          y1 <- dset1[,3]
          st <-regstat(x1,y1,method=method,slptested=slptested,orgn=orgn)
          ctbl <- cbind(ctbl,st)
        }
        if(plotpoints==TRUE) points(x1,y1,pch=psymbol[i],col=pcolor[i],cex=cex,bg=pcolor[i])
        if(plotmeans==TRUE) text(mean(x1),mean(y1),gnames[gorder[i]],adj=c(0.5,0.5),col=pcolor[i],cex=cex*1.5)
        if(plotresiduals) add.resid.one(x1,y1,series=i,linecolor=pcolor,regress=T,colorpattern="Single",method=method) 
      }
      ctbl<-ctbl[,-1];
      if(gnum==1) ctbl <- as.matrix(ctbl)
      colnames(ctbl)<-gnames;try(rownames(ctbl)<-rownames(regstat(x,y,method=method,slptested=slptested)))
      if(plotgraph){
        if(plotlines) addreg(x,y,grp,method=method,pval=pval,num=num,gorder=gorder,lwd=lwd,orgn=orgn,pcolor=pcolor,fcolor=fcolor)
        if(plotlegend) legend(legend.position,legend=gnames[gorder],pch=psymbol[1:gnum],col=pcolor[1:gnum],pt.bg=pcolor[1:gnum])
      }
      common <- data.frame(rep(NA,13)); try(common <- slope.com(dset[,3],dset[,2],grp, method = "SMA"))
      if(!is.na(common[1])) {SMA.Common<-data.frame(SMA.Common=c(nrow(dset),round(common$b,sfig),round(common$ci,sfig),NA,NA,NA,0.95,
                                                                 round(cor(dset[,2],dset[,3]),sfig),round(common$b,sfig),round(common$p,sfig),NA,"SMA"))
      ctbl <- cbind(ctbl,SMA.Common) }
      if(plotstatistics){
        x11()
        textplot(ctbl)
      }
      prange<-cbind(xrange,yrange);colnames(prange)<-c("x","y");rownames(prange)<-c("min","max")
      print(ctbl)
      invisible(list(ctbl=ctbl,common=common,prange=prange))
    }
    
    
    phylo.reg <- function(xin,yin,phylogeny){
      dat <- cbind(yin,xin)
      W <- vcv.phylo(phylogeny)
      invW<-solve(W)
      pcor <- corBrownian(phy=phylogeny)
      one<-matrix(1,length(xin),1)
      xone <- cbind(one=one,xin=xin)
      phylo <- multi2di(phylogeny, random = TRUE)
      xpic <- pic(xin,phylo)
      ypic <- pic(yin,phylo)
      bhat<-solve(t(xone)%*%invW%*%xone)%*%(t(xone)%*%invW%*%yin);
      invW.eig <- eigen(invW)
      A<-t(t(one)%*%invW%*%dat)*sum(invW)^-1
      a<-t(t(one)%*%invW%*%dat)*sum(sum(invW))^-1
      N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
      Xone <- N%*%xone #Rao (4,57); transforming the data to linear
      X <- N%*%xin #Rao (4,57); transforming the data to linear
      Y <- N%*%yin  #Dummy matrix with phylo bias removed
      LM <- lm(Y~Xone+0)
      BHAT <- solve(t(Xone)%*%Xone)%*%t(Xone)%*%Y
      SMA <- sma(ypic~xpic+0,method="SMA")
      SMA.intercept <- A[1]-coef(SMA)[2]*A[2]
      vcv <- t(xone-one%*%t(a))%*%invW%*%(xone-one%*%t(a))/(nrow(xone)-1)
      pgls <- gls(yin~xin,correlation=pcor)
      result <- list(LM=LM,BHAT=BHAT,bhat=bhat,SMA=SMA,A=A,PGLS=pgls,VCV=vcv)
      #      x11();plot(Y~X)
      #      abline(reg=LM,col=4)
      #      abline(reg=SMA,col=2)
      #      textxy(X,Y,rownames(logbalaeno))
      result
    }
    
    {
      scl <- NULL
      for(i in -5:10) scl <- c(scl,seq(10^i,9*10^i,10^i))
      log10scl <- log10(scl)
      logscl <- log(scl)
      SCL <- 10^seq(-5,10)
      log10SCL <- log10(SCL)
      logSCL <- log(SCL)
      Scl <- NULL
      for(i in -5:0) Scl <- c(Scl,seq(10^i,9*10^i,10^i))
      Scl <- c(Scl,seq(10,1000,10),seq(1100,10000,100))
      log10Scl <- log10(Scl)
    }
    
  }
}



arrow.contrib <- function(x,cex = 0.75,choices = c(1,2),ofst.adjst=10)
{
  stopifnot(class(x)=="lda" | class(x)=="prcomp")
  if(class(x)=="lda"){
    p=predict(x)$x[,choices]
    loadings <- coef(x)[,choices]
  }else{
    p <- x$x[,choices]
    loadings <- x$rotation[,choices]
  }
  dcratio.raw <- apply(p,2,range)/apply(loadings,2,range)
  dcratio <- min(dcratio.raw[dcratio.raw>0])
  axis.sizes <- apply(apply(p,2,range),2,dist)
  loadings.norm <- loadings
  for(i in 1:ncol(loadings)) loadings.norm[,i] <-
    loadings.norm[,i]/axis.sizes[i]
  hv=rep(1,nrow(loadings)) #horizontal=1 or vertical=2
  for(i in 1:nrow(loadings))
    if(abs(loadings.norm[i,1])<abs(loadings.norm[i,2])) hv[i] <- 2
  ps <- rep(2,nrow(loadings))
  sgn <- sign(loadings)
  for(i in 1:nrow(loadings)){
    if(sgn[i,hv[i]]>0 & hv[i]==1) ps[i] <- 4
    if(sgn[i,hv[i]]>0 & hv[i]==2) ps[i] <- 3
    if(sgn[i,hv[i]]<0 & hv[i]==2) ps[i] <- 1
  }
  ofst <- nchar(rownames(loadings))/ofst.adjst
  arrows(x0=0,y0=0,x1=dcratio*loadings[,1],y1=dcratio*loadings[,2],length=0.1,col=2,lwd=2)
  text(dcratio*loadings,labels=row.names(loadings),cex=cex,pos=ps,offset=ofst)
}

plot.contrib.one <- function(x,cex = 0.75,ofst.adjst=50,choice=1,axes=T,
                             hv=c("h","v"),main=NULL)
{
  stopifnot(class(x)=="lda" | class(x)=="prcomp")
  stopifnot(length(choice)==1)
  hv=match.arg(hv)
  if(class(x)=="lda"){
    CS=x$scaling
  }else{
    CS=x$rotation
  }
  cs=CS[,choice]
  nams <- names(cs)
  sgn <- sign(cs)
  offst <- max(abs(cs))/ofst.adjst
  if(hv=="h"){
    plot(cs,type="n",xlim=c(-max(abs(cs)),max(abs(cs))),ylim=c(0,length(cs)+1),
         xlab="Scaling Coef.",ylab="Variables",axes=axes,main=main)
    for(i in 1:length(cs)){
      arrows(0,i,cs[i],i,lwd=2,col=2,length=0.1)
      text(-sgn[i]*offst,i,labels=nams[i],adj=c((sgn[i]+1)/2,0),cex=cex)
    }
  }else{
    plot(cs,type="n",ylim=c(-max(abs(cs)),max(abs(cs))),xlim=c(0,length(cs)+1),
         ylab="Scaling Coef.",xlab="Variables",axes=axes,main=main)
    for(i in 1:length(cs)){
      arrows(i,0,i,cs[i],lwd=2,col=2,length=0.1)
      text(i,-sgn[i]*offst,labels=nams[i],adj=c(0.5,(sgn[i]+1)/2),cex=cex,las=3)
    }
  }
}


#All traits LDA with reduced types
#change to the predict function, lda name, dataframe name
 LD <- LDA_df4[(LDA_df4$Locomotor_type4 %in% c("SD", "FP", "HP", "CUR", "QC")),]  
   
    LD$Locomotor_type4 <- factor(LD$Locomotor_type4, levels = c("SD", "FP", "HP", "CUR", "QC"))
    
    
    LD_num <- LD[,sapply(LD,is.numeric)]
    
    LD_mat <- as.matrix(LD_num)
    
    
    
    ld44 <- lda(LD$Locomotor_type4~LD_mat)
    ld1p <- ld44pred
    
    
    #go line by line
    #x11() LDA with LD1 and LD2
    layout(matrix(seq(1,4),2,2))
    regplot(ld1p$x[,1],ld1p$x[,2],LD$Locomotor_type4,xlab="LD1",ylab="LD2",main="A. 127 taxa with 25 traits ",plotline=F,legend.position="none")
    regplot(ld1p$x[,1],ld1p$x[,2],xlab="LD1",ylab="LD2",main="C. Contribution Scores",plotline=F,plotpoint=F,plotlegend=F,plotintervals=F)
    par(xpd=NA)
    arrow.contrib(ld44,choices=c(1,2),cex=0.5)
    par(xpd=NULL)
    plot.contrib.one(ld44,choice=1,hv="h",main="B. LD1")
    plot.contrib.one(ld44,choice=2,hv="h",main="D. LD2")
    #save plot Figure S8
    dev.off()
    
    #x11() LDA with LD3 and LD4
    layout(matrix(seq(1,4),2,2))
    regplot(ld1p$x[,3],ld1p$x[,4],LD$Locomotor_type4,xlab="LD3",ylab="LD4",main="A. 127 taxa with 25 traits ",plotline=F,legend.position="none")
    regplot(ld1p$x[,3],ld1p$x[,4],xlab="LD3",ylab="LD4",main="C. Contribution Scores",plotline=F,plotpoint=F,plotlegend=F,plotintervals=F)
    par(xpd=NA)
    arrow.contrib(ld44,choices=c(3,4),cex=0.5)
    par(xpd=NULL)
    plot.contrib.one(ld44,choice=3,hv="h",main="B. LD3")
    plot.contrib.one(ld44,choice=4,hv="h",main="D. LD4")
    #save plot Figure S9
    dev.off()
    #
    
  #pFDA ------
    
    ## phylo.FDA.v0.2 RUN FIRST
    
    {
      require(nnet)
      require(mda)
      require(ape)
      require(geiger)
      require(lattice)
      
      ###----------------------------------------------------------------------
      ### Internal function from the package mda
      ###----------------------------------------------------------------------
      "contr.fda" <-
        function (p = rep(1, d[1]), contrast.default = contr.helmert(length(p)))
        {
          d <- dim(contrast.default)
          sqp <- sqrt(p/sum(p))
          x <- cbind(1, contrast.default) * outer(sqp, rep(1, d[2] +
                                                             1))
          qx <- qr(x)
          J <- qx$rank
          qr.qy(qx, diag(d[1])[, seq(2, J)])/outer(sqp, rep(1, J -
                                                              1))
        }
      
      ###----------------------------------------------------------------------
      ### Associated functions modified from the package mda
      ###----------------------------------------------------------------------
      "predict.phylo.fda" <-
        function (object, newdata, type = c("class", "variates", "posterior",
                                            "hierarchical", "distances"), prior, dimension = J - 1, ...)
        {
          dist <- function(x, mean, m = ncol(mean)) (scale(x, mean,
                                                           FALSE)^2) %*% rep(1, m)
          type <- match.arg(type)
          means <- object$means
          Jk <- dim(means)
          J <- Jk[1]
          k <- Jk[2]
          if (type == "hierarchical") {
            if (missing(dimension))
              dimension.set <- seq(k)
            else {
              dimension.set <- dimension[dimension <= k]
              if (!length(dimension.set))
                dimension.set <- k
              dimension <- max(dimension.set)
            }
          }
          
          else dimension <- min(max(dimension), k)
          if (missing(newdata))
            y <- predict(object$fit)
          else {
            if (inherits(newdata, "data.frame") || is.list(newdata)) {
              Terms <- delete.response(terms(object))
              attr(Terms, "intercept") <- 0
              newdata <- model.matrix(Terms, newdata)
            }
            y <- predict(object$fit, newdata)
          }
          y <- y %*% object$theta[, seq(dimension), drop = FALSE]
          lambda <- object$values
          alpha <- sqrt(lambda[seq(dimension)])
          sqima <- sqrt(1 - lambda[seq(dimension)])
          newdata <- scale(y, FALSE, sqima * alpha)
          if (missing(prior))
            prior <- object$prior
          else {
            if (any(prior < 0) | round(sum(prior), 5) != 1)
              stop("innappropriate prior")
          }
          means <- means[, seq(dimension), drop = FALSE]
          switch(type, variates = return(newdata), class = {
            n <- nrow(newdata)
            prior <- 2 * log(prior)
            mindist <- dist(newdata, means[1, ], dimension) - prior[1]
            pclass <- rep(1, n)
            for (i in seq(2, J)) {
              ndist <- dist(newdata, means[i, ], dimension) - prior[i]
              l <- ndist < mindist
              pclass[l] <- i
              mindist[l] <- ndist[l]
            }
            ## 2001-10-27: Need to provide levels or else if we get an error
            ## if the predicted classes do no contain all possible classes.
            ## Reported by Greg Jefferis <jefferis@stanford.edu>, fix by
            ## Bj/orn-Helge Mevik <bjorn-helge.mevik@matforsk.no>.
            return(factor(pclass, levels = seq(J),
                          labels = dimnames(means)[[1]]))
          }, posterior = {
            pclass <- matrix(0, nrow(newdata), J)
            for (i in seq(J)) pclass[, i] <- exp(-0.5 * dist(newdata, means[i,
            ], dimension)) * prior[i]
            dimnames(pclass) <- list(dimnames(newdata)[[1]], dimnames(means)[[1]])
            return(pclass/drop(pclass %*% rep(1, J)))
          }, hierarchical = {
            prior <- 2 * log(prior)
            Pclass <- vector("list", length(dimension.set))
            names(Pclass) <- paste("D", dimension.set, sep = "")
            for (ad in seq(along = dimension.set)) {
              d <- dimension.set[ad]
              dd <- seq(d)
              
              mindist <- dist(newdata[, dd, drop = FALSE], means[1, dd, drop = FALSE],
                              d) - prior[1]
              pclass <- rep(1, nrow(newdata))
              for (i in seq(2, J)) {
                ndist <- dist(newdata[, dd, drop = FALSE], means[i, dd,
                                                                 drop = FALSE], d) - prior[i]
                l <- ndist < mindist
                pclass[l] <- i
                mindist[l] <- ndist[l]
              }
              levels(pclass) <- dimnames(means)[[1]]
              Pclass[[ad]] <- pclass
            }
            rownames <- dimnames(newdata)[[1]]
            if (is.null(rownames))
              rownames <- paste(seq(nrow(newdata)))
            return(structure(Pclass, class = "data.frame", row.names = rownames,
                             dimensions = dimension.set))
          }, distances = {
            dclass <- matrix(0, nrow(newdata), J)
            for (i in seq(J)) dclass[, i] <- dist(newdata, means[i, ],
                                                  dimension)
            dimnames(dclass) <- list(dimnames(newdata)[[1]], dimnames(means)[[1]])
            return(dclass)
          })
        }
      "predict.polyreg.modified" <-
        function (object, newdata, ...)
        {
          if (missing(newdata)) {
            z <- fitted(object)
            if (is.null(z))
              stop("need to supply newdata")
            else return(z)
          }
          degree <- object$degree
          monomial <- object$monomial
          newdata %*% object$coef
        }
      "polyreg.modified" <-
        function (x, y, w, degree = 1, monomial = FALSE, ...)
        {
          #x <- polybasis(x, degree, monomial)
          y <- as.matrix(y) # just making sure ...
          if (iswt <- !missing(w)) {
            if (any(w <= 0))
              stop("only positive weights")
            w <- sqrt(w)
            y <- y * w
            x <- x * w
          }
          qrx <- qr(x)
          coef <- as.matrix(qr.coef(qrx, y))
          
          fitted <- qr.fitted(qrx, y)
          if ((df <- qrx$rank) < ncol(x))
            coef[qrx$pivot, ] <- coef
          if (iswt)
            fitted <- fitted/w
          structure(list(fitted.values = fitted, coefficients = coef,
                         degree = degree, monomial = monomial, df = df), class = "polyreg.modified")
        }
      "print.phylo.fda" <-
        function (x, ...)
        {
          if (!is.null(cl <- x$call)) {
            cat("Call:\n")
            dput(cl)
          }
          cat("\nDimension:", format(x$dimension), "\n")
          cat("\nPercent Between-Group Variance Explained:\n")
          print(round(x$percent, 2))
          error <- x$confusion
          df <- x$fit
          if (!is.null(df))
            df <- df$df
          if (!is.null(df)) {
            cat("\nDegrees of Freedom (per dimension):", format(sum(df)),
                "\n")
          }
          if (!is.null(error)) {
            n <- as.integer(sum(error))
            error <- format(round(attr(error, "error"), 5))
            cat("\nTraining Misclassification Error:", error, "( N =",
                n, ")\n")
          }
          invisible(x)
        }
      "plot.phylo.fda" <- function(pfdamodel,gfactor=pfdamodel$g,prdfactor=pfdamodel$prd)
      {
        pfdavar <- predict(pfdamodel, type="variate")
        lim1x <- c(min(pfdavar[,1]),max(pfdavar[,1]))
        lim1y <- c(min(pfdavar[,2]),max(pfdavar[,2]))
        m1 <- 4;m2 <- 1
        oldpar<-par(no.readonly=FALSE);on.exit(par(oldpar));x11(height=8,width=14);par(mfrow=c(1,2),mar=c(m1,m1,m1,m2),oma=c(m2,m2,m2,m2));
        matplot(pfdavar[gfactor==levels(gfactor)[1],1], pfdavar[gfactor==levels(gfactor)[1],2], xlab="pFDA1",ylab="pFDA2", xlim=lim1x, ylim=lim1y, pch=1, col=1, main="True Classes",sub=paste("lambda = ",pfdamodel$val," intrcpt=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior,sep=""))
        for (i in 2:nlevels(gfactor)) matplot(pfdavar[gfactor==levels(gfactor)[i],1], pfdavar[gfactor==levels(gfactor)[i],2], add=TRUE, pch=i, col=i)
        legend(min(lim1x),max(lim1y),levels(gfactor), pch=1:nlevels(gfactor), col=1:nlevels(gfactor))
        
        legend(min(lim1x),min(lim1y)+(max(lim1y)-min(lim1y))*0.1,paste("lambda = ",pfdamodel$val," intrcpt=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior," ",sep=""))
        addEllipseGrp(pfdavar[,1],pfdavar[,2],gfactor, pval=0.95, num=30)
        matplot(pfdavar[prdfactor==levels(prdfactor)[1],1], pfdavar[prdfactor==levels(prdfactor)[1],2], xlab="pFDA1",ylab="pFDA2", xlim=lim1x, ylim=lim1y, pch=1, col=1, main="Predicted Classes",sub=paste("lambda = ",pfdamodel$val," intercept=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior,sep=""))
        for (i in 2:nlevels(prdfactor)) matplot(pfdavar[prdfactor==levels(prdfactor)[i],1], pfdavar[prdfactor==levels(prdfactor)[i],2], add=TRUE, pch=i, col=i)
        legend(min(lim1x),max(lim1y),levels(prdfactor), pch=1:nlevels(prdfactor), col=1:nlevels(prdfactor))
        legend(min(lim1x),min(lim1y)+(max(lim1y)-min(lim1y))*0.1,paste(levels(prdfactor),"=",pfdamodel$prior," ",sep=""))
        legend(max(lim1x)-(max(lim1x)-min(lim1x))*0.2,max(lim1y),signif(attr(pfdamodel$confusion,"error"),4))
        invisible()
      }
      
      ###----------------------------------------------------------------------
      ### Main pFDA function with training data only
      ###----------------------------------------------------------------------
      "phylo.fda" <-function (data,grp,tretre,val=1,treetrans=rescale,
                              dimension = J - 1, eps = .Machine$double.eps,
                              keep.fitted = (n * dimension < 1000), method=polyreg.modified,intercept=TRUE,eqprior=FALSE,priin=1)
      {
        this.call <- match.call()
        if(intercept) data <- cbind(Intercept=rep(1,nrow(data)),data)
        data <- as.matrix(data)
        tretre <- treetrans(tretre,"lambda", val)
        g <- as.factor(grp)
        ng <- nlevels(g)
        W <- vcv.phylo(tretre)
        invW<-solve(W)
        invW.eig <- eigen(invW)
        N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
        divnum <-det(N)^(1/nrow(N))
        N <- N/divnum
        DATA <- N%*%data #Rao (4,57); transforming the data to linear
        n <- nrow(DATA)
        y <- matrix(0,nrow(data),ng)
        for (i in 1:nrow(data)){y[i,g[i]] <- 1}
        Y <- N%*%y #Dummy matrix with phylo bias removed
        x <- DATA
        fg <- factor(g)
        prior <- colSums(Y)/sum(colSums(Y))
        if(eqprior) prior <- c(rep(1/ng,ng))
        if(priin[1] != 1) prior<-priin
        cnames <- levels(fg)
        g <- as.numeric(fg)
        J <- length(cnames)
        weights <- rep(1, n)
        dp <- tapply(weights, g, sum)/n
        theta <- contr.helmert(J)
        theta <- contr.fda(dp, theta)
        
        Theta <- Y%*%theta #fda p.7, above eq2
        fit <- method(x, Theta, weights)
        rss <- t(Theta-fit$fitted) %*% (Theta-fit$fitted)
        ssm <- t(Theta) %*% fitted(fit)/n
        ed <- svd(ssm, nu = 0)
        thetan <- ed$v
        lambda <- ed$d
        lambda[lambda > 1 - eps] <- 1 - eps
        discr.eigen <- lambda/(1 - lambda)
        pe <- (100 * cumsum(discr.eigen))/sum(discr.eigen)
        dimension <- min(dimension, sum(lambda > eps))
        if (dimension == 0) {
          warning("degenerate problem; no discrimination")
          return(structure(list(dimension = 0, fit = fit, call = this.call),
                           class = "phylo.fda"))
        }
        thetan <- thetan[, seq(dimension), drop = FALSE]
        pe <- pe[seq(dimension)]
        alpha <- sqrt(lambda[seq(dimension)])
        sqima <- sqrt(1 - lambda[seq(dimension)])
        vnames <- paste("v", seq(dimension), sep = "")
        means <- scale(theta %*% thetan, FALSE, sqima/alpha)
        dimnames(means) <- list(cnames, vnames)
        names(lambda) <- c(vnames, rep("", length(lambda) - dimension))
        names(pe) <- vnames
        frml <- "grp~"
        nc <- ncol(data)
        varnam <- colnames(data)
        for(i in 1:(nc-1)) frml <- paste(frml,varnam[i],"+", sep="")
        frml <- paste(frml,varnam[nc], sep="")
        frml <- as.formula(frml)
        dset <- as.data.frame(cbind(grp,DATA))
        Terms <- as.call(fda(formula = frml, data = dset, weights = weights))
        obj <- structure(list(percent.explained = pe, values = lambda,
                              means = means, theta.mod = thetan, dimension = dimension,
                              prior = prior, fit = fit, call = this.call, terms = Terms),
                         class = "phylo.fda")
        obj$confusion <- confusion(predict(obj), fg)
        obj$prd <- predict(obj)
        obj$g <- as.factor(grp)
        obj$val <- val
        obj$rss <- sum(diag(rss))
        obj$intercept <- intercept
        obj$eqprior <- eqprior
        if (!keep.fitted)
          obj$fit$fitted.values <- NULL
        obj
      }
      
      ###----------------------------------------------------------------------
      ### Main pFDA function with training and test data
      ###----------------------------------------------------------------------
      
      "phylo.fda.pred" <-function (dataA,grpA,taxtaxA,tretreA,testlistn,val=1,treetrans=rescale,
                                   method=polyreg.modified,sbcls=floor(table(grp)/4),
                                   dimension = J - 1, eps = .Machine$double.eps, keep.fitted = (n * dimension < 1000),intercept=TRUE,eqprior=FALSE,priin=1)
      {
        ## Preparing data
        this.call <- match.call()
        if(intercept) dataA <- cbind(Intercept=rep(1,nrow(dataA)),dataA)
        dataA <- as.data.frame(dataA)
        nA <- nrow(dataA)
        testlist <- taxtaxA[testlistn]
        traininglist <- taxtaxA[-testlistn]
        rownames(dataA) <- taxtaxA
        tretre <- drop.tip(tretreA,testlistn)
        grp <- grpA[-testlistn]
        grp <- grp[grp %in% names(table(grp))[table(grp) > 0], drop=TRUE]
        g <- as.factor(grp)
        ng <- nlevels(g)
        grpA <- as.factor(grpA)
        ntest <- length(testlist)
        dataA <- as.matrix(dataA)
        tretreA <- treetrans(tretreA,"lambda", val)
        W <- vcv.phylo(tretreA)
        invW<-solve(W)
        invW.eig <- eigen(invW)
        N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
        divnum <-det(N)^(1/nrow(N))
        N <- N/divnum
        invN <- solve(N)
        y <- matrix(0,nA,nlevels(grpA))
        for (i in 1:nA){y[i,grpA[i]] <- 1}
        Y <- N%*%y #Dummy matrix with phylo bias removed
        Y <- Y[-testlistn,1:ng]
        DATAA <- N%*%as.matrix(dataA) #Rao (4,57); transforming the data to linear
        DATA <- DATAA[-testlistn,]
        DATAtest <- DATAA[testlistn,]
        n<-nrow(DATA)
        m<-nrow(DATAtest)
        x <- DATA
        fg <- factor(g)
        prior <- colSums(Y)/sum(colSums(Y))
        if(eqprior) prior <- c(rep(1/ng,ng))
        if(priin[1] != 1) prior<-priin
        cnames <- levels(fg)
        g <- as.numeric(fg)
        J <- length(cnames)
        weights <- rep(1, n)
        dp <- tapply(weights, g, sum)/n
        theta <- contr.helmert(J)
        theta <- contr.fda(dp, theta)
        Theta <- Y%*%theta #fda p.7, above eq2
        
        fit <- method(x, Theta, weights)
        rss <- t(Theta-fit$fitted) %*% (Theta-fit$fitted)
        ssm <- t(Theta) %*% fitted(fit)/n
        ed <- svd(ssm, nu = 0)
        thetan <- ed$v
        lambda <- ed$d
        lambda[lambda > 1 - eps] <- 1 - eps
        discr.eigen <- lambda/(1 - lambda)
        pe <- (100 * cumsum(discr.eigen))/sum(discr.eigen)
        dimension <- min(dimension, sum(lambda > eps))
        if (dimension == 0) {
          warning("degenerate problem; no discrimination")
          return(structure(list(dimension = 0, fit = fit, call = this.call),
                           class = "fda"))
        }
        thetan <- thetan[, seq(dimension), drop = FALSE]
        pe <- pe[seq(dimension)]
        alpha <- sqrt(lambda[seq(dimension)])
        sqima <- sqrt(1 - lambda[seq(dimension)])
        vnames <- paste("v", seq(dimension), sep = "")
        means <- scale(theta %*% thetan, FALSE, sqima/alpha)
        dimnames(means) <- list(cnames, vnames)
        names(lambda) <- c(vnames, rep("", length(lambda) - dimension))
        names(pe) <- vnames
        frml <- "grp~"
        nc <- ncol(dataA)
        varnam <- colnames(dataA)
        for(i in 1:(nc-1)) frml <- paste(frml,varnam[i],"+", sep="")
        frml <- paste(frml,varnam[nc], sep="")
        frml <- as.formula(frml)
        dset <- as.data.frame(cbind(grp,DATA))
        Terms <- as.call(fda(formula = frml, data = dset, weights = weights))
        obj <- structure(list(percent.explained = pe, values = lambda,
                              means = means, theta.mod = thetan, dimension = dimension,
                              prior = prior, fit = fit, call = this.call, terms = Terms),
                         class = "phylo.fda")
        obj$confusion <- confusion(predict(obj), fg)
        obj$prd <- predict(obj)
        obj$x<-x
        obj$g <- as.factor(grp)
        obj$val <- val
        obj$rss <- sum(diag(rss))
        obj$intercept <- intercept
        obj$eqprior <- eqprior
        obj$DATAtest <- DATAtest
        obj$DATA <- DATA
        tpred <- predict(obj,DATAtest)
        tpredn <- as.numeric(tpred)
        tpred <- as.matrix(tpred)
        rownames(tpred) <- testlist
        obj$testprediction <- tpred
        obj$testprediction_numeral <- tpredn
        if (!keep.fitted)
          
          obj$fit$fitted.values <- NULL
        obj
      }
      
      
      ###----------------------------------------------------------------------
      ### Function for optimal lambda value search
      ###----------------------------------------------------------------------
      
      "phylo.RSS"<-function (datain,grp,tretre,val=1,treetrans=rescale)
      {
        datainO <- as.matrix(datain)
        datainI <- cbind(Intercept=rep(1,nrow(datainO)),datainO)
        tretre <- treetrans(tretre,"lambda", val)
        n <- nrow(datain)
        g <- as.factor(grp)
        ng <- nlevels(g)
        W <- vcv.phylo(tretre)
        invW<-solve(W)
        y <- matrix(0,n,ng) #Dummy matrix without phylo bias
        for (i in 1:n){y[i,g[i]] <- 1}
        invW.eig <- eigen(invW)
        N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
        Y <- N %*% y # Pretending that there is no phylogenetic bias in y; otherwise Y <- N%*%y
        DATAI <- N%*%datainI
        bhatI <- solve(t(datainI)%*%invW%*%datainI)%*%t(datainI)%*%invW%*%y #Rohlf (9) -- data biased still Rao (4,64)
        yhatI <- datainI%*%bhatI #Rohlf (11)
        RSSyI <- t(y-yhatI) %*% invW %*% (y-yhatI) #Martins and Hansen 1997 (9)
        l0I<- lm(Y~DATAI-1)
        list(RSS=sum(diag(RSSyI))) 
      }
      
      ##dataA=XA;grpA=gA;taxtaxA=taxaA;tretreA=treA;testlistn=testtaxan;val=0;treetrans=lambdaTree
      
      "phylo.RSS.pred" <-function (dataA,grpA,taxtaxA,tretreA,testlistn,val=1,treetrans=rescale)
      {
        dataA <- as.data.frame(dataA)
        nA <- nrow(dataA)
        testlist <- taxtaxA[testlistn]
        traininglist <- taxtaxA[-testlistn]
        rownames(dataA) <- taxtaxA
        tretre <- drop.tip(tretreA,testlistn)
        grp <- grpA[-testlistn]
        grp <- grp[grp %in% names(table(grp))[table(grp) > 0], drop=TRUE]
        g <- as.factor(grp)
        ng <- nlevels(g)
        grpA <- as.factor(grpA)
        icptA <- rep(1,nA)
        dataA <- cbind(icptA,dataA)
        ntest <- length(testlist)
        tretreA <- treetrans(tretreA,"lambda", val)
        
        W <- vcv.phylo(tretreA)
        invW<-solve(W)
        invW.eig <- eigen(invW)
        N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
        invN <- solve(N)
        y <- matrix(0,nA,nlevels(grpA))
        for (i in 1:nA){y[i,grpA[i]] <- 1}
        Y <- N%*%y #Dummy matrix with phylo bias removed
        Y <- Y[-testlistn,1:ng]
        DATAA <- N%*%as.matrix(dataA) #Rao (4,57); transforming the data to linear
        DATA <- DATAA[-testlistn,]
        BHAT <- solve(t(DATA)%*%DATA)%*%t(DATA)%*%Y
        YHAT <- DATA%*%BHAT
        l0<- lm(Y~DATA-1)
        RSSY <- t(Y-YHAT) %*% (Y-YHAT)
        list(RSS=sum(diag(RSSY))) 
      }
      
      ##measurements=X;grps=g;mytree=tre;idc=filename_stem
      
      "optLambda" <- function(measurements,grps,mytree,idc="default",sstep=0.01,srange=c(0,1),fldr="./")
      {
        lambdalist <- seq(min(srange),max(srange),sstep)
        segnum <- length(lambdalist)
        rslt<-matrix(,segnum,2)
        colnames(rslt) <- c("Lambda","RSS") 
        
        for(i in 1:segnum){
          lambdaval <- lambdalist[i]
          rss <- phylo.RSS(X,grps,mytree,val=lambdaval)
          rslt[i,] <- c(lambdaval,rss$RSS) 
        }
        
        optlambda <- matrix(,1,1);colnames(optlambda)<- "RSS"
        optlambda[1,1]<-max(rslt[which(rslt[,2]==min(rslt[,2])),1])
        
        matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
        abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
        
        pdf(height=11,width=6,file=paste(fldr,idc,".optLambda.pdf",sep=''));layout(matrix(c(1,2),2,1))
        matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
        abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
        dev.off()
        
        list(optlambda=optlambda,rslt=rslt)
      }
      
      "optLambda.pred" <- function(measurementsA,grpsA,taxaA,mytreeA,testn,idc="default",sstep=0.01,srange=c(0,1),fldr="./")
        
      {
        lambdalist <- seq(min(srange),max(srange),sstep)
        segnum <- length(lambdalist)
        rslt<-matrix(,segnum,2)
        colnames(rslt) <- c("Lambda","RSS")
        
        for(i in 1:segnum){
          lambdaval <- lambdalist[i]
          rss <- phylo.RSS.pred(measurementsA,grpsA,taxaA,mytreeA,testn,val=lambdaval)
          rslt[i,] <- c(lambdaval,rss$RSS,rss$AICY) #replaced lLy with AICY
        }
        
        optlambda <- matrix(,1,1);colnames(optlambda)<- "RSS"
        optlambda[1,1]<-max(rslt[which(rslt[,2]==min(rslt[,2])),1])
        matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
        abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
        
        pdf(height=11,width=6,file=paste(fldr,idc,".optLambda.pred.pdf",sep=''));layout(matrix(c(1,2),2,1))
        matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
        abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
        dev.off()
        
        list(optlambda=optlambda,rslt=rslt)
      }
      
    }
    
    # Phylogenetic flexible discriminant analysis
    
    # Lars Schmitz, 2014
    
    # 1 Preliminaries
    # 2 Analysis
    
    #1 Preliminaries -----
    #pFDA of All Traits LDA and Head & Forelimb Traits LDA
    

    
    # Loading libraries
    
    library(ape)
    library(class)
    #installed.packages("geiger")
    library(geiger)
    #installed.packages("lattice")
    library(lattice)
    #installed.packages("mda")
    library(mda)
    library(nnet)
    #source("phylo.fda.v0.2.R")
    
    # Reading in the tree.
    
    treA <- read.tree("Forebody_traits_Species.nwk")
    if(!is.binary(treA)) treA <- multi2di(treA, random = TRUE) # Randomly resolving polytomies if there are any
    
    treB <- read.tree("All_traits_Species.nwk")
    if(!is.binary(treB)) treB <- multi2di(treB, random = TRUE)
    
    
    # Reading in the data and assigning rownames.
    
    ddA <- read.csv("Forebody_traits.csv")
    rownames(ddA) <- ddA$Species
    ddA <- ddA[treA$tip.label,]
    ddA #no NAs means everything is good
    
    ddB <- read.csv("All_traits.csv")
    rownames(ddB) <- ddB$Species
    ddB <- ddB[treB$tip.label,]
    ddB #no NAs means everything is good
    
    # Ordering data to match tip labels.
    # Note that for this tutorial data and phylogeny match.
    # Normally that's not the case. You can use 'geiger's treedata() function for that, as explained earlier.
    
    #extra step:manually check that tip labels and species name match using Excel duplicate 
    #write.csv(treB$tip.label, "All_treetips.csv", row.names = FALSE)
    #write.csv(rownames(ddB), "All_specieslist.csv", row.names = FALSE)
    
    ddA <- ddA[match(treA$tip.label,rownames(ddA)),]
    ddB <- ddB[match(treB$tip.label,rownames(ddB)),]
   
    
    #2 Analysis ----
    
    # Head and Forelimb Traits pFDA
    # Defining groups and taxa.
    gA <- ddA$Locomotor_type4 # contains data on ecology/behavior
    taxaA <- ddA$Species # species names
    
    # Tree and data preparation: all taxa (test and training, e.g., fossil and living) or training only (living).
    XA <- ddA[,3:21] # we are selecting traits from the dataset
    XA <- log10(XA) #log10 transformation
    testtaxa <- rownames(ddA[gA=="unknown",]) # specifying taxa with unknown group, e.g. fossils
    testtaxan <- row(ddA)[gA=="unknown",1]
    trainingtaxa <- rownames(ddA[-testtaxan,]) # creating a dataframe that only contains taxa with known group affiliation
    X <- XA
    dd <- ddA
    g <- gA
    tre <- treA

    
    # Identifying optimal lambda: where is the strongest correlation between form and ecology among living taxa?
    
    filename_stem <- "Locomotor_type4" # A plot will appear in separate window; a pdf is saved in your working directory.
    ol1 <- optLambda(X,g,tre,idc=filename_stem)
    ol1$optlambda # displaying the optimal lambda value
    
    #LDA with Forebody Traits with modified names to match tree 
    {
      LDA_df5.5 <- ddA
      
      LDA_df5.5 <- LDA_df5.5[-c(1)]
      
      Habit <- LDA_df5.5[c(1)]
      LDA_df5.5 <- LDA_df5.5[-c(1)]
      LDA_df5.5 <- log10(LDA_df5.5)
      LDA_df5.5 <- cbind(Habit, LDA_df5.5)
      
      
      
      LDA_df5.5$Locomotor_type4 <- factor(LDA_df5.5$Locomotor_type4, levels = c("SD", "CT", "H&P", "HR", "LD", "AQ", "SW", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))

      
      LDA_df5.5_num <- LDA_df5.5[,sapply(LDA_df5.5,is.numeric)]
      
      LDA_df5.5_mat <- as.matrix(LDA_df5.5_num)
      ld1 <- lda(LDA_df5.5$Locomotor_type4~LDA_df5.5_mat)
      
      ld1pred <- predict(ld1)
      print(ld1$prior)
      
      ld1table <- table(ld1pred$class,LDA_df5.5$Locomotor_type4)
      ld1table
      
      #overall classification rate of LDA with Tree Taxa
      sum(diag(ld1table))/sum(ld1table) * 100
      #85.63218   
      
    }
    
    # Run FDA
    opt1 <- ol1$optlambda[1]
    pfda <- phylo.fda(XA,gA,treA,val=opt1)
    #predict(pfda)
    pfda$confusion
    sum(diag(pfda$confusion))/sum(pfda$confusion) * 100
    # FDA: 86.78161
    # LDA: 85.63218  
    summary(pfda)
    pfda$prior
    
    #ran pFDA and found no phylogenetic bias, minimal change in classification rates, no phylogenetic bias in the relationship between measurements and locomotor type
    
    # All Traits pFDA
    # Defining groups and taxa.
    gB <- ddB$Locomotor_type4 # contains data on ecology/behavior
    taxaB <- ddB$Species # species names
    
    # Tree and data preparation: all taxa (test and training, e.g., fossil and living) or training only (living).
    XB <- ddB[,3:27] # we are selecting traits from the dataset
    XB <- log10(XB) #log10 transformation
    testtaxa <- rownames(ddB[gB=="unknown",]) # specifying taxa with unknown group, e.g. fossils
    testtaxan <- row(ddB)[gB=="unknown",1]
    trainingtaxa <- rownames(ddB[-testtaxan,]) # creating a dataframe that only contains taxa with known group affiliation
    X <- XB
    dd <- ddB
    g <- gB
    tre <- treB
    
    # Identifying optimal lambda: where is the strongest correlation between form and ecology among living taxa?
    
    filename_stem <- "Locomotor_type4" # A plot will appear in separate window; a pdf is saved in your working directory.
    ol1 <- optLambda(X,g,tre,idc=filename_stem)
    ol1$optlambda # displaying the optimal lambda value
    
    #LDA with All Traits with modified names to match tree 
    {
      LDA_df4.5 <- ddB
      
      LDA_df4.5 <- LDA_df4.5[-c(1)]
      
      Habit <- LDA_df4.5[c(1)]
      LDA_df4.5 <- LDA_df4.5[-c(1)]
      LDA_df4.5 <- log10(LDA_df4.5)
      LDA_df4.5 <- cbind(Habit, LDA_df4.5)
      
      
      
      LDA_df4.5$Locomotor_type4 <- factor(LDA_df4.5$Locomotor_type4, levels = c("SD", "CT", "H&P", "HR", "LD", "FP", "UF", "HP", "US", "CUR", "RIC", "QC", "QS", "BRA"))
      
      
      LDA_df4.5_num <- LDA_df4.5[,sapply(LDA_df4.5,is.numeric)]
      
      LDA_df4.5_mat <- as.matrix(LDA_df4.5_num)
      ld1 <- lda(LDA_df4.5$Locomotor_type4~LDA_df4.5_mat)
      
      ld1pred <- predict(ld1)
      print(ld1$prior)
      
      ld1table <- table(ld1pred$class,LDA_df4.5$Locomotor_type4)
      ld1table
      
      #overall classification rate of LDA with Tree Taxa
      sum(diag(ld1table))/sum(ld1table) * 100
      #87.01299
      
    }
    
    # Run FDA
    opt1 <- ol1$optlambda[1]
    pfda <- phylo.fda(XB,gB,treB,val=opt1)
    #predict(pfda)
    pfda$confusion
    sum(diag(pfda$confusion))/sum(pfda$confusion) * 100
    # FDA: 87.01299
    # LDA: 87.01299
    summary(pfda)
    pfda$prior
    
    #same classification rates, minimal phylogenetic bias 
    
    # Key references:
    # Hastie, T., Tibshirani, R., Buja, A. (1994) Flexible disriminant analysis by optimal scoring.
    #   J Americ Stat Assoc, 89, 1255-1270.
    # Motani, R., Schmitz, L. (2011) Phylogenetic versus functional signals in the evolution of form-function relationships in terrestrial vision. 
    #   Evolution, 65, 2245-2257.
    # Schmitz, L., Motani, R. (2011) Nocturnality in dinosaurs inferred from scleral ring and orbit morphology. 
    #   Science, 332, 705-708.
    
  #Phylogenetic ANOVA ----
    
    #Forebody Tree
    ForebodyTree <- read.tree("Forebody_traits_Species.nwk")
    if(!is.binary(ForebodyTree)) ForebodyTree <- multi2di(ForebodyTree, random = TRUE) # Randomly resolving polytomies if there are any
    
    Forebody_db2 <- read.csv("Forebody_traits.csv") 
    
    #create columns with MA and log MA 
    {
      
      Forebody_db2$Skull_Extension_MA <- Forebody_db2$OH / Forebody_db2$SL
      Forebody_db2$Shoulder_Flexion_MA <- Forebody_db2$Shoulder_Flexion / Forebody_db2$HL
      Forebody_db2$Shoulder_Extension_MA <- Forebody_db2$Shoulder_Extension / Forebody_db2$HL
      Forebody_db2$Shoulder_Adduction_MA <- Forebody_db2$Shoulder_Adduction / Forebody_db2$HL
      Forebody_db2$Shoulder_Abduction_MA <- Forebody_db2$Shoulder_Abduction / Forebody_db2$HL
      Forebody_db2$Shoulder_Medial_Rotation_MA <- Forebody_db2$Shoulder_Medial_Rotation / Forebody_db2$UL
      Forebody_db2$Shoulder_Lateral_Rotation_MA <- Forebody_db2$Shoulder_Lateral_Rotation / Forebody_db2$UL
      Forebody_db2$Elbow_Extension_MA <- Forebody_db2$OL / Forebody_db2$UL
      Forebody_db2$Elbow_Flexion_MA <- Forebody_db2$Elbow_Flexors / Forebody_db2$UL
      Forebody_db2$Wrist_Extension_MA <- Forebody_db2$Wrist_Extension / Forebody_db2$ML
      Forebody_db2$Wrist_Flexion_MA <- Forebody_db2$Wrist_Flexion / Forebody_db2$ML
      
      Forebody_db2$Skull_Extension_MA_log <- log(Forebody_db2$OH / Forebody_db2$SL)
      Forebody_db2$Shoulder_Flexion_MA_log <- log(Forebody_db2$Shoulder_Flexion_MA)
      Forebody_db2$Shoulder_Extension_MA_log <- log(Forebody_db2$Shoulder_Extension_MA)
      Forebody_db2$Shoulder_Adduction_MA_log <- log(Forebody_db2$Shoulder_Adduction_MA)
      Forebody_db2$Shoulder_Abduction_MA_log <- log(Forebody_db2$Shoulder_Abduction_MA)
      Forebody_db2$Shoulder_Medial_Rotation_MA_log <- log(Forebody_db2$Shoulder_Medial_Rotation_MA)
      Forebody_db2$Shoulder_Lateral_Rotation_MA_log <- log(Forebody_db2$Shoulder_Lateral_Rotation_MA)
      Forebody_db2$Elbow_Extension_MA_log <- log(Forebody_db2$Elbow_Extension_MA)
      Forebody_db2$Elbow_Flexion_MA_log <- log(Forebody_db2$Elbow_Flexion_MA)
      Forebody_db2$Wrist_Extension_MA_log <- log(Forebody_db2$Wrist_Extension_MA)
      Forebody_db2$Wrist_Flexion_MA_log <- log(Forebody_db2$Wrist_Flexion_MA)
      
    }
    
    Forebody_db2 <- Forebody_db2[c(1:2,33:43)]
    


    #install.packages("geiger")
    library(geiger)
    library(ape)
    #reorder to match tree tip labels 
    df1_reordered <- Forebody_db2[match(ForebodyTree$tip.label, Forebody_db2$Species), ]
    
    #check rows match 
    db1 <- cbind(ForebodyTree$tip.label, df1_reordered)
    
    
    # Define the data and grouping
    rownames(df1_reordered) <- df1_reordered$Species
    df4_reordered <- df1_reordered[,-c(1, 2)]
    colnames(df1_reordered)
    Skull_MA<-setNames(df1_reordered[,3],df1_reordered[,1])
    Wrist_Flexion_MA_log<-setNames(df1_reordered[,13],df1_reordered[,1])
    Elbow_Flexion_MA_log<-setNames(df1_reordered[,11],df1_reordered[,1])
    Wrist_Extension_MA_log<-setNames(df1_reordered[,12],df1_reordered[,1])
    Shoulder_Extension_MA_log<-setNames(df1_reordered[,5],df1_reordered[,1])
    #create group category 
    locomotor_type<-setNames(df1_reordered[,2],df1_reordered[,1])
    locomotor_type<-as.factor(locomotor_type)
    levels(locomotor_type)
    locomotor_type
    
    # Run Phylogenetic ANOVA
    x3 = aov.phylo(Skull_MA ~ locomotor_type, phy=ForebodyTree, nsim=1000)
    print(x3)
    
    x4 = aov.phylo(Wrist_Flexion_MA_log ~ locomotor_type, phy=ForebodyTree, nsim=1000)
    
    x5 = aov.phylo(Elbow_Flexion_MA_log ~ locomotor_type, phy=ForebodyTree, nsim=1000)
    
    x6 = aov.phylo(Wrist_Extension_MA_log ~ locomotor_type, phy=ForebodyTree, nsim=1000)
    
    x7 = aov.phylo(Shoulder_Extension_MA_log ~ locomotor_type, phy=ForebodyTree, nsim=1000)
    
    # Run Phylogenetic MANOVA - not in paper 
    x4 = aov.phylo(df4_reordered ~ locomotor_type, phy=ForebodyTree, nsim=1000, test="Pillai")
    print(attributes(x4)$summary)
    
    
    #All trait Tree
    AllTraitsTree <- read.tree("All_traits_Species.nwk")
    if(!is.binary(AllTraitsTree)) AllTraitsTree <- multi2di(AllTraitsTree, random = TRUE) 
    
    AllTraits_db <- read.csv("All_traits.csv") 
    
    #create columns with MA and log MA 
    {
      
      AllTraits_db$Skull_Extension_MA <- AllTraits_db$OH / AllTraits_db$SL
      AllTraits_db$Shoulder_Flexion_MA <- AllTraits_db$Shoulder_Flexion / AllTraits_db$HL
      AllTraits_db$Shoulder_Extension_MA <- AllTraits_db$Shoulder_Extension / AllTraits_db$HL
      AllTraits_db$Shoulder_Adduction_MA <- AllTraits_db$Shoulder_Adduction / AllTraits_db$HL
      AllTraits_db$Shoulder_Abduction_MA <- AllTraits_db$Shoulder_Abduction / AllTraits_db$HL
      AllTraits_db$Shoulder_Medial_Rotation_MA <- AllTraits_db$Shoulder_Medial_Rotation / AllTraits_db$UL
      AllTraits_db$Shoulder_Lateral_Rotation_MA <- AllTraits_db$Shoulder_Lateral_Rotation / AllTraits_db$UL
      AllTraits_db$Elbow_Extension_MA <- AllTraits_db$OL / AllTraits_db$UL
      AllTraits_db$Elbow_Flexion_MA <- AllTraits_db$Elbow_Flexors / AllTraits_db$UL
      AllTraits_db$Wrist_Extension_MA <- AllTraits_db$Wrist_Extension / AllTraits_db$ML
      AllTraits_db$Wrist_Flexion_MA <- AllTraits_db$Wrist_Flexion / AllTraits_db$ML
      
      AllTraits_db$Skull_Extension_MA_log <- log(AllTraits_db$OH / AllTraits_db$SL)
      AllTraits_db$Shoulder_Flexion_MA_log <- log(AllTraits_db$Shoulder_Flexion_MA)
      AllTraits_db$Shoulder_Extension_MA_log <- log(AllTraits_db$Shoulder_Extension_MA)
      AllTraits_db$Shoulder_Adduction_MA_log <- log(AllTraits_db$Shoulder_Adduction_MA)
      AllTraits_db$Shoulder_Abduction_MA_log <- log(AllTraits_db$Shoulder_Abduction_MA)
      AllTraits_db$Shoulder_Medial_Rotation_MA_log <- log(AllTraits_db$Shoulder_Medial_Rotation_MA)
      AllTraits_db$Shoulder_Lateral_Rotation_MA_log <- log(AllTraits_db$Shoulder_Lateral_Rotation_MA)
      AllTraits_db$Elbow_Extension_MA_log <- log(AllTraits_db$Elbow_Extension_MA)
      AllTraits_db$Elbow_Flexion_MA_log <- log(AllTraits_db$Elbow_Flexion_MA)
      AllTraits_db$Wrist_Extension_MA_log <- log(AllTraits_db$Wrist_Extension_MA)
      AllTraits_db$Wrist_Flexion_MA_log <- log(AllTraits_db$Wrist_Flexion_MA)
      
      
    }
    
    AllTraits_db <- AllTraits_db[c(1:2,39:49)]
    
    traits_reordered <- AllTraits_db[match(AllTraitsTree$tip.label, AllTraits_db$Species), ]
    
    #check
    db3 <- cbind(AllTraitsTree$tip.label, traits_reordered)
    
    
    locomotor_type2<-setNames(traits_reordered[,2],traits_reordered[,1])
    locomotor_type2
    
    locomotor_type2<-as.factor(locomotor_type2)
    
    
    levels(locomotor_type2)
    
    
    # Define the data and grouping
    rownames(traits_reordered) <- traits_reordered$Species
    traits1_reordered <- traits_reordered[,-c(1, 2)]
    
    # Phylogenetic MANOVA
    x5 = aov.phylo(traits1_reordered ~ locomotor_type2, phy=AllTraitsTree, nsim=1000, test="Pillai")
    print(attributes(x5)$summary)
    
    
    #Tree with as many species as possible - phyloANOVA
    SpeciesTree <- read.tree("AsManySpecies.nwk")
    if(!is.binary(SpeciesTree)) SpeciesTree <- multi2di(SpeciesTree, random = TRUE) # Randomly resolving polytomies if there are any
    
    Species_db <- read.csv("AsManySpecies_v2.csv") 
    
    Habit <-  Species_db[c(1:2)]
    Species_db <- Species_db[-c(1:2)]
    Species_db <- 10^(Species_db)
    Species_db <- cbind(Habit, Species_db)
    
    #create columns with MA and log MA 
    {
      
      Species_db$Skull_Extension_MA <- Species_db$OH / Species_db$SL
      Species_db$Shoulder_Flexion_MA <- Species_db$Shoulder_Flexion / Species_db$HL
      Species_db$Shoulder_Extension_MA <- Species_db$Shoulder_Extension / Species_db$HL
      Species_db$Shoulder_Adduction_MA <- Species_db$Shoulder_Adduction / Species_db$HL
      Species_db$Shoulder_Abduction_MA <- Species_db$Shoulder_Abduction / Species_db$HL
      Species_db$Shoulder_Medial_Rotation_MA <- Species_db$Shoulder_Medial_Rotation / Species_db$UL
      Species_db$Shoulder_Lateral_Rotation_MA <- Species_db$Shoulder_Lateral_Rotation / Species_db$UL
      Species_db$Elbow_Extension_MA <- Species_db$OL / Species_db$UL
      
      
      Species_db$Skull_Extension_MA_log <- log(Species_db$OH / Species_db$SL)
      Species_db$Shoulder_Flexion_MA_log <- log(Species_db$Shoulder_Flexion_MA)
      Species_db$Shoulder_Extension_MA_log <- log(Species_db$Shoulder_Extension_MA)
      Species_db$Shoulder_Adduction_MA_log <- log(Species_db$Shoulder_Adduction_MA)
      Species_db$Shoulder_Abduction_MA_log <- log(Species_db$Shoulder_Abduction_MA)
      Species_db$Shoulder_Medial_Rotation_MA_log <- log(Species_db$Shoulder_Medial_Rotation_MA)
      Species_db$Shoulder_Lateral_Rotation_MA_log <- log(Species_db$Shoulder_Lateral_Rotation_MA)
      Species_db$Elbow_Extension_MA_log <- log(Species_db$Elbow_Extension_MA)
      
      
    }
    
    #find out order of tip.labels
    SpeciesTree$tip.label
    #reorder
    df_reordered <- Species_db[match(SpeciesTree$tip.label, Species_db$Species), ]
    
    #check rows match 
    db3 <- cbind(SpeciesTree$tip.label, df_reordered)
    
    # Define the data and grouping
    rownames(df_reordered) <- df_reordered$Species
    df5_reordered <- df_reordered[,-c(1, 2)]
    colnames(df_reordered)
    Skull_MA_log<-setNames(df_reordered[,24],df_reordered[,1])
    Shoulder_Flexion_MA_log<-setNames(df_reordered[,25],df_reordered[,1])
    Shoulder_Extension_MA_log<-setNames(df_reordered[,26],df_reordered[,1])
    Shoulder_Adduction_MA_log<-setNames(df_reordered[,27],df_reordered[,1])
    Shoulder_Abduction_MA_log<-setNames(df_reordered[,28],df_reordered[,1])
    Shoulder_Medial_Rotation_MA_log<-setNames(df_reordered[,29],df_reordered[,1])
    Shoulder_Lateral_Rotation_MA_log<-setNames(df_reordered[,30],df_reordered[,1])
    Elbow_Extension_MA_log<-setNames(df_reordered[,31],df_reordered[,1])
    
    
    
    #create group category 
    locomotor_type<-setNames(df_reordered[,2],df_reordered[,1])
    locomotor_type<-as.factor(locomotor_type)
    levels(locomotor_type)
    locomotor_type
    
    
    # Run Phylogenetic ANOVA
    x5 = aov.phylo(Skull_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    print(x5)
    
    x6 = aov.phylo(Shoulder_Flexion_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x7 = aov.phylo(Shoulder_Extension_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x8 = aov.phylo(Shoulder_Adduction_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x9 = aov.phylo(Shoulder_Abduction_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x10 = aov.phylo(Shoulder_Medial_Rotation_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x11 = aov.phylo(Shoulder_Lateral_Rotation_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    x12 = aov.phylo(Elbow_Extension_MA_log ~ locomotor_type, phy=SpeciesTree, nsim=1000)
    
    
    # Run Phylogenetic MANOVA - not in paper 
    x4 = aov.phylo(df5_reordered ~ locomotor_type, phy=SpeciesTree, nsim=1000, test="Pillai")
    print(attributes(x4)$summary)
    
    
    
    