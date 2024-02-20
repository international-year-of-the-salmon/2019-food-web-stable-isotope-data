################################################################
#### Project: International Year of the Salmon - 2019 - Food Webs 
#### Author: Genyffer C. Troina
#### Date: 02/14/2024
################################################################


# 1. Installing and loading packages

if(!require(readxl)){install.packages("readlx");library(readxl)}
if(!require(openxlsx)){install.packages("openxlsx");library(openxlsx)}
if(!require(ggpubr)){install.packages("ggpubr");library(ggpubr)}
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(dplyr)){install.packages("dplyr");library(dplyr)}


rm(list= ls())


# 2. Loading isotope data
(IYS2019 <- read_excel("2019_Food Web_Stable Isotope Data.xlsx",
                       sheet = 2)) 


# 3. Estimating TP using Post 2002 equation, local baseline (NW-GoA x SE-GoA)

# 3.1 Subset POM and zooplankton 0.25 mm size fraction from data
POM_GoA <- subset(IYS2019, Main_Group == "POM", select = c("Region", "ẟ13Clipid_corrected","ẟ15N","Main_Group"))
GoA_0.25 <- subset(IYS2019, Species == "Zoop_0.25mm", select = c("Region", "ẟ13Clipid_corrected","ẟ15N","Main_Group"))


head(POM_GoA)
head(GoA_0.25)

# 3.2 Calculate Baseline values:
# mean values/region for ẟ13Clipid_corrected and ẟ15N for POM and 0.25 mm zoop size fraction
POM_Region_mean      <- aggregate(POM_GoA[, c(2,3)], list(POM_GoA$Region), mean)
zoo_0.25_Region_mean <- aggregate(GoA_0.25[, c(2,3)], list(GoA_0.25$Region), mean)


# 3.3: Plotting POM C and N per Region
POM_N <- ggboxplot(POM_GoA, x = "Region", y = "ẟ15N", xlab = "Area",
                   ylab = "N", main = "POM")

POM_C <- ggboxplot(POM_GoA, x = "Region", y = "ẟ13Clipid_corrected", xlab = "Area",
                   ylab = "C", main = "POM")


# 3.4: Plotting Zooplankton 0.25 size fraction C and N per Region
ZOO_N <- ggboxplot(GoA_0.25, x = "Region", y = "ẟ15N", xlab = "Area",
                   ylab = "N", main = "Zooplankton 0.25 fraction")

ZOO_C <- ggboxplot(GoA_0.25, x = "Region", y = "ẟ13Clipid_corrected",  xlab = "Area",
                   ylab = "C", main = "Zooplankton 0.25 fraction")



# 3.5 Estimate TP using mean ẟ15N local Baseline values calculated in 3.2

# 3.5.1: NW-GoA

# 3.5.1.1: subset NW-GoA data
IYS2019_NW = subset(IYS2019, Region == "NW-GoA")

# 3.5.1.2: calculating TP using local baseline values
## all groups with the same TEF:
IYS2019_NW <- IYS2019_NW %>% mutate(Local_TP_POM = ((ẟ15N-2.573107)/3.4)+1)
IYS2019_NW <- IYS2019_NW %>% mutate(Local_TP_025 = ((ẟ15N-5.939565)/3.4)+2)
head(IYS2019_NW)



# 3.5.2: SE-GoA

# 3.5.2.1: subset SE-GoA data
IYS2019_SE = subset(IYS2019, Region == "SE-GoA")

# 3.5.2.2:calculating TP using local baseline values
IYS2019_SE <- IYS2019_SE %>% mutate(Local_TP_POM = ((ẟ15N-3.700636)/3.4)+1)
IYS2019_SE <- IYS2019_SE %>% mutate(Local_TP_025 = ((ẟ15N-6.663699)/3.4)+2)
head(IYS2019_SE)


# 3.6.1: Combining data
IYS2019_TPs <- rbind(IYS2019_NW,IYS2019_SE)
head(IYS2019_TPs)

# 3.6.2: Plotting TP estimates per Region 
TP_GoA <- within(IYS2019_TPs, {
  Species <- reorder(Species, Local_TP_025)
  Main_Group <- reorder(Main_Group, -Local_TP_025)
})


table(TP_GoA$Region)
barplot(table(TP_GoA$Region))

labels  <- c("POM" = "POM" ,
             "Zooplankton" = "Zooplankton",
             "Fish" = "Fish",
             "Squid" = "Squid",
             "Fish_cart" = "Shark",
             "Jellyfish" = "Jellyfish")

N_fun <- function(x){
  return(data.frame(y = 0.95*22,
                    label = length(x)))
}


TP_GoA <- mutate(TP_GoA, Scientific_name = paste(Genus, Species)) 

TP_boxplot <- ggplot(data = TP_GoA, aes(x = Scientific_name, y = Local_TP_025)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6)+
  scale_y_continuous(name="Trophic Position") +
  scale_x_discrete(name="")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.text.y = element_text(angle=0),
        text=element_text(size = 14),
        legend.text = element_text(colour="black", size = 10),
        legend.title = element_text(colour="black", size=11, face="bold"),
        legend.background = element_rect(colour = "black"),
        axis.text.x = element_text(color="black", size=11),
        axis.text.y = element_text(face="italic", color="black", size=11))+
  coord_flip()


png(file="Boxplot_TP.png",width=3000,height=2500,res=300)
par(mar = c(6,6,4,4))
TP_boxplot +
  facet_grid(Main_Group ~ Region, scales = "free_y", space = "free",
             labeller=labeller(Main_Group = labels))

dev.off()


write.csv(TP_GoA, "2019_Food_Web_Stable_Isotope_Data.csv")

