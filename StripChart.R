#Descriptive Statistics by group
#Install ddplyr as follow:
install.packages("dplyr")
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
display.brewer.all()
TEB_drcoutput <- read.csv(file = "TEB_drcoutput.csv", sep = ",", header = TRUE)
str(TEB_drcoutput)
TEB_drcoutput$Trial <- as.factor(TEB_drcoutput$Trial)
TEB_drcoutput$Isolate <- as.factor(TEB_drcoutput$Isolate)
TEB_drcoutput$Year <- as.factor(TEB_drcoutput$Year)
TEB_drcoutput$Year <- factor(TEB_drcoutput$Year, levels = c("Reference","R-Isolates","2008-Isolates","2010-Isolates","2013-Isolates"))
TEB_drcoutput$Fungicide <- as.factor(TEB_drcoutput$Fungicide)
TEB_drcoutput$Model <- as.factor(TEB_drcoutput$Model)
TEB_drcoutput$AIC <- as.numeric(TEB_drcoutput$AIC)
TEB_drcoutput$EC50 <- as.numeric(TEB_drcoutput$EC50)
#Descriptive statistics by groups:
library(dplyr)
group_by(COMBINED,Fungicide) %>% 
  summarise(
    count = n(), 
    min = min(EC50, na.rm = TRUE),
    mean = mean(EC50, na.rm=TRUE),
    median = median(EC50, na.rm = TRUE),
    mean = mean(EC50, na.rm = TRUE),
    max = max(EC50, na.rm=TRUE),
    sd = sd(EC50, na.rm = TRUE)
  )
sd(TEB_stripchart$EC50, na.rm = TRUE)
summary(TEB_stripchart$EC50)
#Graphics for grouped data:
library("ggpubr")
# Box plot colored by groups: Species
ggboxplot(TEBUCONAZOLE, x = "Trial", y = "EC50.estimate",
          color = "Trial",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

#Import stripchart specific data
TEB_stripchart <- read.csv(file = "stripchart_TEB_by_Year.csv", sep = ",", header = TRUE)
str(TEB_stripchart)
TEB_stripchart$Isolate <- factor(TEB_stripchart$Isolate)
TEB_stripchart$Year <- factor(TEB_stripchart$Year, levels = c("Reference", "R-Isolates", "2008-Isolates","2010-Isolates","2013-Isolates"))
TEB_stripchart$EC50 <- as.numeric(TEB_stripchart$EC50)
# Stripchart colored by groups: Species
tiff(filename = "StripChart_EC50_by_Fungicide.tiff", width = 6.4, height = 3, units = "in", res = 300)
ggstripchart(COMBINED, x = "Fungicide", y = "EC50", ylim=c(0,40),
             ylab = "EC50 values",
            # order = c("Reference", "R-Isolates", "2008-Isolates", "2010-Isolates","2013-Isolates"),
             color = "Fungicide",
             palette = brewer.pal(5, "Set2"),
             #add = "mean_sd",
             legend="none")
text(1,16,labels="R1213", cex=0.6)
?text()
dev.off()
summary(TEBUCONAZOLE$EC50.estimate)
?ggstripchart

#stripchart with ggplot2
tiff(filename = "Fig3B.tiff", units = "in", height = 4, width = 6.4, res = 300)
ggplot(PRO_pheno, aes(x=factor(Year, levels = c("Reference", "R-Isolates", "2008-Isolates", "2010-Isolates", "2013-Isolates")), y=EC50, color=Year))+ labs(x="Year", y="EC50: Prothioconazole", tag = "B")+
  geom_jitter(position = position_jitter(0.2))+
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,5))+
  theme_classic()+
  theme(legend.position = "none", plot.tag.position = c(0.05,0.05))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))+
  stat_compare_means(method = "kruskal.test")+
  #annotate("text",x=5,y=3.5,label="F13_44", size=3)+
  annotate("text",x=5, y=15.6, label="F13_83", size=3)+
  annotate("text", x=4.8, y=19.4, label="F13_84", size=3)+
  annotate("text", x=5.3, y=20, label="F13_159", size=3)+
  annotate("text", x=4.9, y=21.52, label="F13_78", size=3)+
  annotate("text", x=5, y=34.5, label="F13_66", size=3)
dev.off()
summary(TEB_stripchart$EC50)
?annotate()
?ggplot
