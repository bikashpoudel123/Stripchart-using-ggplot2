#Before we create stripchart for our dataset, lets look closely at the dataset (Descriptive statistics)
#Install and load libraries as shown below
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
display.brewer.all()

#The example dataset used is from a fungicide sensitivity assay for Fusarium head blight pathogen to two triazole fungicides 'tebuconazole' and 'prothioconazole'.
#I want to visualize distribution of EC50 values of fungal isolates to tebuconazole using stripchart.
EC50_drcoutput <- read.csv(file = "EC50_phenotypeanalysis_drcoutput.csv", sep = ",", header = TRUE)
str(EC50_drcoutput)
EC50_drcoutput$Trial <- as.factor(EC50_drcoutput$Trial)
EC50_drcoutput$Isolate <- as.factor(EC50_drcoutput$Isolate)
EC50_drcoutput$Year <- factor(EC50_drcoutput$Year, levels = c("Reference","R-Isolates","2008-Isolates","2010-Isolates","2013-Isolates"))
EC50_drcoutput$Fungicide <- as.factor(EC50_drcoutput$Fungicide)
EC50_drcoutput$Model <- as.factor(EC50_drcoutput$Model)
EC50_drcoutput$AIC <- as.numeric(EC50_drcoutput$AIC)
EC50_drcoutput$EC50 <- as.numeric(EC50_drcoutput$EC50)

#Descriptive statistics by groups: using dplyr package
group_by(EC50_drcoutput,Fungicide) %>% 
  summarise(
    count = n(), 
    min = min(EC50, na.rm = TRUE),
    mean = mean(EC50, na.rm=TRUE),
    median = median(EC50, na.rm = TRUE),
    mean = mean(EC50, na.rm = TRUE),
    max = max(EC50, na.rm=TRUE),
    sd = sd(EC50, na.rm = TRUE)
  )

#Subset data to only contain tebuconazole dataset
TEB_EC50 <- subset(EC50_drcoutput, (Fungicide=="Tebuconazole"))

# Box plot grouped by fungicide:
ggboxplot(EC50_drcoutput, x = "Fungicide", y = "EC50",
          color = "Fungicide",
          palette = brewer.pal(2, "Set2")

#Box plot of tebuconazole EC50 values grouped by Year:
ggboxplot(TEB_EC50, x="Year", y="EC50",
          color="Year",
          palette = brewer.pal(5, "Set2")

# Stripchart to show distribution of EC50 values for both fungicides
ggstripchart(EC50_drcoutput, x = "Fungicide", y = "EC50", ylim=c(0,40),
             ylab = "EC50 values",
             #order = c("Reference", "R-Isolates", "2008-Isolates", "2010-Isolates","2013-Isolates"),
             color = "Fungicide",
             palette = brewer.pal(2, "Set2"), 
             legend="none") #This prevents printing out legends
          
#Stripchart to show distribution of EC50 values for tebuconazole fungicide

#stripchart with ggplot2
ggplot(PRO_pheno, aes(x=factor(Year, levels = c("Reference", "R-Isolates", "2008-Isolates", "2010-Isolates", "2013-Isolates")), y=EC50, color=Year))+ labs(x="Year", y="EC50: Prothioconazole", tag = "B")+
  geom_jitter(position = position_jitter(0.2))+
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,5))+
  theme_classic()+
  theme(legend.position = "none", plot.tag.position = c(0.05,0.05))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))+
  stat_compare_means(method = "kruskal.test")+
  annotate("text",x=5,y=3.5,label="F13_44", size=3)+ #this adds text in the chart at (x,y) and labels them
  annotate("text",x=5, y=15.6, label="F13_83", size=3)+
  annotate("text", x=4.8, y=19.4, label="F13_84", size=3)+
  annotate("text", x=5.3, y=20, label="F13_159", size=3)+
  annotate("text", x=4.9, y=21.52, label="F13_78", size=3)+
  annotate("text", x=5, y=34.5, label="F13_66", size=3)
