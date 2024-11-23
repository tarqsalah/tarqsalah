library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

#----Data Preparation----#
Data <- read.csv("GWL_100m_Statistical analysis.csv")

Data$Cluster<- as.factor(Data$Cluster)

Summary_DEM <- Data %>% group_by(Cluster) %>% summarise(mean=mean(MEDIAN_DEM), Median=median(MEDIAN_DEM), Std=sd(MEDIAN_DEM))

Summary_HAND <- Data %>% group_by(Cluster) %>% summarise(mean=mean(MEDIAN_HAND), Median=median(MEDIAN_HAND), Std=sd(MEDIAN_HAND))

Summary_Slope <- Data %>% group_by(Cluster) %>% summarise(mean=mean(MEDIAN_Slope), Median=median(MEDIAN_Slope), Std=sd(MEDIAN_Slope))



ggplot(Data, aes(x=Cluster, y= PCT90_HAND, fill = Cluster)) +
  geom_boxplot()






# #---Data Exploration---#
# # Trend
# ggplot(Data_TR, aes(y =Mean.Trend, x = Cluster, fill = Cluster)) +
#     geom_boxplot() +
#     labs(title = "Mean trend signal across clusters",
#         x = "Cluster",
#         y = "Mean Trend",
#         fill = "Cluster") +
#         theme_minimal()



#---ANOVA Clusters membership and topographical features---#
Slope_test<- kruskal.test(Data$MEDIAN_Slope~Data$Cluster)
HAND_test <- kruskal.test(Data$MEDIAN_HAND~Data$Cluster)
DEM_test <-  kruskal.test(Data$MEDIAN_DEM~Data$Cluster)

summary(Slope_test)
summary(HAND_test)
summary(DEM_test)


ggplot(Data, aes(y =MEDIAN_Slope, x = Cluster, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Slope between GWL clusters",
       x = "Cluster",
       y = "Slope",
       fill = "Cluster") +
  stat_summary(
    fun= median,
    geom = "point",
    shape=23,
    size=3,
    color= "red",
    fill="yellow"
  ) +
  theme_minimal()




Slope_test<- aov(Data$MEDIAN_Slope~Data$Cluster)
HAND_test <- aov(Data$MEDIAN_HAND~Data$Cluster)
DEM_test <- aov(Data$MEDIAN_DEM~Data$Cluster)


# Post-hoc analysis: Tukey's HSD
tukey_result_HAND <- TukeyHSD(HAND_test)  %>% print() %>% plot()
tukey_result_DEM  <- TukeyHSD(DEM_test)   %>% print() %>% plot()
tukey_result_Slope<- TukeyHSD(Slope_test) %>% print() %>% plot()


#perform pairwise t-tests with Bonferroni's correction
pairwise.wilcox.test(Data$MEDIAN_DEM,  Data$Cluster, p.adjust.method="bonferroni")
pairwise.wilcox.test(Data$MEDIAN_Slope,Data$Cluster, p.adjust.method="bonferroni")
pairwise.wilcox.test(Data$MEDIAN_HAND, Data$Cluster, p.adjust.method="bonferroni")



SoilDraina_test    <- fisher.test(table(Data$Cluster, Data$SoilDraina), simulate.p.value = TRUE) %>% print()
AquiferCa_test     <- fisher.test(table(Data$Cluster, Data$AQUIFER_CAT), simulate.p.value = TRUE) %>% print()
Vulnerability_test <- fisher.test(table(Data$Cluster, Data$VUL_CAT), simulate.p.value = TRUE) %>% print()
SoilPermeabil_test <- fisher.test(table(Data$Cluster, Data$SSPERM), simulate.p.value = TRUE) %>% print()
DTB_test           <- fisher.test(table(Data$Cluster, Data$DTB), simulate.p.value = TRUE) %>% print()
Transmissivity_test<- fisher.test(table(Data$Cluster, Data$T), simulate.p.value = TRUE) %>% print()




Cluster_I <- Data$Cluster_I <- ifelse(Data$Cluster == "1", "Cluster I", "Other")


contingency_table <- table(Data$Cluster_I, Data$SoilDraina)
ClusterI_SoilDraina <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_I, Data$AQUIFER_CAT)
ClusterI_AquiferCa <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_I, Data$VUL_CAT)
ClusterI_Vulnerability <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_I, Data$SSPERM)
ClusterI_SoilPermeabil_test <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_I, Data$DTB)
ClusterI_DTB_test <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_I, Data$T)
ClusterI_Transmissivity <- fisher.test(contingency_table, simulate.p.value=TRUE) %>% print()





Cluster_II <- Data$Cluster_II <- ifelse(Data$Cluster == "2", "Cluster II", "Other")


contingency_table <- table(Data$Cluster_II, Data$SoilDraina)
ClusterI_SoilDraina <- chisq.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_II, Data$AQUIFER_CAT)
ClusterI_AquiferCa <- chisq.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_II, Data$VUL_CAT)
ClusterI_Vulnerability <- chisq.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_II, Data$SSPERM)
ClusterI_SoilPermeabil_test <- chisq.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_II, Data$DTB)
ClusterI_DTB_test <- chisq.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_II, Data$T)
ClusterI_Transmissivity <- chisq.test(contingency_table, simulate.p.value=TRUE) %>% print()



Cluster_III <- Data$Cluster_III <- ifelse(Data$Cluster == "3", "Cluster III", "Other")

contingency_table <- table(Data$Cluster_III, Data$SoilDraina)
ClusterI_SoilDraina <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_III, Data$AQUIFER_CAT)
ClusterI_AquiferCa <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_III, Data$VUL_CAT)
ClusterI_Vulnerability <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_III, Data$SSPERM)
ClusterI_SoilPermeabil_test <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_III, Data$DTB)
ClusterI_DTB_test <- fisher.test(contingency_table) %>% print()

contingency_table <- table(Data$Cluster_III, Data$T)
ClusterI_Transmissivity <- fisher.test(contingency_table, simulate.p.value=TRUE) %>% print()





# Separate Group 1 and Other Groups
Cluster_I <- Data$MEDIAN_HAND[Data$Cluster == "1"]
other_Cluster <- Data$MEDIAN_HAND[Data$Cluster != "1"]

HAND_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 1 and Other Groups
Cluster_I <- Data$MEDIAN_DEM[Data$Cluster == "1"]
other_Cluster <- Data$MEDIAN_DEM[Data$Cluster != "1"]

DEM_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 1 and Other Groups
Cluster_I <- Data$MEDIAN_Slope[Data$Cluster == "1"]
other_Cluster <- Data$MEDIAN_Slope[Data$Cluster != "1"]
Slope_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()






# Separate Group 2 and Other Groups
Cluster_I <- Data$MEDIAN_HAND[Data$Cluster == "2"]
other_Cluster <- Data$MEDIAN_HAND[Data$Cluster != "2"]

HAND_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 2 and Other Groups
Cluster_I <- Data$MEDIAN_DEM[Data$Cluster == "2"]
other_Cluster <- Data$MEDIAN_DEM[Data$Cluster != "2"]

DEM_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 2 and Other Groups
Cluster_I <- Data$MEDIAN_Slope[Data$Cluster == "2"]
other_Cluster <- Data$MEDIAN_Slope[Data$Cluster != "2"]
Slope_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()




# Separate Group 3 and Other Groups
Cluster_I <- Data$MEDIAN_HAND[Data$Cluster == "3"]
other_Cluster <- Data$MEDIAN_HAND[Data$Cluster != "3"]

HAND_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 3 and Other Groups
Cluster_I <- Data$MEDIAN_DEM[Data$Cluster == "3"]
other_Cluster <- Data$MEDIAN_DEM[Data$Cluster != "3"]

DEM_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()


# Separate Group 3 and Other Groups
Cluster_I <- Data$MEDIAN_Slope[Data$Cluster == "3"]
other_Cluster <- Data$MEDIAN_Slope[Data$Cluster != "3"]
Slope_test <- wilcox.test(Cluster_I, other_Cluster) %>% print()










#Welch ANOVA test:
library(onewaytests)
W_ANOVA_TR <- welch.test(Mean.Trend~Cluster,data=Data_TR)
W_ANOVA_WDCs_TR <- welch.test(Mean.WDCs.Signal~Cluster,data = Data_WDCs)









library("tidyverse")
library(onewaytests)
library(ggplot2)


data<-read.csv("Trend Data.csv")
data_Ws<-read.csv("WDCs Data.csv")




data_long<-pivot_longer(data, cols=2:54,names_to="Week", values_to = "Values")
data_long_ws<-pivot_longer(data_Ws, cols=2:21,names_to="Percentile", values_to = "Values")


ggplot(data_long, aes(y =Values, x = Cluster, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Trend signal across clusters",
       x = "Cluster",
       y = "Weekly Trend",
       fill = "Cluster") +
  theme_minimal()

ggplot(data_long_ws, aes(y =Values, x = Cluster, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "WDCs signal across clusters",
       x = "Cluster",
       y = "Per.",
       fill = "Cluster") +
  theme_minimal()

#---ANOVA- Clusters and Mean trend ---#
trend_test <- aov(data_long$Values~data_long$Cluster)
summary(trend_test)


#---ANOVA- Clusters and Mean WDCS ---#
WDCS_test <- aov(data_long_ws$Values~data_long_ws$Cluster)
summary(WDCS_test)


# Check assumptions

# Normality
plot(trend_test, 2)
plot(WDCS_test, 2)


# Homogeneity of variances
leveneTest(Values ~ Cluster, data = data_long)
leveneTest(Values ~ Cluster, data = data_long_ws)

#Welch ANOVA test:
W_ANOVA_TR <- welch.test(Values~Cluster,data=data_long)
W_ANOVA_Ws <- welch.test(Values~Cluster,data=data_long_ws)













