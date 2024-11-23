library(tidycomm)
library(readxl)


#Purpose: is to test whether 

#----Data Preparation----#

data <- read_excel("HC_GWL_Stations_Clustering_Spatial Autocorrelation.xlsx")
data <- read_excel("x.xlsx")
dim(data)

head(data)

#---Contingency Tables---#
X<-crosstab(data,TR,SE, add_total = F)
crosstab(data,TR,WDCs, add_total = TRUE)
crosstab(data,WDCs,SE, add_total = TRUE)

#---Chi-Squares Test---#
crosstab(data,TR,SE  ,chi_square = TRUE, add_total=TRUE,percentages = TRUE)
crosstab(data,TR,WDCs,chi_square = TRUE, add_total=TRUE,percentages = TRUE)
crosstab(data,WDCs,SE,chi_square = TRUE, add_total=TRUE,percentages = TRUE)

crosstab(data ,TR, SE) %>% 
visualize()

crosstab(data ,TR, WDCs) %>% 
    visualize()

crosstab(data ,WDCs,SE) %>% 
    visualize()


