# Spatial Autocorrelation of Income and French Knowledge in Nanaimo BC, Canada.
Ben Milligan

Geog 418 Assignment 3

October 20, 2024

---


## Introduction

This assignment will demonstrate how to do spatial autocorrelation on a dataset using rstudio. To complete this assignment, you will need the "ucgsJQnBVLvP_data.csv" csv file and the "lda_000a16a_e.shp" shapefile. 

Spatial Autocorrelation is a process that allows a user to understand how similar values are grouped over a study area. This means that a positive autocorrelation would find large values near large values, and small values near small values (Li, 2012). Random patterns will show no connection between values and their location and negative autocorrelation will find large values near small values. This is a core concept in statistical assessments interested in spatial analysis, and can challenge the assumption of independence many other statistical processes assume (Li, 2012).

This analysis in particular will look at the grouping of two variables in Nanaimo BC, Canada, income and French Knowledge. In many Canadian metropolitan areas, there is significant separation of different income classes (Ross, 2004). This leads to clustering of very high and very low income people, and less dispersion of different income groups across the landscape (Ross, 2004). This can have a broad sweeping effect on life in urban centers, including changes to economic opportunities, health and healthcare, and general social conditions (Ross, 2004). Is income in Nanaimo clustered, distributed, or random, and how might that effect the city?

French knowledge in Canada outside of Quebec has very interesting associations with spatial autocorrelation. 
French speakers are more willing then anglophones to remain in areas of Canada that are predominantly English speaking, as long as there are a few other Francophone present (Kaplan, 1994). The percentage estimated as being the critical level of Francophone density is around 4% (Kaplan, 1994). French knowledge and retention of French as a mother tongue also declines when French is not the dominant language of a region unlike english (Kaplan, 1994). French speaking immigrant populations are known to cluster in areas such as Toronto, and in a 2020 study all French speaking origin country immigrants except Algeria and Belgium showed significant clustering at 95% confidence using a global Morans I (Karwowski, 2020). These clusters did not necessarily correlate to French speaking as much as national origin as determined by a local autocorrelation analysis, as the observed clusters did not significantly overlap (Karwowski, 2020). Will this trend hold consistent for Nanaimo?

## Setting up the Analysis

Libraries are a database of functions available within R to perform various functions. Initially, libraries will need to be installed to ensure they are available for use within the system using the 'install.packages("")' command. Once packages are installed they are always available to be loaded. At the beginning of each coding project, the command 'library("")' will need to be used to activate the necessary libraries. Alternatively, libraries can be manually activated in the "Packages" section of the lower right hand panel. If a command is used in the code that is from a unloaded library, an error will occur. Luckily, the missing command can be entered in the "Help" section of the same window to detail what the command should do, and which package it is apart of.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
#install.packages("knitr")
#install.packages("spatstat")
#install.packages("ggplot2")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("st")

#Load libraries:
library(raster)
library(spatstat)
library(tmap)
library("knitr")
library(sf)
library(ggplot2)
library(raster)
library(sp)
library(tmap)
library(dplyr)
library(st)
library(plyr)
library(e1071)
library(spdep)


```

To begin this analysis, we will first need to set our working directory. To do this, set up a file on your hard drive to work out of, all files necessary to the analysis should be stored here. Note: Make a individual folder for this, do not use a shared folder with other projects and definitely do not use the downloads folder of your computer, good file management and organization is key to stress free projects. When you have the folder set up, use the code: 

dir <- "file path"

setwd(dir)

The file path should resemble the example below.
C:/Users/student/Documents/R-Projects/Assignment/

All files needed for the analysis should be stored here, and R will load and save files into this folder when commanded to.

Next we need to load our CSV and shape file into R. To do this, use the code provided, ensuring you use the correct file names when loading the data. Check the CSV in the top right hand pane of the RStudio window. 

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Set Working Directory
dir <- "C:/Users/student/R-Projects/Assignment/"
setwd(dir)


#Read the csv
csv <- read.csv("ucgsJQnBVLvP_data.csv") 

#Read the Shp file
shp <- st_read("lda_000a16a_e.shp") 

```
For our next step we need to organize and clean the data within the CSV, to make it usable and understandable within R. First we will set column names, so we can accurately draw the variables of interest out of the CSV.

The next two steps are vital to spatial analysis. First we need to tie our spatial data (shapefile) to our non spatial data (CSV) otherwise the CSV data will have no spatial component making spatial analysis of it impossible. Once this is complete, we will need to set the city we will be analyzing, in this example I will use Nanaimo, but you can use any city listed in the CSV.

The data for French Knowledge isn't particularly useful to our analysis in this form, so instead we will turn it into a rate, so we can see the percent of french speakers in an area as compared to the total population.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#Crate column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply column names
colnames(csv) <- cols

#Add column for ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs that are less then 8
csv_clean <- subset(csv, csv$len == 8)

#Merge csv with spatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Choose local subset
Municp <- subset(census_DAs, census_DAs$CMANAME == "Nanaimo")

#Convert frnch knowledge to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Some of the data provided by this data set is incomplete, resulting in a NA value, which could cause issues for our analysis down the line. Luckily these values are easy to remove from the data set, and can be removed using the code below.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

After cleaning and organizing the data we can finally take a look at the general descriptive statistics. We will calculate the mean, standard deviation, and skewness for both the income and the percent of french speaking individuals. We will then display these statistics in an easy to read table that can be included with other deliverables.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive statistics for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive statistics for French
meanFrench <- mean(French_noNA$`PercFrench`)
stdevFrench <- sd(French_noNA$`PercFrench`)
skewFrench <- skewness(French_noNA$`PercFrench`)

#Formate data to prepare for a table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive Statistics for income and french knowedge in Nanaimo BC."))
```
![image](https://github.com/user-attachments/assets/07ac0a80-4e2b-4b00-a5f4-0a88b35d94ab)

This is a spatial analysis however, so we will need to map the results in order to understand their spatial relationships. While there are several mapping systems within R, we will be using the tmaps library. tmaps has a number of useful features, such as choosing the pallete of the map. The code for this is:

tmaptools::palette_explorer()

Make sure to use a # sign to disable this in your code, as it may interfere with how the code runs. This map will show us the study area color coded by the income or percent french speaking variables. Generating maps like this early can allow you to visually examine trends in the data, which can be useful for planning further analysis, or realizing if previous or future steps contain errors. For example, if no data shows up on the map or the data is noticeably incorrect you may realize a previous step may have malfunctioned, while if later data gives conflicting results this may help narrow down where in the code the error is.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Maps of the distribution of income (left) and french Knowledge (right) for Nanaimo BC."}
#Choose a pallete

#tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "Blues", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "Oranges", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/05d5bfc4-1515-4cfc-812a-a46b5d584d2f)

Figure 1: Maps of the distribution of income (left) and french Knowledge (right) for Nanaimo BC.

## Neighbourhood Matrix

Neighborhood analysis looks at how far away the nearest neighboring cell is from each cell. This could be vary tedious to calculate manually, but luckily the spdep library has a number of functions including poly2nb which perform these actions for you. This can help us determine clustering or dispersion patterns, giving us a better understanding of how points are distributed across the landscape.

When using Queen and rook neighborhood matrices we first need to have an understanding of what each of these methods entails. To do this, think chess. The Queen weighting looks at all bordering cells, including the diagonals whereas the  rook weighting uses only the 4 cardinal directions from the cell.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Queens weight
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)

```

These maps show the differences between rooks weight and queens weight for autocorrelation. The first maps shows queens weight alone and the second shows rooks weight alone. The final map shows the two together, overlayed on each other. As you can see the rooks weight is entirely covered by the queens weight, but there are a few lines from queens weight (in blue) that are not also covered by rooks weight. This makes sense when considering the theory as mentioned above, as the queens weight should cover the whole rooks weight as well as a few extra connections.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="A map of the queens weight (Left) rooks weight (center) and the queens weight overlayed on the rooks weight (right) for income."}
#for income
#Make queens weight map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='blue')

#Make rooks weight map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Make combined queen/rooks weight map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)


#Print maps
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```
![image](https://github.com/user-attachments/assets/fe08fcac-85ef-4296-8f9a-f5af32b47d5a)

Figure 2: A map of the queens weight (Left) rooks weight (center) and the queens weight overlayed on the rooks weight (right) for income.

And now we duplicate this process for the French speaking data. Note that the connections are very similar to the previous, with only a few small changes near the edges probably due to a polygon displaying NA for french knowledge.

```{r French Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="A map of the queens weight (Left) rooks weight (center) and the queens weight overlayed on the rooks weight (right) for French speaking."}
#For French

#Make queens weight map
FrenchQueen <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net) + tm_lines(col='blue')

#Make rooks weight map
FrenchRook <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net2) + tm_lines(col='red', lwd = 2)

#Make combined queen/rooks weight map
FrenchBoth <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(French.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(French.net2) + tm_lines(col='red', lwd = 2)

#Print maps
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)
```
![image](https://github.com/user-attachments/assets/fbc9cf3b-f1b3-4948-8aba-abca19b4af18)

Figure 3: A map of the queens weight (Left) rooks weight (center) and the queens weight overlayed on the rooks weight (right) for French speaking

## Weighted Matrix 

Weighted neighbor systems are different from traditional neighbor systems because they are not nessesarily binary (Bivand, n.d.). Instead of a point simply being a neighbor or not, each point near the point of interest has a weighting assigned to it, and contributes to the system in proportion to that weighting.

There are different styles that can be selected with a weighted matrix, that can be used for different things. There are six different styles in all  “B”, “W”, “C”, “U”, “S”, and “minmax” (Bivand, n.d.). B is the most basic style, and is used for basic binary coding (Bivand, n.d.). W, the method used for this study, is for row standardized assessment, and sums up all the values that link to n (Bivand, n.d.). C is a global standardized value, and similarly to W it sums all values that link to n (Bivand, n.d.). U is calculated by dividing the value for C by the number of neighbors (Bivand, n.d.). S is variance-stabilized that also sums over all links to n (Bivand, n.d.). Minmax divides the weights by the maximum column sum and the minimum of the maximum row sum by the weight of the input (Bivand, n.d.).

Setting the Zero policy to "true" inserts weights of 0 for values that have neighbors not on the neighbor list (Bivand, n.d.). This allows the system to run even when neighbor values are empty, which would otherwise stop the program from calculating these values (Bivand, n.d.).

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

subset_weights <- head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)

```


## Global Moran’s I

Once we have finished with the nearest neighbor matrix and the weighted nearest neighbor matrix we can calculate a Morans I. The global Morans I is a statistical method of determining spatial autocorrelation. Spatial autocorrelation is how similar nearby points are to each other. This measures both the location of points and their values at the same time to determine how location and the value interacts with all others across the study area (ESRI, n.d.). This is most effective when factors are uniform across the study area, and other statistical tests can be used for data that has higher spatial variation (ESRI, n.d.).

Positive spatial correlation is present when similar values are grouped together. Whereas negative spatial correlation describes when similar values are spaced out. If you are familiar with other spatial statistical methods, consider this clustered and dispersed distributions, but with the added complexity of considering the value of each point as well. Similar to other spatial statistical calculations this is compared to a simulated random distribution of points. The output of a global Morans I is a singular value that tell us how the entire study area autocorrelates.

A global Morans I is a standard statistical autocorrelation test used for this kind of analysis, and similar studies like the Karwowski (2020) study on immigrant clustering of Francophone populations used a very similar method to the one displayed here (Karwowski, 2020).
 

![image](https://github.com/user-attachments/assets/7dd02690-a0c1-4f7b-9fcb-b4b188a6b295)


While this formula looks extremely complicated it is actually fairly simple. The numerator simply determines the difference between point i and the mean, and the neighbour j and the mean. These values are then multiplied by the weight w, and summed. The denominator is simply used for standardization. Luckily, we do not need to type this whole equation into the code, we can simply use the "moran.test" feature below to calculate it.


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Income Global Moran's I
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Income Global Moran's I results
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#French Global Moran's I results
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

The Global Morans I indexes for Income and French Knowledge are 0.58 and 0.13 respectively, which falls into the expected range of -1 to 1 (Esri). It is always smart to check if calculated values fall into the expected range, as this can help identify problems in the code that may have otherwise been missed. The expected morans I, calculated by the equation Ei=-1/(N-1), was -0.0057 for income, and -0.0056 for French knowledge. Expected I values should always be negative, and the larger the sample size (n) the closer to zero this value should be. Since both of our calculated Morans I values are well above the estimated Morans Is we can infer that the data for both variables is positively correlated. Our variances are also quite small, 0.002 for both variables, showing that the data is fairly evenly distributed across the study area.

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

The Z scores for these variables can then show us the significance of the results. For this analysis, we will be using a 95% confidence interval, as this is standard for most statistical analysis. If the z score falls between -1.96 and 1.96 we fail to reject the null, while if the data falls outside of this range we reject the null. For these values a negative indicates dispersed and a positive indicates clustered.

For income, our null hypothesis is that there is the distribution of income is random and our alternative hypothesis is the distribution of income is not random.

For French speaking our null hypothesis is that the distribution of French knowledge is random and our alternative hypothesis is the distribution of french knowledge is not random.

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

Our data shows that for our income variable our zscore is 13.01, which is well outside of the range of -1.96 and 1.96. Therefore we fail to reject the null hypothesis that income is random. Additionally, because because the z score is positive, we can say that income in Nanaimo may be significantly clustered.

Our data shows that for the french speaking variable the zscore of 3.05 which is outside the range of -1.96 and 1.96 so we reject the null hypothesis that French knowledge is random. Additionally, because the z score is positive, we can say that French knowledge in Nanaimo may be clustered, but not as significantly as income is.


## Local Spatial Autocorrelation

The local Morans I equation is similar to the global Morans I, but instead of outputting a singular value it allows us to analyze how individual points auto correlate to their neighbors. This means each value gets a I, Z, expected I, and variance I. This is very valuable for showing the deeper connections across a landscape, but can create a lot of excess data in larger data sets.

Local Morans Is require two key components. They must indicate spatial clustering around each feature, ant they must sum to a value proportional to the global Morans I (Anselin, 1995). This test, when completed will show were local hotspots of clustering are occurring across the  landscape (Anselin, 1995).

![image](https://github.com/user-attachments/assets/f4980f94-b6c4-4592-87d3-aa85d7be73f2)


For the first section of the equation, the local value (xi) is subtracted by the mean (x), and that value is divided by the standard deviation (si). This division by the standard deviation allows us to see how each individual neighbor value varies from the mean. The second half of the equation multiplies the sum of the difference between a points neighbors and the mean, then divides by the point subtracted from the mean. the code for this feature is "localmoran(" $ ")" as seen below. Organize your code similarly to ensure accurate results.

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Income LISA test
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Income LISA test results 
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#French LISA test
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#French LISA test results
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

In order to properly understand the local Morans I we need to map the results. This involves using our previous map but instead of using the mean values for each area we use the local Morans I score to visualize how spatial autocorrelation is distributed across the study area. To do this we calculate the LISA values or the Local Indicators of Spatial Autocorrelation. Similar to the global Morans I values between -1.96 and 1.96 and considered random, values above 1.96 are positively autocorrelated and values below -1.96 are negatively autocorrelated. 

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Morans I mapping for Income (left) and French Knowledge (right)."}
#Map Income LISA z-scores
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores for Income",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-OrRd", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map French LISA z-scores 
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores For French Speaking",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-Blues", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/1e340ef2-3cf0-4755-9877-933575424b3c)

Firgure 4: LISA mapping for Income (left) and French Knowledge (right).

For income, most areas appear to by randomly distributed or clustered. Random values (between -1.96 and 1.96) are the most common across the landscape, but there are a few clusters of clustered data. From the scale, we know the highest clustered value is 4.0. There is one area that appears distributed however, but its distribution value of -3 does not offset all the clustered values over the entire landscape.

The french knowledge map, on the other hand, seems almost entirely random. There are a few clustered and dispersed values spread around, but there appears to be little pattern across the landscape. There also doesn't appear to be any major similarities in patterns between the maps. In both cases areas are mostly random, but have some small areas that are more clustered or dispersed.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "A local morans I (LISA) plot for income in Nanaimo"}

#Income Moran's I scatter plot
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, 
           spChk=NULL, labels=NULL, xlab="Value at location i", 
           ylab="Value of neighbours j", quiet=NULL)

```
![image](https://github.com/user-attachments/assets/a73de7ac-56d9-48be-bf9e-266e32bc172f)

Figure 5: A local morans I (LISA) plot for income in Nanaimo.

Local Morans I values can also be plotted on a graph to make them easier to interpret. This allows us to visually examine the difference between a value i and the value of neighbors j. Values in the lower left and upper right indicate positive spatial correlation, while values in the top left and lower right indicate negative spatial correlation. The slope of the line indicates the degree of spatial autocorrelation present in the dataset. The diamond shaped outliers are values that have a P = 0.05 or lower, and are identified with their sample number so it is easier to examine them. Use the "moran.plot" with your incomenoNA feature to create these graphs.

As you can see for the income graph, almost all points fall within the positive autocorrelation sections of the graph. The best fit line is also fairly steep, indicating a decent amount of positive spatial autocorrelation is present, which backs up previous findings

```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "A local morans I (LISA) plot for french knowledge in Nanaimo"}
#French Moran's I scatter plot
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE,
           spChk=NULL, labels=NULL, xlab="Value at location i", 
           ylab="Value of neighbours j ", quiet=NULL)
```
![image](https://github.com/user-attachments/assets/b0362fac-0c82-46a4-a542-c8bc61ba8efa)

Figure 6: A local morans I (LISA) plot for french knowledge in Nanaimo.

For the french knowledge graph the values are far more dispersed across the graph. This and the shallower best fit line indicates that the data here is far closer to random, and while some autocorrelation is present, it is minor compared to the income graph.


## Summary

Overall for both variables there appears to be some clustering on the global scale, but this is far less evident on the local scale. The french knowledge map especially appears to be almost entirely random on the local scale, so much so it surprises me that the global value was slightly clustered. The graph of the LISA Morans I gives context to this, showing just how close the French knowledge data is to random.

The income map appeared to be more generally clustered on the local scale, with several distinct areas of clustering across the map. This matches with its global Z score. The LISA graph also demonstrates this, showing just how positive the autocorrelation for this data is.

Overall it appears that income is strongly autocorrelated, meaning rich people live near rich people and poor people live near poor people. This is expected, as often times neighborhoods are delineated by income and income disparity is commonly considered a problem (Ross, 2004). However the near randomness of French knowledge was unexpected. The Karwowski (2020) data led me to expect a more significant autocorrelation between languages (Karwowski, 2020). However this may be more pronounced if we were looking at French as a first language, instead of only french knowledge. This broader view probably catches a lot more people with french as a second language, which may be less clustered due to the lack of a shared cultural component.

Overall the data for Nanaimo, BC, was fairly interesting, and showed a range of what positively autocorrelated data looks like. It would have been interesting to see a negatively autocorrelated dataset to compare to, but we couldn't know what would or wouldn't be autocorrelated until after the analysis was complete. Hopefully this tutorial has demonstrated the general methodologies behind autocorrelation, and explains expected results adequately to the reader to allow them to perform their own analysis in the future. 

## References

Anselin L. (1995), Local Indicators of Spatial Association – LISA; Geographical Analysis, 27(2): 93–115, https://onlinelibrary.wiley.com/doi/10.1111/j.1538-4632.1995.tb00338.x

Bivand R. (n.d) Spatial weights for neighbours lists, spdep 1.3-6 github https://r-spatial.github.io/spdep/reference/nb2listw.html#author

ESRI (n.d.) How Spatial Autocorrelation (Global Moran's I) works,  https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm

Kaplan, D. H. (1994). Population and Politics in a Plural Society: The Changing Geography of Canada’s Linguistic Groups. Annals of the Association of American Geographers, 84(1), 46–67. https://search.library.uvic.ca/permalink/01VIC_INST/1ohem39/cdi_pascalfrancis_primary_6077627

Karwowski N., Rinner K. (2020) French Connections – Examining the Residential Clustering and Dispersion of Francophones in the Toronto Area, REAL CORP 2020 Proceedings/Tagungsband 15-18 September 2020, https://repository.corp.at/686/

Li, L., Jaing, Z., Duan, N., Dong, W., Hu, K., Sun, W (2012) Chapter 8 - An Approach to Optimize Police Patrol Activities Based on the Spatial Pattern of Crime Hotspots, Service Science, Management, and Engineering: 2012, Pages 141-163 https://www.sciencedirect.com/science/article/abs/pii/B9780123970374000089

Ross, N.A., Houle, C., Dunn, J.R. and Aye, M. (2004), Dimensions and dynamics of residential segregation by income in urban Canada, 1991–1996. Canadian Geographer / Le Géographe canadien, 48: 433-445. https://onlinelibrary.wiley.com/doi/full/10.1111/j.0008-3658.2004.00069.x#
