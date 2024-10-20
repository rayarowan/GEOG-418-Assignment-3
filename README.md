# GEOG 418 Assignment 3 - Spatial Autocorrelation Tutorial

By Raya Rowan

October 19, 2024

## Introduction

The objective of this tutorial is to provide visuals to aid in the understanding of spatial variability in the median total income and percentage of residents with french language knowledge in Halifax, Nova Scotia. To conduct the following research, spatial autocorrelation (SAC) techniques will be conducted. Spatial autocorrelation is a measure of the correlation of the provided data (in this case income and french language knowledge) with itself across space, calculated using Morans I. The output is either a positive or negative SAC. Positive SAC tell us that samples at nearby locations have similar data values that are not independent from one another (Dormann et al., 2007). Such results are visually interpreted as being clustered (Griffith, 2017). Negative SAC is when the values close by are dissimilar (Dormann et al., 2007). These results are viewed as dispersed (Griffith, 2017). 

As stated above, the census data sets being used are the median total income and percentage of respondents with french language knowledge. The median total income census data contains information about the population of Halifax's median total income. The percentage of respondents with french language knowledge census data contains information about the percent of the population of Halifax that speak french. The two data sets act as appropriate applications for testing for spatial autocorrelation because of their spatial variability and dependence (Education.nationalgeographic.org, n.d.). Both types of data are prone to great spatial variability across different neighbourhoods in Halifax. Possible correlations can also be drawn between the two variables such as if knowledge of french language is linked to higher or lower income. For example, knowing how to speak french may be of value for a specific job/career and result in higher income. Using visual aids such as maps that show census data, weight matrices, and plotting the Global and Local Moran's I in scatterplots to show the relationships between each location and its mean.

When a package is installed using the function intsall.packages("package") it is stored in a directory called the library (https://www.datacamp.com/doc/r/packages, 2024). These libraries are collections of pre-written code used to complete specific tasks while still maintaining control over R's flow (Woke, 2023). They are beneficial as they act like a short cut, reducing the amount of code needed to be written and can be called upon in the script when needed (Woke, 2023). To install a library we use the code library("package"). After installation of the library R will contain documentation specifying proper syntax and use for that package. This information can be found under Packages in the panel on the bottom right side of the page. 

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
install.packages("knitr")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("e1071")

#Load in libraries:
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("e1071")
```

The first step is to set the working directory. This will be the folder or location on your computer that R pulls files from and saves your work to. Do set the working directory use the code dir <- "name of folder/location" followed by setwd(dir). An example of this can be viewed below.

After this, you will want to read the shape file for the census boundaries into ‘R’ as an st data frame and then bring in the csv file as a data frame. The shape file (shp) contains the location of geographic features such as points, lines and polygons. The csv file consists of text and number values that represent the information gathered. Together the shape file and csv file make the municipality boundary.

The shape file was then transformed to specify Halifax, Nova Scotia as the location boundary using the st_transform function and specifying the coordinate reference system. 

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
dir <- "~/Desktop/GEOG 418/Assignment3_Data"
setwd(dir)

#From the working dir read in the csv
csv <- read.csv("./ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file
shp <- st_read("./lda_000a16a_e.shp") 

#Change the CRS to Halifax's CRS (EPSG:32620)
data_halifax <- st_transform(shp, crs = 32620)

st_crs(data_halifax)
```

Next, we want to clean up our data and make it easier to use. To understand the columns that correspond to our data we will create a vector of the column names. Using our spatial polygon data frame, any unwanted rows such as those in the csv file without exactly 8 numbers will be removed using the csv_clean function. We then merge these results with our spatial polygon data frame to add the data to the shape file. After this, we will subset to only the city of Halifax for the analysis. Lastly, any absolute count data is turned into a rate to calculate, map and analyze the percentage of french speaking individuals.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}


#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(data_halifax, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Halifax
Municp <- subset(census_DAs, census_DAs$CMANAME == "Halifax")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

The final step before analyzing the data is to remove any polygon that contains a value of "NA" for both the median total income or knowledge of french. This is to ensure the results we end up analyzing are accurate as having polygons with missing data in the form of "NA" or 0 values can change the resultant data. By removing such polygons we are only looking at polygons that contain relevant values. 

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

We will now take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with french language knowledge. We will look at some descriptive stats by calculating the mean, standard deviation and skewness of each variable. Then we will do a final check for NA values in the data.

This data will be displayed in a table using the data.frame and kable functions. 

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
<img width="414" alt="Screenshot 2024-10-19 at 6 50 01 PM" src="https://github.com/user-attachments/assets/2ba8feaf-7595-452a-a8d7-92524e57c144">

Two maps will now be created, one for median income and another for percentage of respondants with knowledge of french. First, we want to specify the spatial data object and create a tmap element using the 'tmap' package and the tm_shape() function which specifies the shape, projection and bounding box (Tennekes, 2018). The tmap element is a building block for drawing thematic maps (Tennekes, 2018). For example, for the median income map we are specifying the spatial data as the median total income (Income_noNA). Using this data a tmap element is projected onto the map as polygon layers with borders (Tennekes, 2018). This is because we also specified we want to use polygons when using the tm_polygon function. Within this function we can also include the data colour, title, style, palette (chosen using tmaptools as described in the code below), transparency of the border, and background map colour. Finally, to achieve a desired map layout we use the tm_layout function which specifies the position of the legend.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Halifax census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "RdBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

<img width="694" alt="Screenshot 2024-10-19 at 6 50 53 PM" src="https://github.com/user-attachments/assets/89a7a96a-cbd0-4381-91bd-cc564d9ad450">

## Neighbourhood matrix

Provided that a spatial neighbourhood refers to the spatial proximity of one location to another within a spatial dataset, a weighted neighbourhood matrix can be thought of as weights that spatially connect these neighbourhoods (Moraga, n.d.). It does this by assigning weights to the relationships between the locations or polygons indicating how strong the spatial connection is (Moraga, n.d.). More weight will be assigned to entries corresponding to close areas than those corresponding to areas that are farther apart (Moraga, n.d.). To define the weighted neighbourhoods matrices we are using Queen's and Rook's weights which define the spatial relationships between polygons (neighbours) based on how a queen or rook would be moved on a chess board. The Queen can move in all directions whereas the Rook can only move forward, backward or sideways but not diagonally.

The code to create a list of neighbours in R is very simple thanks to the poly2nb() function in the ‘spdep’ package. If we want to change from default queen weighting to rook weighting in our selection, we simply change the ‘queen = TRUE’ to ‘queen = FALSE’.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_geometry to get the coordinates of polygons
Income.net <- nb2lines(Income.nb, coords=st_geometry(st_centroid(Income_noNA)))
st_crs(Income.net) <- st_crs(Income_noNA)                      

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_geometry(st_centroid(Income_noNA)))
st_crs(Income.net2) <- st_crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_geometry(st_centroid(French_noNA)))
st_crs(French.net) <- st_crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_geometry(st_centroid(French_noNA)))
st_crs(French.net2) <- st_crs(French_noNA)

```

We are creating the maps below using the 'tmap' package to display the spatial neighbourhood relationships. The tm_shape() function is used to define the spatial polygons. The tm_borders() and tm_lines() functions are used to add borders and lines with specific colours and line widths. The tm_layout() function is used to add a title to each map and finally, tmap_arrange() is used to arrange the maps in a three-panel figure where ncol defines the number of columns and nrow the number of rows. The outputted maps display the spatial neighbourhood relationships for the dataset of Halifax using weighted Queen's and Rook's movements. On the left panel the map shows just the Queen's neighbourhoods, with blue lines connecting polygons that mirror how a queen would move in chess. On the middle map, the Rook's neighbourhoods are displayed, with red lines connecting polygons that mirror how a rook would move in chess. On the right panel the map shows both the Queen's and Rook's neighbours. Here we can identify any difference between the two. 

```{r Income neighbours map, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Halifax census dissemination areas showing median total income neighbours queens weight (left) rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='blue', lwd = 1) + tm_layout(title = "Queens")


#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = 1) + tm_layout(title = "Rooks")

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 1) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = 1) + tm_layout(title = "Queens and Rooks")

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```

<img width="691" alt="Screenshot 2024-10-19 at 6 52 50 PM" src="https://github.com/user-attachments/assets/2053c5d2-597d-4b0c-9ab6-cf6c5e65ef1c">


```{r French neighbours map, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Halifax census dissemination areas showing median total french speaking neighbours queens weight (left) rooks weight (middle) and the combination of the two (right)."}

#Make queens map
FrenchQueen <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net) + tm_lines(col='blue', lwd = 1) + tm_layout(title = "Queens")

#Make rooks map
FrenchRook <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net2) + tm_lines(col='red', lwd = 1) + tm_layout(title = "Rooks")

#Make combined map
FrenchBoth <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(French.net) + tm_lines(col='blue', lwd = 1) +
               tm_shape(French.net2) + tm_lines(col='red', lwd = 1) + tm_layout(title = "Queens and Rooks")

#Print maps in a three pane figure
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)

```

<img width="712" alt="Screenshot 2024-10-19 at 6 54 21 PM" src="https://github.com/user-attachments/assets/04cf1874-3eb0-4fe6-acb3-b520f64d68c3">

The code below is converting the neighbourhood relationships created using Queen's and Rook's movements into a spatial weighted matrix for both median total income and total french speaking datasets. It confirms that there are indeed values in the matrix.
In setting zero.policy = TRUE we are setting the weight value of any polygons with no neighbours to zero length. This is to avoid calculation errors. Since weights are defined by "style", we incorporate the style = function. The various styles include “B”, “W”, and “C”. In the output below each neighbour will be given equal weights that sum to 1. This is because we used "W" as the style which indicates the use of a row standardized weighting scheme. To actually create the the weights matrix the nb2listw function from the 'spdep' package is used. Applying this function to the vri.nb variable assigns weights to all the neighbourhood polygons.

```{r Final Queens weights matrix, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]
head(French.lw[["weights"]])[c(1:3)]
```

<img width="602" alt="Screenshot 2024-10-19 at 6 55 09 PM" src="https://github.com/user-attachments/assets/a3d0805d-d509-4a21-85e7-771cfee39b32">

```{r Final Rooks weights matrix, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw2 <- nb2listw(Income.nb2, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw2 <- nb2listw(French.nb2, zero.policy = TRUE, style = "W")
```

## Global Moran’s I

The next step in this tutorial is to calculate the Global Moran's I statistic. This is a measure
of overall spatial autocorrelation across a given area (Getis and Ord, 1992). It assesses the degree of similarity between neighbouring spatial units based on a given variable (Getis and Ord, 1992). In this tutorial the Global Moran's I will be determining if the neighbourhoods across Halifax are similar based on the total median income and french speaking knowledge variables. It is a global scale measurement because we are considering the entirety of Halifax (our dataset) in assessing spatial autocorrelation. A positive Moran's I value tells us that there is positive spatial autocorrelation which translates into a clustered distribution (Getis and Ord, 1992). A negative Moran's I value indicates negative spatial autocorrelation and a dispersed distribution, and a Moran's I value of exactly 0 means the data is randomly distributed (Getis and Ord, 1992).

Now that we have determined how to choose and weight our neighbours, we can mathematically calculate the Global Moran’s I statistic using the following equation:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Where $x$ is the variable being determined and (i) is our point of interest, $x_i$ is the value of the variable at (i). $x_j$ is a neighbout to $x_i$. ($x_i$ - $x$) displays how similar the point is to the mean and ($x_j$ - $x$) displays how similar the neighbour is to the mean. $W_{i,j}$ is the spatial weighting matrix (described above in this tutorial) both the differences are multiplied by. To find the Global Moran's I we sum the differences between the points and neighbours across the study area to measure covariance in the numberator. The denominator is used to standardize our values.

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]

#Create dataframe for display in table
data_global <- data.frame(Variable = c("Income", "French Language"),
                   Global = c(round(mIIncome,5), round(mIFrench,5)),
                   Expected = c(round(eIIncome,5), round(eIFrench,5)),
                   Variance = c(round(varIncome,5), round(varFrench,5)))

#Produce table
kable(data_global, caption = paste0("Global Moran's I, expected I and variance for selected ", 2016, " census variables"))
```

<img width="441" alt="Screenshot 2024-10-19 at 6 56 55 PM" src="https://github.com/user-attachments/assets/5d22a7bd-b7ca-4689-962c-8881932834f4">

Since the Global Moran's I value for median total income is slightly positive, we can conclude that the median total income for neighbourhoods in Halifax have moderate spatial autocorrelation. This means that data nearby is more similar than that farther away and we can expect to see a slightly clustered distribution. The Expected Moran's I is the value we would expect to achieve if the total median income was distributed randomly across Halifax. This value is usually slightly negative, as displayed by our results. In this case we would see no spatial autocorrelation. Variance is used to calculate the z-value for the given dataset. When variance increases there is more variability between the neighbours and our data is less likely to be confidentially clustered. Since the variance is so small it is safe to assume that the Moran's I values are accurate and consistent across the Halifax study area. Hence, the total median income tends to cluster in certain areas and does not vary greatly. 

Similar results are observed for the percentage of the french speaking residents of Halifax. Although the values are different the same explanation applies. 

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
rangeIncome <- moran.range(Income.lw)
minRangeIncome <- rangeIncome[1]
maxRangeIncome <- rangeIncome[2]

#Calculate the range for the French variable
rangeFrench <- moran.range(French.lw)
minRangeFrench <- rangeFrench[1]
maxRangeFrench <- rangeFrench[2]

#Create dataframe for display in table
data_range <- data.frame(Variable = c("Income", "French Language"),
                   Minimum = c(round(minRangeIncome,5), round(minRangeFrench,5)),
                   Maximum = c(round(maxRangeIncome,5), round(maxRangeFrench,5)))
                   

#Produce table
kable(data_range, caption = paste0("Minimum and maximum range of Global Moran's I for selected ", 2016, " census variables"))
```

<img width="455" alt="Screenshot 2024-10-19 at 6 57 42 PM" src="https://github.com/user-attachments/assets/0fb028dc-2066-4c14-b3a0-8e65d6376d6f">

The range indicates the difference between the maximum and minimum values of the Global Moran's I calculation. These values typically falls between +1 and -1, representing spatial clustering and dispersion. In this tutorial, it tells us that some areas in Halifax will display spatial clustering and others dispersion since the lowest values for both variables falls below 0 and the highest above. 

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is randomly distributed, and the alternate hypothesis is the data is spatially autocorrelated. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say that we fail to reject the null hypothesis and the data has no autocorrelation. A value greater than +1.96 would imply the data is spatially clustered, and a value less than -1.96 would imply the data is spatially dispersed.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))

#Create dataframe for display in table
data_z <- data.frame(Variable = c("Income", "French Language"),
                   ZScores = c(round(zIncome,2), round(zFrench,2)))
                  
                  
#Produce table
kable(data_z, caption = paste0("Z-test results for selected ", 2016, " census variables"))
```

<img width="455" alt="Screenshot 2024-10-19 at 7 00 00 PM" src="https://github.com/user-attachments/assets/e5718131-8d11-4975-9bd6-32c335870ddd">

The zscores for both variable confirm that the data is not random, it is significantly clustered. This is because both zscores are significantly greater than +1.96 implying clustering.

## Local Moran's I

Local Moran's I can be thought of as the localized version of Global Moran's I (O’sullivan and Unwin, 2010). Rather than focusing on the entire dataset, the Local Moran's I focuses on individual polygons, calculating the value for one polygon at a time and comparing it with the value of its neighbours (O’sullivan and Unwin, 2010). This provides an indication of areas where data clusters and disperses at a local scale (O’sullivan and Unwin, 2010). Like Global Moran's I, positive values of Local Moran's I indicates positive spatial autocorrelation and similarity of values nearby one another leading to clustering. Negative values indicate negative spatial autocorrelation and dissimilarity between values nearby leading to dispersion. Local Moran's I is useful as it is associated with a set of tools known as Local Indicators of Spatial Association (LISA) (O’sullivan and Unwin, 2010). LISA, as the name implies, is used to detect local patterns in data (O’sullivan and Unwin, 2010).

The equation used to calculate Local Moran's I is very similar to that of Global Moran's I. It uses many of the same features but arranges them in a different way.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

The localmoran() function is used here to the perform the calculations and obtain LISA values for both total median income and total french speaking residents in Halifax. For the calculation we are required to input our variable and weighting scheme calculated prior in this tutorial.

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for French
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

To visualize and better understand what this test is doing we will now create two maps showing the Local Moran I results for both median total income and total percentage of french speaking residents. The maps will display Halifax as our study area and show individual polygons where we would expect to find specific ranges of Local Moran I values that indicate either positive or negative spatial autocorrelation. Since this is statistical technique works on a local scale it is significantly easier to display our results using a visual such as a map. This is because it analyzes each polygon so by using tables like we did for the Global Moran's I would require us to produce a table per polygon... which is a lot of tables and numbers. 


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Halifax census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

<img width="795" alt="Screenshot 2024-10-19 at 7 01 51 PM" src="https://github.com/user-attachments/assets/1e6f9252-9f16-4c9a-b0ac-2170f675033a">

The maps above allow us to decipher between the census tracts with higher vs lower Local Moran I Z scores. The red polygons display high, positive Z values that fall outside of the 95% confidence interval on the positive end (>1.96) and indicate clustering of data. The blue polygons display low, negative Z values that fall outside of the 95% confidence interval on the negative end (<1.96), indicating dispersion of data in these areas. Polygons with values in between -1.96 and +1.96 are coloured white. These census tracts would be considered to have randomly distributed data and the Z values are within the 95% confidence interval and thus the null hypothesis (data is randomly distributed) can not be rejected. 

Graphing these trends can also be quite useful in understanding our results. Using scatter plots we can visualize the relationship between each location and its mean. 

Below is an example of scatter plots displaying spatial lag of median total income and percentage of residents with french language knowledge in Halifax. The black line in each graph is a best-fit line through the points. According to Babish (1998), spatially lagged data refers to data where the values of a variable at one location are influenced or related to the values at nearby locations. 

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

<img width="795" alt="Screenshot 2024-10-19 at 7 02 33 PM" src="https://github.com/user-attachments/assets/64c3ab20-2d98-4933-927c-c4dd5bbd74b5">

```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

<img width="759" alt="Screenshot 2024-10-19 at 7 03 12 PM" src="https://github.com/user-attachments/assets/cb2ef93a-a114-4d72-93b3-e52c70ee74aa">

In these plots, the points with diamonds are considered statistically significant at a 95% confidence interval, and the regression line shows the overall trend. The points with diamonds are far from the regression line meaning they are outside the 95% confidence interval and are likely to heavily influence the analysis. For both plots we can see that the trend shows positive spatial autocorrelation as the regression lines on each graph have positive slopes. The points that fall in the lower left corner of the plot represent locations where the attribute at (i) ($x_i$) and neighbour ($x_j$) are below the mean ($x$).

## Summary

In summary, the median total income and percentage of residents with french language knowledge are both spatially autocorrelated and display data that is clustered across Halifax, Nova Scotia. This conclusion is supported by the results of this tutorial. In using various functions provided by packages installed at the beginning of the tutorial, we were able to perform calculations generating results for Global Moran's I, expected I, variance, range and Local Moran's I as well as create maps of Halifax displaying our census data, weight matrices (using Queen's and Rook's weights) and Local Moran's I. Furthermore, this tutorial was successful in demonstrating how to perform coding techniques in R to provide visuals such as tables and maps and utilize packages and functions. Further testing should be considered to determine the overlap of these variables and the influence they may have on one another. An advanced spatial statistics such as Geographically Weighted Regression could be conducted in this case and further conclusions may be drawn from such data.

## References

Babish, G. (1998). Geostatistics Without Tears. Environment Canada.

F. Dormann, C., M. McPherson, J., B. Araújo, M., Bivand, R., Bolliger, J., Carl, G., G. Davies, R., Hirzel, A., Jetz, W., Daniel Kissling, W., Kühn, I., Ohlemüller, R., R. Peres-Neto, P., Reineking, B., Schröder, B., M. Schurr, F., & Wilson, R. (2007). Methods to account for spatial autocorrelation in the analysis of species distributional data: a review. Ecography, 30(5), 609–628. https://doi.org/10.1111/j.2007.0906-7590.05171.x

Getis, A., & Ord, J. K. (1992). The Analysis of Spatial Association by Use of Distance Statistics. Geographical Analysis, 24(3), 189–206. https://doi.org/10.1111/j.1538-4632.1992.tb00261.x

Griffith, D. (2017). Spatial Autocorrelation. Geographic Information Science & Technology Body of Knowledge, 2017(Q4). https://doi.org/10.22224/gistbok/2017.4.13

Moraga, P. (n.d.). Chapter 7 Spatial neighborhood matrices | Spatial Statistics for Data Science: Theory and Practice with R. In www.paulamoraga.com. https://www.paulamoraga.com/book-spatial/spatial-neighborhood-matrices.html

O’sullivan, D., & D Unwin. (2010). Geographic information analysis. John Wiley & Sons.

R Packages. (2024). DataCamp. https://www.datacamp.com/doc/r/packages

Tennekes, M. (2018). tmap: Thematic Maps in R. Journal of Statistical Software, 84(6). https://doi.org/10.18637/jss.v084.i06

What Is a Census? (n.d.). Education.nationalgeographic.org. https://education.nationalgeographic.org/resource/census-article/

Woke, G. (2023, March 24). The difference between libraries and frameworks. Simple Talk. https://www.red-gate.com/simple-talk/development/other-development/the-difference-between-libraries-and-frameworks/

