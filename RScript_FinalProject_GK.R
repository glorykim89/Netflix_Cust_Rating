----------------------------------------------------------------------------
#1 Accessing and Transforming the Main Dataset (txt.files)
----------------------------------------------------------------------------
#1a-Set working directory
setwd("C:/Users/gchoi/Documents")

#1b-Read the data
library(data.table) #using the fread() function in order to read the data faster
t1 <- fread("c:\\R\\combined_data_1.txt",
            sep = "\t",
            header= FALSE,
            dec = ".",
            data.table = FALSE)
t2 <- fread("c:\\R\\combined_data_2.txt",
            sep = "\t",
            header= FALSE,
            dec = ".",
            data.table = FALSE)
t3 <- fread("c:\\R\\combined_data_3.txt",
            sep = "\t",
            header= FALSE,
            dec = ".",
            data.table = FALSE)
t4 <- fread("c:\\R\\combined_data_4.txt",
            sep = "\t",
            header= FALSE,
            dec = ".",
            data.table = FALSE)

nrow(t1)+nrow(t2)+nrow(t3)+nrow(t4)
# [1] 100498277

library(tidyr) #using the separate() function to break out columns
t1 <- separate(t1,col=1,c("CustomerID","Rating","Date_of_Rating"),
               sep=",",
               remove=TRUE,
               convert=TRUE,
               extra="warn")
t2 <- separate(t2,col=1,c("CustomerID","Rating","Date_of_Rating"),
               sep=",",
               remove=TRUE,
               convert=TRUE,
               extra="warn")
t3 <- separate(t3,col=1,c("CustomerID","Rating","Date_of_Rating"),
               sep=",",
               remove=TRUE,
               convert=TRUE,
               extra="warn")
t4 <- separate(t4,col=1,c("CustomerID","Rating","Date_of_Rating"),
               sep=",",
               remove=TRUE,
               convert=TRUE,
               extra="warn")

d1 <- rbind(t1,t2,t3,t4) #combines all 4 files
dim(d1)
# [1] 100498277         3

#1c-Initial checks
head(d1)
# CustomerID Rating Date_of_Rating
# 1         1:     NA           <NA>
# 2    1488844      3     2005-09-06
# 3     822109      5     2005-05-13
# 4     885013      4     2005-10-19
# 5      30878      4     2005-12-26
# 6     823519      3     2004-05-03
d1[24058264,]
# CustomerID Rating Date_of_Rating
# 24058264      4500:     NA           <NA>
summary(d1)
# CustomerID            Rating      Date_of_Rating    
# Length:100498277   Min.   :1.000   Length:100498277  
# Class :character   1st Qu.:3.000   Class :character  
# Mode  :character   Median :4.000   Mode  :character  
                    # Mean   :3.604                     
                    # 3rd Qu.:4.000                     
                    # Max.   :5.000                     
                    # NA's   :17770 
----------------------------------------------------------------------------
#2-Method to Remove Header Rows and Add Them as a Column "MovieID"
----------------------------------------------------------------------------
Key <- 1:nrow(d1)         
d2 <- cbind(Key,d1)
cutPoints <- subset(d2$Key,is.na(d1$Rating))
cutData <- cut(d2$Key,
               breaks=c(cutPoints,nrow(d1)),
               include.lowest=TRUE,
               right=FALSE)
head(cutData)
# [1] [1,549) [1,549) [1,549) [1,549) [1,549) [1,549)
# 17770 Levels: [1,549) [549,695) [695,2708) [2708,2851) [2851,3992) [3992,5012) ... [1.0049736e+08,1.0049828e+08]
tail(cutData)
# [1] [1.0049736e+08,1.0049828e+08] [1.0049736e+08,1.0049828e+08]
# [3] [1.0049736e+08,1.0049828e+08] [1.0049736e+08,1.0049828e+08]
# [5] [1.0049736e+08,1.0049828e+08] [1.0049736e+08,1.0049828e+08]
# 17770 Levels: [1,549) [549,695) [695,2708) [2708,2851) [2851,3992) [3992,5012) ... [1.0049736e+08,1.0049828e+08]
MovieID <- unclass(cutData)
d3 <- cbind(d2,MovieID)
dim(d3)
# [1] 100498277         5

#2a-Check that movie header ID aligns to MovieID
check <- subset(d3,is.na(d1$Rating))
dim(check)
# [1] 17770     5
tail(check) #header rows match the MovieID column
#              Key      CustomerID Rating Date_of_Rating MovieID
# 100488118 100488118     17765:     NA           <NA>   17765
# 100488432 100488432     17766:     NA           <NA>   17766
# 100489038 100489038     17767:     NA           <NA>   17767
# 100489243 100489243     17768:     NA           <NA>   17768
# 100490606 100490606     17769:     NA           <NA>   17769
# 100497356 100497356     17770:     NA           <NA>   17770

#2b-Remove NAs
nrow(d3)-nrow(check)
# [1] 100480507
d4 <- subset(d3,!is.na(d1$Rating))
dim(d4)
# [1] 100480507         5
head(d4)
#    Key  CustomerID Rating Date_of_Rating  MovieID
# 2   2    1488844      3     2005-09-06       1
# 3   3     822109      5     2005-05-13       1
# 4   4     885013      4     2005-10-19       1
# 5   5      30878      4     2005-12-26       1
# 6   6     823519      3     2004-05-03       1
# 7   7     893988      3     2005-11-17       1
----------------------------------------------------------------------------
#Accessing Feature Datasets 
----------------------------------------------------------------------------
#3a-Adding movie_titles dataset from the same source
mov <- fread("c:\\R\\movie_titles.csv",
            sep = ";",
            header=FALSE,
            data.table = FALSE)
mov <- separate(mov,col=1,c("MovieID","Year_Of_Release","Movie_Title"),
               sep=",",
               remove=TRUE,
               convert=TRUE,
               extra="merge")
head(mov)
#       MovieID    Year_Of_Release            Movie_Title
# 1        1            2003              Dinosaur Planet
# 2        2            2004   Isle of Man TT 2004 Review
# 3        3            1997                    Character
# 4        4            1994 Paula Abdul's Get Up & Dance
# 5        5            2004     The Rise and Fall of ECW
# 6        6            1997                         Sick


#3b-Adding netflix_titles from a different source
net <- fread("c:\\R\\netflix_titles.csv",
            fill = TRUE,
            sep = ",",
            header=TRUE,
            data.table = FALSE)
names(net)
# [1] "show_id"      "type"         "title"        "director"    
# [5] "cast"         "country"      "date_added"   "release_year"
# [9] "rating"       "duration"     "listed_in"    "description"
#eliminate TV shows
net <- subset(net,net$type == "Movie")
names(net)[3] <- "Movie_Title"
names(net)[8] <- "Year_Of_Release"

#3c-Merge all data 
m1 <- merge(net,mov,by=c("Movie_Title","Year_Of_Release"),all= FALSE)
m2 <- merge(d4,m1,by="MovieID",all= FALSE)
----------------------------------------------------------------------------
#4-Data Munging the Final Dataset
----------------------------------------------------------------------------
#4a-fixing columns
names(m2)
# [1] "MovieID"         "Key"             "CustomerID"      "Rating"         
# [5] "Date_of_Rating"  "Movie_Title"     "Year_Of_Release" "show_id"        
# [9] "type"            "director"        "cast"            "country"        
# [13] "date_added"      "rating"          "duration"        "listed_in"      
# [17] "description"  
df <- m2[,c(1,6,7,14,15,16,10,13,12,3,4)]
names(df)[4:9] <- c("Age_Rating","Movie_Duration","Genre","Director","Date_Added_Netflix","Listed_Country")
names(df)[11] <- "Cust_Rating"
df$Cust_RatingC <- as.factor(df$Cust_Rating)
names(df)
df$Age_Rating = as.factor(df$Age_Rating)
df$Genre = as.factor(df$Genre)
summary(df)

#4b-converting date columns from character to dates
df$Date_Added_Netflix <- strptime(df$Date_Added_Netflix, "%B %d, %Y")
df$Date_Added_Netflix <- as.Date(df$Date_Added_Netflix)

#4c-extracting integer from Movie_Duration column
split <- strsplit(df$Movie_Duration, "\\ ")
split[1]
# [[1]]
# [1] "128" "min"
split2 <- sapply(split, function(x){x[1]})
df$Movie_Duration <- as.integer(split2)
names(df)[5] <- "Movie_Duration_min"

#4d-Discretizing Movie_Duration_min and adding it as a new column
max(df$Movie_Duration_min)
Movie_Duration_Ranges_min <- cut(df$Movie_Duration_min, 
                   breaks=seq(0,240,by=30))
table(Movie_Duration_Ranges_min, useNA="ifany")
# Movie_Duration_Ranges_min
# (0,30]   (30,60]   (60,90]  (90,120] (120,150] (150,180] (180,210] (210,240] 
# 99      1890    739024   4602215   3118002    425399    183683      6085 
df$Movie_Duration_Ranges_min <- Movie_Duration_Ranges_min

#4e-final checks
dim(df)
# [1] 9076397      13
summary(df)
names(df)
# [1] "MovieID"                   "Movie_Title"               "Year_Of_Release"          
# [4] "Age_Rating"                "Movie_Duration_min"        "Genre"                    
# [7] "Director"                  "Date_Added_Netflix"        "Listed_Country"           
# [10] "Cust_Rating"           "Movie_Duration_Ranges_min"
head(df)
----------------------------------------------------------------------------
#5-Numeric Exploratory Data Analysis
----------------------------------------------------------------------------
#5a-Check range, number of levels and count/percentage of each level per feature variable
table(df$Age_Rating)
prop.table(table(df$Age_Rating))*100
# G           NR           PG        PG-13            R        TV-14         TV-G 
# 2.364341269  0.012119346 13.444597014 38.573345789 44.937016307  0.265920497  0.057192298 
# TV-MA        TV-PG        TV-Y7 
# 0.082885312  0.252611251  0.009970917 
  ### PG-13,R have the highest percentage

levels(df$Genre)
length(levels(df$Genre))
# [1] 92
prop.table(table(df$Genre))*100 
  ###hard to compare with so many levels...may need to plot

prop.table(table(df$Year_Of_Release))*100
# 1942         1954         1958         1960         1962         1965         1968 
# 0.001090741  0.056101557  0.066689458  0.139901329  0.100127837  0.207251842  0.719514583 
# 1969         1971         1972         1973         1974         1975         1976 
# 0.164206127  0.776376353  0.019842675  0.407529552  0.435492189  0.932649817  2.313770541 
# 1977         1978         1979         1980         1981         1982         1984 
# 0.015953467  1.133280089  0.844795573  0.447258973  0.618340075  0.470803558  1.819642750 
# 1985         1986         1987         1988         1989         1990         1991 
# 0.301463235  1.563098221  0.254891892  1.480697682  1.639571297  0.354843447  0.925257016 
# 1992         1993         1994         1995         1996         1997         1998 
# 1.691265818  3.676183402  2.733011789  1.769358480  3.140772710  7.513917692  4.611918143 
# 1999         2000         2001         2002         2003         2004         2005 
# 7.026080944  5.804450819  7.394321778 10.436266726 13.150956266  9.153279655  3.687773904 
  ### volume of movies are concencentrated in 1997-2004

prop.table(table(df$Movie_Duration_Ranges_min))*100
# (0,30]      (30,60]      (60,90]     (90,120]    (120,150]    (150,180]    (180,210] 
# 0.001090741  0.020823241  8.142261737 50.705307403 34.352860502  4.686870792  2.023743563 
# (210,240] 
# 0.067042021   
  ### most movies are between 90-150 min

length(unique(df$Director))
# [1] 227
  ### a huge variation

#5b-Comparing between feature variables
t5 <- as.data.frame(prop.table(table(df$Genre,df$Movie_Duration_Ranges_min))*100)
t6 <- subset(t5, prop.table(table(df$Genre,df$Movie_Duration_Ranges_min))*100 > 3)
# Var1      Var2     Freq
# 277                             Action & Adventure  (90,120] 5.395930
# 286                   Action & Adventure, Comedies  (90,120] 4.532140
# 291 Action & Adventure, Comedies, Sci-Fi & Fantasy  (90,120] 3.194252
# 369                             Action & Adventure (120,150] 3.873861
# 443                                         Dramas (120,150] 4.358778
# 451                              Dramas, Thrillers (120,150] 4.276796
dim(t5)[1]*dim(t5)[2]
# [1] 2208
sum(t6$Freq)
# [1] 25.63176
### 6 out of 2208 combinations carry 25.6% of the volume
min(t5$Freq)*100
# [1] 0
max(t5$Freq)*100
# [1] 539.593
median(t5$Freq)*100
# [1] 0
mean(t5$Freq)*100
# [1] 13.58696
  ### majority of the frequency is around 0 based on median/mean

#5d-Chi-squared statistical test
X1 <- chisq.test(df$Age_Rating,df$Cust_Rating)
X2 <- chisq.test(df$Movie_Duration_Ranges_min,df$Cust_Rating)
X3 <- chisq.test(df$Genre,df$Cust_Rating)
X4 <- chisq.test(df$Year_Of_Release,df$Cust_Rating)
X5 <- c(X1$p.value,X2$p.value,X3$p.value,X4$p.value)
# [1] 0 0 0 0
X5 < .05
# [1] TRUE TRUE TRUE TRUE
### low p-value that all predictors are dependent on the response variable, Cust_Rating
Y1 <- chisq.test(df$Age_Rating,df$Cust_RatingC)
Y2 <- chisq.test(df$Movie_Duration_Ranges_min,df$Cust_RatingC)
Y3 <- chisq.test(df$Genre,df$Cust_RatingC)
Y4 <- chisq.test(df$Year_Of_Release,df$Cust_RatingC)
Y5 <- c(Y1$p.value,Y2$p.value,Y3$p.value,Y4$p.value)
Y5
# [1] 0 0 0 0

#5e-Quantile
quantile(df$Year_Of_Release, probs=seq(0,1,0.25),na.rm=TRUE)
# 0%  25%  50%  75% 100% 
# 1942 1994 1999 2003 2005 
  ### 75% of the volume are customer ratings for movies 2003 and before
quantile(df$Cust_Rating, probs=seq(0,1,0.25),na.rm=TRUE)
# 0%  25%  50%  75% 100% 
# 1    3    4    4    5 

#5f-Correlations
cor(df[,c(3,5,11)])
                    # Year_Of_Release Movie_Duration_min Cust_Rating
# Year_Of_Release         1.00000000         0.03773258  -0.1158073
# Movie_Duration_min      0.03773258         1.00000000   0.1277536
# Cust_Rating            -0.11580729         0.12775362   1.0000000
----------------------------------------------------------------------------
#6-Exploratory Data Visualization
----------------------------------------------------------------------------
#6a-Histogram - distribution of Customer Rating
histObj_freq <- hist(df$Cust_Rating, freq=TRUE, col="blue")
histObj_den$counts
# [1]  349537       0       0       0  849509       0       0       0       0 2548891
# [11]       0       0       0       0 3182842       0       0       0       0 2145618
histObj_freq$breaks
# [1] 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8 4.0 4.2 4.4 4.6 4.8 5.0
histObj_freq3 <- hist(df$Year_Of_Release, freq=TRUE, col="blue")

#6b-Boxplot - distribution of Movie_Duration_min_range against Customer Rating
boxplot(df$Cust_Rating ~ df$Movie_Duration_Ranges_min, col="blue")

#6c-Density Plot (reference 5b) - distribution of Genre by Movie Duration against Customer Rating
r_den <- density(df$Cust_Rating) 
# x                y           
# Min.   :0.9182   Min.   :0.000000  
# 1st Qu.:1.9591   1st Qu.:0.000000  
# Median :3.0000   Median :0.000000  
# Mean   :3.0000   Mean   :0.240356  
# 3rd Qu.:4.0409   3rd Qu.:0.001177  
# Max.   :5.0818   Max.   :5.072280 
plot(r_den, lwd=3, col="blue", main = "Comparing Dens. Against Highest Freq. of Genre+Duration Combo")
prop1 <- density(df$Cust_Rating[which(df$Genre=="Action & Adventure" & df$Movie_Duration_Ranges_min=="(90,120]")])
lines(prop1, lwd=3, col="purple")     
prop2 <- density(df$Cust_Rating[which(df$Genre=="Action & Adventure, Comedies" & df$Movie_Duration_Ranges_min=="(90,120]")])
lines(prop2, lwd=3, col="orange")
prop3 <- density(df$Cust_Rating[which(df$Genre=="Dramas" & df$Movie_Duration_Ranges_min=="(120,150]")])
lines(prop3, lwd=3, col="green")     
legend(1,4,
       legend=c("All Observations","Action & Adventure 90-120 min","Action & Adventure,Comedies 90-120 min","Dramas 120-150 min"),
       col=c("blue","purple","orange","green"),
       pch=c(19,19),
       cex=c(.75,.75))

#6d-Scatterplot - distribution of Ratings by Year of Movie Release
#Create a random sample index of 1,000 rows
set.seed(1)
#reducing the dataset due to size
samp <- df[sample(nrow(df), 10000, replace=FALSE), ] 
plot(samp$Cust_Rating,samp$Year_Of_Release, pch=20, col="blue",
    main = "Scatterplot of Customer Ratings by Movie Release Year",
    xlab = "Customer Rating Scale",
    ylab = "Year Of Movie Release")


----------------------------------------------------------------------------
#7 - Machine Learning Algorithm
----------------------------------------------------------------------------  
#7a-Setting Train and Test Set
library(randomForest)
n <- nrow(samp)  # Number of observations
ntrain <- round(n*0.6)  # 60% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create an index
train_df <- samp[tindex,]   # Create training set
test_df <- samp[-tindex,]   # Create test set

#7b-Training randomForest to predict Customer Rating using all predictors
rf <- randomForest(Cust_Rating~Movie_Duration_min+Year_Of_Release+Age_Rating
                   ,data=train_df,ntree=500
                   ,mtry=2
                   ,importance=TRUE)
#Customer Rating as Categorical
rf2 <- randomForest(Cust_RatingC~Movie_Duration_min+Year_Of_Release+Age_Rating
                   ,data=train_df,ntree=500
                   ,mtry=2
                   ,importance=TRUE)

#7c-Predicting test set Customer Rating values
prediction <- predict(rf, newdata=test_df, type="class")
head(prediction)
# 4835079  4732495  5779372  4548938  7245204  8504365 
# 4.253606 3.833511 4.565539 3.886492 3.613435 3.493525 
prediction2 <- predict(rf2, newdata=test_df, type="class")
head(prediction2)
# 4835079 4732495 5779372 4548938 7245204 8504365 
# 5       5       5       4       4       4 

#7d-Compare predicted vs. actual values of test set using "confusion matrix"
head(table(prediction, test_df$Cust_Rating))
# prediction         1 2 3 4 5
# 1.84504823137704 2 0 0 0 0
# 2.00684868955849 0 0 1 0 0
# 2.04679119563762 0 1 0 0 0
# 2.11975087945508 1 1 0 1 1
# 2.24828407386265 2 1 1 2 1
# 2.30073029017304 1 3 0 0 0
table(prediction2, test_df$Cust_RatingC)
# prediction2   1   2   3   4   5
# 1   3   3   2  10   4
# 2   3   2   2   5   2
# 3  63 166 439 381 150
# 4  61 163 548 762 455
# 5  18  41 130 270 317

#7e-Calculating the accuracy of the model
misclassification_error_rate <- sum(test_df$Cust_Rating != prediction) / 
  nrow(test_df)*100
misclassification_error_rate   # 8.3% is pretty good error rate
# [1] 100
misclassification_error_rate2 <- sum(test_df$Cust_RatingC != prediction2) / 
  nrow(test_df)*100
misclassification_error_rate2
# [1] 61.925

importance(rf)
varImpPlot(rf)
importance(rf2)
varImpPlot(rf2)
# 1         2         3        4        5 MeanDecreaseAccuracy MeanDecreaseGini
# Movie_Duration_min  4.191771 -14.39739 -0.709344 25.55392 62.06349             68.30486        207.22157
# Year_Of_Release     1.755777 -21.09497 -4.608545 29.75789 52.58765             69.86161        159.55472
# Age_Rating         -2.952084 -14.61173 -9.071096 32.15218 22.30146             42.20359         48.68017
varUsed(rf2, by.tree=FALSE, count=TRUE)
# [1] 48227 38968 11872

rf2$importance
# 1            2            3          4          5 MeanDecreaseAccuracy
# Movie_Duration_min  0.004152514 -0.011319570 -0.001185455 0.04537926 0.09838558           0.03867289
# Year_Of_Release     0.001619915 -0.017067957 -0.007982838 0.05518624 0.09593711           0.03897808
# Age_Rating         -0.002325911 -0.009448949 -0.013369872 0.04581859 0.03096957           0.01874033
# MeanDecreaseGini
# Movie_Duration_min        207.22157
# Year_Of_Release           159.55472
# Age_Rating                 48.68017

#Calculating train set error (RMSE)
train_mse = mean(as.numeric((prediction2 - train_df$Cust_RatingC)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
# [1] NA

print(rf2)
# OOB estimate of  error rate: 63.95%

