###################################################
###     ADVANCED DATA HANDLING AND VISUALISATION 
###           -WORKSHOP SCRIPT 2020-
### BonaRes-R-Group | Authors
###################################################

library(tidyverse) #load in the tidyverse (loading multiple packages belonging to the "tidyverse")
library(data.table)
getwd()
###
# Hooking people up and bringing people to the same level
# ??Very much too basic??
###

#Use R as calculator
2-4

4/2+4 

4/(2+4) #follows mathematical rules

2*4+4/pi #"pi" is a prestored object - what are objects?

#Create objects in R
a <- 2
b <- 4

a-b
b/a+b
b/(a+b)
a*b+b/pi

d <- c(2,4,6) #vector - chain of multiple "entries"
e <- c(2,5,8,11)
d+e

f <- c("can","be","letters")

# Load in Data
# !Starting from here might be ok as well

df <- read.csv("./Data/Land-use_for_WS.csv") #read in data
str(df) #check the structure (what are the names of the columns? what is the format of the columns? is there sth odd happening?)
head(df, 5) #check the first 5 entries of the data-frame

df$Origin #adress with $ (the "accessor") and the name
df[, 3] #adress with the position within the dataframe
df[3] #adress with the position within the dataframe

  ###QUESTION: similar result but whats the difference?
  ###SOLUTION To ASK?
  #str(df[, 3]) #vector of a factor with 2 levels
  #str(df[3]) #data.frame
  ###

df[3,] #df[ROW,COLUMN]

#logical operators (there are plenty - show list in powerpoint?)
df$Biomass>70
df[ ,9]>70
df[9]>70

df$Origin=="EB" #also work for "character" strings
df$Origin!="EB"#also work for "character" strings


#use logical operators to subset
df_EB <- df[df$Origin=="EB", ]
df_EB2 <- subset(df, Origin=="EB")

sum(df_EB$Biomass) # build sum of the subsetted dataframe
sum(df$Biomass[df$Origin=="EB"]) # also works with "chained subset"
sum(df_EB$Biomass) == sum(df$Biomass[df$Origin=="EB"])

#calculate C:N, N:P total C total N
df$CN <- df$C / df$N #divide two vectors of the same length
df$NP <- df$N / (df$P/10) #similar as above but P is in permille and N in percent -> follow mathematical operations and transform P first

  ###QUESTION: Calculate total C and total N as product of %element * biomass
  #df$totC <- df$C*df$Biomass
  #df$totN <- df$N*df$Biomass
  ###

#look again into structure | newly calculated variables are there
str(df) #but date remains a factor

df$ts <- as.POSIXct(df$Datum, format = "%d.%m.%Y" ) #base function to transform dates (transform to whatever system is set!)

#create a date , month, year, DOY using the "tidyverse" date thing lubridate (Paul?)

###
#TIDYVERSE
###

df <- mutate(df, CN = C / N, NP = N / (P/10))
filter(df, Transplant %in% c("EB", "FE"))
select(df, c(Origin, Biomass))
