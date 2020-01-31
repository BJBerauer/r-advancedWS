###################################################
###     ADVANCED DATA HANDLING AND VISUALISATION 
###           -WORKSHOP SCRIPT 2020-
### BonaRes-R-Group | Authors
###################################################

library(tidyverse) #load in the tidyverse (loading multiple packages belonging to the "tidyverse")
# library(magrittr)
library(lubridate)


getwd()#Check the working directory 
setwd() #set working directory to the folder you pasted all materials

###
# Hooking people up and bringing people to the same level
# ??Very much too basic??
#####

##Use R as calculator
2-4

4/2+4 

4/(2+4) #follows mathematical rules

2*4+4/pi #"pi" is a prestored object - what are objects?

##Create objects in R 
a <- 2
b <- 4

a-b
b/a+b
b/(a+b)
a*b+b/pi

d <- c(2,4,6) #vector - chain of multiple "entries"
e <- c(2,5,8,11)
d+e #warning = R is doing an action, but warns you to check whats going on | coercion: R repeats the shorter object 

f <- c("can","be","letters")
d+f #Error = R doesn't know how to execute the task as its of different classes/organization levels

#An Object is like a empty box were you can stuff in whatever you want. But there are different levels of organization of the box possible!

## Load in Data

df <- read.csv("Data/Land-use_for_WS.csv") #read in data | Take care of your working directory
str(df) #check the structure (what are the names of the columns? what is the format of the columns? is there sth odd happening?)
head(df, 5) #check the first 5 entries of the data-frame

df$Origin #adress with $ (the "accessor") and the name
df[, 3] #adress with the position within the dataframe
df[3] #adress with the position within the dataframe

  ###QUESTION: similar result but whats the difference?
  ###SOLUTION?
  #str(df[, 3]) #vector of a factor with 2 levels
  #str(df[3]) #data.frame
  ###

df[3,] #df[ROW,COLUMN]

df$Origin[4] #dimensions of the object need to be respected

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

###Dates & Time (with lubridate)
df$ts <- as.POSIXct(df$Datum, format = "%d.%m.%Y" ) #base function to transform dates (transform to whatever system is set!)
str(df)
#from Lubridate Package | extract months, day, year and more from a date
df$ts <- dmy(df$Datum) #dmy, ymd, mdy ... letters of command need to fit the format of your charactor or vector arrangement | not in posixCT format

df$monat <- month(df$ts)
df$jahr <- year(df$ts)
df$tag <- day(df$ts)
df$Quartal <- quarter(df$ts, with_year = TRUE) # Function:: quarter, extracts the quarter from your dates (from Lubridate Package) 

  ###QUESTION: What does "with_year" argument do?
  ###ANSWER: Also keeps the year

df$DOY <- yday(df$ts) # Function:: yday, extracts the day of year from your dates (from Lubridate Package)

ts_diff <- int_diff(unique(df$ts))
str(ts_diff)
str(ddays(x = 1))
int_length(ts_diff)/ddays(x = 1) #Error - format don't match
int_length(ts_diff)/86400
###Not familiar with package!

#####
#TIDYVERSE
#####

df <- mutate(df, CN = C / N, NP = N / (P/10))

  ###QUESTION: Also calculate total C and N
  ###SOLUTION: df <- mutate(df, totC = C * Biomass, totN = N * Biomass)

filter(df, Transplant == c("EB", "FE")) #analog operation to subset
filter(df, Transplant %in% c("EB", "FE")) #analog operation to subset
  ###subset(df, df$Transplant %in% c("EB", "FE"))
filter(df, Transplant %in% c("EB", "FE") & jahr ==2018) #logical operators can be chained as normal

select(df, c(Origin, Biomass))

#filter is for rows (subsetting a dataframe with logical conditions rows have to meet)
#select is for columns (just picking specific columns out of a dataframe)

#The pipe: %>% - chaining of multiple operations

handy_df <- df %>% select(Origin, Transplant, Treatment, Biomass, N, ts) %>%
                   filter(Biomass>=80) %>%
                   mutate(totN = N * Biomass)

#we can pipe (or chain) multiple operations after each other without retyping a lot of stuff and it also looks nice and clean!
#Tidyverse-packages know where to look for the variables (in the output of the previous pipe-operation!)

#A simple Base R operation for the same "pipe" as above
clunky_df <- df[df$Biomass>=80 ,c("Origin", "Transplant", "Treatment", "Biomass", "N", "ts")]
clunky_df$totN <- clunky_df$N * clunky_df$Biomass

###Summarize variables
lapply(df[df$Origin=="EB" & df$jahr %in% c(2017,2018), c(9,20:21)], mean, na.rm = T) #clunky base R version

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize(sumBM = sum(Biomass), sumN = sum(totN), meanN = mean(N)) #you can put mutliple formulas in here, of different types, complexity etc.

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize(sumBM = sum(Biomass), sumN = sum(totN, na.rm = TRUE), meanN = mean(N, na.rm = TRUE)) #use formula arguments as normal


#same operations for multiple variables can also be done!
df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize_at(., vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE))) #also works with multiple formulas!

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize_at(., vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = T))) #also works with multiple formulas!

  ###QUESTION: I don't know who the suggested list works, besides this one? Oo
  #df %>% 
    #filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
    #summarize_at(., vars(C, N, P, CN, NP), list(~mean(., na.rm = TRUE))) 

#This is allready way quicker, shorter and readable than base R soultions! But the output is somehow questionable - all different climate change treatments are lumped together
#Include grouping now makes it even more efficient and produces a nice output


df_EB <- df %>%
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  group_by(Transplant, jahr) %>%
  summarize_at(vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = T))) #also works with multiple formulas!

str(df_EB) #format is typical Tidyverse format. So far it was a Data.frame now its a tibble

#Advantages of a tibble
#1. visual
df_EB #visually easier to get
as.data.frame(df_EB) #comparing to the data frame 

#2. subsets of tibbles stay tibbles (so a numeric vector is stored as a "special kind of data frame" needed for the Tidyverse operations)
class(df[ ,9]) #class of biomass -> numeric
class(as_tibble(df[ ,9])) #class is a tibble

#3. tibbles can store complex entries (behaves a little like a list)


###
#GGPLOT Visualization!
###


###Purrr - lists
getwd()
rm(sm_list)
txt_files <-  list.files(path = "Data/SoilMoisture", full.names = TRUE) #!Take care of the workingdirectory pathway! - Full..names pastes the path(expansion) before the name
  #test <- paste0("Data/SoilMoisture/",txt_files) #paste0 is equivalent to paste(., ., sep="")
sm_list <-  lapply(txt_files, read.csv2,   header=TRUE)
sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_all=rowMeans(x[ ,c(2:6)], na.rm = TRUE))) #calculates rowMeans of all and binds it to each df in list
sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_15=rowMeans(x[ ,c(3,6)], na.rm = TRUE))) #calculates rowMeans of 5cm depth and binds it to each df in list | Port 2 & 5 are 15 cm (information by Marcus)
sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_5=rowMeans(x[ ,c(2,4,5)], na.rm = TRUE))) #calculates rowMeans of 15cm depth and binds  it to each df in list | port 1,3,4 are 5 cm (information by Marcus)


#purrr ------ NO CLUE
test <- map(sm_list, function(x) x$Site <- print(colnames(x)[1]))
test <- map(sm_list, function(x) rowMeans(x[ ,c(2:6)], na.rm=T))
test <- map_dfr(sm_list, `[`, c(1:6))
test <- map_dfr(sm_list, extract, c(1:6))

map(sm_list, `[`,c("SM2", "SM5")) %>%
  mutate(SM_mean_15 = rowMeans(.,na.rm=TRUE))
#-----RLY NO CLUE!


sm_list <- lapply(sm_list, 
                  function(x) cbind(x, 
                                    setNames(x[7], nm=paste(colnames(x)[1],"SM_all", sep = "_")))
)

sm_list <- lapply(sm_list, 
                  function(x) cbind(x, 
                                    setNames(x[8], nm=paste(colnames(x)[1],"SM_015", sep = "_")))
)

sm_list <- lapply(sm_list, 
                  function(x) cbind(x, 
                                    setNames(x[9], nm=paste(colnames(x)[1],"SM_005", sep = "_")))
)


sm_list <- lapply(sm_list, function(x) cbind(x, Date= as.POSIXct(x[ ,1], format = "%d.%m.%Y")))

sm_little <- lapply(sm_list, function(x) x[ ,10:13])

SM_gather <- Reduce(function(x,y) merge(x,y,by="Date", all=TRUE),sm_little)


#####
### LOOPS


#####
### FUNCTIONS
#####
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}