###################################################
###     ADVANCED DATA HANDLING AND VISUALISATION 
###           -WORKSHOP SCRIPT 2020-
### BonaRes-R-Group | Authors
###################################################

library(tidyverse) #load in the tidyverse (loading multiple packages belonging to the "tidyverse")
# library(magrittr)
library(lubridate)
library(rmarkdown)
library(ggforce)
library(nlme)
library(lsmeans)
library(lme4)
library(car)


getwd()#Check the working directory 
setwd() #set working directory to the folder you pasted all materials

####
# Baseline - Hooking people up and bringing people to the same level
#####

## Use R as calculator
2-4

4/2+4 

4/(2+4) #follows mathematical rules

2*4+4/pi #"pi" is a prestored object - what are objects?

## Create objects in R 
a <- 2
b <- 4

a-b
b/a+b
b/(a+b)
a*b+b/pi

d <- c(2,4,6) #vector - chain of multiple "entries" [elements]
e <- c(2,5,8,11)
d+e #warning = R is doing an action, but warns you to check whats going on | coercion: R repeats the shorter object 

f <- c("can","be","letters")
d+f #Error = R doesn't know how to execute the task as its of different classes/organization levels

#An Object is like a empty box were you can stuff in whatever you want. But there are different levels of organization of the box possible!
#there are manifold classes of objects (e.g. vector, matrix, data.frame, list, tbl,...) and types of elements (e.g. numeric, character, factor, double, integer,...) within an object.

## Load in Data

### Explain briefly the SUSALPS-Land-Use experiment

df <- read.csv("Data/Land-use_for_WS.csv") #read in data | Take care of your working directory
str(df) #check the structure (what are the names of the columns? what is the format of the columns? is there sth odd happening?)
head(df, 5) #check the first 5 entries of the data-frame
summary(df) #quick 7 number summary of dataframe

df$Origin #adress with $ (the "accessor") and the name [in a data.frame columns are "stored" as named vectors]
df[, 3] #adress an entire column with the position within the dataframe
df[3] #adress with the position within the dataframe

###QUESTION: similar result but whats the difference?





df[3,] #adresses an entire row (here: 3) 
df[3:8, ] #multiple rows (works also for columns in [ , ] format not with $)
df[3,5] # addresses a specific row & column | order is df[ROW,COLUMN]


df$Origin[4] #dimensions of the object need to be respected | gets important for example in lists

## Logical operators (there are plenty - show list in powerpoint?)
df$Biomass>70
df[ ,9]>70
df[9]>70

df$Origin=="EB" #also work for "character" strings
df$Origin!="EB"#also work for "character" strings


#use logical operators to subset
df_EB <- df[df$Origin=="EB", ]
df_EB2 <- subset(df, Origin=="EB") # alternative way for subsetting [in R there are always multiple ways for the same solution - often differing in packages they use or computational speed]

sum(df_EB$Biomass) # build sum of the subsetted dataframe
sum(df$Biomass[df$Origin=="EB"]) # you can also subset within an operation
sum(df_EB$Biomass) == sum(df$Biomass[df$Origin=="EB"])

sum(df$Biomass[df$Origin=="EB" & df$Transplant=="EB"]) #Biomass of the on-site installed climatic controls | also multiple logical operators can be chained together

#calculate C:N, N:P total C total N
df$CN <- df$C / df$N #divide two vectors of the same length
df$NP <- df$N / (df$P/10) #similar as above but P is in permille and N in percent -> follow mathematical operations and transform P first

###QUESTION: Calculate total C and total N as product of %element * biomass




#look again into structure | newly calculated variables are there
str(df) #but date remains a factor

###Dates & Time (with lubridate)
df$ts <- as.POSIXct(df$Datum, format = "%d.%m.%Y" ) #base function to transform dates (transform to whatever system is set!)
str(df)
#from Lubridate Package | extract months, day, year and more from a date
df$ts <- dmy(df$Datum) #dmy, ymd, mdy ... letters of command need to fit the format of your charactor or vector arrangement | not in posixCT format
str(df)

#QUESTION: Whats the difference?


df$monat <- month(df$ts)
df$jahr <- year(df$ts)
df$tag <- day(df$ts)
df$Quartal <- quarter(df$ts, with_year = TRUE) # Function:: quarter, extracts the quarter from your dates (from Lubridate Package) 

###QUESTION: What does "with_year" argument do?


df$DOY <- yday(df$ts) # Function:: yday, extracts the day of year from your dates (from Lubridate Package)

df_base <- df

#CHEATSHEET - but I am not familiar with the package
ts_diff <- int_diff(unique(df$ts))
str(ts_diff)
str(ddays(x = 1))
int_length(ts_diff)/ddays(x = 1) #Error - format don't match
int_length(ts_diff)/86400 # just manually extract the number of seconds per day to calculate difference between time steps
###Admit, I am not familiar with this package!

#####
#TIDYVERSE
#####
rm(df)
df <- read.csv("Data/Land-use_for_WS.csv")

df <- mutate(df, CN = C / N, NP = N / (P/10))

###QUESTION: Also calculate total C and N

###Re-run the time-stamp stuff from above
df$ts <- dmy(df$Datum) #dmy, ymd, mdy ... letters of command need to fit the format of your charactor or vector arrangement | not in posixCT format
df$monat <- month(df$ts)
df$jahr <- year(df$ts)
df$tag <- day(df$ts)
df$Quartal <- quarter(df$ts, with_year = TRUE) # Function:: quarter, extracts the quarter from your dates (from Lubridate Package) 
df$DOY <- yday(df$ts) # Function:: yday, extracts the day of year from your dates (from Lubridate Package)

identical(df, df_base) # to check: we have produced similar output


filter(df, Transplant == c("EB", "FE")) #analog operation to subset
filter(df, Transplant %in% c("EB", "FE")) #analog operation to subset | subset(df, df$Transplant %in% c("EB", "FE"))
filter(df, Transplant %in% c("EB", "FE") & jahr ==2018) #logical operators can be chained as normal

select(df, c(Origin, Biomass))

#filter is for rows (subsetting a dataframe with logical conditions rows have to meet)
#select is for columns (just picking specific columns out of a dataframe)

#A simple Base R operation for multiple subset conditions followed by an operation
clunky_df <- df[df$Biomass>=80 ,c("Origin", "Transplant", "Treatment", "Biomass", "N", "ts")]
clunky_df$totN <- clunky_df$N * clunky_df$Biomass


#The pipe: %>% - chaining of multiple operations
handy_df <- df %>% select(Origin, Transplant, Treatment, Biomass, N, ts) %>%
  filter(Biomass>=80) %>%
  mutate(totN = N * Biomass)

#we can pipe (or chain) multiple operations after each other without retyping a lot of stuff and it also looks nice and clean!
#Tidyverse-packages know where to look for the variables (in the output of the previous pipe-operation!)


###Summarize variables
# we have multiple cuts per year but are only intrested in the yearly (ecological meaningful) aggregates 
lapply(df[df$Origin=="EB" & df$jahr %in% c(2017,2018), c(9,13:14)], mean, na.rm = T) #clunky base R version

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize(meanBM = mean(Biomass), meanCN = mean(CN), meanNP = mean(NP)) 
###QUESTION: what went wrong?

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize(meanBM = mean(Biomass, na.rm = TRUE), meanCN = mean(CN, na.rm = TRUE), meanNP = mean(NP, na.rm = TRUE)) #use formula arguments as normal


df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize(sumBM = sum(Biomass), sumN = sum(totN, na.rm = TRUE), meanN = mean(N, na.rm = TRUE)) #you can put mutliple formulas in here, of different types, complexity etc.

#same operations for multiple variables can also be done!
###Explain the DOT (.)
df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize_at(., vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE))) 

df %>% 
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  summarize_at(., vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = T))) #also works with multiple formulas!


#Trying this with the Base R apply approach
lapply(df[df$Origin=="EB" & df$jahr %in% c(2017,2018), c(9,13:14)], c(mean, sd) , na.rm = T) #clunky base R version - doesnt work as easy...

test.func <- function(x){
  c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

lapply(df[df$Origin=="EB" & df$jahr %in% c(2017,2018), c(10:14)], test.func) #You need a helper function in between to make this happen in BaseR







#This is allready way quicker, shorter and readable than base R soultions! But the output is somehow (ecological) questionable - all different climate change treatments are lumped together
#Include grouping now makes it even more efficient and produces a nice output

df_EB <- df %>%
  filter(Origin == "EB" & jahr %in% c(2017,2018)) %>%
  group_by(Transplant, Treatment, jahr) %>%
  summarize_at(vars(C, N, P, CN, NP), funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = T))) #also works with multiple formulas!

str(df_EB) #format is typical Tidyverse format. So far it was a Data.frame now its a tibble

#Advantages of a tibble
#1. visual
df_EB #visually easier to get
as.data.frame(df_EB) #comparing to the data frame 

#2. subsets of tibbles stay tibbles (so a numeric vector is stored as a "special kind of data frame" needed for the Tidyverse operations)
class(df[ ,9]) #class of biomass -> numeric
class(as_tibble(df[ ,9])) #class is a tibble (besides tbl of a vector being somehow "non-sense")

#3. tibbles can store complex entries (behaves a little like a list)


###
#GGPLOT Visualization!
###
df_1718 <- filter(df, jahr %in% c(2017,2018))
df_1718$jahr2 <- as.factor(df_1718$jahr) #create jahr as.factor

p <- ggplot(df_1718, aes(x= jahr2, y= Biomass))

p + geom_point() 

p + geom_boxplot()

#geom_XX is the "geometrical" shape you want your plot to be

p + geom_point(color ="red") #color can be defined

p + geom_point(aes(color = Transplant)) #it can also be defined as one of the input variables (here a factor but can also be numeric)

p + geom_point(aes(color = Transplant, shape = Origin))

# everything gets layered on top of each other [dots are still there - see very top/bottom of the violins]
p + geom_point(aes(color = Transplant))+
  geom_violin()

###QUESTION: How to solve that everything is layered on top of each other?
###ANSWER: Either change the order
p +
  geom_violin()+
  geom_point(aes(color = Transplant))

###ANSWER: Or remove the fill of the geom_violine (set it to "none")
p +
  geom_point(aes(color = Transplant))+
  geom_violin(fill = "none") 


#aes is the aestetics. Can be set manually for each executed geom_XX
ggplot(df_1718, aes(x= jahr2, y= Biomass))+ #this line is actually "p" !
  geom_point(aes(color = Transplant))+
  geom_violin(aes(color = Transplant), fill = "none")

#or on the global level
ggplot(df_1718, aes(x= jahr2, y= Biomass, color = Transplant))+
  geom_point()+
  geom_violin(fill = "none")

#points are not sorted to the shapes of the violins -> we better use geom_dotplot
ggplot(df_1718, aes(x= jahr2, y= Biomass, color = Transplant))+
  geom_violin(fill = "none",
              position=position_dodge(1))+
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(1),
               dotsize = .5)

###QUESTION: Outline of the Dots is coloured by Transplant but not the fill. How to solve this?
###ANSWER: define the fill aes for geom_dotplot
p <- ggplot(df_1718, aes(x= jahr2, y= Biomass, color = Transplant))+
  geom_violin(fill = "none",
              position=position_dodge(1))+
  geom_dotplot(aes(fill = Transplant),
               binaxis='y', stackdir='center',
               position=position_dodge(1),
               dotsize = .5)

#changing colors 
p + scale_color_manual(values = c("grey30", "grey55", "grey80"),
                       breaks = c("EB", "FE", "GW"),
                       name = "What ever")


###QUESTION: How to change the fill?
###ANSWER: Set them manually. Works similar as for "color"
p + scale_fill_manual(values = c("grey30", "grey55", "grey80"),
                      breaks = c("EB", "FE", "GW"),
                      name = "What ever")

p + scale_fill_brewer(type = "div", 
                      palette = "Spectral")

#changing axis titles
p + labs(x= "Year", y= "Biomass [g/m²]")

#changing more complex axis parameters
p + scale_y_continuous(name = "Biomass [g/m²]", 
                       breaks = c(0,100,500,730,1000)) 

p + scale_y_continuous(name = "Biomass [g/m²]", 
                       breaks = seq(0,1000,100)) #you can also use a formula here 

p + scale_y_continuous(name = "Biomass [g/m²]", 
                       breaks = seq(0,1000,100),
                       expand = c(0,0)) #changes the start of the axis (removes the surplus below 0)



#very distracting background...but ggplot offers a variety of "background" plot settings called "theme"
p + theme_bw() 

p + theme_dark()

#My personal most liked theme and updated:
tsize <- 15; tsize2 <- 13; pchsize <- 1; lkey <- 3.5 # lsize <- 0.3

theme_set(theme_bw())
theme_update(
  plot.margin = unit(c(0.4,0.5,0.1,0), "lines"), #sets Margin of Plot
  panel.spacing = unit(0.1, "lines"), #sets Space between panels (by facet_wrap)
  panel.grid.minor = element_line(colour = NA), #removes grid lines
  panel.grid.major = element_line(colour = NA),  #removes grid lines
  panel.background = element_rect(fill = NA, colour = "black"),
  panel.border = element_rect(colour="black", size=0),
  plot.title = element_text(size = tsize, vjust = 0.5, hjust=0),
  axis.title.x = element_text(size = tsize, vjust = 0.35),
  axis.title.y = element_text(size = tsize, hjust = 0.5, vjust = 0.4, angle = 90),
  axis.text.x = element_text(size = tsize2, margin = margin(5 ,5 ,5 ,5, unit="pt")),
  axis.text.y = element_text(size = tsize2, margin = margin(5 ,5 ,5 ,5, unit="pt")),
  axis.ticks.length=unit(1, units="mm"),
  #axis.line = element_line(colour = "black", size = 1) #draws the line at the plot site
  legend.key = element_rect(colour = NA),
  legend.title = element_text(size = tsize2-1, hjust = 0),
  legend.text = element_text(size = tsize2-1, hjust = 0),
  legend.background = element_rect(colour=NA, size=0),
  legend.spacing = unit(0, "mm"),
  legend.key.size = unit(lkey,"mm"),
  legend.key.width = unit(lkey*1.5, "mm"),
  strip.background = element_rect(fill = NA , linetype=NULL, size=0, colour="white"),
  strip.text = element_text(size=tsize, vjust=0.7 , hjust= 0.5)
)

# Options and Opportunities are manifold and almost endless in tidyverse (and R)

#####
#Lists and automated functions
#by: Jessica Clayton
#####

#In this section you will learn how to automate data import and graph making by creating functions, using the map function and lists.  

# purrr::map

## Apply a function to each element of a vector

# create a vector of 5 integers
n <- c(6, 3, 7, 2, 10)


#TASK: You want to add 1 and substract 1 from each number

# vector for + and - 1
m <- c(1, -1)

#QUESTION: What happens when you try n + m?

n + m

#ANSWER:
#Essentially what happened was: 
#    n[1] + m[1], n[2] + m[2], n[3] + m[1], n[4] + m[2], n[5] + m[1]
#As m is a shorter vector than n, the elements of m get repeated. This is not the outcome we wanted. 


#The map functions in the purrr transform their input by applying a function to each element and returning a vector the same length as the input.
map(m, ~ n + .) # '.' is a placeholder (in this case for m)

#When the output is saved, R automatically saves both vectors within a list: 
x <- map(m, ~ n + .)
class(x) #it is a list
x[1] #vector - containing the sum of n+ (+1)
x[2] #vector - containing the sum of n+ (-1)




# . is again a placeholder (as in dplyr) | we use the created list x of "n+m"
x.1 <- map(x, ~ .^2) # square each element
x.2 <- map(x, ~ mean(.)) # calculate the mean of each vector


# Functions

#You can create a function to perform anything you like: 

# square a number
fun_sq <- function(x){
  x^2
}

fun_sq(4) # when x = 4

# for many numbers
fun_sq(n)

## Using functions in conjunction with map

y <- map(n, ~fun_sq(.)) # use the function you created | compare the output with fun_sq(n)
y.1 <- map(x, ~fun_sq(.))
identical(x.1, y.1) #output of little function is similar to the standard R math operation 

x.2 <- map(x, ~ mean(.)) # use existing functions such a mean 


####
##Automated data import
####

###LISTS
#Basic R lapply
files_names <-  list.files(path = "Data/SoilMoisture", full.names = TRUE) #!Take care of the workingdirectory pathway! - Full..names pastes the path(expansion) before the name
sm_list <-  lapply(files_names, read.csv2,   header=TRUE)

sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_all=rowMeans(x[ ,c(2:6)], na.rm = TRUE))) #calculates rowMeans of all and binds it to each df in list
sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_15=rowMeans(x[ ,c(3,6)], na.rm = TRUE))) #calculates rowMeans of 5cm depth and binds it to each df in list | Port 2 & 5 are 15 cm (information by Marcus)
sm_list <- lapply(sm_list, function(x)  cbind(x, SM_mean_5=rowMeans(x[ ,c(2,4,5)], na.rm = TRUE))) #calculates rowMeans of 15cm depth and binds  it to each df in list | port 1,3,4 are 5 cm (information by Marcus)


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

#create a new Date (posixCT format) column in each dataframe of the list
sm_list <- lapply(sm_list, function(x) cbind(x, Date = as.POSIXct(x[ ,1], format = "%d.%m.%Y")))

#Create a new list with dataframes only containing the relevant information (means and date)
sm_little <- lapply(sm_list, function(x) x[ ,10:13])

#merge the shortened dataframes together by date
SM_gather <- Reduce(function(x,y) merge(x,y,by="Date", all=TRUE),sm_little)


#purrr - Tidyvers: To merge all files into one dataframe: map_df from purrr
# Create a list with the names of all files you wish to import into your environment
files_names <- list.files(path = "Data/SoilMoisture",  pattern = "*.csv")

# (this only works if the tables have identical column names and dimensions)
All_data <- map_df(files_names, ~read.csv2(paste0("Data/SoilMoisture/" , .)))
head(All_data, 5)
#Check the result | Works but not senseful!

#We have to bring the input files into similar shape
rm(sm_list)

sm_list <- map(files_names, ~ read.csv2(paste0("Data/SoilMoisture/" , .)))
sm_list <- map(sm_list, ~mutate(., SiteID = paste0(colnames(.)[1]))) #creates a column with SiteID
sm_list <- map(sm_list, ~mutate(., Date = .[[1]])) #creates a new colum of Date (as I did'nt know how to rename it)
sm_list <- map(sm_list, ~select(., -1)) #deletes the first column (to get rid of it because column names I couldnt change)
names(sm_list) <- files_names
sm_list %>%
  names(.) %>%
  map( ~write.csv(sm_list[[.]], file = paste0("Jessica_clayton/Markdown_pdf/Soil_Moisture/", .), row.names = FALSE))


files_names <- list.files(path = "Jessica_clayton/Markdown_pdf/Soil_Moisture/",  pattern = "*.csv")
All_data <- map_df(files_names, ~read.csv(paste0("Jessica_clayton/Markdown_pdf/Soil_Moisture/" , .)))
str(All_data)

#after all having the same format we can bind them together


## Alternatively work using lists

#When your data are not identical in dimensions or file names, it might be better to import the data to a list.

# first define an empty list
My.list <- list()

# use the read.csv function with map to store each file as a separate data frame within your list
My.list <- map(files_names, ~read.csv(paste0("Jessica_clayton/Markdown_pdf/Soil_Moisture/" , .)))

# access each df from list 
My.list[[1]]

# name each dataframe with the file name
names(My.list) <- files_names

#My.list[["Filename1.csv"]]
My.list[["EB_EB_EX.csv"]]

####
##Data manipulation accross all dataframes in the list 
####
#Tidyr and dplyr are especially useful here

# E.g create a new column with changed date format
My.list.2 <- map(My.list, ~mutate(.x, New.Date = as.Date(Date,  format = "%d.%m.%Y"))) 
str(My.list.2[[1]])
# Delete a column , e.g. a comments column
My.list.2 <- map(My.list.2, ~ select(.x, -Date))

# Perform many operations using the pipe function

My.list.3 <- My.list %>% 
  map(., ~ mutate(.x, New.Date = as.Date(Date,  format = "%d.%m.%Y"))) %>% # Change date
  map(., ~ select(.x, -Date)) %>% # remove column
  map(., ~ mutate(.x, sm_all = rowMeans(.[1:5] , na.rm = TRUE),# calculate senseful means of soil moisture
                  sm_05 = rowMeans(.[c(1,3,4)] , na.rm = TRUE),
                  sm_15 = rowMeans(.[c(2,5)] , na.rm = TRUE)))


#####
#Loop - Soil Moisture Bucketmodel
#####
#bind the transformed dataframes together
moist <- My.list.3 %>%
  map_df(., ~cbind(.[-c(1:5)]))
str(moist)
moist$SiteID <- substr(moist$SiteID,1,8) #get rid of unneccessary "dt" at the end

moist <- separate(moist, SiteID, into = c("Transplant","Origin", "Treatment"), sep = "_", remove = TRUE) #seperate the SiteID code into Origin, Transplant and Treatment

moist <- moist %>%
  mutate_at(., vars(c(Origin, Transplant, Treatment)), funs(as.factor(.))) %>% #Transform the new generated "character" into "factor"
  mutate_at(., vars(c(sm_all, sm_05, sm_15)), funs(.*100)) #transform soilmoisture measurements into %

#Read in Soil characteristics
soil <- read.csv2("Data/SoilCharacteristics.csv") #SoilCharacteristics | Server->SUSALPS->Publications->LandUse->Data
soil <- soil[c(1:3), ] 
names(soil) #Bulkdensity [g/cm³], OragnicMatter (OM) kgC/m², SOC [%], pH [CaCl2], WiltingPoint (WP) [%], FieldCapacity (FC) [%]
colnames(soil) <- c("Site", "Origin", "Clay5", "Silt5", "Sand5", "Clay15", "Silt15", "Sand15", "Clay", "Silt", "Sand",
                    "Bulk.Density5", "Bulk.Density15", "Bulk.Density", "OM5", "OM15", "OM",
                    "SOC5", "SOC15", "SOC", "Type", "pH5", "pH15",  "pH",
                    "WP5","WP15", "WP", "FC5", "FC15","FC")

moist <- merge(moist, soil[ ,c(2,25:30)], by="Origin") #merge WiltingPoint and FiledCapacity from "soil characteristics" into soilmoisture
str(moist)

moist <- moist[ ,c(1:7, 10, 8, 9 ,13, 11, 12)] #reorder
str(moist)

###
#EXPLAIN WETNESS-INDEX
###

test <- moist

str(test)
test$Wet <- (test$sm_all - test$FC) / (test$WP - test$FC)

test %>% transmute_at(., vars(sm_all, sm_05, sm_15), funs(if_else(.<=FC, .== FC, .  <-  . )))
####need to get a small loop in here, get the code going for one parameter and then write the loop around it
moist$sm_all >= moist$FC #soils can't store more water then "field capacity" (its full at field capacity, the rest will run through)

#get fucking rid of the stupid NAs!
moist[is.na(moist)] <- NA

for(i in 5:7){
  for(j in 1:nrow(moist)){
    if (moist[j,i]>=moist[j,i+2*3]) moist[j,i] <- moist[j,i+2*3]
  }
}
moist <- test
moist[is.na(moist)] <- NA

for(i in 5:7){
  for(j in 1:nrow(moist)){
    if (is.na(moist[j,i]){
      moist[j,i] <- NA
    }
    else {
      moist[j, i]>=moist[j, i+2*3] moist[j,i] <- moist[j, i+2*3]
    }
  }
}

!is.na(moist[j,i])
str(moist)

if (moist[ ,5]>=moist[ ,5+2*3]) moist[ ,5] <- moist[ ,5+2*3]
(moist[ ,5]-moist[ ,5+3])/(moist[ ,5+2*3]-moist[ ,5+3])

for (i in 5:7) {
  tmp <- moist
  for (j in 1:nrow(tmp)) {
    if (tmp[j ,i]>=tmp[j ,i+2*3]) tmp[j ,i] <- tmp[ ,i+2*3]
    (tmp[ ,i]-tmp[ ,i+3])/(tmp[ ,i+2*3]-tmp[ ,i+3])
  }
}

tmp <- moist
if (tmp[ ,5]>=tmp[ ,5+2*3]) tmp[ ,5] <- tmp[ ,5+2*3]
(moist[ ,5]-moist[ ,5+3])/(moist[ ,5+2*3]-moist[ ,5+3])


###
# Automated Graphical functions
###

#For graph automation using functions, map and ggplot2

## A function for graphs with a common x axis

# define which columns you want to plot against x
My.data <- df_1718
vars <- names(My.data)[9:14]

# make a function with ggplot where y is the placeholder for your y variable:
fun1.1 <- function(y) { 
  My.data %>% 
    ggplot() + 
    aes(ts, get(y), col = Transplant) + #get(y) removes "" from the variable name
    geom_point() + 
    theme_bw() +
    xlab("Date") + 
    ylab(y) + # don't forget to label axis
    facet_wrap(~Origin)
}

# map passes each element of the vars vector through the graph function as y
# and thus creates a separate graph for each variable that you defined
map(vars, fun1.1)


## A function for subsetting data and creating graph for each treatment level

# create a vector for Treatment levels:
ST.filter <-  levels(My.data$Treatment)

# e.g. 
fun2 <- function(x) { 
  My.data %>% 
    filter(Treatment == x) %>% # x is the placeholder for the soil type level
    ggplot() + 
    aes(ts, CN , col = Transplant) + 
    geom_point() + 
    theme_bw() + 
    xlab("Date") + 
    ggtitle(x)+ # don't forget to make a title so you know which soil type is being shown
    facet_wrap(~Origin)
}
map(ST.filter, fun2)

## Save the graphs to your computer

#Use ggsave()

fun2 <- function(x) { 
  My.data %>% 
    filter(Treatment == x) %>% # x is the placeholder for the different treatments
    ggplot() + 
    aes(ts, CN , col = Transplant) + 
    geom_point() + 
    theme_bw() + 
    xlab("Date") + 
    ggtitle(x)+ # don't forget to make a title so you know which treatment is being shown
    facet_wrap(~Origin)
  
  # create a unique file name by using x as placeholder for treatment in the file name
  ggsave(paste0("Jessica_clayton/Markdown_pdf/Result_Graph/CN_", x, ".png"))   
}

map(ST.filter, fun2)



# More automated graph options: 

## Grids usings facet_wrap_paginate from ggforce

#Another way I use automation in my graphing is to quickly check if all the channels on the respiration machines are functioning correctly after I have done a sample run. There are roughly 40 respiration channels combined from two respiration machines (old machine and new machine). Sometimes the channels aren't working or there was an issue with the set up, but this can sometimes only be seen once we look directly at the respiration data. 


#Soil respiration rate (O2 µl/hr) was measured with the machines at hourly intervals over 24 hours. 
#The data provided is semi processed raw data from the respiration machines and contains the following information: Sample_ID, Date of run, Machine (old/new), Channel of the machine, Time (hrs) and Respiration rate (O2_muL_hr)

#I wanted a quick way to see the respiration rate for each channel at a glance. It would be too much information for one graph, so one graph per channel was preferable. Scrolling through up to 40 graphs at a time was also not ideal, so I needed to make plot with a grid of many individual plots. 

#As with previous codes, I created a function for my graph, this time I used the facet_wrap_paginate from ggforce to wrap the data to Channel, Date and Sample_ID level. I chose a 3 x 3 grid: 


#The facet_wrap_paginate function has a page number option. The default page = 1, shows only the first 9 graphs. To get the next 9, use page = 2, etc. 

#I used the following graph function with map to get all the pages I needed. 

#Here is the code: 

load(file = "Jessica_Clayton/Markdown_pdf/O2_data.RData")

graph_fun = function(x) {
  O2.data %>% 
    ggplot() + 
    aes(Time_hrs, O2_muL_hr, col = Machine) + 
    geom_point() + 
    geom_line() + 
    theme_bw() + 
    ylab(O[2]~(mu*l~ hr^-1)) +
    xlab("Incubation time (hrs)") +
    facet_wrap_paginate(vars(Date, Channel, Sample_ID), ncol = 3, nrow = 3, page = x)
  # note that x is the place holder for number of pages
}

# I know that there shouldn't be more than 5 pages worth of graphs
# so I set I map to use 1:5 in the graph function
map(1:2, ~graph_fun(.x))

#Scroll through the plots in the RStudio environment using the arrows. Or save to your hard drive using ggsave(). 

## Quick graphs with mean and error for many variables

#This code utilises mutate_at to calculate mean and sd for many variables and reshapes the data using gather and spread to create a table where it is easy to graph the mean with errorbars for many variables against a common x axis. 


### Sample data: Chronosequence soils

#Soil samples were taken from the restored land after coal mining from 13 sites aged 1-55 years from time of restoration to time of sampling. For each site there were two land use factors (Arable/Grass margin). 5 replicates were taken for each site and land use type and analysed for nutrient content, microbial biomass and soil respiration. 

#The task is to see how soil properties change with increasing age after restoration, comparing the arable to grass margins. Mean and standard deviation per site (age) and land use should be calculated and plotted against soil age for each soil property. 


load(file = "Jessica_Clayton/Markdown_pdf/Chrono.RData")

str(Chrono)

# define a vector list of the name of the variables you want to plot
vars <- c("Ctot.pc", "N.pc", "Cmic", "Nmic", "BAS.O2")

# calculate mean and sd per variable
# use summarise_at with the defined variable vector
Chrono_mean <- Chrono %>% 
  group_by(Age, Land.use) %>% 
  summarise_at(vars,  funs(mean(., na.rm=TRUE), sd(., na.rm = T)) )


head(Chrono_mean)


#Basic graph code: 

Chrono_mean %>% 
  ggplot() + 
  aes(Age, Ctot.pc_mean, col = Land.use) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = Ctot.pc_mean - Ctot.pc_sd, ymax = Ctot.pc_mean + Ctot.pc_sd)) + 
  theme_bw() + 
  xlab( "Soil age [Years]") + 
  ylab("Total carbon [%]")


#To make a similar graph for the next variable, 6 parts of the code need to be changed. 

#This is very tedious! 

#  How to automate? Change the layout of the data...

#Gather the table into a long format with separate columns for the variable, mean and sd in order to automate making the graphs.


Chrono_graph <- Chrono_mean %>% 
  # gather all data - key = variable (Ctot.pc, N.pc,...) _ statistic (mean/sd), value = "Value"
  gather("Variable_stat", "Value", -c(Age, Land.use)) %>% 
  # separate the Variable_stat column to have separate columns for variable and statistic
  # e.g. C.tot and mean
  separate(Variable_stat, c("Variable", "stat"), sep = "_", remove = T) %>% 
  # spread out the stat column so that the mean and sd have their own column
  spread(stat, Value)

head(Chrono_graph)



#Code to produce automated graphs: 

# create a vector for axis labels for your graphs including units
# this must be in the same order as your vars vector!
# the expression() function allows for special characters, sub- and superscript 

units <- expression("Total carbon [%]", 
                    "Total nitrogen [%]", 
                    "Cmic [mg g soil"^-1*"]", 
                    "Nmic [mg g soil"^-1*"]", 
                    "Basal respiration [O"[2]*" "*mu*"l hr"^-1* "g soil"^-1*"]")

# the mean with errorbars is always plotted against soil age, 
# the variable changes for each graph

graph_fun <- function(x) {
  Chrono_graph %>% 
    # filter for the variable using the vars vector with placeholder x 
    filter(Variable == vars[x]) %>% 
    ggplot() + 
    # x = Age, y = mean, different colour for land use
    aes(Age, mean, col = Land.use) + 
    geom_point() + 
    geom_line() +
    # plot error bars using the mean and sd columns
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) + 
    # give y axis label using units vector with placeholder x
    ylab( units[x] ) +
    xlab("Soil age [Years]") + 
    labs(col = "Land use") +
    theme_bw()
}

map(1:5, graph_fun)



#Voila! 

#  You could also think about making a vector for the plot title...

names(df_1718)

se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

df_plot <- df_1718 %>%
  group_by(Origin, Transplant, Treatment, jahr) %>%
  summarise_at(., vars(Biomass, C, N, P, CN, NP), funs(mean(., na.rm=TRUE), se(., na.rm = TRUE)))

Transplant <- c("EB", "GW","FE", "GW","FE")
Origin <- c("EB","EB","EB","GW","GW")
dT <- c("Ctrl","+1","+3","Ctrl","+2")
clim <- as.data.frame(cbind(Origin, Transplant, dT))
df_plot <- merge(df_plot, clim, by=c("Origin", "Transplant"))
df_plot$dT <- factor(df_plot$dT, levels(df_plot$dT)[c(4,1,2,3)])

df_plot <- as.data.frame(df_plot)
df_plot_EB <- df_plot[df_plot$Origin == "EB" , ]
df_plot_EB <- droplevels(df_plot_EB)
label_y_df <- c(c(Sigma~"ANPP [g/m²]",  bar(x)~"Carbon [%]", bar(x)~"Nitrogen [%]", bar(x)~"Phosphorous [g/kg]", bar(x)~"C:N ratio", bar(x)~"N:P ratio"))
rm(ls_graph)
ls_graph <- list()

for (i in 5:10)  local({
  i <- i
  tmp_graph <- ggplot(df_plot_EB, aes(x = jahr, y = df_plot_EB[ ,i],  color = dT))+
    geom_point(size=5, position = position_dodge(.2))+
    geom_errorbar(aes(ymax= df_plot_EB[ ,i] + df_plot_EB[ ,i+6],ymin= df_plot_EB[ ,i]-df_plot_EB[ ,i+6]), position = position_dodge(.2),  width=0.01)+
    geom_line(position = position_dodge(.2))+
    facet_wrap(~Treatment)+
    scale_linetype_manual(values = c(1,5))+
    scale_color_manual(values = c("#3C73B9","#FECC38","#E52620"), 
                       breaks = c("Ctrl", "+1", "+3"),
                       name = "Climatic\n change")+
    scale_x_continuous(breaks = c(2017, 2018))+
    #theme(legend.position = "none")+
    labs(x="Year", y=label_y_df[[i-4]])
  ls_graph[[i-4]] <<- tmp_graph
  names(ls_graph)[i-4] <- paste0(colnames(df_plot_EB)[i])
  
  print(tmp_graph)
})

names(ls_graph) <- paste0(colnames(df_plot_EB)[c(5:10)])

library(grid)
library(gridExtra)
library(ggpubr)
library(cowplot)

#Arrange 2 Plots in one panel
grid.arrange(ls_graph$Biomass_mean, ls_graph$CN_mean, nrow=1) #from grid.Extra

ggarrange(ls_graph$Biomass_mean + rremove("x.text") + rremove("x.title") , ls_graph$CN_mean , nrow = 2, ncol = 1, legend = "none") #ggbur

#arrange multiple plots
ggarrange(ls_graph$CN_mean, ls_graph$C_mean, ls_graph$N_mean, ls_graph$N_mean , nrow = 3, ncol = 2, legend = "none") #ggbur
ggarrange(plotlist = ls_graph) ##ggpubr

#change the size relationship between columns (width)
ggarrange(plotlist = ls_graph, nrow = 3, ncol = 2, legend = "none", widths = c(2,1)) #ggbur

#change the size relationship between rows (heights)
ggarrange(plotlist = ls_graph, nrow = 3, ncol = 2, legend = "none", heights = c(1,2,4)) #ggbur

#arrange multiple plots with different sizes [cowplot]
ggdraw()+
  draw_plot(ls_graph$CN_mean, x = 0 , y=0, width = 0.5, height = 1)+
  draw_plot(ls_graph$C_mean, x = 0.5 , y=0.5, width = 0.5, height = 0.5)+
  draw_plot(ls_graph$N_mean, x = 0.5 , y=0, width = 0.5, height = 0.5)

#and add a label to them
ggdraw()+
  draw_plot(ls_graph$CN_mean, x = 0 , y=0, width = 0.5, height = 1)+
  draw_plot(ls_graph$C_mean, x = 0.5 , y=0.5, width = 0.5, height = 0.5)+
  draw_plot(ls_graph$N_mean, x = 0.5 , y=0, width = 0.5, height = 0.5)+
  draw_plot_label(label = c("A","B","C"), x = c(0, 0.5, 0.5), y = c(1,1,0.5))

#also we can manipulate e.g. the legend
#extract the legend from a plot
p_legend <- get_legend(ls_graph$CN_mean+ theme(legend.position = "bottom"))
ggdraw()+
  draw_plot(ls_graph$CN_mean + theme(legend.position = "none"), x = 0 , y=0, width = 0.5, height = 1)+
  draw_plot(ls_graph$C_mean + theme(legend.position = "none"), x = 0.5 , y=0.5, width = 0.5, height = 0.5)+
  draw_plot(ls_graph$N_mean + theme(legend.position = "none"), x = 0.5 , y=0, width = 0.5, height = 0.5)+
  draw_plot_label(label = c("A","B","C"), x = c(0, 0.5, 0.5), y = c(1,1,0.5))+
  draw_grob(p_legend, 0, -0.45)# add the legend. And change the position wiht the numbers (first number = left right , second numer = top down)

#you can also manipulate the plot in here (more changes to the plot theme or the axis position...)

######
#Statistic & Plot - automation with NIRS results
######
nirs <- read.csv("Data/NIRS_WS.csv")
nirs$year <- as.factor(nirs$year)
str(nirs)
#we want to know if protein content of communities in 2017 changed with management intensity and/or/interacting with climate change
mod_prot <- lme(pProt~Treatment*dT, random = ~1|PlotID, data = nirs[nirs$plotO=="Esterberg" & nirs$year=="2017" ,])
anova(mod_prot) #as it is categorical we perform an ANOVA
hist(mod_prot$residuals, xlab = "Residuals", main = "Esterberg_2017_Protein") #check normal distribution (but ANOVA is somehwat robust against)
plot(mod_prot)# check heteroscedasticity (problem the factorial design)
#conduct post-hoc Test with ecological meaningful comparisons
lsmeans(mod_prot, pairwise ~dT|Treatment ,adjust = "tukey") #either within a treatment across climate change
lsmeans(mod_prot, pairwise ~Treatment|dT ,adjust = "tukey") #or within a site comparing treatments

#And we want to know this for both sites of origins, both years and the three (Ash,Fat,Protein) parameters of forage quality
# 2*2*3 -> 12 times this code... prone to copy and paste errors so we might automate this
library(multcomp)
library(multcompView)

Mod_climXtrt_effect_byYear <- data.frame(NULL)
cSigtrtGathered <- data.frame(NULL)
cSigccGathered <- data.frame(NULL)
for (i in levels(nirs$plotO)){
  for(g in levels(nirs$year)){
    tmp_df <- nirs[nirs$plotO==i & nirs$year==g, ]
    tmp_df <- droplevels(tmp_df)
    
    for (r in 11:13){
      ##Get Model and Statistical Output
      tmp_df$response <- tmp_df[ ,r]
      tmp_mod_trt <- lme(response ~ Treatment*dT, random = ~1|PlotID, data = tmp_df)
      tmp_mod_anova <- anova(tmp_mod_trt)
      tmp_mod_anova$Predictor <- row.names(tmp_mod_anova)
      tmp_mod_anova <- as.data.frame(tmp_mod_anova)
      tmp_mod_anova <- tmp_mod_anova[-1,3:5]
      hist(tmp_mod_trt$residuals, xlab="Residuals", main=paste0(i,g, "_Residual Hist_", colnames(tmp_df[r])))
      plot(tmp_mod_trt, main=paste0(i,g, "_Residual-vs-Fitted_", colnames(tmp_df[r])))
      
      tmp_Tukey_clim <- lsmeans(tmp_mod_trt, pairwise ~dT|Treatment, adjust="tukey" )
      tmp_Tukey_clim <- as.data.frame(tmp_Tukey_clim$contrasts)
      tmp_Tukey_clim <- tmp_Tukey_clim[ ,c(1:3,7)]
      tmp_Tukey_trt <- lsmeans(tmp_mod_trt, pairwise ~Treatment|dT, adjust="tukey" )
      tmp_Tukey_trt <- as.data.frame(tmp_Tukey_trt$contrasts)
      tmp_Tukey_trt <- tmp_Tukey_trt[ ,c(1:3,7)]
      
      tmp_site <- data.frame(Site=paste(i))
      tmp_response <- data.frame(Response=paste(colnames(tmp_df)[r]))
      tmp_year <- data.frame(Year=paste(g))
      
      Mod_summary <- list(Site=tmp_site, Response=tmp_response, Year=tmp_year,
                          p_ANOVA=tmp_mod_anova,
                          Trt_Tukey=tmp_Tukey_trt,
                          Clim_Tukey=tmp_Tukey_clim)
      
      Mod_summary <- do.call(bind_rows,Mod_summary)
      
      Mod_climXtrt_effect_byYear <- rbind(Mod_climXtrt_effect_byYear, Mod_summary)
      
      ##Prepare Dataframes for graphical output
      ctrt <- lsmeans(tmp_mod_trt, pairwise ~Treatment|dT, adjust="tukey" )#Tukey How I do it in the model loop above
      
      #Model Comparison of Ext-vs-Int at different Sites
      cSigtrt <- data.frame(ctrt[[2]])
      cSigtrt$plotO <- paste(i)
      cSigtrt$year <- paste(g)
      cSigtrt$Response <- paste(colnames(tmp_df[r]))
      cSigtrt$let <- NA
      cSigtrt$let <- ifelse(cSigtrt$p.value >0.05,"",cSigtrt$let)
      cSigtrt$let <- ifelse(cSigtrt$p.value <=0.05 & cSigtrt$p.value >0.01,"*",cSigtrt$let)
      cSigtrt$let <- ifelse(cSigtrt$p.value <=0.01 & cSigtrt$p.value >0.001,"**",cSigtrt$let)
      cSigtrt$let <- ifelse(cSigtrt$p.value <0.001,"***",cSigtrt$let)
      
      cSigtrtGathered <- rbind(cSigtrtGathered, cSigtrt)
      
      #Model Comparison of Different sites with same treatment
      cSigcc <- data.frame(cld(lsmeans(tmp_mod_trt, ~dT|Treatment), by='Treatment', Letters = c('ABC')))
      cSigcc$plotO <- paste(i)
      cSigcc$year <- paste(g)
      cSigcc$Response <- paste(colnames(tmp_df[r]))
      #colnames(cSigcc)[1:2] <- c("contrast","Behandlung")
      
      cSigccGathered <- rbind(cSigccGathered, cSigcc)
    }
  }
}
str(Mod_climXtrt_effect_byYear)
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
Mod_climXtrt_effect_byYear <- round_df(Mod_climXtrt_effect_byYear, 4)
Mod_climXtrt_effect_byYear$contrast <- paste0("_" ,Mod_climXtrt_effect_byYear$contrast) #underscore is only added because of exproting to excel (+2 - +4 comparison will be -2 in excel)
Mod_climXtrt_effect_byYear$dT <- paste0("_" ,Mod_climXtrt_effect_byYear$dT) #underscore is only added because of exproting to excel (+2 will be 2 and numeric)

cSig <- left_join(cSigccGathered, cSigtrtGathered[ ,-c(3:5)], by=c("plotO", "year", "Response","dT"))
str(cSig)

#calculating the positions of letters and asteriks
cSig <- cSig %>%
  group_by(plotO, Response, dT, year) %>%
  mutate_at(., vars(lsmean), funs(let_mean=ifelse(Treatment=="Intensive", (lsmean+SE)*1.015 , (lsmean-SE)*0.985), ast_mean = mean(., na.rm = TRUE)))
cSig$dT <- factor(cSig$dT, levels(cSig$dT)[c(3,1,2)]) #resort the factor on the x-axis
cSig <- as.data.frame(cSig) # can't (or don't know) how to change characters of a grouping variable to factor (hence need df)
cSig <- cSig %>%
  mutate_at(., vars(plotO, year, Response), funs(as.factor(.)))
cSig <- as.data.frame(cSig)
str(cSig) #now everything is as desired

##Plotting
label_y <- c(bar(x)~"Ash [%]", bar(x)~"Fat [%]", bar(x)~"Protein [%]") #create a label-vector
for(r in levels(cSig$Response)){
  for(i in levels(cSig$year)){
    tmp_graph <- cSig %>% filter(Response==r & year==i) %>%
      ggplot(data = ., aes(x=dT, y=lsmean, color = Treatment))+
      geom_point(size=3,position=position_dodge(.6))+
      geom_errorbar(aes(ymax= lsmean+SE, ymin= lsmean-SE, color = Treatment),lwd=1 ,  width=0.01, position=position_dodge(.6))+
      geom_text(aes(label=let, x=dT, y= ast_mean*0.93), fontface = "bold", color= "black", size = 5, show.legend = F, vjust = 0)+ #Add Asteriks for EXT-vs-INT comparison
      geom_text(aes(label=.group, x=dT, y=let_mean, color = Treatment), fontface = "bold", position=position_dodge(.6))+ #Add Grouping Letters
      scale_color_manual(name = "Treatment",
                         values= c("#91bfdb", "#fc8d59"))+
      facet_wrap(~plotO, scales = "free_x")+
      labs(x="", y=label_y[[unique(as.numeric(cSig$Response)[cSig$Response==r])]], subtitle = paste0(i) )+
      theme(legend.position = "bottom")#ifelse(r=="pProt","bottom","none")
    print(tmp_graph)
    assign(paste(r, i, sep = "_"), tmp_graph )
  }
}
