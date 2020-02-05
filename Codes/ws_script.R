###################################################
###     ADVANCED DATA HANDLING AND VISUALISATION 
###           -WORKSHOP SCRIPT 2020-
### BonaRes-R-Group | Authors
###################################################

library(tidyverse) #load in the tidyverse (loading multiple packages belonging to the "tidyverse")
# library(magrittr)
library(lubridate)
library(rmarkdown)

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


###Tries to write a function to rename a specific column
#colnames(sm_list[[1]])[1] <- "Date"

#new_col_name <- function(data = x, column = a, new_name = b){
#  colnames(x)[a] <- "Date"
#}
#b <- "b"
#paste0(b)



#Basic R lapply
txt_files <-  list.files(path = "Data/SoilMoisture", full.names = TRUE) #!Take care of the workingdirectory pathway! - Full..names pastes the path(expansion) before the name
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


#####
### Jessica Clayton
load("Jessica_Clayton/Markdown_pdf/Chrono.RData") #load in Chronosequencs
load("Jessica_Clayton/Markdown_pdf/O2_data.RData") #load in O2 data


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
map(m, ~ n + .) # '.' is a placeholder

#When the output is saved, R automatically saves both vectors within a list: 
x <- map(m, ~ n + .)
class(x) #it is a list
x[1] #vector - containing the sum of n+ (+1)
x[2] #vector - containing the sum of n+ (-1)

# Lists

#[Note from Jess: I need to think of a good explanation about lists. Basically lists store all kinds of objects within the shell of one object within the environment. You can have a list which consists of all identical length vectors or identical length dataframes, or all non-identical vectors and dataframes, or a mixture of dataframes and vectors. Just look at the lists which are created when you save the output of a linear model
#An advantage is that you can apply a function to each element of your list.]
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

#[This might seem a bit complicated for such simple tasks but it will come clearer why it is useful later] 

####
##Automated data import
####
## To merge all files into one dataframe: map_df from purrr

# Create a list with the names of all files you wish to import into your environment
files_names <- list.files("File_path/", pattern = "*.csv")

files_names <- list.files(path = "Data/SoilMoisture",  pattern = "*.csv")
# (this only works if the tables have identical column names and dimensions)
All_data <- map_df(files_names, ~read.csv2(paste0("Data/SoilMoisture/" , .)))
#Check the result | Works but not senseful!
###Purrr - lists

rm(sm_list)
txt_files <-  list.files(path = "Data/SoilMoisture", pattern = "*.csv") #!Take care of the workingdirectory pathway! - Full..names pastes the path(expansion) before the name

sm_list <- map(txt_files, ~ read.csv2(paste0("Data/SoilMoisture/" , .)))
sm_list <- map(sm_list, ~mutate(., SiteID = paste0(colnames(.)[1]))) #creates a column with SiteID
sm_list <- map(sm_list, ~mutate(., Date = .[[1]])) #creates a new colum of Date (as I did'nt know how to rename it)
sm_list <- map(sm_list, ~select(., -1)) #deletes the first column (to get rid of it because column names I couldnt change)
names(sm_list) <- txt_files
sm_list %>%
  names(.) %>%
  map( ~write.csv(sm_list[[.]], file = paste0("Jessica_clayton/Markdown_pdf/Soil_Moisture/", .), row.names = FALSE))


files_names <- list.files(path = "Jessica_clayton/Markdown_pdf/Soil_Moisture/",  pattern = "*.csv")
All_data <- map_df(files_names, ~read.csv(paste0("Jessica_clayton/Markdown_pdf/Soil_Moisture/" , .)))
#after all having the same format we can bind them together


## Alternatively work using lists

#When your data are not identical in dimensions or file names, it might be better to import the data to a list.

# first define an empty list
My.list <- list()

# use the read.csv function with map to store each file as a separate data frame within your list
My.list <- map(files_names, ~ read.csv2(paste0("Data/SoilMoisture/" , .)))

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
My.list.2 <- map(My.list, ~mutate(.x, New_Date.format = as.Date(Date,  format = "%d.%m.%Y"))) 

# Delete a column , e.g. a comments column
My.list.2 <- map(My.list2, ~ select(.x, -Comments))

# Perform many operations using the pipe function

My.list.2 <- My.list %>% 
  map(~mutate(., O2.L = O2_ml/1000)) %>% # change units from ml to L
  map(~select(., -c(Channel, Treatment))) # remove columns


####
##Graph functions
####

#For graph automation using functions, map and ggplot2

## A function for graphs with a common x axis

# define which columns you want to plot against x
vars <- names(My.data)[5:10]

# make a function with ggplot where y is the placeholder for your y variable:
fun1.1 <- function(y) { 
  My.data %>% 
    ggplot() + 
    aes(Date, get(y), col = Land.use) + #get(y) removes "" from the variable name
    geom_point() + 
    theme_bw() + 
    xlab("Date") + 
    ylab(y) # don't forget to label axis
}

# map passes each element of the vars vector through the graph function as y
# and thus creates a separate graph for each variable that you defined
map(vars, fun1.1)


## A function for subsetting data and creating graph for each treatment level

# create a vector for soil type:
ST.filter = levels(My.data$Soiltype)

# e.g. 
fun2 <- function(x) { 
  My.data %>% 
    filter(Soiltype == x) # x is the placeholder for the soil type level
  ggplot() + 
    aes(Date, Total.carbon , col = Land.use) + 
    geom_point() + 
    theme_bw() + 
    xlab("Date") + 
    ggtitle(x) # don't forget to make a title so you know which soil type is being shown
}
map(ST.filter, fun2)

## Save the graphs to your computer

#Use ggsave()

fun2 <- function(x) { 
  My.data %>% 
    filter(Soiltype == x) # x is the placeholder for the soil type level
  ggplot() + 
    aes(Date, Total.carbon , col = Land.use) + 
    geom_point() + 
    theme_bw() + 
    xlab("Date") + 
    ggtitle(x) # don't forget to make a title so you know which soil type is being shown
  
  # create a unique file name by using x as placeholder soil type in the file name
  ggsave(paste0("File_path/Total.C_", x, ".png"))   
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

load(file = "O2_data.RData")

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
map(1:5, ~graph_fun(.x) )

#Scroll through the plots in the RStudio environment using the arrows. Or save to your hard drive using ggsave(). 

## Quick graphs with mean and error for many variables

#This code utilises mutate_at to calculate mean and sd for many variables and reshapes the data using gather and spread to create a table where it is easy to graph the mean with errorbars for many variables against a common x axis. 


### Sample data: Chronosequence soils

#Soil samples were taken from the restored land after coal mining from 13 sites aged 1-55 years from time of restoration to time of sampling. For each site there were two land use factors (Arable/Grass margin). 5 replicates were taken for each site and land use type and analysed for nutrient content, microbial biomass and soil respiration. 

#The task is to see how soil properties change with increasing age after restoration, comparing the arable to grass margins. Mean and standard deviation per site (age) and land use should be calculated and plotted against soil age for each soil property. 


load(file = "Chrono.RData")

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
