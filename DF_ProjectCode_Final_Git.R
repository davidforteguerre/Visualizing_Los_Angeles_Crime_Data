# IST719 Information Visualizaton
# David Forteguerre
# Final Project: LA CRIME DATA




# DATA SOURCE:
# "Crime Data from 2010 to Present" (updated frequently by the LAPD)
# https://data.lacity.org/A-Safe-City/Crime-Data-from-2010-to-Present/y8tr-7khq 



# to import the libraries I'll need (dplyr, tidyr, ggplot2)
library(tidyverse)
library(plyr) # to import plyr too (not in tidyverse)
library(forcats) # needed to reorder factors with fct_infreq() function
library(treemapify)
library(treemap)
library(fmsb) # for radar charts
library(wordcloud)
library(RColorBrewer)
library(ngram) # might need


# to import the data
LAcrime_RAW <- read.csv(file = "/Users/davidforteguerre/Google Drive/2-SCHOOL/M.S. Data Science/IST719 Info Viz/FINAL PROJECT/Crime_Data_from_2010_to_Present.csv"
                 , header = TRUE
                 , na.strings = "" # to import empty cells as NAs (or else they will be empty without information)
                 , stringsAsFactors = FALSE)
## str(LAcrime_RAW)

View(LAcrime_RAW)

# to delete unwanted columns and clean the raw dataset
LAcrime <- select(LAcrime_RAW
                  , -DR.Number 
                  , -Reporting.District 
                  , -MO.Codes =
                  , -(Crime.Code.1:Crime.Code.4) 
                  , -Address 
                  , -Cross.Street) 

# to clean the data and replace some values
LAcrime$Victim.Descent <- revalue(LAcrime$Victim.Descent, c(A = "Other Asian" # Victim.Descent
                                                            , B = "Black" 
                                                            , C = "Chinese" 
                                                            , D = "Cambodian" 
                                                            , F = "Filipino" 
                                                            , G = "Guamanian" 
                                                            , H = "Hispanic/Latin/Mexican" 
                                                            , I = "American Indian/Alaskan Native" 
                                                            , J = "Japanese" 
                                                            , K = "Korean" 
                                                            , L = "Laotian" 
                                                            , O = "Other" 
                                                            , P = "Pacific Islander" 
                                                            , S = "Samoan" 
                                                            , U = "Hawaiian" 
                                                            , V = "Vietnamese" 
                                                            , W = "White" 
                                                            , X = "Unknown"
                                                            , Z = "Asian Indian"))
LAcrime$Victim.Descent <- gsub('-', 'Unknown', LAcrime$Victim.Descent) # to get rid of the rows containing a -
##table(LAcrime$Victim.Sex)
LAcrime$Victim.Sex <- revalue(LAcrime$Victim.Sex, c(F = "Female" # # Victim.Sex
                                                    , H = "Other"
                                                    , M = "Male"
                                                    , N = "Other"
                                                    , X = "Unknown"))
LAcrime$Victim.Sex <- gsub('-', 'Unknown', LAcrime$Victim.Sex) # to get rid of the rows containing a -


# to convert some columns to factors and replace missing values by 000's
LAcrime$Area.ID <- as.factor(LAcrime$Area.ID)
LAcrime$Crime.Code <- as.factor(LAcrime$Crime.Code)
LAcrime$Premise.Code  <- as.character(LAcrime$Premise.Code )
LAcrime$Premise.Code  <- replace_na(LAcrime$Premise.Code , "000")
LAcrime$Premise.Code  <- as.factor(LAcrime$Premise.Code )
LAcrime$Status.Code  <- as.character(LAcrime$Status.Code )
LAcrime$Status.Code  <- replace_na(LAcrime$Status.Code , "000")
LAcrime$Status.Code  <- as.factor(LAcrime$Status.Code )
LAcrime$Weapon.Used.Code <- as.character(LAcrime$Weapon.Used.Code)
LAcrime$Weapon.Used.Code <- replace_na(LAcrime$Weapon.Used.Code, "000")
LAcrime$Weapon.Used.Code <- as.factor(LAcrime$Weapon.Used.Code)

# to replace missing categories and/or descriptions by "unknown"
LAcrime$Victim.Sex <- replace_na(LAcrime$Victim.Sex, "Unknown")
LAcrime$Victim.Sex <- as.factor(LAcrime$Victim.Sex)
LAcrime$Victim.Descent <- replace_na(LAcrime$Victim.Descent, "Unknown")
LAcrime$Victim.Descent <- as.factor(LAcrime$Victim.Descent)
LAcrime$Premise.Description <- replace_na(LAcrime$Premise.Description, "Unknown")
LAcrime$Weapon.Description <- replace_na(LAcrime$Weapon.Description, "UNKNOWN")


# to extract latitude and longitude into 2 new columns
LAcrime$coordinates <- gsub(pattern = '[()]', replacement = '', x = LAcrime$Location) # to remove () from the coordinates
LAcrime <- separate(data = LAcrime, col = coordinates, into = c("latitude","longitude"), sep = ",") # to separate the coordinates into 2 new columns
LAcrime$Location <- NULL # to delete the Location column (we don't need it anymore)

# to convert lat and long to numeric values
LAcrime$latitude <- as.numeric(LAcrime$latitude)
LAcrime$longitude <- as.numeric(LAcrime$longitude)

# to replace age outliers
##summary(LAcrime$Victim.Age)
LAcrime$Victim.Age[LAcrime$Victim.Age>100] <- NA # to replace all age values above 100 by an NA
LAcrime$Victim.Age[LAcrime$Victim.Age<=0] <- NA # to replace all age values under or equal to 0 by an NA
summary(LAcrime$Victim.Age)


# to check format of dates and times
head(LAcrime$Date.Reported, 5)
head(LAcrime$Date.Occurred, 5)
head(LAcrime$Time.Occurred, 5)

# to convert dates and set the time zone to LA time
LAcrime$Date.Reported <- as.POSIXct(strptime(LAcrime$Date.Reported, "%m/%d/%Y", tz = 'America/Los_Angeles'))
LAcrime$Date.Occurred <- as.POSIXct(strptime(LAcrime$Date.Occurred, "%m/%d/%Y", tz = 'America/Los_Angeles'))
# ?strptime

# to convert time using lubridate and put it in a new column
library(stringr)
library(lubridate)
LAcrime$Time.Occurred <- str_pad(LAcrime$Time.Occurred, 4, side = "left", pad = "0") # let's first clean the times and add 0's so we always have 4 numbers
LAcrime$Time.Occurred <- gsub('(..)(..)', '\\1:\\2', LAcrime$Time.Occurred) # let's now add a : between min and sec
LAcrime$Time.Occurred_NEW <- hm(LAcrime$Time.Occurred) # let's now convert times to a time object using lubridate

# to create a day difference column between date reported and date occurred
LAcrime$Day.Diff <- as.integer(round(difftime(LAcrime$Date.Reported, LAcrime$Date.Occurred, units = "days"), 0))

# to check that dates and times have been converted properly
head(LAcrime$Date.Reported, 5) # to check the first 5 dates
table(is.na(LAcrime$Date.Reported)) # to check if there are any NAs (which would mean the conversion didn't work in some rows)
any(is.na(LAcrime$Date.Reported))

head(LAcrime$Date.Occurred, 5) 
table(is.na(LAcrime$Date.Occurred))
any(is.na(LAcrime$Date.Occurred))

head(LAcrime$Time.Occurred_NEW, 5) 
table(is.na(LAcrime$Time.Occurred_NEW))
any(is.na(LAcrime$Time.Occurred_NEW))


# to reorder cols
View(LAcrime)
str(LAcrime)
LAcrime <- LAcrime[c(1,2,20,3,19,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]




      # FINAL CHECK AND DATA SUMMARY

# to check for NAs in the data
any(is.na(LAcrime)) # are there any NA's in the data?
data.frame(colSums(is.na(LAcrime))) # since yes, where are they?

# to get a sense of the data (this chunk was originally at the beginning, but I put it at the end after the data wrangling step)
class(LAcrime)
names(LAcrime)
dim(LAcrime)
str(LAcrime)
summary(LAcrime)
View(LAcrime)


######################### TO EXPORT THE NEW FULL/CLEAN DATASET ######################### 
# To save the full dataset 
##write.csv(LAcrime, '/Users/davidforteguerre/Google Drive/2-SCHOOL/M.S. Data Science/IST719 Info Viz/FINAL PROJECT/LAcrime_CLEAN.csv')
##################################################################################




################################################################################
# to subset 2018 crimes only
LAcrime2018 <- subset(LAcrime, Date.Occurred >= "2018-01-01" & Date.Occurred < "2019-01-01")
#View(LAcrime2018)
nrow(LAcrime2018)

# to subset 2015 crimes only
LAcrime2015 <- subset(LAcrime, Date.Occurred >= "2015-01-01" & Date.Occurred < "2016-01-01")
#View(LAcrime2015)
nrow(LAcrime2015)

# to subset 2018 January crimes only
LAcrime2018Jan <- subset(LAcrime, Date.Occurred >= "2018-01-01" & Date.Occurred <= "2018-01-31")
#View(LAcrime2018Jan)
nrow(LAcrime2018Jan)
################################################################################









      # EXPLORATORY DATA ANALYSIS

# to change parameters
##?par
options(scipen = 99) # to turn off scientic notation
#par(bty = "n") # this will delete boxes around plots
#par(mar = c(5, 4, 4, 2) + 0.1) # to reset mar to default (this might come in handy)
#par(mgp = c(3, 1, 0)) # to reset mgp to default (this might come in handy)



     # Victim sex

# barplot
ggplot(LAcrime2015, aes(x=fct_infreq(Victim.Sex))) + # fct_infreq orders the bar (factor). Could also add: fill = Victim.Sex
  geom_bar() + 
  ggtitle("Number Of Victims By Gender (2015)") + 
  xlab("Gender") + 
  ylab("Count") + 
  theme_minimal()


    # Victim age (distribution) (NOTE THAT OUTLIERS >=0 AND <100 WERE REMOVED!)

# histogram
ggplot(LAcrime2015, aes(x=(Victim.Age))) +
  geom_histogram(binwidth=5, colour="white", alpha=0.6) + 
  ggtitle("Victim Age Histogram (2015)") + 
  xlab("Age") +
  ylab("Frequency") + 
  theme_minimal()

# boxplot
ggplot(LAcrime2015, aes(x=LAcrime2015$Victim.Sex, y=Victim.Age)) + # for no groupings (by sex), write: x=factor(0)
  geom_boxplot(outlier.alpha = 0.2) + # to color outliers: outlier.colour = "red",
  stat_summary(fun.y="mean", geom="point", shape=18, size=2, colour="blue") +
  ggtitle("Victim Age Boxplot (2015)") + 
  xlab("") +
  ylab("Age") + 
  theme_minimal() 





     # Victim ethnicity

# subsetting
LAcrime_SUBSET_Victim.Descent = data.frame(head(sort(table(LAcrime2015$Victim.Descent), decreasing=TRUE), 10)) # to subset top 10

# barplot
ggplot(LAcrime_SUBSET_Victim.Descent, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity") + # stat="identity" is needed to tell ggplot2 not to summarize/pivot the data (it's already done)
  ggtitle("Number Of Victims By Ethnicity (Top 10) (2015)") + 
  xlab("Ethnicity") +
  ylab("Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1)) # to change x label size and rotate them

# subsetting
LAcrime_SUBSET_Victim.Descent = data.frame(head(sort(table(LAcrime2015$Victim.Descent), decreasing=TRUE), 50)) # to subset top 50

# treemap
ggplot(LAcrime_SUBSET_Victim.Descent, aes(area = Freq, fill = Freq, label = Var1)) +
  geom_treemap() + 
  ggtitle("Victim Ethnicity Treemap (2015)") +
  geom_treemap_text(fontface = "italic"
                    , colour = "white"
                    , alpha = 0.8
                    #, place = "centre"
                    , grow = F) + 
  scale_fill_gradient(low="white", high="black") + 
  theme(legend.position="bottom", legend.key.width = unit(2, "cm")) # legend







      # Premise

# subsetting
LAcrime_SUBSET_Premise.Description = data.frame(head(sort(table(LAcrime2015$Premise.Description), decreasing=TRUE), 20)) # to subset top 20

# barplot
ggplot(LAcrime_SUBSET_Premise.Description, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") + # stat="identity" is needed to tell ggplot2 not to summarize/pivot the data (it's already done)
  ggtitle("Number Of Victims By Crime Premise (Top 20) (2015)") + 
  xlab("Premise") +
  ylab("Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) # to change x label size and rotate them

# tree map
ggplot(LAcrime_SUBSET_Premise.Description, aes(area = Freq, fill = Freq, label = Var1)) +
  geom_treemap() + 
  geom_treemap_text(fontface = "italic"
                    , colour = "white"
                    , alpha = 0.8
                    #, place = "centre"
                    , grow = F) + 
  scale_fill_gradient(low="darkorange3", high="black") + 
  theme(legend.position="bottom", legend.key.width = unit(1, "cm")) # legend

# wordcloud
wordclouddata = data.frame(head(sort(table(LAcrime2015$Premise.Description), decreasing=TRUE), 100))
#View(wordclouddata)
par(mar = c(0,0,0,0), bg = "black")
wordcloud(wordclouddata$Var1, wordclouddata$Freq
          , min.freq=100
          , scale=c(2,0.9)
          , max.words=50
          , rot.per=0.5
          , colors=gray.colors(20, start = 0.3, end = 0.6)) # to view # alpha = 0.5
par(mar = c(0,0,0,0), bg = "white") # to reset







      # MAPS

################################################################################################
# to activate packages
library(ggmap)
register_google(key = "MY KEY HERE") # My API

# to subset the top 6 crimes
top6crimes = names(tail(sort(table(LAcrime2015$Crime.Code.Description), decreasing=FALSE), 6))
LAcrime_blabla <- filter(LAcrime2015, Crime.Code.Description %in% top6crimes)
View(LAcrime_blabla)

# to get a map of LA using a different function (Jim)
la <- get_map(location = c(-118.243683, 34.052235), maptype = "toner-lite", zoom = 10)
ggmap(la) # to view

# to plot
ggmap(la) + stat_density_2d(data=LAcrime_blabla
                            , aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..)
                            , geom = "polygon"
                            , size = 8 # added that
                            , n = 500) + 
  #scale_fill_gradient(low = "#B8F09E", high = "#3F9E12") + # low = "blue", high = "red"
  scale_fill_gradient(low = "orange", high = "darkred") +
  theme(legend.position = 'none') +
  facet_wrap(. ~ Crime.Code.Description) +  # this is where the magic happens! This creates 4 plots
  ggtitle("LOS ANGELES TOP 6 CRIMES (2015) HEAT MAP")
################################################################################################








      # TIME

############################################################################################################################
# to find how long it takes victims to report crimes usually
summary(LAcrime2015$Day.Diff) # median is about 1 day and mean is about 16 days

# to see if there is a relationship between age and time difference (i.e. time it took to report)
fullsample <- filter(LAcrime2015, !is.na(Victim.Age)) # to only keep records that have a value in age
correl = round(cor(fullsample$Victim.Age, fullsample$Day.Diff, method = "pearson"), 4)
correl # There isn't (the value is very close to 0)
############################################################################################################################



      # BY MONTH

# barchart
ggplot(LAcrime2015, aes(x=factor(month(LAcrime2015$Date.Occurred)))) + # fct_infreq orders the bar (factor). Could also add: fill = Victim.Sex
  geom_bar() + 
  ggtitle("Number Of Victims By Month (2015)") + 
  xlab("Month") + 
  ylab("Count") + 
  theme_minimal()

# radar chart
monthdf1 = data.frame(table(month(LAcrime2015$Date.Occurred)))
month = monthdf1$Var1
value = monthdf1$Freq
monthdf2 = rbind(rep(20000,12), rep(15000,12), value)
monthdf3 <- data.frame(monthdf2)
colnames(monthdf3) = month
monthdf3
radarchart(monthdf3[,c(12, 11:1)] # this will sort the results clockwise
           , title =  "Number Of Victims By Month (2015)"
           , plwd=2 # line thickness
           , seg=5
           #, caxislabels=seq(15000,20000,1000)
           #, pcol="darkred" # color of line
           #, pfcol=rgb(1,0.5,0.2,0.5) # color of shape: red
           , pfcol=rgb(0,0,0,0.5) # color of shape: black
           , cglcol="gray" # color of grid
           , cglty=2 # grid
           #, axislabcol="black", caxislabels=seq(15000,20000,1000), caxislabels=c("worst", "d", "d", "d", "d", "best")
           , cglwd=0.8)



      # BY HOUR

# barchart
ggplot(LAcrime2015, aes(x=factor(hour(LAcrime2015$Time.Occurred_NEW)))) + # fct_infreq orders the bar (factor). Could also add: fill = Victim.Sex
  geom_bar() + 
  ggtitle("Number Of Victims By Hour (2015)") + 
  xlab("Gender") + 
  ylab("Hour") + 
  theme_minimal()

# radar chart of this
hourdf1 = data.frame(table(hour(LAcrime2015$Time.Occurred_NEW)))
hour = hourdf1$Var1
value = hourdf1$Freq
hourdf2 = rbind(rep(20000,24), rep(0,24), value)
hourdf3 <- data.frame(hourdf2)
colnames(hourdf3) = hour
hourdf3
radarchart(hourdf3[,c(1, 24:2)]
           , title =  "Number Of Victims By Hour (2015)"
           , centerzero=F
           , plwd=2 # line thickness
           , seg=4
           , pfcol=rgb(0,0,0,0.5) # color of shape: black
           , cglcol="gray" # color of grid
           , cglty=2 # grid
           # , axislabcol="black", caxislabels=seq(0,20,5)
           , cglwd=0.8)


      # BY WEEKDAY BY MONTH

# barplot (multidimension)
ggplot(LAcrime2015, aes(x=factor(wday(LAcrime2015$Date.Occurred)), fill=..count..)) + 
  geom_bar() +
  facet_grid(~ factor(month(LAcrime2015$Date.Occurred))) + 
  ggtitle("Number of Victims by Weekday by Month (2015)") + 
  labs(x="Month",y="Count") +
  theme(legend.position="none") +
  scale_fill_gradient(low="gray80", high="black")




