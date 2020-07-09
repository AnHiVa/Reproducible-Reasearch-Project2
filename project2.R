library(tidyverse)

filename <- "FStormData.csv.bz2"

## File download from source 
if (!file.exists(filename)){
  
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",filename, method = "curl")
  filedownloadtime <- paste(file.info(filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The FStormData file was downloaded:", filedownloadtime,sep = " "))

} else {
  
  filedownloadtime <- paste(file.info(filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The FStormData file was downloaded:", filedownloadtime, sep = " "))

}

#Read data, only columns that will be used
StormData <- read_csv(filename, 
                       col_types = cols_only(
                         EVTYPE = col_character(),
                         FATALITIES = col_double(),
                         INJURIES = col_double(),
                         PROPDMG = col_double(),
                         PROPDMGEXP = col_character(),
                         CROPDMG = col_double(),
                         CROPDMGEXP = col_character()
                         ))

## Group data by EVTYPE and summarize Total of fatalities and injuries
health <- StormData %>%
  group_by(EVTYPE) %>%
  summarise(Totalfatalities = sum(FATALITIES, na.rm = TRUE), Totalinjuries = sum(INJURIES, na.rm = TRUE))

## Top 10 fatalities slice
fatalitiesfiltered <- health %>%
  select(-Totalinjuries) %>%
  slice_max(Totalfatalities ,n = 10) %>%
  arrange(desc(Totalfatalities)) %>%
  mutate(EVTYPE = factor(EVTYPE, levels = EVTYPE) )

summary(fatalitiesfiltered)

fatalitiesplot <- ggplot(fatalitiesfiltered, aes(x = EVTYPE, y = Totalfatalities)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Fatalities") +
  labs(title = "Top 10 weather events by fatalities")


#Top 10 injuries slice
injuriesfiltered <- health %>%
  select(-Totalfatalities) %>%
  slice_max(Totalinjuries, n = 10) %>%
  arrange(desc(Totalinjuries)) %>%
  mutate(EVTYPE = factor(EVTYPE, levels = EVTYPE))

summary(injuriesfiltered)


injuriesplot <- ggplot(injuriesfiltered, aes(x = EVTYPE, y = Totalinjuries)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Injuries") +
  labs(title = "Top 10 weather events by injuries")

economic <- StormData %>%
  select(EVTYPE,PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

## Replaces text abbreviation to exponent number
economic$PROPDMGEXP <- str_replace_all(string = economic$PROPDMGEXP,
                  pattern = c('B' = "9",
                              'M' = "6",
                              'K' = "3",
                              'k' = "3",
                              'H' = "2",
                              'h' = "2",
                              'm' = "0",
                              '\\+' = "0",
                              '\\?' = "0",
                              '-' = "0"
                              )) %>%
  parse_double()

economic$CROPDMGEXP <- str_replace_all(string = economic$CROPDMGEXP,
                                       pattern = c('B' = "9",
                                                   'M' = "6",
                                                   'K' = "3",
                                                   'k' = "3",
                                                   'H' = "2",
                                                   'h' = "2",
                                                   'm' = "0",
                                                   '\\+' = "0",
                                                   '\\?' = "0",
                                                   '-' = "0"
                                       )) %>%
  parse_double()

## Aggregate data, group by TYPE, summarise by total damage and filter top 10. 
Totaleconomic <- economic %>%
  group_by(EVTYPE) %>%
  summarise(Totaldamage = sum((PROPDMG * 10^(PROPDMGEXP)),(CROPDMG * 10^(CROPDMGEXP)), na.rm = TRUE)/(10^9)) %>%
  slice_max(Totaldamage, n = 10) %>%
  arrange(desc(Totaldamage)) %>%
  mutate(EVTYPE = factor(EVTYPE, levels = EVTYPE))

Economicplot <- ggplot(Totaleconomic, aes(x = EVTYPE, y = Totaldamage)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Billion USD") +
  labs(title = "Top 10 weather events by total damages (Properties and Crops)")
