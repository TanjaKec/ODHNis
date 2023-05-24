library(leaflet)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))

mydata <- read.csv('data/data_nis.csv', 
                   header=T) 

glimpse(mydata)

# separate date from time
mydata <- separate(mydata, timestamp, c("date", "time"), sep = "T")
## remove the last character from the `time` variable
mydata$time <- (str_sub(mydata$time, end = -1))
# scan data
glimpse(mydata)

mydata <- mydata[, -c(2, 3)]

(uniq <- unlist(lapply(mydata, function(x) length(unique(x)))))

mydata %>%
  group_by(sensor_id) %>%
  summarise(no_readings = n()) %>%
  arrange(no_readings) %>% 
  DT::datatable()

mydata[,1] <- as.factor(mydata[,1])
summary(mydata)
# creating a dataframe with only names and lat & lng for each station
stations <- data.frame(mydata[,c(1:3)]) %>% 
  drop_na()
stations [, 2] <- as.numeric(as.character(stations[,2]))
stations [, 3] <- as.numeric(as.character(stations[,3]))
summary(stations)


minlat <- min(stations$lat)
maxlat <- max(stations$lat)
minlng <- min(stations$lon)
maxlng <- max(stations$lon)

stations %>% 
  group_by(sensor_id, lat, lon) %>% 
  leaflet() %>% 
  addTiles() %>%
  fitBounds(~minlng, ~minlat, ~maxlng, ~maxlat) %>% 
  addCircles(lng = ~lon, lat = ~lat,
             radius = 150, weight = 5, color = "black",
             fillColor = "red", fillOpacity = 0.7,  
             popup = ~paste("<b>", sensor_id)
  ) 


mydata%>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarise(no_readings = n()) %>% 
  DT::datatable()

library(lubridate)

View(mydata)


mydata$time <- factor(mydata$time)
mydata$time <- hms(as.character(mydata$time))

mydata %>% 
  group_by(hour(time)) %>% 
  summarise(no_readings = n()) %>% 
  DT::datatable()


mydata%>% 
  group_by(hour(time)) %>%
  summarise(mean_P1 = mean(P1, na.rm = TRUE), mean_P2 = mean(P2, na.rm = TRUE)) %>% 
  gather("pm_no", "mean_pm", -`hour(time)`, factor_key = TRUE) %>% 
  ggplot(aes(x = `hour(time)`, y = mean_pm, fill = pm_no )) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  coord_flip() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
  labs (title = "average value of P1 and P2 per hour", 
        caption = "Data from: https://vazduhgradjanima.rs", 
        x = "month", y = "average pm") +
  scale_fill_brewer(palette="Paired") + 
  theme(legend.position="bottom") 

