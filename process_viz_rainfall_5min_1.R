
###############
# DATA SCREENING


# load packages
library(tidyverse)
library(lubridate)
library(plotly)

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(scattermore)


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2023/20230509_Thinkcity_review/Data')

# STATION NAME
stn_name <- "RF6122064"


#import data
## tideda 5min data
RF_data <- fread(file = "RF6122064 Stor JPS Kota Bharu.CSV", skip = 1,
                 header = TRUE, sep = ",", stringsAsFactors = F)

#set format
str(RF_data)

RF_data$Date <- as.POSIXct(RF_data$Date, format = "%d/%m/%Y %H:%M")

str(RF_data)


#rename column
colnames(RF_data) <- c("Datetime", "Depth")


# REMOVE values

check_values <- RF_data[Depth < 0]
  
#replace value '-999' with NA
#RF_data[Depth == "-999"] <- NA
RF_data[, Depth := as.character(Depth)][Depth == "-999", Depth := NA]

#replace ? with blank
RF_data$Depth = gsub("\\?", "", RF_data$Depth)


#format columns from character to numeric
RF_data$Depth <- as.numeric(as.character(RF_data$Depth))

str(RF_data)

#remove duplicates
duplicates_no <- sum(duplicated(RF_data))

RF_data2 <- unique(RF_data)

#check
max(RF_data2$Depth, na.rm = T)

head(RF_data2, 6)


######
# CHECK VALUES, SCREEN VALUES

## PLOT ALL

gg_rainfall <- RF_data2 %>% 
  ggplot(aes(x = Datetime, y = Depth)) +
  #geom_point(aes(shape = ".", alpha = 0.5), color = "steelblue3", na.rm = T) +
  geom_scattermore(pointsize = 0, na.rm = T, alpha = 0.8, color = "steelblue") +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%y",
                   date_breaks = "1 year",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "5 min Rainfall")

gg_rainfall

#print last plot to file
ggsave(paste0(stn_name, "_RF_5min_all_fil2.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()



# check range of date by filter

gg_rf_check <- RF_data2[Datetime %between% c("2004-01-01 08:05:00","2004-12-31 23:55:00")] %>% 
  ggplot(aes(x = Datetime, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5), color = "steelblue3", na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%y",
                   date_breaks = "1 year",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "5 min Rainfall")

gg_rf_check


ggplotly(gg_rf_check, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


# FILTER PROBLEMATIC DATA ETC
RF_data3 <- RF_data2[!Datetime %between% c("1988-07-05 08:05:00","1990-04-10 00:05:00")]
RF_data3 <- RF_data3[!Datetime %between% c("2003-01-01 08:05:00","2004-06-30 23:55:00")]
RF_data3 <- RF_data3[!Datetime %between% c("2006-09-16 08:05:00","2007-09-12 23:55:00")]




######
# AGGREGATE DATA, SCREEN DATA BY COUNT
## by everyday count, since looking at daily rainfall (288/day)


RF_data_dt <- RF_data3


##data table
#add a year, month, day, hour column to data table
setDT(RF_data_dt)[, Year:= year(Datetime) ]
setDT(RF_data_dt)[, Month:= month(Datetime) ]
setDT(RF_data_dt)[, Day:= day(Datetime) ]
setDT(RF_data_dt)[, Hour:= hour(Datetime) ]
#format columns from character to numeric
RF_data_dt$Year <- as.numeric(as.character(RF_data_dt$Year))
RF_data_dt$Month <- as.numeric(as.character(RF_data_dt$Month))
RF_data_dt$Day <- as.numeric(as.character(RF_data_dt$Day))
RF_data_dt$Hour <- as.numeric(as.character(RF_data_dt$Hour))


#data summary for checking
##hour
RF_data_hr <- RF_data_dt[, .(Depth_hr = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))),
                    by = c("Year", "Month", "Day", "Hour")]

##day
RF_data_day <- RF_data_dt[, .(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))),
                          by = c("Year", "Month", "Day")]


#check
##hour
max(RF_data_hr$cnt) #wrong if more than 60/5 = 12 per hr
min(RF_data_hr$cnt)
max(RF_data_hr$Depth_hr)
##day
max(RF_data_day$cnt) #wrong if more than 24*12 = 288 per day
min(RF_data_day$cnt)
max(RF_data_day$Depth_day)

#check if less than 12 per hr
suspicious <- RF_data_hr[cnt < 12 & cnt != 0]  
#check if less than 288 per day
suspicious <- RF_data_day[cnt < 274]  #95% of 288

#combine ymd into date
RF_data_day$Date <- ymd(paste0(RF_data_day$Year, "-", RF_data_day$Month, "-", RF_data_day$Day))

str(RF_data_day)

##filter days with not enough data first
RF_data_day2 <- RF_data_day[cnt > 274] #95% of 288

# summarize data by month
RF_data_mth <- RF_data_day2[, .(Depth_mth = sum(Depth_day, na.rm = T), cnt = sum(!is.na(Depth_day))),
                       by = c("Year", "Month")]


######
#RECORD HEATMAP BY MONTH

gg_RF_cnt_matrix <- RF_data_mth %>% 
  ggplot(aes(x = Year, y = Month, fill = cnt)) +
  geom_tile(aes(text = paste0(month.abb[Month], ' ', Year,
                              '<br>Record: <b>', cnt, '</b>'))) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 31),
                       name = "Record by month") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     expand = c(0, 0),
                     trans = "reverse",
                     labels = month.abb) +
  scale_x_continuous(name = "Year",
                     breaks = seq(min(RF_data_mth$Year), max(RF_data_mth$Year), by = 1), 
                     expand = c(0, 0),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", size = 6,
                          color = "grey20"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = paste0(stn_name, " Data Availability"))
  #coord_fixed(ratio = 1.5)

gg_RF_cnt_matrix

#print last plot to file
ggsave(paste0(stn_name, "_RF_cnt.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

#plotly
ggplotly(gg_RF_cnt_matrix, tooltip = "text",
         height = 1000,
         width = 900
) %>% 
  layout(legend = list(orientation = "h",
                       y = 1, x = 1))



# GET SELECTED RAINFALL DATA AFTER SCREENING


## join 5min data with selected days
RF_data_dt2 <- RF_data_dt[RF_data_day2, on = .(Year, Month, Day)]



# write to csv
write.table(RF_data_dt2, paste0(stn_name, "_RF5min_clean1.csv"), sep =",", row.names = FALSE)

#####################
# VISUALIZATION


# load packages
library(tidyverse)
library(lubridate)
library(plotly)

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

#library(viridis)
#library(RColorBrewer)
library(paletteer)
#library(gghighlight)


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2023/20230509_Thinkcity_review/Data')

# set names and dates
stn_name <- "RF6122064"

#import data
RF_data_dt2 <- fread(file = paste0(stn_name, "_RF5min_clean1.csv"),
                 header = TRUE, sep = ",", stringsAsFactors = F)


#add time column
RF_data_dt2$Time <- format(RF_data_dt2$Datetime, "%H:%M:%S")

#format columns from character to numeric
RF_data_dt2$Year <- as.numeric(as.character(RF_data_dt2$Year))
RF_data_dt2$Month <- as.numeric(as.character(RF_data_dt2$Month))
RF_data_dt2$Day <- as.numeric(as.character(RF_data_dt2$Day))
RF_data_dt2$Hour <- as.numeric(as.character(RF_data_dt2$Hour))

#set format
str(RF_data_dt2)
RF_data_dt2$Datetime <- as.POSIXct(RF_data_dt2$Datetime, format = "%Y-%m-%d %H:%M")
RF_data_dt2$Date <- as.Date(RF_data_dt2$Date, format = "%Y-%m-%d")
RF_data_dt2$Time <- as.POSIXct(RF_data_dt2$Time, format = "%H:%M")
#RF_data_dt2$Time <- as.POSIXct(RF_data_dt2$Time, format = "%Y-%m-%d %H:%M")

str(RF_data_dt2)
head(RF_data_dt2$Datetime, 6) #check time zone

#check
max(RF_data_dt2$Depth, na.rm = T)

#set dates
max_date <- max(RF_data_dt2$Date)
min_date <- min(RF_data_dt2$Date)


######
# PLOT BY TIME OF DAY FOR MONTH

## set palette 
col_pal <- paletteer_c("grDevices::rainbow", 12)
### rearrange palette
col_pal <- col_pal[c(10,11,12,1,2,3,4,5,6,7,8,9)]
### remove variable
#rm(col_pal)


# set highlight key
RF_data_dt3 <- highlight_key(RF_data_dt2, ~Date)
  

gg_RF_time <- RF_data_dt3 %>% 
  ggplot(aes(x = Time, y = Depth, group = Date, color = as.factor(Month))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "2 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("5-min Rainfall (mm)"),
                     breaks = seq(0, 30, by = 2), 
                     minor_breaks = NULL) + #y axis format
  scale_color_manual("Month", values = col_pal) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0(stn_name, " One Day Rainfall Trend by Month (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 1))

gg_RF_time

#print last plot to file
ggsave(paste0(stn_name, "_RFday_mth_all.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")


ggplotly(gg_RF_time, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  highlight(on = "plotly_click", off = "plotly_doubleclick") %>% 
  rangeslider()



# PLOT BY TIME OF DAY FOR YEAR

gg_RF_time_yr <- RF_data_dt2 %>% 
  ggplot(aes(x = Time, y = Depth, group = Date, color = as.factor(Year))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H",
                   date_breaks = "1 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 30, by = 2), 
                     minor_breaks = NULL) + #y axis format
  scale_color_viridis_d("Year", direction = -1) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0(stn_name, " One Day Rainfall Trend by Year (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 3))

gg_RF_time_yr

#print last plot to file
ggsave(paste0(stn_name, "_RFday_yr_all.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")




######
# PLOT FACET CHART

## BY MONTH

# reorder month
RF_data_dt2$Month2 <- month.name[as.numeric(RF_data_dt2$Month)]

ggfc_RF_time <- lazy_dt(RF_data_dt2) %>% 
  mutate(Month2 = factor(Month2, levels = month.name)) %>% 
  #filter(between(Date, as.Date('2021-01-01'), as.Date('2022-01-01'))) %>% # filter by year for faster generation
  as.data.table() %>% 
  ggplot(aes(x = Time, y = Depth, group = Date, color = as.factor(Year))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "4 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("5-min Rainfall (mm)"),
                     breaks = seq(0, 30, by = 2), 
                     minor_breaks = NULL) + #y axis format
  scale_color_viridis_d("Year", direction = -1) +
  #scale_color_manual("Month", values = col_pal) +
  facet_wrap(~ Month2, ncol = 4) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size=8),
        strip.background = element_rect(colour="white", fill="white"),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 5)) +
  labs(title = paste0(stn_name, " One Day Rainfall Trend by Month (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 3))

ggfc_RF_time

#print last plot to file
ggsave(paste0(stn_name, "_RFday_fct_mth1.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")


## BY YEAR

ggfc_RF_time_yr <- RF_data_dt2 %>% 
  ggplot(aes(x = Time, y = Depth, group = Date, color = as.factor(Month))) +
  geom_line(alpha = 0.5, size = 0.2,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "6 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("5-min Rainfall (mm)"),
                     breaks = seq(0, 30, by = 5), 
                     minor_breaks = NULL) + #y axis format
  scale_color_manual("Month", values = col_pal) +
  facet_wrap(~ Year, ncol = 8) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 5)) +
  labs(title = paste0(stn_name, " One Day Rainfall Trend by Year (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 1))

ggfc_RF_time_yr

#print last plot to file
ggsave(paste0(stn_name, "_RFday_fct_yr1.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")


######
# CUMULATIVE RAINFALL


## cum sum
RF_data_cumsum <- RF_data_dt2[, Depth_cum := cumsum(Depth), by = .(Year, Month, Day)]



# PLOT ALL CUMULATIVE VALUES

# set highlight key
RF_data_cumsum2 <- highlight_key(RF_data_cumsum, ~Date)

ggcm_RF_time <- RF_data_cumsum2 %>% 
  ggplot(aes(x = Time, y = Depth_cum, group = Date, color = as.factor(Month))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "2 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name = paste("Cumulative 5-min rainfall (mm)"),
                     breaks = seq(0, 500, by = 50), 
                     minor_breaks = NULL) + #y axis format
  scale_color_manual("Month", values = col_pal) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0(stn_name, " One Day Cumulative Rainfall Trend (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 1))

ggcm_RF_time

#print last plot to file
ggsave(paste0(stn_name, "_RFcumday_all.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(ggcm_RF_time, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider() %>% 
  highlight(on = "plotly_click", off = "plotly_doubleclick") 



# PLOT FACET CHART

## BY MONTH

ggfcm_RF_time <- lazy_dt(RF_data_cumsum) %>% 
  mutate(Month2 = factor(Month2, levels = month.name)) %>% 
  #filter(between(Date, as.Date('2021-01-01'), as.Date('2022-01-01'))) %>% # filter by year for faster generation
  as.data.table() %>% 
  ggplot(aes(x = Time, y = Depth_cum, group = Date, color = as.factor(Year))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "4 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cumulative 5-min rainfall (mm)"),
                     breaks = seq(0, 500, by = 50), 
                     minor_breaks = NULL) + #y axis format
  scale_color_viridis_d("Year", direction = -1) +
  #scale_color_manual("Month2", values = col_pal) +
  facet_wrap(~ Month2, ncol = 4) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 5)) +
  labs(title = paste0(stn_name, " One Day Cumulative Rainfall Trend (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 3))

ggfcm_RF_time

#print last plot to file
ggsave(paste0(stn_name, "_RFcumday_fc_mth1.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")



## BY YEAR

ggfcy_RF_time <- RF_data_cumsum %>% 
  ggplot(aes(x = Time, y = Depth_cum, group = Date, color = as.factor(Month))) +
  geom_line(alpha = 0.5, size = 0.5,  na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Time of day", date_labels = "%H:00",
                   date_breaks = "6 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cumulative 5-min rainfall (mm)"),
                     breaks = seq(0, 500, by = 50), 
                     minor_breaks = NULL) + #y axis format
  scale_color_manual("Month", values = col_pal) +
  facet_wrap(~ Year, ncol = 8) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 5)) +
  labs(title = paste0(stn_name, " One Day Cumulative Rainfall Trend (", min_date, " to ", max_date, ")")) +
  guides(alpha = "none", shape = "none", colour = guide_legend(nrow = 1))

ggfcy_RF_time

#print last plot to file
ggsave(paste0(stn_name, "_RFcumday_fc_yr1.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")

######


