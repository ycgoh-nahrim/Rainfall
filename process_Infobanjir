# COMBINE XLSX FILES

library(openxlsx)

path <- "J:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR/KL"
merge_file_name <- "J:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR/All_KL.csv"

filenames_list <- list.files(path = path, full.names=TRUE)

All <- lapply(filenames_list, function(filename){
  print(paste("Merging", filename, sep = " "))
  read.xlsx(filename, sheet = 1, startRow = 2)
})

df <- do.call(rbind.data.frame, All)

#write to csv/xlsx

#write.xlsx(df,merge_file_name)

write.table(df, merge_file_name, sep=",", row.names = FALSE, 
            col.names = c("No.","Station ID","Station Code","Station Name",
                          "Date","Time","WL_raw","WL_raw_ecm","WL_clean",
                          "Rainfall","CDR_raw","CDR_raw_ecm","CDR_clean",
                          "CAR_raw","CAR_raw_ecm","CAR_clean"))

#write.csv(df, merge_file_name)


########################################
# COMBINE CSV FILES

#set working directory
setwd('J:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR/All')

#list all files in working directory
filenames<-list.files()

#read all csv in file list above (skip 1 header lines)
all <- lapply(filenames, function(i) cbind(read.csv(i, header = FALSE, skip = 1)))

#combine into dataframe
df <- do.call(rbind.data.frame, all)

#write to one table/csv, assign column names
write.table(df,"IB_Raw_KLSel.csv",sep=",",row.names = FALSE, 
            col.names = c("No.","Station ID","Station Code","Station Name",
                          "Date","Time","WL_raw","WL_raw_ecm","WL_clean",
                          "Rainfall","CDR_raw","CDR_raw_ecm","CDR_clean",
                          "CAR_raw","CAR_raw_ecm","CAR_clean"))


########################################
# CLEAN & REORGANIZE DATA

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR')

#import raw data from Infobanjir
raw_data = read.csv("IB_Raw_KLSel.csv", header = T, sep = ",")

#check data type, columns
head(raw_data, 6)

#replace value '-9999', - with NA
raw_data[raw_data == "-9999.0"] <- NA
raw_data[raw_data == "-"] <- NA


#split column
combined_data <- raw_data %>% 
  separate(Station.ID, c("RF_stn", "WL_stn"), " // ") %>% 
  select(-"No.") #remove column

#replace RF & WL with blank
combined_data$RF_stn = gsub("(RF)", "", combined_data$RF_stn, fixed = T)
combined_data$WL_stn = gsub("(WL)", "", combined_data$WL_stn, fixed = T)

#combine columns
combined_data <- combined_data %>% 
  unite(Datetime, c(Date, Time), sep = " ")


#format date
combined_data$Datetime <- as.POSIXct(combined_data$Datetime, format = "%d/%m/%Y %H:%M")

#raindata$Datetime <- as.Date(raindata$Datetime, format = "%Y-%m-%d")

str(combined_data)

#prepare rainfall data
RF_data <- combined_data %>% 
  select(RF_stn, Station.Code, Station.Name, Datetime, Rainfall) %>% 
  filter(RF_stn != "No Data")


#format column from character to numeric
RF_data$Rainfall <- as.numeric(as.character(RF_data$Rainfall))

str(RF_data)

#rename columns
RF_data <- RF_data %>% 
  rename(Depth = Rainfall)

#remove negative and NA
RF_data_negative <- RF_data %>% 
  filter(Depth < 0)

RF_data <- RF_data %>% 
  filter(Depth > 0)

# write to csv
write.table(RF_data2, "IB_RF_202112_clean.csv", sep=",", row.names = FALSE)



###
#prepare water level data
WL_data <- combined_data %>% 
  select(WL_stn, Station.Code, Station.Name, Datetime, WL_raw) %>% 
  filter(WL_stn != "No Data")


#format column from character to numeric
WL_data$WL_raw <- as.numeric(as.character(WL_data$WL_raw))

str(WL_data)

#rename columns
WL_data <- WL_data %>% 
  rename(Level = WL_raw)

#remove negative and NA
WL_data_negative <- WL_data %>% 
  filter(Level < 0)

WL_data <- WL_data %>% 
  filter(Level > 0)

# write to csv
write.table(WL_data2, "IB_WL_202112_clean.csv", sep=",", row.names = FALSE)



###############
# VISUALIZATION

# RAINFALL DATA

#import data
RF_data = read.csv("IB_RF_202112_clean.csv", header = T, sep = ",")

#set format
str(RF_data)
RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")


#remove duplicates, recount RF_data_hr

duplicates_no <- sum(duplicated(RF_data))

RF_data2 <- unique(RF_data)

#check hourly count

RF_data_hr <- RF_data2 %>% 
  mutate(hour = hour(Datetime), Date = date(Datetime)) %>% 
  group_by(RF_stn, Station.Name, Date, hour) %>% 
  summarise(Depth_hr = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth)))

max(RF_data_hr$cnt) #wrong if more than 4 per hour

#check if more than 4 per hour
suspicious <- RF_data_hr %>% 
  filter(cnt > 4)
suspicious2 <- RF_data %>% 
  filter(RF_stn == 3013002)

#check daily count
RF_data_day <- RF_data2 %>% 
  mutate(Date = date(Datetime)) %>% 
  group_by(RF_stn, Station.Name, Date) %>% 
  summarise(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth)))

max(RF_data_day$cnt) #wrong if more than 24*4=96 per day

#check if more than 96 per day
suspicious <- RF_data_day %>% 
  filter(cnt > 96)



#RECORD HEATMAP

gg_RF_cnt_matrix <- RF_data_day %>% 
  ggplot(aes(x = Date, y = Station.Name, fill = cnt)) +
  geom_tile(aes(text = paste0(Date, '<br>', Station.Name,
                              '<br>Record: <b>', cnt, '</b>'))) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 96),
                       name = "Daily count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station Name"
                     #breaks = seq(1, 12, by = 1), 
                     #minor_breaks = NULL, 
                     #expand = c(0, 0)
                     #labels=Station.Name
                     ) +
  scale_x_date(name = "Date",
               breaks = "1 day", 
               date_labels = "%b %d",
               expand = c(0, 0),
               minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", size = 6,
                          color = "grey20"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "Data Availability") 
  #coord_fixed(ratio = 1.5)

gg_RF_cnt_matrix

#print last plot to file
ggsave(paste0("IB_KLSel_RFcount_day.jpg"), dpi = 300,
       width = 8, height = 12, units = "in")

#plotly
ggplotly(gg_RF_cnt_matrix, tooltip = "text",
         height = 1000,
         width = 900
) %>% 
  layout(legend = list(orientation = "h",
                       y = 1, x = 1))


#VALUES CHART

## 15 min

gg_rainfall <- RF_data2 %>% 
  ggplot(aes(x = Datetime, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = RF_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%b %d",
               date_breaks = "1 day",
               #date_minor_breaks = "1 day",
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "15 min Rainfall (December 2021)")

gg_rainfall

#print last plot to file
ggsave(paste0("IB_KLSel_RFvalues15min.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


## hourly

#combine columns
RF_data_hr2 <- RF_data_hr %>% 
  mutate(hour2 = paste0(hour, ":00")) %>% 
  unite(Datetime, c(Date, hour2), sep = " ")

#date format
RF_data_hr2$Datetime <- as.POSIXct(RF_data_hr2$Datetime, format = "%Y-%m-%d %H:%M")

gg_rainfall_hr <- RF_data_hr2 %>% 
  filter(Datetime >= "2021-12-17" & Datetime <= "2021-12-20") %>% 
  ggplot(aes(x = Datetime, y = Depth_hr)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = RF_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Hourly Rainfall (mm)"),
                     breaks = seq(0, 100, by = 10),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Hourly Rainfall (December 2021)")

gg_rainfall_hr

#print last plot to file
ggsave(paste0("IB_KLSel_RFvalueshr_dec18.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall_hr, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


## daily

#date format
RF_data_day$Date <- as.POSIXct(RF_data_day$Date, format = "%Y-%m-%d")

gg_rainfall_day <- RF_data_day %>% 
  ggplot(aes(x = Date, y = Depth_day)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = RF_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Date", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Daily Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Daily Rainfall (December 2021)")

gg_rainfall_day

#print last plot to file
ggsave(paste0("IB_KLSel_RFvaluesday.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall_day, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#clean data

RF_data2 <- RF_data %>% 
  filter(RF_stn != "MOCKUPWPKL")

######
# WATER LEVEL DATA

#import data
WL_data = read.csv("IB_WL_202112_clean.csv", header = T, sep = ",")

#set format
str(WL_data)
WL_data$Datetime <- as.POSIXct(WL_data$Datetime, format = "%Y-%m-%d %H:%M")


#remove duplicates, recount RF_data_hr

duplicates_no <- sum(duplicated(WL_data))

WL_data2 <- unique(WL_data)

#check hourly count

WL_data_hr <- WL_data2 %>% 
  mutate(hour = hour(Datetime), Date = date(Datetime)) %>% 
  group_by(WL_stn, Station.Name, Date, hour) %>% 
  summarise(Level_hr = sum(Level, na.rm = T), cnt = sum(!is.na(Level)))

max(WL_data_hr$cnt) #wrong if more than 4 per hour

#check if more than 4 per hour
suspicious <- WL_data_hr %>% 
  filter(cnt > 4)
suspicious2 <- WL_data %>% 
  filter(RF_stn == 3013002)

#check daily count
WL_data_day <- WL_data2 %>% 
  mutate(Date = date(Datetime)) %>% 
  group_by(WL_stn, Station.Name, Date) %>% 
  summarise(Level_day = sum(Level, na.rm = T), cnt = sum(!is.na(Level)))

max(WL_data_day$cnt) #wrong if more than 24*4=96 per day

#check if more than 96 per day
suspicious <- WL_data_day %>% 
  filter(cnt > 96)



#RECORD HEATMAP

gg_WL_cnt_matrix <- WL_data_day %>% 
  ggplot(aes(x = Date, y = Station.Name, fill = cnt)) +
  geom_tile(aes(text = paste0(Date, '<br>', Station.Name,
                              '<br>Record: <b>', cnt, '</b>'))) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 96),
                       name = "Daily count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station Name"
                   #breaks = seq(1, 12, by = 1), 
                   #minor_breaks = NULL, 
                   #expand = c(0, 0)
                   #labels=Station.Name
  ) +
  scale_x_date(name = "Date",
               breaks = "1 day", 
               date_labels = "%b %d",
               expand = c(0, 0),
               minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", size = 10,
                          color = "grey20"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "Data Availability") 
#coord_fixed(ratio = 1.5)

gg_WL_cnt_matrix

#print last plot to file
ggsave(paste0("IB_KLSel_WLcount_day.jpg"), dpi = 300,
       width = 8, height = 10, units = "in")

#plotly
ggplotly(gg_WL_cnt_matrix, tooltip = "text",
         height = 1000,
         width = 900
) %>% 
  layout(legend = list(orientation = "h",
                       y = 1, x = 1))


#VALUES CHART

gg_waterlevel <- WL_data2 %>% 
  ggplot(aes(x = Datetime, y = Level)) +
  geom_line(aes(alpha = 0.5, color = WL_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Water Level (m)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Water Level (December 2021)")

gg_waterlevel

#print last plot to file
ggsave(paste0("IB_KLSel_WLvalues15min.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_waterlevel, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#clean data

WL_data2 <- WL_data %>% 
  filter(WL_stn != "MOCKUPWPKL")


