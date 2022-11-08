# load packages
library(tidyverse)
library(lubridate)
library(scales)
library(ggforce)
library(plotly)
library(sf)

library(extrafont)
library(openxlsx)
library(gghighlight)
library(viridis)
library(ggpubr)


# set strings as factors to false
options(stringsAsFactors = FALSE)

###############################################################################
#INPUT
###############################################################################

#select stations by state
type_sel <- "RF"

#minimum record per year for analysis
min_cnt_yr <- 347

#acceptable number of years of record for further analysis
no_yr <- 10

#minimum year
year_min <- 1970
#maximum year
year_max <- 2020
#year interval
year_interval <- 5

#chart subtitle
#chart_subtitle <- paste0("Station ", station_no, ": ", station_name)
chart_subtitle <- "chart_subtitle"

#chart caption
data_source <- "params$Data_source"
chart_caption <- paste0("Data source: ", data_source)

#font
font_family <- "Roboto"


#set working directory
#set filename
filename2 <- paste0("RF_Klang")
# get current working directory
working_dir <- getwd()
dir.create(filename2)
setwd(filename2)


###########################
##OPTIONS (rainfall station data or from database) #############
# average daily rain data



#from database
rain_db <- read.csv(file = "C:/Users/User/Documents/Documents/hydrology/Data/RF_1d_PM_20200720_clean1.csv",
                    header = TRUE, sep=",")
str(rain_db)



#station list
stn_db <- read.csv(file = "C:/Users/User/Documents/Documents/hydrology/Data/JPS_Klang_rev3.csv",
                    header = TRUE, sep=",")
#stn_db <- read.csv(file = paste0(working_dir,"/", "JPS_RF_stn_PM.csv"),
#                   header = TRUE, sep=",")
str(stn_db)

###########################
#check duplicates in rain_db

rain_db2 <- rain_db %>% 
  filter(Stn_no == 3116004 & Type == "Manual")

#to get unique values
rain_db2 <- rain_db %>% 
  distinct(Stn_no, Date, Depth, Type, .keep_all = TRUE)




###########################
#SELECT STATION LIST BY TYPE
stn_sel <- stn_db %>%
  filter(Jenis == type_sel) %>% #check field name
  arrange(STATION_NO)


# join database with station list

## select data
raindata_sel <- rain_db %>%
  right_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, Date, Depth)

str(raindata_sel)

#if date is not in date format
#raindata_sel$Date <- as.Date(raindata_sel$Date, format = "%d/%m/%Y")
raindata_sel$Date <- as.Date(raindata_sel$Date, format = "%Y-%m-%d")
#format column from character to numeric
raindata_sel$Depth <- as.numeric(as.character(raindata_sel$Depth))



# add columns for data aggregation
## add a year column to data.frame
raindata_sel <- raindata_sel %>%
  mutate(Year = year(Date))
## add a month column to data.frame
raindata_sel <- raindata_sel %>%
  mutate(Month = month(Date))
## add a day column to data.frame
raindata_sel <- raindata_sel %>%
  mutate(Day = day(Date))


###############################################################################
#DATA CLEANING
#if skip data cleaning, then
raindata_sel2 <- raindata_sel
###############################################################################

# CHECK RECORDS - CLEAN 1

#dataset: raw

# remove years with missing data

## count non-missing data
raindata_cnt <- raindata_sel %>%
  group_by(Stn_no, Year) %>%
  summarise(sum_depth = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))) 



#format column from character to numeric
raindata_cnt$cnt <- as.numeric(as.character(raindata_cnt$cnt))

# total number of stations
#total_stn <- length(unique(raindata_cnt2$Stn_no))

# join data for stn name
raindata_cnt2 <- raindata_cnt %>%
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, sum_depth, cnt) %>% 
  rename(Stn_name = STATION_NA) %>% 
  filter(!is.na(Year))

#format column from  numeric to character
raindata_cnt2$Stn_no <- as.character(raindata_cnt2$Stn_no)




###########
# remove stations with insufficient years of complete data

# list all stations with number of complete years
raindata_cnt_yr <- raindata_cnt2 %>% 
  filter(cnt >= min_cnt_yr) %>% 
  group_by(Stn_no, Stn_name) %>% 
  summarise(Yr_complete = n()) %>% 
  filter(Yr_complete >=no_yr)


# remove stations with insufficient years
raindata_cnt_clean1 <- raindata_cnt2 %>% 
  filter(cnt >= min_cnt_yr) %>% 
  right_join(raindata_cnt_yr, by = c("Stn_no", "Stn_name")) %>%
  select(Stn_no, Stn_name, Year, sum_depth, cnt)


#format column from character to numeric
raindata_cnt_clean1$Stn_no <- as.numeric(raindata_cnt_clean1$Stn_no)


## select stations with complete data for each year
raindata_sel2 <- raindata_sel %>%
  right_join(raindata_cnt_clean1, by = c("Year", "Stn_no")) %>%
  select(Stn_no, Date, Year, Month, Day, Depth) %>% 
  filter(!is.na(Depth))


###########################

# DATA COUNT HEATMAP

#dataset: raw

rain_cnt_matrix <- raindata_cnt2 %>%
  ggplot(aes(x = Year, y = Stn_name)) +
  geom_tile(aes(fill = cnt, text = paste0(Stn_no, '<br>', Stn_name,
                                          '<br>Year: ', Year,
                                          '<br>Record count: <b>', cnt, '</b>'))) +
  #scale_fill_distiller(palette="YlGnBu", direction=1, name = "Count") +
  #scale_fill_gradient(name = "Count per year",
  #                    low = "yellow",high = "steelblue") +
  scale_fill_viridis(name = "Record per year",
                     limits = c(0, 366),
                     direction = -1) +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station name"
                   #label = paste(raindata_cnt3$Stn_no, raindata_cnt3$Stn_name)
                   #minor_breaks = NULL
                   ) +
  scale_x_continuous(name = "Year",
                     breaks = seq(year_min, year_max, by = year_interval), 
                     expand = c(0, 0),
                     minor_breaks = NULL) + 
  theme(text=element_text(color="grey20", size = 6),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "Data Completeness"
       #subtitle = paste0(chart_subtitle),
       #caption = paste0(chart_caption)
       ) 

rain_cnt_matrix

#print last plot to file
ggsave(paste0(filename2, "_cnt_yr_heatmap_raw.jpg"), dpi = 300, 
       width = 30, height = 20, units = "cm")

ggplotly(rain_cnt_matrix, tooltip = "text",
         height = 800,
         width = 1200
         )




###########################

# DATA SUMMARY

#dataset: clean 1

#summarise data

# calculate the total rain (mm) for each year
Rain_sum_yr <- raindata_sel2 %>%
  select(Stn_no, Year, Depth) %>%
  group_by(Stn_no, Year) %>%
  summarise(sum_Depth = sum(Depth, na.rm = T)) 

#join with station name
Rain_sum_yr <- Rain_sum_yr %>% 
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, sum_Depth) %>% 
  rename(Stn_name = STATION_NA)



###########################

# DAILY RAINFALL

#dataset: clean 1

#plot daily rainfall above value
rain_daily_above <- 0


# plot daily data
rain_daily_plot <- raindata_sel2 %>% 
  filter(Depth > rain_daily_above) %>% 
  ggplot(aes(x = Date, y = Depth, color = as.character(Stn_no),
             text = paste0("Station no: ", Stn_no,
                           "<br> Depth:", Depth,  
                           " mm <br> Date: ", Date))) +
  geom_point(alpha = 0.5, size = 1) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Year", date_labels = "%Y",
               date_breaks = "year",
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Daily rainfall (mm)"),
                     #breaks = seq(0, 3500, by = 500), 
                     #limits = c(0, 400),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        #legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = paste0("Daily rainfall"),
       subtitle = paste0(rain_daily_above, " mm and above"))

rain_daily_plot

#print last plot to file
ggsave(paste0(filename2, "_check_daily.jpg"), dpi = 300,
       width = 30, height = 20, units = "cm")

#plotly
ggplotly(rain_daily_plot, tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


###########################

# ANNUAL RAINFAll

#dataset: clean 1

rain_annual_ly <- Rain_sum_yr %>% 
  ggplot(aes(x = Year, y = sum_Depth, color = as.character(Stn_no), group = Stn_no)) +
  geom_line(aes(text = paste0(Stn_no, '<br>', Stn_name, '<br>',
                              'Year: ', Year, '<br>Total rainfall: ',
                              sum_Depth, ' mm'))) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     #breaks = seq(0, 5000, by = 1000), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual rainfall (mm)"),
                     #breaks = seq(0, 3500, by = 500), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(text=element_text(color="grey20"),
        #legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Annual rainfall",
       color = "Station ID")

rain_annual_ly

#print last plot to file
ggsave(paste0(filename2, "_check_annual.jpg"), dpi = 300, 
       width = 30, height = 20, units = "cm")

#plotly
ggplotly(rain_annual_ly, tooltip = "text",
         width = 1200, height = 800)


###########################

# REMOVE OUTLIERS - CLEAN 2

#dataset: clean 1


#clean data from checking previous charts


#remove low and/or high outlier from total annual rainfall
## by year
Rain_sum_yr2 <- Rain_sum_yr %>% 
  #filter(sum_Depth > 1000 & sum_Depth < 6000) %>% 
  filter(sum_Depth > 1000)


#remove high outlier from daily rainfall
## by depth
raindata_sel3 <- raindata_sel2 %>%
  right_join(Rain_sum_yr2, by = c("Year", "Stn_no")) %>%
  select(Stn_no, Date, Year, Month, Day, Depth) %>% 
  filter(Depth < 300)


# calculate the total rain (mm) for each year
Rain_sum_yr3 <- raindata_sel3 %>%
  select(Stn_no, Year, Depth) %>%
  group_by(Stn_no, Year) %>%
  summarise(sum_Depth = sum(Depth, na.rm = T)) 

#join with station name
Rain_sum_yr3 <- Rain_sum_yr3 %>% 
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, sum_Depth) %>% 
  rename(Stn_name = STATION_NA)


# recalculate data count
raindata_cnt_clean2 <- raindata_sel3 %>% 
  group_by(Stn_no, Year) %>%
  summarise(sum_depth = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))) %>% 
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, sum_depth, cnt) %>% 
  rename(Stn_name = STATION_NA)
  


###########################

# DATA COUNT HEATMAP 2

#dataset: clean 2


# plot heatmap

rain_cnt_matrix2 <- raindata_cnt_clean2 %>%
  ggplot(aes(x = Year, y = Stn_name)) +
  geom_tile(aes(fill = cnt, text = paste0(Stn_no, '<br>', Stn_name,
                                          '<br>Year: ', Year,
                                          '<br>Record count: <b>', cnt, '</b>'))) +
  #scale_fill_distiller(palette="YlGnBu", direction=1, name = "Record per year") +
  #scale_fill_gradient(name = "Count per year",
  #                    low = "yellow",high = "steelblue") +
  scale_fill_viridis(name = "Record per year",
                     limits = c(0, 366),
                     direction = -1) +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station name"
                   #label = paste(raindata_cnt3$Stn_no, raindata_cnt3$Stn_name)
                   #minor_breaks = NULL
  ) +
  scale_x_continuous(name = "Year",
                     breaks = seq(year_min, year_max, by = year_interval), 
                     expand = c(0, 0),
                     minor_breaks = NULL) + 
  theme(text=element_text(color="grey20", size = 6),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "Data Completeness"
       #subtitle = paste0(chart_subtitle),
       #caption = paste0(chart_caption)
  ) 

rain_cnt_matrix2

#print last plot to file
ggsave(paste0(filename2, "_cnt_yr_heatmap.jpg"), dpi = 300, 
       width = 30, height = 20, units = "cm")

ggplotly(rain_cnt_matrix2, tooltip = "text",
         height = 800,
         width = 1200
)


###########################

# ANNUAL MAXIMUM DAILY RAINFALL

#dataset: clean 2


# get annual maximum daily data
rain_daily_max <- raindata_sel3 %>% 
  group_by(Stn_no, Year) %>%
  summarise(max_Depth = max(Depth, na.rm = T),
            max_Date = Date[which.max(Depth)])



# plot annual maximum
rain_daily_max_plot <- rain_daily_max %>% 
  ggplot(aes(x = Year, y = max_Depth, color = as.character(Stn_no),
             text = paste0("Station no: ", Stn_no,
                           "<br> Max Depth:", max_Depth,  
                           " mm <br> Max Date: ", max_Date))) +
  geom_point(alpha = 0.5, size = 1) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     breaks = seq(year_min, year_max, by = year_interval), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Maximum daily rainfall (mm)"),
                     #breaks = seq(0, 3500, by = 500), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        #legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0("Maximum daily rainfall"),
       color = "Station ID")

rain_daily_max_plot

#print last plot to file
ggsave(paste0(filename2, "_annual_max_daily.jpg"), dpi = 300,
       width = 30, height = 20, units = "cm")


#plotly
ggplotly(rain_daily_max_plot, tooltip = "text",
         width = 1200, height = 800)


###########################

# SINGLE MASS CURVE FOR ANNUAL DATA

#dataset: clean 2

#cumulative sum
Rain_sum_yr_cum <- Rain_sum_yr3 %>% 
  group_by(Stn_no) %>% 
  mutate(cum_depth = cumsum(sum_Depth),
         num = row_number())


rain_mass_single_annual <- Rain_sum_yr_cum %>% 
  ggplot(aes(x = num, y = cum_depth, color = as.character(Stn_no), #group = 1,
             text = paste0("Station no: ", Stn_no)
  )
  ) +
  geom_line(size = 0.5, alpha = 1, na.rm = T) +
  theme_bw(base_size = 10) +
  #scale_x_date(name= "Year", date_labels = "%Y", date_breaks = "year", minor_breaks = NULL) + #x axis format
  scale_x_continuous(name= "num", 
                     #breaks = seq(0, max(Rain_sum_yr_cum$num, na.rm = T), by = 5000), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cummulative annual rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  labs(title = "Single mass curves for annual rainfall",
       color = "Station ID")

rain_mass_single_annual

#print last plot to file
ggsave(paste0(filename2, "_mass_curve_single_annual.jpg"), dpi = 300,
       width = 30, height = 20, units = "cm")


#plotly
ggplotly(rain_mass_single_annual, tooltip = "text",
         width = 1200, height = 800)



###########################

# SUMMARIZE DATA FOR ANNUAL

#dataset: clean 2


# calculate the total rain (mm) for long term
Rain_avg_yr_lt <- Rain_sum_yr3 %>%
  select(Stn_no, sum_Depth) %>%
  group_by(Stn_no) %>%
  summarise(avg_Depth_lt = mean(sum_Depth, na.rm = T)) 

###########

# SUMMARIZE DATA FOR MONTH

#dataset: clean 2

# calculate the total rain (mm) for each month, each year
Rain_mth_cnt <- raindata_sel3 %>%
  select(Stn_no, Year, Month, Depth) %>%
  group_by(Stn_no, Year, Month) %>%
  summarise(cnt = sum(!is.na(Depth)))

# select months with more than 28 days to sum
Rain_mth_cnt_list <- Rain_mth_cnt %>%
  filter(cnt >= 28)

# sum all months
Rain_sum_mth_all <- raindata_sel3 %>%
  select(Stn_no, Year, Month, Depth) %>%
  group_by(Stn_no, Year, Month) %>%
  summarise(sum_Depth = sum(Depth, na.rm = T))

# select months
Rain_sum_mth_all2 <- Rain_sum_mth_all %>%
  right_join(Rain_mth_cnt_list, by = c("Year", "Month", "Stn_no")) %>%
  select(Stn_no, Year, Month, sum_Depth)


# long term monthly average
Rain_sum_mth_lt <- Rain_sum_mth_all2 %>%
  group_by(Stn_no, Month) %>%
  summarise(mth_Depth = mean(sum_Depth))


# join data for stn name
Rain_sum_mth_all2 <- Rain_sum_mth_all2 %>%
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, Month, sum_Depth) %>% 
  rename(Stn_name = STATION_NA)



###########################
#boxplot label function


ggplot_box_legend <- function(family = font_family){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, 
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", 
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.25,
                 color = "grey40") +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values), 
                 width = 0.3, 
                 #fill = "lightgrey",
                 color = "steelblue3",
                 outlier.colour="red") +
    # number of observations
    #geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    #geom_text(aes(x = 1.17, y = 950,
    #              label = "Number of values"),
    #          fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    # mean point
    geom_point(aes(x = 1, y = 390),
               shape = 4, size = 2.5) + 
    geom_text(aes(x = 1.2, 
                  y =  390, 
                  label = "Mean"), 
              vjust = 0.5, fontface = "bold") +
    # quartiles lines
    geom_segment(aes(x = 2.3, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["75th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    # interquartile label
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]), 
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.3) +
    # whiskers lines
    geom_segment(aes(x = 1, xend = 1, 
                     y = ggplot_output[["upper_whisker"]], 
                     yend = ggplot_output[["lower_whisker"]]),
                 color = "grey58") +
    # whiskers label
    geom_text(aes(x = c(1.17,1.17), 
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]), 
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    # outlier labels
    geom_text(aes(x = c(1.17), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = "Outlier"), 
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.55), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = ": Value is >1.5 times and"), 
              vjust = 0.5) +
    geom_text(aes(x = 1.17, 
                  y = ggplot_output[["lower_dots"]], 
                  label = "<3 times the interquartile range\nbeyond either end of the box"), 
              vjust = 1.5) +
    # quartiles labels
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]]+1, 
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.6,0.4), 
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 11)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot) 
  
}

legend_plot <- ggplot_box_legend()


###########################

# ANNUAL RAINFALL VARIABILITY

#dataset: clean 2


#boxplot

Rain_sum_yr_box <- Rain_sum_yr3 %>%
  ggplot(aes(x = Year, y = sum_Depth, group = Year
             #alpha = 0.8
             #color = as.factor(month)
  )) +
  
  geom_boxplot(outlier.colour="red", 
               outlier.shape=20,
               outlier.size=3,
               #fill = "white", 
               color = "steelblue3") +
  geom_point(size = 0.9, alpha = 0.5, color = "grey80") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Year",
                     breaks = seq(year_min, year_max, by = year_interval), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual rainfall (mm)"),
                     #breaks = seq(0, 350, by = 50), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  stat_summary(fun=mean, aes(shape = "Mean"), #mean value
               geom="point", size=2.5, show.legend = F) +
  stat_boxplot(geom ='errorbar', show.legend = F, width = 0.5,
               color = "grey58") +
  scale_shape_manual(name =NULL, values=c("Mean"=4)) +
  #scale_color_brewer(palette="Set3") +
  theme(text=element_text(
                          #family=font_family, 
                          color="grey20"),
        panel.grid.major.x = element_blank()) +
  labs(title = "Annual rainfall variability")


#arrange chart and tables in 1 page
rain_annual_box <- ggarrange(Rain_sum_yr_box, legend_plot,
                       ncol = 2, nrow = 1, widths = c(3,1))



#print last plot to file
ggsave(paste0(filename2, "_rain_boxplot_annual.jpg"), 
       dpi = 300, width = 320, height = 210, units = "mm",
       rain_annual_box)


ggplotly(Rain_sum_yr_box, #tooltip = "text",
         width = 1000, height = 500) 


###########################

# MONTHLY RAINFALL VARIABILITY

#dataset: clean 2


#boxplot

#plot boxplot
Rain_sum_mth_box <- Rain_sum_mth_all2 %>%
  ggplot(aes(x = Month, y = sum_Depth, group = Month
             #alpha = 0.8
             #color = as.factor(month)
  )) +
  geom_boxplot(outlier.colour="red", 
               outlier.shape=20,
               outlier.size=3,
               #fill = "white", 
               color = "steelblue3") +
  #geom_point(size = 0.9, alpha = 0.5, color = "grey80") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     labels=month.abb) + #x axis format
  scale_y_continuous(name= paste("Monthly rainfall (mm)"),
                     breaks = seq(0, max(Rain_sum_mth_all2$sum_Depth), by = 100), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  stat_summary(fun=mean, aes(shape = "Mean"), #mean value
               geom="point", size=2.5, show.legend = F) +
  stat_boxplot(geom ='errorbar', show.legend = F, width = 0.5,
               color = "grey58") +
  scale_shape_manual(name =NULL, values=c("Mean"=4)) +
  #scale_color_brewer(palette="Set3") +
  theme(text=element_text(
    #family=font_family, 
    color="grey20"),
    panel.grid.major.x = element_blank()) +
  labs(title = "Monthly rainfall variability")


#arrange chart and tables in 1 page
rain_mth_box <- ggarrange(Rain_sum_mth_box, legend_plot,
                             ncol = 2, nrow = 1, widths = c(3,1))



#print last plot to file
ggsave(paste0(filename2, "_rain_boxplot_mth.jpg"), 
       dpi = 300, width = 297, height = 210, units = "mm",
       rain_mth_box)


ggplotly(Rain_sum_mth_box, #tooltip = "text",
         width = 1000, height = 500) 






###########################
# DOUBLE MASS CURVE?






###########################

# AVERAGE ANNUAL RAINFALL

#dataset: clean 2

#summarize
rain_annual_avg <- Rain_sum_yr3 %>% 
  group_by(Year) %>% 
  summarise(avg_depth = mean(sum_Depth))


#long-term average annual rainfall
rain_annual_avg_lt <- mean(rain_annual_avg$avg_depth, na.rm = T)

#plot

rain_annual_avg_plot <- rain_annual_avg %>%
  ggplot(aes(x = Year, y = avg_depth)) +
  geom_bar(stat = "identity", fill = "skyblue2", alpha = 0.8,
           aes(text = paste0("Average total rainfall for year <b>", rain_annual_avg$Year, "</b> is <b>", 
                         sprintf("%0.1f", rain_annual_avg$avg_depth), " mm</b>"))) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     breaks = seq(year_min, year_max, by = 1), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Average annual rainfall (mm)"),
                     breaks = seq(0, max(rain_annual_avg$avg_depth), by = 500), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  geom_hline(aes(yintercept = rain_annual_avg_lt), 
             color="black", 
             alpha=0.3, 
             size=1) + #avg line
  annotate("text", x = year_min, y = rain_annual_avg_lt, 
           label = paste("Average =", sprintf("%0.1f", rain_annual_avg_lt), "mm"), 
           parse = F, size = 4, color = "grey20",
           vjust = -0.3, hjust = -0.5) +
  theme(text=element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Average annual rainfall")

rain_annual_avg_plot

#print last plot to file
ggsave(paste0(filename2, "_annual_avg.jpg"), dpi = 300,
       width = 320, height = 210, units = "mm")


ggplotly(rain_annual_avg_plot, tooltip = "text",
         width = 900, height = 500)


###########################

# AVERAGE MONTHLY RAINFALL

#dataset: clean 2

#summarize
rain_mth_avg <- Rain_sum_mth_all2 %>% 
  group_by(Month) %>% 
  summarise(avg_depth = mean(sum_Depth))



#plot

rain_mth_avg_plot <- rain_mth_avg %>%
  ggplot(aes(x = Month, y = avg_depth)) +
  geom_bar(stat = "identity", fill = "skyblue2", alpha = 0.8,
           aes(text = paste0("Average monthly rainfall for <b>",month.name[rain_mth_avg$Month], "</b> is <b>", 
                             sprintf("%0.1f", rain_mth_avg$avg_depth), " mm</b>"))) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     labels=month.abb) + #x axis format
  scale_y_continuous(name= paste("Average monthly rainfall (mm)"),
                     breaks = seq(0, max(rain_mth_avg$avg_depth), by = 50), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  labs(title = "Average monthly rainfall")

rain_mth_avg_plot

#print last plot to file
ggsave(paste0(filename2, "_mth_avg.jpg"), dpi = 300,
       width = 300, height = 210, units = "mm")


ggplotly(rain_mth_avg_plot, tooltip = "text",
         width = 900, height = 500)


###########################

# MAPPING

#dataset: clean 2


#average
Rain_sum_yr_avg <- Rain_sum_yr3 %>% 
  group_by(Stn_no, Stn_name) %>% 
  summarise(Avg_annual = mean(sum_Depth))

#join with coordinates
Rain_sum_yr_avg_coord <- Rain_sum_yr_avg %>% 
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, Stn_name, Avg_annual, Long, Lat)


#####

#try map
rain_annual_map <- st_as_sf(Rain_sum_yr_avg_coord, 
                            coords = c("Long", "Lat"), crs = 4326)


#import shapefile
boundary <- st_read("C:/Users/User/Documents/GIS/Boundary/state_pmsia_short.shp")


#plot map

ggplot() + 
  geom_sf(data = boundary, size = 1, color = "grey", fill = "white") + 
  geom_sf(data = rain_annual_map) +
  geom_sf_label(data = rain_annual_map,
                aes(label = Stn_name)) +
  coord_sf(xlim=c(101.5, 102), ylim=c(3, 3.4), crs = 4326)




#####
#create empty grid raster
library(sp)
library(raster)

#transform to SpatialPointsDataFrame
sp_rain_annual <- SpatialPointsDataFrame(coords = Rain_sum_yr_avg_coord[, c("Long", "Lat")],
                                         data = Rain_sum_yr_avg_coord,
                                         proj4string = CRS("EPSG:4326"))



grd              <- as.data.frame(spsample(sp_rain_annual, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(sp_rain_annual) <- proj4string(sp_rain_annual) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(sp_rain_annual)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Avg_annual ~ 1, sp_rain_annual, newdata=grd, idp=2.0)

# Convert to raster object then clip to boundary
r.P.idw <- raster(P.idw)
#r.m     <- mask(r, W) #clip tp boundary w

#plot

plot(r.P.idw)


###########################
# MONTHLY RAINFALL
#boxplot

Rain_sum_mth_all2


###########################


###############################################################################
#OUTPUT
###############################################################################


#list all dataframe
list_worksheet <- list("Annual" = Rain_sum_yr3,
                       "LTAnnual" = Rain_sum_yr_avg_coord,
                       "Record" = raindata_cnt_clean2,
                       "Monthly" = Rain_sum_mth_all2,
                       "LTMthly" = Rain_sum_mth_lt)


# Create a blank workbook
wb <- createWorkbook()

# Loop through the list of split tables as well as their names
#   and add each one as a sheet to the workbook
Map(function(data, name){
  addWorksheet(wb, name)
  writeData(wb, name, data)
  setColWidths(wb, name, cols = 1:3, widths = "auto")
}, list_worksheet, names(list_worksheet))



###########################

# Save workbook to working directory
saveWorkbook(wb, file = paste0(filename2, "_output.xlsx"), 
             overwrite = TRUE)
