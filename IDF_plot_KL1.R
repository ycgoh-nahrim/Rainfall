
# load packages
library(tidyverse)
library(lubridate)
library(plotly)
#library(ggrepel)
library(viridis)
library(RColorBrewer)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2023/20211221_Banjir_2021/Analysis/IDF')

#import IDF data
# HP1 (2021) - grid 403
IDF_data = read.csv("HP1_2021_grid403.csv", header = T, sep = ",")

#rename column
colnames(IDF_data)[2:11] <- c("2","5","10","20","25","50","100","200","500","1000")

# Reshape the data from wide to long format
IDF_data_long <- pivot_longer(IDF_data, c(2:11), names_to = "ARI", values_to = "Depth")

#format column from character to numeric
IDF_data_long$ARI <- as.numeric(as.character(IDF_data_long$ARI))
IDF_data_long$Duration <- as.numeric(as.character(IDF_data_long$Duration))


##########
# Create the IDF curve plot

#for log axis
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))


## color palette
###color for idf
col_idf_pal <- viridis_pal(option = "D")(10)


## legend
###legend text size
lg_title <- 9
lg_txt <- 8


gg_idf <- ggplot(IDF_data_long, aes(x = Duration, y = Depth, group = as.factor(ARI))) +
  geom_line(aes(color = as.factor(ARI)),
            size = 0.5) +
  scale_x_log10(name = "Duration (hr)", 
                breaks = breaks, minor_breaks = minor_breaks,
                limits = c(0.1, 100),
                labels = scales::number_format(accuracy = 0.01)
  ) + 
  scale_y_log10(name = "Rainfall Depth (mm)", 
                breaks = breaks, minor_breaks = minor_breaks,
                limits = c(10, 1000), #adjust limits accordingly
                labels = scales::number_format(accuracy = 1)
  ) +
  scale_color_manual("Return Period", values = col_idf_pal) +
  #scale_color_viridis_d("Return Period", direction = -1) +
  theme_bw(base_size = 10) +
  theme(text=element_text(family = "Roboto", color = "grey20"),
        #panel.grid.major.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = lg_title), 
        legend.text = element_text(size = lg_txt),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Intensity-Duration-Frequency (IDF) Curves",
       subtitle = "Grid 403 (HP1, 2021)")

gg_idf

#print last plot to file
ggsave(paste0("IDF_g403.jpg"), dpi = 300,
       width = 8, height = 5, units = "in")

ggplotly(gg_idf, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

lg_idf <- g_legend(gg_idf)


##########
# COMPARE IDF WITH ACTUAL DATA

#PREPARE RAINFALL DATA FIRST


#import rainfall data
RF_data = read.csv("J:/Backup_main/2023/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR/IB_RF_202112_hr_clean.csv", header = T, sep = ",")

str(RF_data)

#format date
RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")


gg_RF <- RF_data %>% 
  filter(Datetime >= as.POSIXct("2021-12-17 00:00:00") & 
           Datetime <= as.POSIXct("2021-12-19 23:55:00")) %>% 
  ggplot(aes(x = Datetime, y = Depth_hr, group = Station.Name)) +
  geom_line(aes(color = Station.Name), alpha = 0.5, size = 0.5, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Day", date_labels = "%d %H:00",
                   date_breaks = "12 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 200, by = 10), 
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
  labs(title = paste0("Rainfall on 18 December 2021"),
       subtitle = paste0("KL Selangor")) +
  guides(alpha = "none", shape = "none")

gg_RF

#print last plot to file
ggsave(paste0("RF_KLSel_20211218.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")

ggplotly(gg_RF, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 



# FIND CUMULATIVE VALUES

RF_data_sel <- RF_data %>% 
  filter(Datetime >= as.POSIXct("2021-12-17 00:00:00") & 
           Datetime <= as.POSIXct("2021-12-19 23:55:00"))

RF_data_cum <- RF_data_sel %>% 
  group_by(RF_stn) %>% 
  mutate(Depth_cum = cumsum(Depth_hr)) 



gg_RF_cum <- RF_data_cum %>% 
  ggplot(aes(x = Datetime, y = Depth_cum, group = Station.Name)) +
  geom_line(aes(color = Station.Name), alpha = 0.5, size = 0.5, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Day", date_labels = "%d %H:00",
                   date_breaks = "12 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 600, by = 100), 
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
  labs(title = paste0("Rainfall on 18 December 2021"),
       subtitle = paste0("KL Selangor")) +
  guides(alpha = "none", shape = "none")

gg_RF_cum

#print last plot to file
ggsave(paste0("RF_KLSel_cum_20211218.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")

ggplotly(gg_RF_cum, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 


## Select stations with highest cumulative sum

RF_data_cum_stn <- RF_data_cum %>% 
  filter(Depth_cum > 200)

RF_data_cum_stn_sel <- data.frame(unique(RF_data_cum_stn$RF_stn))

colnames(RF_data_cum_stn_sel)[1] <- "RF_stn"

RF_data_cum_sel <- RF_data_cum %>% 
  right_join(RF_data_cum_stn_sel, by = "RF_stn") 


## color palette
###color for cum rainfall
col_rf_pal <- brewer.pal(12, "Paired")



### plot
gg_RF_cum_sel <- RF_data_cum_sel %>% 
  ggplot(aes(x = Datetime, y = Depth_cum, group = Station.Name)) +
  geom_line(aes(color = Station.Name), alpha = 0.8, size = 0.5, na.rm = T) +
  #geom_point(aes(color = Station.Name), alpha = 0.8, size = 0.5, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Datetime", date_labels = "%d/%m %H:00",
                   date_breaks = "12 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cumulative Rainfall (mm)"),
                     breaks = seq(0, 600, by = 100), 
                     limits = c(0, 600),
                     minor_breaks = NULL) + #y axis format
  scale_color_manual("Stn name", values = col_rf_pal,
                     labels = function(x) str_wrap(x, width = 15)) +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        #panel.grid.major.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = lg_title), 
        legend.text = element_text(size = lg_txt),
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
  labs(title = paste0("Cumulative Rainfall from 17-19 December 2021"),
       subtitle = paste0("KL Selangor"), color = "Stn Name") +
  guides(alpha = "none", shape = "none")

gg_RF_cum_sel

#print last plot to file
ggsave(paste0("RF_KLSel_cum_20211218_sel.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")

ggplotly(gg_RF_cum_sel, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 


###extract legend
lg_rfcum <- g_legend(gg_RF_cum_sel)


### write to csv
write.table(RF_data_cum_sel, "RFcum_KLSel_20211218.csv", sep=",", row.names = FALSE)



# PLOT IDF WITH CUMULATIVE RAINFALL DATA

## convert datetime to duration
###calculate difference
RF_data_cum_sel2 <- RF_data_cum_sel %>% 
  group_by(Station.Name) %>% 
  mutate(Dur1 = Datetime - lag(Datetime, default = first(Datetime)))

###convert time duration (sec) to day
RF_data_cum_sel2$Dur1 <- as.numeric(RF_data_cum_sel2$Dur1, units="hours")

###calculate cum duration in hr
RF_data_cum_sel2 <- RF_data_cum_sel2 %>% 
  group_by(Station.Name) %>% 
  mutate(Cum_dur = cumsum(Dur1)) 

###replace 0 in Cum_dur to 0.1
RF_data_cum_sel2$Cum_dur[RF_data_cum_sel2$Cum_dur == 0] <- 0.1


## combine 2 dataframes
###rainfall data
df_rfcum <- RF_data_cum_sel2 %>% 
  select(c("Cum_dur", "Station.Name", "Depth_cum"))
colnames(df_rfcum) <- c("Duration", "ARI", "Depth")
df_rfcum$Type <- "RFcum"
###idf data
df_idf <- IDF_data_long
df_idf$ARI <- as.character(as.numeric(df_idf$ARI))
df_idf$Type <- "IDF"
df_combine <- bind_rows(df_idf, df_rfcum)
  

## color palette
##combine
col_all_pal <- c(col_idf_pal, col_rf_pal)


## sort legend
lgs_rfcum <- c("Air Panas", "Jinjang", "JPS Ampang", "JPS Wilayah", "Kolam Takungan Batu",
               "Ladang Edinburgh", "Lembah Keramat", "Sg. Batu di Sentul",
              "Sg. Bunus di Jln. Tun Razak", "Sg. Kerayong di Kg. Cheras Baru",
              "Sg. Klang di Kg. Berembang", "Sg. Klang di Leboh Pasar")
lgs_idf <- c("2","5","10","20","25","50","100","200","500","1000")
lgs_all <- c(lgs_idf, lgs_rfcum)


gg_idf_rf <- df_combine %>% 
  ggplot(aes(x = Duration, y = Depth, group = as.factor(ARI), color = as.factor(ARI))) +
  geom_line(size = 0.5, alpha = 0.7) +
  #geom_line(data = IDF_data_long,
  #          aes(x = Duration, y = Depth, group = as.factor(ARI), 
  #              color = as.factor(ARI)),
  #          size = 0.5) +
  #geom_line(data = RF_data_cum_sel2,
  #          aes(x = Cum_dur, y = Depth_cum, group = Station.Name, 
  #              color = Station.Name), 
  #          alpha = 0.8, size = 0.5, na.rm = T) +
  scale_x_log10(name = "Duration (hr)", 
                breaks = breaks, minor_breaks = minor_breaks,
                limits = c(0.1, 100),
                labels = scales::number_format(accuracy = 0.01)
  ) + 
  scale_y_log10(name = "Rainfall Depth (mm)", 
                breaks = breaks, minor_breaks = minor_breaks,
                limits = c(1, 1000), #adjust limits accordingly
                labels = scales::number_format(accuracy = 1)
  ) +
  #scale_color_viridis_d("Return Period", direction = -1) +
  scale_color_manual("ARI", values = col_all_pal, breaks = lgs_all,
                     labels = function(x) str_wrap(x, width = 15)) +
  theme_bw(base_size = 10) +
  theme(text=element_text(family = "Roboto", color = "grey20"),
        #panel.grid.major.x = element_blank(),
        legend.position = "none",
        #legend.title = element_text(size = lg_title), 
        #legend.text = element_text(size = lg_txt),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  #labs(title = "Intensity-Duration-Frequency (IDF) Curves") +
  guides(colour = guide_legend(ncol = 2))

gg_idf_rf


#print last plot to file
ggsave(paste0("IDF_g403_RFcum_KLSel_20211218.jpg"), dpi = 300,
       width = 8, height = 5, units = "in")

ggplotly(gg_idf_rf, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 


# ARRANGE LAYOUT

library(gridExtra)

# title font format
title = grid::textGrob('Intensity-Duration-Frequency (IDF) Curves', 
                       gp = grid::gpar(fontsize = 12))


gg_IDF_RFcum <- grid.arrange(gg_idf_rf, lg_idf, lg_rfcum, 
                                 top = title, 
                                 #nrow = 2, heights = c(9, 1)
                                 ncol = 3, widths = c(7, 1.5,1.5))


#print last plot to file
ggsave(paste0("IDF_g403_RFcum_KLSel_20211218_layout.jpg"), gg_IDF_RFcum, dpi = 300,
       width = 8, height = 5, units = "in")
