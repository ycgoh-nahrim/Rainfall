# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(ggrepel)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2022/20220705_Banjir_Baling/Analysis/AM1')


#import data
RF_data = read.csv("AnnualMax_Muda.csv", header = T, sep = ",")


#location/basin
a_name = "Muda"


#set format
str(RF_data)
#RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")

#list stations
stn_list = as.data.frame(unique(RF_data$Stn_No))

# CHECK OUTLIERS
## AM CHART

gg_rainfall <- RF_data2 %>% 
  #filter(Complete == "Y") %>% 
  ggplot(aes(x = Year, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = factor(Complete)), na.rm = T) +
  geom_text_repel(aes(label = ifelse(Depth > 300 | Depth < 10, 
                                     paste0(Stn_No, " (", Year, ")"), "")), 
                  segment.size  = 0.2, max.overlaps = 20, size = 2) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name="Year", 
                     breaks = seq(1970, 2020, by = 5),
                     minor_breaks = NULL) +
  #scale_x_datetime(name= "Datetime", date_labels = "%b %d",
  #                 date_breaks = "1 day", #date_minor_breaks = "1 day",
  #                 minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual Max Rainfall (mm)"),
                     #breaks = seq(0, 500, by = 100), limits = c(0,500),
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 5)) +
  labs(title = paste0("Annual Maximum Rainfall by Duration"),
       color = "Complete") +
  guides(alpha = "none", shape = "none")

gg_rainfall

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()

### print last plot to file
ggsave(paste0("RF_AM_", a_name, "_scatter_notclean.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")


# clean data
RF_data2 <- RF_data %>% 
  filter(Year != 2021) %>%  #remove incomplete year considered as complete
  filter(Stn_No != 5503042 & Stn_No != 6007063) %>%   #remove questionable station 6007063
  filter(Complete == "Y") %>% 
  filter(Depth > 20)



# FACET CHART
## BY DURATION

gg_rainfall_dur <- RF_data2 %>% 
  ggplot(aes(x = Year, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = factor(State)), 
             na.rm = T) +
  #geom_text_repel(aes(label = ifelse(Depth > 500, # | Depth < 10, 
  #                                   paste0(Stn_No, " (", Year, ")"), "")), 
  #                segment.size  = 0.2, max.overlaps = 20, size = 2) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(name="Year", 
                     breaks = seq(1970, 2020, by = 5),
                     minor_breaks = NULL) +
  scale_y_continuous(name= paste("Annual Max Rainfall (mm)"),
                     #breaks = seq(0, 500, by = 100), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  facet_wrap(~Duration, ncol = 2, #nrow = 2, 
             labeller = labeller(paste0(RF_data$Duration, " h")),
             scales = "free_y") +
  theme(text = element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6)) +
  labs(title = paste0("Annual Maximum Rainfall by Duration"),
       color = "State") +
  guides(alpha = "none", shape = "none")

gg_rainfall_dur

ggplotly(gg_rainfall_dur, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()

### print last plot to file
ggsave(paste0("RF_AM_", a_name, "_dur_clean2.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")




## BY STATION

dur_list <- data.frame(unique(RF_data$Duration))
#i=1

for (i in 1:nrow(dur_list)) {
  
  dur_sel = dur_list[i, 1]
  
  gg_rainfall_stn <- RF_data2 %>% 
    filter(Duration == dur_sel) %>% 
    ggplot(aes(x = Year, y = Depth)) +
    geom_point(aes(shape = ".", alpha = 0.5), #color = factor(Complete)), 
               color = "steelblue3",
               na.rm = T) +
    #geom_text_repel(aes(label = ifelse(Depth > 300 | Depth < 10, 
    #                                   paste0(Stn_No, " (", Year, ")"), "")), 
    #                segment.size  = 0.2, max.overlaps = 20, size = 2) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(name="Year", 
                       breaks = seq(1970, 2020, by = 5),
                       minor_breaks = NULL) +
    scale_y_continuous(name= paste("Annual Max Rainfall (mm)"),
                       #breaks = seq(0, 500, by = 100), 
                       limits = c(0, NA),
                       minor_breaks = NULL) + #y axis format
    facet_wrap(~Stn_No, ncol = 8, #nrow = 2, 
               scales = "free_y") +
    theme(text = element_text(family = "Roboto", color = "grey20"),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 5, face = "bold"),
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(angle = 90, hjust = 0.5, size = 5)) +
    labs(title = paste0("Annual Maximum Rainfall by Station"),
         subtitle = paste0("Rainfall Duration: ", dur_sel, " hr" )) +
         #color = "Complete") +
    guides(alpha = "none", shape = "none")
  
  gg_rainfall_stn
  

  ### print last plot to file
  ggsave(paste0("RF_AM_", a_name, "_dur", dur_sel, "h_clean2.jpg"), dpi = 300,
         width = 10, height = 6, units = "in")
  
}


#########################
#########################
# MAPPING

library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
library(rgdal) # read shapefile
library(mapproj)
library(tmap) # animation

library(gridExtra)

library(viridis)
library(RColorBrewer)
library(scales)

# And a lot of different packages to test their interpolation functions
library(gstat)  # inverse distance weighted, Kriging
library(fields) # Thin Plate Spline
library(automap)# Automatic approach to Kriging


# import coordinate data

RF_stn = read.csv("JPS_RF_Muda.csv", header = T, sep = ",")

str(RF_stn)


# map country and coordinate data

## shapefile
basin_shp <- readOGR("J:/Backup_main/2022/20220705_Banjir_Baling/GIS/shp/jsb_sm5_muda_rso.shp",
                  stringsAsFactors = F) #EPSG3375 Kertau RSO m

#sel_shp <- subset(basin_shp, STATE %in% c("Selangor", "Kuala Lumpur"))

sel_shp <- readOGR("J:/Backup_main/2022/20220705_Banjir_Baling/GIS/shp_watershed/Kupang_subbasin_all2.shp",
                   stringsAsFactors = F) #EPSG3168 GDM2000


crs(sel_shp)

crs(basin_shp)

### change projection to WGS84 (original Kertau)
#basin_shp2 <-spTransform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

sel_shp2 <-spTransform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
basin_shp2 <-spTransform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

crs(sel_shp2)
crs(basin_shp2)


## layout map

map <- ggplot() + 
  geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "white") +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "white") +
  geom_point(data = RF_stn, aes(x = LONG_X1, y = LAT_Y1),
              color = 'red', size = 1, alpha = 0.5) +
  coord_map(xlim=c(100, 101.5), ylim=c(5.25, 6.5)) +
  theme_void() +
  theme(text = element_text(family = "Roboto", color = "grey20", size = 8)) +
  labs(title=paste0(a_name, " Basin Rainfall Stations Distribution"),
       subtitle = paste0("Total: ", nrow(RF_stn)))

map

###print last plot to file
ggsave(paste0(a_name, "_stn_map.jpg"), dpi = 300,
       width = 3, height = 3, units = "in")


# INTERPOLATE (one day first)

## subset 
RF_data_y2020 <- RF_data2 %>% 
  filter(Year == "2020", Duration == 24)

str(RF_stn)

RF_stn2 <- RF_stn[, c(2:4, 6, 8, 10, 12)]

## rename columns
str(RF_stn2)
names(RF_stn2)[1] <- "Stn_No"
names(RF_stn2)[4] <- "Lat"
names(RF_stn2)[5] <- "Long"
  


## join data
RF_y2020 <- RF_data_y2020 %>% 
  merge(RF_stn2, by = "Stn_No")

str(RF_y2020)

## convert df to spatial
sf_y2020 <- st_as_sf(RF_y2020, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_y2020)


## create raster template
ras_interp_template <- raster(basin_shp2, res = 0.001)

## make sure same projection
crs(ras_interp_template)
crs(sf_y2020)

#plot(ras_interp_template)


# Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = Depth ~ 1,    # The column  we are interested in
  data = as(sf_y2020, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
y2020_NN <- interpolate(ras_interp_template, fit_NN)
plot(y2020_NN)
y2020_NN_mask <- mask(y2020_NN, mask = basin_shp2)
plot(y2020_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = Depth ~ 1,
  locations = sf_y2020,
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)
y2020_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(y2020_IDW)
y2020_IDW_mask <- mask(y2020_IDW, mask = basin_shp2)
plot(y2020_IDW_mask)

class(y2020_IDW_mask)

## overlay map


map_rf_y2020 <- ggplot() +
  geom_raster(data = as.data.frame(y2020_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  #scale_fill_gradientn(name="Rainfall (mm)", 
  #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
  #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
  #                     limits = c(0, 4000)) +
  scale_fill_distiller(name="Max 1D Rainfall (mm)", 
                       palette = "Spectral", direction = 1,
                       #na.value = "purple",
                       oob = scales::squish, # squish out of bound values to nearest extreme
                       breaks = seq(0, 450, by = 100),
                       limits = c(0, 450)) + # set fixed legend) +
  geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
               colour = "white", fill = NA) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey60", size = 0.1, fill = NA) +
  theme_void() + 
  theme(legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  labs(title = paste0(a_name, " Basin Annual Maximum 1D Rainfall Distribution"),
       subtitle = "2020") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(10, "lines"), barheight = unit(0.5, "lines")))

map_rf_y2020

#print last plot to file
ggsave(paste0(a_name, "_RF_AM1D_2020_lg450.jpg"), dpi = 300,
       width = 5, height = 4, units = "in")



# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_y2019[, c('Long', 'Lat')]), # accepts points but expects them as matrix
  Y = RF_y2019$sum_Depth,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
y2019_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(y2019_TPS)
y2019_TPS_mask <- mask(y2019_TPS, mask = basin_shp)
plot(y2019_TPS_mask)



# Automatized Kriging  

## reproject to GDM 2000 PM
sf_y2019_gdm <- st_transform(sf_y2019, crs = 3375)
crs(sf_y2019_gdm)

fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = sum_Depth ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_y2019_gdm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 
y2019_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = 4326) #no changes to CRS??
plot(y2019_KRIG)


#############################
# INTERPOLATE FOR ALL YEARS

##  Use IDW

# daily rainfall data join stn data

RF_data_stn <- RF_data2 %>% 
  merge(RF_stn2, by = "Stn_No") %>% 
  filter(Duration == 24)

str(RF_data_stn)

max(RF_data_stn$Depth)

# convert df to spatial
sf_rf_all <- st_as_sf(RF_data_stn, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_rf_all)

# extract date for iteration
un_date <- sort(unique(RF_data_stn$Year)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Year"


# PRODUCE INTERPOLATION RASTER AND MAP



# SEPARATE INTERPOLATION AND MAPPING

# produce interpolation raster only (without map)

interp_list = list() #for combination

for (i in 1:(nrow(df_date))) {
  #i=1
  sel_yr <- df_date[i,]
  
  #filter according to date
  RF_data_annual <- RF_data_stn %>% 
    filter(Year == sel_yr)
  
  # convert to sf
  sf_rf_annual <- st_as_sf(RF_data_annual, coords = c('Long', 'Lat'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = Depth ~ 1,
    locations = sf_rf_annual,
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  annual_IDW <- interpolate(ras_interp_template, fit_IDW)
  annual_IDW_mask <- mask(annual_IDW, mask = basin_shp2)
  #plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- annual_IDW_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# produce maps from interpolation raster

maplist <- list()

j = 1

for (j in 1:length(interp_list)) {
  
  sel_yr <- df_date[j,]

  # plot map
  annual_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[j]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred),
                show.legend = FALSE) +
    #scale_fill_gradientn(name = "Rainfall (mm)", 
    #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
    #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
    #                     limits = c(0, 4000)) +
    scale_fill_distiller(name = "Max 1h Rainfall (mm)", 
                         palette = "Spectral", direction = 1,
                         #na.value = "purple",
                         oob = scales::squish, # squish out of bound values to nearest extreme
                         breaks = seq(0, 450, by = 50),
                         limits = c(0, 450)) + # set fixed legend) +
    geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
                 colour = "white", fill = NA) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "grey80", fill = NA) +
    theme_void() + 
    theme(plot.title=element_text(size = 11)) +
    labs(title = sel_yr) +
    coord_fixed() 
  
  
  #counter <- counter + 1
  maplist[[j]] <- annual_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# facet mapping

library(gridExtra)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_rf_y2020)


# arrange layout

facet_map <- grid.arrange(grobs = maplist[1:47], ncol = 10)

#### title font format
title = grid::textGrob(paste0(a_name, ' Basin (Sg Kupang subbasin) Annual Maximum 1D Rainfall\n'), 
                       gp = grid::gpar(fontsize = 14))

facet_legend_map <- grid.arrange(facet_map, mylegend, 
                                 top = title, 
                                 nrow = 2, heights = c(9, 1)
                                 #ncol = 2, widths = c(9, 1)
                                 )


# multiple maps with single particular map
#facet_all_map <- grid.arrange(facet_map, map_rf_y2019, ncol = 2)


#print last plot to file
ggsave(paste0(a_name, "_RF_AM1D_annual_lg450.jpg"), facet_legend_map, dpi = 400,
       width = 17, height = 10, units = "in")

#########################

# CLASSIFIED MAP


## set palette 
col_pal <- brewer.pal(n = 11, name = "Spectral")
#col_brk <- c(0, 25, 50, 75, 100, 125, 150, 200, 250, 300, 350, 460)
col_brk <- c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 460)


## map

### single map

map_cl_AM_2020 <- ggplot() +
  geom_raster(data = as.data.frame(y2020_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Max 1D Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(0, 460)) +
  #geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
  #             colour = "white", fill = NA) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey80", size = 0.1, fill = NA) +
  geom_point(data = RF_y2020, aes(x = Long, y = Lat),
             color = 'red', size = 0.5, alpha = 0.5) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  labs(title = paste0(a_name, " Basin Annual Maximum 1D Rainfall Distribution"),
       subtitle = "2020") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_AM_2020

#print last plot to file
ggsave(paste0(a_name, "_RF_AM1D_2020_lgc1.jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


### produce multiple maps from interpolation raster

maplist_cl <- list()

m = 1

for (m in 1:length(interp_list)) {
  
  sel_yr <- df_date[m,]
  sel_pt <- RF_data_stn %>% 
    filter(Year == sel_yr)
  
  # plot map
  annual_IDW_map_cl <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[m]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred)) +
    scale_fill_stepsn(name = "Max 1D Rainfall (mm)",
                      #n.breaks = 3, 
                      colours = col_pal,
                      breaks = col_brk,
                      values = rescale(col_brk),
                      limits = c(0, 460)) +
    #geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
    #             colour = "white", fill = NA) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "grey80", size = 0.1, fill = NA) +
    geom_point(data = sel_pt, aes(x = Long, y = Lat),
               color = 'red', size = 0.5, alpha = 0.5) +
    theme_void() + 
    theme(legend.position = "none",
          plot.title=element_text(size = 11)) +
    labs(title = sel_yr) +
    coord_fixed()
  
  
  #counter <- counter + 1
  maplist_cl[[m]] <- annual_IDW_map_cl
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}



### facet map

mylegend_cl <- g_legend(map_cl_AM_2020)


# arrange layout

facet_map_cl <- grid.arrange(grobs = maplist_cl[1:47], ncol = 10)

#### title font format
title = grid::textGrob(paste0(a_name, ' Basin (Sg Kupang subbasin) Annual Maximum 1D Rainfall\n'), 
                       gp = grid::gpar(fontsize = 14))

facet_legend_map_cl <- grid.arrange(facet_map_cl, mylegend_cl, 
                                 top = title, 
                                 nrow = 2, heights = c(9, 1)
                                 #ncol = 2, widths = c(9, 1)
)




#print last plot to file
ggsave(paste0(a_name, "_RF_AM1D_annual_lgc2_pt.jpg"), facet_legend_map_cl, dpi = 400,
       width = 17, height = 10, units = "in")


#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)




