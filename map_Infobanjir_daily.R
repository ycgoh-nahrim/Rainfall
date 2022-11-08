# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR')
setwd('E:/Backup_main/2022/20211221_Banjir_2021/Data/PUBLIC INFO BANJIR')


#import data
RF_data = read.csv("IB_RF_202112_clean.csv", header = T, sep = ",")

#set format
str(RF_data)
RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")


# CHECK OUTLIERS
## 15 min CHART

gg_rainfall <- RF_data %>% 
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

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


# clean data based on chart above

RF_data2 <- RF_data %>% 
  filter(Depth < 100)


# AGGREGATE DATA TO DAILY
# ignore incomplete data

RF_data_day <- RF_data2 %>% 
  mutate(Date = date(Datetime)) %>% 
  group_by(RF_stn, Station.Name, Date) %>% 
  summarise(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth)))


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

IB_stn = read.csv("E:/Backup_main/2022/20211221_Banjir_2021/Data/Rainfall/PRABN_stn_KLSel.csv", 
                  header = T, sep = ",")

str(IB_stn)


# map country and coordinate data

## shapefile
pm_shp <- readOGR("E:/Backup_main/GIS_data/Boundary/state/state_pmsia_short.shp",
                  stringsAsFactors = F)

sel_shp <- subset(pm_shp, STATE %in% c("Selangor", "Kuala Lumpur"))

crs(sel_shp)

### change projection to WGS84 (original Kertau)
pm_shp2 <-spTransform(pm_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
sel_shp2 <-spTransform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


# layout map

map <- ggplot() + 
  geom_polygon(data = pm_shp2, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "white") +
  geom_point(data = IB_stn, aes(x = Longitude, y = Latitude),
              color = 'red', size = 1, alpha = 0.5) +
  coord_map(xlim=c(100.5, 102), ylim=c(2.5, 3.9)) +
  theme_void()

map


# INTERPOLATE (one day first)

# subset 
RF_data_day_18 <- RF_data_day %>% 
  filter(Date == "2021-12-18")

# join data
RF_day18 <- RF_data_day_18 %>% 
  merge(IB_stn, by = "Station.Name")

# convert df to spatial
sf_day18 <- st_as_sf(RF_day18, coords = c('Longitude', 'Latitude'), crs = 4326)
plot(sf_day18)


# create raster template
ras_interp_template <- raster(sel_shp2, res = 0.01)

# make sure same projection
crs(ras_interp_template)
crs(sf_day18)

#plot(ras_interp_template)


## Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = Depth_day ~ 1,    # The column  we are interested in
  data = as(sf_day18, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
d18_NN <- interpolate(ras_interp_template, fit_NN)
plot(d18_NN)
d18_NN_mask <- mask(d18_NN, mask = sel_shp2)
plot(d18_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = Depth_day ~ 1,
  locations = sf_day18,
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)
d18_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(d18_IDW)
d18_IDW_mask <- mask(d18_IDW, mask = sel_shp2)
plot(d18_IDW_mask)

class(d18_IDW_mask)

## overlay map

maprf1218 <- ggplot() +
  geom_raster(data = as.data.frame(d18_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_gradientn(name="Rainfall (mm)", 
                       #colors = c("beige", "deepskyblue3", "royalblue3", "purple4"),
                       colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998"),
                       values = rescale(c(0, 30, 60, 100)),
                       na.value = "purple",
                       limits = c(0, 100)) +
  #scale_fill_distiller(name="Rainfall (mm)", 
  #                     palette = "YlGnBu", direction = 1,
                       #na.value = "purple",
  #                     oob = scales::squish, # squish out of bound values to nearest extreme
  #                     breaks = seq(0, 60, by = 10),
  #                     limits = c(0, 60)) + # set fixed legend) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "white", fill = NA) +
  theme_void() + 
  labs(title="18 December 2021 Rainfall Distribution") +
  coord_fixed()

maprf1218

#print last plot to file
ggsave(paste0("KLSel_rainfall_20211218_60mm2.jpg"), dpi = 300,
       width = 6, height = 5, units = "in")



# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_day18[, c('Longitude', 'Latitude')]), # accepts points but expects them as matrix
  Y = RF_day18$Depth_day,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
d18_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(d18_TPS)
d18_TPS_mask <- mask(d18_TPS, mask = sel_shp2)
plot(d18_TPS_mask)



# Automatized Kriging  

## reproject to GDM 2000 PM
sf_day18_gdm <- st_transform(sf_day18, crs = 3375)
crs(sf_day18_gdm)

fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = Depth_day ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_day18_gdm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 
d18_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = 4326) #no changes to CRS??
plot(d18_KRIG)


#############################
# INTERPOLATE FOR ALL DAYS

##  Use IDW

# daily rainfall data join stn data

RF_data_day_stn <- RF_data_day %>% 
  merge(IB_stn, by = "Station.Name")

str(RF_data_day_stn)

# convert df to spatial
sf_rf_dec <- st_as_sf(RF_data_day_stn, coords = c('Longitude', 'Latitude'), crs = 4326)
plot(sf_rf_dec)

# extract date for iteration
un_date <- sort(unique(RF_data_day_stn$Date)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Date"


# PRODUCE INTERPOLATION RASTER AND MAP

# iterate by date, produce interpolation raster and plot map at once

rasterlist = list() #for combination
#rainfall_stack <- stack(d18_IDW_mask) # sth that matches the extent and res of output
#counter <- 0

for (i in 1:(nrow(df_date))) {
  #i=1
  date <- df_date[i,]
  
  #filter according to date
  RF_data_daily <- RF_data_day_stn %>% 
    filter(Date == date)
  
  # convert to sf
  sf_rf_daily <- st_as_sf(RF_data_daily, coords = c('Longitude', 'Latitude'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = Depth_day ~ 1,
    locations = sf_rf_daily,
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  daily_IDW <- interpolate(ras_interp_template, fit_IDW)
  daily_IDW_mask <- mask(daily_IDW, mask = sel_shp2)
  #plot(daily_IDW_mask)
  
  # plot map
  daily_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(daily_IDW_mask, xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred),
                show.legend = FALSE) +
    scale_fill_gradientn(name="Rainfall (mm)", 
                         #colors = c("beige", "deepskyblue3", "royalblue3", "purple4"),
                         colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
                         values = rescale(c(0, 30, 60, 100, 400)),
                         limits = c(0, 400)) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "white", fill = NA) +
    theme_void() + 
    theme(plot.title=element_text(size = 10)) +
    labs(title = date) +
    coord_fixed()
  
  
  #counter <- counter + 1
  rasterlist[[i]] <- daily_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# FACET MAP

library(gridExtra)

facet_map <- grid.arrange(grobs = rasterlist, ncol = 5)


#print last plot to file
ggsave(paste0("KLSel_rainfall_202112_facet_60mm2.jpg"), facet_map, dpi = 400,
       width = 12, height = 8, units = "in")






# SEPARATE INTERPOLATION AND MAPPING

# produce interpolation raster only (without map)

interp_list = list() #for combination

for (i in 1:(nrow(df_date))) {
  #i=1
  date <- df_date[i,]
  
  #filter according to date
  RF_data_daily <- RF_data_day_stn %>% 
    filter(Date == date)
  
  # convert to sf
  sf_rf_daily <- st_as_sf(RF_data_daily, coords = c('Longitude', 'Latitude'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = Depth_day ~ 1,
    locations = sf_rf_daily,
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  daily_IDW <- interpolate(ras_interp_template, fit_IDW)
  daily_IDW_mask <- mask(daily_IDW, mask = sel_shp2)
  #plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- daily_IDW_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# produce maps from interpolation raster

maplist <- list()

for (j in 1:length(interp_list)) {
  
  date <- df_date[j,]

  # plot map
  daily_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[j]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred),
                show.legend = FALSE) +
    scale_fill_gradientn(name="Rainfall (mm)", 
                         #colors = c("beige", "deepskyblue3", "royalblue3", "purple4"),
                         colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998"),
                         values = rescale(c(0, 30, 60, 100)),
                         na.value = "purple",
                         limits = c(0, 100)) +
    #scale_fill_distiller(name="Rainfall (mm)", 
    #                     palette = "YlGnBu", direction = 1,
    #na.value = "purple",
    #                     oob = scales::squish, # squish out of bound values to nearest extreme
    #                     breaks = seq(0, 60, by = 10),
    #                     limits = c(0, 60)) + # set fixed legend) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "white", fill = NA) +
    theme_void() + 
    theme(plot.title=element_text(size = 10)) +
    labs(title = date) +
    coord_fixed()
  
  
  #counter <- counter + 1
  maplist[[j]] <- daily_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# facet mapping

library(gridExtra)

facet_map <- grid.arrange(grobs = maplist, ncol = 5)

facet_all_map <- grid.arrange(facet_map, maprf1218, ncol = 2)

#print last plot to file
ggsave(paste0("KLSel_rainfall_202112_facetall_100mm1.jpg"), facet_all_map, dpi = 400,
       width = 14, height = 8, units = "in")

#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)




