rm(list = ls())
library(ncdf4)
library(ggplot2)
library(dplyr)
library(raster)
library(sf)
setwd('D:/DataBase/Theme_n2/01-DataSet/')
load('D:/DataBase/Theme_n2/01-DataSet/Raster/04-ndvi/tserie.RData')

ndvi_velox <- brick('Raster/07-ndvi_SGolay/ndvi_SGolay.nc')
region   <- read_sf('02-shp/Region.shp')

data <- raster::extract(ndvi_velox, region, fun = mean, na.rm = TRUE) %>% as.vector()

ndvi <- data.frame(date = tserie, data) %>% mutate(mes = substr(date,6,7)) %>% group_by(mes) %>% summarise(ndvimean = mean(data, na.rm = T),
                                                                                                ndvimax  = max(data, na.rm = T),
                                                                                                ndvimin  = min(data, na.rm = T))

ndvi2000 <- data.frame(date = tserie, data) %>% mutate(mes = substr(date,6,7)) %>% dplyr::filter(substr(date,1,4) == 2000) %>%
                                            group_by(mes) %>% summarise(ndvi2000 = mean(data, na.rm = T))
ndvi2005 <- data.frame(date = tserie, data) %>% mutate(mes = substr(date,6,7)) %>% dplyr::filter(substr(date,1,4) == 2005) %>%
                                            group_by(mes) %>% summarise(ndvi2005 = mean(data, na.rm = T))
ndvi2010 <- data.frame(date = tserie, data) %>% mutate(mes = substr(date,6,7)) %>% dplyr::filter(substr(date,1,4) == 2010) %>%
                                            group_by(mes) %>% summarise(ndvi2010 = mean(data, na.rm = T))
ndvi2016 <- data.frame(date = tserie, data) %>% mutate(mes = substr(date,6,7)) %>% dplyr::filter(substr(date,1,4) == 2016) %>%
                                            group_by(mes) %>% summarise(ndvi2016 = mean(data, na.rm = T))

df <- data.frame(date = seq(as.Date('2018-01-01'), as.Date('2018-12-01'), by = 'month'), ndvi) %>% full_join(ndvi2000) %>% full_join(ndvi2005) %>% 
  full_join(ndvi2010) %>% full_join(ndvi2016)

Sys.setlocale(category = 'LC_ALL', locale = 'english')

X <-  ggplot(df, aes(date, ndvimean)) +
  geom_line(colour = 'black', size=.8) + 
  theme_bw() + ylab(label = '[NDVI]') +  xlab(label = '') +
  ggtitle('Seasonal NDVI', subtitle = 'from 2000 to 2018') + 
  theme(plot.title    = element_text(size=15),
        plot.subtitle = element_text(size=15),
        axis.text.x   = element_text(size=12),
        axis.text.y   = element_text(size=12),
        axis.title    = element_text(size=20)) +
  scale_x_date(date_labels = '%b', breaks = '1 month') +
  scale_y_continuous(breaks = seq(3700,6200,500), limits = c(3700, 6200)) +
  geom_line(aes(date, ndvimax),  colour = 'red', linetype = 'dashed', size=.8) +
  geom_line(aes(date, ndvimin),  colour = 'red', linetype = 'dashed', size=.8) +
  geom_line(aes(date, ndvi2000), colour = rgb(233, 228, 91, maxColorValue=255) , size=.8) +
  geom_line(aes(date, ndvi2005), colour = rgb(160, 114, 14, maxColorValue=255) , size=.8) +
  geom_line(aes(date, ndvi2010), colour = 'snow3'       , size=.8) +
  geom_line(aes(date, ndvi2016), colour = 'springgreen3', size=.8) +
  geom_point(aes(date, ndvi2000), colour = rgb(233, 228, 91, maxColorValue=255) , size=2) +
  geom_point(aes(date, ndvi2005), colour = rgb(160, 114, 14, maxColorValue=255) , size=2) +
  geom_point(aes(date, ndvi2010), colour = 'snow3'       , size=2) +
  geom_point(aes(date, ndvi2016), colour = 'springgreen3', size=2)

ggsave(plot = X, filename = 'D:/DataBase/Theme_n2/04-Graphics/Seasonal_Rainfall.png',
       width = 15, height = 15, units = "cm", dpi = 500)
