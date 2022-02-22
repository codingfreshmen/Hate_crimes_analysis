library(tmap)
library(rgdal)
library(maps)
library(broom)
###下面是正確的區域 wales 用補的 ddd全部正確 但是uk_merge_data多一個英國警局 剃掉那個
eng_area_nowales <- readOGR(dsn = "/Users/andychang/onedrive/r project/hate crimes in uk and usa/Regions__December_2017__Boundaries-shp",
                            layer = "Regions__December_2017__Boundaries")
county_area <- readOGR(dsn = "/Users/andychang/onedrive/r project/hate crimes in uk and usa//Police_Force_Areas__December_2018__EW_BGC-shp",
              layer = "Police_Force_Areas__December_2018__EW_BGC")
eng_area_nowales_1 <- fortify(eng_area_nowales, region = "rgn17nm")
county_area_1 <- fortify(county_area, region = "pfa18nm")
eng_area_nowales_1 <- eng_area_nowales_1[,c(1,2,6,7)]  
county_area_1 <- county_area_1[,c(1,2,6,7)]
### county area合併 region area 合併
gg <- ggplot() + geom_polygon(data = eng_area_nowales_1, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25) +
  geom_text(data = eng_area_nowales_1, aes(x = long, y = lat, group= group, label = id))
gg
gg1 <- ggplot() + geom_polygon(data = county_area_1, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25)
cty_crime_merge <- merge(uk_merge_data(), county_area_1, by.x = "Force Name", by.y = "id")
area_crime_merge <- merge(ethinicity_area_plot(),eng_area_nowales_1, by.x = "Region", by.y = "id")
colnames(area_crime_merge)[3] <- "number"
aaa <- data.frame(cty_crime_merge$long, cty_crime_merge$lat, cty_crime_merge$group,
                  cty_crime_merge$`Total offences`, cty_crime_merge$`Number of offences.x`)
colnames(aaa )
gg_1 <- ggplot()+geom_polygon(data = aaa, aes(x = cty_crime_merge.long,y = cty_crime_merge.lat, group = cty_crime_merge.group, fill = cty_crime_merge..Number.of.offences.x.), colour = "black",size = 0.25)+
  coord_map() + scale_fill_gradient(low = "white",high = "blue")
gg_1
###
library(statmod)
library(hrbrthemes)
update.packages("hrmrthemes")
gg_1 <- ggplot()+geom_polygon(data = area_crime_merge, aes(x = long,y = lat, group = group, fill = number), colour = "black",size = 0.25)+
  coord_map() + scale_fill_gradient(low = "white",high = "blue")



