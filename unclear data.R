ethinicity_area_plot <- function(){
  ethinicity_area <- read_csv("/Users/andychang/onedrive/r project/hate crimes in uk and usa/areas-of-england-and-wales-by-ethnicity.csv")
  ethinicity_area$`Standard Ethnicity` <- as.factor(ethinicity_area$`Standard Ethnicity`)
  ethinicity_area$Region <- as.character(ethinicity_area$Region)
  ####除掉wales 因為經緯度資料上沒有這個區域 記得稿子上要提到這個比例
  ethinicity_area[ethinicity_area$Region == "East",]$Region <- "East of England"
  return(ethinicity_area)
}
head(ethinicity_area_plot())
###  條字體大小 另外圖形的編排方式依照區域來分
library(treemap)
colnames(ethinicity_area_plot())
freq(ethinicity_area_plot()$'Standard Ethnicity')
#### treemap done
treemap(ethinicity_area_plot(), index = c("Region","Standard Ethnicity"), vSize = "%",
        type ="index", palette = "Set2", bg.labels =c("transparent"), 
        fontsize.labels = c(13, 7),
        fontface.labels = c(3,3),
        fontfamily.title = "黑體-繁 中黑",
        title = "英格蘭和威爾斯地區種族分佈",
        align.labels = list(c("left","top"),c("center","center")))
 future_population_all <- function(k){
  k1 <- paste('',k,sep = "")
  future_population <- readxl::read_excel("/users/andychang/onedrive/r project/hate crimes in uk and usa/看變化而已 作為假設_ethnic-groups-by-borough.xls", sheet = k1)
  future_population <- future_population[,1:7]
  for (i in 1:nrow(future_population)) {
    if(is.na(future_population$Number[i]) == TRUE){
      future_population <- future_population[-c(i),]
    }
  }  
  future_population <- t(future_population)
  rownames(future_population)[3:7] <- future_population[3:7,1]
  rownames(future_population)[1] <- "year"
  future_population <- future_population[,-c(1,2)] %>% t() %>% as.data.frame()
  future_population$year <- k1
  future_population_new <- future_population %>% pivot_longer(
    cols = -c(year,Area),
    names_to = "ethinicity",
    values_to = "guess_statistics"
  )
  ### 改日期
  future_population_new$year <- as.Date(future_population_new$year, format =  "%Y")
  future_population_new$ethinicity <- as.factor(future_population_new$ethinicity)
  future_population_new$guess_statistics <- as.numeric(as.character(future_population_new$guess_statistics))
  return(future_population_new)
}
future_population_all_12to18 <- rbind(future_population_all(2012), future_population_all(2013),future_population_all(2014),future_population_all(2015),future_population_all(2016),future_population_all(2017),future_population_all(2018))
head(future_population_all_12to18,5)
future_population_all_12to18 <- future_population_all_12to18[future_population_all_12to18$Area != "United Kingdom",]
### plot done
future_population_all_12to18 <- future_population_all_12to18[future_population_all_12to18$ethinicity != "Total",]
future_population_all_12to18$year <- str_remove(future_population_all_12to18$year, pattern = "-07-11")
ggplot(data = future_population_all_12to18, aes(x = year,guess_statistics, y = guess_statistics, fill = ethinicity, group = Area))+
  geom_col() +  
  #scale_x_continuous(limits = c(2012,2018), breaks = c(2012:2018)) +
  #scale_y_discrete(limits = c(0,8000000), breaks = seq(2000000,4000000,6000000)) +
  scale_fill_manual("種族",values = wes_palette(13, name = "BottleRocket2", type = "continuous")) +
  xlab(NULL) + ylab(NULL) +
  #coord_flip() +
  facet_wrap(~Area, nrow = 7) +
  theme_bw() +
  ggtitle("人口預測")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),text = element_text(family = "黑體-繁 中黑"),
        axis.title.y = element_text(angle = 45, vjust = 0.5), strip.text = element_text(size = 7)) 



