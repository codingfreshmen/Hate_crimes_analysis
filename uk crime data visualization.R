library(tidyverse)
library(dplyr)
library(ggmap)
library(stringr)
library(readr)
library(xlsx)
library(jiebaR)
library(reshape2)
#### merge data
uk_merge_data <- function(){
  total_offences <- read_csv("/Users/andychang/onedrive/r project/hate crimes in uk and usa/uk total offences.csv")
  view(total_offences)
  differ_factor <- read_csv("/Users/andychang/onedrive/r project/hate crimes in uk and usa/mocivated factor.csv")
  differ_factor <- differ_factor[differ_factor$`Financial Year` != "2011/12",]
  differ_plus_total <- merge(total_offences,differ_factor, by.x = c("Financial Year","Force Name"),by.y = c("Financial Year","Force Name"), all = TRUE)
  colnames(differ_plus_total)[1] <- "year"
  for (i in 1:6) {
    k = 12
    differ_plus_total$year <- str_remove(differ_plus_total$year, pattern = paste("/", k+i, sep = ""))
  }
  differ_plus_total$`Force Name` <- as.character(differ_plus_total$`Force Name`)
  differ_plus_total$`Total offences` <- as.factor(differ_plus_total$`Total offences`)
  differ_plus_total$`Motivating factor` <- as.factor(differ_plus_total$`Motivating factor`)
  differ_plus_total$year <- as.Date(differ_plus_total$year, format = "%Y")
  ####去掉british transport police 因為這並不是一個區域
  differ_plus_total <- differ_plus_total[differ_plus_total$`Force Name` != "British Transport Police",]
  differ_plus_total[differ_plus_total$`Force Name` == "Devon and Cornwall",]$`Force Name` <- "Devon & Cornwall"
  differ_plus_total$year <- str_remove(differ_plus_total$year, pattern = "-07-12")
  return(differ_plus_total)
}
uk_merge_data_1 <- uk_merge_data()
head(uk_merge_data_1)
colnames(uk_merge_data_1)[4] <- "total_numbers"
colnames(uk_merge_data_1)[6] <- "factor_numbers"
colnames(uk_merge_data_1)[2] <- "force_name"
colnames(uk_merge_data_1)[5] <- "motivating_factor"
a <- uk_merge_data_1[uk_merge_data_1$force_name != "Metropolitan Police",]
a[a$year == "2018",]
max(a[a$year == "2018",]$total_numbers)
#### visualization
library(plotly)
library(wesanderson)
### 需要修圖
pig <- ggplot(data = uk_merge_data_1[uk_merge_data_1$force_name != "Metropolitan Police",], aes(x = total_numbers, y = factor_numbers, 
                                          size = factor_numbers, frame = year,colour = total_numbers, text = paste("年份:",year,"<br>地點：",force_name, "<br>犯罪原因：",motivating_factor))) +
  geom_point() + scale_x_continuous(limits = c(0,9000), breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000)) +
  scale_y_continuous(limits = c(0,7000), breaks = c(0,1000,2000,3000,4000,5000,6000,7000)) +
  xlab("總犯罪次數統計") + ylab("不同因犯罪次數統計") + 
  scale_colour_gradient(low = "#FAD510", high = "#1E1E1E") + ggtitle("英國犯罪") +
  guides(fill = FALSE) + theme_bw() +
  theme(axis.title.y = element_text(angle = 30) ,legend.position = "none" ,text = element_text(family = "黑體-繁 中黑"))
htmlpig <- ggplotly(pig,tooltip = "text")
htmlpig
Sys.setenv("plotly_username"="gan.ganman1117")
Sys.setenv("plotly_api_key"="V2cdYE3VwLVvN1E6A4Sb")
file.create("glen_project.Rprofile")
file.edit("glen_project.Rprofile") 
api_create(htmlpig,filename = "英國仇恨犯罪",fileopt = "overwrite") 

### add ration is a plus
ethinity_1819 <- function(){
  test_4 <- readxl::read_excel("/users/andychang/onedrive/r project/hate crimes in uk and usa/hate-crime-1819-hosb2419-tables.xls", sheet = "Table_3")
  test_4
  for (i in 1:nrow(test_4)) {
    if(is.na(test_4$...2[i]) == TRUE){
      test_4 <- test_4[-c(i),]
    }
  }
  test_4 <- test_4[-c(1,12:14),-c(3)]
  test_4 <- t(test_4)
  rownames(test_4) <- test_4[,1]
  test_4 <- test_4[,-1]
  test_4 <- t(test_4) %>% as.data.frame()
  colnames(test_4)[2] <- "victim ratio"
  test_4$`victim ratio` <- as.numeric(as.character(test_4$`victim ratio`))
  for (i in 1:nrow(test_4)) {
    test_4[i,2] <- round(test_4[i,2]*100/sum(test_4$`victim ratio`), digits = 2)
  }
  #test_4$`Number of offences` <- as.numeric(test_4$`Number of offences`)
  return(test_4)
}
ethinity_1819()
####

