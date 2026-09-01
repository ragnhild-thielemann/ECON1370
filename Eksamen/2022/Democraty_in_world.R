
library(tidyverse)

vdem = read.csv("vdem.csv", head = TRUE, sep = ",")
head(vdem)

#Finner startår og sluttår for målingene
start_ar = min(vdem$year)
slutt_ar = max(vdem$year)

print(c(start_ar,slutt_ar))
#Finner lengden med vektorer med land som var registret i 2021
land_i_2021 = length(vdem$country_name[vdem$year == 2021])

print(land_i_2021)

norway_democary = vdem$v2x_libdem[vdem$country_text_id == "NOR"]
usa_democraty = vdem$v2x_libdem[vdem$country_text_id == "USA"]

ggplot() + geom_line(aes(x = vdem$year[vdem$country_text_id == "USA"] , y = usa_democraty , color = "USA")) + geom_line(aes(x = vdem$year[vdem$country_text_id== "NOR"], y = vdem$v2x_libdem[vdem$country_text_id == "NOR"] , color = "NORWAY")) + labs(x = "Year", y = "Democraty", title = "USA vs NORWAY")

#Finner median demokratiscore fra 1950 og frem til i dag
vek_med = c() ; vek_25 = c() ; vek_75 = c() 
for (y in 1950:2021){ #løper gjennom årene
  #finner 0.25,0.5 og 0.75-kvantilen i datasettet for de ulike årene"
  kvan = quantile(vdem$v2x_libdem[vdem$year == y], na.rm = TRUE, probs = c(0.25,0.5,0.75)) 
  
  vek_med = c(vek_med,as.double(kvan[2]))
  vek_25 = c(vek_25,as.double(kvan[1]))
  vek_75 = c(vek_75,as.double(kvan[3]))
}
print(vek_med)
#plotter median demokratiscore
ggplot() + geom_point(aes ( x = 1950:2021, y = c(vek_25),color = c("0.25"))) + geom_point(aes(x = 1950:2021, y = vek_med, color = "0.5")) + geom_point(aes(x = 1950:2021, y = vek_75, color = "0.75")) + labs(x = "Year", y = "Demokratiscore", color = "kvantil")


a = seq(1980,2020,10) #årene vi skal beregne for

land = unique(vdem$country_name) #bruker komandoen unique

