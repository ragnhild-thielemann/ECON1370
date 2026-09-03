
library(tidyverse)
country <- c("Norway","Sweeden")
gdp2000 <- c(38132,29625)
gdp2020 <- c(14921,1439)

df <- tibble(country, gdp2000,gdp2020)
print(df)
long <- pivot_longer(df,cols = starts_with("gdp"),
                     names_to = "year", #navnene etter prefisken blir i kolonnen år
                      names_prefix = "gdp",
                     values_to = "gdp") #verdiene blir i kolonnen gdp

print(long) #dette blir en tidy fremvisning

wide <- pivot_wider(long,
                    names_prefix = "gdp",
                    names_from = "year",
                    
                    values_from = "gdp")

print(wide)