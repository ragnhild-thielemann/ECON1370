
library(tidyverse)

#importerer codebooken
codebook = read_csv("ESS9_codebook.csv")

# Importerer selve datasette
ESS9 <- read_csv("ESS9.csv")


#tidying data - en observasjon per rad
wide <- tibble ("land" = c("Norge","Sverige"),
                "gdp2000" = c(38121,14922),
                "gdp2020" = c(1493,19294))

long <- pivot_longer(wide, cols = starts_with("gdp"), names_prefix = "gdp", names_to = "year", values_to = "gdp")
print(long)

wide <- pivot_wider(long, names_from = "year", names_prefix = "gdp", values_from = "gdp")

print(wide)

skimr::skim() #ser hvilke data som mangler

drop_na (data, variabel)
#fjerner der det mangler variabeer
