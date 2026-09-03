
library(dplyr) #sorterer ut dataen
library(readr) #importerer readr-pakken fra tidyverse
library(ggplot2)
data <- read_csv("ESS9.csv")
codebook <- read_csv("ESS9_codebook.csv")

#Vil lage et datasett med gjennomsnittlig lykke i Norge, sverige og danmark

data_smaller <- data |> select(c(cntry,netustm,netusoft,agea, gndr))

time <- data_smaller |> filter(agea>18 & gndr == 2) |> mutate(internett = mean(netusoft, na.rm = TRUE), .by = cntry)

View(time)