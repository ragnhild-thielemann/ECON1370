
library(tidyverse)
library(readr)

vdem <- read.csv("vdem.csv", head = TRUE, sep = ",")

#Disse funksjonene leses utover - begynner med filtreingen, før vi teller opp radne. Dette retunerer så et heltall, som er antall tall i datasettet
antall <- vdem  |> filter(year==2021) |> nrow()

print(sprintf("Det er %0d land i verden", antall))
#tar en egen tibble med bare størt og minst
min_max <- vdem |> filter(year == min(year) | year == max(year))
min_max <- sort(unique(min_max$year)) #sørger for at det bare er to verdier, og at disse er sortert, så vi får riktige verdier
print(sprintf("Datasettet strekker seg fra %0d til %0d",min_max[1],min_max[2]))

#kan man plotte inne i data-framen?


summary(vdem)