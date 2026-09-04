
#imporerer bibliotek for å lese exelfiler
library(readxl)
library(tidyverse)
#importerer datasett over strømprisene
Elprices <- read_excel("Elprices.xlsx")

#Lager et nytt datasett med bare de norske prisene

Elprices_norge <- Elprices |> select(c(date,no1:no4)) 


ggplot(Elprices_norge) + geom_line(mapping = aes(x = date, y = no1)) + labs(x = "Date", y = "Pris", title = "Strompris i prissone 1(2023)")

#gjør det om til et korrekt tidy format, slik at vi har  en observasjon per pris
long = pivot_longer(Elprices_norge, cols = c(names(Elprices_norge)[-1]),
                      names_prefix = "no",
                      names_to = "prissone",
                      values_to = "pris")

View(long)

ggplot(long) + geom_line(aes(x = date, y = pris, color = prissone)) + labs(x = "Dato", y= "Pris", title = "Pris i alle prissonene (2023)")