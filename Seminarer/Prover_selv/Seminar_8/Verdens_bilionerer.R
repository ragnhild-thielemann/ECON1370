

library(readr)

data = read.csv("forbes_2022_billionaires.csv", sep = ",", header = TRUE)

#Finne yngste og eldste i datasettet

m = min(na.omit(data$age)) #må bruke na.omit for å fjerne der det ikke er registrert en alder enda
minst = data |> filter(age == m)
print(minst[2:5]) #skriver ut navnet på den minste billioneren

s = max(na.omit(data$age))
eldst = data |> filter(age == s)
print(eldst[2:5]) #skriver ut navnet på den minste billioneren
