

library(readr)

data = read.csv("forbes_2022_billionaires.csv", sep = ",", header = TRUE)

#Finne yngste og eldste i datasettet

m = min(na.omit(data$age)) #må bruke na.omit for å fjerne der det ikke er registrert en alder enda
minst = data |> filter(age == m)
print(minst[2:5]) #skriver ut navnet på den minste billioneren

s = max(na.omit(data$age))
eldst = data |> filter(age == s)
print(eldst[2:5]) #skriver ut navnet på den minste billioneren


#Lager en egen tibble med de hundre rikeste
hundre_rikeste = data[1:100,]
#finner gjennomsnittet av disse
g_rikest = mean(hundre_rikeste$finalWorth )

#Skal sortere dem på land

land = data$country #lager en vektor med alle landene
land_unik = unique(land) #lager en vektor med de unike landen
frekvens = c()

for (l in land_unik){
  f = sum(ifelse(land == l,1,0)) #transformerer det til en dummy-vektor
  frekvens = c(frekvens,f) #legger det inn i en vektor
}

antall_rikinger_per_land = data.frame(land_unik,frekvens)
View(antall_rikinger_per_land)
