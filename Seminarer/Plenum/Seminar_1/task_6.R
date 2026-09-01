

library(readr)
library(tidyverse)
library(ineq)

data = read_csv("inc_wealth.csv")
kronekurs = 11.57
#legger ved en ny kolonne, som gir oss inntektne i euro
data = data |> mutate(inc_euro = income/kronekurs)

#legger til en logisk vektor, som er 0,1 avhenig av om de har positiv eller negativ formue

data = data |> mutate(neg_wealth = ifelse(wealth<0,0,1))
# bruker ifelse for å lage en dummyvariabel

person_3 = data[3,]
print(person_3) #printer ut hele observasjonen

# lager en tibble med de fire første personene

some.persons = data #plukker ut de fire første observasjonene til en ny data-frame

# lager en dataframe med bare kvinnen

woman = data |> filter(female == "1")

#lager en data-frame med bare de 10% rikeste

kvantil = quantile(data$wealth,0.9)
#lager et nytt datasett der vi plukker ut dem som har en formue over kvantilen

wealhy = data[data$wealth>kvantil,]
non_wealthy = data[data$wealth<= kvantil,]

wealthy_ages = wealhy$age
non_wealthy_ages = non_wealthy$age


ggplot() + geom_density(aes(x = wealthy_ages, color = "Wealthy")) + geom_density(aes(x = non_wealthy_ages,color = "Non-wealthy")) + labs(x = "Alder", y = "tetthet", title = "Fordeling av alder")

#lager en funksjon som gir oss info om de ulike gruppene = rike og ikke-rike

info = function(x){
  return(c(Mean = mean(x), Var = var(x), Median = median(x)))
  
}

print(info(wealthy_ages))
print(info(non_wealthy_ages))

#gjør en t-test for å finne ut om forveningen til inntekt er lik hos både menn og kvinner. Den konkluderer med at det er ulik forventing
t.test(data$income~data$female)

#finner GINI-koefesienten til datasettet

gini = Gini(data$income)
print(gini)

#Det er en gini-koefeisent på 62%, som vitner om en ekstrem skjevfordeling i formue

#Skal gjøre bootstrapping


#Lager en vektor med bare inntektene
incomes = data$income
View(data)
gjennomsnitt = c() #lager en tom vektor med alle gjennomsnittene
ggplot(data) + geom_histogram(aes(incomes))
print(nrow(data))
for (i in 1:1000){ #trekker 1000 utvalg
  under_utvalg = sample(incomes,length(incomes),replace = TRUE) #trekker med tilbakelegging
  g = mean(under_utvalg)
  gjennomsnitt = c(gjennomsnitt,g)
}

ggplot() + geom_histogram(aes(x = gjennomsnitt)) + labs(x = "Gjennomsnittinntekt", y = "Antall", title = "Bootstrapping")#Vi ser at gjennomsnittet er normalfordelt, som gir oss et bedre estimat for den sanne forvenintgsverdien av inntektene

a = mean(data$income)
b = mean(gjennomsnitt)
print(c(a,b))

alder = data$age
kjonn = data$female
intekt = data$income

model = lm(intekt ~ kjonn + alder)
summary(model)

