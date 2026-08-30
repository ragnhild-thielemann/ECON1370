

library(readr)
library(tidyverse)
pwt = read.csv("pwt.csv", header = TRUE, sep = ",")


#lager en ny tibble med bare gdp for Norge


gdp_nor = pwt |> filter(country == "Norway") |> filter(year>=1980 & year<2000)
gdp_nor = gdp_nor[3:4] #skal bare ha med året og bruttonasjonalproduktet vidre

# skal finne hvor mye den kinesiske økonomien har vokst

pwt.china = pwt |> filter(country == "China") |> filter(year>=1980 & year<2000)
pwt.china = (pwt.china |> mutate(growh = (gdp - lag(gdp))/lag(gdp)))[3:5] #fjerner igjen variablene som gir landkode og land

#
v_1970 = pwt$gdp[pwt$year == 1970]
v_2000 = pwt$gdp[pwt$year == 2000]

#Oppretter dette som en ny frame
pwt.growth = data.frame(Country = pwt$country[pwt$year == 1970],v_1970,v_2000)

pwt.growth = pwt.growth |> mutate(log_1970 = log(v_1970), growth = (v_2000-v_1970)/v_1970)

ggplot(pwt.growth) + geom_point(aes(x = log_1970, y = growth)) + labs(x = "Log(gdp_1970)" , y = "Vekst i BNP (prosent)")

#Vi ser av plottet at de landene som hadde små økonomier i 1970, har hatt større vekst enn de som var store i 1970

#Skal fortsette med det opprinnlige datasettet

#lager en ny variabel -> d, som viser om bnp har økt eller ikke
pwt = pwt |> group_by(country) |> mutate(d = ifelse(gdp-lag(gdp)<0,0,1))

a = pwt |> group_by(country) |> summarise(f = mean(d, na.rm = TRUE)) #finner andelen av årene der BNP i det gitte landet har økt


ggplot() + geom_point(aes(x = pwt.growth$log_1970, y = pwt.growth$growth)) + labs(x = "Log(gdp_1970)" , y = "Vekst i BNP (prosent)")

total_growth = pwt.growth$growth

ggplot() + geom_point(aes(y = total_growth, x = a$f)) + labs( x = "Andel av arene okonomien har okt", y = "Vekst i prosent")

#Vi ser at disse er positivt korrelert
