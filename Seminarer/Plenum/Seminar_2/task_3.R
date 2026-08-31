
library(tidyverse) #importerer tidyverse-bibliotekt for plotting
R_vektor = c()
F_vektor = c()


Time = 200
a = 0.07
b = 0.002
c = 0.2
d = 0.0025 
R_1 = 80 ; F_1 = 20
R_vektor = c(R_vektor,R_1) ; F_vektor = c(F_vektor,F_1)
T_vektor = 1:Time
for (t in 2:Time){
  R_new = (1+a) *R_vektor[length(R_vektor)] - b*R_vektor[length(R_vektor)]*F_vektor[length(F_vektor)]
  R_vektor = c(R_vektor,R_new)
  F_new = (1-c)*F_vektor[length(F_vektor)] + d*R_vektor[length(R_vektor)]*F_vektor[length(F_vektor)]
  F_vektor = c(F_vektor,F_new)
}

ggplot() + geom_point(aes(x = T_vektor, y = F_vektor,color = "Rever")) + geom_point(aes(x = T_vektor, y = R_vektor, color = "harer")) + labs(x = "Tid", y = "Antall i bestanden", title = "Rever og harer")

#Vi ser at når det er en topp av rever, så tar det ikke lang tid før bestanden av  harer synker til 0. 


#lik kode, men en sykdom dreper alle harene
R_vektor = c()
F_vektor = c()


Time = 200
a = 0.07
b = 0.002
c = 0.2
d = 0.0025 
R_1 = 80 ; F_1 = 20
R_vektor = c(R_vektor,R_1) ; F_vektor = c(F_vektor,F_1)
T_vektor = 1:Time
for (t in 2:Time){
  if (t == 80){
    R_vektor[length(R_vektor)] = R_vektor[length(R_vektor)]*0.3
  }
  R_new = (1+a) *R_vektor[length(R_vektor)] - b*R_vektor[length(R_vektor)]*F_vektor[length(F_vektor)]
  R_vektor = c(R_vektor,R_new)
  F_new = (1-c)*F_vektor[length(F_vektor)] + d*R_vektor[length(R_vektor)]*F_vektor[length(F_vektor)]
  F_vektor = c(F_vektor,F_new)
}

ggplot() + geom_point(aes(x = T_vektor, y = F_vektor,color = "Rever")) + geom_point(aes(x = T_vektor, y = R_vektor, color = "harer")) + labs(x = "Tid", y = "Antall i bestanden", title = "Rever og harer")
