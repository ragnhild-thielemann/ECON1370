
library(tidyverse)
terms = 100


gl = function(n){
  vektor = c()
  for (terms in n){
    
    pi_gl = 0 #startverdien er 3 
    
    for (i in 0:terms){
      ledd = 4/(2*i + 1)*(-1)**(i) #beregner leddene
      pi_gl = pi_gl + ledd
    }
    vektor = c(vektor,pi_gl)}
  return(vektor)}



nil = function(n){
vektor = c()
  for (terms in n){
    
  pi_n = 3 #startverdien er 3 

    for (i in 1:terms){
      nevner = (2*i)*(2*i + 1) *(2*i+2)
      ledd = 4/nevner *(-1)**(i+1)
      pi_n = pi_n + ledd
      }
      vektor = c(vektor,pi_n)}
      return(vektor)}

intraksjoner = c(10,100,1000,10000,100000)

pi_n = nil(intraksjoner) - pi
pi_g = gl(intraksjoner) - pi
print(pi_n)
print(pi_g)

ggplot() + geom_point(aes(x = intraksjoner, y = pi_n , color = "nilakantha")) + geom_point(aes(x = intraksjoner, y = pi_g, color = "Gregory-leinitz")) +scale_x_log10(guide = "axis_logticks") + labs(x = "Intraksjoner(n)", y = "Reusidaler fra pi", title = "Avvik fra pi")