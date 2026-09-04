

library(tidyverse)

n = 10000 #antall terms vi kjører
sigma = 2
b = 5
e = exp(rnorm(n,0,1))

total = 0
Nytte = function(C){
  u = (c**(1-sigma)-1)/(1-sigma)
  return(u)}
c = b + e

A = Nytte(c)
print(sum(A)/n)