
library(tidyverse)

easter = function(Y){
  a = Y%%19
  b = Y%/% 100
  c = Y%% 100
  d = b %/% 4
  e = b%% 4
  g = (13 + 8*b)%/% 25
  h = (15+19*a + b - d - g)%% 30
  i = c%/% 4
  k = c%% 4
  l = (32 + 2*e + 2*i - h - k)%%7
  m = (a + 11*h + 19*l)%/% 433
  
  month = (90+h+l-7*m)%/% 25
  day = (19 + h + l - 7*m + 33*month)%%32
  return (c(day,month, day + (month-3)*7))
}
v = c()
for (y in 1900:3000){
  v = c(v,easter(y)[3])
}

print(length(v))
hist(v)
