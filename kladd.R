
library(tidyverse)
x = c(7,9.3,13.2,16.3,19.1,22.0)
y = c(1046 ,1065 ,1094, 1117 ,1130 ,1135)

model = lm(y~x)
summary(model)

y_hat = function(x){
  a = coefficients(model)[1]
  b = coefficients(model)[2]
  return (a + b*x)
}

reu = y-y_hat(x)
n = length(x)
nevner = c()
S_xx = sum(x**2)-n*mean(x)**2
for (i in 1:n){
  o = (1-1/n-(x[i]-mean(x))**2/S_xx)**0.5*7.265
  nevner = c(nevner,o)
}

print(nevner)
reu = reu
cor(x,y)
ggplot() + geom_point(aes(x = x , y = reu))