


library(tidyverse)


I = function(n){
  verdier = runif(n,0,1)
  total = 0
  
  for (x in verdier){
    verdi <- (x-1)/(log(x))
    total <-total + verdi
  }
  print(total)
  svar <- 1/n*(sum((total)))
  return (svar)
}

verdier = tibble(N = c(10,100,1000,10000,100000))

verdier <- verdier |> mutate(Monte = sapply(N,I)) |> mutate(Diff = Monte-log(2))
View(verdier)

#tar logarismen av x-aksen, slik at den blir lettere å lese
ggplot(verdier) + geom_line(aes(x = N, y = Diff)) + scale_x_log10(TRUE)
View(verdier)
