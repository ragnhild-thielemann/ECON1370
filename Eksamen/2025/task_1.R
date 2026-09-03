
library(tidyverse)

data <- read_csv("esc.csv")

sprintf("Det er %s land som deltar, og %s land som er i finalen",nrow(data),ncol(data)-4)


#lager en vektor med alle overskriftene på kolonnene
vektor = c(names(head(data)))[-1]
land = c(names(head(data)))[-(1:4)]

#Dette er kolonnene vi kjører igjennom, som vi skal bruke
long <- data |> pivot_longer(cols = (vektor),
                     names_to = "Awarding",
                     values_to = "Score"
                     )  |> mutate(Score = replace_na(Score ,0))



#antall 12-ere
#lager en ny tibble, der vi summerer sammen antall 12-ere for hvert tall
sortert_land <- long |> summarise(s_12 = sum(Score == 12), .by = contestant) 
v = long$Score[long$Awarding== "jury_score"]
v = tibble(contestant =sortert_land$contestant ,Jury = vektor_til_merging)
#merger de sammen
sortert_land <- sortert_land |> left_join(v)


ggplot(sortert_land) + geom_point(aes(x = s_12, y=Jury )) + labs ( x = "Antall 12-ere", y = "Score fra Juryen")


cor(sortert_land$s_12, sortert_land$Jury)

deltagere <- unique(long$contestant)

ny_tibble <- long |> filter(Awarding %in% land)

vektor <- c()

print(length(deltagere))
for (d in deltagere){
  for (l in land){
    if (any(deltagere == l)){ #dette er bare relevant informasjon for landene som er deltagere

     s = ny_tibble$Score[ny_tibble$contestant == l & ny_tibble$Awarding == d]
    } else{
       s = 0
     }
      
    
  vektor = c(vektor, s)}
}

ny_tibble <- tibble(ny_tibble,vektor)
View(ny_tibble)


ggplot(ny_tibble) + geom_point(aes(x = Score, y = vektor))

cor(ny_tibble$Score, ny_tibble$vektor)
