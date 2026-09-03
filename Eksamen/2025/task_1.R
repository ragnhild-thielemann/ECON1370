
library(tidyverse)

data <- read_csv("esc.csv")




ncol(data)
sprintf("Det er %s land som deltar, og %s land som er i finalen",nrow(data),ncol(data)-4)


#lager en vektor med alle overskriftene på kolonnene
vektor = c(names(head(data)))

vektor = vektor[-1] 
#Dette er kolonnene vi kjører igjennom, som vi skal bruke
long <- pivot_longer(data,cols = (vektor),
                     names_to = "Awarding",
                     values_to = "Score"
                     )



#bytter ut de tomme verdiene med 0
long <- long |> mutate(across(everything(), ~(replace_na(.x ,0))))

#antall 12-ere
#lager en ny tibble, der vi summerer sammen antall 12-ere for hvert tall
sortert_land <- long |> summarise(s_12 = sum(Score == 12), .by = contestant)
vektor_til_merging <- (long$Score[long$Awarding== "jury_score"])

v = tibble(contestant =sortert_land$contestant ,Jury = vektor_til_merging)
#merger de sammen
sortert_land <- sortert_land |> left_join(v)


ggplot(sortert_land) + geom_point(aes(x = s_12, y=Jury )) + labs ( x = "Antall 12-ere", y = "Score fra Juryen")


