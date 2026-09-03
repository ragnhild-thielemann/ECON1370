library(tidyverse)

# --------------------------------------------------
# 1. LES INN DATA
# --------------------------------------------------

data <- read.csv(
  "MAT2400.csv",
  sep = ";",
  header = TRUE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)


# --------------------------------------------------
# 2. FJERN ID, DATO OG SVARTID
# --------------------------------------------------

# Vi beholder kolonne 3-28.
# Dette er selve spørreskjemasvarene.
data <- data[, 3:28]


# --------------------------------------------------
# 3. GI KOLONNENE NYE NAVN
# --------------------------------------------------

names(data) <- c(
  "antall_ganger_emnet",
  "gjennomforingsmate",
  "studiepoeng",
  "studieprogresjon",
  "inntrykk_av_kurset",
  "vanskelighetsgrad",
  "arbeidsmengde",
  "gjennomsnittlig_arbeidstid",
  "organisering_av_kurset",
  "annen_om_organisering",
  "kommer_pa_forelesning",
  "regner_ukesoppgaver",
  "gamle_eksamensoppgaver",
  "semsestersider",
  "egen_laring",
  "annet_om_laring",
  "kvalitet_pa_forelesningene",
  "forelesningsnotater",
  "relevans_av_ukesoppgaver",
  "annet_1",
  "relevans_av_obliger",
  "vanskelighetsgrad_pa_obliger",
  "tilbakemelding_pa_obliger",
  "annet_2",
  "eksamens_relevans",
  "vanskelighetsgrad_pa_eksamen"
)


# --------------------------------------------------
# 4. FJERN UNØDVENDIGE MELLOMROM
# --------------------------------------------------

# Dette er spesielt viktig for:
# "Ofte " og "Aldri "

data[] <- lapply(data, function(x) {
  if (is.character(x)) {
    trimws(x)
  } else {
    x
  }
})


# --------------------------------------------------
# 5. GJØR BINÆRE VARIABLER NUMERISKE
# --------------------------------------------------

# Hvor mange ganger tatt eksamen
data$antall_ganger_emnet <- as.numeric(
  data$antall_ganger_emnet
)


# Følger undervisning / følger ikke undervisning
data$gjennomforingsmate <- case_when(
  data$gjennomforingsmate ==
    "Følger undervisning og tar eksamen" ~ 1,
  
  data$gjennomforingsmate ==
    "Følger ikke undervisning, men tar eksamen" ~ 0,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 6. STUDIEPOENG
# --------------------------------------------------

data$studiepoeng <- case_when(
  data$studiepoeng == "21-30" ~ 2,
  data$studiepoeng == "31-40" ~ 3,
  data$studiepoeng == "41-50" ~ 4,
  data$studiepoeng == "Mer enn 50" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 7. STUDIEPROGRESJON
# --------------------------------------------------

data$studieprogresjon <- case_when(
  data$studieprogresjon == "2" ~ 2,
  data$studieprogresjon == "4" ~ 4,
  data$studieprogresjon == "6" ~ 6,
  
  data$studieprogresjon ==
    "Er ikke programstudent eller følger ikke vanlig studieprogresjon" ~ NA_real_,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 8. INNTRYKK AV KURSET
# --------------------------------------------------

# Godt / Svært godt

data$inntrykk_av_kurset <- case_when(
  data$inntrykk_av_kurset == "Godt" ~ 4,
  data$inntrykk_av_kurset == "Svært godt" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 9. VANSKELIGHETSGRAD
# --------------------------------------------------

data$vanskelighetsgrad <- case_when(
  data$vanskelighetsgrad == "Passe" ~ 3,
  data$vanskelighetsgrad == "Vanskelig" ~ 4,
  data$vanskelighetsgrad == "Svært vanskelig" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 10. ARBEIDSMENGDE
# --------------------------------------------------

data$arbeidsmengde <- case_when(
  data$arbeidsmengde == "Svært lite" ~ 1,
  data$arbeidsmengde == "Lite" ~ 2,
  data$arbeidsmengde == "Passe" ~ 3,
  data$arbeidsmengde == "Mye" ~ 4,
  data$arbeidsmengde == "Svært mye" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 11. GJENNOMSNITTLIG ARBEIDSTID
# --------------------------------------------------

data$gjennomsnittlig_arbeidstid <- case_when(
  data$gjennomsnittlig_arbeidstid == "0 - 5" ~ 1,
  data$gjennomsnittlig_arbeidstid == "6 - 10" ~ 2,
  data$gjennomsnittlig_arbeidstid == "11 - 15" ~ 3,
  data$gjennomsnittlig_arbeidstid == "16 - 20" ~ 4,
  data$gjennomsnittlig_arbeidstid == "21+" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 12. BRA / DÅRLIG-SKALA
# --------------------------------------------------

darlig_til_bra <- c(
  "organisering_av_kurset",
  "egen_laring"
)

for (kolonne in darlig_til_bra) {
  
  data[[kolonne]] <- case_when(
    data[[kolonne]] == "Svært dårlig" ~ 1,
    data[[kolonne]] == "Dårlig" ~ 2,
    data[[kolonne]] == "Hverken" ~ 3,
    data[[kolonne]] == "Bra" ~ 4,
    data[[kolonne]] == "Svært bra" ~ 5,
    
    TRUE ~ NA_real_
  )
}


# --------------------------------------------------
# 13. KOMMER PÅ / GJØR OPPGAVER
# --------------------------------------------------

aldri_til_alltid <- c(
  "kommer_pa_forelesning",
  "regner_ukesoppgaver",
  "gamle_eksamensoppgaver",
  "semsestersider"
)

for (kolonne in aldri_til_alltid) {
  
  data[[kolonne]] <- case_when(
    data[[kolonne]] == "Aldri" ~ 1,
    data[[kolonne]] == "Sjelden" ~ 2,
    data[[kolonne]] == "Av og til" ~ 3,
    data[[kolonne]] == "Ofte" ~ 4,
    data[[kolonne]] == "Alltid" ~ 5,
    
    TRUE ~ NA_real_
  )
}


# --------------------------------------------------
# 14. KVALITET PÅ FORELESNINGENE
# --------------------------------------------------

data$kvalitet_pa_forelesningene <- case_when(
  data$kvalitet_pa_forelesningene == "Dårlig" ~ 2,
  data$kvalitet_pa_forelesningene == "Hverken" ~ 3,
  data$kvalitet_pa_forelesningene == "Bra" ~ 4,
  data$kvalitet_pa_forelesningene == "Svært bra" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 15. FORELESNINGSNOTATER
# --------------------------------------------------

data$forelesningsnotater <- case_when(
  data$forelesningsnotater == "Svært lite" ~ 1,
  data$forelesningsnotater == "Lite" ~ 2,
  data$forelesningsnotater == "Passe" ~ 3,
  data$forelesningsnotater == "Mye" ~ 4,
  data$forelesningsnotater == "Svært mye" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 16. RELEVANS AV UKESOPPGAVER
# --------------------------------------------------

data$relevans_av_ukesoppgaver <- case_when(
  data$relevans_av_ukesoppgaver == "Dårlig" ~ 2,
  data$relevans_av_ukesoppgaver == "Hverken" ~ 3,
  data$relevans_av_ukesoppgaver == "Bra" ~ 4,
  data$relevans_av_ukesoppgaver == "Svært bra" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 17. RELEVANS AV OBLIGATORISKE OPPGAVER
# --------------------------------------------------

data$relevans_av_obliger <- case_when(
  data$relevans_av_obliger == "Hverken" ~ 3,
  data$relevans_av_obliger == "Relevant" ~ 4,
  data$relevans_av_obliger == "Svært relevant" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 18. VANSKELIGHETSGRAD PÅ OBLIGER
# --------------------------------------------------

data$vanskelighetsgrad_pa_obliger <- case_when(
  data$vanskelighetsgrad_pa_obliger == "Passe" ~ 3,
  data$vanskelighetsgrad_pa_obliger == "Vanskelig" ~ 4,
  data$vanskelighetsgrad_pa_obliger == "Svært vanskelig" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 19. TILBAKEMELDING PÅ OBLIGER
# --------------------------------------------------

data$tilbakemelding_pa_obliger <- case_when(
  data$tilbakemelding_pa_obliger == "Lite nyttig" ~ 2,
  data$tilbakemelding_pa_obliger == "Hverken" ~ 3,
  data$tilbakemelding_pa_obliger == "Nyttig" ~ 4,
  data$tilbakemelding_pa_obliger == "Svært nyttig" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 20. EKSAMENS RELEVANS
# --------------------------------------------------

data$eksamens_relevans <- case_when(
  data$eksamens_relevans == "Dårlig" ~ 2,
  data$eksamens_relevans == "Hverken" ~ 3,
  data$eksamens_relevans == "Bra" ~ 4,
  data$eksamens_relevans == "Svært bra" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 21. VANSKELIGHETSGRAD PÅ EKSAMEN
# --------------------------------------------------

data$vanskelighetsgrad_pa_eksamen <- case_when(
  data$vanskelighetsgrad_pa_eksamen == "Passe" ~ 3,
  data$vanskelighetsgrad_pa_eksamen == "Vanskelig" ~ 4,
  data$vanskelighetsgrad_pa_eksamen == "Svært vanskelig" ~ 5,
  
  TRUE ~ NA_real_
)


# --------------------------------------------------
# 22. SE PÅ DATASETTET
# --------------------------------------------------

View(data)

print(data)

str(data)

#spørre om hvordan man kan håndtere at det ikke er riktige bokstaver for det norske alfabetet