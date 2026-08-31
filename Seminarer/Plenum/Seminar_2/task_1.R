

#vi må løpe gjennom alle myntene
#begynner på indeks nummer i. Lager dette som en funksjon av prisen
money = function(pris){
  mynter = c(100,50,20,10,5,1) #kronestykkene han har
  kronestykker = length(mynter) #antall kronestykker han kan teste
  
  lommebok = c() #en tom lommebok
  betaling = 0

i = 1 
while (i<=kronestykker){
  mynt = mynter[i] #begynner med største seddel
  betaling = betaling + mynt
  lommebok = c(lommebok, mynt)
  
  
  if (betaling>pris){
    betaling = betaling - mynt
    lommebok = lommebok[-(length(lommebok))] #fjerner det overflødige elementet
    i = i + 1

  } 
}
return (length(lommebok))} #retunerer lengden på lommeboken



kandidatpriser = 1:100 #løper over prisene

for (pris in kandidatpriser){
  en_is = money(pris)
  to_is = money(2*pris)
  tre_is = money(3*pris)
  
  if (en_is == 4 & to_is == 6 & tre_is == 2){ #lager en boolsk opperator, som krever at alle har riktig verdi
    print(c("Prisen pa en is = ", pris))
  }
}

#Får ut at prisen på en is er 17 kroner

