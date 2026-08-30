

#Funksjon for om de truer hverandre eller ikke
t = function(x1,y1,x2,y2){
  col = y1 == y2
  row = x1 == x2
  dig = abs(x2-x1) == abs(y2-y1)
  
  if (col == T| row == T | dig == T){
    return (TRUE)
  } else{
    return (FALSE)
  }
}


true = function(x,gamle){
  
  row = length(gamle) + 1 # vi er en rad over de gamle x-verdiene
  if (row == 1){
    return (FALSE)
  }
  x1 = x
  y1 = row
  truet_vektor = c() #lager en vektor for hvilke posisjoner som truer dronninga
  for (r in 1:(row-1)){
    y2 = r
    x2 = gamle[r]
    print(c(x1,y1,x2,y2))
    if ((t(x1,y1,x2,y2))== TRUE){
      truet_vektor = c(truet_vektor, TRUE)
    } else{
      truet_vektor = c(truet_vektor, FALSE)
      
    }
  }
  print(truet_vektor)
  if (any(truet_vektor)== TRUE){
    return(TRUE)} else{
      return (FALSE)
    }
}

#skal nå jobbe oss oppover langs brettet
check_row = function(ro)