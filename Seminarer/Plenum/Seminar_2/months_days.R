#Gjør først oppgaven der jeg gjør sekunder om til timer osv

sekunder = 583965

info = function(input){

  sec = 1
  min = sec* 60
  hour = min * 60
  day = hour*24
  vek = c(day,hour,min,sec)
  returning = c()
  
  for (i in 1:(length(vek))){ #løper gjennom lista
    par = vek[i]
    element = input%/% par
    input = input - element*par
    returning = c(returning, element)
    print(i)
  }
  return (returning)
  
  
}


