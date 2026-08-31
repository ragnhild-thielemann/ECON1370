library(microbenchmark) #importerer biblioteket som tar tiden
#Fibonacci som vektorfunksjon

fibo_vector = function(n){

fib = c() #gjør det elementvis, der jeg henter ut elementer i vektoren

for (i in 1:n){
  if (i == 1 | i == 2){ #starter med F_1 = 1 og F_2 = 1
    fib = c(fib, 1)
  } else{ 
    f_1 = fib[length(fib)]
    f_2 = fib[length(fib)-1]
    nytt_ledd = f_1 + f_2
    fib = c(fib, nytt_ledd)
  }
}
return (fib[length(fib)])}

 #printer siste element i denne listen

#Fibonaccio som rekusiv funksjon som kaller på seg selv

fibo_funk = function(n){
  if (n == 1 | n == 2){ #passer på at den kjører for de to første
    return (1)
  } else{
    return (fibo(n-1) + fibo(n-2))
  }
}


#begge funksjonene skriver  ut det samme

#Skal nå finne ut hvor lang tid de ulike funksjonene tar

# Bruker microbenchmark-bibloiteket til dette

n = 20

red = microbenchmark(fibo_funk(n),fibo_vector(n),times = 100) #kjører over begge kodene 100 ganger

summary(red)

#Vi ser at når det er en rekusiv kode, så tar koden vesenlig lengre tid