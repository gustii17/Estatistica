#armazenamento numerio - não tem float
sa <- 3.6
si <- as.integer(sa) #trunca
sin <- round(sa) #arredonda

#armazenamento de caracters
nome = "nome"
nome2 = "jon"
nomes = c(nome, nome2)

nome == nome2

#fatores

carga = c(200, 220, 150)
summary(carga)
carga2 = as.factor(carga)
summary(carga2)
mode(carga)
class(carga)
carga3 = as.numeric(carga2)
summary(carga3)

#logico
L1 <- 3.6 < 3.8
L1

L2 <- TRUE
L3 <- c(1,TRUE, 5)

#vetores
#so um tipo
L3 <-c(1, 2, 3)
is.vector(L3[1])
mode(L3)

#lista
#vetor com tipos de dados diferentes
a <-c(1, 2, 3)
b <-c(1, "a", 3)
b
#b <- as.numeric(b)
#b

is.list(a)
is.list(b)
is.vector(b)

b <- list(10, "28", 2)
is.list(b)
mode(b)
str(b)

e <- list(c(10, 2, 3), "2, 8")
e[[1]][1]
e[[2]]


#matrizers
#um tipo de dado
m <- matrix(1:9, 3, 3)
m
m[1,3]

#dataframe






