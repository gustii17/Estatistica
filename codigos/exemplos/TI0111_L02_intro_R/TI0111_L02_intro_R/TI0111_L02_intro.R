# Introduction to R
######## Before start ######## 
# To execute the commands, copy and paste them on the R console.
# Verify your working directory
(WD <- getwd()) 									# print the current working directory

# importing rstudioapi package
install.packages("rstudioapi")
library("rstudioapi") 

# retrieving path from getSourceEditorContext() 
# using $ operator 
k <- getSourceEditorContext()$path 

# Move to your working directory
setwd(dirname(k))    				# change to your directory

# If you wish, change the precision in R (only for printing)
# This option controls the number of digits to print when printing numeric values. It is a suggestion only. Valid values are 1...22 with default 7.  
options(digits=2) 

###### Getting help ######
help.start()			# start online manual
??help

help(solve) 				# get help on a specific topic
example(solve)
?solve
??solve				# search within all the local R functions on your computer
apropos("solve")		# search within all the local R function names
RSiteSearch("solve")	# search on the web
	
######## An overgrowth calculator ######## 
# Numeric
5+2
5-2
5*2
5/2
sqrt(4) # Square root
2^2 	# Power
abs(-2)	# Absolute value
exp(2) 	# Exponential 
log(2) 	# Logarithm
rnorm(15) # Normal distribution
options(digits=5)

# "equal" ----> ==
# "greater than"/"less than" ---->  	>; <
# "greater equal" / "less equal"---->  	>= ; <= 
# "notequal" ---->  	!=  

#  Comparison between objects returns TRUE (T) o FALSE (F)
3*6 == 18
6 > 8

# Logic operators
# "and" ----->  &
# "or"  ----->  |
#  "not" ----->  !

-2 >5 & 3-1>0
3*2 >5 | 3-1<0
!FALSE

## Assignment
a = 3
3 = a 		# ops!!
1 -> c
b <- 2

# Inspect the objects
print(a)
a

# Operations between objects
c=a+b 	# it overwrites the c=1  

rm(a) # removes a from workspace
ls()  # lists the objects in the workspace 
rm(list=ls()) # clean the workspace

######## VECTORS ######## 
# The function is c() 
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
# alternatively
assign("x", c(16, 5.6, 3.1, 6.4, 21.7))
c(10.4, 5.6, 3.1, 6.4, 21.7) -> x
y <- c(x,0,x) # concatenates x and add elements to the new vector y
v <- 2*x+y+1

# Generating regular sequences
x <- c(1:30)
xr<- c(35:1)

n <- 10
x<-c(1:n-1)
xx <-c(1:(n-1)); xx

seq(-5,5,by=0.3) ->s3; s3
s4 <- seq(length=51, from=-5, by=.2); s4

x <- c(1, 3, 5, 7, 9)
s5 <- rep(x, times=5)
s6 <- rep(x, each=5)

# Logical vectors
temp <- x>5

# NB The elements of the vector must be of the same type!
v.num=c(0,2,-3,5,6,7,8,9,1)
v.string=c("a","b","c")
v.logic=c(T,T,F,T)

# The function mode() get the type of the object
mode(v.num)
mode(v.string)
mode(v.logic)

# Extract elements from a vector
v.num[3]					# Get the third element
v.num[c(1,5)] 			# Get the first and fifth element
v.num[1:3]				# Get the first three elements

# Extract element with a condition
maggiore0 = v.num >0
# Alternatively
v.num[v.num>0]
v.num[v.num<0]

# Combining logical conditions
v.num[v.num >0 & v.num <6]
v.num[v.num <0 | v.num >6]

########  ARRAYS ######## 
z<-c(1:1500)
dim(z) <- c(3,5,100)
x <- array(1:20, dim=c(4,5)) 		# Generate a 4 by 5 array. 
i <- array(c(1:3,3:1), dim=c(3,2)) 	# i is a 3 by 2 index array
x[i]  								# Extract those elements
x[i] <- 0							# Replace those elements by zeros.

# Permute arrays
A <- array(1:5, dim=c(4,5)) 
B <- aperm(A,c(2,1))
# Alternatively 
B<-t(A)
nrow(A) # Gives the number of rows in A
ncol(A) # Gives the number of columns in A

########  MATRIX ######## 
# The function is matrix()
data=c(1,2,3,4,5,6)
m=matrix(data, nrow = 3, ncol = 2) # or m=matrix(data,3,2) 
mode(m)

# To assign a name to the rows and columns we use rownames() and colnames()
rownames(m)=c("first_r", "second_r", "third_r")
colnames(m)=c("first_c", "second_c")

# By default, R fills the matrix cell by column, using the option "byrow=T" does it by row
m2=matrix(data, nrow = 3, ncol = 2, byrow = T)

m.num <- m 										 		# Numerical matrix
m.string <- matrix(c("a","b","c","d"), ncol=2)  		# String matrix
m.logic = matrix(c(T,TRUE,FALSE), nrow = 1, ncol = 3) 	# Logical matrix

diag(1, 5)	# Diagonal matrix

# Extract elements from a matrix
m[1,2]		# Get the element in (1,2)
m[1,]		# Get the elements on the first row
m[,2]		# Get the elements on the second column

# Matrix multiplication
data <-c(1:9)
A <- matrix(data,3,3) 
B <- t(A)

A * B 	# Matrix of element by element products
A %*% B # Matrix product
y <- c(10, 11, 12) 
y %*% A %*% y
crossprod(A,y)
diag(y)
diag(A)

########  LISTS ######## 
Lst <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9))
Lst$name
Lst$wife
Lst$child.ages[1] 

#########  DATA FRAME ######## 
# Define some vectors
height <- c(1.75, 1.80, 1.65, 1.90, 1.80, 1.71)
weight <- c(60, 72, 57, 90, 82, 72)
bmi <- weight / height^2   				# Body Mass Index
nam <- c("Francesca","Marco","Stefania","Davide","Luca","Valentina")
sex <- c("F", "M", "F", "M", "M", "F")
degree <- c(TRUE, FALSE, FALSE, T, T, F)

# Group the vectors in a data frame
df <- data.frame(nam,height,weight,bmi,sex,degree)

# Add a new object to the frame
driver <- c(T,F,T,T, NA, NA)
df <- data.frame(nam,height,weight,bmi,sex,degree,driver)
df <- data.frame(df,driver)

# Extract elements from the dataframe
df[[2]] 	# Based on the position
df$height 	# Based on the label

#########  Our first function ######## 
# getwd() 					# to get your current working directory
# setwd(your_new_dir) 		# to modify the working directory
file.edit("myfunsum.R")

source('myfunsum.r')
ls()

sumab(10,20)
sumab(-10,-20)


######## Example of dataset in R ########
data()
str(precip)
precip[1:4]

elements <- data.frame()
elements <- edit(elements)






