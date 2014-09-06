# Week 2 Quiz
# Anthony C Pineda
# 1. Create a vector that contains 20 numbers. 
#    (You may choose whatever numbers you like, but make sure there are some duplicates.)
randomNum <- sample(1:20, 5, replace=T)

# 2. Use R to convert the vector from question 1 into a character vector.
charVector <- as.character(randomNum)

# 3. Use R to convert the vector from question 1 into a vector of factors.
factorVector  <- cut(randomNum, 2)

# 4. Use R to show how many levels the vector in the previous question has.

factorLevel <- factor(factorVector)

# 5. Use R to create a vector that takes the vector from question 1 and performs on it the formula 3^2 ??? 4x + 1
a) Create Function Formula
  factorFormula <-  function(x) {
    3^2- 4*(x) + 1
  }
b)Apply Formula
  factorFormula(randomNum)

6. Implement ordinary least-squares regression in matrix form: ????^ = (????????????)???1????????????. As a useful double check 
you should be able to run your regression on the matrices X and y to get ????^ below:
X <- matrix(c(1.000, 1.000, 1.000,
                         1.000, 1.000, 1.000,
                         1.000, 1.000, 5.000,
                         4.000, 6.000, 2.000,
                         3.000, 2.000, 7.000,
                         8.000, 8.000, 9.000,
                         4.000, 7.000, 4.000,
                         9.000, 6.000, 4.000), nrow = 8, ncol = 3)

y <- matrix(c(45.2, 46.9, 31.0,
                         35.3, 25.0, 43.1,
                         41.0, 35.1), nrow = 8, ncol = 1)

 b = t(X)*(X) ^ -1 * t(X) * y


#7. Create a named list. That is, create a list with several elements that are each able to be referenced by name.
vName = list(Anthony=c('Anthony', 'Taiwan', 'M'), Sheryl=c('Sheryl', 'Philippines', 'F'))

#8. Create a data frame with four columns - one each character, factor (with three levels), numeric, and date. 
#   Your data frame should have at least 10 observations (rows).

name <- c('Anthony','Sheryl','Sandee','Chris','Sam','Dennis','Danny','Tom','Christina','Abby')
age <- cut(c(25, 30, 32, 30, 27, 29 , 23, 25, 39 ,41 ),3)
height<-c(5.8,5.5, 5.2, 4.9, 6.0, 5.8, 5.6, 6.2, 4.8, 5.7)
HireDate <- as.Date(c('2007-01-1','2007-2-1','2007-3-1','2007-4-1','2007-5-1','2007-6-1','2007-7-1','2007-8-1','2007-9-1','2007-10-1'))
Bio.data <- data.frame(name, age, height, HireDate)

#9. Illustrate how to add a row with a value for the factor column that isn't already in the list of levels. 
#   (Note: You do not need to accomplish this with a single line of code.)

age2 <- factor(age, levels=c(levels(age), '52'))

#10. Show the code that would read in a CSV file called temperatures.csv from the current working directory.

fileName <- "C:/Anthony/School_CUNY/IS 607/Week2/a.csv"
# Read FileName
csv.data = read.csv(fileName)

#11. Show the code that would read in a TSV file called measurements.txt from a directory other than the 
#    working directory on your local machine.

fileName <- "C:/Anthony/School_CUNY/IS 607/Week2/a.csv"
# Read FileName
tsv.data = read.delim(fileName)


#12. Show the code that will read in a delimited file with a pipe separator (the "|" symbol) from a website 
#    location. (You may make up an appropriate URL.)

fileName <-  read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module3/ExampleData3.txt"
x <- read.table(fileName, header = TRUE, sep = "\t")


#13. Write a loop that calculates 12-factorial.

getRandom <- function() {
  # appendVal as the Incremental Numerica Value Array[1:n]
  appendVal <- 2  
    for (i in 1:12 ) {
      # 2nd For Loop for Generating Random Generation using RStream Package
        appendVal <- appendVal * 2
        print(appendVal)
      }
  
} 


#14. Use a loop to calculate the final balance, rounded to the nearest cent, in an account that earns 3.24% 
#interest compounded monthly after six years if the original balance is $1,500.

getInterest <- function(x) {
  
  for (i in 1:72 ) {
    # Loop value is 72 for 6 Years
    #  3.24 / 12 Months = .27 
    x <- x * 1.0027              
    #print(i)
    #print(x)
  }
  print( round(x, digits=2) ) 
} 


#15. Create a numeric vector of length 20 and then write code to calculate the sum of every third element of the 
#vector you have created.

getThirdElement <- function(x) {
  a <- 0
  thirdElem <- 0 
  for (i in 1:20 ) {
    a <- i / 3
    print(a)
    if (  a == 1  ) | (  a == 2  )| (  a == 3  )| (  a == 4  )| (  a == 5  )| (  a == 5  )   {
      thirdElem <- thirdElem + x 
    }
  } 
  
  print(thirdElem)  
}


#16. Use a for loop to calculate ??? ???? 10 ????
#????=1 for the value ???? = 2.

getSumLoopFor <- function(x) {
  
  for (i in 1:10 ) {
    y <-  x ^ i
    print(y)
  }
}


#17. Use a while loop to accomplish the same task as in the previous exercise.
getSumLoopWhile <- function(x) {
      i <- 0
    while(i < 11) {
      i <- i + 1
      y <-  x ^ i
      print(y)
    }
}

#18. Solve the problem from the previous two exercises without using a loop.



#19. Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.

getSequence <- function(x,y) {
  
  for (i in 1:20 ) {
    if (x == y)  { 
      break
    } else {
      x <-  x+5  
    }
    print(x)
  }
}

#20. Show how to create a character vector of length 10 with the same word, "example", ten times.
name <- c('example','example','example','example','example','example','example','example','example','example')

#21. Show how to take a trio of input numbers a, b, and c and implement the quadratic equation

getQuad <- function(a,b,c) {
    x <-    ( (-1*b)  -   sqrt((b^2)-(4*a*c)) )  / 2*a
    print (x)
}  

