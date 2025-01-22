# Biological Networks
## Spring 2025
### A brief introduction to R computing

By the end of this quick crash course, you'll understand object assignment, a few data structures, creating functions and basic plotting. 
You might find it helpful **NOT** to copy/paste these code snippets into your programming environment. 
Instead, take the time to write everything out on your own, dissect and understand each argument.
> R is a language. Use it everyday and you will learn quickly.
> -Henry Stevens

#### 1. Object assignment
In general in R, you perform an action, take the results of that action and
assign the results to a new object. Here I add
two numbers and assign the result to an object called `a`.

```r
a <- 2 + 3
a
```
You can use an arrow to make the assignment with a
less-than sign (<) and a dash (-). Note also, that to reveal the contents of the object,
you can type the name of the object. You can also make assignments with the `=` operator.

```r
a = 2 + 3
a
```
This way goes against historical conventional but, honestly, who cares? There are a few edge cases where this can lead
to undesirable behavior, but I don't think we will encounter those in our course. I would just use `=`.

You can then use the object to perform other actions, for instance:

```r
b = a + a
b
```

Oh, also we use `#` to annotate our code. Anything you put after a `#` will not be evaluated by the computer.


#### 2. Data structures
A single real number is called a *scalar*. But, most
objects in R are more complex. Here I'll describe some of these other objects that will make your life easier: *vectors*, *functions*, *matrices*, and *data frames*. 
R operates (does stuff) on objects.

#### 2a. Vectors
So many operations in R are performed on vectors. A vector may be a sequence of numbers, for instance [1, 2, 3, 4]. 
Let's create a vector called `x`. 
In R, we do this manually with `c()`.

```r
x = c(1, 2, 3, 4)
```

#### 2b. Functions
In R, a function is a set of instructions that perform a specific task. 
Functions take input in the form of one or more arguments, and they return an output. 
You have already used a
function in this primer (`seq` is one). 
Let’s examine functions more closely. Among other things, a function has a *name*, *arguments*, and *values*.
The syntax for defining a function in R is as follows:

```r
function_name = function(argument1, argument2, ...) {
    # A set of instructions
    # ...
    # return output
}
```

Once a function is defined, you can call it by typing the function name, 
followed by the input arguments in parentheses. 
For example, if you have a function called `add_numbers` that takes two arguments and returns their sum, 
you would define it and call the function like this:

```r
## First, define my function
add_numbers = function(oneNumber, anotherNumber){
    # Add my two numbers together
    mySum = oneNumber + anotherNumber
    return(mySum)
}

## Now, let's test it
add_numbers(6, 2.5)
```

R also has tons of built in functions that do various things. You can pull up the help page
for any function by typing `?theNameOfMyFunction` like:

```r
?mean
```

This will open the help page (again), showing us the *arguments*. 
The first argument `x` is the object for which a mean will be calculated. The second argument
is `trim = 0`. If we read about this argument, we find that it will “trim” a specified
fraction of the most extreme observations of x. The fact that the argument trim
is already set equal to zero means that is the default. If you do not use trim,
then the function will use trim = 0.

```r
x = 1:4
mean(x)
```

#### 2c. Writing your own functions
The reason R has become *lingua franca* for scientific computing is its extensibility. 
It's *object-oriented* nature means anyone can write code that anyone can use. 
People publish entire ***packages*** (which I'll show you how to install and load in a moment), 
which are integrated collections of functions that do different cool stuff. 
There are hundreds if not thousands of these; if you need to do something in R, odds are that there is already a package for it.

A function can take any input, do stuff, including produce graphics, or
interact with the operating system, or manipulate numbers. 
You decide on the arguments of the function. 
Let’s make our own function to calculate a mean. 
Pretend you work for an unethical boss who wants you to show that average sales are higher
than in reality; i.e., your function should be able to take a vector (a list) of company sales 
(we'll call this vector `realSales`), provide the average of `realSales` and fudge it
by adding 10%.

```r
bogusMean = function(x, cheat){
	## Add up all of my sales to get a total
	totalSales = sum(x)
	## Calculate the number of sales I had
	numberOfSales = length(x)
	## Calculate the actual average sale amount
	trueMean = totalSales / numberOfSales
	fakeMean = (1 + cheat) * trueMean
	return(fakeMean)
}
```

Remember, you decide the arguments of the function, in this case, `x` and `cheat`.

```r
realSales = c(9.99, 7.99, 1.99, 4.99)
adjustment = 0.1
bogusMean(x = realSales, cheat = adjustment)
```

### 3. Matrices and Data Frames
If you imagine a vector as a 1-dimensional object, matrices and data frames can be thought of as 2-dimensional ones.
We can make a matrix like this:

```r
A = matrix(          ## call the `matrix` function to indicate that I want to make a matrix
    c(1, 2, 3, 4),   ## specify the values in each cell of my matrix
    nrow = 2,        ## indicate that my matrix has 2 rows
    byrow = TRUE     ## indicate that I want to fill in my matrix by row
)                    ## close the parentheses on `matrix`
```

Note that you can continue things on a new line to help with readability.
We can do tons of things with matrices.

```r
## Scale each element by a constant
2 * A

## Add and multiply elements in a matrix together
B = matrix(c(5, 6, 7, 8), nrow = 2, byrow = FALSE) ## Note: see what happens with FALSE here?
A + B
A * B

## Matrix multiplication
A %*% B
```
Data frames are like matrices, but with names columns and a few other properties.
We can't do matrix algebra with them, but we can do stuff like store mixed data in a clean way.

```r
## Define my data frame
df = data.frame(
    'x' = c('A', 'B', 'C', 'D'),
    'y' = c(1, 2, 3, 4),
    'z' = c(5, 6, 7, 8)
)

## Look at the whole thing
df

## Pull out just column `x` using a `$`
df$x
df$y

## Add two columns together element-wise
df$y + df$z

## We can also add new columns after the fact, like:
df$a = df$y * df$z
```

And we can use R's base graphics system to make quick and easy plots.

```r
plot(a ~ y, data = df)
```

In RStudio, we can save this plot as a PDF or PNG by clicking `Export` above the graphics pane.
