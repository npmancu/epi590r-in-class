# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2

square <- function(x) {
	square<-x*x
	return(square)
}

# test it out
square(x)

square(53)
53^2 # does this match?

#new power function
raise <- function(x, power=2) {
	new_val <- x^power
	return(new_val)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

# test
raise(x = 5)
# should give you
5^2
