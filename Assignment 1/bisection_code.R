func_a <- function(x){
	return((x^3) - 2);
}

find_roots <- function(func, a, b) {
	tol <- 0.0001;
	n_max <- 10000;
	n <- 1;
	old_a <- 0;
	old_b <- 0;
	while(n < n_max){
		c <- (a + b) / 2; # replace with operation in lecture slides
		if(func(c) == 0 || ((b - a) / 2) < tol){
			return(c);
		}
		old_a = a;
		old_b = b;
		if(func(c) * func(a) < 0){
			b <- c;
		} else {
			a <- c;
		}
		n <- n + 1;
	}
	return(c);
}
