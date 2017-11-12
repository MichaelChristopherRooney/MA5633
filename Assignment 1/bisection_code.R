# TODO: cleanup (= / <-)

func_a <- function(x){
	return((x^3) - 2);
}

func_b <- function(x){
	#exp(x) is R's way of doing e^x
	return(exp(x) - 2);
}

# differentiated version of func b
func_b_diff <- function(x){
	#exp(x) is R's way of doing e^x
	return(exp(x));
}

find_roots_bisection <- function(func, a, b) {
	tol <- 0.0000001;
	n_max <- 100000;
	n <- 1;
	old_a <- 0;
	old_b <- 0;
	while(n < n_max){
		c <- (a + b) / 2; # replace with operation in lecture slides
		# TODO: cache func(c) return value
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

find_roots_newton <- function(func, func_diff, x){
	tol <- 0.0000001;
	n_max <- 100000;
	n <- 0;
	while(n < n_max){
		y = func(x);
		y_prime = func_diff(x);
		x_next = x - (y / y_prime);
		if(abs(x - x_next) < tol){
			return(x);
		}
		x = x_next;
		n <- n + 1;
	}
}

find_roots_secant <- function(func, a, b){
	tol <- 0.0000001;
	n_max <- 100000;
	n <- 0;
	while(n < n_max){
		temp = (b - a)/(func(b) - func(a));
		temp = temp * func(b);
		a = b;
		b = b - temp;
		if(abs(b - a) < tol){
			return(b);
		}
		n = n + 1;
	}
	return(b);
}

