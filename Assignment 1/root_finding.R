# TODO: cleanup (= / <-)
# TODO: use better operations as described in slides
# TODO: better saving to file
# TODO: check if initial value is root

# Global limits for each of the root finding functions
TOLERANCE <- 0.000001; # 10^-6
N_MAX <- 100000;

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

# n xn (bn-1 - an-1) an bn
find_roots_bisection <- function(func, a, b, filename) {
	n <- 1;
	old_a <- 0;
	old_b <- 0;
	c <- 0;
	results <- c("n", "xn", "(bn-1 - an-1)", "an", "bn");
	write(results, filename, ncolumns=5, append=FALSE, sep=" ");
	while(n < N_MAX){
		c <- (a + b) / 2; # replace with operation in lecture slides
		results <- c(n, c, old_b - old_a, a, b);
		write(results, filename, ncolumns=5, append=TRUE, sep=" ");
		# TODO: cache func(c) return value
		if(func(c) == 0 || ((b - a) / 2) < TOLERANCE){
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
	results <- c(n, c, old_b - old_a, a, b);
	write(results, filename, ncolumns=5, append=TRUE, sep=" ");
	return(c);
}

find_roots_newton <- function(func, func_diff, x, filename){
	n <- 1;
	x_next <- 0;
	actual_root = log(2);
	results <- c("n", "xn", "α - xn", "log10(α - xn)");
	write(results, filename, ncolumns=4, append=FALSE, sep=" ");
	while(n < N_MAX){
		y = func(x);
		y_prime = func_diff(x);
		x_next = x - (y / y_prime);
		results <- c(n, x_next, actual_root - x_next, log10(abs(actual_root - x_next)));
		write(results, filename, ncolumns=4, append=TRUE, sep=" ");
		if(abs(x - x_next) < TOLERANCE){
			return(x_next);
		}
		x = x_next;
		n <- n + 1;
	}
}

find_roots_secant <- function(func, a, b, filename){
	n <- 1;
	actual_root = log(2);
	results <- c("n", "xn", "α - xn");
	write(results, filename, ncolumns=3, append=FALSE, sep=" ");
	while(n < N_MAX){
		temp = (b - a)/(func(b) - func(a));
		temp = temp * func(b);
		a = b;
		b = b - temp;
		results <- c(n, b, actual_root - b);
		write(results, filename, ncolumns=3, append=TRUE, sep=" ");
		if(abs(b - a) < TOLERANCE){
			return(b);
		}
		n = n + 1;
	}
	return(b);
}

run_all <- function(){
	find_roots_bisection(func_a, 0, 2, "bisection1.txt");
	find_roots_bisection(func_b, 0, 1, "bisection2.txt");
	find_roots_newton(func_b, func_b_diff, 1, "newton.txt");
	find_roots_secant(func_b, 0, 1, "secant.txt");
}
