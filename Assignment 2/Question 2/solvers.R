# Generates the A matrix as described by the question.
gen_a_matrix <- function(){
	n <- 16;
	v <- c(length=n*n);
	mat <- matrix(v, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			if(i == j){
				mat[i, j] = -4.0;
			} else if(abs(i - j) == 1){
				mat[i, j] = 1.0;
			} else if(abs(i - j) == 4){
				mat[i, j] = 1.0;
			} else {
				mat[i, j] = 0.0;
			}
		}
	}
	return(mat);
}

# The values at each location in the A matrix described in the question follow a patten.
# Using this pattern here we can find the value at each location, while not needing to 
# store the many elements with a value of zero.
# With small matrices this does not matter so much, but with larger matrices significant
# space would be saved by using this approach.
get_value_from_a <- function(i, j){
	if(i == j){
		return(-4.0);
	}
	abs_diff <- abs(i - j);
	if(abs_diff == 1 || abs_diff == 4){
		return(1.0);
	}
	return(0.0);
}

# Generates the B matrix as described by the question.
gen_b_matrix <- function(){
	b_vec <- c(5, 11, 18, 21, 29, 40, 48, 48, 57, 72, 80, 76, 69, 87, 94, 85);
	b <- matrix(b_vec, ncol=1, nrow=16)
	return(b);
}

# Generates a zero-filled matrix with 1 column and n rows
gen_guess_matrix <- function(n){
	g_vec <- numeric(n);
	g <- matrix(g_vec, ncol=1, nrow=n);
	return(g);
}

# Algorithm taken from https://en.wikipedia.org/wiki/Jacobi_method#Algorithm
# TODO: use tolerance
jacobi_solver <- function(b_mat, max_iter, tol, len){
	guess_mat <- gen_guess_matrix(len);
	guess_mat_next <- gen_guess_matrix(len);
	for(iter in 1:max_iter){
		for(i in 1:len){
			sigma <- 0;
			for(j in 1:len){
				if(i != j){
					sigma = sigma + (get_value_from_a(i, j) * guess_mat[j, 1]);
				}
			}	
			guess_mat_next[i, 1] = (1 / get_value_from_a(i, i)) * (b_mat[i] - sigma);
		}
		guess_mat <- guess_mat_next;
	}
	return(guess_mat);
}

# Algorithm taken from https://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method#Algorithm
# TODO: use tolerance
gauss_seidel_solver <- function(b_mat, max_iter, tol, len){
	result <- gen_guess_matrix(len);
	for(iter in 1:max_iter){
		for(i in 1:len){
			sigma <- 0;
			for(j in 1:len){
				if(i != j){
					sigma <- sigma + (get_value_from_a(i, j) * result[j, 1]);
				}
			}
			result[i, 1] <- (1 / get_value_from_a(i, i)) * (b[i] - sigma);
		}
	}
	return(result);
}

do_work <- function(){
	a_mat <- gen_a_matrix();
	b_mat <- gen_b_matrix();
	result <- jacobi_solver(b_mat, 100, 0.01, 16);
	cat("Result using Jacobi solver is:\n");
	print(result);
	result <- gauss_seidel_solver(b_mat, 100, 0.01, 16);
	cat("Result using Gauss-Seidel solver is:\n");
	print(result);
	cat("Result using R's built in solve function is:\n");
	print(solve(a_mat, b_mat));
}
