# Generates the A matrix as described by the question.
# TODO: eventually should only store the non-zero elements of this matrix
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

# Generates the B matrix as described by the question.
gen_b_matrix <- function(){
	b_vec <- c(5, 11, 18, 21, 29, 40, 48, 48, 57, 72, 80, 76, 69, 87, 94, 85);
	b <- matrix(b_vec, ncol=1, nrow=16)
	return(b);
}

# Algorithm taken from https://en.wikipedia.org/wiki/Jacobi_method#Algorithm
# TODO: use tolerance
# TODO: create guess using len
jacobi_solver <- function(a_mat, b_mat, max_iter, tol, len){
	guess_mat <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
	guess_mat_next <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); # filled in later
	for(iter in 1:max_iter){
		for(i in 1:len){
			sigma <- 0;
			for(j in 1:len){
				if(i != j){
					sigma = sigma + (a_mat[i, j] * guess_mat[j]);
				}
			}	
			guess_mat_next[i] = (1 / a_mat[i, i]) * (b_mat[i] - sigma);
		}
		guess_mat <- guess_mat_next;
	}
	return(guess_mat);
}

# Algorithm taken from https://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method#Algorithm
# TODO: use tolerance
# TODO: create guess using len
gauss_seidel_solver <- function(a_mat, b_mat, max_iter, tol, len){
	result <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	for(iter in 1:max_iter){
		for(i in 1:len){
			sigma <- 0;
			for(j in 1:len){
				if(i != j){
					sigma <- sigma + (a[i, j] * result[j]);
				}
			}
			result[i] <- (1 / a[i, i]) * (b[i] - sigma);
		}
	}
	return(result);
}

do_work <- function(){
	a_mat <- gen_a_matrix();
	b_mat <- gen_b_matrix();
	#result <- jacobi_solver(a_mat, b_mat, 100, 0.01, 16);
	result <- gauss_seidel_solver(a_mat, b_mat, 100, 0.01, 16);
	return(result);
}