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
jacobi_solver <- function(a_mat, b_mat, guess_mat, max_iter, tol, len){
	guess_mat_t = guess_mat;
	guess_mat_t1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); # filled in later
	for(iter in 1:max_iter){
		for(i in 1:len){
			sigma <- 0;
			for(j in 1:len){
				if(i != j){
					sigma = sigma + (a_mat[i, j] * guess_mat_t[j]);
				}
			}	
			guess_mat_t1[i] = (1 / a_mat[i, i]) * (b_mat[i] - sigma);
		}
		guess_mat_t <- guess_mat_t1;
		guess_mat_t1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); # filled in later
	}
	return(guess_mat_t);
}

do_work <- function(){
	a_mat <- gen_a_matrix();
	b_mat <- gen_b_matrix();
	guess_mat <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	result <- jacobi_solver(a_mat, b_mat, guess_mat, 100, 0.01, 16);
	return(result);
}
