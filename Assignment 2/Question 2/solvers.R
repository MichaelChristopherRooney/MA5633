# Generates the A matrix as described by the question.
# TODO: eventually should only store the non-zero elements of this matrix
gen_a_matrix <- function(n){
	v <- c(length=n*n);
	mat <- matrix(v, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			if(i == j){
				mat[i, j] = -4;
			} else if(abs(i - j) == 1){
				mat[i, j] = 1;
			} else if(abs(i - j) == 4){
				mat[i, j] = 1;
			} else {
				mat[i, j] = 0;
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
