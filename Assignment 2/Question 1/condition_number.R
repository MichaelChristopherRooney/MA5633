# Generates matrix Hn = [hij]
# Hij = 1 / (i + j - 1)
gen_hn_matrix <- function(n){
	v <- c(length=n*n);
	mat <- matrix(v, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			mat[i, j] = (1 / ((i + j) - 1));
		}
	}
	return(mat);
}

# Generates matrix An = [aij]
# Aij = 
# 1 if i = j
# 4 if i - j = 1
# -4 if i - j = -1
# 0 otherwise
gen_an_matrix <- function(n){
	v <- c(length=n*n);
	mat <- matrix(v, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			if(i == j){
				mat[i, j] = 1;
			} else if(i - j == 1){
				mat[i, j] = 4;
			} else if(i - j == -1){
				mat[i, j] = -4;
			} else {
				mat[i, j] = 0;
			}
			
		}
	}
	return(mat);
}

calculate_infinity_norm <- function(mat){
	if(nrow(mat) != ncol(mat)){
		warning("Matrix must be square!");
		return(NA);
	}
	n <- nrow(mat);
	max <- 0;
	for(i in 1:n){
		row_sum <- 0;
		for(j in 1:n){
			row_sum = row_sum + mat[i, j]
		}
		cat(sprintf("Sum of row %d is %f\n", i, row_sum));
		if(row_sum > max){
			max = row_sum;
		}
	}
	cat(sprintf("Max is %f\n", max));
	return(max);
}

estimate_condition_number <- function(mat, inf_norm){

}

do_work <- function(n){
	mat <- gen_hn_matrix(n);
	inf_norm <- calculate_infinity_norm(mat);
	return(inf_norm);
}
