source("lu_factorisation.R")

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

get_initial_guess <- function(n){
	mat <- matrix(ncol=1, nrow=n)
	for(i in 1:n){
		mat[i, 1] = runif(1, 0.01, 0.02);
		#mat[i, 1] = 0.1;
	}
	return(mat);
}

calculate_infinity_norm <- function(mat){
	max <- 0;
	for(i in 1:nrow(mat)){
		row_sum <- 0;
		for(j in 1:ncol(mat)){
			row_sum = row_sum + abs(mat[i, j])
		}
		if(row_sum > max){
			max = row_sum;
		}
	}
	return(max);
}

# TODO: comment here
estimate_condition_number <- function(mat, n){
	# Result is a list that contains L, U and pivot in that order
	# This won't change and WON'T be recalculated later
	result <- lu_factorise(mat, n);
	inf_norm <- calculate_infinity_norm(mat);
	y = get_initial_guess(n);
	for(i in 1:5){
		y_temp <- y / calculate_infinity_norm(y);
		vec <- solve_system(mat, n, y_temp[,1], result[[1]], result[[2]], result[[3]]);
		y <- matrix(vec, ncol=1, nrow=n);
	}
	v <- calculate_infinity_norm(y);
	return((inf_norm * v));
}

do_work <- function(){
	for(n in 4:20){
		cat(sprintf("i = %d\n", n));
		mat <- gen_an_matrix(n);
		est <- estimate_condition_number(mat, n)
		actual <- 1 / rcond(mat, "I");
		cat(sprintf("An matrix: estimate=%f, actual=%f\n", est, actual));
		mat <- gen_hn_matrix(n);
		est <- estimate_condition_number(mat, n)
		actual <- 1 / rcond(mat, "I");
		cat(sprintf("Hn matrix: estimate=%f, actual=%f\n", est, actual));
	}
}
