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

# See report for details on how this works.
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

plot_data <- function(an_est, an_actual, hn_est, hn_actual){
	indices <- rep(4:20);
	title <- "Estimated (black) and actual (red)\n condition number vs N";
	plot(y = an_est, x = indices, type="l", col="black", ylim=c(0, 20), ylab="Condition number", xlab="N", main = title);
	lines(y = an_actual, x = indices, col="red");
	dev.copy(jpeg,"an_mat.jpg");
	dev.off();
	plot(y = hn_est, x = indices, type="l", col="black", ylab="Condition number (log scale)", xlab="N", log="y", main = title);
	lines(y = hn_actual, x = indices, col="red");
	dev.copy(jpeg,"hn_mat.jpg");
	dev.off();
}

do_work <- function(){
	an_est <- c(length=16);
	an_actual <- c(length=16);
	hn_est <- c(length=16);
	hn_actual <- c(length=16);
	for(n in 4:20){
		index <- n - 3;
		mat <- gen_an_matrix(n);
		an_est[index] <- estimate_condition_number(mat, n)
		an_actual[index] <- 1 / rcond(mat, "I");
		mat <- gen_hn_matrix(n);
		hn_est[index] <- estimate_condition_number(mat, n)
		hn_actual[index] <- 1 / rcond(mat, "I");
	}
	plot_data(an_est, an_actual, hn_est, hn_actual);

}
