# Tolerance for floating point operations.
# If the operation results in a value smaller than this then we instead use 0.
TOLERANCE <- 0.0000001;

##############################################################
# Helper functions for the LU factorisation with partial pivoting
##############################################################

# Takes a NxN matrix and a starting row/column number 'index'. 
# Returns the number of the row with the largest absolute value in the specified column, 
# where the row is either the starting row or after it.
find_row_with_largest_column_value <- function(mat, index){
	val <- abs(mat[index, index]);
	row_num <- index;
	for(i in (index + 1):nrow(mat)){
		if(abs(mat[i, index]) > val){
			val <- mat[i, index];
			row_num <- i;
		}
	}
	return(row_num);
}

# Swaps two rows of a matrix.
swap_matrix_rows <- function(mat, row_1, row_2){
	for(i in 1:ncol(mat)){
		temp <- mat[row_2, i];
		mat[row_2, i] <- mat[row_1, i];
		mat[row_1, i] <- temp;
	}
	return(mat);
}

# Subtracts each column of row_2 * the ratio from the matching columns of row_1.
subtract_rows_with_ratio <- function(mat, row_1, row_2, ratio){
	for(i in 1:ncol(mat)){
		val <- mat[row_1, i] - (ratio * mat[row_2, i]);
		if(abs(val) < TOLERANCE){
			mat[row_1, i] <- 0;
		} else {
			mat[row_1, i] <- val;
		}
	}
	return(mat);
}

# Creates a pivot matrix that has 1s on the diagonal.
create_pivot_matrix <- function(n){
	v <- vector(length=n*n);
	i <- 1;
	j <- 0;
	while(i < n*n){
		v[i + j] <- 1;
		i <- i + n;
		j <- j + 1;
	}
	mat <- matrix(v, ncol=n, nrow=n)
	return(mat);
}

# Fills a square matrix's diagonal with 1s.
insert_1_into_diagonal <- function(mat, n){
	i <- 1;
	j <- 1;
	while(i <= n){
		mat[i, j] <- 1;
		i <- i + 1;
		j <- j + 1;
	}
	return(mat);
}

# Returns a list that contains L, U and the pivot matrix in that order.
lu_factorise <- function(mat, n){
	ratios <- matrix(0L, nrow=n, ncol=n)
	pivot <- create_pivot_matrix(n);
	for(i in 1:(n - 1)){
		index = find_row_with_largest_column_value(mat, i);
		if(index != i){
			mat <- swap_matrix_rows(mat, index, i);
			pivot <- swap_matrix_rows(pivot, index, i);
			ratios <- swap_matrix_rows(ratios, index, i);
		}
		for(j in (i + 1):n){
			value <- mat[j, i];
			larger_value <- mat[i, i];
			mat <- subtract_rows_with_ratio(mat, j, i, (value / larger_value));
			ratios[j, i] <- (value / larger_value);
		}
	}
	ratios <- insert_1_into_diagonal(ratios, n);
	return(list(ratios, mat, pivot));
}

##############################################################
# Functions for solving the system
##############################################################

solve_for_c <- function(l, pivot_mat, b, n){
	c <- vector(length=n);
	b_pivot <- pivot_mat %*% b;
	# Handle first c value outside of loop.
	# This is trivial to solve as only the first element in the first row 
	# of L has a non-zero value and it will always be 1.
	c[1] <- b_pivot[1];
	for(i in 2:n){
		temp <- 0;
		for(j in 1:(i-1)){
			temp <- temp + (c[j] * l[i, j]);
		}
		c[i] <- b_pivot[i] - temp;
	}
	return(c);
}

solve_for_x <- function(u, c, n){
	x <- vector(length=n);
	# Handle first x value outside of loop.
	# This is trivial to solve as only the last element in the last row 
	# of U has a non-zero value.
	x[n] = c[n] / u[n,n];
	for(i in (n-1):1){
		temp <- 0;
		for(j in n:2){
			temp <- temp + (u[i, j] * x[j]);	
		}
		temp <- (c[i] - temp) / u[i,i];
		if(abs(temp) < TOLERANCE){
			x[i] <- 0;
		} else {
			x[i] <- temp;
		}

	}
	return(x);
}

print_results <- function(mat, b, u_mat, l_mat, pivot, c, x){
	cat(sprintf("======================================\nMatrix is:\n"));
	print(mat);
	cat(sprintf("\nB is:\n"));
	print(b);
	cat(sprintf("\nL is:\n"));
	print(l_mat);
	cat(sprintf("\nU is:\n"));
	print(u_mat);
	cat(sprintf("\nLU is:\n"));
	print(l_mat %*% u_mat);
	cat(sprintf("\nP is:\n"));
	print(pivot);
	cat(sprintf("\nPA (should equal LU) is:\n"));
	print(pivot %*% mat);
	cat(sprintf("\nc is:\n"));
	print(c);
	cat(sprintf("\nand x is:\n"));
	print(x)
	cat(sprintf("\n======================================\n"));
}

# mat should be a nxn matrix.
# n should be >0 and <= 99.
# b should be a vector of length n.
solve_system <- function(mat, n, b){
	result <- lu_factorise(mat, n);
	l_mat <- result[[1]];
	u_mat <- result[[2]];
	pivot_mat <- result[[3]];
	c <- solve_for_c(l_mat, pivot_mat, b, n);
	x <- solve_for_x(u_mat, c, n);
	print_results(mat, b, u_mat, l_mat, pivot_mat, c, x);
}

##############################################################
# Functions for generating A and b for each part of question 3.
##############################################################

gen_matrix_part_a <- function(){
	v <- c(9, 7, 2, 0, 7, 3, 6, 7, 9, 3, 2, 9, 7, 7, 6, 0, 6, 8, 2, 4, 7, 4, 2, 2, 3);
	mat <- matrix(v, ncol=5, nrow=5)
	return(mat)
}

gen_b_part_a <- function(){
	b <- c(35, 58, 53, 37, 39);
	return(b);
}

gen_matrix_part_b <- function(n){
	v <- c(length=n*n);
	mat <- matrix(v, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			mat[i, j] = (1 / ((i + j) - 1));
		}
	}
	return(mat);
}

gen_b_part_b <- function(){
	b <- c(5.0, 3.550 , 2.81428571428571, 2.34642857142857, 2.01746031746032);
	return(b);
}

gen_matrix_part_c <- function(n){
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

gen_b_part_c <- function(){
	b <- c(-4, -7, -6, -5, 16);
	return(b);
}

run_all_parts <- function(){
	# Part A
	mat <- gen_matrix_part_a();
	b <- gen_b_part_a();
	solve_system(mat, 5, b);
	# Part B
	mat <- gen_matrix_part_b(5);
	b <- gen_b_part_b();
	solve_system(mat, 5, b);
	# Part C
	mat <- gen_matrix_part_c(5);
	b <- gen_b_part_c();
	solve_system(mat, 5, b);
}

