# Tolerance for floating point operations.
# If the operation results in a value smaller than this then we instead use 0.
TOLERANCE <- 0.0000001;

# Takes a NxN matrix and a starting row/column number 'index'. 
# Returns the number of the row with the largest value in the specified column, 
# where the row is either (TODO: confirm ?) the starting row or after it.
find_row_with_largest_column_value <- function(mat, index){
	val <- mat[index, index];
	row_num <- index;
	for(i in (index + 1):nrow(mat)){
		if(mat[i, index] > val){
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

# Note: ratio is applied to row_2 row.
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

# TODO: comment and cleanup.
# Highly WIP.
# Returns a list that contains L, U and the pivot matrix in that order.
lu_factorise <- function(mat, n){
	if(n == 0 || n > 99){
		error("Invalid matrix size.");
		return(NA);
	}
	#cat("=== STARTING ===\n");
	#cat("Original matrix looks like:\n");
	#print(mat);
	#cat("\n");
	ratios <- matrix(0L, nrow=n, ncol=n)
	pivot <- create_pivot_matrix(n);
	for(i in 1:(n - 1)){
		index = find_row_with_largest_column_value(mat, i);
		#cat(sprintf("Largest value in column %d after row %d is in row %d\n", i, i, index));
		if(index != i){
			cat(sprintf("Swapping %d and %d\n", i, index));
			mat <- swap_matrix_rows(mat, index, i);
			pivot <- swap_matrix_rows(pivot, index, i);
			ratios <- swap_matrix_rows(ratios, index, i);
			#cat("After swap the matrix looks like:\n");
			#print(mat);
			#cat("\n");
		} else {
			#cat(sprintf("Not swapping for %d\n", i));
		}
		for(j in (i + 1):n){
			value <- mat[j, i];
			larger_value <- mat[i, i];
			#cat(sprintf("Ratio is %f\n", value / larger_value));
			mat <- subtract_rows_with_ratio(mat, j, i, (value / larger_value));
			ratios[j, i] <- (value / larger_value);
			#cat("After subtract the matrix looks like:\n");
			#print(mat);
			#cat("\n");
			#cat("Ratios are:\n");
			#print(ratios);
			#cat("\n");
		}
	}
	ratios <- insert_1_into_diagonal(ratios, n);
	#cat("U is:\n");
	#print(mat);
	#cat("\n");
	#cat("L is:\n");
	#print(ratios);
	#cat("\n");
	return(list(ratios, mat, pivot));
}

solve_for_c <- function(l, pivot_mat, b, n){
	c <- vector(length=n);
	b_pivot <- pivot_mat %*% b;
	#print(b_pivot);
	# Handle first c value outside of loop.
	# This is trivial to solve as only the first element in the first row 
	# of L has a non-zero value and it will always be 1.
	c[1] <- b_pivot[1];
	for(i in 2:n){
		temp <- 0;
		for(j in 1:(i-1)){
			temp <- temp + (c[j] * l[i, j]);
			#cat(sprintf("i: %d, j: %d, c[j]: %f, l[i, j]: %f, temp: %f, b_pivot[i]: %f\n", i, j, c[j], l[i,j], temp, b_pivot[i]));	
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
	return(x);
}

# Crappy functions for testing.

getb <- function(){
	#b <- c(5, 0, 6);
	b <- c(35, 58, 53, 37, 39);
	return(b);
}

getmatrix <- function(n){
	#v <- c(2, 4, 1, 1, 4, 3, 5, -4, 1);
	v <- c(9, 7, 2, 0, 7, 3, 6, 7, 9, 3, 2, 9, 7, 7, 6, 0, 6, 8, 2, 4, 7, 4, 2, 2, 3);
	mat <- matrix(v, ncol=n, nrow=n)
	return(mat)
}

