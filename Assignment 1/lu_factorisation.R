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
# Uses XOR swap algorithm to avoid temp variables.
# https://en.wikipedia.org/wiki/XOR_swap_algorithm
swap_matrix_rows <- function(mat, row_1, row_2){
	for(i in 1:ncol(mat)){
		mat[row_1, i] <- bitwXor(mat[row_1, i], mat[row_2, i]);
		mat[row_2, i] <- bitwXor(mat[row_2, i], mat[row_1, i]);
		mat[row_1, i] <- bitwXor(mat[row_1, i], mat[row_2, i]);
	}
	return(mat);
}

# Note: ratio is applied to row_2 row.
subtract_rows_with_ratio <- function(mat, row_1, row_2, ratio){
	for(i in 1:ncol(mat)){
		mat[row_1, i] <- mat[row_1, i] - (ratio * mat[row_2, i]);
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

# TODO: comment and cleanup.
# Highly WIP.
lu_factorise <- function(mat, n){
	if(n == 0 || n > 99){
		error("Invalid matrix size.");
		return(NA);
	}
	cat("=== STARTING ===\n");
	cat("Original matrix looks like:\n");
	print(mat);
	cat("\n");
	pivot <- create_pivot_matrix(n);
	for(i in 1:(n - 1)){
		index = find_row_with_largest_column_value(mat, i);
		cat(sprintf("Largest value in column %d after row %d is in row %d\n", i, i, index));
		if(index != i){
			cat(sprintf("Swapping %d and %d\n", i, index));
			mat <- swap_matrix_rows(mat, index, i);
			pivot <- swap_matrix_rows(pivot, index, i);
			cat("After swap the matrix looks like:\n");
			print(mat);
			cat("\n");
			for(j in (i + 1):n){
				value <- mat[j, i];
				larger_value <- mat[i, i];
				cat(sprintf("Ratio is %f\n", value / larger_value));
				mat <- subtract_rows_with_ratio(mat, j, i, (value / larger_value));
				cat("After subtract the matrix looks like:\n");
				print(mat);
				cat("\n");
			}
			
			

		} else {
			cat(sprintf("Not doing anything for %d\n", i));
		}
	}
}

# Takes n and returns a nxn matrix filled with values 1:(n*n)
# Just a helper function for testing
getmatrix <- function(n){
	v <- c(2, 4, 1, 1, 4, 3, 5, -4, 1);
	mat <- matrix(v, ncol=n, nrow=n)
	return(mat)
}

