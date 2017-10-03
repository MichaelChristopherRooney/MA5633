get_col_mean <- function(mat, col_num){
	avg <- 0.0
	for(i in 1:nrow(mat)){
		avg <- avg + mat[i, col_num]
	}
	avg <- avg / nrow(mat)
	cat("Average of column ", col_num)
	cat(" is ", avg)
	cat("\n")
}

get_col_std_dev <- function(mat, col_num){

}

analyse_table <- function(){
	mat <- read.table("mytable.txt", header=FALSE, sep=" ")
	if(ncol(mat) < 3){
		warning("Number of columns must be greater than 3")
		return(NA)
	}
	for(j in 1:ncol(mat)){
		get_col_mean(mat, j)
	}
}
