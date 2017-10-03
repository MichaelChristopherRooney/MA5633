print_col_mean <- function(col_num, mean){
	cat("Mean of column ", col_num)
	cat(" is ", mean)
	cat("\n")
}

print_col_std_dev <- function(col_num, std_dev){
	cat("Standard deviation of column ", col_num)
	cat(" is ", std_dev)
	cat("\n")
}

get_col_mean <- function(mat, col_num){
	mean <- 0.0
	for(i in 1:nrow(mat)){
		mean <- mean + mat[i, col_num]
	}
	mean <- mean / nrow(mat)
	return(mean)
}

get_col_std_dev <- function(mat, col_num, col_mean){
	std_dev <- 0.0
	for(i in 1:nrow(mat)){
		diff <- col_mean - mat[i, col_num]
		diff <- diff * diff
		std_dev <- std_dev + diff
	}
	std_dev <- std_dev / nrow(mat)
	std_dev = sqrt(std_dev)
	return(std_dev)
}

plot_table <- function(mat){
	x <- mat[,1]
	y <- mat[,2]
	err <- mat[,3]
	par(mar=c(5, 4, 4, 8) + 0.1)
	plot(x, y, type="b", pch=21, col="red", yaxt="n", lty=3, xlab="From 1st col", ylab="From 2nd col")
	arrows(x, y, x1=x, y1=err, length=0.05, angle=90, code=3)
	
}

analyse_table <- function(){
	mat <- read.table("mytable.txt", header=FALSE, sep=" ")
	if(ncol(mat) < 3){
		warning("Number of columns must be greater than 3")
		return(NA)
	}
	if(nrow(mat) == 0){
		warning("Matrix has 0 rows, nothing to do")
		return(NA)
	}
	for(j in 1:ncol(mat)){
		mean = get_col_mean(mat, j)
		print_col_mean(j, mean);
		std_dev = get_col_std_dev(mat, j, mean)
		print_col_std_dev(j, std_dev)
	}
	plot_table(mat)
}
