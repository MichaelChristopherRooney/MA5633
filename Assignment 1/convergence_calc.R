calculate_ratio <- function(x0, x1, p, alpha){
	temp1 <- abs(alpha - x1);
	temp2 <- abs(alpha - x0);
	temp2 <- temp2 ^ p;
	return(temp1 / temp2);
}


calculate_ratios <- function(){
	data <- c(4.013476, 3.049617, 2.144371, 1.378654, 0.882489, 0.7099926, 0.6932883, 0.6931472);
	cat("For p=2:\n")
	for(i in 1:7){
		ratio <- calculate_ratio(data[i], data[i+1], 2, log(2));
		cat(sprintf("n=%d, ratio=%f\n", i, ratio));
	}
	cat("For p=1:\n")
	for(i in 1:7){
		ratio <- calculate_ratio(data[i], data[i+1], 1, log(2));
		cat(sprintf("n=%d, ratio=%f\n", i, ratio));
	}
	cat("For p=3:\n")
	for(i in 1:7){
		ratio <- calculate_ratio(data[i], data[i+1], 3, log(2));
		cat(sprintf("n=%d, ratio=%f\n", i, ratio));
	}
	cat("For p=1.9:\n")
	for(i in 1:7){
		ratio <- calculate_ratio(data[i], data[i+1], 1.9, log(2));
		cat(sprintf("n=%d, ratio=%f\n", i, ratio));
	}
	cat("For p=2.1:\n")
	for(i in 1:7){
		ratio <- calculate_ratio(data[i], data[i+1], 2.1, log(2));
		cat(sprintf("n=%d, ratio=%f\n", i, ratio));
	}
}
