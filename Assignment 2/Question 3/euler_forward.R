# y' as described in i)
func_i <- function(x, y){
	return((-y) * log(y));
}

# y' as described in ii)
func_ii <- function(x, y){
	return(1 - (4*y));
}

# y' as described in iii)
func_iii <- function(x, y){
	return(y);
}

euler_forward <- function(dy_dx = function(x, y){}, initial_val, step_size, start, end){
	num_steps <- (end - start) / step_size;
	ys <- numeric(num_steps + 1);
	ys[1] <- initial_val;
	for(i in 1:num_steps){
		x <- start + ((i - 1) * step_size);
		ys[i+1] <- ys[i] + (step_size * dy_dx(x, ys[i]));
	}
	return(ys);
}

do_work <- function(){
	return(euler_forward(func_iii, 1, 0.0125, 0, 4));
}
