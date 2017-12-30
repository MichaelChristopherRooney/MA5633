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

# Returns all y values that were calculated
euler_forward_with_all_ys <- function(dy_dx = function(x, y){}, initial_val, step_size, start, end){
	num_steps <- (end - start) / step_size;
	ys <- numeric(num_steps + 1);
	ys[1] <- initial_val;
	for(i in 1:num_steps){
		x <- start + ((i - 1) * step_size);
		ys[i+1] <- ys[i] + (step_size * dy_dx(x, ys[i]));
	}
	return(ys);
}

# Returns only the last y value that was calculated
euler_forward <- function(dy_dx = function(x, y){}, initial_val, step_size, start, end){
	num_steps <- (end - start) / step_size;
	y_prev <- initial_val;
	y_next <- 0;
	for(i in 1:num_steps){
		x <- start + ((i - 1) * step_size);
		y_next <- y_prev + (step_size * dy_dx(x, y_prev));
		y_prev <- y_next;
	}
	return(y_next);
}

trapezoid_predictor_corrector <- function(dy_dx = function(x, y){}, initial_val, step_size, start, end){
	num_steps <- (end - start) / step_size;
	y_prev <- initial_val;
	y_next <- 0;
	for(i in 1:num_steps){
		# Predict using forward euler's method
		x <- start + ((i - 1) * step_size);
		y_next <- y_prev + (step_size * dy_dx(x, y_prev));
		# Now correct with trapezoidal rule
		y_next <- y_prev + (0.5 * step_size * (dy_dx(x, y_prev) + dy_dx(x + step_size, y_next)));
		y_prev <- y_next;
	}
	return(y_next);
}

runge_kutta_4th_order <- function(dy_dx = function(x, y){}, initial_val, step_size, start, end){
	num_steps <- (end - start) / step_size;
	half_step_size = step_size / 2;
	y <- initial_val;
	t <- start;
	for(i in 1:num_steps){
		k1 <- dy_dx(t, y);
		k2 <- dy_dx(t + half_step_size, y + (half_step_size * k1));
		k3 <- dy_dx(t + half_step_size, y + (half_step_size * k2));
		k4 <- dy_dx(t + step_size, y + (step_size * k3));
		y <- y + (step_size / 6) * (k1 + (2 * (k2 + k3)) + k4);
		t <- t + (i * step_size);
	}
	return(y);
}

num_grid_sizes <- 10;
grid_sizes_inverted <- c(2,4,8,16,32,64,128,256,512,1024);

# Takes a dy/dx function and an initial value, then uses euler's method to solve
# it with the various grid sizes defined above.
# TODO: write out En part
run_euler_with_grids <- function(dy_dx = function(x, y){}, initial_val, output_file){
	results <- c("n=h^-1", "yn(t=1)");
	write(results, output_file, ncolumns=2, append=FALSE, sep="\t");
	for(i in 1:num_grid_sizes){
		ys <- euler_forward(dy_dx, initial_val, 1 / grid_sizes_inverted[i], 0, 1);
		results <- c(grid_sizes_inverted[i], ys);
		write(results, output_file, ncolumns=2, append=TRUE, sep="\t");
	}
}

# Takes a dy/dx function and an initial value, then uses trapezoid predictor 
# corrector to solve it with the various grid sizes defined above.
# TODO: write out En part
run_trapezoid_with_grids <- function(dy_dx = function(x, y){}, initial_val, output_file){
	results <- c("n=h^-1", "yn(t=1)");
	write(results, output_file, ncolumns=2, append=FALSE, sep="\t");
	for(i in 1:num_grid_sizes){
		ys <- trapezoid_predictor_corrector(dy_dx, initial_val, 1 / grid_sizes_inverted[i], 0, 1);
		results <- c(grid_sizes_inverted[i], ys);
		write(results, output_file, ncolumns=2, append=TRUE, sep="\t");
	}
}

# Takes a dy/dx function and an initial value, then uses 2nd and 4th order 
# Runge-Kutta to solve it with the various grid sizes defined above.
# TODO: write out En part
# TODO: second order Runge-Kutta
run_runge_kutta_with_grids <- function(dy_dx = function(x, y){}, initial_val, output_file){
	results <- c("n=h^-1", "yn(t=1)");
	write(results, output_file, ncolumns=2, append=FALSE, sep="\t");
	for(i in 1:num_grid_sizes){
		ys <- runge_kutta_4th_order(dy_dx, initial_val, 1 / grid_sizes_inverted[i], 0, 1);
		results <- c(grid_sizes_inverted[i], ys);
		write(results, output_file, ncolumns=2, append=TRUE, sep="\t");
	}
}

do_work <- function(){
	run_euler_with_grids(func_i, 0.5, "forward_i.txt");
	run_euler_with_grids(func_ii, 1, "forward_ii.txt");
	run_euler_with_grids(func_iii, 1, "forward_iii.txt");
	run_trapezoid_with_grids(func_i, 0.5, "implicit.txt");
	run_runge_kutta_with_grids(func_i, 0.5, "rungekutta.txt");
}
