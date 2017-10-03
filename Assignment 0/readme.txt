My turned implementation rotates a square matrix 90 degrees anti-clockwise without using any temp variables or extra storage.

It does this by using bitwise XOR to swap two values without using a temp variable.
See the "Swapping without "temp"" section on this page for details.
http://www.cs.umd.edu/class/sum2003/cmsc311/Notes/BitOp/xor.html

I have written my own transpose function that does not use any extra space.
This is because I do not know if R's implementation uses any temp variables.

The algorithm for rotating the matrix 90 degrees anti-clockwise is simple:
1) Reverse the order of the elements in each row
2) Transpose the matrix
