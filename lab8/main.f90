program exercise_8_5
   use Environment
   use Array_IO
   use Array_process

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: N, M, tmp
   integer, allocatable    :: A(:, :) 

   call RedN(input_file, A, N, M) 
   
write (*,'('//M//'i3)') A   
   call SortM(A, N, M)  

   call OutputArrays(output_file, A, N, M)
end program exercise_8_5
