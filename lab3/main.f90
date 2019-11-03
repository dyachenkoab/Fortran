program sum_el
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = 'output.txt'
   integer                 :: In = 0, Out = 0, E = 0
   real(R_), allocatable   :: A(:)
   real(R_)                :: summa = 0

   open (file = input_file, newunit = In)
      read (In, *) E
      allocate (A(E))
      read (In, *) A
   close (In)
   
   summa = sum(A(1::2))

   open (file = output_file, encoding = E_, newunit = Out)
     write (Out, '(T3, i0)') E
     write (Out, '(T4, f6.2)') A
     write (Out, *)
     write (Out, '("summa = ", f0.2)') summa
   close (Out)
end program sum_el
