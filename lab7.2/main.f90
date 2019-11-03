program exercise_7_9b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, maxa = 0
   real(R_), allocatable   :: Z(:, :), Sums(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)

   allocate(Sums(N)) 
   call Matsum(Z, maxa, Sums)
   !print *, maxa
   !print *, Sums
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (Z(i, :), i = 1, N)
      write (Out, '(f6.2\)') Sums
      write (Out, '(/T4, "Max = ", i0)') maxa
   close (Out)

contains
      subroutine Matsum(Z, maxa, Sums)
      real(R_)       Z(:, :), Sums(:)
      intent(in)     Z
      intent(inout)  Sums
      integer        j, maxa
      
     do concurrent (j = 1:N)
         Sums = Sums + abs(Z(j, :)) 
     end do

      maxa = MaxVal(Sums, 1)

   end subroutine Matsum
end program exercise_7_9b
