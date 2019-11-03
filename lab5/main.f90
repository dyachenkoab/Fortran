program inter
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, X = 0
   real(R_)                :: summ = 0
   real(R_), allocatable   :: A(:)

   open (file = input_file, newunit = In)
      read (In, *) N
      allocate (A(N))
      read (In, *) A
   close (In)

  call in_fun(A, X, summ)

   open (file = output_file, newunit = Out)
      write (Out, *) summ
   close (Out)

contains
   subroutine in_fun(A, X, summ)
      real        A(:), summ
      integer     X
      intent(in)  A
      intent(out) X, summ
  
   X = FINDLOC(A>0.and.A<1, value=.true., dim=1)
   summ = SUM(A(1:X-1))
   
   end subroutine in_fun   
end program inter 
