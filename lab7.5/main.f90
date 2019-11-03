program a7_47
      use Environment
      implicit none

      character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
      integer                 :: In = 0, Out = 0, N = 0
      integer, allocatable    :: A(:,:), B(:,:), tmp(:)
      character(:), allocatable :: fmt

      open (file=input_file, newunit=In)
         read (In, *) N
         allocate (A(N, N))
         read (in, *) (A) 
      close (In)

      allocate (B(N,N))
      
      call CMat(A, B, tmp)
      
      open (file=output_file, encoding=E_, newunit=Out)
         fmt = '('//N//'i3)'
         write (Out, fmt) A
         write (Out, '(/'//N//'i3/)' ) tmp
         write (Out, fmt) B
      close (Out)

contains
   pure subroutine CMat(A, B, tmp)
      integer, allocatable, intent(InOut)    :: A(:,:), B(:,:), tmp(:)
      integer                                :: j
   
      tmp = MaxVal(A, 1)
      do concurrent (j = 1:N)
         B(1:N,j) = A(1:N,j)*tmp(j)
      end do
    
   end subroutine CMat
end program a7_47
