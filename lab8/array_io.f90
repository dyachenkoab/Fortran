module Array_IO
   use Environment

   implicit none
contains
   subroutine RedN(input_file, A, N, M)
      character(*), intent(in) :: input_file
      integer                  :: In = 0, M, N, i = 0
      integer, allocatable     :: A(:,:)
      
      open (file=input_file, newunit=In)
         read (In, *) M, N
         allocate(A(M,N))
         read (In, *) A 
      close (In)
   end subroutine RedN


   subroutine OutputArrays(output_file, A, N, M)
      character(*), intent(in)   :: output_file
      integer, intent(in)        :: A(:, :)
      integer                    :: Out = 0, i = 0, M, N

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, *) N
         write (Out, '('//M//'i3)') A 
      close (Out)
   end subroutine OutputArrays
end module Array_IO
