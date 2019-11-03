     program Sort
      use Environment

      implicit none
      character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
      integer                :: In = 0, Out = 0, X = 0
      integer, allocatable   :: A(:)
      
      open (file=input_file, newunit=In)
         read (In, *) X
         allocate (A(X))
         read (In, *) A
      close (In)
print *, A
     call sortmass(A)
     
     open(file=output_file, encoding=E_, newunit=Out)
         write (Out, "("//X//"i3)") A
     close (Out)
     

contains
      subroutine sortmass(A)
         integer, intent(inout) :: A(:)
         logical, allocatable   :: N(:)
         integer                :: tmp, i, MinI, Z
          
     N = A<=0
     Z = Count(N)
     A = [Pack(A, N), Pack(A, .not.N)]
     
     do concurrent (i = 1:Z-1)
      MinI = MinLoc(A(i:Z), 1) + i-1
        if (i /= MinI) then
            tmp = A(i) 
            A(i) = A(MinI)
            A(MinI) = tmp 
         end if
     end do

     end subroutine sortmass
     end program Sort
