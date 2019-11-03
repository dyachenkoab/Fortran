program Tabulation
   use Environment

   implicit none
   character(*), parameter :: input_file = '../data/input.txt', output_file = 'output.txt'
   integer                 :: In = 0, Out = 0, Nx = 0, Ny = 0, i = 0, j = 0
   real(R_)                :: x1 = 0, x2 = 0, hx = 0, y1 = 0, y2 = 0, hy = 0
   real(R_), allocatable   :: X(:), Y(:), F(:), x3(:), y3(:)

   open (file = input_file, newunit = In)
      read (In, *) x1, x2, hx
      read (In, *) y1, y2, hy
   close (In)

   open (file = output_file, newunit = Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') 'x1', x1, 'x2', x2, 'hx', hx
      write (Out, '(3(a, T4, "= ", f0.4/))') 'y1', y1, 'y2', y2, 'hy', hy
   close (Out)

   Nx = NInt((x2-x1) / hx) + 1
   Ny = NInt((y2-y1) / hy) + 1

   allocate (X(Nx), Y(Ny))
      
   call Tabul(x1, y1, hx, hy, X, Y)

    x3 = [SPREAD(X, 1, Ny)]
    y3 = [SPREAD(Y, 2, Nx)]
    print *, x3
    print*, X 
    F = Sin(X3+Y3) / (Cos(X3+Y3)**2 - 1)
   
    write (*, '(T8, f0.4/)') (x3) 
    !write (*, '(T17, f0.4/)') (y3)
    !write (*, '(T22, f0.4/)') (F)

    open (file = output_file, encoding = E_, newunit = Out, position = 'append')
     write (Out, '("  x", T8, "|", T13, "y", T17, "|", T22, "f")')
     write (Out, '(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)') (x3(i), y3(i), F(i), i = 1, Nx*Ny)
   close (Out)

contains

   pure subroutine Tabul(x1, y1, hx, hy, X, Y)
      real(R_)     x1, y1, hx, hy, X(:), Y(:)
      intent(in)   x1, y1, hx, hy
      intent(out)  X, Y
      integer      i, j

      do concurrent (i = 1:Nx)
         X(i) = x1 + hx*(i-1)
      end do

      do concurrent ( j = 1:Ny)
         Y(j) = y1 + hy*(j-1)
      end do

   end subroutine Tabul
end program Tabulation

