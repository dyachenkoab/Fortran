program exercise_6
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(8)                 :: reallog = 0, mainlog = 0, eps = 0, x 
   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   reallog = log(1+x)

   eps = epsilon(0._R_)
   
   mainlog = logarifm(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(2(a, T8, " =", e13.6/))') "reallog", reallog, "mainlog", mainlog, "Error", reallog - mainlog
   close(Out)

contains 
   real(8) pure function logarifm(x) result(sidelog)
      real(8), intent(In)  :: x
      real(8)              :: r
      integer              :: n
  
   n = 1
   r = x          
   sidelog = r
         
   do
      n = n + 1              
      r = -r * x  
      sidelog = sidelog + r/n    
      if (r/sidelog <= eps) exit
   end do
  ! print *, n

end function logarifm

end program exercise_6
