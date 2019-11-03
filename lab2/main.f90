program func
   use Environment

   implicit none
   character(*), parameter :: input_file = '../data/input.txt', output_file = 'output.txt'
   integer                 :: In = 0, Out = 0 !почему нули
   real(R_)                :: y = 0, x = 0

   open (file = input_file, newunit = In) !что за in, out
      read (In, *) x
   close (In)
   
   if (x>0) then
      y = x
   else
      y = -x
   end if

   open (file = output_file, encoding = E_, newunit = Out) 
      write (Out,'(2(a, f0.2/))') 'x = ', x, 'y = ', y !формат. 2-повторяй два раза, бэкслэш-перевод каретки
   close (Out)
end program func
