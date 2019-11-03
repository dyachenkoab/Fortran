module io_proc
   use Environment

   implicit none
   integer, parameter   :: TECH_AMOUNT = 8, SURNAME_LEN = 15, PROF_LEN = 15
   character(:), allocatable  :: input_file, output_file, data_file, format
   type worker
      character(SURNAME_LEN, kind=CH_) :: Surnames
      character(PROF_LEN, kind=CH_) :: prof
   end type worker
   
   
contains
      subroutine crt_file(input_file, data_file)
         character(*), intent(In)   :: input_file, data_file
         integer                    :: In, Out, i
         type(worker)               :: work

         open (file=input_file, encoding=E_, newunit=In)
         open (file=data_file, form='unformatted', newunit=Out, access= 'stream')
         do i = 1, TECH_AMOUNT
            format = '(a, 1x, a)'
            read (In, format) work
            write(Out) work
         end do
         close(In)
         close(Out)
      end subroutine crt_file

      function read_list(data_file) result(Group)
         type(worker)               :: group(TECH_AMOUNT)
         character(*), intent(In)   :: data_file
         integer                    :: In = 0

         open (file=data_file, form='unformatted', newunit=In, access='stream')
         read(In) group
         close(In)
       end function read_list

       subroutine out_file(output_file, A, B, k)
          character(*), intent(In)      :: output_file
          integer                       :: A(:), i, k, Out
          character(*)                  :: B(:)

          open (file=output_file, encoding=E_, newunit=Out)
          format = '(a, "- ", i5)'
          write (Out, format) (B(i), A(i), i = 1, k)
       end subroutine out_file

end module io_proc
