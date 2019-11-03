program reference_lab_1_4
   use Environment
   use io_proc
   use work_proc

   implicit none

   character(PROF_LEN, kind=CH_)          :: B(TECH_AMOUNT) = ""
   integer                                :: A(tech_amount) = 0, k = 0, i = 1
   type(worker)                           :: Group(TECH_AMOUNT)
   logical, allocatable                   :: Match(:), Mask(:)
   
   input_file = "../data/class.txt"
   output_file = "output.txt"
   data_file = "work.dat"
   
   call crt_file(input_file, data_file)
   
   group = read_list(data_file)

   allocate(Match(TECH_AMOUNT), source=.false.)
   allocate(Mask(TECH_AMOUNT))

   call sort_prof(group, A, B, k, i, Mask, Match)
   call out_file(output_file, A, B, k)

end program reference_lab_1_4
