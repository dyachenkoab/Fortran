program reference_lab_1_3
   use Environment
   use io_proc
   use work_proc

   implicit none

   character(PROF_LEN, kind=CH_)          :: B(TECH_AMOUNT)
   integer                                :: A(tech_amount), k
   type(worker)                           :: Group(TECH_AMOUNT)
   
   input_file = "../data/class.txt"
   output_file = "output.txt"
   data_file = "work.dat"
   
   call crt_file(input_file, data_file)
   group = read_list(data_file)
   call sort_prof(group, A, B, k)
   call out_file(output_file, A, B, k)

end program reference_lab_1_3
