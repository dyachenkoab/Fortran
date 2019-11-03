program reference_lab_2
   use Environment

   implicit none
   Integer, parameter            :: TECH_AMOUNT = 7, SURNAME_LEN = 15, PROF_LEN = 15

   character(:), allocatable     :: input_file, output_file, format
   character(kind=CH_)           :: surnames(TECH_AMOUNT, SURNAME_LEN)
   character(kind=CH_)           :: prof(TECH_AMOUNT, PROF_LEN)
   character(kind=CH_)           :: B(TECH_AMOUNT, PROF_LEN)
   integer                       :: In = 0, Out = 0, i, IO, k, N, j
   integer                                :: A(tech_amount)
   logical, allocatable                   :: Mask(:)
   logical, allocatable                   :: Match(:)

   input_file = "../data/class.txt"
   output_file = "output.txt"

call ReadM(input_file, surnames, prof)
call work(A, B, prof, k)
call output(output_file, prof, A, B)

contains
 subroutine work(A, B, prof, k)
      integer                 :: i, N, k, A(:)
      character(*, kind=CH_)  :: prof(:,:), B(:,:)
      logical, allocatable    :: Mask(:), Match(:)
      intent(Out)             :: A, B, k
      intent(In)              :: prof
      
allocate(Match(TECH_AMOUNT), source=.false.)
allocate(Mask(TECH_AMOUNT))
  
   N = tech_amount 
   k = 0
   do i = 1, N
      if(.not. Match(i)) then
         Mask(i) = .true.
         do concurrent (j = i+1:N)
         Mask(j) = All(prof(j,:)==prof(i,:))
         end do
         Match(i:) = Match(i:).or.Mask(i:)
         k = k + 1
         A(k) = Count(Mask(i:))
         B(k,:) = prof(i,:)
      end if
   end do
end subroutine work
   
subroutine ReadM(input_file, surnames, prof)
  character(*)                 ::   input_file
  character(kind=CH_)          ::   surnames(:,:), prof(:,:)
  integer                      ::   i = 0, In = 0
  character(:), allocatable    ::   format
  intent(In)                   ::   input_file
  intent(inOut)                ::   surnames, prof
  open (file=input_file, encoding=E_, newunit=In)
     format = '('//SURNAME_LEN//'a, 1x, '//PROF_LEN//'a)'
     read (In, format) (surnames(i,:), prof(i,:), i = 1, TECH_AMOUNT)
  close (In)
end subroutine ReadM

subroutine output(output_file, prof, A, B)
     character(*)                  :: output_file
     character(*, kind=CH_)        :: prof(:,:)
     integer                       :: i, io, Out, A(:)
     character(:), allocatable     :: format
     character(*, kind=CH_)        :: B(:,:)
     open (file=output_file, encoding=E_, newunit=Out)
     format = '('//PROF_LEN//'a, "- ", T17, i5)'
     write (Out, format) (B(i,:), A(i), i  = 1, k)
   close (Out)
end subroutine
 
end program reference_lab_2
