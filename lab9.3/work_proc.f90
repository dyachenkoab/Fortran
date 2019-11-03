module work_proc
   use Environment
   use io_proc
   implicit none

contains
  pure subroutine sort_prof(group, A, B, k)
      integer, intent(Out)                  :: A(:), k
      integer                               :: N, i
      logical, allocatable                  :: Match(:), Mask(:)
      character(*), intent(Out)             :: B(:)
      type(worker), intent(InOut)           :: group(:)
   allocate(Match(TECH_AMOUNT), source=.false.)
   allocate(Mask(TECH_AMOUNT))
  
   N = tech_amount 
   k = 0
   do i = 1, N
      if(.not. Match(i)) then
         Mask(i:N) = group(i:N)%prof==group(i)%prof
         Match(i:N) = Match(i:N).or.Mask(i:N)
         k = k + 1
         A(k) = Count(Mask(i:N))
         B(k) = group(i)%prof
      end if
   end do
   end subroutine sort_prof
end module work_proc
