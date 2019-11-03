module work_proc
   use Environment
   use io_proc
   implicit none

contains
  pure recursive subroutine sort_prof(group, A, B, k, i, Mask, Match)
      integer, intent(Out)                    :: A(:), k
      integer, intent(In)                     :: i
      integer                                 :: N
      logical, intent(InOut)                  :: Match(:), Mask(:)
      character(*), intent(Out)               :: B(:)
      type(worker), intent(InOut)             :: group(:)
  
   N = tech_amount 
      if(.not. Match(i)) then
         Mask(i:N) = group(i:N)%prof==group(i)%prof
         Match(i:N) = Match(i:N).or.Mask(i:N)
         k = k + 1
         A(k) = Count(Mask(i:N))
         B(k) = group(i)%prof
      end if
      if (i < N) call sort_prof(group, A, B, k, i+1, Mask, Match)
     end subroutine sort_prof

end module work_proc
