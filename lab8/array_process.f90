module Array_process
   use Environment
   implicit none

contains
  pure subroutine SortM(A, N, M)
      integer, intent(InOut) :: A(:,:)
      integer, intent(In)    :: N, M
      integer                :: i, j, k, tmp

      do k = 1, N
         do i = 1, M
            do j = 1, N-K
               if (A(i,j) < A(i,j+1)) then
               tmp = A(i,j)
               A(i,j) = A(i,j+1)
               A(i,j+1) = tmp
               end if
            end do
         end do
      end do
   end subroutine SortM
end module Array_process
