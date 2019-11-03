program a7_18
      use Environment

      character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
      integer                 :: In = 0, Out = 0, N = 0, i = 0, U = 0, M = 0, D = 0
      logical, allocatable    :: Mask(:)
      integer, allocatable    :: A(:,:), K(:), Indexes(:), IndP(:,:)

      open (file=input_file, newunit=In)
         read (In, *) N
         allocate (A(N, N))
         read (in, *) (A(i, :), i = 1, N)
      close (In)

     
     K=[(A(i,i), i = 1,N), (A(N-i+1, i), i = 1,N)]
     !print *, K
     
     D = MaxVal(K)
     U = Count(K(:N)==D)
     M = Count(K(N+1:)==D)
     
     allocate (Indexes(N))
     
     Indexes = [(i, i = 1, N)]
     
     allocate (IndP(U+M, 2))

     IndP(:U, 1) = Pack(Indexes, K(:N)==D)
     IndP(U+1:U+M, 1) = Pack(Indexes, K(N+1:)==D)
     Indp(:U, 2) = IndP(:, 1)
     IndP(U+M:U+1:-1, 2) = Pack(Indexes, K(N*2:N+1:-1)==D)
   
     !print *, IndP 
     write (*, '(2i3)') (IndP(i, :), i = 1, U+M)
     !print *, (K(N+1:)==D)
     !print *, U
     !print *, M
  
 end program a7_18
