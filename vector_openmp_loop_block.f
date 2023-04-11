program main

  use omp_lib

  implicit none
  
  integer :: n
    integer :: itmax
    integer :: ierr7
    integer :: i,j,k, it,ii,jj,kk    
    integer :: c1,c2,cr,cm
    integer :: id
    REAL(4):: wtime,dt,temp
    REAL(4) :: rand_num
    REAL m,etime,t(2)
    integer MAX=512
    integer BS=16
    integer BS_sum
    integer NOBS_sum
    
    REAL(4), allocatable :: a(:)
    REAL(4), allocatable :: b(:)
    REAL(4), allocatable :: c(:)
    REAL(4), allocatable :: e(:,:)
    REAL(4), allocatable :: d(:,:)
    REAL(4), allocatable :: f(:,:)
    REAL(4), allocatable :: g(:,:,:)
    REAL(4), allocatable :: h(:,:,:)
    REAL(4), allocatable :: x(:,:,:)
    
    REAL A(MAX,MAX), B(MAX,MAX)
    
    call system_clock(c1,cr,cm)
    m=etime(t)
  
     n=5000
     itmax=10
     ii=40
     jj=40
     kk=40
     BS_sum=0
     NOBS_sum=0
     
     allocate(a(n),b(n),c(n))
     allocate(e(kk,jj),d(kk,jj),f(kk,jj))
     allocate(g(ii,jj,kk))
     allocate(h(ii,jj,kk))
     allocate(x(ii,jj,kk))

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VECTOR_SIMD_OPENMP_TEST'
  
  wtime = omp_get_wtime ( )

  write ( *, '(a,i8)' ) &
    '  The number of processors available = ', omp_get_num_procs ( )
  write ( *, '(a,i8)' ) &
    '  The number of threads available    = ', omp_get_max_threads ( )
  id = omp_get_thread_num ( )
  call random_seed()
  

!$omp parallel private(id)
  id = omp_get_thread_num ( )
  write ( *, '(a,i8,a,i8)' ) '  HELLO from process ', id

   !$OMP DO schedule(runtime) private(it)
    do it=1, itmax

    DO I =1, MAX, BS
        DO J = 1, MAX, BS
            DO II = I, I+MAX, BS-1
                DO JJ = J, J+MAX, BS-1
                    A(II,JJ) = A(II,JJ) + B(JJ,II)
                    BS_sum=BS_sum+1
                ENDDO   
            ENDDO
        ENDDO 
    ENDDO
       
   DO I =1, MAX
        DO J = 1, MAX
            A(I,J) = A(I,J) + B(J,I)
            NOBS_sum=NOBS_sum+1
        ENDDO 
    ENDDO 
       
!$OMP END PARALLEL    
   
    write ( *, '(a)' ) 'matrix multiplication completed '
 write ( *, '(a,g14)' ) '  BS_sum = ', BS_sum
  write ( *, '(a,g14)' ) '  NOBS_sum= ', NOBS_sum
!  Finish up by measuring the elapsed time.
  wtime = omp_get_wtime ( ) - wtime
  call system_clock(c2,cr,cm)
  dt=dble((c2-c1)*1.10)/dble(cr*1.0)
  m=etime(t)

  !write ( *, '(a,g14.6)' ) '  Elapsed wall clock time 1 = ', wtime
  write ( *, '(a,g14.6)' ) '  Elapsed wall clock time 2 = ', dt
  !write ( *, '(a,g14.6)' ) '  Elapsed wall clock time 3 = ', m
  deallocate(a,b,c,d,e,f,g,h,x)
  stop
end
