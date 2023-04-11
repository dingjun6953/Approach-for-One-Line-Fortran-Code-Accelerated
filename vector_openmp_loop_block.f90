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
    integer iMAX
    integer BS,iquotient,iremainder
    integer BS_sum
    integer NOBS_sum
    
    REAL(4), allocatable :: c(:)
    REAL(4), allocatable :: e(:,:)
    REAL(4), allocatable :: d(:,:)
    REAL(4), allocatable :: f(:,:)
    REAL(4), allocatable :: g(:,:,:)
    REAL(4), allocatable :: h(:,:,:)
    REAL(4), allocatable :: x(:,:,:)
    REAL(8),  allocatable :: A(:,:)
    REAL(8),  allocatable :: B(:,:)
    
    call system_clock(c1,cr,cm)
    m=etime(t)
  
     n=5000
     itmax=10
     ii=40
     jj=40
     kk=40
     BS_sum=0
     NOBS_sum=0
     iMAX=1026
     BS=256
     
     allocate(c(n))
     allocate(e(kk,jj),d(kk,jj),f(kk,jj))
     allocate(g(ii,jj,kk))
     allocate(h(ii,jj,kk))
     allocate(x(ii,jj,kk))
     allocate(A(iMAX,iMAX))
     allocate(B(iMAX,iMAX))

  iquotient=iMAX/BS
  iremainder=mod(iMAX,BS)
  write ( *, '(a,i8)' )' iquotient= ',iquotient 
  write ( *, '(a,i8)' )' remainder= ',iremainder 
  if(iremainder > 0) then 
       iMAX=iquotient*BS
  endif 


     do i=1,iMAX
         do j =1,iMAX
            A(i,j) =1.0 
            B(i,j) =2.0
         end do
    end do


  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VECTOR_SIMD_OPENMP_TEST'
  
  wtime = omp_get_wtime ( )
 


  write ( *, '(a,i8)' ) &
    '  The number of processors available = ', omp_get_num_procs ( )
  write ( *, '(a,i8)' ) &
    '  The number of threads available    = ', omp_get_max_threads ( )
  id = omp_get_thread_num ( )
  call random_seed()
  

!!$omp parallel private(id)
  id = omp_get_thread_num ( )
  write ( *, '(a,i8,a,i8)' ) '  HELLO from process ', id

!   !$OMP DO schedule(runtime) private(it)
    do it=1, itmax

    DO I =1, iMAX, BS
        DO J = 1, iMAX, BS
            DO II = I, I-1+BS
                DO JJ = J, J-1+BS
                    A(II,JJ) = A(II,JJ) + B(II,JJ)
                    BS_sum=BS_sum+1
                ENDDO   
            ENDDO
        ENDDO 
    ENDDO
       
   DO I =1, iMAX
        DO J = 1, iMAX
            A(I,J) = A(I,J) + B(I,J)
            NOBS_sum=NOBS_sum+1
        ENDDO 
    ENDDO 

enddo

       
!!$OMP END PARALLEL    
   
    write ( *, '(a)' ) 'matrix multiplication completed '
 write ( *, '(a,g14.1)' ) '  BS_sum = ', BS_sum
  write ( *, '(a,g14.1)' ) '  NOBS_sum= ', NOBS_sum
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
