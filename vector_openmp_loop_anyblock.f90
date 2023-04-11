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
    integer iMAX1,iMAX2
    integer BS1,iquotient1,iremainder1
    integer BS2,iquotient2,iremainder2
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
     iMAX1=1024
     iMAX2=1024
     
     BS1=256
     BS2=128
     
     allocate(c(n))
     allocate(e(kk,jj),d(kk,jj),f(kk,jj))
     allocate(g(ii,jj,kk))
     allocate(h(ii,jj,kk))
     allocate(x(ii,jj,kk))
     allocate(A(iMAX1,iMAX2))
     allocate(B(iMAX1,iMAX2))

  iquotient1=iMAX1/BS1
  iremainder1=mod(iMAX1,BS1)
  write ( *, '(a,i8)' )' iquotient1= ',iquotient1 
  write ( *, '(a,i8)' )' remainder1= ',iremainder1 

  iquotient2=iMAX2/BS2
  iremainder2=mod(iMAX2,BS2)
  write ( *, '(a,i8)' )' iquotient2= ',iquotient2 
  write ( *, '(a,i8)' )' remainder2= ',iremainder2 


  !if(iremainder > 0) then 
  !     iMAX=iquotient*BS
  !endif 


     do i=1,iMAX1
         do j =1,iMAX2
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

    DO I =1, iquotient1*BS1, BS1
        DO J = 1, iquotient2*BS2, BS2
            DO II = I, I-1+BS1
                DO JJ = J, J-1+BS2
                    A(II,JJ) = A(II,JJ) + B(II,JJ)
                    BS_sum=BS_sum+1
                ENDDO   
            ENDDO
        ENDDO 
    ENDDO

    if(iremainder1 > 0 .and. iremainder2>0) then 
    DO I =1, iMAX1 
        DO J = iquotient2*BS2+1,iMAX2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    DO I =iquotient1*BS1+1, iMAX1 
        DO J =1, iquotient2*BS2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif

    if(iremainder1 > 0 .and. iremainder2==0) then 
    DO I =iquotient1*BS1+1, iMAX1 
        DO J =1, iquotient2*BS2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif

    if(iremainder1 == 0 .and. iremainder2>0) then 
    DO I =1, iMAX1 
        DO J = iquotient2*BS2+1,iMAX2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif


   DO I =1, iMAX1
        DO J = 1, iMAX2
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
