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
    
    REAL(4), allocatable :: a(:)
    REAL(4), allocatable :: b(:)
    REAL(4), allocatable :: c(:)
    REAL(4), allocatable :: e(:,:)
    REAL(4), allocatable :: d(:,:)
    REAL(4), allocatable :: f(:,:)
    REAL(4), allocatable :: g(:,:,:)
    REAL(4), allocatable :: h(:,:,:)
    REAL(4), allocatable :: x(:,:,:)
  
    call system_clock(c1,cr,cm)
    m=etime(t)
  
     n=50000
     itmax=10000
     ii=400
     jj=400
     kk=400
     
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
  
 !  !!$OMP DO schedule(runtime) private(i)
 !  do i=1,n
 !       a(i)=cos(i*0.2)*sqrt(2.4)
 !       b(i)=sin(i*0.3)*sqrt(1.5)
 !       c(i)=0.0
 !  enddo 
 !  !!$OMP END DO

!$omp parallel private(id)
  id = omp_get_thread_num ( )
  write ( *, '(a,i8,a,i8)' ) '  HELLO from process ', id

   !$OMP DO schedule(runtime) private(it)
    do it=1, itmax


 !  !$OMP DO schedule(runtime) private(it)
 !   do it=1, itmax
 !   !!DIR$ SIMD
 !   !!DIR$ novector
 !       do i=1,n
 !           c(i)=a(i)*b(i)+it*3.1415927
 !           b(i)=sqrt(1.0*sin(b(i))*sin(b(i)))*1.21
 !       enddo
 !   enddo
 !  !$OMP END DO

 !  !$OMP DO schedule(runtime) 
    do i=1,ii
      do j=1,jj
        !!DIR$ SIMD
        !!$OMP SIMD
        !!DIR$ VECTOR ALWAYS
        !!DIR$ VECTOR ALIGNED
        !!DIR$ IVDEP
        do k=1,kk
          g(i,j,k)=1.0*i*j*k
        enddo
      enddo
    enddo
  ! !$OMP END DO

  ! !$OMP DO schedule(runtime) 
    do k=1,kk
      do j=1,jj
        !!DIR$ SIMD
        !!$OMP SIMD
        !!DIR$ VECTOR ALWAYS
        !!DIR$ VECTOR ALIGNED
        !!DIR$ IVDEP
        do i=1,ii
          h(i,j,k)=1.0*i*i*j*j*k*k
        enddo
      enddo
    enddo
  ! !$OMP END DO
   
!!$OMP DO schedule(runtime) 
    do k=1,kk
      do j=1,jj
        !!DIR$ SIMD
        !!$OMP SIMD
        !!DIR$ VECTOR ALWAYS
        !!DIR$ VECTOR ALIGNED
        !!DIR$ IVDEP
        do i=1,ii
          x(i,j,k)=cos(i*0.1)*cos(j*0.1)*sin(j*0.1)*sin(k*0.1)*k
        enddo
      enddo
    enddo
!   !$OMP END DO
 enddo        
    !write ( *, '(a)' ) 'matrix multiplication completed 1'
!   call random_seed()
!!$OMP DO private(j,k)   
!    do j=1,jj
!        do k=1,kk
!            call random_number(rand_num)
!            d(k,j)=rand_num+9384273.0465*46389229.90876
!            call random_number(rand_num)
!            e(k,j)=rand_num+9874567.06475*9487501.9843
!
!        end do
!    end do
!!$OMP END DO  

!$OMP END PARALLEL    
   
    write ( *, '(a)' ) 'matrix multiplication completed '

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
