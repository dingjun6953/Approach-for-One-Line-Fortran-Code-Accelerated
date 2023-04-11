      module cmgl_mod
!dir$ attributes align: 64 :: adx
         real(8), allocatable::  adx(:)   ! adx(mdend)
!dir$ attributes align: 64 :: vq
         real(8), allocatable::  vq(:,:)  ! vq(mdend_vq,mditt)
         !dir$ attributes align: 64 :: A
         real(8), allocatable::  A(:,:)  
         !dir$ attributes align: 64 :: B
         real(8), allocatable::  B(:,:)  
      end module cmgl_mod

      program cmgl

      use cmgl_mod

      implicit none

      interface
         subroutine  t1
     +             (adx, vq, tempc3a, kscast, kscafn, ica, mdend,
     +              mdend_vq, ittst, ittbl, iblin, iblout,
     +              mditt, mdittal, nclass )
          
!            real(8), contiguous, dimension(:)   :: adx     ! adx(iend)
!            real(8), contiguous, dimension(:,:) :: vq      ! vq(mdend,itt)
!            real(8), contiguous, dimension(:,:) :: tempc3a
!                                                 ! tempc3a(mdittal,nclass)
      real(8) adx(mdend)
      real(8) vq(mdend_vq,mditt)
      real(8) tempc3a(mdittal,nclass)
            integer mdend
            integer mdend_vq
            integer mditt
            integer mdittal
            integer nclass
            integer kscast(nclass)
            integer kscafn(0:nclass)
            integer ica
            integer ittst
            integer ittbl
            integer iblin
            integer iblout
         end subroutine t1
     
            
     
      subroutine t3(A,B,iMAX1,iMAX2,blocksize1,blocksize2,ittbl,mditt) 
      !real(8) A(iMAX,iMAX)
      !real(8) B(iMAX,iMAX)
      
      real(8), contiguous, dimension(:,:) :: A    
      real(8), contiguous, dimension(:,:) :: B  
            integer iMAX1 
            integer iMAX2 
            integer mditt
            integer ittbl
            integer blocksize1
            integer blocksize2
            
         end subroutine t3     
                 
      end interface

!      integer, parameter :: istart=1, iend =1957, itt=33
!      integer, parameter :: istart=1, iend =4100, itt=33
!      integer, parameter :: istart=1, iend =41001, itt=133
      integer istart, iend, ittst, itt, ittbl, nthrds, nclass, ica
      integer iloop, ivlenb, ivlendw, ivlpcl, kount, kount_max
      integer mdend, mdend_vq, mditt, mxloops_tot
      integer iMAX1,iMAX2, ierr, mdittal, iblin, iblout
      real(8)  tempc3,sum_A, solvetime
!     real(8)  adx(mdend)
!     real(8)  vq(mdend_vq,mditt)
      integer i,j
      integer(8) :: count0, count1, count2, count_rate, count_max
      integer, allocatable :: kscast(:)      ! (nclass)
      integer, allocatable :: kscafn(:)      ! (nclass)
      real(8), allocatable :: tempc3a(:,:)   ! (mdittal,nclass)

!     call system_clock(count0, count_rate, count_max)

      istart = 1
      iend = 41001
      itt = 133
!      kount_max = 2000000

      write(*,*)
     +' Input nvares, min ortho, max ortho, max ortho for bl, loops,'
      write(*,*)
     +' threads, array align (bytes),'
      write(*,*)
     +' inner and outer loop blocking factors'
      read(5,*) iMAX1,iMAX2, iend, ittst, mditt, ittbl, kount_max, 
     + nthrds,ivlenb, iblin, iblout
      write(*,*) iMAX1,iMAX2,iend, ittst, mditt, ittbl, kount_max, 
     + nthrds,ivlenb,iblin, iblout

      !kount_max = 1000
      !nthrds = 4
      nclass = nthrds
#if defined (OPENMP_VER)
      call omp_set_num_threads ( nthrds )
#endif


      mdend = iend

      ivlendw = ivlenb / 8

      if (ivlendw.gt.0) then
         if (mod(iend,ivlendw).gt.0) then
            mdend_vq = iend + ivlendw - mod(iend,ivlendw)
            mdend_vq = iend
         end if
      else
         mdend_vq = iend
      end if


      allocate ( adx(mdend), stat = ierr )
      allocate ( vq(mdend_vq,mditt), stat = ierr )
      allocate ( A(iMAX1,iMAX2), stat = ierr )
      allocate ( B(iMAX1,iMAX2), stat = ierr )

      print *,' loc(adx(1)) ',loc(adx(1)),mod(loc(adx(1)),64)
      print *,' loc(vq(1,1)) ',loc(vq(1,1)),mod(loc(vq(1,1)),64)
      print *,' loc(vq(1,2)) ',loc(vq(1,2)),mod(loc(vq(1,2)),64)

      allocate ( kscast(nclass), stat = ierr )
      allocate ( kscafn(0:nclass), stat = ierr )

!     allocate ( adx(mdend), stat = ierr )
!     allocate ( vq(mdend_vq,mditt), stat = ierr )

c* calculate the number of elements per class and round up so fits in
c* even number of vector lengths
c* include remainder, rounded up to even number of vector lengths to be
c* included in first loop
c* if nclass is large enough then nclass may be empty

      ivlpcl = iend/nclass
      if (ivlendw.gt.0) then
         if (mod(ivlpcl,ivlendw).gt.0) then
            ivlpcl = ivlpcl + ivlendw - mod(iend/nclass,ivlendw)
         end if
      end if
      kscafn(0) = 0
      kscast(1) = 1
      kscafn(1) = ivlpcl + mod(iend,nclass)
      if (ivlendw.gt.0) then
         if (mod(mod(iend,nclass),ivlendw).gt.0) then
            kscafn(1) = kscafn(1) + ivlendw -
     +                  mod(mod(iend,nclass),ivlendw)
         end if
      end if

      mdittal = mditt
      if (mod(mditt,ivlendw).gt.0) then
         mdittal = mdittal - mod(mditt,ivlendw) + ivlendw
      end if
      allocate ( tempc3a(mdittal,nclass), stat = ierr )

      do ica = 2, nclass
         kscast(ica) = kscafn(ica-1) + 1
         kscafn(ica) = kscafn(ica-1) + ivlpcl
      end do
      kscafn(nclass) = iend

cc    adx = 1.0d+00
cc    vq = 2.0d+00
      solvetime = 0.0d+00

c* initialize adx and vq - these loops will need to have the inner loop
c* in parallel to match first touch in the simulator

         print *,'begin to intialize matrix A and B. ' 
          do i=1,iMAX1
               do j =1,iMAX2
                  A(i,j) =1.0 
                  B(i,j) =2.0
               end do
            end do


          print *,'To end of intializing matrix A and B. ' 


      do itt = 1, mditt
c$omp parallel
c$omp& private(ica)
c$omp    do schedule(runtime)
         do ica = 1, nclass
             if (itt.eq.mditt) then
                adx(kscast(ica):kscafn(ica)) = 1.0d+00
             end if
             vq(kscast(ica):kscafn(ica),itt) = 2.0d+00
         end do
c$omp    end parallel
      end do

         print *,'call t3...'
      call system_clock(count1, count_rate, count_max)
      do kount = 1, kount_max
         tempc3a = 0.0d+00
c$omp parallel
c$omp& private(ica,itt,iloop,tempc3)
c$omp    do schedule(runtime)
         do ica = 1, nclass
            call t3(A,B,iMAX1,iMAX2,iblout,iblin, ittbl, mditt) 
         enddo
c$omp    end parallel
      enddo

      call system_clock(count2, count_rate, count_max)
      solvetime = dble(count2-count1)/dble(count_rate)

      tempc3 = 0.0d+00
      do itt = ittst, mditt
         do ica = 1, nclass
            tempc3 = tempc3 + tempc3a(itt,ica)
         end do
      end do

!          do i=1,kount_max
!               do j =1,kount_max
!                  sum_A=sum_A+A(i,j)  
!               end do
!            end do



!      print *,' sum_A = ', sum_A 
      print *,' tempc3 = ', tempc3
      print *,'    diff from expected answer =',
     +          tempc3-int(mditt-ittst+1,8)*iend*2
      print *,' time ', solvetime
!     print *,' time ', real(count2-count1) / real(count_rate)
!     print *,' solvetime ', real(count2-count1) / real(count_rate)
!     print *,' totaltime ', real(count2-count0) / real(count_rate)

      stop
      end

      subroutine t1(adx, vq, tempc3a, kscast, kscafn, ica, mdend,
     +    mdend_vq, ittst, ittbl, iblin, iblout,
     +    mditt, mdittal, nclass )

      implicit none

!      real(8), contiguous, dimension(:)   :: adx   ! adx(mdend)
!      real(8), contiguous, dimension(:,:) :: vq    ! vq(mdend_vq,itt)
!      real(8), contiguous, dimension(:,:) :: tempc3a ! tempc3a(mdittal,nclass)
      real(8) adx(mdend)
      real(8) vq(mdend_vq,mditt)
      real(8) tempc3a(mdittal,nclass)

      integer mdend, mdend_vq, mditt, mdittal, nclass
      real(8) tempc2

      integer kscast(nclass), kscafn(0:nclass)
      integer ica, ittst, ittbl, iblin, iblout
      integer iloop, itt, ks, ii, jj
!bp
      integer k1, k2, k3
      k1 = kscast(ica)
      k2 = kscafn(ica-1)
      k3 = kscafn(ica)

cc      VECTOR
!!         !DIR$ IVDEP
!!         !DIR$ VECTOR ALWAYS
!!         !DIR$ vector align
!bp                     do 405 ks = ksts(icatis(ica)+1)+1, kstscf(ica)
         if (mditt.le.ittbl) then
!!         !DIR$ BLOCK_LOOP factor(iblout) level(1)
!!         !DIR$ BLOCK_LOOP factor(iblin)  level(2)
            do jj=1,mditt/iblout+1
!bp               do ii=1,(kscafn(ica)-kscafn(ica-1))/iblin+1
               do ii=1,(k3-k2)/iblin+1
                  do itt=(jj-1)*iblout+ittst,min(jj*iblout,mditt)
                     tempc2 = 0.0
!DIR$ VECTOR ALIGNED
!bp                     do ks = (ii-1)*iblin+kscast(ica), 
!bp     +                        min(ii*iblin+kscafn(ica-1),kscafn(ica))
                     do ks = (ii-1)*iblin+k1, 
     +                        min(ii*iblin+k2,k3)
                        tempc2 = tempc2 + adx(ks)*vq(ks,itt)
                     end do
                     tempc3a(itt,ica) = tempc3a(itt,ica) + tempc2
                  end do
               end do
            end do
         else
            do itt=ittst,mditt
               tempc2 = 0.0
!!         !DIR$ VECTOR ALIGNED
               do ks = kscast(ica), kscafn(ica)
                  tempc2 = tempc2 + adx(ks)*vq(ks,itt)
               end do
               tempc3a(itt,ica) = tempc3a(itt,ica) + tempc2
            end do
         end if

      return

      end


           
      subroutine t3(A,B,iMAX1,iMAX2,blocksize1,blocksize2,ittbl,mditt)

      
       implicit none

      real(8), contiguous, dimension(:,:) :: A    
      real(8), contiguous, dimension(:,:) :: B  
           
      integer iMAX1, blocksize1,mditt, ittbl
      integer iMAX2, blocksize2
      integer i,j, ii, jj
      integer iquotient1,iquotient2
      integer iremainder1,iremainder2
      
       iquotient1=iMAX1/blocksize1
       iquotient2=iMAX2/blocksize2
       iremainder1=mod(iMAX1,blocksize1)
       iremainder2=mod(iMAX2,blocksize2)

         if (mditt.le.ittbl) then
         !print *, 'enter blockloop branch....'
            do i=1,iquotient1*blocksize1,blocksize1
                do j=1,iquotient2*blocksize2,blocksize2
                    do ii=i,i+blocksize1-1
                            do jj = j,j+blocksize2-1
                                A(ii,jj)=A(ii,jj)+B(ii,jj)
                            end do
                    end do
                end do
            end do

        if(iremainder1 > 0 .and. iremainder2>0) then 
                do i =1, iMAX1 
                        do j = iquotient2*blocksize2+1,iMAX2 
                            A(i,j) = A(i,j) + B(i,j)
                        enddo 
                   enddo
            do i =iquotient1*blocksize1+1, iMAX1 
                do j =1, iquotient2*blocksize2 
                    A(i,j) = A(i,j) + B(i,j)
                enddo 
           enddo 
        endif

         if(iremainder1 > 0 .and. iremainder2==0) then 
            do i =iquotient1*blocksize1+1, iMAX1 
                do j =1, iquotient2*blocksize2 
                    A(i,j) = A(i,j) + B(i,j)
                enddo 
           enddo 
        endif

       if(iremainder1 == 0 .and. iremainder2>0) then 
            do i =1, iMAX1 
                do j = iquotient2*blocksize2+1,iMAX2 
                    A(i,j) = A(i,j) + B(i,j)
                enddo 
           enddo 
       endif

         else
         !print *, 'enter noblockloop branch....'
            do i=1,iMAX1
               do j =1,iMAX2
                  A(i,j) = A(i,j) + B(i,j)
               end do
            end do
         end if

      return

      end
