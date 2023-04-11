      module cmgl_mod
!dir$ attributes align: 64 :: adx
!dir$ attributes align: 64 :: intel_a
!dir$ attributes align: 64 :: intel_b
!dir$ attributes align: 64 :: intel_c
         real(8), allocatable::  adx(:)   ! adx(mdend)
         real(8), allocatable::  intel_a(:)   
         real(8), allocatable::  intel_b(:)   
         real(8), allocatable::  intel_c(:)   
!dir$ attributes align: 64 :: vq
         real(8), allocatable::  vq(:,:)  ! vq(mdend_vq,mditt)
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
     
                
       subroutine t4(intel_a,intel_b,intel_c, kount_max,  
     +    ittbl, mditt,iblin,iblout)

      real(8), contiguous, dimension(:) :: intel_a    
      real(8), contiguous, dimension(:) :: intel_b
      real(8), contiguous, dimension(:) :: intel_c
                
      integer kount_max
      integer ittbl
      integer mditt
      integer iblin
      integer iblout
      end subroutine t4
      
      
      end interface

!      integer, parameter :: istart=1, iend =1957, itt=33
!      integer, parameter :: istart=1, iend =4100, itt=33
!      integer, parameter :: istart=1, iend =41001, itt=133
      integer istart, iend, ittst, itt, ittbl, nthrds, nclass, ica
      integer iloop, ivlenb, ivlendw, ivlpcl, kount, kount_max
      integer mdend, mdend_vq, mditt, mxloops_tot
      integer iMAX, ierr, mdittal, iblin, iblout
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
      read(5,*) iMAX, iend, ittst, mditt, ittbl, kount_max, nthrds,
     + ivlenb, iblin, iblout
      write(*,*) iMAX,iend, ittst, mditt, ittbl, kount_max, nthrds,
     + ivlenb,iblin, iblout

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
      allocate ( intel_a(kount_max), stat = ierr )
      allocate ( intel_b(kount_max), stat = ierr )
      allocate ( intel_c(kount_max), stat = ierr )
      allocate ( vq(mdend_vq,mditt), stat = ierr )

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

           do  j = 1,kount_max
               intel_a(j)=1.0
               intel_b(j)=2.0
               intel_c(j)=3.0
           end do

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

         print *,'call t4...'
      call system_clock(count1, count_rate, count_max)
      do kount = 1, kount_max
         tempc3a = 0.0d+00
c$omp parallel
c$omp& private(ica,itt,iloop,tempc3)
c$omp    do schedule(runtime)
         do ica = 1, nclass
            call t4(intel_a,intel_b,intel_c, kount_max,  
     +    ittbl, mditt,iblin,iblout)
!            call t1(adx, vq, tempc3a, kscast, kscafn, ica, mdend,
!     +             mdend_vq, ittst, ittbl, iblin, iblout,
!     +             mditt, mdittal, nclass )      
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
            do jj=1,mditt,iblout
!bp               do ii=1,(kscafn(ica)-kscafn(ica-1))/iblin+1
               do ii=1,(k3-k2), iblin
                  do itt=jj-1+ittst,jj-1+ittst+iblout-1
                     tempc2 = 0.0
!!DIR$ VECTOR ALIGNED
!bp                     do ks = (ii-1)*iblin+kscast(ica), 
!bp     +                        min(ii*iblin+kscafn(ica-1),kscafn(ica))
                     do ks = ii-1+k1, ii-1+k1+iblin-1
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


      subroutine t4(intel_a,intel_b,intel_c, kount_max,  
     +    ittbl, mditt,iblin,iblout)

      
       implicit none

      real(8), contiguous, dimension(:) :: intel_a    
      real(8), contiguous, dimension(:) :: intel_b
      real(8), contiguous, dimension(:) :: intel_c
      
           
      integer kount_max, ittbl, mditt,iblin,iblout
      integer i,j, ii, jj
      real(8) f
     
         
         if (mditt.le.ittbl) then
         !print *, 'enter blockloop branch....'
         !print *, 'kount_max=',kount_max
         
         do jj=1,kount_max/256+1
              do ii = 1,kount_max/256+1
                  do j = (jj-1)*256+1, min(jj*256, kount_max)
                      f = 0
                      do i = (ii-1)*256+1, min(ii*256,kount_max)
                          f = f + intel_a(i) * intel_b(i)
                      enddo
                  intel_c(j) = intel_c(j) + f
              enddo
          enddo
      enddo

         else
         !print *, 'enter noblockloop branch....'
         !print *, 'kount_max=',kount_max
         !dir$ block_loop factor(256) level(1:2)
            do  j = 1,kount_max 
               f  = 0 
                  do  i =1,kount_max 
                      f  =  f +   intel_a (i) *  intel_b (i) 
                  enddo 
              intel_c(j) = intel_c(j) + f 
          enddo 
         end if

      return

      end
     
