      program cmgl

      implicit double precision (a-h,o-z)

!      integer, parameter :: istart=1, iend =1957, itt=33
!      integer, parameter :: istart=1, iend =4100, itt=33
      integer, parameter :: istart=1, iend =41001, itt=133
      real(8)  tempc3
      real(8)  adx(iend)
      real(8) vq(iend,itt)
      Integer(8) :: count1, count2, count_rate, count_max

      adx = 1.0
      vq = 2.0
      tempc3 = 0.0
 
      call system_clock(count1, count_rate, count_max)

      !!do kount = 1, 20000000
      do kount = 1, 5000000
      call t1(adx(1), vq(1,1), tempc3, istart, iend, itt)
      enddo

      call system_clock(count2, count_rate, count_max)

      print *,' tempc3 = ', tempc3
      print *,' time ', real(count2-count1) / real(count_rate)

      stop
      end

      subroutine t1(adx, vq, tempc3, istart, iend, itt)

      implicit double precision (a-h,o-z)

      real(8)  tempc3,tempc2
      real(8)  adx(iend)
      real(8) vq(iend,itt)
      tempc2 = 0.0


!!         !DIR$ IVDEP
!!         !DIR$ VECTOR ALWAYS
!!         !DIR$ vector align
!bp                     do 405 ks = ksts(icatis(ica)+1)+1, kstscf(ica)
                     do 405 ks = istart, iend
                        tempc2 = tempc2 + adx(ks)*vq(ks,itt)
  405                continue
                     tempc3=tempc2

      end
