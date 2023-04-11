
export CMG_LIC_HOST=rlmserv
export LD_LIBRARY_PATH=~/imex/linux_x64/lib3
export OMP_SCHEDULE=static,1
export KMP_AFFINITY=compact,0

echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_revision2_O3_xhost.exe > tcs2m_defaultblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_revision2_O3_xhost.exe > tcs2m_defaultblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run2.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_revision2_O3_xhost.exe > tcs2m_defaultblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run3.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_revision2_O3_xhost.exe > tcs2m_defaultblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run4.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_revision2_O3_xhost.exe > tcs2m_defaultblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run5.out 2>&1

echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_noblockloop_O3_xhost.exe > tcs2m_noblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_noblockloop_O3_xhost.exe > tcs2m_noblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run2.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_noblockloop_O3_xhost.exe > tcs2m_noblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run3.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_noblockloop_O3_xhost.exe > tcs2m_noblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run4.out 2>&1
echo 4101001 01 80 200 00200000 40  64 | ./tcs2m_noblockloop_O3_xhost.exe > tcs2m_noblockloop_O3_xhost_4101001_01_80_200_00200000_40_64_NotFitting_compact0_turbo_enabled_run5.out 2>&1



