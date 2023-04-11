
export CMG_LIC_HOST=rlmserv
export LD_LIBRARY_PATH=~/imex/linux_x64/lib3
export OMP_SCHEDULE=static,1
export KMP_AFFINITY=compact,0

echo 41001 01 80 200 15000000 40  64 01024 80 | ./tcs2m_revision_O3_common_avx512.exe > tcs2m_revision_O3_common_avx512_41001_01_80_200_15000000_40_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 40  64 01024 80 | ./tcs2m_revision_O3_xhost_intel.exe > tcs2m_revision_O3_xhost_intel_41001_01_80_200_15000000_40_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 40  16 01024 80 | ./tcs2m_revision_O2_xSSE2_intel.exe > tcs2m_revision_O2_xSSE2_intel_41001_01_80_200_15000000_40_16_1024_80_NotFittingCache_compact0_turbo_enabled_run1.out 2>&1

echo 41001 01 80 200 15000000 36  64 01024 80 | ./tcs2m_revision_O3_common_avx512.exe > tcs2m_revision_O3_common_avx512_41001_01_80_200_15000000_36_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 36  64 01024 80 | ./tcs2m_revision_O3_xhost_intel.exe > tcs2m_revision_O3_xhost_intel_41001_01_80_200_15000000_36_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 36  16 01024 80 | ./tcs2m_revision_O2_xSSE2_intel.exe > tcs2m_revision_O2_xSSE2_intel_41001_01_80_200_15000000_36_16_1024_80_NotFittingCache_compact0_turbo_enabled_run1.out 2>&1

echo 41001 01 80 200 15000000 20  64 01024 80 | ./tcs2m_revision_O3_common_avx512.exe > tcs2m_revision_O3_common_avx512_41001_01_80_200_15000000_20_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 20  64 01024 80 | ./tcs2m_revision_O3_xhost_intel.exe > tcs2m_revision_O3_xhost_intel_41001_01_80_200_15000000_20_64_1024_80_NotFitting_compact0_turbo_enabled_run1.out 2>&1
echo 41001 01 80 200 15000000 20  16 01024 80 | ./tcs2m_revision_O2_xSSE2_intel.exe > tcs2m_revision_O2_xSSE2_intel_41001_01_80_200_15000000_20_16_1024_80_NotFittingCache_compact0_turbo_enabled_run1.out 2>&1



