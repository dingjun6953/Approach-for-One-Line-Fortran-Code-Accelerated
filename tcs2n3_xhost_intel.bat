
echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O3_core_avx512_array64byte_40threads.exe > tcs2n3_O3_core_avx512_array64byte_4101001_01_80_200_00200000_36_64_1024_40_NotFittingCache_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O2_sse2_array64byte_40threads.exe > tcs2n3_O2_sse2_array64byte_4101001_01_80_200_00200000_36_64_1024_40_NotFittingCache_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O2_sse2_array16byte_40threads.exe > tcs2n3_O2_sse2_array16byte_4101001_01_80_200_00200000_36_64_1024_40_NotFittingCache_turbo_boosting_run1.out 2>&1

export CMG_LIC_HOST=rlmserv
export LD_LIBRARY_PATH=~/imex/linux_x64/lib3
export OMP_SCHEDULE=static,1
export KMP_AFFINITY=compact,0

echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O3_core_avx512_array64byte_40threads.exe > tcs2n3_O3_core_avx512_array64byte_4101001_01_80_200_00200000_36_64_1024_40_NotFitting_compact0_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O2_sse2_array64byte_40threads.exe > tcs2n3_O2_sse2_array64byte_4101001_01_80_200_00200000_36_64_1024_40_NotFittingCache_compact0_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 36  64 01024 40 | ./tcs2n3_O2_sse2_array16byte_40threads.exe > tcs2n3_O2_sse2_array16byte_4101001_01_80_200_00200000_36_64_1024_40_NotFittingCache_compact0_turbo_boosting_run1.out 2>&1

echo 4101001 01 80 200 00200000 20  64 01024 40 | ./tcs2n3_O3_core_avx512_array64byte_40threads.exe > tcs2n3_O3_core_avx512_array64byte_4101001_01_80_200_00200000_20_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run2.out 2>&1
echo 4101001 01 80 200 00200000 20  64 01024 40 | ./tcs2n3_O2_sse2_array64byte_40threads.exe > tcs2n3_O2_sse2_array64byte_4101001_01_80_200_00200000_20_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 20  64 01024 40 | ./tcs2n3_O2_sse2_array16byte_40threads.exe > tcs2n3_O2_sse2_array16byte_4101001_01_80_200_00200000_20_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run1.out 2>&1

echo 4101001 01 80 200 00200000 10  64 01024 40 | ./tcs2n3_O3_core_avx512_array64byte_40threads.exe > tcs2n3_O3_core_avx512_array64byte_4101001_01_80_200_00200000_10_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 10  64 01024 40 | ./tcs2n3_O2_sse2_array64byte_40threads.exe > tcs2n3_O2_sse2_array64byte_4101001_01_80_200_00200000_10_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run1.out 2>&1
echo 4101001 01 80 200 00200000 10  64 01024 40 | ./tcs2n3_O2_sse2_array16byte_40threads.exe > tcs2n3_O2_sse2_array16byte_4101001_01_80_200_00200000_10_64_1024_40_NotFittingCachecompact0_compact0_turbo_boosting_run1.out 2>&1
