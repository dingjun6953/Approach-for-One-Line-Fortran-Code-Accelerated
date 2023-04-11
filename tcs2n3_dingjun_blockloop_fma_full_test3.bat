
export CMG_LIC_HOST=rlmserv
export LD_LIBRARY_PATH=~/imex/linux_x64/lib3
export OMP_SCHEDULE=static,1
export KMP_AFFINITY=compact,0

echo 2560 4101001 01 80 200 2560 40 64 256 256 | ./tcs2n3_dingjun_v9_O3_core_avx512_fma_manualblookloop.exe > tcs2n3_dingjun_v9_O3_core_avx512_fma_manualblookloop_256BS256_run1.out 2>&1
echo 2560 4101001 01 80 20 2560 40 64 256 256 | ./tcs2n3_dingjun_v9_O3_core_avx512_fma_manualblookloop.exe > tcs2n3_dingjun_v9_O3_core_avx512_fma_NOmanualblookloop_256BS256_run1.out 2>&1






