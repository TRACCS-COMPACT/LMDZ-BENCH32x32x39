#!/bin/bash

# LMDZ BENCH with OASIS and Eophis
 ./makelmdz_fcm -prod -config Bench -arch local-gfortran-parallel-cpl -j 8   -cpp key_eophis     -p lmd -d 32x32x39 -v false -mem -parallel mpi_omp -io xios -c true gcm
