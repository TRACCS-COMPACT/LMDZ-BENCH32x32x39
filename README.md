# LMDZ-BENCH32x32x39

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24132345.svg)](https://doi.org/10.5281/zenodo.24132345)


## Context and Motivation

Purpose of the experiment is to couple LMDZ BENCH test case with Python toolboxes for online diagnostics using formulations not implemented in LMDZ. Externally computed fields are written in an output file with the LMDZ output system (XIOS).


#### Variations
- **DJ08** : Computation of the wet-bulb potential temperature using the [Davies-Jones (2008)](https://doi.org/10.1175/2007MWR2224.1?utm_source=chatgpt.com) formulation from the [MetPy](https://unidata.github.io/MetPy/latest/) library.

## Requirements

### Compilation

- [LMDZ6](http://svn.lmd.jussieu.fr/LMDZ/LMDZ6/trunk/) patched with [TRACCS-COMPACT Patch](https://github.com/TRACCS-COMPACT/Patches-LMDZ) and local `CONFIG/my_src` sources.
```bash
wget http://lmdz.lmd.jussieu.fr/pub/install_lmdz.sh
chmod +x install_lmdz.sh
./install_lmdz.sh -name LMDZ6_PY -netcdf 0 -parallel mpi_omp -veget none -rad oldrad -bench 0
```

- Code Compilation manager : none, use `compile.sh` script provided in `CONFIG`


### Python

- Eophis version : [v1.1.0](https://github.com/alexis-barge/eophis/tree/v1.1.0)
- **DJ08** dependencies:
```bash
cd LMDZ-BENCH32x32x39/BENCH32x32x39.DJ08/INFERENCES
pip install -r requirements.txt
```


### Run

- Input files :
```bash
wget --no-check-certificate -nv http://lmdz.lmd.jussieu.fr/pub/3DBenchs/bench_lmdz_32x32x39.tar.gz
```

- LMDZ Production Manager : None, use ``job.ksh`` in ``RUN``


### Post-Process

- No Post-Process libraries required

- Plotting : Python script `plot_res.py` in `POSTPROCESS`
```bash
cd LMDZ-BENCH32x32x39/BENCH32x32x39.DJ08/POSTPROCESS
pip install -r requirements.txt
```

