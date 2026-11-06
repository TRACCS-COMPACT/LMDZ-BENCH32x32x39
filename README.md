# LMDZ-BENCH32x32x39


```DOI TO CREATE```

## Context and Motivation

Purpose of the experiment is to couple LMDZ BENCH test case with Python toolboxes to compute online diagnostics using formulations not implemented in LMDZ. Internal and external computed fields are written in an output file with the LMDZ output system (XIOS).


#### Variations
- **DJ08** : Computation of the wet-bulb potential temperature using the [Davies-Jones (2008)](https://doi.org/10.1175/2007MWR2224.1?utm_source=chatgpt.com) formulation from the [MetPy](https://unidata.github.io/MetPy/latest/) library.

## Requirements

### Compilation

- LMDZ6 trunk [5891](http://svn.lmd.jussieu.fr/LMDZ/LMDZ6/trunk/) patched with [TRACCS-COMPACT Patch](https://github.com/TRACCS-COMPACT/Patches-LMDZ) and local `CONFIG/my_src` sources.
```bash
wget http://lmdz.lmd.jussieu.fr/pub/install_lmdz.sh
chmod +x install_lmdz.sh
./install_lmdz.sh -name LMDZ6_PY -netcdf 0 -parallel mpi_omp -veget none -rad oldrad
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

- LMDZ Production Manager : None, use ``job.ksh`` in ``RUN``


### Post-Process

- No Post-Process libraries required

- Plotting : Python script `plots_res.py` in `POSTPROCESS`
```bash
cd LMDZ-BENCH32x32x39/BENCH32x32x39.DJ08/POSTPROCESS
pip install -r requirements.txt
```

