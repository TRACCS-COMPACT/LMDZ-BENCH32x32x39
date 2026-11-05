# LMDZ-BENCH32x32x39-DEMO


```DOI TO CREATE```

## Context and Motivation

Purpose of the experiment is to provide templates for realistic hybrid Python-LMDZ general atmospheric circulation configuration. Internal and external computed fields are written in an output file with the LMDZ output system (XIOS).

#### Variations
- **01** : Dummy Python script
- **K26** : Correction computed with [dummy_ref](https://gitlab.in2p3.fr/ipsl/espri/espri-ia/projects/lmdz_wind_bc) ML model 

## Requirements

### Compilation

- LMDZ6 trunk [5891](http://svn.lmd.jussieu.fr/LMDZ/LMDZ6/trunk/) patched with [TRACCS-COMPACT Patch](https://github.com/TRACCS-COMPACT/Patches-LMDZ) and local `CONFIG/my_src` sources.
```bash
wget http://lmdz.lmd.jussieu.fr/pub/install_lmdz.sh
chmod +x install_lmdz.sh
./install_lmdz.sh -name LMDZ6_ML -netcdf 0 -parallel mpi_omp -veget none -rad oldrad
```

- Code Compilation manager : none, use `compile.sh` script provided in `CONFIG`


### Python

- Eophis version : [v1.1.0](https://github.com/alexis-barge/eophis/tree/v1.1.0)
- **01** dependencies: none


### Run

- BENCH test case : 
```bash

```

- LMDZ Production Manager : None, use ``job.ksh`` in ``RUN``


### Post-Process

- No Post-Process libraries

- Plotting : Python script `plots_res.py` in `POSTPROCESS`


