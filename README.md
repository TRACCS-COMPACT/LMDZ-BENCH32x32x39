# LMDZ-BECNH32x32x39-DEMO


```DOI TO CREATE```

## Context and Motivation

Purpose of the experiment is to provide a simple template for Python hybrid realistic atmospheric general circulation configuration. Internal and external computed fields are written in an output file with the LMDZ output system (XIOS).

#### Variations
- **01** : Dummy Python script
- **K26** : Correction computed with [dummy_ref](https://gitlab.in2p3.fr/ipsl/espri/espri-ia/projects/lmdz_wind_bc) ML model 

## Requirements

### Compilation

- LMDZ6 trunk [5891](http://svn.lmd.jussieu.fr/LMDZ/LMDZ6/trunk/) patched with [TRACCS-COMPACT Patch](https://github.com/TRACCS-COMPACT/Patches-LMDZ) and local `CONFIG/my_src` sources.

- Code Compilation manager : none, use `compile.sh` script provided in `CONFIG`


### Python

- Eophis version : [v1.1.0](https://github.com/alexis-barge/eophis/tree/v1.1.0)
- **01** dependencies:
  ```bash
  cd BENCH32x32x39-DEMO.01/INFERENCES/
  pip install -r requirements.txt
  ```
- **K26** dependencies:
  ```bash
  cd BENCH32x32x39-DEMO.K26/INFERENCES/
  pip install -r requirements.txt
  ```


### Run

- LMDZ Production Manager : None, use ``job.ksh`` in ``RUN``


### Post-Process

- No Post-Process libraries

- Plotting : Python script `plots_res.py` in `POSTPROCESS`


