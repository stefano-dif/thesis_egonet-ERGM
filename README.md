# Thesis Code: From Egocentric Networks to Diffusion Processes

This repository contains the R code used in the thesis titled "From Egocentric Networks to Diffusion Processes: A Data-Driven Approach to Social Network Reconstruction". The code implements various steps for network reconstruction and simulation of dynamical processes.

## Setup

To use the code in this repository, you need to set up the environment using the `renv` package. Once you have cloned the repository, follow these steps to restore the environment:

```bash
# Restore the R environment based on renv.lock
renv::restore()
```

## Acknowledgments
- ergm.ego: This package was used for fitting Exponential Random Graph Models (ERGMs) to the egocentric data.
- epinet: This package was used for simulating epidemic spreading processes (SEIR model) on the reconstructed networks.
