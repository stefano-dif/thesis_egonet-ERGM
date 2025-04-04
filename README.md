# Thesis Code: From Egocentric Networks to Diffusion Processes

This repository contains the R code used in the thesis titled "From Egocentric Networks to Diffusion Processes: A Data-Driven Approach to Social Network Reconstruction". The code implements various steps for network reconstruction and simulation of dynamical processes.

## Setup

To use the code in this repository, you need to set up the environment using the `renv` package. Once you have cloned the repository, follow these steps to restore the environment:

```bash
# Restore the R environment based on renv.lock
renv::restore()
```

## Script Descriptions

    data_preparation.R: This script performs preprocessing steps on the data (e.g., smoothing degree distributions) and creates the _egor_ objects that are going to be used in the following scripts.

    .R: This script processes the raw egocentric data, performs preprocessing steps (e.g., smoothing degree distributions), and applies the Exponential Random Graph Model (ERGM) to reconstruct global network structures based on egocentric information.

    .R: This script simulates the dynamics of two spreading processes: (1) the SEIR epidemic model and (2) the Friedkin–Johnsen opinion dynamics model. These processes are simulated on the reconstructed networks to study the effects of network structure on dynamic behaviors.

    .R: This script evaluates the goodness-of-fit for the ERGM model by comparing the degree distributions and edgewise shared partners (ESP) of the simulated networks with the empirical data. The script also computes KL divergence to assess model performance.

    .R: This script generates all the visualizations used in the thesis, including degree distributions, clustering coefficients, epidemic spread patterns, and opinion dynamics over time. It saves the generated plots as PNG or PDF files.

## Acknowledgments
- ergm.ego: This package was used for fitting Exponential Random Graph Models (ERGMs) to the egocentric data.
- epinet: This package was used for simulating epidemic spreading processes (SEIR model) on the reconstructed networks.
