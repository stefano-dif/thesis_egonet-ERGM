# Thesis Code: From Egocentric Networks to Diffusion Processes

This repository contains the R code used in the thesis titled "From Egocentric Networks to Diffusion Processes: A Data-Driven Approach to Social Network Reconstruction". The code implements various steps for network reconstruction and simulation of dynamical processes.

## Setup

To use the code in this repository, you need to set up the environment using the `renv` package. Once you have cloned the repository, follow these steps to restore the environment:

```bash
# Restore the R environment based on renv.lock
renv::restore()
```

## Script Descriptions

(Missing scripts will be loaded soon)

```data_preparation.R```: This script performs preprocessing steps on the data (e.g., smoothing degree distributions) and creates the _egor_ objects that are going to be used in the following scripts.

```data_analysis.qmd```: This script provides an exploratory analysis of the egocentric data.

```final_ERGMs.qmd```: This script fits Exponential Random Graph Models (ERGMs) to the egocentric data and computes GoF measures, by comparing the degree distributions and edgewise shared partners (ESP) of the simulated networks with the data.

```[to add].R```: This script evaluates the global metrics of the reconstructed networks against benchmark reconstructions (ER and AB).

```[to add].R```: This script simulates the dynamics of two spreading processes: (1) the SEIR epidemic model and (2) the Friedkin–Johnsen opinion dynamics model. These processes are simulated on the reconstructed networks to study the effects of network structure on dynamic behaviors and compared against the dynamics on benchmark reconstructions (ER and AB).



## Acknowledgments
- ergm.ego: This package was used for fitting Exponential Random Graph Models (ERGMs) to the egocentric data.
- epinet: This package was used for simulating epidemic spreading processes (SEIR model) on the reconstructed networks.
