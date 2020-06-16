# T-ridge

This repository provides the implementations of the methods described in [Tuning-free ridge estimators for high-dimensional generalized linear models](https://arxiv.org/abs/2002.11916).

## Simulations

We provide an example code in `SimulationStudy.Rmd` for a comparison of averaged relative prediction errors with 10-fold cross-validated ridge for three types of generalized linear models including Gaussian, Poisson, and Bernoulli cases. Developed for `R 3.6.1`.

## Repository authors 

* Shih-Ting Huang, Ph.D. student in Mathematical Statistics, Ruhr-University Bochum

* Fang Xie, post-doctoral researcher in Mathematical Statistics, Ruhr-University Bochum

* Johannes Lederer, Professor in Mathematical Statistics, Ruhr-University Bochum

### Other folders

**Additional functions** : The source codes of some functions loaded in `SimulationStudy.Rmd` that are required in the simulation study.

**SimulationProcess** : The source codes loaded in `SimulationStudy.Rmd` for generating the simulation results.

## Supported languages and platforms

All of the codes in this repository are written in R and supports all plarforms which are
 supported by R itself.

## Dependencies

This repository depends on R libraries glmnet, MASS, htmlTable, and pander.

## Licensing

The HDIM package is licensed under the MIT license. To
view the MIT license please consult `LICENSE.txt`.

## References
 [Tuning-free ridge estimators for high-dimensional generalized linear models](https://arxiv.org/abs/2002.11916)
 
 cite as "Huang, Xie, and Lederer, Tuning-free ridge estimators for high-dimensional generalized linear models, arXiv:2002.11916".

