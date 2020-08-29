# FSIQ Stability Simulation

This repository includes a simulation of data using the known psychometric relationships between the WISC-V and WAIS-IV. All outcomes rely on simulated scores, known correlations between tests, and known SEM for each score.

An example of this is the stability of FSIQ after accounting for test-retest reliability, WISC-V FSIQ SEM, WISC-V/WAIS-IV correlations and WAIS-IV SEM.

![FSIQ Bias Plot](https://github.com/jakekraska/iqstability/blob/master/fsiqbias.png?raw=true)

## Authors and Contributions

Jake Kraska wrote the code for this analysis based on a blog post from [Shane Costello](https://shanecostello.net/temporalstability/).

## Correspondence

All correspondence regarding this analysis can be directed to [Jake Kraska](mailto:jake.kraska@monash.edu).

## Installation and Running

* Clone this repo to your local machine using https://github.com/jakekraska/iqstability.
* Open code.R in your preferred R IDE
* Install `ggplot2` necessary packages using the `install.packages("ggplot2")` command
* The code is commented heavily and minimal psychometrics knowledge should allow understanding of the output. Minimal R knowledge is required to understand the code.