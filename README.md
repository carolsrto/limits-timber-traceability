
# Repository Overview

This repository contains the latest reproduceability tools for the study *Mapping the limits to timber traceability at origin: The case of Pará, Brazil*. Data required in the reproduction of this study is available on Zenodo at [https://doi.org/10.5281/zenodo.15672070](https://doi.org/10.5281/zenodo.15672070).

## Citation
Franca, C.S.S., Persson, U.M., Cardoso, D.R.R.S., Damasceno, C., Ward, R.S., Souza-Jr, C.M. (2025) Mapping the limits to timber traceability at origin: The case of Pará, Brazil [http://dx.doi.org/10.1016/j.oneear.2025.101447](http://dx.doi.org/10.1016/j.oneear.2025.101447))

## Repository Content 

Code
- data collection when automated
- data import and pre-processing
- geospatial analyses
- manuscript graphics 

Data 
- documentation of sources and aquisition
- data is not part of the repository 
- a *processed* (intermediary) dataset is available via Zenodo 
- other data can be made available from authors upon request 

## Instructions for results reproduction

For a *last step* reproduction of study numbers and figures use the script "10_limits_timber_traceability.R". Data to be used with this script can be found in the file "limits-traceability-v1.1.Rdata".

For the full reproduction of the study (from data collection, cleaning and pre-processing of data objects) you can follow the repository structure below. Not all scripts as listed below may have been added at this time, but can be made available upon request.



## Repository Structure

```
|-- limits-timber-traceability-main/
|-- README.txt
|   |-- data/
|   |   |-- raw/
|   |   |   |-- README.txt <-- documentation on data sources
|   |   |   |-- ...
|   |   |-- temp/
|   |   |-- processed/
|   |   |   |-- limits-traceability-v1.0.RData <-- available on Zenodo
|   |-- results/
|   |-- src/
|   |   |-- data-collection-and-pre-processing/
|   |   |   |-- download-autef-pa/ <-- tools used to auto-download autef data
|   |   |   |-- autef-pa-scrapping/ <-- tools to scrape autef pdf data
|   |   |   |-- 01_import-clean-upa.R
|   |   |   |-- 02_import-clean-amf.R 
|   |   |   |-- 03_import-clean-car.R
|   |   |   |-- 04_import-clean-atlas.R
|   |   |   |-- 05_import-clean-cnpf.R
|   |   |   |-- 06_import-clean-simex.R
|   |   |   |-- 07_logging-permits.R
|   |   |   |-- 08_transport.R
|   |   |-- data-analysis/
|   |   |   |-- 10_limits-timber-traceability.R
|   |   |-- data-supplementary/

```

## References 

Research reproduceability based on good practices from Kitzes, J., Turek, D., & Deniz, F. (Eds.). (2018). The Practice of Reproducible Research: Case Studies and Lessons from the Data-Intensive Sciences. Oakland, CA: University of California Press. 


## License
<a rel="license" href="http://creativecommons.org/licenses/by/3.0/">
  <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/3.0/88x31.png" /> 
</a>
<br />
This work is licensed under a 
<a rel="license" href="http://creativecommons.org/licenses/by/3.0/">
  Creative Commons Attribution 3.0 Unported License
</a>.
<br />
Check the 
<a rel="license" href="https://github.com/carolsrto/illegality-risk-ns/blob/main/LICENSE">
LICENSE
</a> 
for more details

