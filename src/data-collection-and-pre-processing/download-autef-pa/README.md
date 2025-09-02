
# Data Collection Tools 

You can use this script to speed up the collection of three types of information made publicly available at the SEMAS Transparency portal: AUTEFs, PMFs and GFs.   

## Getting Started

This is developed in Python. The tools are under development and suggestions for improvement are most welcome. 

1. Install Python > 3.11
2. From the root of this repo, create a virtualenv: `python -m venv venv` (this creates your virtual environment in the `venv` directory)
3. Activate the venv (Windows) `.\venv\Scripts\Activate.ps1` (you should now see `(venv)` to the left of your command prompt)
4. Install and Run the required packages


## Running the script

When starting the code you will be prompted to make the selection. 

- for "AUTEF", you further choose y/n to collect both `produtos` (details of species volume) and `ações` (geospatial details of authorizations)

- for "PMF", only `ações` available will be collected since there are no `produtos` details. 

- for "GFs", `ações`, `Xlsx` or `produtos` are available and can be selected. When collecting `ações` it is also possible to collect `produtos`, but at this stage it is not recommended as it delays the data collection

- when prompted to select a year if desired, the format follows e.g. `2022-2023`, with years of interest separated by a hyphen

- the script saves the latest proggress on `last_page.obj` for all data collected except the `Xlsx`, which is collected in `last_page_xlsx.obj`. This file is created automatically in your workspace. To restart collection simply delete it and restart/rerun script. 

- in line 12, you find `P_COUNT`, which determines the number of paralel processes ocurryingg at the same time. 8 appears to be a ggood value for the `ações`, `Xlsx`, but for `products` you can use 16. If you notice the proggram is running slow/failing, try tweaking these values. 

