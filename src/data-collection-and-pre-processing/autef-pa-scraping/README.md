# Scraper

Use the command line interface like this:

`python main.py --stage {pdfs_to_html|html_to_csv} --permits-to-process {int} --start-at {int}`

Where three parameters can be selected: 
- for `stage`, select the first conversion step `pdfs_to_html` or the last conversion step `html_to_csv`
- for `permits-to-process` select how many records to run 
- for the `start-at` select which index to begin converting at

## fields.json

You configure the scraper by telling it where to look for fields using the `fields.json` config format.

## Getting Started

1. Install Python > 3.11
2. From the root of this repo, create a virtualenv: `python -m venv venv` (this creates your virtual environment in the `venv` directory)
3. Activate the venv (Windows) `.\venv\Scripts\Activate.ps1` (you should now see `(venv)` to the left of your command prompt)
4. Install the required packages: `pip install -r .\requirements.txt`
5. Copy the PDFs to scrape from your source folder to `./pdfs/raw`

## Running the script

Below are the steps for running the entire batch of PDFs.

### Convert PDFs to HTML

`python main.py --stage pdfs_to_html`

Your `pdfs/html` folder should now contain all the PDFs in their own folders.

###  Convert HTMLs to CSV

`python main.py --stage html_to_csv`

Your `pdfs/done` folder should now contain all the CSVs produced.
