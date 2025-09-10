import logging
import os
from datetime import datetime
from time import strftime

import humanize

from src.logger import Logger
from src.scraper import Scraper

import typer


def main(
        stage: str = typer.Option(..., help="Stage to run"),
        permits_to_process: int = typer.Option(-1, help="Number of permits to process"),
        start_at: int = typer.Option(0, help="Permit index to start at"),
        model: str = typer.Option("*", help="Model to scrape"),
        start_over: bool = typer.Option(0, help="Should we delete the existing CSVs and start again?"),
        verbose: bool = typer.Option(0, help="Should we print out more information?")
):
    dirname = os.path.dirname(os.path.abspath(__file__))

    input_dir = dirname + "/pdfs/raw/"
    html_dir = dirname + "/pdfs/html/"
    output_dir = dirname + "/pdfs/done/"
    error_dir = dirname + "/pdfs/"

    logging.basicConfig(
        level=logging.INFO,
        handlers=[
            logging.FileHandler("%s/errors.log" % error_dir, 'a'),
        ],
    )

    scraper = Scraper(
        Logger(),
        input_dir,
        html_dir,
        output_dir,
        error_dir,
        dirname + "/field_map.json",
        verbose
    )

    start_time = datetime.now()
    print("Script started at: %s\n" % start_time)

    if stage == "pdfs_to_html":
        # Step 1: Convert PDFs into HTML using PDFNet package.
        # Scraper.pdfs_to_html generates the HTML versions of the PDFs.
        print("Converting PDFs to HTML...\n")
        scraper.pdfs_to_html()

    if stage == "html_to_csv":
        # Step 2: Get data from HTML using beautifulsoup.
        # Scraper.scrape... scrapes the HTML files from Step 1 and adds each desired value to the data frame.
        scraper.html_to_csv(permits_to_process, start_at, model, start_over)

        print("Script took: %s to run." % humanize.naturaldelta(datetime.now() - start_time))


if __name__ == '__main__':
    typer.run(main)
