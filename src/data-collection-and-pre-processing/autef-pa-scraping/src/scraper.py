import csv
import glob
import json
import re
import time
from pathlib import Path

import pandas as pd
from PDFNetPython3 import *
from PDFNetPython3.PDFNetPython import PDFNet, Convert
from bs4 import BeautifulSoup

from src.pdf_field import PDF_Field


class Scraper:

    def __init__(
            self,
            logger,
            input_dir,
            html_dir,
            output_dir,
            error_dir,
            field_map_path,
            verbose=False
    ):
        self.logger = logger
        self.input_dir = input_dir
        self.html_dir = html_dir
        self.output_dir = output_dir
        self.error_dir = error_dir
        self.field_map_path = field_map_path
        self.verbose = verbose

    # Convert the PDFs to HTML, which should be easier to crawl.
    def pdfs_to_html(self):
        errors = []
        PDFNet.Initialize("demo:1653215173602:7b8663780300000000158a6730bd31c4ffd5b0091160797d38327d8f76")

        for file in sorted(glob.glob(os.path.join(self.input_dir, "*.pdf"))):
            print("Converting %s to HTML" % file)
            filename = Path(os.path.basename(file)).stem.replace(' ', '_').lower()

            # Convert PDF document to HTML with fixed positioning option turned on (default)
            try:
                Convert.ToHtml(file, self.html_dir + filename)
            except:
                errors.append(filename)
                continue

        json_string = json.dumps(errors)
        json_file = open(self.error_dir + "last_run_errors.json", "w")
        json_file.write(json_string)
        json_file.close()

    # Creates the cover.html files (first page).
    # permits_to_process allows you to specify the number of permits to process at a time for testing and validation.
    # permits_to_process == -1 will process all permits.
    def html_to_csv(self, permits_to_process=-1, start_at=1, model="*", start_over=False):
        rows = []

        # Load the JSON file with field mappings.
        f = open(self.field_map_path, "r", encoding="utf-8")
        data = json.load(f)

        if start_over:
            self.create_csvs(data)

        self.hydrate_csvs(data, permits_to_process, start_at, model)

    # For each filename in the JSON file, create a new CSV file with the column names.
    def create_csvs(self, data):
        for model in data["models"]:
            columns = []

            # Find column names by page.
            for field in model["fields"]:
                if field["csvHeader"] not in columns:
                    columns.append(field["csvHeader"])

            # Create an empty dataframe with the columns we found.
            df = pd.DataFrame(columns=columns)

            # Write the columns to a new CSV file.
            df.to_csv(self.output_dir + model["modelname"] + ".csv", mode="w", sep=',', encoding='utf-8', index=False,
                      quoting=csv.QUOTE_ALL, header=columns)
            print("Created CSV for %s" % model["modelname"])

    # Scrape the HTML files and write the values to the CSV files.
    def hydrate_csvs(self, data, permits_to_process, start_at, model_to_scrape):
        permit_index = 0

        # Fill in the CSVs for each permit, folder by folder.

        # Get all the permits as subdirectories of the html dir.
        permits = []
        for root, dirs, filenames in os.walk(self.html_dir):
            for d in dirs:
                permits.append(d)
            break

        permits = sorted(permits)

        self.logger.info("Found %s permits" % len(permits))

        # Loop through all our permits.
        permits_processed = 0
        for idx, permit in enumerate(permits):
            # If we're starting at a specific permit, skip all the ones before it.
            if idx < start_at:
                continue

            # If we've processed all the permits we want, stop.
            if 0 < permits_to_process <= permits_processed:
                return

            # Scrape the permit for each model.
            models_processed = []
            for m in data["models"]:
                # Check if we've specified a model to scrape.
                if m["modelname"] == model_to_scrape or model_to_scrape == "*":
                    models_processed.append(m["modelname"])
                    self.process_model_for_permit(permit, m)

            permits_processed += 1
            self.logger.done(permit, models_processed)

    # Scrape a single HTML file for a single model.
    def process_model_for_permit(self, permit_dir, model):
        rows = []

        # Read the HTML file.
        filename = "%s%s/%s.xhtml" % (self.html_dir, permit_dir, model["filename"])
        if not os.path.isfile(filename):
            self.logger.warn("File '%s' does not exist for permit '%s' and model '%s'" % (model["filename"], permit_dir, model["modelname"]))
            return

        with open(filename, encoding="utf8") as hf_buffer:
            # Parse with BeautifulSoup.
            soup = BeautifulSoup(hf_buffer.read(), 'html.parser')

            # Legend is the CSV that associates filenames with autef numbers.
            if model.get("type") == "legend":
                row = self.scrape_html_file(soup, model["fields"])
                row[-1] = permit_dir
                rows.append(row)
                if self.verbose:
                    print("%s: Adding %s to the %s CSV" % (permit_dir, row[1], model["modelname"]))

            # Table models parse tables in the page, row-by-row.
            elif model.get("type") == PDF_Field.TABLE:
                beforestr = ""
                afterstr = ""

                if "beforeTableSearchString" in model:
                    beforestr = model["beforeTableSearchString"]

                if "afterTableSearchString" in model:
                    afterstr = model["afterTableSearchString"]

                rows = self.scrape_pseudo_table(
                    soup,
                    hf_buffer.name,
                    model["modelname"],
                    model["fields"],
                    beforestr,
                    afterstr,
                )

            # Simple models simply find values and populate the row.
            else:
                row = self.scrape_html_file(soup, model["fields"])
                rows.append(row)
                if self.verbose:
                    print("Adding %s to the %s CSV" % (row[0], model["modelname"]))

        # Write the row to the CSV file.
        df = pd.DataFrame(rows)
        df.to_csv(self.output_dir + model["modelname"] + ".csv", header=False, mode="a", sep=',',
                  encoding='utf-8', index=False, quoting=csv.QUOTE_ALL)

    # Where we actually grab values from the HTML files.
    @staticmethod
    def scrape_html_file(soup, fields):
        values = []

        # Run the cover page fields.
        for field in fields:

            if field.get("type") == PDF_Field.TABLE:
                continue

            f = PDF_Field(soup)
            v = f.get_value(field.get("searchString"), field.get("type"), field.get("siblingIndex"),
                            field.get("comesAfter"))

            values.append(v)

        return values

    # Takes the values from the tables embedded in PDF pages.
    def scrape_pseudo_table(self, soup, filename, modelname, fields, start_search, end_search):
        # Check whether the PDF contains the table.
        pdf_table = soup.find_all('span', string=re.compile('^' + start_search))
        if len(pdf_table) == 0:
            return []

        ret = []
        values = []
        column_count = 0

        for field in fields:
            f = PDF_Field(soup)

            # Get the simple fields.
            if not field.get("isColumn"):
                v = f.get_value(
                    field.get("searchString"),
                    field.get("type"),
                    field.get("siblingIndex"),
                    start_search
                )

                values.append(v)

            # Count the number of columns -- some pseudo-tables have 5 columns and some have 4, so we will need to
            # check whether the cols exist before adding.
            else:
                if self.field_exists(f, field):
                    column_count += 1

        # Now get the tablefields with the column_count being the siblingIndex (so, if there are 5 columns,
        # then the value should be five places away).
        if column_count:
            initial_count = column_count
            initial_vals = values[:]
            final_field = False

            # Hack get current time.
            start_time = time.time()

            while final_field is False:
                for field in fields:
                    v = ""  # Default for empty fields.
                    if not field.get("isColumn"):
                        continue

                    # Add the field to the CSV if it is found in the PDF, but if not, add an empty string.
                    # We do this so that the JSON config file determines the structure of the output CSV,
                    # not the data itself.
                    if self.field_exists(f, field):
                        f = PDF_Field(soup)
                        v = f.get_value(
                            field.get("searchString"),
                            field.get("type"),
                            column_count,
                            start_search
                        )

                    values.append(v)

                    if f.next_is(
                            field.get("searchString"),
                            field.get("type"),
                            column_count + 1,
                            end_search,
                            start_search
                    ):
                        final_field = True

                # Move to the next row by adding the number of columns the column count. This will look at siblingIndex
                # + column_count.
                column_count += initial_count
                ret.append(values)
                values = initial_vals[:]

                # Hack -- any table taking longer than 2s goes into the problem JSON.
                if time.time() - start_time > 2:
                    self.logger.fail("The table in %s for file %s could not be processed" % (modelname, filename))
                    return []

            # if next field is the terminal, stop.
        else:
            ret = values

        if self.verbose:
            print("%s: Processed %s rows for the %s table" % (ret[0][0], len(ret), modelname))

        return ret

    @staticmethod
    def field_exists(pdf_field, field):
        return pdf_field.get_element(
            field.get("type"),
            field.get("searchString"),
            0,
            field.get("comesAfter")
        ) is not None
