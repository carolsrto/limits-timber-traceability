from bs4 import BeautifulSoup


class PDF_Table:
    def __init__(self, soup: BeautifulSoup, column_count, start_search, end_search):
        self.soup = soup

        self.column_count = column_count
        self.start_search = start_search
        self.end_search = end_search
       