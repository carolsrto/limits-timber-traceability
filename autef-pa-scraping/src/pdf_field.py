import re

from bs4 import BeautifulSoup


class PDF_Field:
    TABLE = "table"
    ID = "id"
    REGEX = "re"
    CHILD = "child"

    # Inject BeautifulSoup.
    def __init__(self, soup: BeautifulSoup):
        self.soup = soup

    # Get the value of a field.
    def get_value(self, search_text, ftype, sibling_index, comes_after=""):
        if not search_text:
            return ""

        el = self.get_element(ftype, search_text, sibling_index, comes_after)

        # If the element was not found, return an empty string.
        if not el:
            return ""

        # Get the text.
        txt = el.text

        if sibling_index == 0:
            txt = el.text.replace(search_text, "").replace(":", "").strip()

        if txt:  # If found.
            return txt
        else:  # If not found.
            return ""

    def get_element(self, ftype, search_text, sibling_index, child_index, comes_after=""):
        el = None

        # Default values.
        if not ftype:
            ftype = self.REGEX

        if sibling_index is None:
            # Next sibling of the span in question.
            sibling_index = 1

        # For finding an element as a child of another element.
        if ftype == self.CHILD:
            el = self.soup.find(id=search_text)
            if not el:
                return None

            children = el.findChildren()
            el_span = children[sibling_index]

        # For an element retrieved by id.
        if ftype == self.ID:
            el = self.soup.find(id=search_text)
            if not el:
                return None

            el_span = el.find_all('span')[sibling_index]

        # For an element retrieved by regular expression.
        elif ftype == self.REGEX:

            # If the field comes after another field, find that field first.
            if comes_after is not None and comes_after != "":
                found_el = self.soup.select('span', string=re.compile('^' + comes_after))
                if len(found_el) == 0:
                    return None

                els = found_el[0].findAllNext(
                    'span',
                    string=re.compile('^' + search_text)
                )
                try:
                    el = els[0]
                except IndexError:
                    return None
            else:
                el = self.soup.find('span', string=re.compile('^' + search_text))

            if not el:
                return None

            # For elements in which the value is in the same span as the text, retrieve the text and extract the search
            # text.
            # Get the first element of the siblings.
            if sibling_index == 1 and len(el.find_next_siblings(name="span")) > 0:
                el_span = el.next_sibling
            elif sibling_index > 1:
                try:
                    el_span = el.find_next_siblings(name="span")[sibling_index - 1]
                except IndexError:
                    el_span = el
            else:
                el_span = el

        if sibling_index == 0:
            return el

        return el_span

    def next_is(self, search_text, ftype, sibling_index, end, comes_after=""):
        next_val = self.get_value(search_text, ftype, sibling_index, comes_after)

        return next_val == end
