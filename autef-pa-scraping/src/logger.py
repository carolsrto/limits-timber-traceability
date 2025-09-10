import logging


class Logger:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

    def info(self, string):
        print(self.BOLD + "\u2139 " + string + self.ENDC)

    def ok(self, string):
        print(self.OKGREEN + "\u2713 " + string + self.ENDC)

    def done(self, permit, models):
        print("%s %s Processed %s%s: %s" % (self.OKGREEN, "\u2713 ", permit, self.ENDC, ", ".join(models)))

    def warn(self, string):
        print(self.WARNING + "\u26A0 " + string + self.ENDC)
        logging.warning(string)

    def fail(self, string):
        print(self.FAIL + "\u2717 " + string + self.ENDC)
        logging.fatal(string)
