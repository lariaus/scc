import os
import sys

from xgenlibs import XGENDriver
import sir_defs

if __name__ == '__main__':
    root_dir = os.path.realpath(os.path.abspath(sys.argv[1]))
    driver = XGENDriver(root_dir)
    driver.run()