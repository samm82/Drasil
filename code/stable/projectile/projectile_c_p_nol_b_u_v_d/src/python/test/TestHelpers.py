## \file TestHelpers.py
# \author Samuel J. Crawford
# \brief Defines helper functions for testing

from pathlib import Path
from sys import path
path.append("../")
from python import InputParameters

# \brief Reads input from a file with the given file name (without extension)
def read_inParams(filename):
    return InputParameters.InputParameters(
        Path("test/test_input") / f"{filename}.txt")
        # TODO: does this work on Linux?
