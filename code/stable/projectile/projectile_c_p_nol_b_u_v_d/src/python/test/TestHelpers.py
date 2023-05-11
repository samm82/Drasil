## \file TestHelpers.py
# \author Samuel J. Crawford
# \brief Defines helper functions for testing

from pathlib import Path
from sys import path
path.append("../")
from python import InputParameters

def read_inParams(filename):
    return InputParameters.InputParameters(
        Path("test/test_input") / f"{filename}.txt")
