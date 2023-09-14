## \file TestHelpers.py
#  \author Samuel J. Crawford
#  \brief Defines helper functions for testing

from pathlib import Path

from python import InputParameters
from python.test.test_input.input_data import valid_input_data

## \brief Reads the contents of a file from its full file name
def read_file(filename):
    with open(filename) as f:
        lines = f.readlines()
    return lines

## \brief Reads input from a file with the given file name (without extension)
def read_inParams(filename):
    return InputParameters.InputParameters(
        Path("test/test_input") / f"{filename}.txt")
        # TODO: does this work on Linux?

## \brief Returns a list of tuples with relevant value for each valid input
def get_expected(*fields):
    if len(fields) == 0:
        return [d["filename"] for d in valid_input_data]
    
    out = [(d["filename"],) for d in valid_input_data]
    for i in range(len(out)):
        for field in fields:
            out[i] = out[i] + (valid_input_data[i][field],)
    return out
