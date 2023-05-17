## \file TestHelpers.py
# \author Samuel J. Crawford
# \brief Defines helper functions for testing

from pathlib import Path

from python import InputParameters
from python.test.test_input.expected_outputs import expected_outputs

# \brief Reads input from a file with the given file name (without extension)
def read_inParams(filename):
    return InputParameters.InputParameters(
        Path("test/test_input") / f"{filename}.txt")
        # TODO: does this work on Linux?

# \brief Returns a list of tuples with relevant value for each valid input
def get_expected(*fields):
    out = [(d["filename"],) for d in expected_outputs]
    for i in range(len(out)):
        for field in fields:
            out[i] = out[i] + (expected_outputs[i][field],)
    return out
