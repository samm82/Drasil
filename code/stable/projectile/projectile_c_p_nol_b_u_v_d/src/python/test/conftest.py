## \file conftest.py
# \author Samuel J. Crawford
# \brief Defines setup function for initializing inParams

from .TestHelpers import get_expected, read_inParams
from .test_input.expected_outputs import invalid_input_files

inParams = dict()
for filename in get_expected() + invalid_input_files:
    inParams[filename] = read_inParams(filename)
