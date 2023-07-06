## \file OutputFormat_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the output of calculated values

from pytest import mark

from python import OutputFormat
from .TestHelpers import get_expected

## \brief Tests writing valid output
# \par Types of Testing:
# Dynamic Black-Box (Behavioural) Testing
# Equivalence Partitioning/Classing
# Logic Flow Testing
@mark.parametrize("s, d_offset, t_flight", get_expected("d_offset", "t_flight"))
def test_write_output_valid(s, d_offset, t_flight):
    OutputFormat.write_output(s, d_offset, t_flight)
    with open("output.txt") as f:
        assert f.readlines() == [f"s = {s}\n",
                                 f"d_offset = {d_offset}\n",
                                 f"t_flight = {t_flight}\n",]
