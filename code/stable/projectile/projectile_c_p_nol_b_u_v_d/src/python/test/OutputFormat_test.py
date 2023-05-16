## \file Calculations_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from pytest import mark

from sys import path
path.append("../")
from python import OutputFormat

expected_outputs = [
    # (s, d_offset, t_flight)
    ("The target was hit.",        -0.183673469, 2.8861496557), # result of default_float
    ("The projectile fell short.", -3.885819313, 3.4345754482), # result of default_int
    ("The projectile went long.",  0.816326531,  2.8861496557), # result of projectile_went_long
]

# \brief Tests writing valid input
@mark.parametrize("s, d_offset, t_flight", expected_outputs)
def test_get_input_valid(s, d_offset, t_flight):
    OutputFormat.write_output(s, d_offset, t_flight)
    with open("output.txt") as f:
        assert f.readlines() == [f"s = {s}\n",
                                 f"d_offset = {d_offset}\n",
                                 f"t_flight = {t_flight}\n",]
