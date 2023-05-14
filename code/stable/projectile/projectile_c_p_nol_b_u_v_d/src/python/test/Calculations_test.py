## \file Calculations_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark
from TestHelpers import read_inParams

from sys import path
path.append("../")
from python import Calculations

g = 9.8
valid_input_files = ["default_float", "default_int"]
expected_valid_input_calculations = [
    {
        "filename": "default_float",
        "t_flight": 2.8861496557,
        "p_land":   40.816326531,
    },
    {
        "filename": "default_int",
        "t_flight": 3.4345754482,
        "p_land":   37.114180687,
    },
]

# \brief Returns a list of tuples with relevant value for each valid input
def get_expected(field):
    return [(d["filename"], d[field]) for d in expected_valid_input_calculations]

# \brief Tests calculation of t_flight with valid input
@mark.parametrize("filename,t_flight", get_expected("t_flight"))
def test_func_t_flight(filename, t_flight):
    inParams = read_inParams(filename)
    assert isclose(Calculations.func_t_flight(inParams, g), t_flight)

# \brief Tests calculation of p_land with valid input
@mark.parametrize("filename,p_land", get_expected("p_land"))
def test_func_p_land(filename, p_land):
    inParams = read_inParams(filename)
    assert isclose(Calculations.func_p_land(inParams, g), p_land)
