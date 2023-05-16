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
epsilon = 0.02
valid_input_files = ["default_float", "default_int"]
expected_outputs = [
    {
        "filename": "default_float",
        "t_flight": 2.8861496557,
        "p_land":   40.816326531,
        "d_offset": -0.183673469,
        "s":        "The target was hit.",
    },
    {
        "filename": "default_int",
        "t_flight": 3.4345754482,
        "p_land":   37.114180687,
        "d_offset": -3.885819313,
        "s":        "The projectile fell short."
    },
]

# \brief Returns a list of tuples with relevant value for each valid input
def get_expected(*fields):
    out = [(d["filename"],) for d in expected_outputs]
    for i in range(len(out)):
        for field in fields:
            out[i] = out[i] + (expected_outputs[i][field],)
    return out

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

# \brief Tests calculation of d_offset with valid input
@mark.parametrize("filename,d_offset", get_expected("d_offset"))
def test_func_d_offset(filename, d_offset):
    inParams = read_inParams(filename)
    assert isclose(Calculations.func_d_offset(
        inParams, Calculations.func_p_land(inParams, g)), d_offset,
        abs_tol=1e-09) # needed for cancellation?
    
# \brief Tests calculation of s with valid input
@mark.parametrize("filename,d_offset,s", get_expected("d_offset", "s"))
def test_func_s(filename, d_offset, s):
    inParams = read_inParams(filename)
    assert Calculations.func_s(inParams, epsilon, d_offset) == s
