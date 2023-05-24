## \file Calculations_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark

from .TestHelpers import get_expected, read_inParams
from python import Calculations

g = 9.8
epsilon = 0.02

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
        inParams, Calculations.func_p_land(inParams, g)), d_offset)
    
# \brief Tests calculation of s with valid input
@mark.parametrize("filename,d_offset,s", get_expected("d_offset", "s"))
def test_func_s(filename, d_offset, s):
    inParams = read_inParams(filename)
    assert Calculations.func_s(inParams, epsilon, d_offset) == s
