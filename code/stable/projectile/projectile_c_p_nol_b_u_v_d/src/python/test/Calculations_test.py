## \file Calculations_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark, raises

from python import Calculations
from . import conftest
from .TestHelpers import get_expected, read_inParams

valid_g = 9.8
neg_g = -9.8
zero_g = 0
epsilon = 0.02

# \brief Tests calculation of t_flight with valid input
@mark.parametrize("filename,t_flight", get_expected("t_flight"))
def test_func_t_flight_valid(filename, t_flight):
    assert isclose(Calculations.func_t_flight(conftest.inParams[filename], valid_g), t_flight)

# \brief Tests calculation of p_land with valid input
@mark.parametrize("filename,p_land", get_expected("p_land"))
def test_func_p_land_valid(filename, p_land):
    assert isclose(Calculations.func_p_land(conftest.inParams[filename], valid_g), p_land)

# \brief Tests calculation of d_offset with valid input
@mark.parametrize("filename,d_offset", get_expected("d_offset"))
def test_func_d_offset_valid(filename, d_offset):
    assert isclose(Calculations.func_d_offset(
        conftest.inParams[filename], Calculations.func_p_land(conftest.inParams[filename], valid_g)), d_offset)
    
# \brief Tests calculation of s with valid input
@mark.parametrize("filename,d_offset,s", get_expected("d_offset", "s"))
def test_neg_func_s_valid(filename, d_offset, s):
    assert Calculations.func_s(conftest.inParams[filename], epsilon, d_offset) == s

# TODO: should the following tests also be made into Control tests?
# \brief Tests calculation of t_flight with negative gravitational acceleration
@mark.parametrize("filename,t_flight", get_expected("t_flight"))
def test_func_t_flight_neg_g(filename, t_flight):
    assert isclose(Calculations.func_t_flight(conftest.inParams[filename], neg_g), -t_flight)

# \brief Tests calculation of p_land with negative gravitational acceleration
@mark.parametrize("filename,p_land", get_expected("p_land"))
def test_func_p_land_neg_g(filename, p_land):
    assert isclose(Calculations.func_p_land(conftest.inParams[filename], neg_g), -p_land)

# \brief Tests calculation of d_offset with negative gravitational acceleration
@mark.parametrize("filename,d_offset,p_target", get_expected("d_offset", "p_target"))
def test_func_d_offset_neg_g(filename, d_offset,p_target):
    assert isclose(Calculations.func_d_offset(
        conftest.inParams[filename], Calculations.func_p_land(conftest.inParams[filename], neg_g)), -d_offset - 2 * p_target)

# \brief Tests calculation of t_flight with zero gravitational acceleration
@mark.parametrize("filename", get_expected())
def test_func_t_flight_zero_g(filename):
    with raises(ZeroDivisionError):
        Calculations.func_t_flight(conftest.inParams[filename], zero_g)

# \brief Tests calculation of p_land with zero gravitational acceleration
@mark.parametrize("filename", get_expected())
def test_func_p_land_zero_g(filename):
    with raises(ZeroDivisionError):
        Calculations.func_p_land(conftest.inParams[filename], zero_g)
