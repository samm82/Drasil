## \file Calculations_mock_test.py
#  \author Samuel J. Crawford
#  \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark, raises
from unittest.mock import Mock, patch, PropertyMock

from python.InputParameters import InputParameters
from python import Calculations
from . import conftest
from .TestHelpers import get_expected, read_inParams

valid_g = 9.8
neg_g = -9.8
zero_g = 0
epsilon = 0.02

def build_mocks(*attrs):
    mocks = []
    defaults = ["v_launch", "theta", "p_target"]
    for d in get_expected(*(defaults + list(attrs))):
        mock_attrs = dict()
        for i, attr in enumerate(defaults + list(attrs), start=1):
            mock_attrs[attr] = d[i]
        mock = Mock()
        mock.configure_mock(**mock_attrs)
        mocks.append(mock)
    return mocks

## \brief Tests calculation of t_flight with valid mock input
@mark.parametrize("mock", build_mocks("t_flight"))
def test_func_t_flight_mock_valid(mock):
    assert isclose(Calculations.func_t_flight(mock, valid_g), mock.t_flight)

## \brief Tests calculation of p_land with valid mock input
@mark.parametrize("mock", build_mocks("p_land"))
def test_func_p_land_mock_valid(mock):
    assert isclose(Calculations.func_p_land(mock, valid_g), mock.p_land)

## \brief Tests calculation of d_offset with valid mock input
@mark.parametrize("mock", build_mocks("p_land", "d_offset"))
def test_func_d_offset_mock_valid(mock):
    assert isclose(Calculations.func_d_offset(mock, mock.p_land), mock.d_offset)
    
## \brief Tests calculation of s with valid mock input
@mark.parametrize("mock", build_mocks("d_offset", "s"))
def test_func_s_mock_valid(mock):
    assert Calculations.func_s(mock, epsilon, mock.d_offset) == mock.s

# TODO: should the following tests also be made into Control tests?
## \brief Tests calculation of t_flight with negative gravitational acceleration and \n
#  valid mock input
@mark.parametrize("mock", build_mocks("t_flight"))
def test_func_t_flight_mock_neg_g(mock):
    assert isclose(Calculations.func_t_flight(mock, neg_g), -mock.t_flight)

## \brief Tests calculation of p_land with negative gravitational acceleration and \n
#  valid mock input
@mark.parametrize("mock", build_mocks("p_land"))
def test_func_p_land_mock_neg_g(mock):
    assert isclose(Calculations.func_p_land(mock, neg_g), -mock.p_land)

## \brief Tests calculation of d_offset with negative gravitational acceleration and \n
#  valid mock input
@mark.parametrize("mock", build_mocks("p_land", "d_offset", "p_target"))
def test_func_d_offset_mock_neg_g(mock):
    assert isclose(Calculations.func_d_offset(mock, -mock.p_land),
                     -mock.d_offset - 2 * mock.p_target)

## \brief Tests calculation of t_flight with zero gravitational acceleration and \n
#  valid mock input
@mark.parametrize("mock", build_mocks())
def test_func_t_flight_mock_zero_g(mock):
    with raises(ZeroDivisionError):
        Calculations.func_t_flight(mock, zero_g)

## \brief Tests calculation of p_land with zero gravitational acceleration and \n
#  valid mock input
@mark.parametrize("mock", build_mocks())
def test_func_p_land_mock_zero_g(mock):
    with raises(ZeroDivisionError):
        Calculations.func_p_land(mock, zero_g)
