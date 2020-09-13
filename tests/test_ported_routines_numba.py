import sys, os
sys.path.insert(0, os.path.abspath('..'))
from gin import ported_routines, ported_routines_numba
from test_ported_routines import assert_equal
import pytest
import numpy as np
from numba import jit


XGK = np.array([0.9914553711208126e+00, 0.9491079123427585e+00, 0.8648644233597691e+00, 0.7415311855993944e+00,
                0.5860872354676911e+00, 0.4058451513773972e+00, 0.2077849550078985e+00, 0.0000000000000000e+00],
                dtype=np.float64)
WGK = np.array([0.2293532201052922e-01, 0.6309209262997855e-01, 0.1047900103222502e+00, 0.1406532597155259e+00,
                0.1690047266392679e+00, 0.1903505780647854e+00, 0.2044329400752989e+00, 0.2094821410847278e+00],
                dtype=np.float64)
WG = np.array([0.0000000000000000e+00, 0.1294849661688697e+00, 0.0000000000000000e+00, 0.2797053914892767e+00,
               0.0000000000000000e+00, 0.3818300505051189e+00, 0.0000000000000000e+00, 0.4179591836734694e+00],
               dtype=np.float64)


@pytest.mark.parametrize('fun_args', [
    (lambda x: np.exp(-0.5*x**2), jit(lambda x: np.exp(-0.5*x**2), nopython=True), ()),
    (lambda x: np.abs(x)*np.exp(-0.5*x**2), jit(lambda x: np.abs(x)*np.exp(-0.5*x**2), nopython=True), ()),
    (lambda x: x*np.exp(-0.5*x**2), jit(lambda x: x*np.exp(-0.5*x**2), nopython=True), ()),
    (lambda x: np.sin(x)*np.exp(-x**20), jit(lambda x: np.sin(x)*np.exp(-x**20), nopython=True), ()),
    (lambda x, y: x*np.exp(y*x**2), jit(lambda x, y: x*np.exp(y*x**2), nopython=True), (-0.5,)),
    (lambda x, y: np.abs(x)*np.exp(y*x**2), jit(lambda x, y: np.abs(x)*np.exp(y*x**2), nopython=True), (-0.5,)),
    (lambda x, y, z: x*np.exp(y*x**z), jit(lambda x, y, z: x*np.exp(y*x**z), nopython=True), (-0.5, 2)),
    (lambda x, y, z: np.abs(x)*np.exp(y*x**z), jit(lambda x, y, z: np.abs(x)*np.exp(y*x**z), nopython=True), (-0.5, 2)),
])
@pytest.mark.parametrize('boun_inf', [
    (0.0, 2),
    (0.1, 1),
    (0.5, 1),
    (1.5, 1),
    (-0.1, 1),
    (-0.5, 1),
    (-1.5, 1),
    (0.1, -1),
    (0.5, -1),
    (1.5, -1),
    (-0.1, -1),
    (-0.5, -1),
    (-1.5, -1)
])
@pytest.mark.parametrize('a', np.linspace(0, 0.4, 3, endpoint=True))
@pytest.mark.parametrize('b', np.linspace(0.6, 1.0, 3, endpoint=True))
def test_qk15i_equality(fun_args, boun_inf, a, b):
    fun = fun_args[0]
    numba_fun = fun_args[1]
    args = fun_args[2]
    boun = boun_inf[0]
    inf = boun_inf[1]
    
    fv1 = np.zeros(shape=7, dtype=np.float64)
    fv2 = np.zeros(shape=7, dtype=np.float64)

    ported_output = ported_routines.qk15i(fun, boun, inf, a, b, *args)
    ported_output_numba = ported_routines_numba.qk15i(numba_fun, boun, inf, a, b, XGK, WGK, WG, fv1, fv2, *args)

    epsilon = 1e-6
    assert_equal(ported_output[0], ported_output_numba[0], epsilon)
    assert_equal(ported_output[1], ported_output_numba[1], epsilon)
    assert_equal(ported_output[2], ported_output_numba[2], epsilon)
    assert_equal(ported_output[3], ported_output_numba[3], epsilon)
    


@pytest.mark.parametrize('input', [
    ((5, 1, 0, 3.0, np.array([2.0, 1.0, 0.0, 0.0, 0.0]), np.array([0, 0, 0, 0, 0]), 0)),
    ((5, 2, 0, 2.0, np.array([1.7, 1.0, 0.3, 0.0, 0.0]), np.array([0, 1, 0, 0, 0]), 0)),
    ((5, 2, 0, 1.2, np.array([0.7, 1.0, 0.5, 0.0, 0.0]), np.array([0, 1, 0, 0, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.7, 0.5, 0.3, 0.0]), np.array([1, 0, 2, 0, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.5, 0.5, 0.5, 0.0]), np.array([1, 0, 2, 0, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 2)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 1)),
    ((11, 7, 1, 0.81, np.array([0.7, 0.41, 0.6, 0.5, 0.45, 0.43, 0.42, 0.4, 0.0, 0.0, 0.0]), np.array([1, 0, 2, 3, 4, 5, 6, 0, 0, 0, 0]), 0))
])
def test_qpsrt_equality(input):
    limit = input[0]
    limit_n = limit
    last = input[1]
    last_n = last
    maxerr = input[2]
    maxerr_n = maxerr
    ermax = input[3]
    ermax_n = ermax
    elist = input[4]
    elist_n = np.copy(elist)
    iord = input[5]
    iord_n = np.copy(iord)
    nrmax = input[6]
    nrmax_n = nrmax

    ported_output = ported_routines.qpsrt(limit, last, maxerr, ermax, elist, iord, nrmax)
    ported_output_numba = ported_routines_numba.qpsrt(limit_n, last_n, maxerr_n, ermax_n, elist_n, iord_n, nrmax_n)

    epsilon = 1e-6

    assert_equal(ported_output[0], ported_output_numba[0], epsilon)
    assert_equal(ported_output[1], ported_output_numba[1], epsilon)
    assert_equal(ported_output[2], ported_output_numba[2], epsilon)
    assert_equal(ported_output[3], ported_output_numba[3], epsilon)


