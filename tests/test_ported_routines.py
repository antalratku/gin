import sys, os
sys.path.insert(0, os.path.abspath('..'))
sys.path.insert(0, os.path.join(os.getcwd(), 'tests/fortran_routines'))
import fortran_routines.f90.f_routines as f
from gin import ported_routines
import pytest
import numpy as np


@pytest.mark.parametrize('i', [1, 2, 3, 4, 5])
def test_r1mach(i):
    fortran_val = f.r1mach(i)
    ported_val = ported_routines.r1mach(i)
    
    assert(fortran_val == ported_val)


@pytest.mark.parametrize('fun_args', [
    (lambda x: (np.exp(-0.5*x**2), 0), ()),
    (lambda x: (x*np.exp(-0.5*x**2), 0), ()),
    (lambda x, y: (x*np.exp(y*x**2), 0), (-0.5,)),
    (lambda x, y, z: (x*np.exp(y*x**z), 0), (-0.5, 2)),
])
@pytest.mark.parametrize('boun_inf', [
    (0.5, 1),
    (1.5, 1),
    (-0.5, -1),
    (-1.5, -1)
])
@pytest.mark.parametrize('a', np.linspace(0, 0.4, 3, endpoint=True))
@pytest.mark.parametrize('b', np.linspace(0.6, 1.0, 3, endpoint=True))
def test_qk15i_with_bound(fun_args, boun_inf, a, b):
    fun = fun_args[0]
    args = fun_args[1]
    boun = boun_inf[0]
    inf = boun_inf[1]

    ported_output = ported_routines.qk15i(fun, boun, inf, a, b, *args)
    fortran_output = f.qk15i(fun, boun, inf, a, b, args)
    
    epsilon = 1e-4

    assert np.abs(ported_output[0] - fortran_output[0]) < epsilon
    assert np.abs(ported_output[1] - fortran_output[1]) < epsilon
    assert np.abs(ported_output[2] - fortran_output[2]) < epsilon
    assert np.abs(ported_output[3] - fortran_output[3]) < epsilon
    assert ported_output[4] == fortran_output[4]


@pytest.mark.parametrize('fun_args', [
    (lambda x: (np.exp(-0.5*x**2), 0), ()),
    (lambda x: (np.abs(x)*np.exp(-0.5*x**2), 0), ()),
    (lambda x, y: (np.abs(x)*np.exp(y*x**2), 0), (-0.5,)),
    (lambda x, y, z: (np.abs(x)*np.exp(y*x**z), 0), (-0.5, 2)),
])
@pytest.mark.parametrize('boun_inf', [(0, 2)])
@pytest.mark.parametrize('a', np.linspace(0, 0.4, 3, endpoint=True))
@pytest.mark.parametrize('b', np.linspace(0.6, 1.0, 3, endpoint=True))
def test_qk15i_no_bound(fun_args, boun_inf, a, b):
    fun = fun_args[0]
    args = fun_args[1]
    boun = boun_inf[0]
    inf = boun_inf[1]

    ported_output = ported_routines.qk15i(fun, boun, inf, a, b, *args)
    fortran_output = f.qk15i(fun, boun, inf, a, b, args)
    
    epsilon = 1e-4

    assert np.abs(ported_output[0] - fortran_output[0]) < epsilon
    assert np.abs(ported_output[1] - fortran_output[1]) < epsilon
    assert np.abs(ported_output[2] - fortran_output[2]) < epsilon
    assert np.abs(ported_output[3] - fortran_output[3]) < epsilon
    assert ported_output[4] == fortran_output[4]
