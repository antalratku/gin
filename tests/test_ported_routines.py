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


@pytest.mark.parametrize('params', [
    ((5, 1, 0, 3.0, np.array([2.0, 1.0, 0.0, 0.0, 0.0]), np.array([0, 0, 0, 0, 0]), 0),
     (0, 2.0, np.array([0, 1, 0, 0, 0]), 0)),
    ((5, 2, 0, 2.0, np.array([1.7, 1.0, 0.3, 0.0, 0.0]), np.array([0, 1, 0, 0, 0]), 0),
     (0, 1.7, np.array([0, 1, 2, 0, 0]), 0)),
    ((5, 2, 0, 1.2, np.array([0.7, 1.0, 0.5, 0.0, 0.0]), np.array([0, 1, 0, 0, 0]), 0),
     (1, 1.0, np.array([1, 0, 2, 0, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 0),
     (1, 0.8, np.array([1, 0, 2, 3, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.7, 0.5, 0.3, 0.0]), np.array([1, 0, 2, 0, 0]), 0),
     (1, 0.7, np.array([1, 0, 2, 3, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.5, 0.5, 0.5, 0.0]), np.array([1, 0, 2, 0, 0]), 0),
     (0, 0.7, np.array([0, 1, 3, 2, 0]), 0)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 2),
     (1, 0.8, np.array([1, 1, 0, 3, 0]), 1)),
    ((5, 3, 1, 1.0, np.array([0.7, 0.8, 0.5, 0.2, 0.0]), np.array([1, 0, 2, 0, 0]), 1),
     (1, 0.8, np.array([1, 1, 2, 3, 0]), 1)),
    ((11, 7, 1, 0.81, np.array([0.7, 0.41, 0.6, 0.5, 0.45, 0.43, 0.42, 0.4, 0.0, 0.0, 0.0]), np.array([1, 0, 2, 3, 4, 5, 6, 0, 0, 0, 0]), 0),
     (0, 0.7, np.array([0, 2, 3, 4, 1, 7, 6, 0, 0, 0, 0]), 0))
])
def test_qpsrt(params):
    input = params[0]
    expected = params[1]

    limit = input[0]
    last = input[1]
    maxerr = input[2]
    ermax = input[3]
    elist = input[4]
    iord = input[5]
    nrmax = input[6]

    flast = last+1
    fmaxerr = maxerr+1
    fiord = np.copy(iord)
    for i in range(last):
        fiord[i] += 1
    fnrmax = nrmax+1

    ported_output = ported_routines.qpsrt(limit, last, maxerr, ermax, elist, iord, nrmax)
    fortran_output = f.qpsrt(limit=limit, last=flast, maxerr=fmaxerr, ermax=ermax,
                             elist=elist, iord=fiord, nrmax=fnrmax)

    epsilon = 1e-6

    assert ported_output[0] == expected[0]
    assert np.abs(ported_output[1] - expected[1]) < epsilon
    assert np.max(np.abs(ported_output[2] - expected[2])) < epsilon
    assert ported_output[3] == expected[3]

    assert fortran_output[0] == (ported_output[0]+1)
    assert np.abs(fortran_output[1] - ported_output[1]) < epsilon
    fiord_out = ported_output[2]

    # This part is necessary to test the jupbn = limit + 3 - last part
    if (flast > (limit//2)+2):
        add_until = limit + 3 - (last+1) - 1
    else:
        add_until = flast

    for i in range(add_until):
        fiord_out[i] += 1
    for i in range(add_until, limit):
        if (fiord_out[i] != 0):
            fiord_out[i] += 1

    assert np.max(np.abs(fortran_output[2] - np.array(fiord_out))) < epsilon
    assert fortran_output[3] == (ported_output[3]+1)


@pytest.mark.parametrize('params', [
    ((2, np.array([1.0, 2.0, 2.5]), np.array([0.0, 0.0, 0.0]), 0),
     (2, np.array([3.0, 2.0, 2.5, 0.0, 2.5]), 3.0, ported_routines.r1mach(2), np.array([3.0, 0.0, 0.0]), 1)),
    ((3, np.array([3.0, 2.0, 2.5, 2.8, 2.5]), np.array([3.0, 0.0, 0.0]), 1),
     (3, np.array([3.0, 3.25, 2.5, 2.8, 2.5, 2.8]), 3.25, ported_routines.r1mach(2), np.array([3.0, 3.25, 0.0]), 2)),
    ((4, np.array([3.0, 3.25, 2.5, 2.8, 2.91, 2.8]), np.array([3.0, 3.25, 0.0]), 2),
     (4, np.array([3.090909, 3.25, 2.97368421, 2.8, 2.91, 2.8, 2.91]), 2.97368421, ported_routines.r1mach(2), np.array([3.0, 3.25, 2.97368421]), 3)),
    ((5, np.array([3.090909, 3.25, 2.97368421, 2.8, 2.91, 2.97, 2.91]), np.array([3.0, 3.25, 2.97368421]), 3),
     (5, np.array([3.090909, 3.01532567, 2.97368421, 3.042, 2.91, 2.97, 2.91, 2.97]), 3.042, 0.3183157, np.array([3.25, 2.97368421, 3.042]), 4)),
])
def test_qelg(params):
    input = params[0]
    expected = params[1]
    
    n = input[0]
    epstab = np.zeros(52, dtype=np.float)
    for i in range(len(input[1])):
        epstab[i] = input[1][i]
    res3la = input[2]
    nres = input[3]
    
    epstab_exp = np.zeros(52, dtype=np.float)
    for i in range(len(expected[1])):
        epstab_exp[i] = expected[1][i]
    
    fn = n+1
    fepstab = np.copy(epstab)
    fres3la = np.copy(res3la)
    fnres = np.copy(nres)
    
    ported_output = ported_routines.qelg(n, epstab, res3la, nres)
    fortran_output = f.qelg(n=fn, epstab=fepstab, res3la=fres3la, nres=fnres)

    epsilon = 1e-6

    assert ported_output[0] == expected[0]
    assert np.max(np.abs(ported_output[1] - epstab_exp)) < epsilon
    assert np.abs(ported_output[2] - expected[2]) < epsilon
    assert np.abs(ported_output[3] - expected[3]) < epsilon
    assert np.max(np.abs(ported_output[4] - expected[4])) < epsilon
    assert ported_output[5] == expected[5]

    assert fortran_output[0] == (ported_output[0]+1)
    assert np.max(np.abs(fortran_output[1] - ported_output[1])) < epsilon
    assert np.abs(fortran_output[2] - ported_output[2]) < epsilon
    assert np.abs(fortran_output[3] - ported_output[3]) < epsilon
    assert np.max(np.abs(fortran_output[4] - ported_output[4])) < epsilon
    assert fortran_output[5] == ported_output[5]


@pytest.mark.parametrize('iter_cnt', np.arange(1, 100))
@pytest.mark.parametrize('fun', [
    lambda i: 2 + np.sum(1/(2**np.arange(0, i))),
    lambda i: 2 + np.sum((-1/2)**np.arange(0, i)),
    lambda i: np.sin((i+1)*np.pi/8)/((i+1)**2)
])
def test_qelg_iter(iter_cnt, fun):
    epstab = np.zeros(52, dtype=np.float)
    for i in range(3):
        epstab[i] = fun(i)
    n = 2
    res3la = np.zeros(3, dtype=np.float)
    nres = 0
    
    fn = np.copy(n)+1
    fepstab = np.copy(epstab)
    fres3la = np.copy(res3la)
    fnres = np.copy(nres)
    
    for _ in range(iter_cnt):
        n, epstab, result, abserr, res3la, nres = ported_routines.qelg(n, epstab, res3la, nres)
        fn, fepstab, fresult, fabserr, fres3la, fnres = f.qelg(fn, fepstab, fres3la, fnres)
        n += 1
        fn += 1
        epstab[n] = fun(n)
        fepstab[fn-1] = fun(fn-1)

    epsilon = 1e-5

    assert n+1 == fn
    assert np.max(np.abs(epstab - fepstab)) < epsilon
    assert np.abs(result - fresult) < epsilon
    assert np.abs(abserr - fabserr) < epsilon
    assert np.max(np.abs(res3la - fres3la)) < epsilon
    assert nres == fnres

