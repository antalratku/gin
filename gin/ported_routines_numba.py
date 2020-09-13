import sys, os
sys.path.insert(0, os.path.abspath('..'))
import numpy as np
from numba import jit

@jit
def d1mach(i: int):
    '''
    http://computer-programming-forum.com/49-fortran/9d39e9771b0d8e20.htm

    The machine dependent constants are returned from
    numpy finfo. To ensure consistence with the original Fortran
    routine, the constants are returned for 64-bit floats.
    '''

    if (i == 1):
        return np.finfo(np.float64).tiny
    elif (i == 2):
        return np.finfo(np.float64).max
    elif (i == 3):
        return np.finfo(np.float64).epsneg
    elif (i == 4):
        return np.finfo(np.float64).eps
    elif (i == 5):
        return np.float64(np.log10(2))
    else:
        raise ValueError('d1mach(i): The supplied index is out of bounds.')


@jit(nopython=True)
def qk15i(f, boun, inf, a, b, xgk, wgk, wg, fv1, fv2, *args):    
    epmach = d1mach(4)
    uflow = d1mach(1)
    dinf = min(1, inf)

    result = 0.0
    abserr = 0.0
    resabs = 0.0
    resasc = 0.0

    fvalt = 0.0
    
    centr = 0.5e+00*(a+b)
    hlgth = 0.5e+00*(b-a)
    tabsc1 = boun+dinf*(0.1e+01-centr)/centr
    fval1 = f(tabsc1, *args)
    if (inf == 2):
        fvalt = f(-tabsc1, *args)
        fval1 = fval1 + fvalt
    fc = (fval1/centr)/centr
    resg = wg[7]*fc
    resk = wgk[7]*fc
    resabs = abs(resk)
    for j in range(7):
        absc = hlgth*xgk[j]
        absc1 = centr-absc
        absc2 = centr+absc
        tabsc1 = boun+dinf*(0.1e+01-absc1)/absc1
        tabsc2 = boun+dinf*(0.1e+01-absc2)/absc2
        fval1 = f(tabsc1, *args)
        fval2 = f(tabsc2, *args)
        if (inf == 2):
            fvalt = f(-tabsc1, *args)
            fval1 = fval1 + fvalt
            fvalt = f(-tabsc2, *args)
            fval2 = fval2 + fvalt
        fval1 = (fval1/absc1)/absc1
        fval2 = (fval2/absc2)/absc2
        fv1[j] = fval1
        fv2[j] = fval2
        fsum = fval1+fval2
        resg = resg+wg[j]*fsum
        resk = resk+wgk[j]*fsum
        resabs = resabs+wgk[j]*(abs(fval1)+abs(fval2))
    reskh = resk*0.5e+00
    resasc = wgk[7]*abs(fc-reskh)
    for j in range(7):
        resasc = resasc+wgk[j]*(abs(fv1[j]-reskh)+abs(fv2[j]-reskh))
    result = resk*hlgth
    resasc = resasc*hlgth
    resabs = resabs*hlgth
    abserr = abs((resk-resg)*hlgth)
    if (resasc != 0.0e+00) and (abserr != 0.e0):
        abserr = resasc*min(0.1e+01,(0.2e+03*abserr/resasc)**1.5e+00)
    if (resabs > uflow / (0.5e+02*epmach)):
        abserr = max((epmach*0.5e+02)*resabs, abserr)
    return result, abserr, resabs, resasc
