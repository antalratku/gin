import numpy as np


def r1mach(i: int):
    '''
    http://www.lahey.com/docs/lgf13help/slatec/R1MACH.htm

    The machine dependent constants are returned from
    numpy finfo. To ensure consistence with the original Fortran
    routine, the constants are returned for 32-bit floats.
    '''
    if (i == 1):
        return np.finfo(np.float32).tiny
    elif (i == 2):
        return np.finfo(np.float32).max
    elif (i == 3):
        return np.finfo(np.float32).epsneg
    elif (i == 4):
        return np.finfo(np.float32).eps
    elif (i == 5):
        return np.float32(np.log10(2))
    else:
        raise ValueError(f'r1mach(i): i = {i} is out of bounds.')


def qk15i(f, boun, inf, a, b, *args):
    '''
    http://www.netlib.org/quadpack/qk15i.f
    '''
    xgk = [0.9914553711208126e+00, 0.9491079123427585e+00, 0.8648644233597691e+00, 0.7415311855993944e+00,
           0.5860872354676911e+00, 0.4058451513773972e+00, 0.2077849550078985e+00, 0.0000000000000000e+00]
    wgk = [0.2293532201052922e-01, 0.6309209262997855e-01, 0.1047900103222502e+00, 0.1406532597155259e+00,
           0.1690047266392679e+00, 0.1903505780647854e+00, 0.2044329400752989e+00, 0.2094821410847278e+00]
    wg = [0.0000000000000000e+00, 0.1294849661688697e+00, 0.0000000000000000e+00, 0.2797053914892767e+00,
          0.0000000000000000e+00, 0.3818300505051189e+00, 0.0000000000000000e+00, 0.4179591836734694e+00]
    
    epmach = r1mach(4)
    uflow = r1mach(1)
    dinf = min(1, inf)

    result = 0.0
    abserr = 0.0
    resabs = 0.0
    resasc = 0.0
    ierr = 0

    fv1 = np.zeros(shape=7, dtype=np.float)
    fv2 = np.zeros(shape=7, dtype=np.float)

    fvalt = 0.0
    
    centr = 0.5e+00*(a+b)
    hlgth = 0.5e+00*(b-a)
    tabsc1 = boun+dinf*(0.1e+01-centr)/centr
    fval1, ierr = f(tabsc1, *args)
    if ierr < 0:
        return result, abserr, resabs, resasc, ierr
    if (inf == 2):
        fval1, ierr = f(-tabsc1, *args)
        if ierr < 0:
            return result, abserr, resabs, resasc, ierr
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
        fval1, ierr = f(tabsc1, *args)
        if ierr < 0:
            return result, abserr, resabs, resasc, ierr
        fval2, ierr = f(tabsc2, *args)
        if ierr < 0:
            return result, abserr, resabs, resasc, ierr
        if (inf == 2):
            fvalt, ierr = f(-tabsc1, *args)
            if ierr < 0:
                return result, abserr, resabs, resasc, ierr
            fval1 = fval1 + fvalt
            fvalt, ierr = f(-tabsc2, *args)
            if ierr < 0:
                return result, abserr, resabs, resasc, ierr
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
    return result, abserr, resabs, resasc, ierr


def qpsrt(limit, last, maxerr, ermax, elist, iord, nrmax):
    '''
    http://www.netlib.org/quadpack/qpsrt.f
    '''

    errmax = 0.0
    errmin = 0.0

    ibeg = 0
    ido = 0
    isucc = 0
    jbnd = 0
    jupbn = 0
    k = 0

    if not (last > 1):
        iord[0] = 0
        iord[1] = 1
        maxerr = iord[nrmax]
        ermax = elist[maxerr]
        return maxerr, ermax, iord, nrmax    
    errmax = elist[maxerr]
    if (nrmax != 0):
        ido = nrmax
        for _ in range(ido):
            isucc = iord[nrmax-1]
            if errmax <= elist[isucc]:
                break
            iord[nrmax] = isucc
            nrmax = nrmax-1
    jupbn = last
    if (last+1 > limit//2+2):
        jupbn = limit+3-(last+1)-1
    errmin = elist[last]
    jbnd = jupbn-1
    ibeg = nrmax+1
    if ibeg > jbnd:
        iord[jbnd] = maxerr
        iord[jupbn] = last
        maxerr = iord[nrmax]
        ermax = elist[maxerr]
        return maxerr, ermax, iord, nrmax
    for i in range(ibeg, jbnd+1):
        isucc = iord[i]
        if errmax >= elist[isucc]:
            iord[i-1] = maxerr
            k = jbnd
            for _ in range(i, jbnd+1, 1):
                isucc = iord[k]
                if errmin < elist[isucc]:
                    iord[k+1] = last
                    maxerr = iord[nrmax]
                    ermax = elist[maxerr]
                    return maxerr, ermax, iord, nrmax
                iord[k+1] = isucc
                k = k+1
            iord[i] = last
            maxerr = iord[nrmax]
            ermax = elist[maxerr]
            return maxerr, ermax, iord, nrmax
        iord[i-1] = isucc
    iord[jbnd] = maxerr
    iord[jupbn] = last
    maxerr = iord[nrmax]
    ermax = elist[maxerr]
    return maxerr, ermax, iord, nrmax


def qelg(n, epstab, result, abserr, res3la, nres):
    '''
    http://www.netlib.org/quadpack/qelg.f

    Parameters:
    ----------
        n: integer
            epstab[n] contains the new element in the first column of
            the epsilon table.
            Coming from the QAGIE function this is numrl2 -> number of
            elements currently in rlist2. Therefore, the minimum value is 3.

        epstab: float[]
            Vector of dimension 52, containing the elements of the two
            lower diagonals of the triangular epsilon table.
            Coming from the QAGIE function this is rlist2.

        result: float
            The resulting approximation to the integral.
    
        abserr: float[]
            The estimate of the absolute error computed from result and the
            3 previous results.

        res3la: float[]
            The vector of dimension 3, containing the last 3 results.

        nres: int
            Number of calls to the routine -> should be zero at first call
            Note: nres is incremented at the beginning of the routine.
    '''
    epmach = 1e-20
    oflow = 1e20

    delta1 = 0.0
    delta2 = 0.0
    delta3 = 0.0

    epsinf = 0.0
    error = 0.0

    err1 = 0.0
    err2 = 0.0
    err3 = 0.0

    e0 = 0.0
    e1 = 0.0
    e2 = 0.0
    e3 = 0.0
    e1abs = 0.0

    res = 0.0
    ss = 0.0
    tol1 = 0.0
    tol2 = 0.0
    tol3 = 0.0

    nres = nres+1
    abserr = oflow
    result = epstab[n-1]
    if n < 3:
        abserr = max(abserr, 0.5e+01*epmach*abs(result))
        return n, epstab, result, abserr, res3la, nres
    limexp = 50
    epstab[n-1+2] = epstab[n-1]
    newelm = (n-1)//2
    epstab[n-1] = oflow
    num = n
    k1 = n-1
    for i in range(1, newelm+1):
        k2 = k1-1
        k3 = k1-2
        res = epstab[k1+2]
        e0 = epstab[k3]
        e1 = epstab[k2]
        e2 = res
        e1abs = abs(e1)
        delta2 = e2-e1
        err2 = abs(delta2)
        tol2 = max(abs(e2),e1abs)*epmach
        delta3 = e1-e0
        err3 = abs(delta3)
        tol3 = max(e1abs,abs(e0))*epmach
        if not ((err2 > tol2) or (err3 > tol3)):
            result = res
            abserr = err2+err3
            abserr = max(abserr, 0.5e+01*epmach*abs(result))
            return n, epstab, result, abserr, res3la, nres
        e3 = epstab[k1]
        epstab[k1] = e1
        delta1 = e1-e3
        err1 = abs(delta1)
        tol1 = max(e1abs,abs(e3))*epmach
        if ((err1 <= tol1) or (err2 <= tol2) or (err3 <= tol3)):
            n = i+i-1
            break
        ss = 0.1e+01/delta1+0.1e+01/delta2-0.1e+01/delta3
        epsinf = abs(ss*e1)
        if not (epsinf > 0.1e-03):
            n = i+i-1
            break
        res = e1+0.1e+01/ss
        epstab[k1] = res
        k1 = k1-2
        error = err2+abs(res-e2)+err3
        if not (error > abserr):
            abserr = error
            result = res
    if (n == limexp):
        n = 2*(limexp//2)-1
    ib = 0
    if ((num//2)*2 == num):
        ib = 1
    ie = newelm+1
    for i in range(ie):
        ib2 = ib+2
        epstab[ib] = epstab[ib2]
        ib = ib2
    if not (num == n):
        indx = num-n
        for i in range(n):
            epstab[i]= epstab[indx]
            indx = indx+1
    if (nres < 4):
        res3la[nres-1] = result # nres is the count of the call -> to use as index, subtract 1
        abserr = oflow
        abserr = max(abserr, 0.5e+01*epmach*abs(result))
        return n, epstab, result, abserr, res3la, nres
    abserr = abs(result-res3la[2])+abs(result-res3la[1])+abs(result-res3la[0])
    res3la[0] = res3la[1]
    res3la[1] = res3la[2]
    res3la[2] = result
    abserr = max(abserr,0.5e+01*epmach*abs(result))
    return n, epstab, result, abserr, res3la, nres            

