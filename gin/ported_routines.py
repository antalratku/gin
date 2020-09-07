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
                k = k-1
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


def qelg(n, epstab, res3la, nres):
    '''
    http://www.netlib.org/quadpack/qelg.f
    '''

    epmach = r1mach(4)
    oflow = r1mach(2)

    nres = nres+1
    abserr = oflow
    result = epstab[n]
    if n < 2:
        abserr = max(abserr, 0.5e+01*epmach*abs(result))
        return n, epstab, result, abserr, res3la, nres
    limexp = 49
    epstab[n+2] = epstab[n]
    newelm = (n-2)//2
    epstab[n] = oflow
    num = n
    k1 = n
    for i in range(1, newelm+2):
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
            n = i+i-2
            break
        ss = 0.1e+01/delta1+0.1e+01/delta2-0.1e+01/delta3
        epsinf = abs(ss*e1)
        if not (epsinf > 0.1e-03):
            n = i+i-2
            break
        res = e1+0.1e+01/ss
        epstab[k1] = res
        k1 = k1-2
        error = err2+abs(res-e2)+err3
        if (error <= abserr):
            abserr = error
            result = res
    if (n == limexp):
        n = 2*(limexp//2)
    ib = 0
    if ((num//2)*2 != num):
        ib = 1
    ie = newelm+2
    for _ in range(ie):
        ib2 = ib+2
        epstab[ib] = epstab[ib2]
        ib = ib2
    if (num != n):
        indx = num-n
        for i in range(n+1):
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


def qagie(f, bound, inf, epsabs, epsrel, limit, *args):
    '''
    http://www.netlib.org/quadpack/qagie.f
    '''

    alist = np.zeros(shape=limit, dtype=np.float)
    blist = np.zeros(shape=limit, dtype=np.float)
    elist = np.zeros(shape=limit, dtype=np.float)
    iord = np.zeros(shape=limit, dtype=np.int)
    res3la = np.zeros(shape=3, dtype=np.float)
    rlist = np.zeros(shape=limit, dtype=np.float)
    rlist2 = np.zeros(shape=52, dtype=np.float)

    epmach = r1mach(4)

    ier = 0
    neval = 0
    last = 0
    result = 0.0e+00
    abserr = 0.0e+00
    alist[0] = 0.0e+00
    blist[0] = 0.1e+01
    rlist[0] = 0.0e+00
    elist[0] = 0.0e+00
    iord[0] = 0

    if ((epsabs <= 0.0e+00) & (epsrel < max(0.5e+02*epmach, 0.5e-14))):
        ier = 6
        return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last

    boun = bound
    if (inf == 2):
        boun = 0.0e+00

    result, abserr, defabs, resabs, ier = qk15i(f, boun, inf, 0.0e+00, 0.1e+01, *args)
    if (ier < 0):
        return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
    
    last = 1
    rlist[0] = result
    elist[0] = abserr
    iord[0] = 0
    dres = abs(result)
    errbnd = max(epsabs, epsrel*dres)
    if ((abserr <= 1.0E+02*epmach*defabs) & (abserr>errbnd)):
        ier = 2
    if (limit == 1):
        ier = 1    
    if ((ier != 0) | ((abserr <= errbnd) & (abserr != resabs)) | (abserr == 0.0E+00 )):
        neval = 30*last-15
        if (inf == 2):
            neval = 2*neval
        if (ier > 2):
            ier = ier -1
        return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
    uflow = r1mach(1)
    oflow = r1mach(2)
    rlist2[0] = result
    errmax = abserr
    maxerr = 0
    area = result
    errsum = abserr
    abserr = oflow
    nrmax = 0
    nres = 0
    ktmin = 0
    numrl2 = 1
    extrap = False
    noext = False
    ierro = 0
    iroff1 = 0
    iroff2 = 0
    iroff3 = 0
    ksgn = -1
    if (dres >= (0.1E+01-0.5E+02*epmach)*defabs):
        ksgn = 1
    for last in range(1, limit):
        a1 = alist[maxerr]
        b1 = 0.5E+00*(alist[maxerr]+blist[maxerr])
        a2 = b1
        b2 = blist[maxerr]
        erlast = errmax
        area1, error1, resabs, defab1, ier = qk15i(f, boun, inf, a1, b1, *args)
        if (ier < 0):
            return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
        area2, error2, resabs, defab2, ier = qk15i(f, boun, inf, a2, b2, *args)
        if (ier < 0):
            return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
        area12 = area1 + area2
        erro12 = error1 + error2
        errsum = errsum + erro12 - errmax
        area = area + area12 - rlist[maxerr]
        if ((defab1 != error1) & (defab2 != error2)):
            if ((abs(rlist[maxerr]-area12) <= 0.1E-04*abs(area12)) & (erro12 >= 0.99E+00*errmax)):
               if extrap:
                   iroff2 = iroff2 + 1
               else:
                   iroff1 = iroff1 + 1
            if ((last >= 10) & (erro12 > errmax)):
                iroff3 = iroff3 + 1
        rlist[maxerr] = area1
        rlist[last] = area2
        errbnd = max(epsabs,epsrel*abs(area))
        if ((iroff1+iroff2 >= 10) | (iroff3>=20)):
            ier = 2
        if (iroff2 >= 5):
            ierro = 3
        if (last == limit-1):
            ier = 1
        if (max(abs(a1), abs(b2)) <= (0.1E+01+0.1E+03*epmach)*(abs(a2)+0.1E+04*uflow)):
            ier = 4
        if (error2 > error1):
            alist[maxerr] = a2
            alist[last] = a1
            blist[last] = b1
            rlist[maxerr] = area2
            rlist[last] = area1
            elist[maxerr] = error2
            elist[last] = error1
        else:
            alist[last] = a2
            blist[maxerr] = b1
            blist[last] = b2
            elist[maxerr] = error1
            elist[last] = error2
        maxerr, errmax, iord, nrmax = qpsrt(limit, last, maxerr, errmax, elist, iord, nrmax)
        if (errsum <= errbnd):
            result = 0.0e+00
            for k in range(last+1):
                result = result + rlist[k]
            abserr = errsum
            neval = 30*(last+1) - 15
            if (inf == 2):
                neval = 2*neval
            if (ier > 2):
                ier = ier - 1
            return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
        if (ier != 0):
            break
        if (last == 1):
            small = 0.375e+00
            erlarg = errsum
            ertest = errbnd
            rlist2[1] = area
        elif (not noext):
            erlarg = erlarg - erlast
            if (abs(b1-a1) > small):
                erlarg = erlarg + erro12
            if (not extrap):
                if (abs(blist[maxerr]-alist[maxerr]) > small):
                    continue
                extrap = True
                nrmax = 1
            if ((ierro != 3) & (erlarg > ertest)):
                id = nrmax
                jupbnd = last
                if (last > (2+limit//2)):
                    jupbnd = limit + 3 - last
                for k in range(id, jupbnd+1):
                    maxerr = iord[nrmax]
                    errmax = elist[maxerr]
                    if (abs(blist[maxerr]-alist[maxerr]) > small):
                        break
                    nrmax = nrmax + 1
                if (abs(blist[maxerr]-alist[maxerr]) > small):
                    continue
            numrl2 = numrl2 + 1
            rlist2[numrl2] = area
            numrl2, rlist2, reseps, abseps, res3la, nres = qelg(numrl2, rlist2, res3la, nres)
            ktmin = ktmin + 1
            if ((ktmin > 5) & (abserr < 0.1E-02*errsum)):
                ier = 5
            if (abseps < abserr):
                ktmin = 0
                abserr = abseps
                result = reseps
                correc = erlarg
                ertest = max(epsabs, epsrel*abs(reseps))
                if (abserr <= ertest):
                    break
            if (numrl2 == 0):
                noext = True
            if (ier == 5):
                break
            maxerr = iord[0]
            errmax = elist[maxerr]
            nrmax = 0
            extrap = False
            small = small*0.5e+00
            erlarg = errsum
    if (abserr != oflow):
        if ((ier+ierro) != 0):
            if (ierro == 3):
                abserr = abserr + correc
            if (ier == 0):
                ier = 3
            if ((result == 0.0e+00) | (area == 0.0e+00)):
                if (abserr > errsum):
                    result = 0.0
                    for k in range(last+1):
                        result = result + rlist[k]
                    abserr = errsum
                    neval = 30*(last+1) - 15
                    if (inf == 2):
                        neval = 2*neval
                    if (ier > 2):
                        ier = ier - 1
                    return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
                if (area == 0.0e+00):
                    neval = 30*(last+1) - 15
                    if (inf == 2):
                        neval = 2*neval
                    if (ier > 2):
                        ier = ier - 1
                    return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
            elif (abserr/abs(result) > errsum/abs(area)):
                result = 0.0
                for k in range(last+1):
                    result = result + rlist[k]
                abserr = errsum
                neval = 30*(last+1) - 15
                if (inf == 2):
                    neval = 2*neval
                if (ier > 2):
                    ier = ier - 1
                return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last
        if ((ksgn != -1) | max(abs(result), abs(area)) > defabs*0.1e-01):
            if ((0.1e-01 > result/area) | (result/area > 0.1e+03) | (errsum > abs(area))):
                ier = 6
    result = 0.0
    for k in range(last+1):
        result = result + rlist[k]
    abserr = errsum
    neval = 30*(last+1) - 15
    if (inf == 2):
        neval = 2*neval
    if (ier > 2):
        ier = ier - 1
    return result, abserr, neval, ier, alist, blist, rlist, elist, iord, last

