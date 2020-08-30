!*==QELG.spg  processed by SPAG 6.72Dc at 12:06 on 30 Aug 2020
      SUBROUTINE QELG(N,Epstab,Result,Abserr,Res3la,Nres)
      IMPLICIT NONE
!*--QELG4
!***begin prologue  qelg
!***refer to  qagie,qagoe,qagpe,qagse
!***routines called  r1mach
!***revision date  830518   (yymmdd)
!***keywords  epsilon algorithm, convergence acceleration,
!             extrapolation
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math & progr. div. - k.u.leuven
!***purpose  the routine determines the limit of a given sequence of
!            approximations, by means of the epsilon algorithm of
!            p. wynn. an estimate of the absolute error is also given.
!            the condensed epsilon table is computed. only those
!            elements needed for the computation of the next diagonal
!            are preserved.
!***description
!
!           epsilon algorithm
!           standard fortran subroutine
!           real version
!
!           parameters
!              n      - integer
!                       epstab(n) contains the new element in the
!                       first column of the epsilon table.
!
!              epstab - real
!                       vector of dimension 52 containing the elements
!                       of the two lower diagonals of the triangular
!                       epsilon table. the elements are numbered
!                       starting at the right-hand corner of the
!                       triangle.
!
!              result - real
!                       resulting approximation to the integral
!
!              abserr - real
!                       estimate of the absolute error computed from
!                       result and the 3 previous results
!
!              res3la - real
!                       vector of dimension 3 containing the last 3
!                       results
!
!              nres   - integer
!                       number of calls to the routine
!                       (should be zero at first call)
!
!***end prologue  qelg
!
      REAL Abserr , delta1 , delta2 , delta3 , R1MACH , epmach ,        &
         & epsinf , Epstab , error , err1 , err2 , err3 , e0 , e1 ,     &
         & e1abs , e2 , e3 , oflow , res , Result , Res3la , ss , tol1 ,&
         & tol2 , tol3
      INTEGER i , ib , ib2 , ie , indx , k1 , k2 , k3 , limexp , N ,    &
            & newelm , Nres , num
      DIMENSION Epstab(52) , Res3la(3)
!
!           list of major variables
!           -----------------------
!
!           e0     - the 4 elements on which the
!           e1       computation of a new element in
!           e2       the epsilon table is based
!           e3                 e0
!                        e3    e1    new
!                              e2
!           newelm - number of elements to be computed in the new
!                    diagonal
!           error  - error = abs(e1-e0)+abs(e2-e1)+abs(new-e2)
!           result - the element in the new diagonal with least value
!                    of error
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           oflow is the largest positive magnitude.
!           limexp is the maximum number of elements the epsilon
!           table can contain. if this number is reached, the upper
!           diagonal of the epsilon table is deleted.
!
!***first executable statement  qelg
      epmach = R1MACH(4)
      oflow = R1MACH(2)
      Nres = Nres + 1
      Abserr = oflow
      Result = Epstab(N)
      IF ( N>=3 ) THEN
         limexp = 50
         Epstab(N+2) = Epstab(N)
         newelm = (N-1)/2
         Epstab(N) = oflow
         num = N
         k1 = N
         DO i = 1 , newelm
            k2 = k1 - 1
            k3 = k1 - 2
            res = Epstab(k1+2)
            e0 = Epstab(k3)
            e1 = Epstab(k2)
            e2 = res
            e1abs = ABS(e1)
            delta2 = e2 - e1
            err2 = ABS(delta2)
            tol2 = AMAX1(ABS(e2),e1abs)*epmach
            delta3 = e1 - e0
            err3 = ABS(delta3)
            tol3 = AMAX1(e1abs,ABS(e0))*epmach
            IF ( err2>tol2 .OR. err3>tol3 ) THEN
               e3 = Epstab(k1)
               Epstab(k1) = e1
               delta1 = e1 - e3
               err1 = ABS(delta1)
               tol1 = AMAX1(e1abs,ABS(e3))*epmach
!
!           if two elements are very close to each other, omit
!           a part of the table by adjusting the value of n
!
               IF ( err1>tol1 .AND. err2>tol2 .AND. err3>tol3 ) THEN
                  ss = 0.1E+01/delta1 + 0.1E+01/delta2 - 0.1E+01/delta3
                  epsinf = ABS(ss*e1)
!
!           test to detect irregular behaviour in the table, and
!           eventually omit a part of the table adjusting the value
!           of n.
!
                  IF ( epsinf>0.1E-03 ) THEN
!
!           compute a new element and eventually adjust
!           the value of result.
!
                     res = e1 + 0.1E+01/ss
                     Epstab(k1) = res
                     k1 = k1 - 2
                     error = err2 + ABS(res-e2) + err3
                     IF ( error<=Abserr ) THEN
                        Abserr = error
                        Result = res
                     ENDIF
                     GOTO 50
                  ENDIF
               ENDIF
               N = i + i - 1
! ***jump out of do-loop
               GOTO 100
            ELSE
!
!           if e0, e1 and e2 are equal to within machine
!           accuracy, convergence is assumed.
!           result = e2
!           abserr = abs(e1-e0)+abs(e2-e1)
!
               Result = res
               Abserr = err2 + err3
! ***jump out of do-loop
               GOTO 200
            ENDIF
 50      ENDDO
!
!           shift the table.
!
 100     IF ( N==limexp ) N = 2*(limexp/2) - 1
         ib = 1
         IF ( (num/2)*2==num ) ib = 2
         ie = newelm + 1
         DO i = 1 , ie
            ib2 = ib + 2
            Epstab(ib) = Epstab(ib2)
            ib = ib2
         ENDDO
         IF ( num/=N ) THEN
            indx = num - N + 1
            DO i = 1 , N
               Epstab(i) = Epstab(indx)
               indx = indx + 1
            ENDDO
         ENDIF
         IF ( Nres>=4 ) THEN
!
!           compute error estimate
!
            Abserr = ABS(Result-Res3la(3)) + ABS(Result-Res3la(2))      &
                   & + ABS(Result-Res3la(1))
            Res3la(1) = Res3la(2)
            Res3la(2) = Res3la(3)
            Res3la(3) = Result
         ELSE
            Res3la(Nres) = Result
            Abserr = oflow
         ENDIF
      ENDIF
 200  Abserr = AMAX1(Abserr,0.5E+01*epmach*ABS(Result))
      END
