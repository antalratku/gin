!*==QPSRT.spg  processed by SPAG 6.72Dc at 12:06 on 30 Aug 2020
      SUBROUTINE QPSRT(Limit,Last,Maxerr,Ermax,Elist,Iord,Nrmax)
      IMPLICIT NONE
!*--QPSRT4
!***begin prologue  qpsrt
!***refer to  qage,qagie,qagpe,qagse,qawce,qawse,qawoe
!***routines called  (none)
!***keywords  sequential sorting
!***description
!
! 1.        qpsrt
!           ordering routine
!              standard fortran subroutine
!              real version
!
! 2.        purpose
!              this routine maintains the descending ordering
!              in the list of the local error estimates resulting from
!              the interval subdivision process. at each call two error
!              estimates are inserted using the sequential search
!              method, top-down for the largest error estimate
!              and bottom-up for the smallest error estimate.
!
! 3.        calling sequence
!              call qpsrt(limit,last,maxerr,ermax,elist,iord,nrmax)
!
!           parameters (meaning at output)
!              limit  - integer
!                       maximum number of error estimates the list
!                       can contain
!
!              last   - integer
!                       number of error estimates currently
!                       in the list
!
!              maxerr - integer
!                       maxerr points to the nrmax-th largest error
!                       estimate currently in the list
!
!              ermax  - real
!                       nrmax-th largest error estimate
!                       ermax = elist(maxerr)
!
!              elist  - real
!                       vector of dimension last containing
!                       the error estimates
!
!              iord   - integer
!                       vector of dimension last, the first k
!                       elements of which contain pointers
!                       to the error estimates, such that
!                       elist(iord(1)),... , elist(iord(k))
!                       form a decreasing sequence, with
!                       k = last if last.le.(limit/2+2), and
!                       k = limit+1-last otherwise
!
!              nrmax  - integer
!                       maxerr = iord(nrmax)
!
! 4.        no subroutines or functions needed
!***end prologue  qpsrt
!
      REAL Elist , Ermax , errmax , errmin
      INTEGER i , ibeg , ido , Iord , isucc , j , jbnd , jupbn , k ,    &
            & Last , Limit , Maxerr , Nrmax
      DIMENSION Elist(Last) , Iord(Last)
!
!           check whether the list contains more than
!           two error estimates.
!
!***first executable statement  qpsrt
      IF ( Last>2 ) THEN
!
!           this part of the routine is only executed
!           if, due to a difficult integrand, subdivision
!           increased the error estimate. in the normal case
!           the insert procedure should start after the
!           nrmax-th largest error estimate.
!
         errmax = Elist(Maxerr)
         IF ( Nrmax/=1 ) THEN
            ido = Nrmax - 1
            DO i = 1 , ido
               isucc = Iord(Nrmax-1)
! ***jump out of do-loop
               IF ( errmax<=Elist(isucc) ) GOTO 50
               Iord(Nrmax) = isucc
               Nrmax = Nrmax - 1
            ENDDO
         ENDIF
!
!           compute the number of elements in the list to
!           be maintained in descending order. this number
!           depends on the number of subdivisions still
!           allowed.
!
 50      jupbn = Last
         IF ( Last>(Limit/2+2) ) jupbn = Limit + 3 - Last
         errmin = Elist(Last)
!
!           insert errmax by traversing the list top-down,
!           starting comparison from the element elist(iord(nrmax+1)).
!
         jbnd = jupbn - 1
         ibeg = Nrmax + 1
         IF ( ibeg<=jbnd ) THEN
            DO i = ibeg , jbnd
               isucc = Iord(i)
! ***jump out of do-loop
               IF ( errmax>=Elist(isucc) ) GOTO 100
               Iord(i-1) = isucc
            ENDDO
         ENDIF
         Iord(jbnd) = Maxerr
         Iord(jupbn) = Last
      ELSE
         Iord(1) = 1
         Iord(2) = 2
      ENDIF
      GOTO 300
!
!           insert errmin by traversing the list bottom-up.
!
 100  Iord(i-1) = Maxerr
      k = jbnd
      DO j = i , jbnd
         isucc = Iord(k)
! ***jump out of do-loop
         IF ( errmin<Elist(isucc) ) GOTO 200
         Iord(k+1) = isucc
         k = k - 1
      ENDDO
      Iord(i) = Last
      GOTO 300
 200  Iord(k+1) = Last
!
!           set maxerr and ermax.
!
 300  Maxerr = Iord(Nrmax)
      Ermax = Elist(Maxerr)
      END
