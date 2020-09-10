!        -------------------- D1MACH.f90 --------------------

      FUNCTION d1mach(i)
!!!-------------------------------------------------------------------
!!!
!!! This function is  intended to replace  the old D1MACH by using F90
!!! intrinsic functions.
!!!
!!!
!!! The traditional D1MACH constants are ...
!!!
!!!
!!! -- DOUBLE-PRECISION MACHINE CONSTANTS --
!!!
!!! D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
!!!
!!! D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
!!!
!!! D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
!!!
!!! D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
!!!
!!! D1MACH( 5) = LOG10(B)
!!!
!!!-------------------------------------------------------------------
! .. Implicit None Statement ..
        IMPLICIT NONE
! ..
! .. Function Return Value ..
        DOUBLE PRECISION :: d1mach
! ..
! .. Scalar Arguments ..
        INTEGER :: i
! ..
! .. Local Scalars ..
        LOGICAL, SAVE :: qfirst_call = .TRUE.
! ..
! .. Local Arrays ..
        DOUBLE PRECISION, SAVE :: d1mach_values(5)
! ..
! .. Intrinsic Functions ..
        INTRINSIC digits, epsilon, huge, kind, log10, radix, real, tiny
! ..
! .. Executable Statements ..

        IF (i<1 .OR. i>5) THEN

          WRITE (*,'(1x,''D1MACH(I) - I out of bounds, I ='',i10)') i
          STOP ' D1MACH(I) - I out of bounds'

        END IF

        IF (qfirst_call) THEN

          d1mach_values = (/ tiny(1.0D0), huge(1.0D0), &
            real(radix(1.0D0),kind(1.0D0))**(-digits(1.0D0)), &
            epsilon(1.0D0), log10(real(radix(1.0D0),kind(1.0D0))) /)

          qfirst_call = .FALSE.

        END IF

        d1mach = d1mach_values(i)

        RETURN

      END FUNCTION d1mach 

!*==R1MACH.spg  processed by SPAG 6.72Dc at 12:07 on 30 Aug 2020
      REAL FUNCTION R1MACH(I)
      IMPLICIT NONE
!*--R1MACH4
      INTEGER I
!
!  SINGLE-PRECISION MACHINE CONSTANTS
!
!  R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
!
!  R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
!
!  R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
!
!  R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
!
!  R1MACH(5) = LOG10(B)
!
!  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
!  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
!  REMOVING THE C FROM COLUMN 1.
!
!  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), THE FIRST
!  SET OF CONSTANTS BELOW SHOULD BE APPROPRIATE.
!
!  WHERE POSSIBLE, DECIMAL, OCTAL OR HEXADECIMAL CONSTANTS ARE USED
!  TO SPECIFY THE CONSTANTS EXACTLY.  SOMETIMES THIS REQUIRES USING
!  EQUIVALENT INTEGER ARRAYS.  IF YOUR COMPILER USES HALF-WORD
!  INTEGERS BY DEFAULT (SOMETIMES CALLED INTEGER*2), YOU MAY NEED TO
!  CHANGE INTEGER TO INTEGER*4 OR OTHERWISE INSTRUCT YOUR COMPILER
!  TO USE FULL-WORD INTEGERS IN THE NEXT 5 DECLARATIONS.
!
!  COMMENTS JUST BEFORE THE END STATEMENT (LINES STARTING WITH *)
!  GIVE C SOURCE FOR R1MACH.
!
      INTEGER small(2)
      INTEGER large(2)
      INTEGER right(2)
      INTEGER diver(2)
      INTEGER log10(2)
      INTEGER CRAy1 , sc
      COMMON /D8MACH/ CRAy1
!/6S
!/7S
      SAVE small , large , right , diver , log10 , sc
!/
      REAL rmach(5)
!
      EQUIVALENCE (rmach(1),small(1))
      EQUIVALENCE (rmach(2),large(1))
      EQUIVALENCE (rmach(3),right(1))
      EQUIVALENCE (rmach(4),diver(1))
      EQUIVALENCE (rmach(5),log10(1))
!
!     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
!     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
!     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
!
!      DATA SMALL(1) /     8388608 /
!      DATA LARGE(1) /  2139095039 /
!      DATA RIGHT(1) /   864026624 /
!      DATA DIVER(1) /   872415232 /
!      DATA LOG10(1) /  1050288283 /, SC/987/
!
!     MACHINE CONSTANTS FOR AMDAHL MACHINES.
!
!      DATA SMALL(1) /    1048576 /
!      DATA LARGE(1) / 2147483647 /
!      DATA RIGHT(1) /  990904320 /
!      DATA DIVER(1) / 1007681536 /
!      DATA LOG10(1) / 1091781651 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
!
!      DATA RMACH(1) / Z400800000 /
!      DATA RMACH(2) / Z5FFFFFFFF /
!      DATA RMACH(3) / Z4E9800000 /
!      DATA RMACH(4) / Z4EA800000 /
!      DATA RMACH(5) / Z500E730E8 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.
!
!      DATA RMACH(1) / O1771000000000000 /
!      DATA RMACH(2) / O0777777777777777 /
!      DATA RMACH(3) / O1311000000000000 /
!      DATA RMACH(4) / O1301000000000000 /
!      DATA RMACH(5) / O1157163034761675 /, SC/987/
!
!     MACHINE CONSTANTS FOR FTN4 ON THE CDC 6000/7000 SERIES.
!
!      DATA RMACH(1) / 00564000000000000000B /
!      DATA RMACH(2) / 37767777777777777776B /
!      DATA RMACH(3) / 16414000000000000000B /
!      DATA RMACH(4) / 16424000000000000000B /
!      DATA RMACH(5) / 17164642023241175720B /, SC/987/
!
!     MACHINE CONSTANTS FOR FTN5 ON THE CDC 6000/7000 SERIES.
!
!      DATA RMACH(1) / O"00564000000000000000" /
!      DATA RMACH(2) / O"37767777777777777776" /
!      DATA RMACH(3) / O"16414000000000000000" /
!      DATA RMACH(4) / O"16424000000000000000" /
!      DATA RMACH(5) / O"17164642023241175720" /, SC/987/
!
!     MACHINE CONSTANTS FOR CONVEX C-1.
!
!      DATA RMACH(1) / '00800000'X /
!      DATA RMACH(2) / '7FFFFFFF'X /
!      DATA RMACH(3) / '34800000'X /
!      DATA RMACH(4) / '35000000'X /
!      DATA RMACH(5) / '3F9A209B'X /, SC/987/
!
!     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
!
!      DATA RMACH(1) / 200034000000000000000B /
!      DATA RMACH(2) / 577767777777777777776B /
!      DATA RMACH(3) / 377224000000000000000B /
!      DATA RMACH(4) / 377234000000000000000B /
!      DATA RMACH(5) / 377774642023241175720B /, SC/987/
!
!     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
!
!     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
!     STATIC RMACH(5)
!
!      DATA SMALL/20K,0/,LARGE/77777K,177777K/
!      DATA RIGHT/35420K,0/,DIVER/36020K,0/
!      DATA LOG10/40423K,42023K/, SC/987/
!
!     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7.
!
!      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
!      DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
!      DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
!      DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
!      DATA LOG10(1),LOG10(2) / '23210115, '00000377 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
!
!      DATA RMACH(1) / O402400000000 /
!      DATA RMACH(2) / O376777777777 /
!      DATA RMACH(3) / O714400000000 /
!      DATA RMACH(4) / O716400000000 /
!      DATA RMACH(5) / O776464202324 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
!     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
!
!      DATA RMACH(1) / Z00100000 /
!      DATA RMACH(2) / Z7FFFFFFF /
!      DATA RMACH(3) / Z3B100000 /
!      DATA RMACH(4) / Z3C100000 /
!      DATA RMACH(5) / Z41134413 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE INTERDATA 8/32
!     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
!
!     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
!     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
!
!      DATA RMACH(1) / Z'00100000' /
!      DATA RMACH(2) / Z'7EFFFFFF' /
!      DATA RMACH(3) / Z'3B100000' /
!      DATA RMACH(4) / Z'3C100000' /
!      DATA RMACH(5) / Z'41134413' /, SC/987/
!
!     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).
!
!      DATA RMACH(1) / "000400000000 /
!      DATA RMACH(2) / "377777777777 /
!      DATA RMACH(3) / "146400000000 /
!      DATA RMACH(4) / "147400000000 /
!      DATA RMACH(5) / "177464202324 /, SC/987/
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
!     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
!
!      DATA SMALL(1) /    8388608 /
!      DATA LARGE(1) / 2147483647 /
!      DATA RIGHT(1) /  880803840 /
!      DATA DIVER(1) /  889192448 /
!      DATA LOG10(1) / 1067065499 /, SC/987/
!
!      DATA RMACH(1) / O00040000000 /
!      DATA RMACH(2) / O17777777777 /
!      DATA RMACH(3) / O06440000000 /
!      DATA RMACH(4) / O06500000000 /
!      DATA RMACH(5) / O07746420233 /, SC/987/
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
!     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
!
!      DATA SMALL(1),SMALL(2) /   128,     0 /
!      DATA LARGE(1),LARGE(2) / 32767,    -1 /
!      DATA RIGHT(1),RIGHT(2) / 13440,     0 /
!      DATA DIVER(1),DIVER(2) / 13568,     0 /
!      DATA LOG10(1),LOG10(2) / 16282,  8347 /, SC/987/
!
!      DATA SMALL(1),SMALL(2) / O000200, O000000 /
!      DATA LARGE(1),LARGE(2) / O077777, O177777 /
!      DATA RIGHT(1),RIGHT(2) / O032200, O000000 /
!      DATA DIVER(1),DIVER(2) / O032400, O000000 /
!      DATA LOG10(1),LOG10(2) / O037632, O020233 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
!
!      DATA SMALL(1) / $00800000 /
!      DATA LARGE(1) / $7F7FFFFF /
!      DATA RIGHT(1) / $33800000 /
!      DATA DIVER(1) / $34000000 /
!      DATA LOG10(1) / $3E9A209B /, SC/987/
!
!     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
!
!      DATA RMACH(1) / O000400000000 /
!      DATA RMACH(2) / O377777777777 /
!      DATA RMACH(3) / O146400000000 /
!      DATA RMACH(4) / O147400000000 /
!      DATA RMACH(5) / O177464202324 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE VAX UNIX F77 COMPILER.
!
!      DATA SMALL(1) /       128 /
!      DATA LARGE(1) /    -32769 /
!      DATA RIGHT(1) /     13440 /
!      DATA DIVER(1) /     13568 /
!      DATA LOG10(1) / 547045274 /, SC/987/
!
!     MACHINE CONSTANTS FOR THE VAX-11 WITH
!     FORTRAN IV-PLUS COMPILER.
!
!      DATA RMACH(1) / Z00000080 /
!      DATA RMACH(2) / ZFFFF7FFF /
!      DATA RMACH(3) / Z00003480 /
!      DATA RMACH(4) / Z00003500 /
!      DATA RMACH(5) / Z209B3F9A /, SC/987/
!
!     MACHINE CONSTANTS FOR VAX/VMS VERSION 2.2.
!
!      DATA RMACH(1) /       '80'X /
!      DATA RMACH(2) / 'FFFF7FFF'X /
!      DATA RMACH(3) /     '3480'X /
!      DATA RMACH(4) /     '3500'X /
!      DATA RMACH(5) / '209B3F9A'X /, SC/987/
!
!  ***  ISSUE STOP 777 IF ALL DATA STATEMENTS ARE COMMENTED...
      IF ( sc/=987 ) THEN
!        *** CHECK FOR AUTODOUBLE ***
         small(2) = 0
         rmach(1) = 1E13
         IF ( small(2)==0 ) THEN
            rmach(1) = 1234567.
            IF ( small(1)==1234613304 ) THEN
!              *** IEEE ***
               small(1) = 8388608
               large(1) = 2139095039
               right(1) = 864026624
               diver(1) = 872415232
               log10(1) = 1050288283
            ELSEIF ( small(1)==-1271379306 ) THEN
!              *** VAX ***
               small(1) = 128
               large(1) = -32769
               right(1) = 13440
               diver(1) = 13568
               log10(1) = 547045274
            ELSEIF ( small(1)==1175639687 ) THEN
!              *** IBM MAINFRAME ***
               small(1) = 1048576
               large(1) = 2147483647
               right(1) = 990904320
               diver(1) = 1007681536
               log10(1) = 1091781651
            ELSEIF ( small(1)==1251390520 ) THEN
!              *** CONVEX C-1 ***
               small(1) = 8388608
               large(1) = 2147483647
               right(1) = 880803840
               diver(1) = 889192448
               log10(1) = 1067065499
            ELSE
!              CRAY1 = 4617762693716115456
               CRAy1 = 4617762
               CRAy1 = 1000000*CRAy1 + 693716
               CRAy1 = 1000000*CRAy1 + 115456
               IF ( small(1)/=CRAy1 ) THEN
                  WRITE (*,99001)
99001             FORMAT (/                                             &
                       &' Adjust R1MACH by uncommenting data statements'&
                      & /' appropriate for your machine.')
                  STOP 777
               ENDIF
!              *** CRAY 1, XMP, 2, AND 3 ***
!              SMALL(1) = 2306828171632181248
               small(1) = 2306828
               small(1) = 1000000*small(1) + 171632
               small(1) = 1000000*small(1) + 181248
!              LARGE(1) = 6917247552664371198
               large(1) = 6917247
               large(1) = 1000000*large(1) + 552664
               large(1) = 1000000*large(1) + 371198
!              RIGHT(1) = 4598878906987053056
               right(1) = 4598878
               right(1) = 1000000*right(1) + 906987
               right(1) = 1000000*right(1) + 053056
!              DIVER(1) = 4599160381963763712
               diver(1) = 4599160
               diver(1) = 1000000*diver(1) + 381963
               diver(1) = 1000000*diver(1) + 763712
!              LOG10(1) = 4611574008272714704
               log10(1) = 4611574
               log10(1) = 1000000*log10(1) + 008272
               log10(1) = 1000000*log10(1) + 714704
            ENDIF
!           *** AUTODOUBLED ***
         ELSEIF ( small(1)==1117925532 .AND. small(2)==-448790528 ) THEN
!              *** IEEE BIG ENDIAN ***
            small(1) = 1048576
            small(2) = 0
            large(1) = 2146435071
            large(2) = -1
            right(1) = 1017118720
            right(2) = 0
            diver(1) = 1018167296
            diver(2) = 0
            log10(1) = 1070810131
            log10(2) = 1352628735
         ELSEIF ( small(2)==1117925532 .AND. small(1)==-448790528 ) THEN
!              *** IEEE LITTLE ENDIAN ***
            small(2) = 1048576
            small(1) = 0
            large(2) = 2146435071
            large(1) = -1
            right(2) = 1017118720
            right(1) = 0
            diver(2) = 1018167296
            diver(1) = 0
            log10(2) = 1070810131
            log10(1) = 1352628735
         ELSEIF ( small(1)==-2065213935 .AND. small(2)==10752 ) THEN
!              *** VAX WITH D_FLOATING ***
            small(1) = 128
            small(2) = 0
            large(1) = -32769
            large(2) = -1
            right(1) = 9344
            right(2) = 0
            diver(1) = 9472
            diver(2) = 0
            log10(1) = 546979738
            log10(2) = -805796613
         ELSEIF ( small(1)==1267827943 .AND. small(2)==704643072 ) THEN
!              *** IBM MAINFRAME ***
            small(1) = 1048576
            small(2) = 0
            large(1) = 2147483647
            large(2) = -1
            right(1) = 856686592
            right(2) = 0
            diver(1) = 873463808
            diver(2) = 0
            log10(1) = 1091781651
            log10(2) = 1352628735
         ELSE
            WRITE (*,99002)
!/6S
!9010 FORMAT(/42H Adjust autodoubled R1MACH by getting data/
!    *42H appropriate for your machine from D1MACH.)
!9020 FORMAT(/46H Adjust R1MACH by uncommenting data statements/
!    *30H appropriate for your machine.)
!/7S
99002       FORMAT (/' Adjust autodoubled R1MACH by getting data'/      &
                   &' appropriate for your machine from D1MACH.')
            STOP 777
         ENDIF
         sc = 987
      ENDIF
!
!  ***  ISSUE STOP 776 IF ALL DATA STATEMENTS ARE OBVIOUSLY WRONG...
      IF ( rmach(4)>=1.0 ) STOP 776
!C/6S
!C     IF (I .LT. 1  .OR.  I .GT. 5)
!C    1   CALL SETERR(24HR1MACH - I OUT OF BOUNDS,24,1,2)
!C/7S
!      IF (I .LT. 1  .OR.  I .GT. 5)
!     1   CALL SETERR('R1MACH - I OUT OF BOUNDS',24,1,2)
!C/
!
      IF ( I<1 .OR. I>5 ) THEN
         WRITE (*,*) 'R1MACH(I): I =' , I , ' is out of bounds.'
         STOP
      ENDIF
      R1MACH = rmach(I)
      RETURN
!/
!
! /* C source for R1MACH -- remove the * in column 1 */
!#include <stdio.h>
!#include <float.h>
!#include <math.h>
!
!float r1mach_(long *i)
!{
!	switch(*i){
!	  case 1: return FLT_MIN;
!	  case 2: return FLT_MAX;
!	  case 3: return FLT_EPSILON/FLT_RADIX;
!	  case 4: return FLT_EPSILON;
!	  case 5: return log10(FLT_RADIX);
!	  }
!
!	fprintf(stderr, "invalid argument: r1mach(%ld)\n", *i);
!	exit(1);
!	return 0; /* for compilers that complain of missing return values */
!	}
      END


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
      REAL*8 Elist , Ermax , errmax , errmin
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


!*==QELG.spg  processed by SPAG 6.72Dc at 12:06 on 30 Aug 2020
      SUBROUTINE QELG(N,Epstab,Result,Abserr,Res3la,Nres)
      IMPLICIT NONE
!*--QELG4
!***begin prologue  qelg
!***refer to  qagie,qagoe,qagpe,qagse
!***routines called  d1mach
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
      REAL*8 Abserr , delta1 , delta2 , delta3 , D1MACH , epmach ,      &
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
      epmach = D1MACH(4)
      oflow = D1MACH(2)
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
            tol2 = MAX(ABS(e2),e1abs)*epmach
            delta3 = e1 - e0
            err3 = ABS(delta3)
            tol3 = MAX(e1abs,ABS(e0))*epmach
            IF ( err2>tol2 .OR. err3>tol3 ) THEN
               e3 = Epstab(k1)
               Epstab(k1) = e1
               delta1 = e1 - e3
               err1 = ABS(delta1)
               tol1 = MAX(e1abs,ABS(e3))*epmach
!
!           if two elements are very close to each other, omit
!           a part of the table by adjusting the value of n
!
               IF ( err1>tol1 .AND. err2>tol2 .AND. err3>tol3 ) THEN
                  ss = 0.1D+01/delta1 + 0.1D+01/delta2 - 0.1D+01/delta3
                  epsinf = ABS(ss*e1)
!
!           test to detect irregular behaviour in the table, and
!           eventually omit a part of the table adjusting the value
!           of n.
!
                  IF ( epsinf>0.1D-03 ) THEN
!
!           compute a new element and eventually adjust
!           the value of result.
!
                     res = e1 + 0.1D+01/ss
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
 200  Abserr = MAX(Abserr,0.5D+01*epmach*ABS(Result))
      END


!*==QK15I.spg  processed by SPAG 6.72Dc at 12:06 on 30 Aug 2020
      SUBROUTINE QK15I(F,Boun,Inf,A,B,Result,Abserr,Resabs,Resasc)
      IMPLICIT NONE
!*--QK15I4
!*** Start of declarations inserted by SPAG
      INTEGER Ierr
!*** End of declarations inserted by SPAG
!***begin prologue  qk15i
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a2,h2a4a2
!***keywords  15-point transformed gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the original (infinite integration range is mapped
!            onto the interval (0,1) and (a,b) is a part of (0,1).
!            it is the purpose to compute
!            i = integral of transformed integrand over (a,b),
!            j = integral of abs(transformed integrand) over (a,b).
!***description
!
!           integration rule
!           standard fortran subroutine
!           real version
!
!           parameters
!            on entry
!              f      - real
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              boun   - real
!                       finite bound of original integration
!                       range (set to zero if inf = +2)
!
!              inf    - integer
!                       if inf = -1, the original interval is
!                                   (-infinity,bound),
!                       if inf = +1, the original interval is
!                                   (bound,+infinity),
!                       if inf = +2, the original interval is
!                                   (-infinity,+infinity) and
!                       the integral is computed as the sum of two
!                       integrals, one over (-infinity,0) and one over
!                       (0,+infinity).
!
!              a      - real
!                       lower limit for integration over subrange
!                       of (0,1)
!
!              b      - real
!                       upper limit for integration over subrange
!                       of (0,1)
!
!            on return
!              result - real
!                       approximation to the integral i
!                       result is computed by applying the 15-point
!                       kronrod rule(resk) obtained by optimal addition
!                       of abscissae to the 7-point gauss rule(resg).
!
!              abserr - real
!                       estimate of the modulus of the absolute error,
!                       which should equal or exceed abs(i-result)
!
!              resabs - real
!                       approximation to the integral j
!
!              resasc - real
!                       approximation to the integral of
!                       abs((transformed integrand)-i/(b-a)) over (a,b)
!
!***references  (none)
!***routines called  d1mach
!***end prologue  qk15i
!
      REAL*8 A , absc , absc1 , absc2 , Abserr , B , Boun , centr ,     &
         & dinf , D1MACH , epmach , fc , fsum , fval1 , fval2 , fvalt , &
         & fv1 , fv2 , hlgth , Resabs , Resasc , resg , resk , reskh ,  &
         & Result , tabsc1 , tabsc2 , uflow , wg , wgk , xgk
      INTEGER Inf , j , MIN
      EXTERNAL F
!
      DIMENSION fv1(7) , fv2(7) , xgk(8) , wgk(8) , wg(8)
!
!           the abscissae and weights are supplied for the interval
!           (-1,1).  because of symmetry only the positive abscissae and
!           their corresponding weights are given.
!
!           xgk    - abscissae of the 15-point kronrod rule
!                    xgk(2), xgk(4), ... abscissae of the 7-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 7-point gauss rule
!
!           wgk    - weights of the 15-point kronrod rule
!
!           wg     - weights of the 7-point gauss rule, corresponding
!                    to the abscissae xgk(2), xgk(4), ...
!                    wg(1), wg(3), ... are set to zero.
!
      DATA xgk(1) , xgk(2) , xgk(3) , xgk(4) , xgk(5) , xgk(6) , xgk(7) &
         & , xgk(8)/0.9914553711208126D+00 , 0.9491079123427585D+00 ,   &
         & 0.8648644233597691D+00 , 0.7415311855993944D+00 ,            &
         & 0.5860872354676911D+00 , 0.4058451513773972D+00 ,            &
         & 0.2077849550078985D+00 , 0.0000000000000000D+00/
!
      DATA wgk(1) , wgk(2) , wgk(3) , wgk(4) , wgk(5) , wgk(6) , wgk(7) &
         & , wgk(8)/0.2293532201052922D-01 , 0.6309209262997855D-01 ,   &
         & 0.1047900103222502D+00 , 0.1406532597155259D+00 ,            &
         & 0.1690047266392679D+00 , 0.1903505780647854D+00 ,            &
         & 0.2044329400752989D+00 , 0.2094821410847278D+00/
!
      DATA wg(1) , wg(2) , wg(3) , wg(4) , wg(5) , wg(6) , wg(7) ,      &
         & wg(8)/0.0000000000000000D+00 , 0.1294849661688697D+00 ,      &
         & 0.0000000000000000D+00 , 0.2797053914892767D+00 ,            &
         & 0.0000000000000000D+00 , 0.3818300505051189D+00 ,            &
         & 0.0000000000000000D+00 , 0.4179591836734694D+00/
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc*  - abscissa
!           tabsc* - transformed abscissa
!           fval*  - function value
!           resg   - result of the 7-point gauss formula
!           resk   - result of the 15-point kronrod formula
!           reskh  - approximation to the mean value of the transformed
!                    integrand over (a,b), i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  qk15i
      epmach = D1MACH(4)
      uflow = D1MACH(1)
      dinf = MIN(1,Inf)
!
      centr = 0.5D+00*(A+B)
      hlgth = 0.5D+00*(B-A)
      tabsc1 = Boun + dinf*(0.1D+01-centr)/centr
      CALL F(tabsc1,fval1)
      IF ( Inf==2 ) THEN
         CALL F(-tabsc1,fvalt)
         fval1 = fval1 + fvalt
      ENDIF
      fc = (fval1/centr)/centr
!
!           compute the 15-point kronrod approximation to
!           the integral, and estimate the error.
!
      resg = wg(8)*fc
      resk = wgk(8)*fc
      Resabs = ABS(resk)
      DO j = 1 , 7
         absc = hlgth*xgk(j)
         absc1 = centr - absc
         absc2 = centr + absc
         tabsc1 = Boun + dinf*(0.1D+01-absc1)/absc1
         tabsc2 = Boun + dinf*(0.1D+01-absc2)/absc2
         CALL F(tabsc1,fval1)
         CALL F(tabsc2,fval2)
         IF ( Inf==2 ) THEN
            CALL F(-tabsc1,fvalt)
            fval1 = fval1 + fvalt
         ENDIF
         IF ( Inf==2 ) THEN
            CALL F(-tabsc2,fvalt)
            fval2 = fval2 + fvalt
         ENDIF
         fval1 = (fval1/absc1)/absc1
         fval2 = (fval2/absc2)/absc2
         fv1(j) = fval1
         fv2(j) = fval2
         fsum = fval1 + fval2
         resg = resg + wg(j)*fsum
         resk = resk + wgk(j)*fsum
         Resabs = Resabs + wgk(j)*(ABS(fval1)+ABS(fval2))
      ENDDO
      reskh = resk*0.5D+00
      Resasc = wgk(8)*ABS(fc-reskh)
      DO j = 1 , 7
         Resasc = Resasc + wgk(j)*(ABS(fv1(j)-reskh)+ABS(fv2(j)-reskh))
      ENDDO
      Result = resk*hlgth
      Resasc = Resasc*hlgth
      Resabs = Resabs*hlgth
      Abserr = ABS((resk-resg)*hlgth)
      IF ( Resasc/=0.0D+00 .AND. Abserr/=0.E0 )                         &
         & Abserr = Resasc*MIN(0.1D+01,(0.2D+03*Abserr/Resasc)          &
         & **1.5D+00)
      IF ( Resabs>uflow/(0.5D+02*epmach) )                              &
         & Abserr = MAX((epmach*0.5D+02)*Resabs,Abserr)
      END


!*==QAGIE.spg  processed by SPAG 6.72Dc at 12:05 on 30 Aug 2020
      SUBROUTINE QAGIE(F,Bound,Inf,Epsabs,Epsrel,Limit,Result,Abserr,   &
                     & Neval,Ier,Alist,Blist,Rlist,Elist,Iord,Last)
      IMPLICIT NONE
!*--QAGIE5
!*** Start of declarations inserted by SPAG
      REAL*8 F
!*** End of declarations inserted by SPAG
!***begin prologue  qagie
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1,h2a4a1
!***keywords  automatic integrator, infinite intervals,
!             general-purpose, transformation, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math & progr. div - k.u.leuven
!           de doncker,elise,appl. math & progr. div - k.u.leuven
!***purpose  the routine calculates an approximation result to a given
!            integral   i = integral of f over (bound,+infinity)
!                    or i = integral of f over (-infinity,bound)
!                    or i = integral of f over (-infinity,+infinity),
!                    hopefully satisfying following claim for accuracy
!                    abs(i-result).le.max(epsabs,epsrel*abs(i))
!***description
!
! integration over infinite intervals
! standard fortran subroutine
!
!            f      - real
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            bound  - real
!                     finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            inf    - real
!                     indicating the kind of integration range involved
!                     inf = 1 corresponds to  (bound,+infinity),
!                     inf = -1            to  (-infinity,bound),
!                     inf = 2             to (-infinity,+infinity).
!
!            epsabs - real
!                     absolute accuracy requested
!            epsrel - real
!                     relative accuracy requested
!                     if  epsabs.le.0
!                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            limit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), limit.ge.1
!
!         on return
!            result - real
!                     approximation to the integral
!
!            abserr - real
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                   - ier.gt.0 abnormal termination of the routine. the
!                             estimates for result and error are less
!                             reliable. it is assumed that the requested
!                             accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             limit (and taking the according dimension
!                             adjustments into account). however,if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties.
!                             if the position of a local difficulty can
!                             be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             result is the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs.le.0 and
!                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                             result, abserr, neval, last, rlist(1),
!                             elist(1) and iord(1) are set to zero.
!                             alist(1) and blist(1) are set to 0
!                             and 1 respectively.
!
!            alist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            blist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            rlist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real
!                     vector of dimension at least limit,  the first
!                     last elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension limit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last.le.(limit/2+2), and k = limit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced
!                     in the subdivision process
!
!***references  (none)
!***routines called  qelg,qk15i,qpsrt,d1mach
!***end prologue  qagie
!
      REAL*8 abseps , Abserr , Alist , area , area1 , area12 , area2 ,  &
         & a1 , a2 , Blist , boun , Bound , b1 , b2 , correc , defabs , &
         & defab1 , defab2 , dres , D1MACH , Elist , epmach , Epsabs ,  &
         & Epsrel , erlarg , erlast , errbnd , errmax , error1 ,        &
         & error2 , erro12 , errsum , ertest , oflow , resabs , reseps ,&
         & Result , res3la , Rlist , rlist2 , small , uflow
      INTEGER id , Ier , ierro , Inf , Iord , iroff1 , iroff2 , iroff3 ,&
            & jupbnd , k , ksgn , ktmin , Last , Limit , maxerr ,       &
            & Neval , nres , nrmax , numrl2
      LOGICAL extrap , noext
!
      DIMENSION Alist(Limit) , Blist(Limit) , Elist(Limit) , Iord(Limit)&
              & , res3la(3) , Rlist(Limit) , rlist2(52)
!
      EXTERNAL F
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine qelg.
!
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least (limexp+2),
!                       containing the part of the epsilon table
!                       wich is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements currently in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained, it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           small     - length of the smallest interval considered up
!                       to now, multiplied by 1.5
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation
!                       is no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
      epmach = D1MACH(4)
!
!           test on validity of parameters
!           -----------------------------
!
!***first executable statement  qagie
      Ier = 0
      Neval = 0
      Last = 0
      Result = 0.0D+00
      Abserr = 0.0D+00
      Alist(1) = 0.0D+00
      Blist(1) = 0.1D+01
      Rlist(1) = 0.0D+00
      Elist(1) = 0.0D+00
      Iord(1) = 0
      IF ( Epsabs<=0.0D+00 .AND. Epsrel<MAX(0.5D+02*epmach,0.5D-14) )   &
         & Ier = 6
      IF ( Ier==6 ) GOTO 99999
!
!
!           first approximation to the integral
!           -----------------------------------
!
!           determine the interval to be mapped onto (0,1).
!           if inf = 2 the integral is computed as i = i1+i2, where
!           i1 = integral of f over (-infinity,0),
!           i2 = integral of f over (0,+infinity).
!
      boun = Bound
      IF ( Inf==2 ) boun = 0.0D+00
      CALL QK15I(F,boun,Inf,0.0D+00,0.1D+01,Result,Abserr,defabs,resabs)
!
!           test on accuracy
!
      Last = 1
      Rlist(1) = Result
      Elist(1) = Abserr
      Iord(1) = 1
      dres = ABS(Result)
      errbnd = MAX(Epsabs,Epsrel*dres)
      IF ( Abserr<=1.0D+02*epmach*defabs .AND. Abserr>errbnd ) Ier = 2
      IF ( Limit==1 ) Ier = 1
      IF ( Ier/=0 .OR. (Abserr<=errbnd .AND. Abserr/=resabs) .OR.       &
         & Abserr==0.0D+00 ) GOTO 400
!
!           initialization
!           --------------
!
      uflow = D1MACH(1)
      oflow = D1MACH(2)
      rlist2(1) = Result
      errmax = Abserr
      maxerr = 1
      area = Result
      errsum = Abserr
      Abserr = oflow
      nrmax = 1
      nres = 0
      ktmin = 0
      numrl2 = 2
      extrap = .FALSE.
      noext = .FALSE.
      ierro = 0
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      IF ( dres>=(0.1D+01-0.5D+02*epmach)*defabs ) ksgn = 1
!
!           main do-loop
!           ------------
!
      DO Last = 2 , Limit
!
!           bisect the subinterval with nrmax-th largest
!           error estimate.
!
         a1 = Alist(maxerr)
         b1 = 0.5D+00*(Alist(maxerr)+Blist(maxerr))
         a2 = b1
         b2 = Blist(maxerr)
         erlast = errmax
         CALL QK15I(F,boun,Inf,a1,b1,area1,error1,resabs,defab1)
         CALL QK15I(F,boun,Inf,a2,b2,area2,error2,resabs,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
         area12 = area1 + area2
         erro12 = error1 + error2
         errsum = errsum + erro12 - errmax
         area = area + area12 - Rlist(maxerr)
         IF ( defab1/=error1 .AND. defab2/=error2 ) THEN
            IF ( ABS(Rlist(maxerr)-area12)<=0.1D-04*ABS(area12) .AND.   &
               & erro12>=0.99D+00*errmax ) THEN
               IF ( extrap ) iroff2 = iroff2 + 1
               IF ( .NOT.extrap ) iroff1 = iroff1 + 1
            ENDIF
            IF ( Last>10 .AND. erro12>errmax ) iroff3 = iroff3 + 1
         ENDIF
         Rlist(maxerr) = area1
         Rlist(Last) = area2
         errbnd = MAX(Epsabs,Epsrel*ABS(area))
!
!           test for roundoff error and eventually
!           set error flag.
!
         IF ( iroff1+iroff2>=10 .OR. iroff3>=20 ) Ier = 2
         IF ( iroff2>=5 ) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals limit.
!
         IF ( Last==Limit ) Ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at some points of the integration range.
!
         IF ( MAX(ABS(a1),ABS(b2))<=(0.1D+01+0.1D+03*epmach)            &
            & *(ABS(a2)+0.1D+04*uflow) ) Ier = 4
!
!           append the newly-created intervals to the list.
!
         IF ( error2>error1 ) THEN
            Alist(maxerr) = a2
            Alist(Last) = a1
            Blist(Last) = b1
            Rlist(maxerr) = area2
            Rlist(Last) = area1
            Elist(maxerr) = error2
            Elist(Last) = error1
         ELSE
            Alist(Last) = a2
            Blist(maxerr) = b1
            Blist(Last) = b2
            Elist(maxerr) = error1
            Elist(Last) = error2
         ENDIF
!
!           call subroutine qpsrt to maintain the descending ordering
!           in the list of error estimates and select the
!           subinterval with nrmax-th largest error estimate (to be
!           bisected next).
!
         CALL QPSRT(Limit,Last,maxerr,errmax,Elist,Iord,nrmax)
         IF ( errsum<=errbnd ) GOTO 300
         IF ( Ier/=0 ) GOTO 200
         IF ( Last==2 ) THEN
            small = 0.375D+00
            erlarg = errsum
            ertest = errbnd
            rlist2(2) = area
         ELSEIF ( .NOT.(noext) ) THEN
            erlarg = erlarg - erlast
            IF ( ABS(b1-a1)>small ) erlarg = erlarg + erro12
            IF ( .NOT.(extrap) ) THEN
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
               IF ( ABS(Blist(maxerr)-Alist(maxerr))>small ) GOTO 100
               extrap = .TRUE.
               nrmax = 2
            ENDIF
            IF ( ierro/=3 .AND. erlarg>ertest ) THEN
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors
!           over the larger intervals (erlarg) and perform
!           extrapolation.
!
               id = nrmax
               jupbnd = Last
               IF ( Last>(2+Limit/2) ) jupbnd = Limit + 3 - Last
               DO k = id , jupbnd
                  maxerr = Iord(nrmax)
                  errmax = Elist(maxerr)
                  IF ( ABS(Blist(maxerr)-Alist(maxerr))>small ) GOTO 100
                  nrmax = nrmax + 1
               ENDDO
            ENDIF
!
!           perform extrapolation.
!
            numrl2 = numrl2 + 1
            rlist2(numrl2) = area
            CALL QELG(numrl2,rlist2,reseps,abseps,res3la,nres)
            ktmin = ktmin + 1
            IF ( ktmin>5 .AND. Abserr<0.1D-02*errsum ) Ier = 5
            IF ( abseps<Abserr ) THEN
               ktmin = 0
               Abserr = abseps
               Result = reseps
               correc = erlarg
               ertest = MAX(Epsabs,Epsrel*ABS(reseps))
               IF ( Abserr<=ertest ) GOTO 200
            ENDIF
!
!            prepare bisection of the smallest interval.
!
            IF ( numrl2==1 ) noext = .TRUE.
            IF ( Ier==5 ) GOTO 200
            maxerr = Iord(1)
            errmax = Elist(maxerr)
            nrmax = 1
            extrap = .FALSE.
            small = small*0.5D+00
            erlarg = errsum
         ENDIF
 100  ENDDO
!
!           set final result and error estimate.
!           ------------------------------------
!
 200  IF ( Abserr/=oflow ) THEN
         IF ( (Ier+ierro)/=0 ) THEN
            IF ( ierro==3 ) Abserr = Abserr + correc
            IF ( Ier==0 ) Ier = 3
            IF ( Result==0.0D+00 .OR. area==0.0D+00 ) THEN
               IF ( Abserr>errsum ) GOTO 300
               IF ( area==0.0D+00 ) GOTO 400
            ELSEIF ( Abserr/ABS(Result)>errsum/ABS(area) ) THEN
               GOTO 300
            ENDIF
         ENDIF
!
!           test on divergence
!
         IF ( ksgn/=(-1) .OR. MAX(ABS(Result),ABS(area))                &
            & >defabs*0.1D-01 ) THEN
            IF ( 0.1D-01>(Result/area) .OR. (Result/area)>0.1D+03 .OR.  &
               & errsum>ABS(area) ) Ier = 6
         ENDIF
         GOTO 400
      ENDIF
!
!           compute global integral sum.
!
 300  Result = 0.0D+00
      DO k = 1 , Last
         Result = Result + Rlist(k)
      ENDDO
      Abserr = errsum
 400  Neval = 30*Last - 15
      IF ( Inf==2 ) Neval = 2*Neval
      IF ( Ier>2 ) Ier = Ier - 1
99999 END


!*==QK21.spg  processed by SPAG 6.72Dc at 08:15 on  2 Sep 2020
      SUBROUTINE QK21(F,A,B,Result,Abserr,Resabs,Resasc)
      IMPLICIT NONE
!*--QK214
!*** Start of declarations inserted by SPAG
      INTEGER Ierr
      REAL*8 Resasc
!*** End of declarations inserted by SPAG
!***begin prologue  qk21
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  21-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real version
!
!           parameters
!            on entry
!              f      - real
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the driver program.
!
!              a      - real
!                       lower limit of integration
!
!              b      - real
!                       upper limit of integration
!
!            on return
!              result - real
!                       approximation to the integral i
!                       result is computed by applying the 21-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 10-point gauss rule (resg).
!
!              abserr - real
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result)
!
!              resabs - real
!                       approximation to the integral j
!
!              resasc - real
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***routines called  d1mach
!***end prologue  qk21
!
      REAL*8 A , absc , Abserr , B , centr , dhlgth , epmach , fc ,     &
         & fsum , fval1 , fval2 , fv1 , fv2 , hlgth , Resabs , resg ,   &
         & resk , reskh , Result , D1MACH , uflow , wg , wgk , xgk
      INTEGER j , jtw , jtwm1
      EXTERNAL F
!
      DIMENSION fv1(10) , fv2(10) , wg(5) , wgk(11) , xgk(11)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 21-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 10-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 10-point gauss rule
!
!           wgk    - weights of the 21-point kronrod rule
!
!           wg     - weights of the 10-point gauss rule
!
      DATA xgk(1) , xgk(2) , xgk(3) , xgk(4) , xgk(5) , xgk(6) , xgk(7) &
         & , xgk(8) , xgk(9) , xgk(10) , xgk(11)                        &
         & /0.9956571630258081D+00 , 0.9739065285171717D+00 ,           &
         & 0.9301574913557082D+00 , 0.8650633666889845D+00 ,            &
         & 0.7808177265864169D+00 , 0.6794095682990244D+00 ,            &
         & 0.5627571346686047D+00 , 0.4333953941292472D+00 ,            &
         & 0.2943928627014602D+00 , 0.1488743389816312D+00 ,            &
         & 0.0000000000000000D+00/
!
      DATA wgk(1) , wgk(2) , wgk(3) , wgk(4) , wgk(5) , wgk(6) , wgk(7) &
         & , wgk(8) , wgk(9) , wgk(10) , wgk(11)                        &
         & /0.1169463886737187D-01 , 0.3255816230796473D-01 ,           &
         & 0.5475589657435200D-01 , 0.7503967481091995D-01 ,            &
         & 0.9312545458369761D-01 , 0.1093871588022976D+00 ,            &
         & 0.1234919762620659D+00 , 0.1347092173114733D+00 ,            &
         & 0.1427759385770601D+00 , 0.1477391049013385D+00 ,            &
         & 0.1494455540029169D+00/
!
      DATA wg(1) , wg(2) , wg(3) , wg(4) ,                              &
         & wg(5)/0.6667134430868814D-01 , 0.1494513491505806D+00 ,      &
         & 0.2190863625159820D+00 , 0.2692667193099964D+00 ,            &
         & 0.2955242247147529D+00/
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result of the 10-point gauss formula
!           resk   - result of the 21-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  qk21
      epmach = D1MACH(4)
      uflow = D1MACH(1)
!
      centr = 0.5D+00*(A+B)
      hlgth = 0.5D+00*(B-A)
      dhlgth = ABS(hlgth)
!
!           compute the 21-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      resg = 0.0D+00
      CALL F(centr,fc)
      resk = wgk(11)*fc
      Resabs = ABS(resk)
      DO j = 1 , 5
         jtw = 2*j
         absc = hlgth*xgk(jtw)
         CALL F(centr-absc,fval1)
         CALL F(centr+absc,fval2)
         fv1(jtw) = fval1
         fv2(jtw) = fval2
         fsum = fval1 + fval2
         resg = resg + wg(j)*fsum
         resk = resk + wgk(jtw)*fsum
         Resabs = Resabs + wgk(jtw)*(ABS(fval1)+ABS(fval2))
      ENDDO
      DO j = 1 , 5
         jtwm1 = 2*j - 1
         absc = hlgth*xgk(jtwm1)
         CALL F(centr-absc,fval1)
         CALL F(centr+absc,fval2)
         fv1(jtwm1) = fval1
         fv2(jtwm1) = fval2
         fsum = fval1 + fval2
         resk = resk + wgk(jtwm1)*fsum
         Resabs = Resabs + wgk(jtwm1)*(ABS(fval1)+ABS(fval2))
      ENDDO
      reskh = resk*0.5D+00
      Resasc = wgk(11)*ABS(fc-reskh)
      DO j = 1 , 10
         Resasc = Resasc + wgk(j)*(ABS(fv1(j)-reskh)+ABS(fv2(j)-reskh))
      ENDDO
      Result = resk*hlgth
      Resabs = Resabs*dhlgth
      Resasc = Resasc*dhlgth
      Abserr = ABS((resk-resg)*hlgth)
      IF ( Resasc/=0.0D+00 .AND. Abserr/=0.0D+00 )                      &
         & Abserr = Resasc*MIN(0.1D+01,(0.2D+03*Abserr/Resasc)          &
         & **1.5D+00)
      IF ( Resabs>uflow/(0.5D+02*epmach) )                              &
         & Abserr = MAX((epmach*0.5D+02)*Resabs,Abserr)
      END


!*==QAGPE.spg  processed by SPAG 6.72Dc at 08:15 on  2 Sep 2020
      SUBROUTINE QAGPE(F,A,B,Npts2,Points,Epsabs,Epsrel,Limit,Result,   &
                     & Abserr,Neval,Ier,Alist,Blist,Rlist,Elist,Pts,    &
                     & Iord,Level,Ndin,Last)
      IMPLICIT NONE
!*--QAGPE6
!*** Start of declarations inserted by SPAG
      REAL*8 F
!*** End of declarations inserted by SPAG
!***begin prologue  qagpe
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, general-purpose,
!             singularities at user specified points,
!             extrapolation, globally adaptive.
!***author  piessens,robert ,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result to a given
!            definite integral i = integral of f over (a,b),hopefully
!            satisfying following claim for accuracy abs(i-result).le.
!            max(epsabs,epsrel*abs(i)). break points of the integration
!            interval, where local difficulties of the integrand may
!            occur(e.g. singularities,discontinuities),provided by user.
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real version
!
!        parameters
!         on entry
!            f      - real
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real
!                     lower limit of integration
!
!            b      - real
!                     upper limit of integration
!
!            npts2  - integer
!                     number equal to two more than the number of
!                     user-supplied break points within the integration
!                     range, npts2.ge.2.
!                     if npts2.lt.2, the routine will end with ier = 6.
!
!            points - real
!                     vector of dimension npts2, the first (npts2-2)
!                     elements of which are the user provided break
!                     points. if these points do not constitute an
!                     ascending sequence there will be an automatic
!                     sorting.
!
!            epsabs - real
!                     absolute accuracy requested
!            epsrel - real
!                     relative accuracy requested
!                     if  epsabs.le.0
!                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            limit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), limit.ge.npts2
!                     if limit.lt.npts2, the routine will end with
!                     ier = 6.
!
!         on return
!            result - real
!                     approximation to the integral
!
!            abserr - real
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier.gt.0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             limit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (i.e. singularity,
!                             discontinuity within the interval), it
!                             should be supplied to the routine as an
!                             element of the vector points. if necessary
!                             an appropriate special-purpose integrator
!                             must be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table. it is presumed that
!                             the requested tolerance cannot be
!                             achieved, and that the returned result is
!                             the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.gt.0.
!                         = 6 the input is invalid because
!                             npts2.lt.2 or
!                             break points are specified outside
!                             the integration range or
!                             (epsabs.le.0 and
!                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
!                             or limit.lt.npts2.
!                             result, abserr, neval, last, rlist(1),
!                             and elist(1) are set to zero. alist(1) and
!                             blist(1) are set to a and b respectively.
!
!            alist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the left end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            blist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the right end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            rlist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            pts    - real
!                     vector of dimension at least npts2, containing the
!                     integration limits and the break points of the
!                     interval in ascending sequence.
!
!            level  - integer
!                     vector of dimension at least limit, containing the
!                     subdivision levels of the subinterval, i.e. if
!                     (aa,bb) is a subinterval of (p1,p2) where p1 as
!                     well as p2 is a user-provided break point or
!                     integration limit, then (aa,bb) has level l if
!                     abs(bb-aa) = abs(p2-p1)*2**(-l).
!
!            ndin   - integer
!                     vector of dimension at least npts2, after first
!                     integration over the intervals (pts(i)),pts(i+1),
!                     i = 0,1, ..., npts2-2, the error estimates over
!                     some of the intervals may have been increased
!                     artificially, in order to put their subdivision
!                     forward. if this happens for the subinterval
!                     numbered k, ndin(k) is put to 1, otherwise
!                     ndin(k) = 0.
!
!            iord   - integer
!                     vector of dimension at least limit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last.le.(limit/2+2), and k = limit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced in the
!                     subdivisions process
!
!***references  (none)
!***routines called  qelg,qk21,qpsrt,d1mach
!***end prologue  qagpe
      REAL*8 A , abseps , Abserr , Alist , area , area1 , area12 ,      &
         & area2 , a1 , a2 , B , Blist , b1 , b2 , correc , defabs ,    &
         & defab1 , defab2 , dres , D1MACH , Elist , epmach , Epsabs ,  &
         & Epsrel , erlarg , erlast , errbnd , errmax , error1 ,        &
         & erro12 , error2 , errsum , ertest , oflow , Points , Pts ,   &
         & resa , resabs , reseps , Result , res3la , Rlist , rlist2 ,  &
         & sign , temp , uflow
      INTEGER i , id , Ier , ierro , ind1 , ind2 , Iord , ip1 , iroff1 ,&
            & iroff2 , iroff3 , j , jlow , jupbnd , k , ksgn , ktmin ,  &
            & Last , levcur , Level , levmax , Limit , maxerr , Ndin ,  &
            & Neval , nint , nintp1 , npts , Npts2 , nres , nrmax ,     &
            & numrl2
      LOGICAL extrap , noext
!
!
      DIMENSION Alist(Limit) , Blist(Limit) , Elist(Limit) , Iord(Limit)&
              & , Level(Limit) , Ndin(Npts2) , Points(Npts2) ,          &
              & Pts(Npts2) , res3la(3) , Rlist(Limit) , rlist2(52)
!
      EXTERNAL F
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine epsalg (rlist2 should be of dimension
!            (limexp+2) at least).
!
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least limexp+2
!                       containing the part of the epsilon table which
!                       is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained, it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation is
!                       no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  qagpe
      epmach = D1MACH(4)
!
!            test on validity of parameters
!            -----------------------------
!
      Ier = 0
      Neval = 0
      Last = 0
      Result = 0.0D+00
      Abserr = 0.0D+00
      Alist(1) = A
      Blist(1) = B
      Rlist(1) = 0.0D+00
      Elist(1) = 0.0D+00
      Iord(1) = 0
      Level(1) = 0
      npts = Npts2 - 2
      IF ( Npts2<2 .OR. Limit<=npts .OR.                                &
         & (Epsabs<=0.0D+00 .AND. Epsrel<MAX(0.5D+02*epmach,0.5D-14))   &
         & ) Ier = 6
      IF ( Ier/=6 ) THEN
!
!            if any break points are provided, sort them into an
!            ascending sequence.
!
         sign = 1.0D+00
         IF ( A>B ) sign = -1.0D+00
         Pts(1) = MIN(A,B)
         IF ( npts/=0 ) THEN
            DO i = 1 , npts
               Pts(i+1) = Points(i)
            ENDDO
         ENDIF
         Pts(npts+2) = MAX(A,B)
         nint = npts + 1
         a1 = Pts(1)
         IF ( npts/=0 ) THEN
            nintp1 = nint + 1
            DO i = 1 , nint
               ip1 = i + 1
               DO j = ip1 , nintp1
                  IF ( Pts(i)>Pts(j) ) THEN
                     temp = Pts(i)
                     Pts(i) = Pts(j)
                     Pts(j) = temp
                  ENDIF
               ENDDO
            ENDDO
            IF ( Pts(1)/=MIN(A,B) .OR. Pts(nintp1)/=MAX(A,B) )          &
               & Ier = 6
            IF ( Ier==6 ) GOTO 99999
         ENDIF
!
!            compute first integral and error approximations.
!            ------------------------------------------------
!
         resabs = 0.0D+00
         DO i = 1 , nint
            b1 = Pts(i+1)
            CALL QK21(F,a1,b1,area1,error1,defabs,resa)
            Abserr = Abserr + error1
            Result = Result + area1
            Ndin(i) = 0
            IF ( error1==resa .AND. error1/=0.0D+00 ) Ndin(i) = 1
            resabs = resabs + defabs
            Level(i) = 0
            Elist(i) = error1
            Alist(i) = a1
            Blist(i) = b1
            Rlist(i) = area1
            Iord(i) = i
            a1 = b1
         ENDDO
         errsum = 0.0D+00
         DO i = 1 , nint
            IF ( Ndin(i)==1 ) Elist(i) = Abserr
            errsum = errsum + Elist(i)
         ENDDO
!
!           test on accuracy.
!
         Last = nint
         Neval = 21*nint
         dres = ABS(Result)
         errbnd = MAX(Epsabs,Epsrel*dres)
         IF ( Abserr<=0.1D+03*epmach*resabs .AND. Abserr>errbnd )       &
            & Ier = 2
         IF ( nint/=1 ) THEN
            DO i = 1 , npts
               jlow = i + 1
               ind1 = Iord(i)
               DO j = jlow , nint
                  ind2 = Iord(j)
                  IF ( Elist(ind1)<=Elist(ind2) ) THEN
                     ind1 = ind2
                     k = j
                  ENDIF
               ENDDO
               IF ( ind1/=Iord(i) ) THEN
                  Iord(k) = Iord(i)
                  Iord(i) = ind1
               ENDIF
            ENDDO
            IF ( Limit<Npts2 ) Ier = 1
         ENDIF
         IF ( Ier/=0 .OR. Abserr<=errbnd ) GOTO 99999
!
!           initialization
!           --------------
!
         rlist2(1) = Result
         maxerr = Iord(1)
         errmax = Elist(maxerr)
         area = Result
         nrmax = 1
         nres = 0
         numrl2 = 1
         ktmin = 0
         extrap = .FALSE.
         noext = .FALSE.
         erlarg = errsum
         ertest = errbnd
         levmax = 1
         iroff1 = 0
         iroff2 = 0
         iroff3 = 0
         ierro = 0
         uflow = D1MACH(1)
         oflow = D1MACH(2)
         Abserr = oflow
         ksgn = -1
         IF ( dres>=(0.1D+01-0.5D+02*epmach)*resabs ) ksgn = 1
!
!           main do-loop
!           ------------
!
         DO Last = Npts2 , Limit
!
!           bisect the subinterval with the nrmax-th largest
!           error estimate.
!
            levcur = Level(maxerr) + 1
            a1 = Alist(maxerr)
            b1 = 0.5D+00*(Alist(maxerr)+Blist(maxerr))
            a2 = b1
            b2 = Blist(maxerr)
            erlast = errmax
            CALL QK21(F,a1,b1,area1,error1,resa,defab1)
            CALL QK21(F,a2,b2,area2,error2,resa,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
            Neval = Neval + 42
            area12 = area1 + area2
            erro12 = error1 + error2
            errsum = errsum + erro12 - errmax
            area = area + area12 - Rlist(maxerr)
            IF ( defab1/=error1 .AND. defab2/=error2 ) THEN
               IF ( ABS(Rlist(maxerr)-area12)<=0.1D-04*ABS(area12) .AND.&
                  & erro12>=0.99D+00*errmax ) THEN
                  IF ( extrap ) iroff2 = iroff2 + 1
                  IF ( .NOT.extrap ) iroff1 = iroff1 + 1
               ENDIF
               IF ( Last>10 .AND. erro12>errmax ) iroff3 = iroff3 + 1
            ENDIF
            Level(maxerr) = levcur
            Level(Last) = levcur
            Rlist(maxerr) = area1
            Rlist(Last) = area2
            errbnd = MAX(Epsabs,Epsrel*ABS(area))
!
!           test for roundoff error and eventually
!           set error flag.
!
            IF ( iroff1+iroff2>=10 .OR. iroff3>=20 ) Ier = 2
            IF ( iroff2>=5 ) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals limit.
!
            IF ( Last==Limit ) Ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range
!
            IF ( MAX(ABS(a1),ABS(b2))<=(0.1D+01+0.1D+03*epmach)         &
               & *(ABS(a2)+0.1D+04*uflow) ) Ier = 4
!
!           append the newly-created intervals to the list.
!
            IF ( error2>error1 ) THEN
               Alist(maxerr) = a2
               Alist(Last) = a1
               Blist(Last) = b1
               Rlist(maxerr) = area2
               Rlist(Last) = area1
               Elist(maxerr) = error2
               Elist(Last) = error1
            ELSE
               Alist(Last) = a2
               Blist(maxerr) = b1
               Blist(Last) = b2
               Elist(maxerr) = error1
               Elist(Last) = error2
            ENDIF
!
!           call subroutine qpsrt to maintain the descending ordering
!           in the list of error estimates and select the
!           subinterval with nrmax-th largest error estimate (to be
!           bisected next).
!
            CALL QPSRT(Limit,Last,maxerr,errmax,Elist,Iord,nrmax)
! ***jump out of do-loop
            IF ( errsum<=errbnd ) GOTO 150
! ***jump out of do-loop
            IF ( Ier/=0 ) GOTO 100
            IF ( .NOT.(noext) ) THEN
               erlarg = erlarg - erlast
               IF ( levcur+1<=levmax ) erlarg = erlarg + erro12
               IF ( .NOT.(extrap) ) THEN
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
                  IF ( Level(maxerr)+1<=levmax ) GOTO 50
                  extrap = .TRUE.
                  nrmax = 2
               ENDIF
               IF ( ierro/=3 .AND. erlarg>ertest ) THEN
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors
!           over the larger intervals (erlarg) and perform
!           extrapolation.
!
                  id = nrmax
                  jupbnd = Last
                  IF ( Last>(2+Limit/2) ) jupbnd = Limit + 3 - Last
                  DO k = id , jupbnd
                     maxerr = Iord(nrmax)
                     errmax = Elist(maxerr)
! ***jump out of do-loop
                     IF ( Level(maxerr)+1<=levmax ) GOTO 50
                     nrmax = nrmax + 1
                  ENDDO
               ENDIF
!
!           perform extrapolation.
!
               numrl2 = numrl2 + 1
               rlist2(numrl2) = area
               IF ( numrl2>2 ) THEN
                  CALL QELG(numrl2,rlist2,reseps,abseps,res3la,nres)
                  ktmin = ktmin + 1
                  IF ( ktmin>5 .AND. Abserr<0.1D-02*errsum ) Ier = 5
                  IF ( abseps<Abserr ) THEN
                     ktmin = 0
                     Abserr = abseps
                     Result = reseps
                     correc = erlarg
                     ertest = MAX(Epsabs,Epsrel*ABS(reseps))
! ***jump out of do-loop
                     IF ( Abserr<ertest ) GOTO 100
                  ENDIF
!
!           prepare bisection of the smallest interval.
!
                  IF ( numrl2==1 ) noext = .TRUE.
                  IF ( Ier>=5 ) GOTO 100
               ENDIF
               maxerr = Iord(1)
               errmax = Elist(maxerr)
               nrmax = 1
               extrap = .FALSE.
               levmax = levmax + 1
               erlarg = errsum
            ENDIF
 50      ENDDO
!
!           set the final result.
!           ---------------------
!
!
 100     IF ( Abserr/=oflow ) THEN
            IF ( (Ier+ierro)/=0 ) THEN
               IF ( ierro==3 ) Abserr = Abserr + correc
               IF ( Ier==0 ) Ier = 3
               IF ( Result==0.0D+00 .OR. area==0.0D+00 ) THEN
                  IF ( Abserr>errsum ) GOTO 150
                  IF ( area==0.0D+00 ) GOTO 200
               ELSEIF ( Abserr/ABS(Result)>errsum/ABS(area) ) THEN
                  GOTO 150
               ENDIF
            ENDIF
!
!           test on divergence.
!
            IF ( ksgn/=(-1) .OR. MAX(ABS(Result),ABS(area))             &
               & >resabs*0.1D-01 ) THEN
               IF ( 0.1D-01>(Result/area) .OR. (Result/area)            &
                  & >0.1D+03 .OR. errsum>ABS(area) ) Ier = 6
            ENDIF
            GOTO 200
         ENDIF
!
!           compute global integral sum.
!
 150     Result = 0.0D+00
         DO k = 1 , Last
            Result = Result + Rlist(k)
         ENDDO
         Abserr = errsum
      ENDIF
 200  IF ( Ier>2 ) Ier = Ier - 1
      Result = Result*sign
99999 END

