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
