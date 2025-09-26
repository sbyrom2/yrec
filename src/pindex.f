C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C PINDEX
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      SUBROUTINE PINDEX(JXBEG,JXEND,LSHELL,M,ID,IDM)

      PARAMETER (JSON=5000)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      COMMON/CCOUT/LSTORE,LSTATM,LSTENV,LSTMOD,LSTPHYS,LSTROT,LSCRIB,LSTCH,LPHHD
      COMMON/CCOUT1/NPENV,NPRTMOD,NPRTPT,NPOINT
      DIMENSION ID(JSON)
      SAVE

C  ID IS THE ARRAY OF THE SHELLS TO BE PRINTED OUT;
C  I.E. IF SHELLS 1,3,5,..TO BE PRINTED ID(1)=1,ID(2)=3,..
      ID(1) = 1
      IDM = 2
      IF(LSHELL) THEN
C  PRINT OUT EVERY ZONE IN H-BURNING SHELL
       IF(NPRTPT.LT.JXBEG) THEN
          IBEG = MAX(2,NPRTPT)
          IEND = INT(JXBEG/NPRTPT)*NPRTPT
          DO 10 J = IBEG,IEND,NPRTPT
             ID(IDM) = J
             IDM = IDM + 1
   10       CONTINUE
       ENDIF
       IF(IEND .EQ. JXBEG) THEN
           IXBEG = JXBEG + 1
       ELSE
           IXBEG = JXBEG
       END IF
       DO 20 J = IXBEG,JXEND
          ID(IDM) = J
          IDM = IDM + 1
   20    CONTINUE
       IF(NPRTPT.LT.M) THEN
          IBEG = INT(JXEND/NPRTPT+1)*NPRTPT
          IEND = INT(M/NPRTPT)*NPRTPT
          DO 30 J = IBEG,IEND,NPRTPT
             ID(IDM) = J
             IDM = IDM + 1
   30       CONTINUE
       ENDIF
      ELSE IF(NPRTPT.LT.M) THEN
C  GENERAL CASE; PRINT OUT EVERY NPRTPT POINTS.
       IBEG = MAX(2,NPRTPT)
       IEND = INT(M/NPRTPT)*NPRTPT
       IF (IEND .EQ. M) IEND = IEND - NPRTPT
       DO 40 J = IBEG,IEND,NPRTPT
          ID(IDM) = J
          IDM = IDM + 1
   40    CONTINUE
      ENDIF
      ID(IDM) = M
      RETURN
      END
