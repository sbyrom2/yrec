C
C**********************************************************************
      SUBROUTINE READCO
      IMPLICIT REAL*8 (A-H,O-Z)
C..... THE PURPOSE OF THIS SUBROUTINE IS TO READ THE DATA TABLES
      SAVE
      PARAMETER (MX=5,MV=10,NR=77,NT=56)
C YCK >>>  2/95 OPAL EOS
      LOGICAL*4 LOPALE,lopale01,lopale06,LNumDeriv
      COMMON/LUOUT/ILAST,IDEBUG,ITRACK,ISHORT,IMILNE,IMODPT,ISTOR,IOWR
C MHP 8/25 Remove unused variables
C      CHARACTER*256 FOPALE,fopale01,fopale06
C KC 2025-05-30 reordered common block elements
C       COMMON/OPALEOS/FOPALE,LOPALE,IOPALE,fopale01,lopale01,fopale06,
C      x     lopale06,LNumDeriv
C MHP 8/25 Remove file names from common blocks
      COMMON/OPALEOS/LOPALE,IOPALE,lopale01,lopale06,lNumDeriv
C <<< YCK
      REAL*8 MOLES
      CHARACTER*1 BLANK
      COMMON/AA/ Q(4),H(4),XXH
      COMMON/A/  XZ(MX,MV,NT,NR),
     .T6LIST(NR,NT),RHO(NR),T6A(NT),ESK(NT,NR),ESK2(NT,NR),DFSX(MX),
     . DFS(NT),DFSR(NR),XA(MX),M,MF
      COMMON/B/ ZZ(MX),IRI(10),INDEX(10),NTA(NR)
      COMMON/E/ ESACT,EOS(MV)
      COMMON/EE/ EPL(MX,NT,NR),XX(MX)
      COMMON/EEE/MOLES(MX),XIN(MX),TMASS(MX), RHOGR(MX,NR),
     X FRAC(MX,6),ALOGR(NR,NT),ICYCUSE(MX,NR)
      COMMON/RMPOPEOS/RMX(NT),KRA(NT),KT
      DATA (KRA(I),I=1,NT)/7*77,2*76,2*74,2*72,2*70,68,67,66,65,64,
     X 63,61,60,59,58,57,55,54,53,52,51,2*49,48,2*47,46,2*45,
     X 15*44,2*37/

      BLANK=' '

        IF (ITIMECO .NE. 12345678) THEN
        DO I=1,MX
          DO J=1,MV
            DO K=1,NT
              DO L=1,NR
                XZ(I,J,K,L)=1.D+35
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        ITIMECO=12345678
        ENDIF

C..... READ  TABLES
C MHP 8/25 Moved opening of file to parmin
      DO 3 M=1,MX

      READ (IOPALE,'(3X,F6.4,3X,F6.4,11X,F10.7,17X,F10.7)')
     X  XIN(M),ZZ(M),MOLES(M),TMASS(M)
      READ (IOPALE,'(21X,E14.7,4X,E14.7,3X,E11.4,3X,E11.4,3X,E11.4,
     X 4X,E11.4)') (FRAC(M,I),I=1,6)
      READ (IOPALE,'(A)') BLANK
      DO 2 JCS=1,NR
      READ (IOPALE,'(2I5,2F12.7,17X,E15.7)') NUMTOT,ICYCUSE(M,JCS)
     X  ,DUM,DUM,RHOGR(M,JCS)
      IF(NUMTOT .NE. JCS) THEN
         WRITE(ISHORT,'(" DATA FILE INCORRECT: NUMTOT,JCS= ",2I5)')
     X     NUMTOT,JCS
         STOP
      ENDIF
      READ(IOPALE,'(A)') BLANK
      READ(IOPALE,'(A)') BLANK
      IF (ICYCUSE(M,JCS) .LT. NTA(JCS)) THEN
         WRITE(ISHORT,'("PROBLEM WITH DATA FILES: X=",F6.4," DENSITY=",
     X      E14.4)') XIN(M), RHOGR(M,JCS)
         STOP
      ENDIF
      DO  I=1,ICYCUSE(M,JCS)
      IF (I .GT. NTA(JCS)) THEN
         READ (IOPALE,'(A)') BLANK
         GO TO 4
      ENDIF
      READ (IOPALE,'(F8.4,1X,F6.2,3E13.5,E11.3,6F8.4)')
     X T6LIST(JCS,I),ALOGR(JCS,I),(XZ(M,INDEX(IV),I,JCS),IV=1,10)
    4    CONTINUE
      ENDDO
      READ(IOPALE,'(A)') BLANK
      READ(IOPALE,'(A)') BLANK
      READ(IOPALE,'(A)') BLANK
    2 CONTINUE
      READ(IOPALE,'(A)') BLANK
    3 CONTINUE

      DO 11 I=1,NT
        IF(T6LIST(1,I) .EQ. 0.0D0) THEN
          NTUSE=I
          GO TO 14
        ENDIF
C KC 2025-05-30 fixed "DO termination statement which is not END DO or CONTINUE"
C    11 T6A(I)=T6LIST(1,I)
        T6A(I)=T6LIST(1,I)
   11 CONTINUE
   14 DO 12 I=2,NT
C KC 2025-05-30 fixed "DO termination statement which is not END DO or CONTINUE"
C    12 DFS(I)=1.D0/(T6A(I)-T6A(I-1))
         DFS(I)=1.D0/(T6A(I)-T6A(I-1))
   12 CONTINUE
      RHO(1)=RHOGR(1,1)
      DO 13 I=2,NR
C KC 2025-05-30 fixed "DO termination statement which is not END DO or CONTINUE"
C       RHO(I)=RHOGR(1,I)
C    13 DFSR(I)=1.D0/(RHO(I)-RHO(I-1))
         RHO(I)=RHOGR(1,I)
         DFSR(I)=1.D0/(RHO(I)-RHO(I-1))
   13 CONTINUE
      DO I=2,MX
      DFSX(I)=1.D0/(XX(I)-XX(I-1))
      ENDDO
C
      CLOSE (IOPALE)
C  MHP 8/98 ADDED RAMP BETWEEN OPAL AND OTHER EOS
C  NEED EDGE OF TABLE AT HIGH RHO, FIXED T.
      KT = 1
      DO I = 1, NT
         RMX(I) = RHO(KRA(I))
      END DO
      RETURN
      END
