C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C PUTSTORE - Write out a verbose stellar model to the .store file. This program
C            is mostly a clone of wrtlst.f and putmodel2.f, combined.
C
C G Somers  11/14
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C 06
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      SUBROUTINE PUTSTORE(HCOMP,HD,HL,HP,HR,HS,HT,LC,TRIT,TRIL,PS,
     *TS,RS,CFENV,FTRI,TLUMX,JCORE,JENV,MODEL,M,SMASS,TEFFL,BL,HSTOT,
     *DAGE,DDAGE,OMEGA,HS1,ETA2,R0,FP,FT,HJM,HI)

C  PUTSTORE PUTS THE MOST RECENT VERBOSE OUTPUT FILE INTO THE .STORE FILE,
C  EITHER AT SPECIFIED AGES, EVERY NPRTMOD MODELS, OR AT THE END OF RUNS.

C     WRITE MODEL OUT IN ASCII FORMAT
      PARAMETER (JSON=5000)
      PARAMETER (NTS=63, NPS=76)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      CHARACTER*6 EOS
c     CHARACTER*4 ATM, LOK, HIK, COMPMIX
c MHP 4/25 changed LOK name to make it unique, used elsewhere
      CHARACTER*4 ATM, ALOK, HIK, COMPMIX
C MHP 8/25 Removed unused variables
C      CHARACTER*256 FLAOL, FPUREZ
C      CHARACTER*256 FOPALE,FOPALE01,FOPALE06  ! FcondOpacP

      COMMON/LUOUT/ILAST,IDEBUG,ITRACK,ISHORT,IMILNE,IMODPT,ISTOR,IOWR
      COMMON/CONST3/CDELRL,CMIXL,CMIXL2,CMIXL3,CLNDP,CSECYR
      COMMON/FLAG/LEXCOM
      COMMON/HEFLSH/LKUTHE
      COMMON/ROT/WNEW,WALPCZ,ACFPFT,ITFP1,ITFP2,LROT,LINSTB,LWNEW
      DIMENSION HCOMP(15,JSON),HD(JSON),HL(JSON),HP(JSON),HR(JSON),
     * HS(JSON),HT(JSON),LC(JSON),TRIT(3),TRIL(3),PS(3),TS(3),RS(3),
     * CFENV(9),TLUMX(8),OMEGA(JSON),FP(JSON),FT(JSON),ETA2(JSON),R0(JSON),
     * HJM(JSON),HI(JSON),ID(JSON),HS1(JSON)
C llp  3/19/03 Add COMMON block /I2O/ for info directly transferred from
C      input to output model - starting with a code for th initial model
C      compostion (COMPMIX)
      COMMON /I2O/ COMPMIX

C llp 3/19/03 Add required COMMON blocks such that header flags
C     ATM, EOS, HIK and LOK can be determined.
      COMMON/ATMOS/HRAS,KTTAU,KTTAU0,LTTAU
C MHP 8/17 ADDED EXCEN, C_2 TO COMMON BLOCK FOR MATT ET AL. 2012 CENT. TERM
      COMMON/CWIND/WMAX,EXMD,EXW,EXTAU,EXR,EXM,EXL,EXPR,CONSTFACTOR,
     *             STRUCTFACTOR,EXCEN,C_2,LJDOT0
C      COMMON/CWIND/WMAX,EXMD,EXW,EXTAU,EXR,EXM,CONSTFACTOR,STRUCTFACTOR,LJDOT0
      COMMON/DEBHU/CDH,ETADH0,ETADH1,ZDH(18),XXDH,
     1             YYDH,ZZDH,DHNUE(18),LDH
      COMMON/DISK/SAGE,TDISK,PDISK,LDISK
      COMMON/DPMIX/DPENV,ALPHAC,ALPHAE,ALPHAM,BETAC,IOV1,IOV2,
     *      IOVIM, LOVSTC, LOVSTE, LOVSTM, LSEMIC, LADOV, LOVMAX
c      COMMON/DPMIX/DPENV,ALPHAC,ALPHAE,ALPHAM,IOV1,IOV2,
c     *      IOVIM, LOVSTC, LOVSTE, LOVSTM, LSEMIC, LADOV
      COMMON/GRAVST/GRTOL,ILAMBDA,NITER_GS,LDIFY
      COMMON/GRAVS3/FGRY,FGRZ,LTHOUL,LDIFZ
C OPACITY COMMON BLOCKS - modified 3/09
      COMMON /NEWOPAC/ZLAOL1,ZLAOL2,ZOPAL1,ZOPAL2, ZOPAL951,
     +       ZALEX1, ZKUR1, ZKUR2,TMOLMIN,TMOLMAX,LALEX06,
     +       LLAOL89,LOPAL92,LOPAL95,LKUR90,LALEX95,L2Z
C MHP 8/25 Removed character file names from common block
      COMMON/NWLAOL/OLAOL(12,104,52),OXA(12),OT(52),ORHO(104),TOLLAOL,
     *  IOLAOL, NUMOFXYZ, NUMRHO, NUMT, LLAOL, LPUREZ, IOPUREZ
C KC 2025-05-30 reordered common block elements
C       COMMON/OPALEOS/FOPALE,LOPALE,IOPALE,FOPALE01,LOPALE01,
C      x  FOPALE06,LOPALE06,LNumDeriv
C MHP 8/25 Remove file names from common blocks
      COMMON/OPALEOS/LOPALE,IOPALE,lopale01,lopale06,lNumDeriv
      COMMON/SCVEOS/TLOGX(NTS),TABLEX(NTS,NPS,12),
     *TABLEY(NTS,NPS,12),SMIX(NTS,NPS),TABLEZ(NTS,NPS,13),
     *TABLENV(NTS,NPS,12),NPTSX(NTS),LSCV,IDTT,IDP

C INCLUDE COMMON BLOCKS WITH REQUIRED PHYSICS AND OUTPUT FLAGS
      COMMON/CCOUT/LSTORE,LSTATM,LSTENV,LSTMOD,LSTPHYS,LSTROT,LSCRIB,LSTCH,LPHHD
      COMMON/CCOUT1/NPENV,NPRTMOD,NPRTPT,NPOINT
      COMMON/CONST/CLSUN,CLSUNL,CLNSUN,CMSUN,CMSUNL,CRSUN,CRSUNL,CMBOL
      COMMON/CONST1/ CLN,CLNI,C4PI,C4PIL,C4PI3L,CC13,CC23,CPI
      COMMON/CONST2/CGAS,CA3,CA3L,CSIG,CSIGL,CGL,CMKH,CMKHN
      COMMON/SCRTCH/SESUM(JSON),SEG(7,JSON),SBETA(JSON),SETA(JSON),
     *LOCONS(JSON),SO(JSON),SDEL(3,JSON),SFXION(3,JSON),SVEL(JSON),SCP(JSON)
      COMMON/SOUND/GAM1(JSON),LSOUND
      COMMON/TEMP2/VES(JSON),VES0(JSON),VSS(JSON),VSS0(JSON),
     *     HLE(JSON),VGSF(JSON),VGSF0(JSON),VMU(JSON)
      COMMON/ROTEN/DEROT(JSON)

      SAVE

C physics flags:
C Determine atmosphere flag, ATM
      IF (KTTAU .EQ. 0) THEN
         ATM='EDD '
      ELSEIF (KTTAU .EQ. 1) THEN
         ATM='KS  '
      ELSEIF (KTTAU .EQ. 2) THEN
         ATM='HRA '
      ELSEIF (KTTAU .EQ. 3) THEN
         ATM='KUR '
      ELSEIF (KTTAU .EQ. 4) THEN
         ATM='ALL '
      ENDIF
C Determine equation of state flag, EOS
      EOS='SAHA  '
      IF (LDH) EOS='SAH+DH'
      IF (LSCV) THEN
         EOS='SCV   '
         IF (LOPALE) EOS='SCV+OP'
         IF (LOPALE01) EOS='SCV+O1'
         IF (LDH) THEN
         IF (LOPALE06) EOS='SCV+O6'
            EOS='SCV+DH'
            IF (LOPALE) EOS='SCVDHO'
            IF (LOPALE01) EOS='SCDHO1'
            IF (LOPALE06) EOS='SCDHO6'
         ENDIF
      ELSE
         IF (LOPALE) THEN
            EOS='OPAL  '
            IF (LDH) EOS='OPA+DH'
         ENDIF
         IF (LOPALE01) THEN
            EOS='OPAL01'
            IF (LDH) EOS='OP1+DH'
         ENDIF
         IF (LOPALE06) THEN
            EOS='OPAL06'
            IF (LDH) EOS='OP6+DH'
         ENDIF
      ENDIF
C Determine low temperature opacities flag, LOK
      ALOK='NONE'
      IF (LALEX95) ALOK='ALEX'
      IF (LKUR90) ALOK='KURZ'
C Determine high temperature opacities flag, HIK
      HIK='NONE'
      IF (LOPAL95) HIK='OP95'
      IF (LOPAL92) HIK='OP92'
      IF (LLAOL89) HIK='LL89'

      IF(ATM .EQ. ' ? ') THEN
         WRITE(ISHORT,7)
  7      FORMAT('*** YREC7 input file, flags, etc., have been ',
     *          'defaulted.  ***')
      ENDIF

C 09/25 JvS: Add secondary format option that prints stitched interior and envelope 
C points. Output is either in the old format or new format, not both. 
      IF(LSTCH)THEN
          IF(LPHHD)THEN
            WRITE(ISTOR,1013) ! header key
          ! write model physics header. Should only happen upon first model output.
          ! write physics flags:
            WRITE(ISTOR,29) JCORE,JENV,CMIXL,EOS,ATM,ALOK,HIK,LPUREZ,
     &     COMPMIX,LEXCOM,LDIFY,LDIFZ,LSEMIC,LOVSTC,LOVSTE,LOVSTM,
     &     LROT,LINSTB,LJDOT0,LDISK,TDISK,PDISK,WMAX,LSTORE,LSTATM,LSTENV,
     &     LSTMOD,LSTPHYS,LSTROT
   29      FORMAT('#',2I8,F16.10,1X,A6,1X,3(A4,1X),L1,1X,A4,1X,11(L1,1X),
     &     3(1PE18.10),1X,6(L1,1X))
           WRITE(ISTOR,1014) ! profile header
           LPHHD = .FALSE. ! Turn off the physics header fo the rest of the run.
          ENDIF
        CALL STITCH(HCOMP,HR,HP,HD,HS,HT,HL,HS1,OMEGA,ETA2,
     * HI,R0,HJM,FP,FT,TEFFL,HSTOT,BL,M,LC,MODEL)
 1013     FORMAT('# JCORE  JENV  CMIXL  EOS  ATM  ALOK HIK  LPUREZ  COMPMIX',
     1 '  LEXCOM  LDIFY  LDIFZ  LSEMIC  LOVSTC  LOVSTE  LOVSTM',
     1 '  LROT  LINSTB  LJDOT0  LDISK  TDISK  PDISK  WMAX  LSTORE',
     1 '  LSTATM  LSTENV  LSTMOD  LSTPHYS  LSTROT')
 1014     FORMAT(
     1'MODEL SHELL MASS RADIUS LUMINOSITY PRESSURE TEMPERATURE DENSITY OMEGA ',
     1'CONVECTIVE INTERIOR_PT H1 He4 METALS He3 C12 C13 N14 N15 O16 O17 O18 H2 Li6 Li7 ',
     1'Be9 OPACITY GRAV DELR DEL DELAD V_CONV GAM1 HII HEII HEIII BETA ',
     1'ETA PPI PPII PPIII CNO TRIPLE_ALPHA E_NUC E_NEU E_GRAV CP DLNRHODLNT A RP/RE FP ',
     1'FT J/M MOMENT DEL_KE V_ES V_GSF V_SS VTOT ')

       ELSE
C write header records
      IF(DAGE .lt. 1d3) THEN
         WRITE(ISTOR,10) 'MOD2 ',MODEL,M,SMASS,TEFFL,BL,HSTOT,DAGE,
     *      DDAGE,HS(1),HS(M)
 10      FORMAT(A5,2I8,5F16.11,1PE18.10,0P2F16.12)
      ELSE IF (DAGE .LT. 1D4) THEN
         WRITE(ISTOR,11) 'MOD2 ',MODEL,M,SMASS,TEFFL,BL,HSTOT,DAGE,
     *      DDAGE,HS(1),HS(M)
 11      FORMAT(A5,2I8,4F16.12,F16.10,1PE18.10,0P2F16.12)
      ELSE IF (DAGE .LT. 1D5) THEN
         WRITE(ISTOR,12) 'MOD2 ',MODEL,M,SMASS,TEFFL,BL,HSTOT,DAGE,
     *      DDAGE,HS(1),HS(M)
 12      FORMAT(A5,2I8,4F16.12,F16.9,1PE18.10,0P2F16.12)
      ELSE
         WRITE(ISTOR,13) 'MOD2 ',MODEL,M,SMASS,TEFFL,BL,HSTOT,DAGE,
     *      DDAGE,HS(1),HS(M)
 13      FORMAT(A5,2I8,4F16.12,F16.8,1PE18.10,0P2F16.12)
      ENDIF

C write physics flags:
      WRITE(ISTOR,30) JCORE,JENV,CMIXL,EOS,ATM,ALOK,HIK,LPUREZ,
     &     COMPMIX,LEXCOM,LDIFY,LDIFZ,LSEMIC,LOVSTC,LOVSTE,LOVSTM,
     &     LROT,LINSTB,LJDOT0,LDISK,TDISK,PDISK,WMAX,LSTORE,LSTATM,LSTENV,
     &   LSTMOD,LSTPHYS,LSTROT
   30 FORMAT(2I8,F16.10,1X,A6,1X,3(A4,1X),L1,1X,A4,1X,11(L1,1X),
     &     3(1PE18.10),1X,6(L1,1X))

C write luminosities and output flags
C If TLUMX are in solar units, convert to ergs.  Decide by
C comparing to 10**20, if smaller, multiply by CLSUN

      CCCMAX = DMAX1(TLUMX(1),TLUMX(2),TLUMX(3),TLUMX(4),TLUMX(5),
     *     DABS(TLUMX(6)),TLUMX(7))
      IF(CCCMAX.LE.1.0D20) THEN
       DO J = 1,7
          TLUMX(J) = TLUMX(J) * CLSUN
         ENDDO
      ENDIF
      WRITE(ISTOR,40) (TLUMX(J),J=1,7)
   40 FORMAT('TLUMX',5X,1P7E17.9)

C write ENVELOPE DATA
      DO I = 1,3
         I0 = I
         IF(FTRI.LT.0D0) I0 = -I
         WRITE(ISTOR,50)I0,TRIT(I),TRIL(I),PS(I),TS(I),RS(I),
     &        (CFENV(3*I-3+J),J=1,3)
      ENDDO
   50 FORMAT('ENV',I2,5F16.12,1P3E20.12)

      CALL PINDEX(JXBEG,JXEND,LSHELL,M,ID,IDM)

      IF(LSTMOD)THEN
C write column headings for all per shell information
         WRITE(ISTOR,55)
 55      FORMAT(/,
     1' SHELL       MASS             RADIUS             LUMINOSITY            ',
     1'PRESSURE         TEMPERATURE         DENSITY               OMEGA      ',
     1'    C     H1          He4        METALS         He3             C12   ',
     1'          C13             N14             N15             O16         ',
     1'    O17             O18             H2              Li6             Li7',
     1'             Be9           OPAC       GRAV        DELR        DEL      ',
     1'   DELA       V_CONV     GAM1      HII     HEII     HEIII    BETA      ',
     1'ETA       PPI         PPII       PPIII        CNO         3HE         ',
     1'E_NUC        E_NEU       E_GRAV          A           RP/RE           FP',
     1'            FT           J/M          MOMENT        DEL_KE       V_ES ',
     1'      V_GSF      V_SS       VTOT   ',/)

C write out the requested information.
       CG=DEXP(CLN*CGL)
         DO II = 1,IDM
            I = ID(II)
C write out the basic info
            WRITE(ISTOR,62,ADVANCE='no') I,HS(I),HR(I),HL(I),HP(I),
     *         HT(I),HD(I),OMEGA(I),LC(I),(HCOMP(J,I),J=1,15)
C write out additional physics if desired
            IF(LSTPHYS)THEN
             SG = DEXP(CLN*(CGL - 2.0D0*HR(I)))*HS1(I)
               WRITE(ISTOR,63,ADVANCE='no') SO(I),SG,SDEL(1,I),SDEL(2,I),
     *           SDEL(3,I),SVEL(I),GAM1(I),0.0,0.0,0.0,SBETA(I),SETA(I),
     *           (SEG(K,I),K=1,5),SESUM(I),SEG(6,I),SEG(7,I)
c               WRITE(ISTOR,63,ADVANCE='no') SO(I),SG,SDEL(1,I),SDEL(2,I),
c     *           SDEL(3,I),SVEL(I),GAM1(I),SFXION(1,I),SFXION(2,I),SFXION(3,I),
c     *           SBETA(I),SETA(I),(SEG(K,I),K=1,5),SESUM(I),SEG(6,I),SEG(7,I)
            ELSE
               WRITE(ISTOR,63,ADVANCE='no') 0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
            ENDIF
C write out additional rotation info if desired
            IF(LSTROT.AND.LROT)THEN
             FM = DEXP(CLN*HS(I))
             DUMA = CC13*OMEGA(I)**2/(CG*FM)*5.D0/(2.D0+ETA2(I))
             A = DUMA * R0(I)**3
             RPOLEQ = (1.0D0 - A)/(1.0D0 + 0.5D0*A)
               VTOT = VES(I)+VGSF(I)+VSS(I)
               WRITE(ISTOR,64) A,RPOLEQ,FP(I),FT(I),HJM(I),HI(I),DEROT(I),
     *            VES(I),VGSF(I),VSS(I),VTOT
            ELSE
               WRITE(ISTOR,64) 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
            ENDIF
         ENDDO
 62   FORMAT(I6,0P2F18.14,1PE24.16,0P3F18.14,1PE24.16,1x,L1,
     &     3(0PF12.9),12(0PE16.8),2X)

 63   FORMAT(1PE10.4,1PE11.3,E12.4,E12.4,E12.4,1PE12.4,0PF9.5,F9.5,F9.5,F9.5,
C     &     F9.5,F9.5,F9.5,F9.5,F9.5,F9.5,F9.5,E13.5,E13.5,E13.5)
     &     F9.5,F9.5,E12.4,E12.4,E12.4,E12.4,E12.4,E13.5,E13.5,E13.5)

 64   FORMAT(E14.6,E14.6,E14.6,E14.6,E14.6,E14.6,E14.6,E11.3,E11.3,E11.3,E11.3,
     &     E11.3)

      ENDIF
C now call wrtmod, with the goal of outputting the envelope and atmosphere, or
C if required by LPULSE.
      IF(LSTATM.OR.LSTENV)THEN
       IF(LMILNE) CALL WRTMIL(HCOMP,HD,HL,HP,HR,HS1,M,MODEL)
C        CALL WRTMOD(M,LSHELL,JXBEG,JXEND,JCORE,JENV,HCOMP,HS1,HD,HL,
C      *   HP,HR,HT,LC,MODEL,BL,TEFFL,OMEGA,FP,FT,ETA2,R0,HJM,HI,HS,
C      *   DAGE)  ! KC 2025-05-31
         CALL WRTMOD(M,JENV,HCOMP,HS1,HD,HL,
     *               HP,HR,HT,MODEL,BL,TEFFL,FP,FT,HS,DAGE)
      ENDIF

      WRITE(ISTOR,65)
 65   FORMAT(/,/)

       ENDIF !closes logic block for new vs old store file format

C G Somers END
CCCCCCCCCCCCCCCCCCCCCCCCC

      RETURN
      END

























