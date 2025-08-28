      RECURSIVE SUBROUTINE STITCH(HCOMP,HR,HP,HD,HS,HT,HL,HS1,
     * OMEGA,ETA2,HI,R0,HJM,FP,FT,TEFFL,HSTOT,BL,M,LC,MODEL)
     
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      PARAMETER (JSON=5000)
      REAL*8 DUM1(4),DUM2(3),DUM3(3),DUM4(3)
      REAL*8 ENVS1(JSON)
      COMMON/LUOUT/ILAST,IDEBUG,ITRACK,ISHORT,IMILNE,IMODPT,ISTOR,IOWR,ISTCH
      COMMON/ENVGEN/ATMSTP,ENVSTP,LENVG
      COMMON/INTATM/ATMERR,ATMD0,ATMBEG,ATMMIN,ATMMAX 
      COMMON/INTENV/ENVERR,ENVBEG,ENVMIN,ENVMAX
      COMMON/CONST1/ CLN,CLNI,C4PI,C4PIL,C4PI3L,CC13,CC23,CPI
      COMMON/CONST/CLSUN,CLSUNL,CLNSUN,CMSUN,CMSUNL,CRSUN,CRSUNL,CMBOL
      COMMON/CONST2/CGAS,CA3,CA3L,CSIG,CSIGL,CGL,CMKH,CMKHN
      COMMON/ROT/WNEW,WALPCZ,ACFPFT,ITFP1,ITFP2,LROT,LINSTB,LWNEW
      COMMON/ENVSTRUCT/ENVP(JSON),ENVT(JSON),ENVS(JSON),ENVD(JSON),
     *     ENVR(JSON),ENVX(JSON),ENVZ(JSON),LCENV(JSON),
     *     EDELS(3,JSON),EVELS(JSON),EBETAS(JSON),
     *     EGAM1(JSON),EQQDP(JSON),EFXIONS(3,JSON),
     *     ENVO(JSON), ENVL(JSON), NUMENV     
      COMMON/SCRTCH/SESUM(JSON),SEG(7,JSON),SBETA(JSON),SETA(JSON),
     *LOCONS(JSON),SO(JSON),SDEL(3,JSON),SFXION(3,JSON),SVEL(JSON)
      DIMENSION HS(JSON),HL(JSON),HR(JSON),HP(JSON),HT(JSON),HD(JSON),
     * LC(JSON),HCOMP(15,JSON),OMEGA(JSON),HS1(JSON),
     * ETA2(JSON),HI(JSON),HJM(JSON),R0(JSON),FP(JSON),FT(JSON)
C G Somers 10/14, Add spot common block
        COMMON/SPOTS/SPOTF,SPOTX,LSDEPTH
C G Somers END
C G Somers 3/17, ADDING NEW TAUCZ COMMON BLOCK
      COMMON/OVRTRN/LNEWTCZ,LCALCENV,TAUCZ,TAUCZ0,PPHOT,PPHOT0,FRACSTEP
      COMMON/ROTEN/DEROT(JSON)
      COMMON/SOUND/GAM1(JSON),LSOUND
      COMMON/TEMP2/VES(JSON),VES0(JSON),VSS(JSON),VSS0(JSON),
     *     HLE(JSON),VGSF(JSON),VGSF0(JSON),VMU(JSON)      
      SAVE
C
C
C STITCH: Pieces together the interior and envelope portions of the stellar
C model. 
C
C INPUTS:
C	HCOMP: array of compositions
C	HR: radius, interior only, logged
C	HP: pressure (interior, logged)
C	HD: density (interior, logged)
C	HG: gravity (interior, logged)
C	HS: mass (interior, logged)
C	HT: temperature (interior,logged)
C	FP: rotational distortion (pressure)
C	FT: rotational distortion (temp)
C	TEFFL: logged effective temperature
C	HSTOT: total mass, logged, in grams
C	BL: logged surface luminosity
C	M: number of shells in the interior
C
C
C OUTPUTS:
C	HCOMPF: full run of compositions, interior + envelope 
C	HRF: full run of radii, logged
C	HPF: full run of pressures, logged
C	HDF: full run of densities, logged
C	HGF: full run of gravities, logged
C   LCF: T/F convective
C   LINT: T/F part of the interior solution
C	MM: number of shells in interior + envelope 
C
C First write out the values for the interior
C
C write column headings for all per shell information
C         WRITE(ISTCH,55)
C 55      FORMAT(/,
C     1' SHELL       MASS             RADIUS             LUMINOSITY            ',
C     1'PRESSURE         TEMPERATURE         DENSITY               OMEGA      ',
C     1'    C     H1          He4        METALS         He3             C12   ',
C     1'          C13             N14             N15             O16         ',
C     1'    O17             O18             H2              Li6             Li7',
C     1'             Be9           OPAC       GRAV        DELR        DEL      ',
C     1'   DELA       V_CONV     GAM1      HII     HEII     HEIII    BETA      ',
C     1'ETA       PPI         PPII       PPIII        CNO         3HE         ',
C     1'E_NUC        E_NEU       E_GRAV          A           RP/RE           FP',
C     1'            FT           J/M          MOMENT        DEL_KE       V_ES ',
C     1'      V_GSF      V_SS       VTOT   ',/)
C
C ****************************  WRITE OUT INTERIOR INFORMATION   **********************

      CG=DEXP(CLN*CGL)
         DO I = 1,M
C write out the basic info           
            WRITE(ISTCH,62,ADVANCE='no') MODEL,I,HS(I),HR(I),HL(I),HP(I),
     *         HT(I),HD(I),OMEGA(I),LC(I),.TRUE.,(HCOMP(J,I),J=1,15)
C write out additional physics if desired
C            IF(LSTPHYS)THEN
             SG = DEXP(CLN*(CGL - 2.0D0*HR(I)))*HS1(I)
               WRITE(ISTCH,63,ADVANCE='no') SO(I),SG,SDEL(1,I),SDEL(2,I),
     *           SDEL(3,I),SVEL(I),GAM1(I),0.0,0.0,0.0,EBETAS(I),SETA(I),
     *           (SEG(K,I),K=1,5),SESUM(I),SEG(6,I),SEG(7,I)
C            ELSE
C               WRITE(ISTOR,63,ADVANCE='no') 0.0,0.0,0.0,0.0,0.0,0.0,0.0,
C     *           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
C            ENDIF
C write out additional rotation info if rotation is on
            IF(LROT)THEN
              FM = DEXP(CLN*HS(I))
              DUMA = CC13*OMEGA(I)**2/(CG*FM)*5.D0/(2.D0+ETA2(I))
              A = DUMA * R0(I)**3
              RPOLEQ = (1.0D0 - A)/(1.0D0 + 0.5D0*A)
              VTOT = VES(I)+VGSF(I)+VSS(I)
              WRITE(ISTCH,64) A,RPOLEQ,FP(I),FT(I),HJM(I),HI(I),DEROT(I),
     *            VES(I),VGSF(I),VSS(I),VTOT
            ELSE
               WRITE(ISTCH,64) 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
            ENDIF
         ENDDO
 

C **************************   WRITE OUT ENVELOPE INFORMATION   **********************


C Begin by "dropping a sinkline" with the envelope integrator
      ABEG0 = ATMBEG
      AMIN0 = ATMMIN
      AMAX0 = ATMMAX
      EBEG0 = ENVBEG
      EMIN0 = ENVMIN
      EMAX0 = ENVMAX
      ATMBEG = ATMSTP
      ATMMIN = ATMSTP
      ATMMAX = ATMSTP
      ENVBEG = ENVSTP
      ENVMIN = ENVSTP
      ENVMAX = ENVSTP
      IDUM = 0
      B = DEXP(CLN*BL)
      FPL = FP(M)
      FTL = FT(M)
      KATM = 0
      KENV = 0
      KSAHA = 0
C MHP 2/12 OMITTED OVERWRITE OF GLOBAL FLAG
C		LPULPT=.TRUE.
      IXX=0
      LPRT=.TRUE.
      LSBC0 = .FALSE.
      X = HCOMP(1,M)
      Z = HCOMP(3,M)
      RL = 0.5D0*(BL + CLSUNL - 4.0D0*TEFFL - C4PIL - CSIGL)
      GL = CGL + HSTOT - RL - RL
      PLIM = HP(M)
C G Somers 10/14, FOR SPOTTED RUNS, FIND THE
C PRESSURE AT THE AMBIENT TEMPERATURE ATEFFL
      IF(LC(M).AND.SPOTF.NE.0.0.AND.SPOTX.NE.1.0)THEN
          ATEFFL = TEFFL - 0.25*LOG10(SPOTF*SPOTX**4.0+1.0-SPOTF)
      ELSE
          ATEFFL = TEFFL
      ENDIF
      CALL ENVINT(B,FPL,FTL,GL,HSTOT,IXX,LPRT,LSBC0,
     *   PLIM,RL,ATEFFL,X,Z,DUM1,IDUM,KATM,KENV,KSAHA,
     *   DUM2,DUM3,DUM4,LPULPT)

C DEFINE SOME ARRAYS WE NEED
      DO I=1,NUMENV
          ENVS1(I) = DEXP(CLN*(ENVS(I)+HSTOT))  
      ENDDO
         DO I=M+1,M+NUMENV    
C write out the basic info. Omega and abundances take value of last interior point.           
            WRITE(ISTCH,62,ADVANCE='no') MODEL,I,ENVS(I-M)+HSTOT,ENVR(I-M),ENVL(I-M),ENVP(I-N),
     *         ENVT(I-M),ENVD(I-M),OMEGA(M),LCENV(I-M),.FALSE.,(HCOMP(J,M),J=1,15)
C write out additional physics if desired
C            IF(LSTPHYS)THEN
               SG = DEXP(CLN*(CGL - 2.0D0*ENVR(I-M)))*ENVS1(I-M) 
               WRITE(ISTCH,63,ADVANCE='no') ENVO(I-M),SG,EDELS(1,I-M),EDELS(2,I-M),
     *           EDELS(3,I-M),EVELS(I-M),EGAM1(I-M),0.0,0.0,0.0,EBETAS(I-M),0.0,
     *           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
C            ELSE
C               WRITE(ISTOR,63,ADVANCE='no') 0.0,0.0,0.0,0.0,0.0,0.0,0.0,
C     *           0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
C            ENDIF
C Rotation output set to zero placeholders for now
C            IF(LSTROT.AND.LROT)THEN
C	       FM = DEXP(CLN*HS(I))
C	       DUMA = CC13*OMEGA(I)**2/(CG*FM)*5.D0/(2.D0+ETA2(I))
C	       A = DUMA * R0(I)**3
C	       RPOLEQ = (1.0D0 - A)/(1.0D0 + 0.5D0*A)
C               VTOT = VES(I)+VGSF(I)+VSS(I)
C               WRITE(ISTCH,64) A,RPOLEQ,FP(I),FT(I),HJM(I),HI(I),DEROT(I),
C     *            VES(I),VGSF(I),VSS(I),VTOT
C            ELSE
               WRITE(ISTCH,64) 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
C            ENDIF
         ENDDO
         
         
C Output format codes **********************************************************         
         
 62   FORMAT(I6,1X,I6,0P2F18.14,1PE24.16,0P3F18.14,1PE24.16,1x,L1,1X,L1,
     &     3(0PF12.9),12(0PE16.8),2X)

 63   FORMAT(1PE10.4,1PE11.3,E12.4,E12.4,E12.4,1PE12.4,0PF9.5,F9.5,F9.5,F9.5,
C     &     F9.5,F9.5,F9.5,F9.5,F9.5,F9.5,F9.5,E13.5,E13.5,E13.5)
     &     F9.5,F9.5,E12.4,E12.4,E12.4,E12.4,E12.4,E13.5,E13.5,E13.5)

 64   FORMAT(E14.6,E14.6,E14.6,E14.6,E14.6,E14.6,E14.6,E11.3,E11.3,E11.3,E11.3,
     &     E11.3)
     
     
     
     
     
     
     
C G Somers END
CC	Stitch everything together
C      DO I=1,M
C        DO J=1,15
C          HCOMPF(J,I) = HCOMP(J,I)
C        ENDDO
C      HRF(I) = HR(I)
C      HPF(I) = HP(I)
C      HDF(I) = HD(I)
C      HSF(I) = HS(I)
C      HTF(I) = HT(I)
C      LCF(I) = LC(I)
C      LINT(I) = .TRUE.
C
C      ENDDO
C      MM = M+NUMENV
C      DO I=M+1,MM
C		DO J=1,15
C			HCOMPF(J,I) = HCOMP(J,M)
C		ENDDO
C      HRF(I) = ENVR(I-M)
C      HPF(I) = ENVP(I-M) 
C      HDF(I) = ENVD(I-M)
C      HSF(I) = ENVS(I-M)+HSTOT
C      HTF(I) = ENVT(I-M)
C      LCF(I) = LCENV(I-M)
C      LINT(I) = .FALSE.
C	ENDDO
C
CC    WRITE EVERYTHING TO FILE
C      DO I = 1, MM
C          WRITE(ISTCH,1900) MODEL,HSF(I),HRF(I),HTF(I),HPF(I),HDF(I),HCOMPF(1,I),
C     *      HCOMPF(2,I),HCOMPF(3,I),LCF(I),LINT(I)
C 1900     FORMAT(1X,1I8,8E16.8,1X,L1,1X,L1)   
C      ENDDO 
   

   
      RETURN
      END










