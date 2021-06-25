*          DATA SET CTFIL04    AT LEVEL 005 AS OF 01/27/03                      
*&&      SET   NOP=N                                                            
*PHASE TA1304A                                                                  
CTFIL04  TITLE 'FILE ROUTINE PROGRAM LEVEL OBJECTS'                             
CTFIL04  CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,CTFIL4**,R6,RR=R3,CLEAR=YES                              
         USING RTWORKD,RC                                                       
         ST    R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         ST    R3,RTRELO                                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     R7,ACOMMON                                                       
         A     R7,RTRELO                                                        
         USING COMMON,R7                                                        
         LR    R2,RF                                                            
         GOTO1 VDICTAT,RTPARM,C'LU  ',DCLIST,RTLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,RTLISTL                                       
         B     OBJECT                                                           
*                                                                               
ACOMMON  DC    A(COMMON)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT                                                           
         DC    12XL4'00'                                                        
         EJECT                                                                  
***********************************************************************         
* OBJECT - PROVIDES INTERFACE TO OBJECTS                              *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   LA    R1,RTPARMS                                                       
         TM    0(R1),GCBOVER       CAN WE OVERRIDE THIS CALL?                   
         BZ    OO02                                                             
         L     RF,AOLY                                                          
         TM    0(R1),GCBPS         FILTERING?                                   
         BZ    *+8                                                              
         L     RF,APSOLY           PREVIOUS SESSION OVERLAY REQUIRED            
         BASR  RE,RF                                                            
         BNH   EXIT                OVERRIDDEN AT A LOWER LEVEL                  
*                                                                               
OO02     LA    R5,TABLEOO          OBJECTS KNOWN AT THIS LEVEL                  
         USING OBJTABD,R5                                                       
         L     RE,0(R1)                                                         
*                                                                               
OO04     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    OO06                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     OO04                ITERATE KNOWN OBJECTS                        
*                                                                               
OO06     ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                  INVOKE OBJECT                                
         DROP  R5                                                               
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION FOR INTERNAL VERBS                              *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R3,4(R1)                                                      
         LA    R5,KEYTABL                                                       
         USING OBJTABD,R5                                                       
*                                                                               
KEY02    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    KEY04               MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     KEY02               ITERATE                                      
*                                                                               
KEY04    ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
KEYTABL  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R1,R3,4(R1)                                                      
         LA    R5,TABLREC                                                       
         USING OBJTABD,R5                                                       
*                                                                               
REC02    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    REC04               MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     REC02               BUMP & LOOP                                  
*                                                                               
REC04    ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
TABLREC  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR ZERO                            *         
* P3 BYTE  0   HOLDS GLOBAL ACTION IF P2 IS ZERO                      *         
* P3 BYTES 1-3 HOLD EQUATED VERB                                      *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
***********************************************************************         
         SPACE 1                                                                
DATA     L     RE,4(R1)            RE HOLDS DATA IDENTIFIER                     
         LTR   RE,RE               NO GLOBAL OVERRIDES (SO FAR)                 
         BZ    EXITH                                                            
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA02   CLC   KNOWID,=AL2(EOT)    E.O.T                                        
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,3,KNOWID         MATCH ON DATA TYPE                           
         BE    DATA04                                                           
         LA    RF,KNOWLQ(RF)       ITERATE THE TABLE                            
         B     DATA02                                                           
*                                                                               
DATA04   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,RTRELO           RELOCATE IT                                  
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00513),AL4(NAMDTA)  NAMEREC     NAME                         
         DC    AL2(00200),AL4(WHNDTA)  RUN WHEN                                 
         DC    AL2(00201),AL4(DSTDTA)  DESTINATION                              
         DC    AL2(00202),AL4(RPODTA)  REPORT SUB-ID                            
         DC    AL2(00203),AL4(IDODTA)  DOWNLOAD MESSAGE                         
         DC    AL2(00204),AL4(RIDDTA)  DOWNLOAD ID                              
         DC    AL2(00523),AL4(COMML1)  FFTTPCOM    COMMNET LINE 1               
         DC    AL2(00524),AL4(COMML2)  FFTTPCOM    COMMNET LINE 2               
         DC    AL2(00525),AL4(COMML3)  FFTTPCOM    COMMNET LINE 3               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
CTFIL04  CSECT                                                                  
         LTORG                                                                  
DCLIST   DS    0D                                                               
DCLISTX  DC    X'00'                                                            
         DROP  RB,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO  &VBTAB,&ID,&LB                                                  
         AIF   (T'&ID NE 'O').L1                                                
         DC    CL8' '                                                           
         AGO  .L2                                                               
.L1      ANOP                                                                   
         DC    CL8'&ID'                                                         
.L2      ANOP                                                                   
         DS    0H                                                               
         USING *,RB                                                             
&NTRDO   NTR1  BASE=*,LABEL=*                                                   
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,&VBTAB           TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
&LB.03   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    &LB.H                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    *+12                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     &LB.03              BUMP & LOOP                                  
*                                                                               
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
&LB.XL   MVI   RTPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     &LB.E                                                            
&LB.XE   MVI   RTPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     &LB.E                                                            
&LB.XH   MVI   RTPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     &LB.E                                                            
&LB.XX   MVI   RTPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     &LB.E                                                            
*                                                                               
&LB.L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     &LB.CC                                                           
&LB.H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
&LB.E    MVI   RTBYTE1,1           SET CC EQUAL                                 
&LB.CC   CLI   RTBYTE1,1                                                        
*                                                                               
*                                                                               
&LB.X    XIT1                                                                   
         MEND                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NAMELD                                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NAMDTA   NTR1  BASE=*,LABEL=*                                                   
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,NAMTABL          TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
NAM02    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    NAMH                                                             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    NAM04               MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     NAM02               BUMP & LOOP                                  
*                                                                               
NAM04    ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
NAML     MVI   RTBYTE1,0           SET CC LOW                                   
         B     NAMCC                                                            
NAMH     MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
NAME     MVI   RTBYTE1,1           SET CC EQUAL                                 
NAMCC    CLI   RTBYTE1,1                                                        
*                                                                               
NAMX     XIT1                                                                   
*                                                                               
NAMTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(DVALNAM)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A NAME                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('NAMELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   NAME                                                             
         L     RF,12(R1)           A(NAMEL)                                     
         USING NAMELD,RF                                                        
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     NAME                                                             
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A NAME                                                     *         
***********************************************************************         
         SPACE 1                                                                
DVALNAM  GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('NAMELQ',ACTRECD),0               
         CLI   FVILEN,0            WAS A NAME INPUT?                            
         BE    NAME                NO                                           
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         LA    R3,BOWORK2          BUILD A NAMEL                                
         USING NAMELD,R3                                                        
         MVI   NAMEL,NAMELQ                                                     
         IC    RE,FVILEN                                                        
         LA    RE,NAMLN1Q(RE)                                                   
         STC   RE,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),(R2),NAMELD                        
         CLI   12(R1),0                                                         
         BE    NAME                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VALIDATE WHEN REPORT TO RUN                         *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WHNDTA   NTRDO WHNTBL,RUNWHEN,WHN                                               
*                                                                               
WHNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWHN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWHN)                                 
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALWHN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WHEN REPORT TO RUN                                          *         
***********************************************************************         
         SPACE 1                                                                
DISWHN   B     WHNE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WHEN REPORT TO RUN                                         *         
***********************************************************************         
         SPACE 1                                                                
VALWHN   GOTOX ('VALWHEN',AGROUTS)                                              
         BL    WHNL                                                             
         B     WHNE                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VALIDATE DESTINATION OF REPORT                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DSTDTA   NTRDO DSTTBL,DESTN,DST                                                 
*                                                                               
DSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDST)                                 
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WHEN REPORT TO RUN                                          *         
***********************************************************************         
         SPACE 1                                                                
DISDST   B     DSTE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WHEN REPORT TO RUN                                         *         
***********************************************************************         
         SPACE 1                                                                
VALDST   GOTOX ('VALDEST',AGROUTS)                                              
         BL    DSTL                                                             
         B     DSTE                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VALIDATE REPORT SUB-ID                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RPODTA   NTRDO RPOTBL,WHORAN,RPO                                                
*                                                                               
RPOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRPO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRPO)                                 
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALRPO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REPORT SUB-ID                                               *         
***********************************************************************         
         SPACE 1                                                                
DISRPO   B     RPOE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REPORT SUB-ID                                              *         
***********************************************************************         
         SPACE 1                                                                
VALRPO   L     R4,AREP                                                          
         USING REPD,R4                                                          
         MVC   REPSUBID,FVIFLD                                                  
         MVC   INUSER,FVIFLD                                                    
         B     RPOE                                                             
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DOWNLOAD ID MESSAGE                                 *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
IDODTA   NTRDO IDOTBL,MESSAGE,IDO                                               
*                                                                               
IDOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REPORT SUB-ID                                               *         
***********************************************************************         
         SPACE 1                                                                
DISIDO   OC    DLNAME,DLNAME                                                    
         BZ    IDOE                                                             
         LA    R1,BOPARM                                                        
         XC    BOPARM,BOPARM                                                    
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,=AL2(120) *** CHANGE THIS TO EQUATE                      
         MVI   GTMAXL,L'FVIFLD                                                  
         LA    RF,FVIFLD                                                        
         STCM  RF,7,GTAOUT                                                      
         OI    GT1INDS,GT1NOREF+GT1OWRK                                         
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GTMSYS,GTGENSYS                                                  
         GOTOX VGETTXT                                                          
         B     IDOE                                                             
         DROP  R1                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DOWNLOAD ID NAME FIELD                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RIDDTA   NTRDO RIDTBL,FDLKCODE,RID                                              
*                                                                               
RIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRID)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REPORT SUB-ID                                               *         
***********************************************************************         
         SPACE 1                                                                
DISRID   OC    DLNAME,DLNAME                                                    
         BZ    RIDE                                                             
         MVC   FVIFLD(L'DLNAME),DLNAME                                          
         B     RIDE                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 1                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COMML1   NTR1  BASE=*,LABEL=*                                                   
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,COM1TABL         TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
COM102   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    COM1H               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    COM104              MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     COM102              BUMP & LOOP                                  
*                                                                               
COM104   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
COM1L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     COM1CC                                                           
COM1H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
COM1E    MVI   RTBYTE1,1           SET CC EQUAL                                 
COM1CC   CLI   RTBYTE1,1                                                        
*                                                                               
COM1X    XIT1                                                                   
*                                                                               
COM1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM1)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM1  GOTO1 AGETFFT,RTPARM,ACTRECD,(1,FFTTPCOM)                              
         B     COM1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM1  GOTO1 ABLDFFT,RTPARM,ACTRECD,(1,FFTTPCOM)                              
         B     COM1E                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 2                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COMML2   NTR1  BASE=*,LABEL=*                                                   
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,COM2TABL         TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
COM202   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    COM2H               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    COM204              MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     COM202              BUMP & LOOP                                  
*                                                                               
COM204   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
COM2L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     COM2CC                                                           
COM2H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
COM2E    MVI   RTBYTE1,1           SET CC EQUAL                                 
COM2CC   CLI   RTBYTE1,1                                                        
*                                                                               
COM2X    XIT1                                                                   
*                                                                               
COM2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM2)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM2  GOTO1 AGETFFT,RTPARM,ACTRECD,(2,FFTTPCOM)                              
         B     COM2E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 2                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM2  GOTO1 ABLDFFT,RTPARM,ACTRECD,(2,FFTTPCOM)                              
         B     COM2E                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 3                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COMML3   NTR1  BASE=*,LABEL=*                                                   
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,COM3TABL         TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
COM302   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    COM3H               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    COM304              MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     COM302              BUMP & LOOP                                  
*                                                                               
COM304   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
COM3L    CLI   *,255               SET CC LOW                                   
         B     COM3X                                                            
COM3H    CLI   *,0                 SET CC HIGH                                  
         B     COM3X                                                            
COM3E    CR    RB,RB               SET CC EQUAL                                 
         B     COM3X                                                            
*                                                                               
COM3X    XIT1  ,                                                                
*                                                                               
COM3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM3)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM3)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 3                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM3  GOTO1 AGETFFT,RTPARM,ACTRECD,(3,FFTTPCOM)                              
         B     COM3E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 3                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM3  GOTO1 ABLDFFT,RTPARM,ACTRECD,(3,FFTTPCOM)                              
         B     COM3E                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMONLY ADDRESSIBLE STORAGE                                        *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB                                                               
         USING COMMON,R7                                                        
COMMON   DS    0D                                                               
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     L     R1,RTPARMA          RETURN PARAMS TO CALLER                      
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMO  DS    F                                                                
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
RTDATA   DS    XL300                                                            
RTWORK   DS    XL80                                                             
RTBYTE1  DS    X                                                                
*                                                                               
RTLISTU  DS    0D                  DICTIONARY EQUATES USED                      
*                                                                               
RTLISTL  DS    0D                  DICTIONARY EQUATES USED                      
RTWORKL  EQU   *-RTWORKD                                                        
*                                                                               
CTFIL04  CSECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
*        CONBLKD                                                                
CONBLKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
CTFIL04  CSECT                                                                  
         ORG   CTFIL04+(((*-CTFIL04)/2048)+1)*2048                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTFIL04   01/27/03'                                      
         END                                                                    
