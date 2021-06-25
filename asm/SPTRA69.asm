*          DATA SET SPTRA69    AT LEVEL 029 AS OF 02/15/07                      
*PHASE T21669B                                                                  
         TITLE 'T21669 CLASS RECORD DISPLAY, CHANGE, ADD, DELETE, LIST'         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 - REC READ IN  FOR CHANGE COMPARE                  *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - UNUSED                                                  *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
*  LEV 27 SMUR NOV13/03 BRAND LEVEL SECURITY                          *         
*  LEV 25 SMUR JUL08/02 CLIENT STRING SECURITY                        *         
*  LEV 24 BGRI OCT10/01 PUT IN CLIENT SECURITY                        *         
*  LEV 22    JAN12/94 ADD BACK VSWITCH                                *         
*  LEV 23    JUL21/94 CHANGE TO FILENAME                              *         
*  LEV 26 BGRI OCT20/03 ADD 253 PRODUCTS                              *         
*  LEV 27 SMUR MAR12/03 BRAND LEVEL SECURITY                          *         
*  LEV 28 SMUR JUL26/04 SOX                                           *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21669   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21669**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         MVC   QCLT,SPACES                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK05                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK10                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         B     VK10                                                             
         SPACE                                                                  
VK05     GOTO1 VALICLT                                                          
         SPACE                                                                  
VK10     LA    R2,TRAPRDH          PRODUCT CODE                                 
         XC    QPRD,QPRD                                                        
         MVI   BPRD,0                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                                                             
         SPACE                                                                  
         OC    BCLT,BCLT           CLIENT MUST BE ENTERED FOR PROD              
         BZ    MISSCLT                                                          
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
VK15     MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
         SPACE                                                                  
VK20     LA    R2,TRACLASH         CLASS                                        
         XC    CLASS,CLASS                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      LIST                                         
         BNE   MISSERR             YES, NOT NEEDED                              
         B     VK40                                                             
         SPACE                                                                  
VK30     GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   CLASS,WORK                                                       
         SPACE                                                                  
VK40     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=XL2'0A44'                                                
         MVC   CLSKAM,BAGYMD                                                    
         MVC   CLSKCLT,BCLT        MOVE IN CLIENT                               
         MVC   CLSKPROD,QPRD       MOVE PRODUCT INTO KEY                        
         MVC   CLSKCLAS,CLASS                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING CLSKEY,R4                                                        
         MVC   BAGYMD,CLSKAM                                                    
         CLC   BCLT,CLSKCLT        IS CLIENT SAME                               
         BE    VR10                YES                                          
         MVC   BCLT,CLSKCLT                                                     
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    VR10                                                             
         DC    H'0'                SHOULD NOT GET AN ILLEGAL CLIENT             
         SPACE                                                                  
VR10     MVC   QPRD,CLSKPROD                                                    
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VR30                                                             
         SPACE                                                                  
         OC    QPRD,QPRD           ANY PROD                                     
         BNZ   VR20                 YES                                         
         SPACE                                                                  
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR30                                                             
         SPACE                                                                  
         LA    R2,TRAPRDH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
         SPACE                                                                  
* SEE IF PRODUCT IS IN PRODUCT LIST                                             
         SPACE                                                                  
VR20     DS    0H                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
         SPACE                                                                  
VR22     CLC   QPRD,0(R1)                                                       
         BE    VR30                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VR22                                                          
         SPACE                                                                  
         LA    R2,TRAPRDH          PRODUCT CODE                                 
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         B     TRAPERR                                                          
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
VR30     MVI   ELCODE,X'10'        TITLE ELEMENT                                
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE X'10' ELEMENT                    
         LA    R6,ELEM                                                          
         USING CLSDSCEL,R6                                                      
         SPACE                                                                  
         MVI   CLSDSCEL,X'10'      TITLE ELEMENT                                
         MVI   CLSDSCLN,CLSDSLEN                                                
         SPACE                                                                  
         LA    R2,TRADESCH         COMMERCIAL CLASS DESCRIPTION                 
         GOTO1 ANY                                                              
         MVC   CLSDESC,WORK                                                     
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 ELEMENT                          
         LA    R2,TRADESCH                                                      
         USING CLSDSCEL,R6                                                      
         MVC   TRADESC,CLSDESC                                                  
         OI    TRADESCH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       L     R4,AIO                                                           
         USING CLSKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,CLSKCLT,QCLT                                         
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRAPRD),WORK                                              
         MVC   WORK(L'CLSKPROD),CLSKPROD                                        
         CLC   TRAPRD,WORK                                                      
         BE    DK20                                                             
         MVC   TRAPRD,WORK         MOVE IN PROD                                 
         OI    TRAPRDH+6,X'80'     SET ON TRANSMIT BIT                          
DK20     XC    WORK(L'TRACLAS),WORK                                             
         SPACE                                                                  
         MVC   WORK(L'CLSKCLAS),CLSKCLAS                                        
         CLC   TRACLAS,WORK                                                     
         BE    DK30                                                             
         MVC   TRACLAS,WORK        MOVE IN TYPE                                 
         OI    TRACLASH+6,X'80'     SET ON TRANSMIT BIT                         
         SPACE                                                                  
DK30     MVC   BAGYMD,CLSKAM                                                    
         CLC   BCLT,CLSKCLT        IS CLIENT SAME                               
         BE    DK32                YES                                          
         MVC   BCLT,CLSKCLT                                                     
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    DK32                                                             
         SPACE                                                                  
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BNE   DK31                                                             
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    DK32                OK TO DISPLAY                                
         SPACE                                                                  
         LA    R2,TRACLTH                                                       
         B     TRAPERR                                                          
         SPACE                                                                  
DK31     DC    H'0'                SHOULD NOT GET AN ILLEGAL CLIENT             
         SPACE                                                                  
DK32     MVC   QPRD,CLSKPROD                                                    
         MVC   CLASS,CLSKCLAS                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE 3                                                                
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR22                NO, CONTINUE WITH READ SEQUENTIAL            
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    RECCT,RECCT                                                      
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=XL2'0A44'                                                
         MVC   CLSKAM,BAGYMD                                                    
         MVC   CLSKCLT,BCLT                                                     
         MVC   CLSKPROD,QPRD                                                    
         SPACE                                                                  
* SET UP COMPARE KEYS TO FILTER LIST                                            
         SPACE                                                                  
         MVC   SVBCLT,BCLT                                                      
         MVC   SVPROD,QPRD                                                      
         SPACE                                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR30                                                             
         B     LR50                GO SEND NO SEL MSG                           
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         LA    R4,KEY                                                           
         SPACE                                                                  
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   EXIT                YES                                          
         MVC   SVKEY(2),=XL2'0A44' TO INSURE                                    
         MVC   SVKEY+2(1),BAGYMD   BUILD KEY                                    
         CLC   SVKEY(3),KEY        ONLY WANTED KEYS ARE PASSED                  
         BL    EXIT                                                             
         BH    LR20                                                             
         SPACE                                                                  
LR30     OC    SVBCLT,SVBCLT       FILTER ON CLIENT                             
         BZ    LR32                                                             
         CLC   CLSKCLT,SVBCLT      FILTER ON CLIENT                             
         BNE   LR20                 BYPASS                                      
         SPACE                                                                  
         OC    SVPROD,SVPROD       FILTER ON PRODUCT                            
         BZ    LR32                                                             
         CLC   CLSKPROD,SVPROD     FILTER ON CLIENT                             
         BNE   LR20                 BYPASS                                      
         SPACE                                                                  
LR32     OC    CLASS,CLASS         WAS CLASS ENTERED                            
         BZ    LR34                 NO                                          
         CLC   CLASS,CLSKCLAS      ONLY LIST REQUESTED CLASS                    
         BNE   LR20                                                             
         SPACE                                                                  
LR34     DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LR36                                                             
         CLC   BCLT,CLSKCLT                                                     
         BE    LR40                                                             
         SPACE                                                                  
LR36     MVC   BCLT,CLSKCLT                                                     
         BAS   RE,FCLT                                                          
         BNE   LR22                CHECK NEXT REC                               
         SPACE                                                                  
LR40     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
         SPACE                                                                  
LR50     OC    RECCT,RECCT                                                      
         BNZ   EXIT                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    EXIT                GO FORMAT FOR OFFLINE REPORT                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
         SPACE                                                                  
* FORMAT ONLINE LIST                                                            
         SPACE                                                                  
         USING CLSKEY,R4                                                        
LRL      MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,CLSKCLT,LCLT                                         
         MVC   LPROD,CLSKPROD                                                   
         MVC   LCLASS,CLSKCLAS                                                  
         SPACE                                                                  
         DROP  R4                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CLSDSCEL,R6                                                      
         MVC   LDESC,CLSDESC                                                    
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
         GOTO1 LISTMON                                                          
         B     LR20                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
         SPACE                                                                  
         USING CLSKEY,R4                                                        
LRR      DS   0H                                                                
         MVC   PCLASS,CLSKCLAS                                                  
         MVC   PCLT,QCLT                                                        
         MVC   PCLTNM,CLTNM                                                     
         MVC   PPROD,CLSKPROD                                                   
         SPACE                                                                  
         LR    R6,R4                                                            
         DROP  R4                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    LRR20                                                            
         DC    H'0'                                                             
         USING CLSDSCEL,R6                                                      
LRR20    MVC   PDESC,CLSDESC                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R6                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
FCLT     NTR1                                                                   
         OC    BCLT,BCLT CSKCLT    NON-CLIENT SPECIFIC                          
         BZ    FCLT50               YES                                         
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0             RETURN ON ERROR                              
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         L     R0,AIO1             MOVE CML CLASS RECORD BACK                   
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BNE   FCLT15                                                           
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT35               NO                                          
         B     FCLT18                                                           
         SPACE                                                                  
FCLT15   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT23                                                           
         SPACE                                                                  
* CHECK OUT BRAND LEVEL SECURITY                                                
         SPACE                                                                  
FCLT18   MVC   KEY,SVKEY           RESTORE KEY                                  
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         SPACE                                                                  
         OC    CLSKPROD,CLSKPROD   ANY PRODUCT                                  
         BZ    FCLT35              NO, OK TO DISPLAY                            
         SPACE                                                                  
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
         SPACE                                                                  
FCLT20   CLC   CLSKPROD,0(R1)                                                   
         BE    FCLT35                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,FCLT20                                                        
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BE    FCLT23                                                           
         CLI   ACTNUM,ACTLIST      OK TO LIST IT                                
         BE    FCLT23                                                           
         SPACE                                                                  
         LA    R2,TRAPRDH          PRODUCT CODE                                 
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         B     TRAPERR                                                          
         SPACE                                                                  
*NOP                                                                            
*CLT23   MVC   KEY,SVKEY           RESTORE KEY                                  
*******                                                                         
FCLT23   TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT25               NO                                          
         SPACE                                                                  
         MVI   KEY+12,X'FF'        FORCE NEXT PROD                              
         B     *+8                                                              
FCLT25   MVI   KEY+9,X'FF'         FORCE TO NEXT (MAY BE NULL CLIENT)           
         SPACE                                                                  
         GOTO1 HIGH                                                             
         MVI   BCLT,X'FF'          FORCE CHANGE ON NEXT REC                     
         LTR   RB,RB               SET CC FOR RETURN                            
         B     EXIT                                                             
         SPACE                                                                  
FCLT35   DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         SPACE                                                                  
FCLT40   MVC   AIO,AIO1                                                         
         SPACE                                                                  
         CR    RB,RB               SET CC FOR RETURN                            
         B     EXIT                                                             
         SPACE                                                                  
FCLT50   SR    R2,R2                                                            
         SR    R3,R3                                                            
         SPACE                                                                  
         L     RE,ASVCLIST                                                      
         LA    RF,880                                                           
         MVCL  RE,R2                                                            
         SPACE                                                                  
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
         MVCL  RE,R2                                                            
         SPACE                                                                  
         MVC   CLTNM,=CL20'NON-CLIENT SPECIFIC'                                 
         MVC   QCLT,SPACES                                                      
         CR    RB,RB               SET CC FOR RETURN                            
         B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
         EJECT                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
MISSCLT  LA    R2,TRACLTH                                                       
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING       NO DATA ENTERED, REQUIRED                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'COMMERIAL CLASS LIST'                                    
         SSPEC H2,35,C'--------------------'                                    
         SSPEC H1,61,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,61,AGYADD                                                     
         SSPEC H4,73,RUN                                                        
         SSPEC H4,61,REPORT                                                     
         SSPEC H5,61,REQUESTOR                                                  
         SSPEC H5,91,PAGE                                                       
         SSPEC H7,11,C'CLASS'                                                   
         SSPEC H8,11,C'-----'                                                   
         SSPEC H7,23,C'CLT   NAME'                                              
         SSPEC H8,23,C'----------'                                              
         SSPEC H7,57,C'PRODUCT'                                                 
         SSPEC H8,57,C'-------'                                                 
         SSPEC H7,69,C'DESCRIPTION'                                             
         SSPEC H8,69,C'-----------'                                             
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRCMLCLS                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA7AD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* DSECT FOR OFFLINE PRINT LINE *                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL10                                                             
PCLASS   DS    CL4                                                              
         DS    CL8                                                              
PCLT     DS    CL3                                                              
         DS    CL3                                                              
PCLTNM   DS    CL20                                                             
         DS    CL10                                                             
PPROD    DS    CL3                                                              
         DS    CL7                                                              
PDESC    DS    CL24                                                             
         SPACE                                                                  
* DSECT FOR ONLINE LIST LINE *                                                  
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLASS   DS    CL4                                                              
         DS    CL4                                                              
LCLT     DS    CL3                                                              
         DS    CL3                                                              
LPROD    DS    CL3                                                              
         DS    CL3                                                              
LDESC    DS    CL24                                                             
         SPACE                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
RECCT    DS    H                                                                
CLASS    DS    CL4                                                              
SVBCLT   DS    XL2                 COMPARE KEYS FOR ONLINE LIST                 
SVPROD   DS    CL3                                                              
         SPACE                                                                  
* FIELD USED BY FCLT - DUMMY SCREEN FIELD FOR VALICLT                           
         SPACE                                                                  
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPTRA69   02/15/07'                                      
         END                                                                    
