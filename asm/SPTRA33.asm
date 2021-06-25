*          DATA SET SPTRA33    AT LEVEL 017 AS OF 03/06/07                      
*PHASE T21633B                                                                  
*INCLUDE DPTRD                                                                  
         TITLE 'T21633 DAYPART EQUIVALENCY MAINT AND LIST'                      
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 - REC READ IN FOR CHANGE COMPARE                             
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - WORK                                                              
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
*  LEV 05    JAN08/87 ADD OFFICE PROFILE                                        
*  LEV 06-08 JAN12/87 USE DPTRD TO GET DAYPARTS                                 
*  LEV 09    FEB26/90 FIX FILTER PRINT OUT IN DK                                
*  LEV 10    JUN17/92 ADD AGENCY CODE TO RECORD                       *         
*  LEV 11    MAY12/93 ADD TRAFFIC SYSTEM                              *         
*  LEV 12    JUL20/94 CHANGE TO FILENAME                              *         
*  LEV 13 SM APR11/01 USE TRAFFIC OFFICE                              *         
*  LEV 14 BG OCT10/01 ADD CLIENT SECURITY                             *         
*  LEV 15 SMUR JUN26/02 CLIENT STRING SECURITY                        *         
*  LEV 16 SMUR JUL29/04 SOX                                           *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21633   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1633**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR33RR                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         SPACE                                                                  
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
* VALIDATE KEY ROUTINE                                                          
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
         XC    SVBCLT,SVBCLT                                                    
         XC    BPRD,BPRD                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                YES                                          
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK13                 NO                                          
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
         SPACE                                                                  
VK13     CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK30                YES, NOT NEEDED FOR LIST                     
         B     MISSERR             NO, MUST BE ENTRY                            
VK10     GOTO1 VALICLT                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
*K20     LA    R2,TRAPRDH          PRODUCT CODE                                 
*        CLI   5(R2),0                                                          
*        BE    VK30                                                             
*        OC    BCLT,BCLT           IF CLIENT NOT ENTERED                        
*        BZ    MISSCLT             ERROR                                        
*        SPACE                                                                  
*        GOTO1 VALIPRD                                                          
*        CLC   =C'POL',WORK        PRODUCT POL INVALID                          
*        BE    INVPRDER                                                         
*        MVC   PROD,WORK           SAVE EBCIC PRODUCT                           
*        MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
         SPACE                                                                  
VK30     LA    R2,TRAESTH          ESTIMATE VALIDATION                          
         MVI   BEST,0                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK50                                                             
VK40     BAS   RE,VEST                                                          
         SPACE                                                                  
VK50     LA    R2,TRAFLTRH         FILTER VALIDATION                            
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK60                                                             
         BAS   RE,VFTR                                                          
         EJECT                                                                  
* BUILD KEY *                                                                   
         SPACE                                                                  
VK60     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING DPEKEY,R4                                                        
         MVC   DPEKID,=XL2'0A34'                                                
         MVC   DPEKAM,BAGYMD                                                    
         MVC   DPEKCLT,BCLT        MOVE IN CLIENT                               
*        MVC   DPEKPRD,QPRD        MOVE PRODUCT INTO KEY                        
         MVC   DPEKEST,BEST        ESTIMATE                                     
         CLI   ACTNUM,ACTLIST      ONLY SET COMPARE KEY                         
         BNE   EXIT                FOR LIST                                     
         SPACE                                                                  
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED LEFT TO RIGHT)              
         SPACE                                                                  
         CLI   BEST,0                                                           
         BZ    VK70                                                             
         OC    BCLT,BCLT                                                        
         BNZ   VK70                                                             
         LA    R2,TRACLTH                                                       
         B     MISSERR                                                          
         SPACE 2                                                                
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
         SPACE                                                                  
VK70     LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
VK72     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   VK74                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,VK72                                                          
VK74     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE                                                                  
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         L     R6,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING DPEKEY,R4                                                        
         MVC   BAGYMD,DPEKAM                                                    
         MVC   BEST,DPEKEST                                                     
         CLC   BCLT,DPEKCLT        IS CLIENT SAME                               
         BE    VR02                 YES                                         
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         SPACE                                                                  
VR02     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYEST,BEST                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  CHANGE TO SPT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VR04                                                             
         MVC   KEY,KEYSAVE                                                      
         MVC   EKEYPRD,=C'AAA'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DPMENUER                                                         
         DROP  R4                                                               
         SPACE                                                                  
VR04     L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL'  CHANGE TO SPT                             
         GOTO1 GETREC                                                           
         USING EKEY,R6                                                          
         MVC   DPTAGY,AGENCY                                                    
         MVC   DPTMED,QMED                                                      
         MVC   DPMENU,EDAYMENU                                                  
         DROP  R6                                                               
* GET DAYPARTS FOR THIS MENU *                                                  
         SPACE                                                                  
         MVC   DMCB(4),DPTPARM                                                  
         GOTO1 =V(DPTRD),DMCB,,DPTTAB,DATAMGR,RR=SPTR33RR                       
         CLI   DMCB+8,X'FF'        ERROR                                        
         BE    DPMENUER                                                         
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         MVI   ELCODE,X'10'        DAYPART ELEMENT                              
         SPACE                                                                  
         GOTO1 REMELEM                                                          
         LA    R2,TRADP01H         FIRST DAYPART PAIR                           
         LA    R3,MAXDPTS          MAX DAYPART PAIRS POSSIBLE                   
VR08     NI    4(R2),X'FF'-X'20'   SET OFF VALIDATED                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VR08                                                          
         SPACE                                                                  
*************************************************                               
* LOOP THRU VALIDATING ONE TO MAXDPTS ELEMENTS  *                               
*************************************************                               
         SPACE                                                                  
         LA    R2,TRADP01H         FIRST DATE PAIR                              
         LA    R3,MAXDPTS          MAX DATE PAIRS POSSIBLE                      
         MVI   DPEQVSW,0          SET VALID PAIR SW OFF                         
VR20     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DPEDTAEL,R6                                                      
         MVI   DPEDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   DPEDTALN,DPEDTLEN   ELEMENT LENGTH                               
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VR30                NO                                           
         SPACE                                                                  
         BAS   RE,VDP              VALIDATE DAYPART                             
         SPACE                                                                  
         MVC   DPEDPCDE,8(R2)                                                   
         MVC   DPEQVCDE,10(R2)                                                  
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         MVI   DPEQVSW,1           SET-FOUND 1 (OR MORE) PAIR(S)                
         SPACE                                                                  
VR30     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VR20                                                          
         CLI   DPEQVSW,1           FIND ANY VALID PAIRS                         
         BNE   NOENTER             YES                                          
         SPACE                                                                  
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   KEY(13),0(R1)                                                    
         CLI   ACTNUM,ACTADD                                                    
         BE    DR                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       BAS   RE,CLRSCR                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 DATES ELEMENT                    
         USING DPEDTAEL,R6                                                      
         LA    R2,TRADP01H                                                      
         LA    R3,MAXDPTS          MAX DATE PAIR FIELDS                         
         SPACE                                                                  
DR10     MVC   8(1,R2),DPEDPCDE                                                 
         MVI   9(R2),C'='                                                       
         MVC   10(1,R2),DPEQVCDE                                                
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         BCT   R3,DR10                                                          
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         DC    H'0'                MORE THAN MAXDPTS ELEMENTS                   
DR20     LTR   R3,R3                                                            
         BZ    DR30                                                             
         SPACE                                                                  
         LR    R4,R2                                                            
DR22     OC    8(L'TRADP01,R4),8(R4)                                            
         BZ    *+14                                                             
         MVC   8(L'TRADP01,R4),WORK                                             
         OI    6(R4),X'80'                                                      
         ZIC   R1,0(R4)                                                         
         AR    R4,R1                                                            
         ZIC   R1,0(R4)                                                         
         AR    R4,R1                                                            
         BCT   R3,DR22                                                          
DR30     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING DPEKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         SPACE                                                                  
         OC    DPEKCLT,DPEKCLT     NULL CLIENT?                                 
         BZ    DK06                                                             
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,DPEKCLT,WORK                                         
DK06     DS    0H                                                               
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         MVC   QCLT,WORK                                                        
         CLC   BCLT,DPEKCLT        IS CLIENT SAME                               
         BE    DK10                 YES                                         
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
DK10     XC    WORK(L'TRAEST),WORK                                              
         ZIC   R0,DPEKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   TRAEST,WORK                                                      
         BE    *+14                                                             
         MVC   TRAEST,WORK         MOVE IN PROD                                 
         OI    TRAESTH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         OC    FILTERS,FILTERS                                                  
         BZ    EXIT                                                             
         BAS   RE,DFTR             DISPLAY FILTER(S)                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       DS   0H                                                                
         SPACE                                                                  
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GET THIS KEY                             
         SPACE                                                                  
         MVC   SVBCLT,BCLT                                                      
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    PRINTCTR,PRINTCTR                                                
         LA    R4,KEY              BUILD KEY FOR READHI                         
         USING DPEKEY,R4                                                        
         MVC   DPEKID(2),=XL2'0A34'                                             
         MVC   DPEKAM,BAGYMD                                                    
         MVC   DPEKCLT,BCLT                                                     
*        MVC   DPEKPRD,QPRD                                                     
         MVC   DPEKEST,BEST                                                     
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         MVC   P(22),=CL22'NO DAYPART RECS FOUND'                               
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
LR20     DS   0H                                                                
         LA    R4,KEY                                                           
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
         CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LR40                YES                                          
LR22     ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LR40                YES, ALL DONE                                
         SPACE                                                                  
         CLC   SVBCLT,DPEKCLT      NEW CLIENT?                                  
         BE    LR30                 NO                                          
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST & CK SECURITY          
         BNE   LR20                                                             
         SPACE                                                                  
         MVC   SVBCLT,DPEKCLT                                                   
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LR30     DS   0H                                                                
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING DPEKEY,R4                                                        
         SPACE                                                                  
         BAS   RE,FTR                                                           
         BNE   LR20                                                             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         SPACE                                                                  
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LR40     CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BNE   EXIT                NO, JUST EXIT                                
         OC    PRINTCTR,PRINTCTR   WERE ANY RECS LISTED                         
         BNZ   EXIT                YES                                          
         MVC   CONHEAD,=CL60'* NOTE * NO DAYPART EQUIV RECS FOUND *'            
         B     ERREXIT                                                          
LRCLC    CLC   COMPKEY(0),KEY                                                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
         SPACE                                                                  
LRR      DS    0H                                                               
         ZIC   R1,DPEKEST                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
         MVC   EKEYAM(3),DPEKAM                                                 
         MVC   EKEYEST,DPEKEST                                                  
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   FILENAME,=CL8'SPTDIR'  CHANGE TO SPT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    LRR04                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   EKEYPRD,=C'AAA'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    LRR04                                                            
         SPACE                                                                  
         MVC   PESTST,=C'??/??/??'                                              
         MVC   PESTEND,=C'??/??/??'                                             
         MVC   PESTNM,=CL20'ESTIMATE NOT ON FILE'                               
         B     LRR06                                                            
         DROP  R3                                                               
         SPACE                                                                  
LRR04    L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL'  CHANGE TO SPT                             
         GOTO1 GETREC                                                           
         USING EKEY,R2                                                          
         MVC   PESTNM,EDESC                                                     
         GOTO1 DATCON,DMCB,(0,ESTART),(5,PESTST)                                
         GOTO1 DATCON,DMCB,(0,EEND),(5,PESTEND)                                 
         DROP  R2                                                               
         SPACE                                                                  
LRR06    DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,PDPTS                                                         
         LA    R3,MAXDPTS                                                       
         SPACE                                                                  
LRR10    MVC   0(1,R2),DPEDPCDE                                                 
         MVI   1(R2),C'='                                                       
         MVC   2(1,R2),DPEQVCDE                                                 
         BAS   RE,NEXTEL                                                        
         BNE   LRR20                                                            
         MVI   3(R2),C','                                                       
         LA    R2,4(,R2)                                                        
         BCT   R3,LRR10                                                         
         DC    H'0'                                                             
LRR20    MVI SPACING,2                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
         MVC   LMED,QMED                                                        
         MVC   LCLT,QCLT                                                        
         ZIC   R0,DPEKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         LA    R2,LDPTS                                                         
         LA    R3,MAXDPTS                                                       
         SPACE                                                                  
LRL10    MVC   0(1,R2),DPEDPCDE                                                 
         MVI   1(R2),C'='                                                       
         MVC   2(1,R2),DPEQVCDE                                                 
         BAS   RE,NEXTEL                                                        
         BNE   LRL20                                                            
         MVI   3(R2),C','                                                       
         LA    R2,4(,R2)                                                        
         BCT   R3,LRL10                                                         
         DC    H'0'                                                             
LRL20    GOTO1 LISTMON                                                          
         LH    R1,PRINTCTR                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,PRINTCTR                                                      
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
********************************************                                    
* VALIDATE ESTIMATE                        *                                    
********************************************                                    
         SPACE                                                                  
VEST     NTR1                                                                   
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    NUMERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VESTPACK                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0               EST MUST BE 1                                
         BZ    ESTSIZER                                                         
         CH    R0,=H'255'          TO 255                                       
         BH    ESTSIZER                                                         
*                                                                               
         L     R3,ASVCLIST                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM(3),BAGYMD AND BCLT                                        
         STC   R0,EKEYEST                                                       
         STC   R0,BEST                                                          
VEST10   MVC   EKEYPRD,0(R3)                                                    
         MVC   FILENAME,=CL8'SPTDIR'  CHANGE TO SPT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VESTX                                                            
         LA    R3,4(,R3)                                                        
         MVC   KEY,KEYSAVE                                                      
         CLI   0(R3),C' '                                                       
         BH    VEST10                                                           
         B     ESTERR                                                           
         SPACE                                                                  
VESTX    XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         SPACE                                                                  
VESTPACK PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
**************************************                                          
* VALIDATE DAYPART-EQUIVALENCY ENTRY *                                          
**************************************                                          
         SPACE                                                                  
VDP      NTR1                                                                   
         CLI   8(R2),C'Z'                                                       
         BE    DAYPTZER                                                         
         CLI   9(R2),C'='                                                       
         BNE   DPFMTER                                                          
         CLI   10(R2),C'Z'                                                      
         BE    DAYPTZER                                                         
*                                                                               
         LA    R1,DPTTAB                                                        
VDP10    CLC   8(1,R2),0(R1)                                                    
         BE    VDP20                                                            
         LA    R1,5(,R1)                                                        
         CLI   0(R1),0                                                          
         BNE   VDP10                                                            
         B     NODPMENU                                                         
         SPACE                                                                  
VDP20    L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         SPACE                                                                  
         USING DPEDTAEL,R6                                                      
VDP24    CLC   DPEDPCDE,8(R2)      CK FOR EQ DAYPARTS                           
         BE    EQDPER                                                           
         BAS   RE,NEXTEL                                                        
         BE    VDP24                                                            
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
********************************************                                    
* VALIDATE FILTER - ONLY OPTION IS DAYPART *                                    
********************************************                                    
         SPACE                                                                  
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELPA),FTRHELPA                                     
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK)                             
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         SPACE                                                                  
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DAYPART                                      
         BNE   VFTR80                                                           
         MVC   DAYPTFTR,22(R4)                                                  
VFTR18   MVC   DYPTSFTR,HOLDSIGN                                                
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELPA),FTRMSG                              
         B     ERREXIT                                                          
VFTRCLCA CLC   12(0,R4),=CL7'DAYPART'                                           
         EJECT                                                                  
* FILTER - ONLY OPTION IS DAYPART *                                             
         SPACE                                                                  
         USING DPEKEY,R4                                                        
FTR      NTR1                                                                   
         CLI   DAYPTFTR,0                                                       
         BE    EXIT                                                             
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         CLI   DYPTSFTR,0                                                       
         BE    FTR34                                                            
         CLI   DYPTSFTR,X'4C'      LESS THAN                                    
         BE    FTR14                                                            
         CLI   DYPTSFTR,X'6E'      GREATER THAN                                 
         BE    FTR24                                                            
         DC    H'0'                                                             
FTR10    BAS   RE,NEXTEL                                                        
         BNE   FTRNE                                                            
FTR14    CLC   DPEDPCDE,DAYPTFTR                                                
         BNH   FTREQ                                                            
         B     FTR10                                                            
FTR20    BAS   RE,NEXTEL                                                        
         BNE   FTRNE                                                            
FTR24    CLC   DPEDPCDE,DAYPTFTR                                                
         BNL   FTREQ                                                            
         B     FTR20                                                            
FTR30    BAS   RE,NEXTEL                                                        
         BNE   FTRNE                                                            
FTR34    CLC   DPEDPCDE,DAYPTFTR                                                
         BE    FTREQ                                                            
         B     FTR30                                                            
FTREQ    CR    RB,RB                                                            
         B     EXIT                                                             
FTRNE    LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY FILTER - DATE(S), CHANGE, DELETE *                                    
         SPACE                                                                  
DFTR     NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         LR    R3,R2                                                            
         CLI   DAYPTFTR,0                                                       
         BZ    DFTR20                                                           
         MVC   0(7,R3),=C'DAYPART'                                              
         LA    R3,7(,R3)                                                        
         CLI   DYPTSFTR,0                                                       
         BE    DFTR04                                                           
         MVC   0(1,R3),DYPTSFTR                                                 
         LA    R3,1(,R3)                                                        
DFTR04   MVI   0(R3),C'='                                                       
         MVC   1(1,R3),DAYPTFTR                                                 
         LA    R3,6(,R3)                                                        
DFTR20   CLC   TRAFLTR,WORK                                                     
         BE    EXIT                                                             
         MVC   TRAFLTR,WORK                                                     
         OI    TRAFLTRH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE C LIST                                            
         SPACE                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
* SAVE CURRENT RECORD & KEY                                                     
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING DPEKEY,R4                                                        
         OC    DPEKCLT,DPEKCLT     THIS NULL CLIENT?                            
         BZ    FCLT50                                                           
         SPACE                                                                  
FCLT10   DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,DPEKCLT,QCLT                                         
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0                                                          
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CR    RB,RC               SET NE CC                                    
         B     EXIT                                                             
         SPACE                                                                  
FCLT20   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         L     R0,AIO1             MOVE CML RECORD BACK                         
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
FCLT50   DS    0H                                                               
         CLI   ERROR,0             SET CC FOR RETURN                            
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 2                                                                
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         B     EXIT                                                             
         SPACE                                                                  
* CLEAR DISPLAY AREA OF SCREEN *                                                
         SPACE                                                                  
CLRSCR   LA    RF,TRADP01H                                                      
*                                                                               
CLRSCR10 TM    1(RF),X'20'         PROTECTED                                    
         BO    CLRSCR20                                                         
         OC    8(L'TRADP01,RF),8(RF)                                            
         BZ    CLRSCR30                                                         
         CLC   8(L'TRADP01,RF),SPACES                                           
         BE    CLRSCR30                                                         
CLRSCR20 XC    8(L'TRADP01,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         NI    1(RF),X'FF'-X'20'                                                
*                                                                               
CLRSCR30 ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         LA    R0,TRATAGH                                                       
         CR    RF,R0                                                            
         BL    CLRSCR10                                                         
         BR    RE                                                               
         EJECT                                                                  
DPMENUER MVC   CONHEAD,DPMENUMS                                                 
         B     ERREXIT                                                          
*                                                                               
NODPMENU MVC   CONHEAD,NODPMUMS                                                 
         B     ERREXIT                                                          
*                                                                               
EQDPER   MVC   CONHEAD,EQDPMS                                                   
         B     ERREXIT                                                          
*                                                                               
EQEQER   MVC   CONHEAD,EQEQMS                                                   
         B     ERREXIT                                                          
*                                                                               
DPFMTER  MVC   CONHEAD,DPFMTMS                                                  
         B     ERREXIT                                                          
*                                                                               
DAYPTZER MVC   CONHEAD,DAYPTZMS                                                 
         B     ERREXIT                                                          
*                                                                               
ESTERR   MVC   CONHEAD,ESTERRMS                                                 
         B     ERREXIT                                                          
*                                                                               
ESTSIZER MVC   CONHEAD,ESTSIZMS                                                 
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NOENTER  MVI   ERROR,MISSING                                                    
         LA    R2,TRADP01H         POINT TO FIRST MISSING FIELD                 
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELPA DC    C'VALID FILTERS - DAYPART='                                      
NODPMUMS DC    CL60'* ERROR * DAYPART NOT ON DAYPART MENU *'                    
DPMENUMS DC    CL60'* ERROR * NO EST FOR POL OR AAA, NO MENU CODE *'            
DAYPTZMS DC    CL60'* ERROR * DAYPART Z IS MISC CODE, NOT ALLOWED *'            
DPFMTMS  DC    CL60'* ERROR * FORMAT IS DAYPART=EQUIVALENCY *'                  
EQDPMS   DC    CL60'* ERROR * DAYPART EQUAL TO PREVIOUS DAYPART *'              
EQEQMS   DC    CL60'* ERROR * EQUIVALENCY = TO PREVIOUS EQUIVALENCY *'          
ESTSIZMS DC    CL60'* ERROR * ESTIMATE MUST BE 1 TO 255 *'                      
ESTERRMS DC    CL60'* ERROR * ESTIMATE NOT ON SYSTEM *'                         
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,36,C'D A Y P A R T  L I S T'                                  
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,36,C'----------------------'                                  
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'EST'                                                      
         SSPEC H9,3,C'---'                                                      
         SSPEC H8,10,C'ESTIMATE NAME'                                           
         SSPEC H9,10,C'--------------------'                                    
         SSPEC H8,35,C'START DATE'                                              
         SSPEC H9,35,C'----------'                                              
         SSPEC H8,50,C'END DATE'                                                
         SSPEC H9,50,C'--------'                                                
         SSPEC H8,65,C'DAYPART EQUIVALENCY PAIRS'                               
         SSPEC H9,65,C'-------------------------'                               
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRDPEQV                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD8D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
SPTR33RR DS    F                                                                
         SPACE                                                                  
* PARM 1 FOR DAYPART READ *                                                     
         SPACE                                                                  
DPTPARM  DS    0CL4                                                             
DPTAGY   DS    CL2                                                              
DPTMED   DS    CL1                                                              
DPMENU   DS    CL1                                                              
         SPACE                                                                  
DPEQVSW  DS    XL1                                                              
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
PRINTCTR DS    H                                                                
SVBCLT   DS    XL2                                                              
FILTERS  DS    0CL3                                                             
DAYPTFTR DS    CL1                                                              
DYPTSFTR DS    CL1                                                              
HOLDSIGN DS    CL1                                                              
DPTTAB   DS    CL182                                                            
MAXDPTS  EQU   14                                                               
FLDH     DS    XL8                                                              
FLD      DS    CL40                                                             
         SPACE                                                                  
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL4                                                              
PESTNM   DS    CL20                                                             
         DS    CL5                                                              
PESTST   DS    CL8                                                              
         DS    CL7                                                              
PESTEND  DS    CL8                                                              
         DS    CL7                                                              
PDPTS    DS    CL66                                                             
         SPACE                                                                  
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMED     DS    CL1                                                              
         DS    CL3                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL2                                                              
LDPTS    DS    CL62                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPTRA33   03/06/07'                                      
         END                                                                    
