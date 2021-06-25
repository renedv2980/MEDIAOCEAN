*          DATA SET SPTRA65    AT LEVEL 044 AS OF 02/17/16                      
*PHASE T21665A                                                                  
*INCLUDE GRID                                                                   
*        TITLE 'T21665 SPOT COMMERICAL LIST AND REPORT'                         
***********************************************************************         
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
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
*                                                                               
***********************************************************************         
*                                                                               
*  LEV  6    SEP28/89                                                           
*  LEV  7    MAR02/90 POINT R2 TO TRACLT IF DEALER TAG ERROR                    
*  LEV  8    MAR11/91 SHOW A/F FOR RADIO                                        
*  LEV  9    MAY02/91 FIX BUG WITH FILM CODES AND AFFIDAVITS ELEMS    *         
*  LEV 10    DEC22/92 FIX FOR MSUNPK                                  *         
*  LEV 11    MAR01/93 ALLOW UNKNOWN PRODUCT (NO DUMP) OPT DSKAD       *         
*  LEV 12    MAR23/93 CABLE HEAD                                      *         
*  LEV 13    JUN08/93 ALLOW SPOT LIST TO RUN DEALER TAGS              *         
*  LEV 14    JUN25/93 CHANGE SVSPARE TO SVSPAREX                      *         
*  LEV 15    JUL13/93 FIX AUTO TURN-AROUND DATES                      *         
*  LEV 16    JUL19/93 FIX OPTFLAG TO TM, NOT CLI                      *         
*  LEV 17    JUL29/93 FIX GETCMML, PRINT 15 POS PROG NAME             *         
*  LEV 18    SEP20/93 ADD PRINTED FLAG TEST FOR ACTIVITY OPTION       *         
*                     AND SHOW PRINTED FLAG ON REPORT, NEW TRAFFIC SYS*         
*  LEV 19    NOV24/93 BYPASS SPILL POINTER BUY KEYS                   *         
*                     AND FIX BAD KEYS AS A RESULT OF USEIO = Y       *         
*  LEV 20    DEC11/93 FIX SILLY CLC                                   *         
*  LEV 21    JUN17/94 ALLOW FOR BDDAY OF ZERO                         *         
*  LEV 22    JUL12/94 ADD TRLSTPGM, CATCHIO FOR MAX I/O'S             *         
*  LEV 23    JUL21/94 CHANGE TO FILENAME                              *         
* LEV 24 SMUR OCT04/96 CHK IF PRD/LEN WAS CHANGED ON THE BUY          *         
*                    - DO NOT INVERT PRDS, FIX BUGS                   *         
* LEV 25 SMUR SEP05/97 TA PROFILE TO DETERMINE # OF WEEKS TO SEE      *         
* LEV 26 SMUR APR11/01 USE TRAFFIC OFFICE                             *         
* LEV 27 SMUR AUG01/01 ADD MORE CODE TO VALIDATE CMLS                 *         
* LEV 29 BGRI JUN11/02 FIX FILENAME USAGE                             *         
* LEV 30 BGRI OCT20/03 ADD OPTION SEED - WILL ACCEPT SEEDED AS OK     *         
* LEV 31 SMUR JUL07/06 INCLUDE ROTATION IN CML VALIDATION             *         
* LEV 33 SMUR DEC14/07 BYPASS TRAFFIC=NO                              *         
* LEV 34 MNAS NOV13/08 SET R2 BEFORE ERROR EXIT                       *         
* LEV 40 MNAS SEP03/10 ADD TA PROFILE TO CONTROL COST DISPLAY         *         
* LEV 41 MNAS OCT23/12 MORE BANDS                                     *         
* LEV 42 MNAS JAN22/13 MORE BANDS - PROFILE                           *         
* LEV 43 SMUR FEB24/15 FIX BAD INSTRUCTIONS                           *         
* LEV 44 SMUR JAN06/16 NEW BAND CM FOR IHEART RADIO                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21665 SPOT COMMERICAL LIST AND REPORT'                         
T21665   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21665**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTRRR                                                        
         MVI   NLISTS,14                                                        
         MVI   USEIO,C'N'                                                       
*&&OS                                                                           
         LA    RE,HEAD1                                                         
         LHI   RF,2000                                                          
         XCEFL                                                                  
*                                                                               
         LA    RE,DLCB                                                          
         LHI   RF,256                                                           
         XCEFL                                                                  
*&&                                                                             
*                                                                               
         CLI   ACTNUM,29                                                        
         BNE   SPOTL1                                                           
         GOTOR GRIDI,GRIDCLRQ                                                   
*                                                                               
SPOTL1   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
         CLI   ACTNUM,29                                                        
         BNE   SPOTL3                                                           
         GOTOR GRIDI,GRIDINIQ                                                   
         BE    LR                                                               
         GOTOR GRIDI,GRIDXITQ                                                   
*                                                                               
SPOTL3   CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
                                                                                
VK       CLI   TRLSTPGM,X'65'      WAS THIS LAST PROGRAM ACTIVE                 
         BNE   VK10                 NO, FORCE REVALIDATION                      
         TM    TRAMEDH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRACLTH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAPRDH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAPTRH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAESTH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRASTAH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAPERH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAOPTH+4,X'20'                                                  
         BO    EXIT                                                             
                                                                                
VK10     DS    0H                                                               
         LA    R2,TRAMEDH          MEDIA HEADER                                 
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    MISSERR             MEDIA IS REQUIRED                            
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
                                                                                
VKCLT    LA    R2,TRACLTH          CLIENT HEADER                                
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    MISSERR             CLIENT IS REQUIRED                           
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GO GET PROFILE RECORD(S)                     
         OI    4(R2),X'20'                                                      
                                                                                
VKPRD    LA    R2,TRAPRDH          PRODUCT                                      
         XC    BPRD(2),BPRD                                                     
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK18                PRODUCT IS NOT REQUIRED                      
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
VK18     OI    4(R2),X'20'                                                      
                                                                                
VK20     LA    R2,TRAPTRH          PARTNER                                      
         MVC   QPRD2,SPACES                                                     
         XC    BPRD2(2),BPRD2                                                   
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK28                                                             
         CLI   BPRD,0             MUST HAVE PROD. IF HAVE PTR PROD              
         BE    NEEDPROD                                                         
         GOTO1 ANY                                                              
         CLC   =C'NONE',WORK       DON'T ALLOW ANY PIGGYBACK PRODS              
         BE    VK26                                                             
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2(2),WORK+3                                                  
         B     VK28                                                             
VK26     MVI   BPRD2,255                                                        
                                                                                
VK28     OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRAESTH          EST NUMBER                                   
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK30                                                             
         CLI   BPRD,0                                                           
         BE    NEEDPROD           HAVE ESTIMATE -MUST HAVE PROD                 
         GOTO1 VALINUM                                                          
         MVC   QBEST,ACTUAL        SET AS START EST                             
         MVC   QBESTEND,ACTUAL     AND AS END                                   
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   BDESTPR                                                          
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
                                                                                
         USING ESTHDRD,R6                                                       
                                                                                
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVGENST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVGENEND)                                  
         DROP  R6                                                               
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
VK30     OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRASTAH          MARKET/STATION                               
         XC    BMKTSTA,BMKTSTA                                                  
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK46                MKT/STATION NOT REQUIRED                     
         TM    4(R2),X'08'         IS THIS NUMERIC                              
         BO    VK44                                                             
         GOTO1 VALISTA                                                          
         B     VK46                                                             
VK44     GOTO1 VALIMKT                                                          
                                                                                
VK46     OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRAPERH          PERIOD                                       
         XC    PERSTP,PERSTP                                                    
         XC    PERENDP,PERENDP                                                  
         GOTO1 =A(VPER),RR=SPTRRR   VALIDATE PERIOD/FLIGHT                      
                                                                                
VK50     OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRAOPTH                                                       
         MVI   OPTFLAG,0                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK60                NO                                           
         BAS   RE,VOPT             GO VALIDATE OPTIONS                          
                                                                                
VK60     OI    4(R2),X'20'                                                      
                                                                                
         MVI   TRLSTPGM,X'65'      SET THIS AS LAST PROGRAM ACTIVE              
                                                                                
***************************                                                     
* BUILD KEY               *                                                     
***************************                                                     
                                                                                
         LA    R4,KEY                                                           
         USING BUYREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,QBEST                                                    
                                                                                
         MVI   NOTFRST,0                                                        
         MVI   LISTNO,0           COUNTER ON SCREEN                             
         XC    LSTCNTR,LSTCNTR    COUNTER INTO BUY RECORD                       
         XC    SVKEY,SVKEY        CLEAR SVKEY                                   
         B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
* ROUTINES CALLED FROM VALIDATE KEY FIELDS *                                    
********************************************                                    
                                                                                
VOPT     NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VOPTCLC                                                       
         BNE   VOPT10                                                           
         OI    OPTFLAG,OPTACT                                                   
         B     EXIT                                                             
                                                                                
VOPT10   EX    R1,VOPTCLCA         DSKAD                                        
         BNE   VOPT20                                                           
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   VOPT20                                                           
         OI    OPTFLAG,OPTDSK                                                   
         B     EXIT                                                             
                                                                                
VOPT20   EX    R1,VOPTCLCS         SEED                                         
         BNE   VOPT30                                                           
         OI    OPTFLAG,OPTSEED                                                  
         B     EXIT                                                             
                                                                                
VOPT30   MVC   CONHEAD(34),=C'ONLY VALID OPTIONS = ACTIVITY/SEED'               
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX2                                                           
                                                                                
VOPTCLC  CLC   8(0,R2),=CL9'ACTIVITY'                                           
VOPTCLCA CLC   8(0,R2),=CL6'DSKAD'                                              
VOPTCLCS CLC   8(0,R2),=CL6'SEED '                                              
                                                                                
* GET PROFILE REC(S)                                                            
FPRO     NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOTO'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ TA PROFILE *                                                             
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVTAPR14,SVT1PROF+13   # OF WEEKS TO SHOW                        
         MVC   SVTAPR15,SVT1PROF+14   SHOW COST ON GRIDS                        
*MNMB                                                                           
* READ T3 PROFILE *                                                             
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT3PR06,SVT1PROF+5                                              
*MNMB                                                                           
         B     EXIT                                                             
                                                                                
* READ T2 PROFILE *                                                             
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT2PR9,SVT1PROF+8  BRAND AGENCY                                 
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
* ONLINE LIST OR OFFLINE REPORT ROUTINE    *                                    
********************************************                                    
                                                                                
LR       DS    0H                                                               
         LA    RF,HEADINGS                                                      
         ST    RF,SPECS                                                         
         LA    RF,HDHK                                                          
         ST    RF,HEADHOOK                                                      
                                                                                
         MVI   USEIO,C'N'                                                       
         MVC   AIO,AIO1                                                         
                                                                                
         CLI   BYTE,1                                                           
         BE    LR01A               PERIOD IS SET BY TA PROFILE                  
                                                                                
         CLI   PERENDP,0          ANY PERIOD ENTERED?                           
         BNE   LR01A                                                            
                                                                                
*****************************************************************               
**** GET TODAY'S DATE - USE MONDAY AS START AND 60 + AS END *****               
*****************************************************************               
                                                                                
         GOTO1 DATCON,DMCB,(5,WORK),(0,TDAY)                                    
         MVC   SVDAY,TDAY                                                       
         GOTO1 GETDAY,(R1),TDAY,WORK                                            
         CLI   0(R1),1            IS IT MONDAY?                                 
         BE    LR00                                                             
         ZIC   R2,0(R1)                                                         
         BCTR  R2,0                                                             
         LNR   R0,R2              NEGATIVE (SUBTRACT TO GET TO MONDAY)          
         GOTO1 ADDAY,(R1),SVDAY,XDAY,(R0)                                       
         MVC   SVDAY,XDAY                                                       
         XC    XDAY,XDAY                                                        
                                                                                
LR00     GOTO1 DATCON,(R1),(0,SVDAY),(2,PERSTP)                                 
                                                                                
         GOTO1 ADDAY,(R1),SVDAY,XDAY,F'20'                                      
         GOTO1 DATCON,(R1),(0,XDAY),(2,PERENDP)                                 
                                                                                
LR01A    MVC   SYSDIR(3),=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                
         MVC   SYSFIL(3),=C'SPT'                                                
                                                                                
                                                                                
         XC    LPTRC,LPTRC                                                      
                                                                                
         CLI   MODE,PRINTREP      DO WE WANT TO DO REPORT?                      
         BNE   LR01               NO                                            
         MVI   LISTNO,0           YES - RESET FLAGS AND REBUILD KEY             
         MVI   NOTFRST,0                                                        
         XC    LSTCNTR,LSTCNTR                                                  
                                                                                
LR01     CLI   NOTFRST,1          BACK TO TOP?                                  
         BNE   LR02                                                             
         MVI   NOTFRST,0                                                        
         B     LR05                                                             
                                                                                
LR02     CLI   LISTNO,14                                                        
         BL    LR05                                                             
         MVC   KEY,SVKEY          NOT FIRST TIME ROUND                          
         B     LR10                                                             
                                                                                
LR05     DS    0H                                                               
*&&DO                                                                           
         TM    TSTFLG,TSTSET                                                    
         BNO   *+6                                                              
         DC    H'0'                                                             
         OI    TSTFLG,TSTSET                                                    
*&&                                                                             
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING BUYREC,R4                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,QBEST                                                    
         MVC   MYKEY,KEY          SAVE KEY FOR SEQ. COMPARE                     
                                                                                
LR10     LA    R3,3               ONE LESS THAN ACTUAL                          
         OC    BMKT,BMKT                                                        
         BZ    LR15                                                             
         LA    R3,5               ONE LESS THAN ACTUAL                          
         OC    BSTA,BSTA                                                        
         BZ    LR15                                                             
         LA    R3,8               ONE LESS THAN ACTUAL                          
         CLI   QBEST,0                                                          
         BE    LR15                                                             
         LA    R3,9               ONE LESS THAN ACTUAL                          
                                                                                
LR15     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         EX    R3,LRCLC                                                         
         BE    LR30                                                             
         B     LRXIT                                                            
                                                                                
LR16     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH               REREAD RECORD FOR GENCON                      
                                                                                
LR18     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   EXCEEDIO             YES                                         
                                                                                
LR20     EX    R3,LRCLC                                                         
         BE    LR30                                                             
                                                                                
         MVI   NOTFRST,1          FLAG ON REENTRY- START DISPLAY OVER           
         XC    LSTCNTR,LSTCNTR                                                  
         MVI   LISTNO,0                                                         
LRXIT    NI    TRAMEDH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRAPRDH+4,X'FF'-X'20'                                            
         NI    TRAPTRH+4,X'FF'-X'20'                                            
         NI    TRAESTH+4,X'FF'-X'20'                                            
         NI    TRASTAH+4,X'FF'-X'20'                                            
         NI    TRAPERH+4,X'FF'-X'20'                                            
         NI    TRAOPTH+4,X'FF'-X'20'                                            
                                                                                
         MVC   SYSDIR(3),=C'TRF'     SWITCH TO SPOT MEDIA SYSTEM                
         MVC   SYSFIL(3),=C'TRF'                                                
                                                                                
         CLI   ACTNUM,29           TEST GRIDS                                   
         BNE   EXIT                                                             
         GOTOR GRIDI,GRIDENDQ                                                   
         B     EXIT                                                             
*                                                                               
LRCLC    CLC   KEY(0),MYKEY                                                     
                                                                                
LR30     DS    0H                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   LR32                                                             
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    LR18                                                             
         CLI   DUB+4,C'S'                                                       
         BE    LR18                                                             
         CLI   DUB+4,C'C'          CM FOR IHEART                                
         BE    LR18                                                             
LR32     DS    0H                                                               
*MNMB                                                                           
         CLI   QBEST,0                                                          
         BE    LR34                                                             
         CLC   QBEST,KEY+9                                                      
         BNE   LR18                                                             
                                                                                
LR34     L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         TM    15(R4),X'80'       TEST DELETED BUY                              
         BO    LR18                                                             
                                                                                
* BYPASS SPILL MARKET KEYS *                                                    
                                                                                
         CLC BUYMSTA,BUYMSTA-BUYRECD+KEY                                        
         BNE   LR18                                                             
                                                                                
         SR    R0,R0                                                            
         SR    RE,RE              CLEAR COUNTER                                 
         ICM   R0,1,BDDAY                                                       
         BZ    LR40                                                             
         SLL   R0,25                                                            
         LTR   R0,R0                                                            
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
                                                                                
         SLL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
LR40     STH   RE,ROTDAYS         AND SAVE DAYS                                 
                                                                                
*                                                                               
* LOOK FOR TRAFFIC=NO                                                           
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
LR42     BAS   RE,BUYEL                                                         
         BNE   LR45                                                             
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    LR18                BYPASS THIS BUY                              
         B     LR42                                                             
*                                                                               
                                                                                
* USED IN BUYEL TO GET ELEMENTS FROM '0B' TO '0D'                               
                                                                                
LR45     MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
                                                                                
         CLI   LISTNO,14          FIRST TIME AROUND                             
         BL    LR50               YES                                           
         MVI   LISTNO,0                                                         
         LH    R1,LSTCNTR         GET BACK TO RIGHT BUY                         
         LTR   R1,R1              IS THIS A NEW REC?                            
         BZ    LR50               GET FIRST ELEMENT                             
         BAS   RE,BUYEL           SO CAN DISPLAY NEXT SCREEN FULL               
         BCT   R1,*-4                                                           
                                                                                
LR50     BAS   RE,BUYEL                                                         
         BE    LR55                                                             
         XC    LSTCNTR,LSTCNTR    NEXT RECORD -CLR CNTR INTO BUY                
         B     LR16                                                             
                                                                                
LR55     LH    R1,LSTCNTR         INCREMENT COUNTER OF BUYS                     
         AH    R1,=H'1'           PROCESSED                                     
         STH   R1,LSTCNTR                                                       
         TM    6(R6),X'C0'        TEST MINUS OR MISUSED                         
         BNZ   LR50                                                             
         CLI   1(R6),10           TEST UNALLOCATED                              
         BNH   LR50                                                             
         MVC   ELDATE,2(R6)       TEST PAST PERIOD                              
         XC    SVPROD(10),SVPROD                                                
         MVC   SVBPRD(2),10(R6)                                                 
         LA    R1,SVPROD                                                        
         BAS   RE,FPROD                                                         
         CLI   1(R6),18           THIS PIGGYBACK PROD                           
         BNE   LR60                                                             
         MVC   SVBPRD2(2),14(R6)                                                
         LA    R1,SVPROD2                                                       
         BAS   RE,FPROD                                                         
         MVI   SVPRDFLG,0                                                       
         CLC   SVPROD,SVPROD2     SEE IF PRODS IN ALPHA ORDER                   
*NOP     BL    LR60               BYPASS SWAP PRODS (BY SMUR)                   
         B     LR60                                                             
                                                                                
*REVERSE PRODUCT ORDER                                                          
                                                                                
         MVC   DUB(5),SVPROD                                                    
         MVC   SVPROD(5),SVPROD2                                                
         MVC   SVPROD2(5),DUB                                                   
         MVI   SVPRDFLG,1                                                       
                                                                                
LR60     CLI   BPRD,0             WAS PROD ENTERED?                             
         BE    LR64                                                             
         CLC   BPRD,SVBPRD        THIS PROD                                     
         BNE   LR50                                                             
         CLI   BSLN,0             ANY SPOT LEN                                  
         BE    *+14                                                             
         CLC   BSLN,SVBSLN        THIS SPOT LEN                                 
         BNE   LR50                                                             
         CLI   BPRD2,0            NO P/B'S- SHOW ALL                            
         BE    LR64                                                             
         CLI   BPRD2,255          NO P/B'S AT ALL (NONE)                        
         BE    LR50               YES BYPASS                                    
         CLI   1(R6),18           THIS PIGGYPACK                                
         BNE   LR50               NO P/B IN ELEM - SKIP CAUSE KEY DOES          
         CLC   BPRD2,SVBPRD2                                                    
         BNE   LR50                                                             
         CLI   BSLN2,0            LIMIT ON P/B'S SPOT LEN                       
         BE    LR64                                                             
         CLC   BSLN2,SVBSLN2                                                    
         BNE   LR50                                                             
                                                                                
LR64     MVC   ELDATEX,2(R6)      END DATE                                      
         CLC   ELDATE,PERENDP     TEST AFTER PERIOD                             
         BH    LR50                                                             
                                                                                
         CLI   0(R6),11           TEST REGEL                                    
         BNE   LR70                                                             
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
                                                                                
LR70     CLC   ELDATEX,PERSTP     TEST BEFORE PERIOD                            
         BL    LR50                                                             
                                                                                
         MVC   LISTAR,SPACES                                                    
                                                                                
* HAVE BUY NEED TO SEE IF CML ASSIGN FOR IT *                                   
* ALSO NEED TO SAVE A FEW VALUES FOR LATER PRINTING *                           
                                                                                
         MVC   SVMSTA,BUYMSTA     SAVE MKT/STA                                  
         MVC   SVEST,BUYKEST      SAVE ESTIMATE                                 
         MVC   SVDTIMST,BDTIMST   SAVE TIME                                     
*                                                                               
         LLC   R0,BUYKEY+10                                                     
         TM    BUYRCNTL,BUYRLN2                                                 
         BZ    *+8                                                              
         ICM   R0,3,BUYKEY+10                                                   
         STH   R0,SVLINE                                                        
*                                                                               
         MVC   SVDDAY,BDDAY       SAVE DAY                                      
         MVC   SVDDAYPT,BDDAYPT   SAVE D/P                                      
         MVC   SVDATE,ELDATE      SAVE RDATE(SPOT DATE)                         
         MVC   SVDATEX,ELDATEX    + ROTATION                                    
                                                                                
         XC    GRFSTR(GRFLEN),GRFSTR      CLEAR FIELDS FROM LAST DIS            
         MVC   GRDPT,BDDAYPT      DAYPART                                       
         GOTO1 UNDAY,DMCB,SVDDAY,GRROT                                          
         CLI   SVTAPR15,C'Y'       SHOW COST ON GRIDS                           
         BNE   LR70C                                                            
         EDIT  (B3,BDCOST),(10,GRCOST),0,ZERO=NOBLANK,ALIGN=LEFT                
                                                                                
LR70C    DS    0H                                                               
         ST    R6,SVR6                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR71                                                             
         USING BDELEM,R6                                                        
         OC    BDMGDATE,BDMGDATE                                                
         BZ    *+10                                                             
         MVC   GRMKGD,=C'MAKEGOOD'                                              
LR71     L     R6,SVR6                                                          
         DROP  R6                                                               
                                                                                
LR72     MVI   SVSPTFLG,0          SET DEALER TAG OR NO ELEM (UNKNOWN)          
         XC    SVSQ,SVSQ                                                        
         XC    SVSQ2,SVSQ2                                                      
         XC    SVPREF,SVPREF          OR PATTERN REF                            
                                                                                
         LR    RE,R6                                                            
         ZIC   R0,1(R6)                                                         
         B     LR76                                                             
                                                                                
LR74     ZIC   R0,1(RE)                                                         
                                                                                
LR76     AR    RE,R0                                                            
         CLI   0(RE),13           THIS ANOTHER SPOT                             
         BNH   LR80                YES, NO X'18'                                
                                                                                
         CLI   0(RE),X'18'        THIS A CML ASSIGN ELEMENT                     
         BNE   LR74                                                             
         CLI   1(RE),9            THIS A DEALER TAG?                            
         BE    LR78                YES                                          
         MVC   SVSQ,2(RE)                                                       
         MVC   SVSQ2,4(RE)                                                      
         MVC   SVPREF,6(RE)        SAVE PATTERN REF (IF ANY                     
                                                                                
         XC    SVDLR,SVDLR                                                      
         MVI   SVSPTFLG,1          SET AS SPOT ASSIGN                           
         B     LR79                                                             
                                                                                
LR78     MVC   SVSQ,3(RE)          SAVE COMML SEQ                               
         MVC   SVDLR,5(RE)         SAVE DEALER TAGS                             
                                                                                
* READ CMML PASSIVE PTR - GET PRD-CMML AND PTR-CMML   *                         
                                                                                
LR79     BAS   RE,GETCMML                                                       
                                                                                
         OC    SVDLR,SVDLR                                                      
         BZ    LR79A                                                            
         EDIT  (B2,SVDLR),(4,LPTRC)                                             
         MVC   LPTRC+5(2),=C'DT'                                                
         MVI   BADCPRD,0          SET OFF PCOMML DELETED?                       
                                                                                
***********************************************************************         
* HAVE BUY WITH CML ASSIGN - CHECK IF WANT TO LIST THIS BUY           *         
* OPTFLAG=1 ACTIVITY - ONLY ONES WITHOUT CML ASSIGN/NOT PRTD ON INST  *         
* OPTFLAG=0 STRAIGHT LIST - ONES WITH AND WITHOUT CML ASSIGN          *         
***********************************************************************         
                                                                                
LR79A    TM    OPTFLAG,OPTACT                                                   
         BO    LR79AC             SEE IF PRINTED ON INSTR                       
                                                                                
         TM    OPTFLAG,OPTSEED                                                  
         BO    LR79AE             LIST ALL BUYS                                 
         B     LR80                                                             
                                                                                
LR79AC   DS    0H                                                               
         TM    SVPREF,X'80'        PRINTED ON INSTR YET                         
         BZ    LR80                 NO                                          
                                                                                
* USER WANTS BUYS W/O CMML ASSIGN - BUT BAD CMML ASSIGN MUST PRINT              
                                                                                
LR79AE   DS    0H                                                               
         CLI   BADFLAG,1                                                        
         BNE   LR50                IS ASSIGNED TO GOOD COMML                    
                                                                                
* PRINT OUT THIS BUY - ARE WE ON OR OFFLINE *                                   
                                                                                
LR80     CLI   MODE,PRINTREP                                                    
         BE    LR180                                                            
                                                                                
* ONLINE REPORT - FILL LIST LINE WITH INFO                                      
                                                                                
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SVMSTA),LMKT,WORK                             
         MVC   LSTA(5),WORK                                                     
         CLC   WORK+5(3),SPACES    CABLE HEAD                                   
         BE    LR81                                                             
         MVC   LSTA+5(3),WORK+5                                                 
         MVI   LSTA+4,C'/'                                                      
         B     LR83                                                             
                                                                                
LR81     LA    R1,LSTA+4                                                        
         IC    R0,LSTA+4                                                        
         CLI   LSTA+3,C' '                                                      
         BH    LR82                                                             
         BCTR  R1,0                                                             
LR82     MVI   0(R1),C'-'                                                       
*MNMB                                                                           
         CLI   WORK+4,C'D'                                                      
         BNE   *+14                                                             
         MVC   1(1,R1),WORK+4                                                   
         B     *+10                                                             
*MNMB                                                                           
         MVC   1(1,R1),QMED                                                     
         CLI   QMED,C'R'                                                        
         BNE   LR83                                                             
         STC   R0,1(,R1)                                                        
                                                                                
LR83     ZIC   R0,SVEST         ESTIMATE                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
                                                                                
         MVC   TMPTIME,SVDTIMST                                                 
         GOTO1 UNTIME,DMCB,TMPTIME,LTIME                                        
                                                                                
         TM    SVPREF,X'80'       WAS SPOT PRINTED ON INSTR                     
         BZ    *+8                                                              
         MVI   LPRTD,C'P'                                                       
                                                                                
         OC    SVPROD,SVPROD      IS THERE PRODUCT                              
         BZ    LR85                                                             
         MVC   LPRDS(L'SVPROD),SVPROD  MOVE IN PRODUCT                          
         LA    R1,LPRDS+3                                                       
         CLI   LPRDS+2,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'         MOVE - SPOT LENGTH                            
                                                                                
         EDIT  (B1,SVBSLN),(3,1(R1)),ALIGN=LEFT                                 
                                                                                
LR85     OC    SVPROD2,SVPROD2    IS THERE A PARTNER PROD                       
         BZ    LR90                                                             
         MVC   LPTRS,SVPROD2      MOVE IN PTR PROD                              
         LA    R1,LPTRS+3                                                       
         CLI   LPTRS+2,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'         MOVE IN - SPOT LENGTH                         
         EDIT  (B1,SVBSLN2),(3,1(R1)),ALIGN=LEFT                                
                                                                                
LR90     CLC   LDATE,SPACES                                                     
         BNE   LR95                                                             
         GOTO1 DATCON,DMCB,(2,ELDATE),(4,LDATE)                                 
                                                                                
LR95     MVC   SVKEY,KEY          SAVE GENCON KEY                               
         XC    KEY(13),KEY                                                      
                                                                                
         L     R4,AIO1                                                          
         USING BUYRECD,R4                                                       
         MVC   LPGM(12),BDPROGRM  MOVE IN PGM NAME                              
         ZIC   R1,LISTNO                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,LISTNO                                                        
                                                                                
         TM    OPTFLAG,OPTDSK      PRINT DISK ADDRESS                           
         BZ    LR100                NO                                          
         GOTO1 HEXOUT,DMCB,KEY+14,LTIME-1,4                                     
                                                                                
LR100    DS   0H                                                                
         CLI   ACTNUM,29                                                        
         BNE   LR105                                                            
         GOTOR GRIDI,GRIDPUTQ                                                   
         MVC   KEY,SVKEY          RESTORE KEY                                   
         B     LR50                                                             
*                                                                               
LR105    DS    0H                                                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 LISTMON                                                          
                                                                                
         CLC   LPTRC,SPACES                                                     
         BNH   LR130                                                            
         CLI   LISTNUM,13                                                       
         BNH   LR120                                                            
         GOTO1 LISTMON                                                          
                                                                                
LR120    DS    0H                                                               
         ZIC   R1,LISTNO                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,LISTNO                                                        
         MVC   LISTAR,SPACES                                                    
         MVC   LPRDC2,LPTRC                                                     
         MVC   LPTRC,SPACES                                                     
         GOTO1 LISTMON                                                          
                                                                                
LR130    DS    0H                                                               
         MVI   USEIO,C'N'                                                       
         MVC   KEY,SVKEY          RESTORE KEY                                   
         B     LR50               GET NEXT ELEMENT                              
         EJECT                                                                  
****************************                                                    
* OFF LINE REPORT        ***                                                    
****************************                                                    
                                                                                
LR180    LA    R5,P                                                             
         USING PRTLINE,R5                                                       
                                                                                
         GOTO1 MSUNPK,DMCB,(X'80',SVMSTA),PMKT,WORK                             
         MVC   PSTA(5),WORK                                                     
                                                                                
         CLC   WORK+5(3),SPACES    CABLE HEAD                                   
         BE    LR181                NO                                          
         MVI   PSTA+4,C'/'                                                      
         MVC   PSTA+5(3),WORK+5                                                 
         B     LR183                                                            
                                                                                
LR181    LA    R1,PSTA+4                                                        
         IC    R0,PSTA+4                                                        
         CLI   PSTA+3,C' '                                                      
         BH    LR182                                                            
         BCTR  R1,0                                                             
LR182    MVI   0(R1),C'-'                                                       
*MNMB                                                                           
         CLI   WORK+4,C'D'                                                      
         BNE   *+14                                                             
         MVC   1(1,R1),WORK+4                                                   
         B     *+10                                                             
*MNMB                                                                           
         MVC   1(1,R1),QMED                                                     
         CLI   QMED,C'R'                                                        
         BNE   LR183                                                            
         STC   R0,1(,R1)                                                        
                                                                                
LR183    CLC   LDATE(3),=C'BAD'                                                 
         BNE   LR184                                                            
         CLI   BDDDATE,1          IS IT THE RELEASE DATE?                       
         BNE   LR183D             NO                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),(5,PDATE)                                 
         MVC   P+147(16),=CL16'BAD RELEASE DATE'                                
         B     LR184X                                                           
                                                                                
LR183D   CLI   BDDDATE,2          IS IT THE RECALL DATE?                        
         BE    *+6                                                              
         DC    H'0'               ONLY CHOICES-BAD RELEASE OR RECALL            
         GOTO1 DATCON,DMCB,(2,ELDATE),(5,PDATE)                                 
         MVC   P+147(15),=CL15'BAD RECALL DATE'                                 
         B     LR184X                                                           
                                                                                
LR184    GOTO1 DATCON,DMCB,(2,ELDATE),(5,PDATE)                                 
                                                                                
LR184X   ZIC   R0,SVEST         ESTIMATE                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
                                                                                
         LH    R0,SVLINE          LINE                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINE,DUB                                                        
*                                 MOVE IN TIME                                  
         GOTO1 UNDAY,DMCB,SVDDAY,PDAY                                           
*                                 MOVE IN TIME                                  
         MVC   TMPTIME,SVDTIMST                                                 
         GOTO1 UNTIME,DMCB,TMPTIME,PTIME                                        
*                                 MOVE IN DAYPART                               
         MVC   PDAYPT,SVDDAYPT                                                  
                                                                                
         OC    SVPROD,SVPROD      IS THERE PRODUCT                              
         BZ    LR185                                                            
         MVC   PPRDS,SVPROD       MOVE IN PRODUCT                               
         LA    R1,PPRDS+3                                                       
         CLI   PPRDS+2,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'         MOVE - SPOT LENGTH                            
         EDIT  (B1,SVBSLN),(3,1(R1)),ALIGN=LEFT                                 
                                                                                
LR185    OC    SVPROD2,SVPROD2    IS THERE A PARTNER PROD                       
         BZ    LR190                                                            
         MVC   PPTRS,SVPROD2      MOVE IN PTR PROD                              
         LA    R1,PPTRS+3                                                       
         CLI   PPTRS+2,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'         MOVE IN - SPOT LENGTH                         
         EDIT  (B1,SVBSLN2),(3,1(R1)),ALIGN=LEFT                                
                                                                                
LR190    CLC   LPRDC,SPACES                                                     
         BE    LR193                                                            
         MVC   PCOMML,LPRDC       FUDGE FOR OFFLINE PRINT                       
         CLI   BADCPRD,1          IS THIS PCOMML DELETED?                       
         BNE   *+14               NO                                            
         MVC   P+213(18),=CL18'COMMERCIAL DELETED'                              
         B     LR193                                                            
         CLC   =C'REASSIGN',PCOMML                                              
         BNE   LR193                                                            
         TM    CMLERR,BADLEN                                                    
         BZ    *+14                                                             
         MVC   P+213(31),=C'SPOT LEN DIFFERENT FROM CML LEN'                    
         B     LR193                                                            
         TM    CMLERR,NOAIR                                                     
         BZ    *+14                                                             
         MVC   P+213(24),=C'CML NOT APPROVED FOR AIR'                           
         B     LR193                                                            
         TM    CMLERR,MAXDTE                                                    
         BZ    *+14                                                             
         MVC   P+213(22),=C'CML MAX USE DATE ERROR'                             
         B     LR193                                                            
         TM    CMLERR,CADTE                                                     
         BZ    *+14                                                             
         MVC   P+213(26),=C'CLIENT APPROVAL DATE ERROR'                         
         B     LR193                                                            
         MVC   P+213(15),=C'PRODUCT CHANGED'                                    
                                                                                
LR193    XC    LPRDC,LPRDC                                                      
         CLC   LPTRC,SPACES                                                     
         BE    LR195                                                            
         MVC   PCOMML2,LPTRC                                                    
         CLI   BADCPRD,2          IS THIS PCOMML DELETED?                       
         BNE   *+14                                                             
         MVC   P+231(18),=CL18'COMMERCIAL DELETED'                              
         B     LR194                                                            
         CLC   =C'REASSIGN',PCOMML2                                             
         BNE   LR194                                                            
         CLC   PCOMML,PCOMML2                                                   
         BE    LR194                                                            
         TM    CMLERR,BADLEN                                                    
         BZ    *+14                                                             
         MVC   P+213(31),=C'SPOT LEN DIFFERENT FROM CML LEN'                    
         B     LR194                                                            
         TM    CMLERR,NOAIR                                                     
         BZ    *+14                                                             
         MVC   P+213(24),=C'CML NOT APPROVED FOR AIR'                           
         B     LR194                                                            
         TM    CMLERR,MAXDTE                                                    
         BZ    *+14                                                             
         MVC   P+213(22),=C'CML MAX USE DATE ERROR'                             
         B     LR194                                                            
         TM    CMLERR,CADTE                                                     
         BZ    *+14                                                             
         MVC   P+213(26),=C'CLIENT APPROVAL DATE ERROR'                         
         B     LR194                                                            
         MVC   P+213(15),=C'PRODUCT CHANGED'                                    
LR194    XC    LPTRC,LPTRC                                                      
                                                                                
LR195    MVC   SVKEY,KEY          SAVE KEY FOR GENCON                           
         XC    KEY(13),KEY                                                      
         L     R4,AIO                                                           
         USING BUYRECD,R4                                                       
         MVC   PPGM,BDPROGRM      MOVE IN PGM NAME                              
         MVC   KEY,SVKEY          RESTORE KEY                                   
                                                                                
         TM    SVPREF,X'80'        PRINTED ON INSTR YET                         
         BZ    LR200                NO                                          
         MVI   PPRTD,C'P'                                                       
                                                                                
LR200    MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         B     LR50               GET NEXT ELEMENT                              
                                                                                
FPROD    L     RF,ASVCLIST                                                      
FPROD10  CLC   3(1,R1),3(RF)                                                    
         BE    FPROD20                                                          
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    FPROD10                                                          
         LA    RF,=C'???'                                                       
FPROD20  MVC   0(3,R1),0(RF)                                                    
         BR    RE                                                               
                                                                                
HDHK     NTR1                                                                   
         MVC   H2+7(L'QMED),QMED                                                
         MVC   H2+12(L'MEDNM),MEDNM                                             
         MVC   H4+7(L'QCLT),QCLT                                                
         MVC   H4+12(L'CLTNM),CLTNM                                             
         GOTO1 DATCON,DMCB,(2,PERSTP),(5,H4+48)                                 
         MVI   H4+58,C'-'                                                       
         GOTO1 DATCON,DMCB,(2,PERENDP),(5,H4+61)                                
                                                                                
         TM    OPTFLAG,OPTACT      THIS AN ACTIVITY REPORT                      
         BZ    EXIT                                                             
                                                                                
         MVC   H3+47(15),=C'A C T I V I T Y'                                    
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
GETCMML  NTR1                                                                   
         MVI   BADCMML,0          TEMP FLAG-FOR PRD AND PTR CMML                
         MVI   BADFLAG,0          FINAL FLAG                                    
         MVI   BDDDATE,0          FLAG ERROR MSG FOR OFFLINE REPORT             
         MVI   BADCPRD,0          ERROR MSG FOR OFFLINE REPORT                  
         MVI   CMMLNO,1           SET FLAG - FIRST CMML                         
         MVC   SVKEY,KEY                                                        
                                                                                
         OC    SVSQ,SVSQ           IS THERE A COMML SEQ                         
         BZ    GETCM10                                                          
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM(3),BAGYMD & BCLT                                          
         MVC   CMLPSEQ+1(2),SVSQ                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFDIR'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GETCM10                                                          
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 GETREC                                                           
                                                                                
         MVC   LPRDC(8),5(R4)     CMLKCML                                       
                                                                                
         XC    GRCMLCM1,GRCMLCM1                                                
         XC    GRCMLAD1,GRCMLAD1                                                
         MVC   GRCMLCM1(8),5(R4)     CMLKCML                                    
                                                                                
         USING CMLRECD,R4                                                       
         MVI   ELCODE,X'A0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   GETCM03                                                          
                                                                                
         USING CMLADIEL,R6                                                      
         MVC   LPRDC,CMLADID                                                    
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    GETCM01                                                          
         MVC   GRCMLCM1(8),=C'SEE ADID'                                         
GETCM01  MVC   GRCMLAD1,CMLADID                                                 
                                                                                
GETCM03  DS    0H                                                               
         LA    R3,SVBPRD                                                        
         BAS   RE,DCMML                                                         
         CLI   BADCMML,0          WAS FLAG SET?                                 
         BE    GETCM05            NOPE                                          
         MVI   BADFLAG,1          SET FINAL FLAG                                
         MVI   BADCMML,0          RESET FOR SECOND ROUND (PTR)                  
                                                                                
GETCM05  OC    SVSQ2,SVSQ2                                                      
         BZ    GETCM20                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM,BAGYMD                                                    
         MVC   CMLPCLT,BCLT                                                     
         MVC   CMLPSEQ+1(2),SVSQ2                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFDIR'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GETCM10                                                          
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 GETREC                                                           
                                                                                
         MVC   LPTRC(8),5(R4)                                                   
                                                                                
         XC    GRCMLCM2,GRCMLCM2                                                
         XC    GRCMLAD2,GRCMLAD2                                                
         MVC   GRCMLCM2(8),5(R4)                                                
                                                                                
         USING CMLRECD,R4                                                       
         MVI   ELCODE,X'A0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   GETCM08                                                          
                                                                                
         USING CMLADIEL,R6                                                      
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    GETCM07                                                          
         MVC   LPTRC,CMLADID                                                    
         MVC   GRCMLCM2(8),=C'SEE ADID'                                         
GETCM07  MVC   GRCMLAD2,CMLADID                                                 
                                                                                
GETCM08  DS    0H                                                               
         LA    R3,SVBPRD2                                                       
         CLI   SVBPRD2,0           2 CML TO COVER 1 PROD                        
         BNE   *+8                 NO                                           
         LA    R3,SVBPRD                                                        
         MVI   CMMLNO,2           SET FLAG - TO CML2                            
         BAS   RE,DCMML                                                         
         CLI   BADCMML,1                                                        
         BNE   GETCM20                                                          
GETCM10  MVI   BADFLAG,1                                                        
                                                                                
GETCM20  MVC   KEY,SVKEY          RESTORE KEY                                   
         MVC   AIO,AIO1                                                         
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         B     EXIT                                                             
                                                                                
         DROP  R4                                                               
         EJECT                                                                  
**************************************************************                  
* DCMML CHECKS IF COMMERICAL ASSIGNMENT WAS DELETED OR                          
* IF RELEASE OR RECALL DATES WERE CHANGED - SO NO LONGER                        
* FALL WITHIN PERIOD DATES  - IF ANY OF THESE CONDITIONS                        
* ARE MET - THIS CMML REALLY MUST BE PRINTED -EVEN IF OPTFLAG                   
* IS =1 - BECAUSE HAVING A BAD CMML ASSIGN IS AS GOOD AS NOT                    
* HAVING ONE AT ALL                                                             
**************************************************************                  
DCMML    NTR1                                                                   
                                                                                
         MVI   CMLERR,0            INIT CML ERROR FLAG                          
                                                                                
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
                                                                                
         MVC   SVCMLRCL,CMLRCL                                                  
                                                                                
         GOTO1 DATCON,DMCB,(2,SVDATE),(3,TEMPDATE)  FTD                         
         GOTO1 DATCON,DMCB,(2,SVDATEX),(3,TEMPDTEX) +ROTATION                   
                                                                                
         CLC   CMLRLSE,TEMPDATE   RELEASE DATE BEFORE SPOT DATE                 
         BNH   DCM20                                                            
         CLC   CMLRLSE,TEMPDTEX   REL DTE BEFORE END DTE (PARTIAL COV)          
         BNH   DCM20                                                            
                                                                                
         MVC   LDATE(3),=C'BAD'                                                 
         MVI   BADCMML,1                                                        
         MVI   BDDDATE,1          BAD RELEASE DATE - FOR OFFLINE ONLY           
                                                                                
DCM20    CLC   CMLRCL,TEMPDATE                                                  
         BNL   DCM30              RECALL DATE                                   
         MVC   LDATE(3),=C'BAD'                                                 
         MVI   BADCMML,1                                                        
         MVI   BDDDATE,2          BAD RECALL DATE - FOR OFFLINE ONLY            
                                                                                
DCM30    TM    CMLSTAT,X'80'      DELETED?                                      
         BZ    DCM50                                                            
                                                                                
         CLI   CMMLNO,1           PRODUCT OR PTR?                               
         BNE   DCM40                                                            
         CLI   MODE,PRINTREP      ARE WE OFFLINE?                               
         BNE   DCM32              NO                                            
         MVI   BADCPRD,1          PRODUCT CMML IS DELETED                       
         B     DCM34              WANT TO SHOW THE BAD CMML ID                  
                                                                                
DCM32    MVC   LPRDC(8),=C'DEL-CMML'                                            
DCM34    MVI   BADCMML,1                                                        
         B     EXIT                                                             
                                                                                
DCM40    CLI   CMMLNO,2                                                         
         BE    *+6                                                              
         DC    H'0'               ONLY CAN BE PRD OR PTR                        
         CLI   MODE,PRINTREP      ARE WE OFFLINE ?                              
         BNE   DCM42              NO                                            
                                                                                
         MVI   BADCPRD,2          PTR CMML IS DELETED                           
         B     DCM44              WANT TO SHOW THE BAD CMML ID                  
                                                                                
DCM42    MVC   LPTRC(8),=C'DEL-CMML'                                            
DCM44    MVI   BADCMML,1                                                        
                                                                                
DCM50    CLI   SVBPRD2,0          IS THERE A SECOND PROD                        
         BE    DCM50A              NO                                           
         CLI   SVBSLN2,0          IS THERE A SECOND LEN                         
         BE    DCM50F              NO, 1ST MUST COVER BOTH                      
                                                                                
DCM50A   CLC   1(1,R3),CMLSLN      IS THIS CML SAME SPOT LENGTH                 
         BE    DCM52                YES                                         
         CLC   SVSQ,SVSQ2          SEE IF ONE CML COVERS BOTH                   
         BNE   DCM50B               NO,ERROR                                    
         ZIC   R0,1(R3)            SPOT LENGTH                                  
         AR    R0,R0               DOUBLE IT UP                                 
         CLM   R0,1,CMLSLN         SAME LENGTH ?                                
         BE    DCM52                YES                                         
         B     DCM50D                                                           
                                                                                
DCM50B   CLI   CMMLNO,1                                                         
         BNE   DCM50B2                                                          
                                                                                
         OC    SVSQ2,SVSQ2         P/B CML?                                     
         BZ    DCM50D                                                           
                                                                                
         CLI   SVBPRD2,0           YES, BUT ONLY 1 PROD                         
         BNE   DCM50D               NO,ERROR                                    
                                                                                
         MVC   SVCMSLN,CMLSLN      SAVE CML1 LEN                                
                                                                                
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM,BAGYMD                                                    
         MVC   CMLPCLT,BCLT                                                     
         MVC   CMLPSEQ+1(2),SVSQ2                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFDIR'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   DCM50D                                                           
                                                                                
         DROP  R1                                                               
                                                                                
         L     R1,AIO3                                                          
         ST    R1,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'  SWITCH TO SPOT TRAFFIC SYSTEM             
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO2            RESET BACK TO AIO2                           
                                                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
                                                                                
         ZIC   RE,SVCMSLN          CML1 LEN                                     
         ZIC   RF,CMLSLN           CML2 LEN                                     
         AR    RE,RF                                                            
         CLM   RE,1,SVBSLN                                                      
         BE    DCM52                                                            
         BNE   DCM50D                                                           
                                                                                
DCM50B2  ZIC   RE,SVCMSLN          CML1 LEN                                     
         ZIC   RF,CMLSLN           CML2 LEN                                     
         AR    RE,RF                                                            
         CLM   RE,1,SVBSLN                                                      
         BE    DCM52                                                            
                                                                                
DCM50D   OI    CMLERR,BADLEN                                                    
         B     DCM60                                                            
                                                                                
DCM50F   ZIC   R0,SVBSLN           GET TOTAL LEN                                
         ZIC   R1,SVBSLN2                                                       
         AR    R1,R0                                                            
         OI    CMLERR,BADLEN                                                    
         CLM   R1,1,CMLSLN         THIS EQUAL BOTH                              
         BNE   DCM60                                                            
         NI    CMLERR,X'FF'-BADLEN                                              
                                                                                
         DROP  R6                                                               
                                                                                
DCM52    DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
                                                                                
         CLI   CMLPRDS,X'FF'        ALL PRDS                                    
         BE    DCM100              MORE VALIDATION FOR STARCOM                  
                                                                                
         ZIC   R0,1(R6)            ELEM LEN                                     
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,2(,R6)           FIRST PRD                                    
DCM55    CLC   0(1,R3),0(R1)       DO PRDS MATCH (BUY/CML)                      
         BE    DCM100                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,DCM55                                                         
DCM60    CLI   CMMLNO,1                                                         
         BNE   *+18                                                             
         MVC   LPRDC(8),=C'REASSIGN'  BAD PRODUCT REASSIGN CML                  
         OI    CMLERR,REASGN1     REASSIGN CML 1, FOR OFFLINE REPORT            
         B     *+14                                                             
         MVC   LPTRC(8),=C'REASSIGN'                                            
         OI    CMLERR,REASGN2     REASSIGN CML 2, FOR OFFLINE REPORT            
         MVI   BADFLAG,1                                                        
         B     EXIT                                                             
                                                                                
* DO MORE CML VALIDATION (LEGAL APPROVALS AND DATES IF ANY)                     
                                                                                
DCM100   GOTO1 =A(VCMLAPR),RR=SPTRRR   VALIDATE CML APPROVALS                   
         B     EXIT                                                             
         EJECT                                                                  
BUYEL    CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
                                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
                                                                                
EXCEEDIO LA    R2,CONWHENH         FOR CURSOR POSN                              
*         SPOOL IGNORES CODE!                                                   
*         MVI   SPMODE,X'FE'        DELETE PARTIALLY GENERATED REPORT           
*         GOTO1 SPOOL,DMCB,(R8)                                                 
         CLI   SPOOLKEY,0          TEST REPORT GENERATED                        
         BE    TRAPERR              NO                                          
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    TRAPERR                                                          
         DC    H'0'                                                             
                                                                                
INVCMLER MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
NOESTER  MVI   ERROR,NOESTS                                                     
         B     TRAPERR                                                          
CMLNERR  MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
BDESTPR  MVI   ERROR,BADESTS                                                    
         B     TRAPERR                                                          
DLRLENER MVI   ERROR,INVTXTLN     TOO LONG                                      
         B     TRAPERR                                                          
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
                                                                                
NEEDPROD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * PRODUCT NEEDED *'                       
         LA    R2,TRAPRDH          PRODUCT                                      
         GOTO1 ERREX2                                                           
DLRTAGER XC    CONHEAD,CONHEAD                                                  
         LA    R2,TRACLTH          CLIENT HEADER                                
         MVC   CONHEAD(41),=C'* ERROR * SPOTS HAVE BEEN DEALER TAGGED *X        
               '                                                                
         GOTO1 ERREX2                                                           
         EJECT                                                                  
                                                                                
         TITLE 'T21665 - SPOT CMML LIST AND REPORT'                             
HEADINGS SSPEC H1,1,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'S P O T   C O M M E R C I A L   L I S T'                 
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H2,35,C'---------------------------------------'                 
         SSPEC H2,80,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,80,RUN                                                        
         SSPEC H5,80,PAGE                                                       
         SSPEC H5,90,REQUESTOR                                                  
         SSPEC H4,40,C'PERIOD'                                                  
         SSPEC H8,1,C'MARKET STATION  DATE     EST LIN DAY'                     
         SSPEC H8,42,C'TIME        PROGRAM NAME    D/P PRD/SLN'                 
         SSPEC H8,82,C'COMMERCIAL    PTR/SLN COMMERCIAL'                        
         SSPEC H9,1,C'------ -------  -------- --- --- -------'                 
         SSPEC H9,42,C'----------- --------------- --- -------'                 
         SSPEC H9,82,C'----------    ------- ----------'                        
         DC    X'00'                                                            
         DROP  RB,R7                                                            
         EJECT                                                                  
                                                                                
VCMLAPR  NMOD1 0,**VAPR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEMENT                   
                                                                                
         L     R6,AIO                                                           
         BAS   RE,GETELB                                                        
         BE    VAPR10                                                           
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BNE   VAPRXIT                                                          
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR        NOT APPROVED TO AIR                          
         B     VAPRERR                                                          
                                                                                
         USING CMLBBEL,R6                                                       
                                                                                
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'Y'       IS BRAND AGY=Y                               
         BNE   VAPRXIT              NO                                          
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
                                                                                
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         BNH   VAPR20                                                           
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
                                                                                
         CLC   SVCMLRCL,ELDATE     IS CML RECALL BEFORE FTD                     
         BNL   VAPR20                                                           
         MVC   DUB,SVCMLRCL                                                     
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,MAXDTE       BAD DATES                                    
         B     VAPRERR                                                          
                                                                                
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         BNE   VAPRXIT              NO, DONE                                    
                                                                                
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         BNZ   VAPR22                                                           
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,CADTE        BAD DATES                                    
         B     VAPRERR                                                          
                                                                                
                                                                                
VAPR22   CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         BE    VAPRXIT                                                          
                                                                                
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BE    VAPR24                                                           
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR                                                     
         B     VAPRERR                                                          
                                                                                
VAPR24   OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         BNZ   VAPR26                                                           
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR                                                     
         B     VAPRERR                                                          
                                                                                
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
                                                                                
VAPR26   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,CMLBBREF                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VAPR28                                                           
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR                                                     
         B     VAPRERR                                                          
                                                                                
VAPR28   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEMENT                   
                                                                                
         BAS   RE,GETELB                                                        
         BE    VAPR30                                                           
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR                                                     
         B     VAPRERR                                                          
                                                                                
         USING CMLBBEL,R6                                                       
                                                                                
VAPR30   CLI   CMLATAIR,C'Y'                                                    
         BE    VAPRXIT                                                          
                                                                                
         CLI   MODE,PRINTREP       ARE WE OFFLINE?                              
         BE    VAPROERR             YES, DO OFFLINE ERROR EXIT                  
                                                                                
         OI    CMLERR,NOAIR                                                     
                                                                                
* FOR ONLINE ERROR EXIT                                                         
                                                                                
VAPRERR  DS    0H                                                               
         CLI   CMMLNO,2            CML2                                         
         BE    *+18                                                             
         MVC   LPRDC(8),=C'REASSIGN' BAD PRODUCT REASSIGN CML                   
         OI    CMLERR,REASGN1     REASSIGN CML 1                                
         B     VAPRXIT                                                          
         MVC   LPTRC(8),=C'REASSIGN' BAD PRODUCT REASSIGN CML                   
         OI    CMLERR,REASGN2     REASSIGN CML 2                                
         B     VAPRXIT                                                          
                                                                                
* FOR OFFLINE ERROR EXIT                                                        
                                                                                
VAPROERR CLI   CMMLNO,2            CML2                                         
         BE    *+18                                                             
         MVC   LPRDC(8),=C'REASSIGN'  REASSIGN CML1                             
         OI    CMLERR,REASGN1                                                   
         B     VAPRXIT                                                          
         MVC   LPTRC(8),=C'REASSIGN'  REASSIGN CML2                             
         OI    CMLERR,REASGN2                                                   
VAPRXIT  XIT1                                                                   
                                                                                
GETELB   AH    R6,DATADISP                                                      
FIRSTELB CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTELB  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTELB                                                         
                                                                                
         DROP  R6,RB,RC                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD *                             
                                                                                
VPER     DS    0D                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         XC    SVPERDTS,SVPERDTS                                                
         MVI   BYTE,0                                                           
                                                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VPER70                                                           
                                                                                
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   QBEST,0             OR IF EST ENTERED                            
         BNE   VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,SVPERST)                                    
                                                                                
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
VPER14   BAS   RE,NEXTEL1                                                       
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX2                                                           
                                                                                
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVGENEND),(4,CONHEAD+21)                            
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX2                                                           
                                                                                
VPER30   CLI   QBEST,0             IS THERE AN ESTIMATE                         
         BE    VPER32                                                           
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVGENEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVGENST),SVQSTART                                   
         GOTO1 (RF),(R1),(3,SVGENEND),SVQEND                                    
         MVC   SVPERDTS,SVGENDTS                                                
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER90                                                           
                                                                                
VPER32   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
                                                                                
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
                                                                                
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
                                                                                
VPER40   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER50                                                           
                                                                                
         B     VPER90                                                           
                                                                                
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
                                                                                
VPER50   CLC   SVPERST,SVGENEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVPERST,SVGENST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
                                                                                
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BNZ   VPER54                                                           
         MVC   SVPEREND,SVGENEND   USE EST END DATE                             
         B     VPER56                                                           
                                                                                
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
                                                                                
VPER54   CLC   SVPEREND,SVGENEND   LAST TLCST MUST BE EST END                   
         BNE   ESTDTERR                                                         
                                                                                
VPER56   GOTO1 DATCON,DMCB,(3,SVPERST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
                                                                                
         B     VPER90                                                           
                                                                                
* SEE IF TA PROFILE IS SET (# OF WEEKS TO DISPLAY)                              
                                                                                
VPER70   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK) TODAY'S DATE                          
                                                                                
         MVC   SVDAY,WORK                                                       
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   0(R1),1            IS IT MONDAY?                                 
         BE    VPER75                                                           
         ZIC   R2,0(R1)                                                         
         BCTR  R2,0                                                             
         LNR   R0,R2              NEGATIVE (SUBTRACT TO GET TO MONDAY)          
         GOTO1 ADDAY,(R1),SVDAY,XDAY,(R0)                                       
         MVC   SVDAY,XDAY                                                       
         XC    XDAY,XDAY                                                        
VPER75   GOTO1 DATCON,DMCB,(0,SVDAY),(3,SVPERST)                                
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,SVTAPR14       # OF WEEKS TO SHOW                           
         BZ    VPERX                                                            
                                                                                
         MH    R0,=H'7'                                                         
         BCTR  R0,0                                                             
                                                                                
         GOTO1 ADDAY,(R1),SVDAY,WORK+8,(R0)                                     
         GOTO1 DATCON,DMCB,(0,WORK+8),(3,SVPEREND)                              
                                                                                
         MVI   BYTE,1              SHOW ACCORDING TO TA PROFLIE                 
                                                                                
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
                                                                                
VPER90   GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
                                                                                
VPERX    XIT1                                                                   
                                                                                
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX2                                                           
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERR1                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERR1                                                         
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR1                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
TRAPERR1 GOTO1 ERREX                                                            
         EJECT                                                                  
GETEL1   AH    R6,DATADISP                                                      
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
         LTORG                                                                  
         EJECT                                                                  
GETEL3   AH    R6,DATADISP                                                      
FIRSTEL3 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL3  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL3                                                         
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
*----------------------------------------------------------------------         
* GRID INTERFACE                                                                
*----------------------------------------------------------------------         
GRIDI    NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R1                                                            
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
*                                                                               
         L     RF,=V(GRID)                                                      
         A     RF,SPTRRR                                                        
         LA    R0,GRIDT                                                         
         GOTO1 ,DMCB,,ATWA,ASPOOLD,ACOMFACS                                     
*                                                                               
         SLL   R3,2                                                             
         B     *(R3)                                                            
         B     GRIDCLR                                                          
         B     GRIDINI                                                          
         B     GRIDPUT                                                          
         B     GRIDEND                                                          
         B     GRIDXIT                                                          
*                                                                               
GRIDCLR  GOTO1 (RF),DMCB,(C'C',(R0))                                            
         B     GRIDX                                                            
*                                                                               
GRIDINI  DS    0H                                                               
         OI    CONSERVH+6,X'81'                                                 
         NI    GSTAT,X'FF'-X'80'                                                
         CLI   1(RA),C'*'                                                       
         BNE   *+8                                                              
         OI    GSTAT,X'80'                                                      
         TM    PCGRIDS,PCGCOPQ                                                  
         BNO   GRIDOK                                                           
         TM    PCGRIDS,PCGFINQ                                                  
         BNO   GRIDOK                                                           
         B     GRIDEND                                                          
*                                                                               
GRIDPUT  GOTO1 (RF),DMCB,(R0)                                                   
         BE    GRIDX                                                            
         XR    R1,R1                                                            
         ICM   R1,3,LSTCNTR        REDUCE LIST COUNTER                          
         SHI   R1,1                                                             
         STCM  R1,3,LSTCNTR                                                     
         MVI   LISTNO,14                                                        
         B     GRIDXIT                                                          
*                                                                               
GRIDEND  GOTO1 (RF),DMCB,(C'E',(R0))                                            
         JNL   GRIDXIT                                                          
                                                                                
         OI    CONHEADH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD                                                  
         LA    R2,TRAMEDH                                                       
         MVC   CONHEAD(22),=C'EE#0547 NO UNITS FOUND'                           
         GOTO1 ERREX2                                                           
                                                                                
GRIDXIT  MVC   CONHEAD,GMSG                                                     
         OI    CONHEAD+6,X'80'                                                  
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX2                                                           
*                                                                               
GRIDNO   LTR   RB,RB                                                            
         B     GRIDX                                                            
GRIDOK   CR    RB,RB                                                            
GRIDX    J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
GRIDCLRQ EQU   1                   CLEAR                                        
GRIDINIQ EQU   2                   INIT                                         
GRIDPUTQ EQU   3                   PUT                                          
GRIDENDQ EQU   4                   END                                          
GRIDXITQ EQU   5                   XIT                                          
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* GRID COLUMN TABLE                                                             
*----------------------------------------------------------------------         
*                                                                               
TOGENDQ  EQU   SPOOLEND-SPOOLD                                                  
TOLISTQ  EQU   (LISTAR-GEND)+TOGENDQ                                            
TOSYSDQ  EQU   LIOALBLQ+(IO-GEND)+TOGENDQ                                       
*                                                                               
GRIDT    DC    CL3'650'                       GRID FORMAT ID                    
         DC    AL1(24)                        DEFAULT DISP TO 1ST ELEM          
         DC    AL1(8)                         DOR ADJUSTMENT                    
         DC    AL1(0)                         INDICATOR                         
         DC    AL2(GRDDAT1-T216FFD)           DISP TO FIRST LINE                
         DC    AL2(GRDDATL-T216FFD)           DISP TO LAST LINE                 
         DC    AL1(0)                         GENERAL INDICATOR                 
         DC    AL1(0)                         N/D                               
         DC    AL1(L'GRDDAT1)                 LENGTH OF LINE                    
         DC    AL1(GRDDAT2-GRDDAT1)           DISP BETWEEN LINES                
         DC    CL1' '                         NUMBER OF FIXED COLS-C''          
         DC    AL1(0)                         N/D                               
         DC    AL2(PCDRIVE-T216FFD)           TWA-PCDRIVEN                      
         DC    AL2((AIO1-GEND)+TOGENDQ)       W/S-A(IO AREA)                    
         DC    AL2((DLCB-SYSD)+TOSYSDQ)       W/S-DLCB                          
         DC    AL2(HEAD1-SPOOLD)              W/S-GSB                           
         DC    AL2((GCOSEL-SYSD)+TOSYSDQ)     W/S-COLUMN SELECTOR               
         DC    AL2((GMSG-SYSD)+TOSYSDQ)       W/S-MESSAGE OUTPUT AREA           
         DC    AL2((GSTAT-SYSD)+TOSYSDQ)      W/S-GRID STATUS BYTE              
         DC    AL1(2)                         SYSTEM FOR DICTATE                
         DC    AL1(0,0,0)                     N/D                               
*---------------------------------------------                                  
* GRID COLUMNS                                                                  
*---------------------------------------------                                  
         DC    CL3'MKT'   * MARKET *          COLUMN ID           +00           
         DC    AL1(GRCTTXT)                   DATA TYPE           +03           
         DC    AL1(0)                         FORMAT OPTIONS      +04           
         DC    AL1(0)                         GENERAL INDICATOR   +05           
         DCDDL SP#MRKT,4                      COLUMN NAME         +06           
         DC    AL2(TOLISTQ+(LMKT-LISTAR))     DISP TO DATA        +10           
         DC    AL1(L'LMKT)                    LENGTH OF DATA      +12           
         DC    AL1(GRCDWS)                    DATA INDICATOR      +13           
         DC    AL1(0)                         ELEMENT CODE        +14           
         DC    AL1(0)                         ELEMENT SUB-CODE    +15           
         DC    AL1(0)                         OVERRIDE KEY LENGTH +16           
         DC    AL1(0)                         SPLIT POSITION      +17           
         DC    AL1(0)                         COLUMN SELECTOR     +18           
         DC    AL1(0)                         COLUMN INDICATOR    +19           
         DC    AL2(0)                         N/D                 +20           
*                                                                               
         DC    CL3'STA'   * STATION *                                           
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#STATN,7                                                       
         DC    AL2(TOLISTQ+(LSTA-LISTAR))                                       
         DC    AL1(8)                                                           
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'EST'   * ESTIMATE *                                          
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#EST,3                                                         
         DC    AL1(0)                                                           
         DC    AL2(TOLISTQ+(LEST-LISTAR))                                       
         DC    AL1(L'LEST)                                                      
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'TIM'   * TIME *                                              
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#TIME,4                                                        
         DC    AL2(TOLISTQ+(LTIME-LISTAR))                                      
         DC    AL1(L'LTIME)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'PGM'   * PROGRAM NAME *                                      
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#PRG,7                                                         
         DC    AL2(TOLISTQ+(LPGM-LISTAR))                                       
         DC    AL1(L'LPGM)                                                      
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'PRD'   * PRD-SLN *                                           
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#PRDSL,7                                                       
         DC    AL2(TOLISTQ+(LPRDS-LISTAR))                                      
         DC    AL1(L'LPRDS)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'PRT'   * PTR-SLN *                                           
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#PTRSL,7                                                       
         DC    AL2(TOLISTQ+(LPTRS-LISTAR))                                      
         DC    AL1(L'LPTRS)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'DAT'   * DATE *                                              
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#DATE,4                                                        
         DC    AL2(TOLISTQ+(LDATE-LISTAR))                                      
         DC    AL1(L'LDATE)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'PRC'   * PRD-COMML *                                         
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#PRDCL,8                                                       
*        DC    AL2(TOLISTQ+(LPRDC-LISTAR))                                      
*        DC    AL1(L'LPRDC)                                                     
         DC    AL2(TOSYSDQ+(GRCMLCM1-SYSD))                                     
         DC    AL1(L'GRCMLCM1)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'PTC'   * PTR-COMML *                                         
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL SP#PTRCL,8                                                       
*        DC    AL2(TOLISTQ+(LPTRC-LISTAR))                                      
*        DC    AL1(L'LPTRC)                                                     
         DC    AL2(TOSYSDQ+(GRCMLCM2-SYSD))                                     
         DC    AL1(L'GRCMLCM2)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*GRID                                                                           
*        NEW COLUMNS                                                            
         DC    CL3'AD1'   * ADID      *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#ADID,8                                                        
         DC    AL2(TOSYSDQ+(GRCMLAD1-SYSD))                                     
         DC    AL1(L'GRCMLAD1)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'AD2'   * ADID      *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#ADID2,8                                                       
         DC    AL2(TOSYSDQ+(GRCMLAD2-SYSD))                                     
         DC    AL1(L'GRCMLAD2)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'DPT'   * DAYPART   *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#DAYPT,7                                                       
         DC    AL2(TOSYSDQ+(GRDPT-SYSD))                                        
         DC    AL1(L'GRDPT)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'RT '   * ROTATION  *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#ROTAT,8                                                       
         DC    AL2(TOSYSDQ+(GRROT-SYSD))                                        
         DC    AL1(L'GRROT)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'CS '   * COST      *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#ACOST,11                                                      
         DC    AL2(TOSYSDQ+(GRCOST-SYSD))                                       
         DC    AL1(L'GRCOST)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    CL3'MG '   * MAEKGOOD  *                                         
         DC    AL1(GRCTTXT,32,0)                                                
         DCDDL SP#MAKGD,8                                                       
         DC    AL2(TOSYSDQ+(GRMKGD-SYSD))                                       
         DC    AL1(L'GRMKGD)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*GRID                                                                           
         DC    X'FFFF'                                                          
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         TITLE 'T21640 - SPOT CMML LIST   - DSECTS'                             
*   INCLUDE DDSPOOLD                                                            
*   INCLUDE DDSPLWORKD                                                          
*   INCLUDE SPTRAFFD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRA75D                                                       
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRA9DD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
*&&DO                                                                           
         ORG   TWASTAT1+1                                                       
TSTFLG   DS    XL1                                                              
TSTSET   EQU   X'80'                                                            
*&&                                                                             
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
* TELECAST DATES                                                                
                                                                                
PERSTP   DS    H                   FLIGHT/TELECAST START DATE                   
PERENDP  DS    H                   FLIGHT/TELECAST END DATE                     
                                                                                
* FROM BUY ELEMENT                                                              
                                                                                
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
ROTDAYS  DS    H                                                                
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SVBSTA   DS    XL3                                                              
SVQSTA   DS    CL5                                                              
STAAFFL  DS    CL1                                                              
STATYPE  DS    CL1                                                              
SPOTNUMB DS    XL1                                                              
SPTWORK  DS    CL35                                                             
SPTRRR   DS    A                                                                
APTNSTRT DS    A                                                                
ASVSTOR  DS    A                                                                
TOPASVTB DS    A                   1ST TABLE ENTRY ON CURR SCREEN               
NXTASVTB DS    A                   1ST TABLE ENTRY ON NEXT SCREEN               
SVR6     DS    F                                                                
SVCMLPRS DS    CL60                15 CML SEQ PAIRS                             
CURRCML  DS    H                                                                
SVTAPR14 DS    CL1                 # OF WEEKS TO SHOW                           
SVTAPR15 DS    CL1                 SHOW COST                                    
SVT2PR9  DS    CL1                 BRAND AGY (Y/N)                              
*MNMB                                                                           
SVT3PR06 DS    CL1                 EXCLUDE DV/SM FROM TRAFFIC                   
*MNMB                                                                           
                                                                                
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
                                                                                
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
                                                                                
* FROM FLIGHT OR ESTIMATE RECORD                                                
                                                                                
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 INSTRUCTION START DATE                       
SVGENEND DS    XL3                 INSTRUCTION END DATE                         
TODAYP   DS    XL2                 TODAY PACKED - USED IN UPD RTN               
                                                                                
* KEEP ALL SVPROD TO SVBSLN2 TOGETHER AND IN ORDER *                            
                                                                                
SVPROD   DS    CL3                                                              
SVBPRD   DS    XL1                                                              
SVBSLN   DS    XL1                                                              
SVPROD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
SVBSLN2  DS    XL1                                                              
SVPRDFLG DS    XL1                                                              
                                                                                
LSTCNTR  DS    H                  # BUYS ALREADY PROCESSED                      
                                                                                
MYKEY    DS    CL32                                                             
TDAY     DS    CL6                TODAY'S DATE                                  
XDAY     DS    CL6                TODAY'S DATE + 60 DAYS                        
SVDAY    DS    CL6                TEMP SAVE TODAY'S DATE                        
TMPTIME  DS    XL4                                                              
SVSQ     DS    XL2                                                              
SVSQ2    DS    XL2                                                              
SVDLR    DS    XL2                                                              
SVPREF   DS    XL2                                                              
SVSPTFLG DS    XL1                 1 IF SPOT ASSIGN, 0 IF DEALER TAGS           
TEMPDATE DS    CL3                                                              
TEMPDTEX DS    CL3                                                              
SVDATE   DS    CL2                                                              
SVDATEX  DS    CL2                                                              
CMMLNO   DS    XL1                                                              
BADCMML  DS    XL1                                                              
                                                                                
BADFLAG  DS    XL1                                                              
                                                                                
BDDDATE  DS    XL1                                                              
BADCPRD  DS    XL1                                                              
SVCMLRCL DS    XL3                 CML RECALL DATE                              
CMLERR   DS    XL1                                                              
REASGN1  EQU   X'80'               REASSIGN CML1                                
REASGN2  EQU   X'40'               REASSIGN CML2                                
BADLEN   EQU   X'20'               BUY/CML LEN NOT EQUAL                        
NOAIR    EQU   X'10'               CML NOT APPROVED FOR AIR                     
MAXDTE   EQU   X'08'               MAX DATE ERROR                               
CADTE    EQU   X'04'               NO CLIENT APPROVAL DATE                      
*                                                                               
OPTFLAG  DS    XL1                IF OPTION WAS CHOOSEN                         
OPTACT   EQU   X'01'              1 - ACTIVITY LIST                             
OPTDSK   EQU   X'02'              2 - SHOW DISK ADDRESSES (DDS ONLY)            
OPTSEED  EQU   X'04'              4 - ACCEPT SEEDED AS OKAY                     
NOTFRST  DS    XL1                                                              
SVMSTA   DS    CL5                                                              
SVDTIMST DS    CL4                                                              
SVEST    DS    XL1                                                              
SVDDAY   DS    CL1                                                              
SVLINE   DS    H                                                                
SVDDAYPT DS    CL1                                                              
LISTNO   DS    XL1                                                              
SVCMSLN  DS    XL1                 SAVE CML LEN                                 
*                                                                               
GCOSEL   DS    X                   GRID COLUMN SELECTOR                         
GSTAT    DS    X                   GRID STATUS INDICATOR                        
GMSG     DS    CL60                GRID MESSAGE COMMAND                         
DLCB     DS    XL256               DOWNLOAD CONTROL BLOCK                       
SVCMLAD1 DS    CL12                                                             
SVCMLAD2 DS    CL12                                                             
                                                                                
*GRCMLCM1 DS    CL12                                                            
*GRCMLAD1 DS    CL12                                                            
*GRCMLCM2 DS    CL12                                                            
*GRCMLAD2 DS    CL12                                                            
                                                                                
LPTRC    DS    CL12                                                             
*GRID    GRIDS FIELDS                                                           
GRFSTR   DS    0C                                                               
GRDPT    DS    CL1                 DAYPART CODE                                 
GRROT    DS    CL7                 ROTATION                                     
GRMKGD   DS    CL8                 MAKEGOOD                                     
GRADID   DS    CL12                ADID                                         
         DS    CL5                 ROOM FOR EDIT                                
GRCOST   DS    CL10                COST                                         
GRCMLCM1 DS    CL12                                                             
GRCMLAD1 DS    CL12                                                             
GRCMLCM2 DS    CL12                                                             
GRCMLAD2 DS    CL12                                                             
GRFLEN   EQU   *-GRFSTR            GRID FIELD BLOCK LENGTH                      
*GRID                                                                           
         EJECT                                                                  
* OFFLINE PRINT                                                                 
                                                                                
PRTLINE  DSECT                                                                  
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL6                                                              
         DS    CL3                                                              
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLINE    DS    CL3                                                              
         DS    CL1                                                              
PDAY     DS    CL7                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PPGM     DS    CL15                                                             
         DS    CL2                                                              
PDAYPT   DS    CL1                                                              
         DS    CL2                                                              
PPRDS    DS    CL7                                                              
         DS    CL1                                                              
PCOMML   DS    CL12                                                             
         DS    CL1                                                              
PPTRS    DS    CL8                                                              
         DS    CL1                                                              
PCOMML2  DS    CL12                                                             
         DS    CL1                                                              
PPRTD    DS    CL1                                                              
                                                                                
* ONLINE LIST                                                                   
                                                                                
NLISTQ   EQU   14                  NUMBER ON LIST SCREEN                        
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMKT     DS    CL4                                                              
         DS    CL1                                                              
LSTA     DS    CL5                                                              
         DS    CL4                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LTIME    DS    CL6                                                              
         DS    CL1                                                              
LPGM     DS    CL12                                                             
         DS    CL1                                                              
LPRDS    DS    CL7                                                              
LPRTD    DS    CL1                                                              
LPTRS    DS    CL7                                                              
         DS    CL1                                                              
LDATE    DS    CL5                                                              
         DS    CL1                                                              
LPRDC    DS    CL12                                                             
         ORG   LPRDC+2                                                          
LPRDC2   DS    CL12                                                             
**LPTRC    DS    CL8                                                            
         EJECT                                                                  
* INCLUDE SPDDEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPTRA65   02/17/16'                                      
         END                                                                    
