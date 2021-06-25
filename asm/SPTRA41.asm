*          DATA SET SPTRA41    AT LEVEL 032 AS OF 03/04/19                      
*PHASE T21641C                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE KHDUMMY                                                                
*        TITLE 'T21641 GOAL LIST AND REPORT'                                    
***********************************************************************         
*                                                                     *         
*        AIO 1 - GOAL RECS                                            *         
*            2 - TBUY RECS IF ACTIVITY                                *         
*            3 - CLIENT/MARKET                                        *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE       *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE REG                                         *         
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
***********************************************************************         
*                                                                     *         
*  LEV  8     JUL07/89 ADD OFFICE                                     *         
*  LEV 18-20  SEP12/89 FIX ALPHA ORDER PROD BUG (1 PROD)              *         
*  LEV 21     OCT31/89 FIX READ PROFILE BUG (USED O, NOT 0)           *         
*  LEV 22     MAR11/91 FIX READ PAST AGENCY                           *         
*  LEV 23     APR04/91 FIX OFFCD NOT CLEARED                          *         
*  LEV 24     MAY12/93 ADD TRAFFIC SYSTEM                             *         
*  LEV 25     JUN16/93 FIX TRAFFIC BUG - READING OFFICE               *         
*  LEV 26     JUL21/94 CHANGE TO FILENAME                             *         
*  LEV 26     NOV06/96 CHANGE FOR PRINTING OFFICE CODE-SPRI           *         
*  LEV 27 SMUR APR04/01 USE TRAFFIC OFFICE                            *         
*  LEV 28 SMUR JUN26/02 CLIENT STRING SECURITY                        *         
*  SPEC-33288 MHER MAR04/19 CHK FOR MODE SETFILE TO SWITCH TO SPT     *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21641 GOAL LIST AND REPORT'                                    
T21641   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GOAL**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTRRR                                                        
         MVI   NLISTS,14           ONLY 14 LINES ON LIST SCREEN                 
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,SETFILE                                                     
         JNE   EXIT                                                             
         CLI   CONREC,C'G'         IF DOING GOALS                               
         JNE   EXIT                                                             
         MVC   SYSDIR,=C'SPTDIR'   TELL GENCON TO READ SPTFILE!                 
         MVC   SYSFIL,=C'SPTFIL'                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         OI    GENSTAT3,MULTFILS   PROG READS SPT AND TRF                       
         LA    R2,TRAMEDH          MEDIA HEADER                                 
         TM    TRAMEDH+4,X'20'     MEDIA CHANGED?                               
         BO    VKCLT               NO                                           
         NI    TRACLTH+4,X'DF'                                                  
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    MISSERR             MEDIA IS REQUIRED                            
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VKCLT    LA    R2,TRACLTH          CLIENT HEADER                                
         TM    TRACLTH+4,X'20'     CLIENT CHANGED?                              
         BO    VKPRD               NO                                           
         NI    TRAPRDH+4,X'DF'                                                  
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    MISSERR             CLIENT IS REQUIRED                           
         XC    BCLT,BCLT                                                        
         XC    ASVNXPRD,ASVNXPRD                                                
         XC    OFFCD,OFFCD                                                      
         XC    OBCLT,OBCLT                                                      
         SPACE                                                                  
         CLI   8(R2),C'*'          REQUEST BY OFFICE                            
         BE    VKCLT10                                                          
         CLI   8(R2),C'$'          REQUEST BY OFFICE                            
         BE    VKCLT10                                                          
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GO GET PROFILE RECORD(S)                     
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     VKPRD                                                            
         SPACE                                                                  
VKCLT10  CLI   5(R2),2             MUST BE LEN OF 2                             
         BNE   OFFLNERR                                                         
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VKCLT20              NO                                          
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
VKCLT20  MVC   OFFCD,9(R2)                                                      
         BAS   RE,VOFF             GO VALIDATE OFFICE                           
         BNE   OFFERR                                                           
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VKPRD    LA    R2,TRAPRDH          PRODUCT                                      
         TM    TRAPRDH+4,X'20'     PRODUCT CHANGED                              
         BO    VKPTR               NO                                           
         XC    BPRD(4),BPRD                                                     
         MVC   QPRD,SPACES                                                      
         NI    TRAPTRH+4,X'DF'                                                  
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK20                PRODUCT IS NOT REQUIRED                      
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
         MVC   PRDNM,WORK+5                                                     
VK20     OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VKPTR    LA    R2,TRAPTRH          PARTNER                                      
         TM    TRAPTRH+4,X'20'     PARTNER CHANGED?                             
         BO    VKEST               NO                                           
         MVC   QPRD2,SPACES                                                     
         NI    TRAESTH+4,X'DF'                                                  
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
         SPACE                                                                  
VK26     MVI   BPRD2,255                                                        
         SPACE                                                                  
VK28     OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VKEST    LA    R2,TRAESTH          EST NUMBER                                   
         TM    TRAESTH+4,X'20'     ESTIMATE CHANGED?                            
         BO    VKPER               NO                                           
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
         NI    TRAPERH+4,X'DF'                                                  
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK30                                                             
         CLI   BPRD,0                                                           
         BE    NEEDPROD           HAVE ESTIMATE -MUST HAVE PROD                 
         GOTO1 VALINUM                                                          
         MVC   QBEST,ACTUAL        SET AS START EST                             
         MVC   QBESTEND,ACTUAL     AND AS END                                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   BDESTPR                                                          
         SPACE                                                                  
         CLI   BPRD2,0             ANY PTR PROD                                 
         BE    VKEST10                                                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   BDESTPR                                                          
         SPACE                                                                  
VKEST10  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         SPACE                                                                  
         USING ESTHDRD,R6                                                       
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVGENST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVGENEND)                                  
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
VK30     OI    4(R2),X'20'        SET ON VALIDATED                              
         DROP  R6                                                               
         SPACE                                                                  
VKPER    LA    R2,TRAPERH          PERIOD                                       
         TM    TRAPERH+4,X'20'     PERIOD CHANGED?                              
         BO    VKFLT               NO                                           
         XC    PERSTP,PERSTP                                                    
         XC    PERENDP,PERENDP                                                  
         NI    TRAFLTRH+4,X'DF'                                                 
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK50                PERIOD NOT REQUIRED                          
         GOTO1 =A(VPER),RR=SPTRRR   VALIDATE PERIOD/FLIGHT                      
         GOTO1 DATCON,DMCB,(2,PERSTP),(0,TDAY)                                  
         BAS   RE,GETMNDY                                                       
         GOTO1 DATCON,DMCB,(0,TDAY),(2,PERSTP)                                  
         SPACE                                                                  
* TEST IF PERIOD END WITHIN 15 WKS (105 DAYS)                                   
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,TDAY,XDAY,F'105'                                      
         GOTO1 DATCON,DMCB,(0,XDAY),(2,COMPDATE)                                
         CLC   COMPDATE,PERENDP                                                 
         BL    INVALPER                                                         
         B     VK58                                                             
         SPACE                                                                  
* USE TODAY'S DATE (NEAREST MONDAY) AS START PERIOD                             
         SPACE                                                                  
VK50     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TDAY)                                       
         BAS   RE,GETMNDY                                                       
         GOTO1 DATCON,DMCB,(0,TDAY),(2,PERSTP)                                  
         GOTO1 ADDAY,DMCB,TDAY,XDAY,F'98'                                       
         GOTO1 DATCON,DMCB,(0,XDAY),(2,PERENDP)                                 
         SPACE                                                                  
VK58     BAS   RE,SETTABLE                                                      
         SPACE                                                                  
VK60     OI    TRAPERH+4,X'20'                                                  
         SPACE                                                                  
VKFLT    LA    R2,TRAFLTRH                                                      
         TM    TRAFLTRH+4,X'20'      OPTIONS CHANGED                            
         BO    VK76                                                             
         MVI   OPTFLAG,0                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK64                NO                                           
         BAS   RE,VOPT             GO VALIDATE OPTIONS                          
VK64     OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
***   BUILD KEY        ****                                                     
         SPACE                                                                  
VK76     LA    R4,KEY                                                           
         USING GKEY,R4                                                          
         XC    KEY,KEY                                                          
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
         MVC   GKEYPRD,BPRD                                                     
         MVC   GKEYEST,QBEST                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
********************************************                                    
* ONLINE LIST OR OFFLINE REPORT ROUTINE    *                                    
********************************************                                    
         SPACE                                                                  
LR       LA    RF,HEADINGS                                                      
         ST    RF,SPECS                                                         
         LA    RF,HDHK                                                          
         ST    RF,HEADHOOK                                                      
         MVC   AIO,AIO1                                                         
         LA    R4,KEY                                                           
         SPACE                                                                  
         MVC   SYSDIR(3),=C'SPT'  SWITCH TO SPOT SYSTEM                         
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
         OC    KEY(13),KEY                                                      
         BNZ   LR06               NOT FIRST TIME                                
         SPACE                                                                  
         XC    SVBCLT,SVBCLT                                                    
         XC    SVMKT,SVMKT                                                      
         XC    SVPROD,SVPROD                                                    
         MVI   SVBPRD,0                                                         
         XC    SVPROD2,SVPROD2                                                  
         MVI   SVBPRD2,0                                                        
         SPACE                                                                  
         USING GKEY,R4                                                          
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM(3),BAGYMD  & BCLT                                         
         MVC   GKEYPRD,BPRD                                                     
         MVC   GKEYEST,QBEST                                                    
         SPACE                                                                  
         CLI   OFFCD,0             THIS REQUEST BY OFFICE                       
         BE    LR00                 NO                                          
         MVC   GKEYCLT,OBCLT                                                    
         MVC   SVBCLT,KEY+2                                                     
         BAS   RE,FCLT             GET CLT HDR, PROFILES, CLIST                 
         BE    LR24                T/A PROFILE BYPASS                           
         SPACE                                                                  
LR00     CLI   BPRD,0             WAS PRODUCT ENTERED?                          
         BNE   LR06                YES                                          
         BAS   RE,PSQ             GET PRODS IN ALPHA SEQ                        
         MVC   SVBPRD,KEY+4                                                     
         SPACE                                                                  
LR06     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR16                                                             
         SPACE                                                                  
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                REREAD KEY FOR GENCON                        
         SPACE                                                                  
LR14     MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         SPACE                                                                  
LR16     LA    R4,KEY                                                           
         CLC   KEY(2),KEYSAVE                                                   
         BNE   LRX                                                              
         TM    KEY+11,X'80'                                                     
         BO    LR14                PASSIVE PTR - SKIP IT                        
         SPACE                                                                  
         MVI   DUB,02                                                           
         MVC   DUB+1(1),BAGYMD                                                  
         CLC   KEY(2),DUB                                                       
         BNE   LRX                                                              
         SPACE                                                                  
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR20                 NO                                          
         CLC   BCLT,KEY+2                                                       
         BE    LR26                                                             
         SPACE                                                                  
LRX      MVC   SYSDIR(3),=C'TRF'  SWITCH TO SPOT SYSTEM                         
         MVC   SYSFIL(3),=C'TRF'                                                
         B     EXIT                                                             
         SPACE                                                                  
LR20     CLC   SVBCLT,KEY+2                                                     
         BE    LR22                                                             
         MVC   SVBCLT,KEY+2                                                     
         BAS   RE,FCLT             GET CLT HDR, PROFILES                        
         BE    LR06                T/A PROFILE BYPASS                           
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PSQ              GET PRODS IN ALPHA SEQ                       
         MVC   SVBPRD,KEY+4                                                     
         SPACE                                                                  
         B     LR06                                                             
         SPACE                                                                  
LR22     CLI   OFFCD,0             RUNNING BY OFFICE                            
         BE    LR26                 NO                                          
         CLC   OBCLT,KEY+2                                                      
         BE    LR26                                                             
LR24     GOTO1 =A(NOFF),RR=SPTRRR  GET NEXT CLIENT FOR OFFICE                   
         BNE   LRX                                                              
         MVC   SVBCLT,KEY+2                                                     
         BAS   RE,FCLT             GET CLT HDR, PROFILES                        
         BE    LR24                T/A PROFILE BYPASS                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GKEY,R4                                                          
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,OBCLT                                                    
         BAS   RE,PSQ              GET PRODS IN ALPHA SEQ                       
         SPACE                                                                  
         MVC   SVBPRD,KEY+4                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     LR06                                                             
         SPACE                                                                  
LR26     CLI   BPRD,0             WAS PRODUCT ENTERED?                          
         BE    LR28                NO -                                         
         CLC   GKEYPRD,BPRD       ARE THEY THE SAME PROD?                       
         BNE   LR14                NO GET NEXT                                  
         B     LR30                                                             
         SPACE                                                                  
* PRINT PRODS IN ALPHA SEQUENCE HERE *                                          
         SPACE                                                                  
LR28     CLC   SVBPRD,GKEYPRD                                                   
         BE    LR30                                                             
         BAS   RE,PSQ             FIND NEXT ALPHA PRODUCT                       
         SPACE                                                                  
         MVC   SVBPRD,KEY+4                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     LR06                                                             
         SPACE                                                                  
LR30     CLI   BSLN,0             WAS SPOT LENGTH ENTERED?                      
         BE    LR34                NO                                           
         CLC   GKEYSLN,BSLN       SAME SPOT LENGTH                              
         BNE   LR14                GET NEXT RECORD                              
LR34     MVC   SVBSLN,GKEYSLN                                                   
         SPACE                                                                  
         CLI   BPRD2,0            WAS PTR PROD ENTERED?                         
         BE    LR36                NO                                           
         CLC   GKEYPRD2,BPRD2     ARE THEY THE SAME PTR PROD?                   
         BNE   LR14                NO GET NEXT                                  
         SPACE                                                                  
LR36     LA    R1,SVPROD                                                        
         MVC   3(1,R1),GKEYPRD                                                  
         BAS   RE,FPROD           FIND PRODUCT NAME FROM CODE                   
         XC    SVPROD2,SVPROD2                                                  
         CLI   GKEYPRD2,0                                                       
         BE    LR40                                                             
         LA    R1,SVPROD2                                                       
         MVC   3(1,R1),GKEYPRD2                                                 
         BAS   RE,FPROD                                                         
         SPACE                                                                  
LR40     ZIC   RE,GKEYSLN                                                       
         ZIC   RF,GKEYSEC                                                       
         SR    RF,RE              TOT SECS - SPT LEN = 2ND SPT LEN              
         STC   RF,SVBSLN2                                                       
         CLI   BSLN2,0            WAS 2ND SPOT LENGTH ENTERED?                  
         BE    LR44                                                             
         CLC   SVBSLN2,BSLN2                                                    
         BNE   LR14                                                             
         SPACE                                                                  
LR44     CLI   QBEST,0            ANY ESTIMATE                                  
         BE    LR46                                                             
         CLC   QBEST,GKEYEST      ARE THEY THE SAME                             
         BNE   LR14                                                             
LR46     ZIC   R0,GKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST,DUB                                                        
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING GKEY,R6                                                          
         TM    GCNTRLS,X'80'      TEST DELETED RECORD                           
         BO    LR14                GET NEXT RECORD                              
         DROP  R6                                                               
         SPACE                                                                  
* TEST IF ELEMENT DATES ARE WITHIN PERIOD REQUESTED                             
* IF ANY ELEMENT WITHIN GOAL RECORD FALLS WITHIN PERIOD                         
* GOAL RECORD PRINTS - MOVE DATES INTO TABLE IN DATE ORDER                      
         SPACE                                                                  
         SR    R5,R5              COUNTER FOR # DATES IN TABLE                  
         LA    R3,DTTABLE         DATE TABLE                                    
         XC    0(32,R3),0(R3)                                                   
         MVC   GOALFRST(4),=XL4'FFFF0000'                                       
         MVI   ELCODE,X'21'       ELEMENT FOR DATES                             
         BAS   RE,GETEL                                                         
         BNE   LR14                                                             
         SPACE                                                                  
LR60     CLC   2(2,R6),PERSTP       FRST ELE DATE<PERIOD STRT                   
         BL    LR66                 CHECK END DATE                              
         CLC   2(2,R6),PERENDP      FIRST DATE > END DATE                       
         BH    LR66                 GET NEXT RECORD                             
         SPACE                                                                  
         CLC   GOALFRST,2(R6)                                                   
         BNH   *+10                                                             
         MVC   GOALFRST,2(R6)                                                   
         CLC   GOALLAST,2(R6)                                                   
         BNL   *+10                                                             
         MVC   GOALLAST,2(R6)                                                   
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,TDAY)                                   
         BAS   RE,GETMNDY                                                       
         GOTO1 DATCON,DMCB,(0,TDAY),(2,0(R3))                                   
         LA    R5,1(R5)                                                         
         LA    R3,2(R3)           INCREMENT TABLE                               
LR66     BAS   RE,NEXTEL                                                        
         BE    LR60               LOOP FOR NEXT ELEMENT                         
         SPACE                                                                  
LR70     LTR   R5,R5                                                            
         BZ    LR14                                                             
         XC    DMCB(24),DMCB                                                    
         GOTO1 XSORT,DMCB,(0,DTTABLE),(R5),2,0                                  
         SPACE                                                                  
         MVC   LISTAR,SPACES                                                    
         LA    R3,DTTABLE                                                       
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,GOALFRST),(5,LDATES)                              
         MVI   LDATES+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(2,GOALLAST),(5,LDATES+9)                            
         SPACE                                                                  
         CLI   OPTFLAG,0                                                        
         BE    LR80                 LIST ALL GOALS                              
         LA    R5,P                                                             
         USING PRTLINE,R5                                                       
         SPACE                                                                  
* ROUTINE RDTBUY ONLY FOR FILTER - ACTIVITY                                     
         SPACE                                                                  
         MVC   PACTV(56),SPACES                                                 
         BAS   RE,RDTBUY                                                        
         CLC   PACTV(56),SPACES   LIST THIS RECORD?                             
         BE    LR14               DON'T LIST THIS ONE -IT'S COVERED             
         SPACE                                                                  
LR80     L     R4,AIO1                                                          
         USING GKEY,R4                                                          
         SPACE                                                                  
         EDIT  GKEYMKT,(4,LMKT)   MARKET NUMBER                                 
         SPACE                                                                  
         CLC   SVMKT,GKEYMKT                                                    
         BE    LR82                                                             
         MVC   SVMKT,GKEYMKT                                                    
         BAS   RE,FMKT            MARKET NAME                                   
         SPACE                                                                  
LR82     MVC   LMKTNM,MKTNM                                                     
         MVC   LDAYPT,GKEYDPT                                                   
         SPACE                                                                  
         OC    SVPROD,SVPROD      ANY PRODUCT?                                  
         BZ    LR85               NO                                            
         MVC   LPRDSLN(3),SVPROD  PRIMARY PRODUCT                               
         LA    R1,LPRDSLN+3                                                     
         CLI   LPRDSLN+2,C' '                                                   
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'           MOVE - SPOT LENGTH                          
         EDIT  (B1,SVBSLN),(3,1(R1)),ALIGN=LEFT                                 
         SPACE                                                                  
LR85     OC    SVPROD2,SVPROD2    ANY PARTNER?                                  
         BZ    LR90                                                             
         MVC   LPTRSLN(3),SVPROD2   PARTNER PRODUCT                             
         LA    R1,LPTRSLN+3                                                     
         CLI   LPTRSLN+2,C' '                                                   
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'         MOVE IN - SPOT LENGTH                         
         EDIT  (B1,SVBSLN2),(3,1(R1)),ALIGN=LEFT                                
         SPACE                                                                  
LR90     MVC   LEST,SVEST       ESTIMATE                                        
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BE    LRR                                                              
         SPACE                                                                  
         MVC   SYSDIR(3),=C'TRF'  SWITCH TO TRAFFIC SYSTEM                      
         MVC   SYSFIL(3),=C'TRF'                                                
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         SPACE                                                                  
         MVC   SYSDIR(3),=C'SPT'  SWITCH TO SPOT SYSTEM                         
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
         B     LR14               GET NEXT RECORD                               
         EJECT                                                                  
****************************                                                    
* OFF LINE REPORT        ***                                                    
****************************                                                    
         SPACE                                                                  
LRR      DS    0H                                                               
         LA    R5,P                                                             
         USING PRTLINE,R5                                                       
         MVC   PMKT(PEST+3-PMKT),LMKT    SAME AS ONLINE LISTING                 
         SPACE                                                                  
         CLI   OPTFLAG,0                                                        
         BNE   LRR20                                                            
         SPACE                                                                  
*  MARK WITH '*' GOAL ACTIVITY FOR PROPER WEEK (IF NO FILTER)                   
         SPACE                                                                  
         BAS   RE,MARKGOAL                                                      
         SPACE                                                                  
LRR20    MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR10               GET NEXT RECORD                               
         EJECT                                                                  
* VOPT - VALIDATES FILTER FIELD IN KEY                                          
         SPACE                                                                  
         DS    0H                                                               
VOPT     NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VOPTCLC                                                       
         BNE   VOPTERR                                                          
         MVI   OPTFLAG,1                                                        
         B     EXIT                                                             
         SPACE                                                                  
VOPTCLC  CLC   8(0,R2),=CL9'ACTIVITY'                                           
         SPACE 2                                                                
* GETMNDY - EXIT WITH TDAY (6 BYTE-MONDAY) *                                    
         SPACE                                                                  
         DS    0H                                                               
GETMNDY  NTR1                                                                   
         MVC   SVDAY,TDAY                                                       
         GOTO1 GETDAY,DMCB,TDAY,WORK                                            
         CLC   WORK(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),1            IS IT A MONDAY?                               
         BE    EXIT                                                             
         SPACE                                                                  
         ZIC   R2,0(R1)                                                         
         BCTR  R2,0                                                             
         LNR   R0,R2                                                            
         GOTO1 ADDAY,DMCB,SVDAY,TDAY,(R0)                                       
         B     EXIT                                                             
         SPACE                                                                  
* SETTABLE -- STORES IN DTTABLE2 EVERY MONDAY IN REQUEST PERIOD                 
         SPACE                                                                  
         DS    0H                                                               
SETTABLE NTR1                                                                   
         LA    R6,15              MAX ENTRIES                                   
         LA    R5,DTTABLE2        MONDAY PERIOD DATES                           
         XC    0(32,R5),0(R5)                                                   
         MVC   COMPDATE,PERSTP                                                  
SET10    MVC   0(2,R5),COMPDATE                                                 
         GOTO1 DATCON,DMCB,(2,COMPDATE),(0,TDAY)                                
         GOTO1 ADDAY,DMCB,TDAY,SVDAY,F'7'                                       
         GOTO1 DATCON,DMCB,(0,SVDAY),(2,COMPDATE)                               
         LA    R5,2(R5)                                                         
         CLC   COMPDATE,PERENDP                                                 
         BH    SET20                                                            
         BCT   R6,SET10                                                         
SET20    LA    RE,15                                                            
         LTR   R6,R6                                                            
         BZ    SET30                                                            
         BCTR  R6,0                                                             
         SR    RE,R6                                                            
SET30    STC   RE,TBL2CNTR                                                      
         B     EXIT                                                             
         EJECT                                                                  
* PRINT PRODUCTS IN ALPHA ORDER *                                               
         SPACE                                                                  
         DS    0H                                                               
PSQ      NTR1                                                                   
         L     R3,ASVNXPRD                                                      
         LTR   R3,R3               1ST TIME IN                                  
         BZ    PSQ10                YES                                         
         AR    R3,R9                                                            
         B     PSQ24                                                            
         SPACE                                                                  
PSQ10    L     R3,ASVCLIST                                                      
         SPACE                                                                  
PSQ20    CLI   0(R3),C' '          AT END OF LIST                               
         BNH   PSQ40                                                            
         CLC   =C'POL',0(R3)                                                    
         BNE   PSQ30                                                            
PSQ24    LA    R3,4(,R3)                                                        
         B     PSQ20                                                            
         SPACE                                                                  
PSQ30    MVC   QPRD,0(R3)                                                       
         MVC   KEY+4(1),3(R3)                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+1                                                 
         MVC   KEY+4(3),0(R3)                                                   
         SR    R3,R9                                                            
         ST    R3,ASVNXPRD                                                      
         GOTO1 HIGH                                                             
         LA    R1,=CL20'* UNKNOWN PRODUCT *'                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PSQ34                                                            
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
         LA    R1,PNAME                                                         
         SPACE                                                                  
PSQ34    MVC   PRDNM,0(R1)                                                      
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         CR    RB,RB                                                            
         B     PSQX                                                             
         SPACE                                                                  
PSQ40    MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         XC    ASVNXPRD,ASVNXPRD                                                
         LTR   RB,RB                                                            
         SPACE                                                                  
PSQX     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
         DS    0H                                                               
FPRO     NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         B     EXIT                                                             
         SPACE                                                                  
* CONVERT BINARY PRODUCT TO ALPHA *                                             
         SPACE                                                                  
FPROD    L     RF,ASVCLIST                                                      
FPROD10  CLC   3(1,R1),3(RF)                                                    
         BE    FPROD20                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    FPROD10                                                          
         DC    H'0'                                                             
FPROD20  MVC   0(3,R1),0(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
* FMKT - READS MARKET RECORD (GIVEN MARKET NUMBER) FOR MARKET NAME *            
         SPACE                                                                  
         DS    0H                                                               
FMKT     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(17),SPACES                                                   
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         LH    RF,SVMKT                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         SPACE                                                                  
         L     R5,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R5)                     
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME-MKTRECD(R5)                                           
         MVC   MKTNM,0(R1)                                                      
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* RDTBUY - ONLY FOR FILTER ACTIVITY - GOAL DATES ARE USED                       
* TO LOOK FOR ANY TBUY FOR ANY STATION IN THAT MARKET.  IF THE                  
* GOAL DATES ARE NOT COVERED THAT MARKET WILL APPEAR ON THE                     
* REPORT/ONLINE LIST                                                            
         SPACE                                                                  
         DS    0H                                                               
RDTBUY   NTR1                                                                   
         BAS   RE,MARKGOAL                                                      
         MVC   SVKEY,KEY          SAVE GENCON'S KEY                             
         SPACE                                                                  
         SPACE                                                                  
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING TBYKEY,R4          READ TBUY                                     
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM,BAGYMD                                                    
         MVC   TBYKCLT(5),SVKEY+2  BCLT, BPRD, BMKT                             
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFDIR'  SWITCH TO TRAFFIC SYSTEM                  
         GOTO1 HIGH                                                             
         B     RDT14                                                            
         DROP  R4                                                               
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'  SWITCH TO TRAFFIC SYSTEM                  
RDT10    GOTO1 SEQ                                                              
         SPACE                                                                  
RDT14    CLC   KEY(8),KEYSAVE                                                   
         BNE   RDTX               GOAL DATES NOT MEET - FLAG IT                 
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'  SWITCH TO TRAFFIC SYSTEM                  
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDT10              GOAL DATES NOT MEET - FLAG IT                 
         USING TBYDTAEL,R6                                                      
RDT20    CLI   SVKEY+9,0          ANY SPOT LENGTH IN GOAL REC?                  
         BE    RDT22                NO                                          
         CLC   TBYSLN,SVKEY+9     SAME SPOT LENGTH?                             
         BNE   RDT40                                                            
RDT22    CLI   SVKEY+12,0         ANY PTR-PROD IN GOAL REC?                     
         BE    RDT24                                                            
         CLC   TBYPRD2,SVKEY+12   SAME PTR-PRD?                                 
         BNE   RDT40                                                            
         ZIC   RE,SVKEY+10                                                      
         ZIC   RF,SVKEY+9                                                       
         SR    RE,RF                                                            
         ZIC   R1,TBYSLN2                                                       
         CR    R1,RE              SAME PTR SPOT LENGTH?                         
         BNE   RDT40                                                            
         SPACE                                                                  
RDT24    CLI   SVPROF11,C'E'      IS THIS CLT BY EST                            
         BNE   RDT26                NO                                          
         CLC   TBYCODE,SVKEY+7     SAME EST                                     
         BNE   RDT40                                                            
         B     RDT28                                                            
         SPACE                                                                  
RDT26    CLI   SVPROF11,C'D'      IS THIS BUY BY DAYPART                        
         BE    *+12                 YES                                         
         CLI   SVPROF11,C'Y'      IS THIS BUY BY DAYPART                        
         BNE   RDT27                NO                                          
         CLC   TBYCODE,SVKEY+7     SAME EST                                     
         BNE   RDT40                                                            
         B     RDT28                                                            
         SPACE                                                                  
RDT27    CLI   TBYCODE,0           MUST BE NO CODE                              
         BNE   RDT40                                                            
         SPACE                                                                  
RDT28    LA    R4,DTTABLE         GOAL DATES                                    
         GOTO1 DATCON,DMCB,(3,TBYSTART),(0,TDAY)                                
         BAS   RE,GETMNDY                                                       
         GOTO1 DATCON,DMCB,(0,TDAY),(2,TSTART)                                  
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,TBYEND),(0,TDAY)                                  
         BAS   RE,GETMNDY                                                       
         GOTO1 DATCON,DMCB,(0,TDAY),(2,TEND)                                    
         SPACE                                                                  
RDT30    CLC   0(2,R4),TSTART                                                   
         BL    RDT37                                                            
         SPACE                                                                  
         CLC   0(2,R4),TEND                                                     
         BH    RDT37                                                            
         SPACE                                                                  
         BAS   RE,RMASTRK         GOAL COVERED -REMOVE C'*'                     
         SPACE                                                                  
RDT37    LA    R4,2(R4)           CHECK NEXT GOAL DATE                          
         CLC   0(2,R4),=XL2'0000'                                               
         BNE   RDT30              NOT END OF TABLE                              
         SPACE                                                                  
RDT40    BAS   RE,NEXTEL                                                        
         BE    RDT20                                                            
         B     RDT10                                                            
         SPACE                                                                  
RDTX     MVC   AIO,AIO1                                                         
         SPACE                                                                  
         XC    FILENAME,FILENAME   USE SPOT SYSTEM                              
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
* RMASTRK - REMOVES '*' FROM SPECIFIC GOAL DATE GIVEN IN R4                     
         SPACE                                                                  
         DS    0H                                                               
RMASTRK  NTR1                                                                   
         LA    R3,DTTABLE2        PERIOD DATES                                  
         LA    R6,PACTV           PRINT LINE                                    
AST10    CLC   0(2,R3),0(R4)      FIND GOAL COVERED                             
         BNE   AST20                                                            
         MVI   1(R6),C' '                                                       
         B     EXIT                                                             
AST20    LA    R6,L'PACTV(R6)                                                   
         LA    R3,2(R3)                                                         
         CLC   0(2,R3),=XL2'0000'                                               
         BNE   AST10                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
* MARKGOAL - PUTS '*' WHERE EVER THERE IS A GOAL - DATES IN DTTABLE *           
         SPACE                                                                  
         DS    0H                                                               
MARKGOAL NTR1                                                                   
         LA    R3,DTTABLE         GOAL ELEMENT DATES                            
MRKGL05  LA    R1,DTTABLE2        PERIOD DATES                                  
         LA    R6,PACTV           PRINT LINE                                    
MRKGL15  CLC   0(2,R3),0(R1)      ANY GOAL ACTV FOR THAT WEEK?                  
         BNE   MRKGL30                                                          
         MVI   1(R6),C'*'         WEEKS MATCH - MARK ACTIV WITH *               
         B     MRKGL35                                                          
         SPACE                                                                  
MRKGL30  LA    R1,2(R1)                                                         
         LA    R6,L'PACTV(R6)                                                   
         CLC   0(2,R1),=XL2'0000'                                               
         BNE   MRKGL15                                                          
         SPACE                                                                  
MRKGL35  LA    R3,2(R3)           NEXT GOAL ELEMENT                             
         CLC   0(2,R3),=XL2'0000'                                               
         BNE   MRKGL05                                                          
         B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE *                                                           
         SPACE                                                                  
         DS    0H                                                               
HDHK     NTR1                                                                   
         MVC   H2+9(L'QMED),QMED                                                
         MVC   H2+13(L'MEDNM),MEDNM                                             
         SPACE                                                                  
         CLI   OFFCD,0                                                          
         BE    HDHK00                                                           
         MVC   H3(6),=C'OFFICE'                                                 
         GOTO1 =V(OFFOUT),DMCB,TRACLT+1,HEXOUT,H3+9                             
         SPACE                                                                  
HDHK00   CLI   OPTFLAG,0                                                        
         BE    HDHK10                                                           
         MVC   H3+46(13),=CL13'TBUY ACTIVITY'                                   
HDHK10   MVC   H4+9(L'QCLT),QCLT                                                
         MVC   H4+13(L'CLTNM),CLTNM                                             
         MVC   H5+9(L'QPRD),QPRD PRODUCT                                        
         MVC   H5+13(L'CLTNM),PRDNM                                             
         GOTO1 DATCON,DMCB,(2,PERSTP),(5,H4+46)                                 
         MVI   H4+55,C'-'                                                       
         GOTO1 DATCON,DMCB,(2,PERENDP),(5,H4+57)                                
         LA    R3,H9+PACTV-PMKT                                                 
         LA    R4,H10+PACTV-PMKT                                                
         LA    R5,DTTABLE2        PERIOD TABLE OF DATES                         
HDHK20   GOTO1 DATCON,DMCB,(2,0(R5)),(4,HEADATE)                                
         MVC   0(3,R3),HEADATE                                                  
         MVC   0(2,R4),HEADATE+3                                                
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,2(R5)                                                         
         CLC   0(2,R5),=XL2'0000'  END OF TABLE?                                
         BNE   HDHK20                                                           
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
         DS    0H                                                               
FCLT     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+CKEYAM-CLTHDRD(1),BAGYMD                                     
         MVC   KEY+CKEYCLT-CLTHDRD(2),SVBCLT                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE  BYPASS IF NO CLIENT HEADER                  
         BE    FCLT10                                                           
         DC    H'0'                                                             
         SPACE                                                                  
FCLT10   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   CLTNM,CNAME-CLTHDRD(R6)                                          
         SPACE                                                                  
         MVC   SVCLTOFF,COFFICE-CLTHDRD(R4)                                     
         CLI   CTRAFOFC-CLTHDRD(R4),0                                           
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC-CLTHDRD(R4)                                    
         SPACE                                                                  
         CLI   TRACLT,C'$'         BY OFFICER OFFICE                            
         BNE   *+10                                                             
         MVC   SVOFFCD,SVCLTOFF                                                 
         SPACE                                                                  
         LA    R2,CLIST-CLTHDRD(R6)                                             
         SPACE                                                                  
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
* SEE IF CLIENT USES IS EXCLUDED (READ TA PROFILE) *                            
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TA'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         CLI   SVPROF+2,C'Y'       EXCLUDE CLIENT                               
         BNE   *+8                                                              
         MVI   KEY+5,X'FF'                                                      
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLI   SVPROF+2,C'Y'       EXCLUDE CLIENT                               
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
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
         B     EXIT                                                             
VOPTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'ONLY VALID OPTION = ACTIVITY'                     
         B     EREXIT2                                                          
INVALPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'INVALID PERIOD -MAX 15 WEEKS'                     
         B     EREXIT2                                                          
NEEDPROD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * PRODUCT NEEDED *'                       
         B     EREXIT2                                                          
OFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFMSG),OFFMSG                                         
         B     EREXIT2                                                          
OFFLNERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFLNMSG),OFFLNMSG                                     
         B     EREXIT2                                                          
DLRTAGER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(41),=C'* ERROR * SPOTS HAVE BEEN DEALER TAGGED *X        
               '                                                                
EREXIT2  GOTO1 ERREX2                                                           
         LTORG                                                                  
OFFLNMSG DC    C'* ERROR * OFFICE MUST BE * OR $ AND 1 CHARACTER *'             
OFFMSG   DC    C'* ERROR * NO CLIENTS FOUND FOR OFFICE *'                       
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
* VALIDATE OFFICE CODE (ONLY SUPPORTED IN OFFLINE REPORT) *                     
*              -READ CLIENT HEADER RECORDS TO BUILD                             
*               TABLE OF CLIENTS FOR REQUESTED OFFICE                           
         SPACE                                                                  
         DS    0H                                                               
VOFF     NMOD1 0,**VOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERRA                                                          
         SPACE                                                                  
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,0                                                            
         MVC   KEY+1(1),BAGYMD                                                  
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
         GOTO1 HIGH                                                             
         SPACE                                                                  
VOFF10   CLI   KEY,0               TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
         SPACE                                                                  
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
         SPACE                                                                  
VOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT SYSTEM                      
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
         SPACE                                                                  
         CLI   TRACLT,C'$'         USE OFFICER                                  
         BNE   VOFF40                                                           
         SPACE                                                                  
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
         SPACE                                                                  
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         DROP  R1                                                               
         SPACE                                                                  
         L     RF,DMCB                                                          
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),DMCB,DUB,ACOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   VOFF20                                                           
         MVC   SVOFFCD,SVCLTOFF                                                 
         B     VOFF44                                                           
         SPACE                                                                  
VOFF40   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
VOFF42   CLC   OFFCD,0(R1)         TEST RIGHT OFFICE                            
         BE    VOFF44                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VOFF42                                                        
         B     VOFF20                                                           
*                                                                               
         SPACE                                                                  
*OFF44   L     R4,ATWA             TEST FOR SECURITY BREACHES                   
*NOP     USING T216FFD,R4                                                       
         SPACE                                                                  
VOFF44   OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFFX                                                            
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    VOFF60                                                           
         CLI   T216FFD+6,C'$'          TEST OFFICE LOCKOUT                      
         BE    VOFF70                                                           
         SPACE                                                                  
         CLC   T216FFD+6(2),CKEYCLT    ELSE SINGLE CLIENT ACCESS                
         BNE   VOFF20                                                           
         B     VOFFX                                                            
         SPACE                                                                  
VOFF60   CLC   T216FFD+7(1),OFFCD     MATCH OFFICE CODE                         
         BNE   VOFF20                                                           
         SPACE                                                                  
VOFFX    MVC   OBCLT,CKEYCLT                                                    
         MVC   CLTNM,CNAME                                                      
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         XIT1                                                                   
         SPACE                                                                  
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
         SPACE                                                                  
VOFF70   XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         DROP  R1                                                               
         SPACE                                                                  
         L     RF,DMCB                                                          
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),DMCB,DUB,ACOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   VOFF20                                                           
         B     VOFFX                                                            
REPERRA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSGA),REPMSGA                                       
         GOTO1 ERREX2                                                           
REPMSGA  DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* GET NEXT CLIENT FOR THIS OFFICE CODE (ONLY IN OFFLINE REPORT) *               
*          END OF CLIENTS, RETURN NE COND CODE                                  
         SPACE                                                                  
NOFF     NMOD1 0,**NOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERR                                                           
         SPACE                                                                  
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),OBCLT      LAST CLIENT THIS OFFICE                      
         SPACE                                                                  
         GOTO1 HIGH                                                             
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         SPACE                                                                  
NOFF10   CLI   KEY,0               TEST CLIENT HEADER RECS                      
         BNE   NEQXIT                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NOFFXX                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
         SPACE                                                                  
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         B     NOFF10                                                           
         SPACE                                                                  
NOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         CLC   KEY(13),0(R6)                                                    
         BE    NOFF32                                                           
         USING CLTHDRD,R6                                                       
         GOTO1 GETREC                                                           
         SPACE                                                                  
NOFF32   DS    0H                                                               
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
         SPACE                                                                  
         CLI   TRACLT,C'$'         USING OFFICER AS FILTER                      
         BNE   NOFF34                                                           
         BAS   RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20               NOT OK                                      
         MVC   SVOFFCD,SVCLTOFF                                                 
         B     NOFF36                                                           
         SPACE                                                                  
NOFF34   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
NOFF35   CLC   OFFCD,0(R1)         TEST RIGHT OFFICE                            
         BE    NOFF35C                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,NOFF35                                                        
         B     NOFF20                                                           
*                                                                               
NOFF35C  CLC   SVCLTOFF,OFFCD      TEST DESIRED OFFICE CODE                     
         BNE   NOFF20                                                           
         SPACE                                                                  
*OFF36   L     R4,ATWA             TEST FOR SECURITY BREACHES                   
*NOP     USING T216FFD,R4                                                       
         SPACE                                                                  
NOFF36   OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    NOFFX                                                            
         SPACE                                                                  
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    NOFF60                                                           
         SPACE                                                                  
         CLI   T216FFD+6,C'$'          TEST OFFICE LOCKOUT                      
         BE    NOFF70                                                           
         SPACE                                                                  
         CLC   T216FFD+6(2),CKEYCLT   ELSE SINGLE CLIENT ACCESS                 
         BNE   NOFF20                                                           
         B     NOFFX                                                            
         SPACE                                                                  
NOFF60   CLC   T216FFD+7(1),OFFCD     MATCH OFFICE CODE                         
         BNE   NOFF20                                                           
         B     NOFFX                                                            
         SPACE                                                                  
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
         SPACE                                                                  
NOFF70   BAS   RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20                                                           
         SPACE                                                                  
NOFFX    MVC   OBCLT,CKEYCLT                                                    
         MVC   CLTNM,CNAME                                                      
NOFFXX   XIT1                                                                   
         SPACE                                                                  
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
         XIT1                                                                   
         SPACE                                                                  
* CHECK OFFICE TO BE VALID *                                                    
         SPACE                                                                  
COFF     NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         DROP  R1                                                               
         SPACE                                                                  
         L     RF,DMCB                                                          
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),DMCB,DUB,ACOMFACS                                           
         CLI   0(R1),0                                                          
         XIT1                                                                   
REPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSG),REPMSG                                         
         GOTO1 ERREX2                                                           
REPMSG   DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
         LTORG                                                                  
         EJECT                                                                  
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD *                             
         SPACE                                                                  
VPER     DS    0D                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    SVPERDTS,SVPERDTS                                                
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,SVPERST)                                    
         SPACE                                                                  
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
         SPACE                                                                  
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         GOTO1 HIGH                                                             
         SPACE                                                                  
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
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
         GOTO1 ERREX2                                                           
         SPACE                                                                  
VPER26   XC    CONHEAD,CONHEAD                                                  
         SPACE                                                                  
         CLI   QBEST,0             WAS ESTIMATE ENTERED                         
         BE    MISSEST                                                          
         SPACE                                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVGENEND),(4,CONHEAD+21)                            
         GOTO1 ERREX2                                                           
         SPACE                                                                  
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
         B     VPER60                                                           
         SPACE                                                                  
VPER32   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         SPACE                                                                  
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
         SPACE                                                                  
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
         SPACE                                                                  
VPER40   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER50                                                           
         SPACE                                                                  
         B     VPER60                                                           
         SPACE                                                                  
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
         SPACE                                                                  
VPER50   CLC   SVPERST,SVGENEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVPERST,SVGENST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
         SPACE                                                                  
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BNZ   VPER54                                                           
         MVC   SVPEREND,SVGENEND   USE EST END DATE                             
         B     VPER56                                                           
         SPACE                                                                  
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
         SPACE                                                                  
VPER54   CLC   SVPEREND,SVGENEND   LAST TLCST MUST BE EST END                   
         BH    ESTDTERR                                                         
         SPACE                                                                  
VPER56   GOTO1 DATCON,DMCB,(3,SVPERST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
         SPACE                                                                  
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
         SPACE                                                                  
VPER60   GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
         XIT1                                                                   
         SPACE                                                                  
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         GOTO1 ERREX2                                                           
MISSEST  LA    R2,TRAESTH                                                       
         MVI   ERROR,MISSING                                                    
         B     TRAPERR1                                                         
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERR1                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERR1                                                         
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR1                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
TRAPERR1 GOTO1 ERREX                                                            
         DROP  R6                                                               
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
         TITLE 'T21641 - GOAL REPORT HEADLINES'                                 
HEADINGS SSPEC H1,1,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'G O A L   A C T I V I T Y   L I S T'                     
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H2,35,C'-----------------------------------'                     
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,77,RUN                                                        
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,87,REQUESTOR                                                  
         SSPEC H4,39,C'PERIOD'                                                  
         SSPEC H8,1,C'MARKET-NAME                DP PRD-SLN'                    
         SSPEC H8,39,C'PRT-SLN EST -------------------ACTIVITY WEEKS'           
         SSPEC H8,84,C'--------------------------'                              
         SSPEC H9,1,C'-------------------------- -- -------'                    
         SSPEC H9,39,C'------- ---'                                             
         DC    X'00'                                                            
         EJECT                                                                  
GOARECD  DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         TITLE 'T21641 - GOAL LIST  - DSECTS'                                   
*   INCLUDE DDOFFICED                                                           
*   INCLUDE DDSPOOLD                                                            
*   INCLUDE DDSPLWORKD                                                          
*   INCLUDE SPTRAFFD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRAF1D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTRRR   DS    A                                                                
ASVNXPRD DS    A                                                                
         SPACE                                                                  
* TELECAST DATES                                                                
         SPACE                                                                  
PERSTP   DS    H                   FLIGHT/TELECAST START DATE                   
PERENDP  DS    H                   FLIGHT/TELECAST END DATE                     
         SPACE                                                                  
SVMKT    DS    H                                                                
SVBCLT   DS    CL2                                                              
SVOFFCD  DS    CL1                 FROM CLT HDR                                 
OFFCD    DS    CL1                                                              
OBCLT    DS    XL2                                                              
         SPACE                                                                  
* FROM BUY ELEMENT                                                              
         SPACE                                                                  
SVBSTA   DS    XL3                                                              
SVQSTA   DS    CL5                                                              
STAAFFL  DS    CL1                                                              
STATYPE  DS    CL1                                                              
         SPACE                                                                  
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
         SPACE                                                                  
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
         SPACE                                                                  
* FROM FLIGHT OR ESTIMATE RECORD                                                
         SPACE                                                                  
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 INSTRUCTION START DATE                       
SVGENEND DS    XL3                 INSTRUCTION END DATE                         
TODAYP   DS    XL2                 TODAY PACKED - USED IN UPD RTN               
         SPACE                                                                  
* KEEP ALL SVPROD TO SVBSLN2 TOGETHER AND IN ORDER *                            
         SPACE                                                                  
SVPROD   DS    CL3                                                              
SVBPRD   DS    XL1                                                              
SVBSLN   DS    XL1                                                              
SVPROD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
SVBSLN2  DS    XL1                                                              
SVPRDFLG DS    XL1                                                              
         SPACE                                                                  
TDAY     DS    CL6                TODAY'S DATE                                  
XDAY     DS    CL6                TODAY'S DATE + 60 DAYS                        
SVDAY    DS    CL6                TEMP SAVE TODAY'S DATE                        
TMPTIME  DS    XL4                                                              
SVSQ     DS    XL2                                                              
SVSQ2    DS    XL2                                                              
TEMPDATE DS    CL3                                                              
SVDATE   DS    CL2                                                              
CMMLNO   DS    XL1                                                              
OPTFLAG  DS    XL1                IF OPTION WAS CHOOSEN                         
LISTBUY  DS    XL1                                                              
TBL2CNTR DS    XL1                                                              
SVEST    DS    XL3                                                              
COMPDATE DS    XL2                                                              
TEMPEND  DS    CL3                TBUY END DATE                                 
HEADATE  DS    CL2                                                              
TBLCNTR  DS    F                                                                
DTTABLE  DS    CL32               16 X 2 -GOAL ELEMENT DATES                    
DTTABLE2 DS    CL32               16 X 2 -PERIOD DATES                          
GOALFRST DS    XL2                                                              
GOALLAST DS    XL2                                                              
TSTART   DS    XL2                                                              
TEND     DS    XL2                                                              
ENDSYSD  EQU   *                IF THIS ADDRESS >1F70, PAST END OF SYSD         
         SPACE                                                                  
         EJECT                                                                  
* OFFLINE PRINT                                                                 
         SPACE                                                                  
PRTLINE  DSECT                                                                  
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PMKTNM   DS    CL20                                                             
         DS    CL1                                                              
PDAYPT   DS    CL1                                                              
         DS    CL2                                                              
PPRDSLN  DS    CL7                                                              
         DS    CL1                                                              
PPTRSLN  DS    CL7                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PACTV    DS    15CL4                                                            
         SPACE                                                                  
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMKT     DS    CL4                                                              
         DS    CL2                                                              
LMKTNM   DS    CL20                                                             
         DS    CL1                                                              
LDAYPT   DS    CL1                                                              
         DS    CL2                                                              
LPRDSLN  DS    CL7                                                              
         DS    CL1                                                              
LPTRSLN  DS    CL7                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LDATES   DS    CL17                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPTRA41   03/04/19'                                      
         END                                                                    
