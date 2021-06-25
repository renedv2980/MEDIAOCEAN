*          DATA SET SPTRA25    AT LEVEL 032 AS OF 12/06/16                      
*PHASE T21625C                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE SORTER                                                                 
*                                                                               
*                                                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30) SHIPPING RECORD TO BE MAINTAINED         
*                    OFFLINE ONLY READ OUTPUT SORT RECS                         
*             AIO2 - OFFLINE ONLY BUILD INPUT SORT RECS                         
*                                 1ST 1K = STATION FND TABLE                    
*                                 2ND 1K = MKT NAME TABLE                       
*             AIO3 - USED TO READ CLT, MKT, AND STATION RECS                    
*                    ALSO BUY ACTIVITY RECORDS                                  
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR                
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE                                                       
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
***********************************************************************         
         TITLE 'T21625 TRAFFIC BUY ACTIVITY REPORT'                             
***********************************************************************         
*  THIS PROGRAM READS SPOT BUYS AND INSTRUCTION RECAP RECORDS         *         
*  IT THEN GENERATES A BUY ACTIVITY REPORT FOR THE BUYS THAT ARE NOT  *         
*  COVERED BY INSTRUCTION RECAP RECORDS.                              *         
***********************************************************************         
*                                                                     *         
*                       CHANGE LOG                                    *         
*                                                                     *         
*  LEV  1 SMUR SEP27/00 LIMITED ACCESS                                *         
*  LEV  2 SMUR APR15/02 TRAFFIC OFFICE                                *         
*  LEV  4 SMUR JUN12/02 BYPASS BUY DATES BEFORE PERIOD START          *         
*  LEV  5 SMUR JUN26/02 CLIENT STRING SECURITY REQUEST BY CLT ONLY    *         
*  LEV  6 SMUR JUL17/02 FIX COUNTER WHEN TBA PATTERN ARE PRESENT      *         
*  LEV  7 SMUR AUG07/02 FIX FCLT                                      *         
*  LEV  8 SMUR SEP11/02 BUMP UP 1 LAST BYTE OF CLT AND CLEAR PRD FIELD*         
*  LEV  9 SMUR OCT23/02 BYPASS UNALLOC ELEMS                          *         
*  LEV 10 BGRI NOV07/02 NEW INST RECAP RECS                           *         
*  LEV 11 SMUR DEC17/03 MOVE TO LARGE SOON REQUEST                    *         
*  LEV 12 SMUR JAN28/04 2 CHAR MARKET GROUP                           *         
*  LEV 13 BGRI MAR31/04 FIX NEW CABLE STATION MAX                     *         
*  LEV 14 SMUR APR23/04 FIX P/B PRODUCT                               *         
*  LEV 13 BGRI MAY10/04 FIX NEW CABLE STATION 'TINY BUG'              *         
*  LEV 16 SMUR MAR03/05 NOP OF PRESET END DATE (KEEP ACTUAL BUY DATE) *         
*  LEV 17 SMUR SEP01/05 2 CHAR OFFICE CODE                            *         
*  LEV 18 SMUR FEB27/06 FIX TBA/HIATUS PATTERNS                       *         
*  LEV 21 SMUR DEC10/07 BYPASS TRAFFIC=NO BUYS                        *         
*  LEV 23 SMUR FEB27/08 FIX BUG                                       *         
*  LEV 24 SMUR OCT17/08 MOVE FROM LONG SOON TO MEDIUM                 *         
*  LEV 25 SMUR JUN18/08 RUN SOME REQUESTS IN LONG SOONS               *         
*  LEV 26      FEB21/12 SUPPRESSS VIA PROFILE NOADDR ON REPORT        *         
*  LEV 27 MNAS JAN22/13 MORE BANDS                                    *         
*  LEV 29 MNAS FEB21/13 SEND RUN LAST MODE TO CLOSE FILES ON EXIT     *         
*  LEV 30 SMUR FEB23/15 FIX BAD BRANCH INTRODUCED AT LEVEL 10         *         
*  LEV 31 SMUR JAN05/16 NEW BAND CM FOR IHEART RADIO                  *         
*  LEV 32 SMUR NOV22/16 2 CHAR OFFICE LIST ($A1)                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21625 TRAFFIC BUY ACTIVITY REPORT'                             
T21625   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21625**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR25RR                                                      
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    INVAL                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECPUT         PUT RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
                                                                                
EXIT     XIT1                                                                   
                                                                                
* VALIDATE RECORD ROUTINE *  (NOT SUPPORTED)                                    
                                                                                
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
                                                                                
VK       CLI   ACTNUM,ACTREP       TEST ACTION REPORT                           
         BE    VK00                                                             
         CLI   ACTNUM,ACTSEL       TEST ACTION SELECT                           
         BNE   INVAL                                                            
         CLI   ACTNUM,ACTDEL       DEL RECORD                                   
         BE    INVAL                                                            
                                                                                
VK00     TM    TRAMEDH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRACLTH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAPRDH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRASTAH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAESTH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAPERH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAFLTRH+4,X'20'    VALIDATED                                    
         BO    EXIT                 YES                                         
                                                                                
VK10     MVI   MEDSOON,0           INIT RUN REPORT IN MED SOON                  
*                                                                               
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE/GLOBAL FILES              
*MN                                                                             
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
                                                                                
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         XC    SVCLT,SVCLT                                                      
         MVI   BOFFCD,0                                                         
                                                                                
         CLI   5(R2),0             TEST DATA                                    
         BNE   VK20                                                             
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
         CLI   ACTNUM,ACTREP       OMIT ONLY FOR REPORT                         
         BNE   MISSERR                                                          
                                                                                
         MVI   BYTE,0                                                           
         B     VK40                                                             
                                                                                
VK20     CLI   8(R2),C'*'          TEST OFFICE CODE                             
         BE    VK25                                                             
         CLI   8(R2),C'$'          TEST OFFICE CODE                             
         BE    VK25                                                             
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT          SAVE BCLT TO STOP TA PROFILE LOOKUP          
         MVC   BYTE,SVCLTOFF                                                    
                                                                                
         B     VK40                                                             
                                                                                
VK25     CLI   5(R2),3                                                          
         BH    OFFLNERR                                                         
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
         CLI   8(R2),C'*'          BY OFFICE                                    
         BNE   VK33                                                             
                                                                                
* VALIDATE AND CONVERT TO 1 BYTE IF NEEDED                                      
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES      1 CHAR OFFICE PADDED W/SPACE                 
                                                                                
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK30                JUST CONVERT, DO NOT VALIDATE                
                                                                                
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK28                                                             
                                                                                
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK28                VALIDATE AND CONVERT                         
                                                                                
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK28                                                             
                                                                                
*                                  SINGLE CLIENT ACCESS                         
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
                                                                                
         BAS   RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VK30                                                             
                                                                                
VK28     MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
VK30     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR                                                         
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VK33                2 CHAR OFFICE IS NOT ON                      
                                                                                
         MVC   BOFFCD,OFCOFC       SAVE 1 BYTE OFFICE CODE                      
         B     *+10                                                             
VK33     MVC   BOFFCD(1),9(R2)                                                  
                                                                                
         GOTO1 =A(VOFF),RR=SPTR25RR                                             
                                                                                
         MVC   BYTE,BOFFCD                                                      
                                                                                
* READ T0 PROFILE *                                                             
VK40     XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),BYTE                                                  
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ T2 PROFILE *                                                             
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT2PR05,SVT1PROF+4                                              
                                                                                
* READ T1 PROFILE *                                                             
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT1PR01,SVT1PROF+0                                              
         MVC   SVT1PR02,SVT1PROF+1                                              
                                                                                
* READ T3 PROFILE *                                                             
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT3PR06,SVT1PROF+5                                              
* READ TA PROFILE *                                                             
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
                                                                                
                                                                                
VK50     LA    R2,TRAPRDH                                                       
         MVI   BPRD,0              CLEAR PRD                                    
         CLI   5(R2),0             TEST DATA                                    
         BNE   VK60                                                             
         CLI   ACTNUM,ACTREP       OMIT ONLY FOR REPORT                         
         BNE   MISSERR                                                          
         B     VK70                                                             
                                                                                
VK60     CLC   =C'ALL',8(R2)                                                    
         BE    VK70                                                             
         CLC   =C'POL',8(R2)                                                    
         BE    VK70                                                             
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
                                                                                
VK70     XC    BMKTSTA,BMKTSTA                                                  
         LA    R2,TRASTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK80                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   MISSERR                                                          
         XC    TRAMKNM,TRAMKNM                                                  
         OI    TRAMKNMH+6,X'80'                                                 
         B     VK100                                                            
                                                                                
VK80     MVI   MEDSOON,C'Y'        STA SPECIFIC REQ, RUN IN MED SOON            
         GOTO1 VALISTA                                                          
         MVC   TRAMKNM,MKTNM                                                    
         OI    TRAMKNMH+6,X'80'                                                 
                                                                                
VK100    LA    R2,TRAPERH                                                       
         GOTO1 =A(VPER),RR=SPTR25RR CALL TO VPER                                
                                                                                
         LA    R2,TRAFLTRH         VALIDATE FILTERS                             
         GOTO1 =A(VFTR),RR=SPTR25RR                                             
                                                                                
         LA    R2,TRAESTH          VALIDATE ESTIMATE                            
         BAS   RE,VEST                                                          
                                                                                
         OI    TRAMEDH+4,X'20'     VALIDATED                                    
         OI    TRACLTH+4,X'20'     VALIDATED                                    
         OI    TRAPRDH+4,X'20'     VALIDATED                                    
         OI    TRASTAH+4,X'20'     VALIDATED                                    
         OI    TRAESTH+4,X'20'     VALIDATED                                    
         OI    TRAPERH+4,X'20'     VALIDATED                                    
         OI    TRAFLTRH+4,X'20'    VALIDATED                                    
*                                                                               
* BUILD KEY                                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),BAGYMD       A-M/CLT                                      
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BMKTSTA                                                 
         MVC   KEY+9(1),BEST                                                    
                                                                                
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
*                                                                               
         MVI   REQSML,C'M'         PRESET TO M(EDUIM) REQUEST                   
*                                                                               
         CLI   MEDSOON,C'Y'        RUN IN MEDIUM SOON IS SET                    
         BE    VKX                 YES, DONE                                    
*                                                                               
* SEE IF THIS NEEDS TO RUN IN LONG SOON                                         
*                                                                               
         LA    R0,AGYTABCT                                                      
         LA    RE,AGYTAB           LONG SOONS AGENCY TABLE                      
         USING LSAGYTAB,RE                                                      
*                                                                               
VK103    CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    VKX                                                              
         CLC   AGENCY,LSAGY        THIS AGENCY                                  
         BNE   VK103C                                                           
         CLC   QMED,LSQMED         THIS MEDIA                                   
         BNE   VK103C                                                           
         CLC   QCLT,LSQCLT         THIS CLIENT                                  
         BNE   VK103C                                                           
         CLC   QPRD,LSQPRD         THIS PROD                                    
         BE    VK104                                                            
VK103C   LA    RE,LSNEXT                                                        
         BCT   R0,VK103                                                         
         B     VKX                                                              
*                                                                               
         DROP  RE                                                               
*                                                                               
VK104    MVI   REQSML,C'L'         MAKE IT A L(ARGE) REQUEST                    
*                                                                               
VKX      B     EXIT                                                             
                                                                                
*        DROP  R6                                                               
         EJECT                                                                  
* OFFLINE REPORT ROUTINE *                                                      
*                                                                               
LR       XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
                                                                                
         TM    WHEN,X'C0'          MUST BE RUN OFFLINE                          
         BNZ   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
                                                                                
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE/GLOBAL FILES              
*MN                                                                             
         L     R2,VADUMMY                                                       
         AHI   R2,9000             ROOM FOR 1000 STATIONS                       
                                                                                
         MVC   0(8,R2),=C'*BUYACT*'                                             
         LA    R2,8(R2)                                                         
         ST    R2,ABUYACT          SAVE ADDRESS OF BUY ACTIVITY TABLE           
         XC    0(L'NBUYENT,R2),0(R2)  CLEAR FIRST ENTRY                         
                                                                                
         LM    R4,R5,=A(SORTCARD,RECCARD)                                       
         GOTO1 =V(SORTER),DMCB,(R4),(R5)                                        
*                                                                               
* BUILD KEY *                                                                   
*                                                                               
         MVI   TBLSW,0             MUST BUILD DATE TABLE                        
         MVC   KEY(3),BAGYMD       A-M/CLT                                      
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BMKTSTA                                                 
         MVC   KEY+9(1),BEST                                                    
         MVC   KEY+4(2),MKTFTR     IF NOT ENTERED, BINARY ZEROS                 
                                                                                
         CLI   BOFFCD,0            THIS BY OFFICE                               
         BE    *+10                                                             
         MVC   KEY+1(2),OCLT                                                    
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(1),KEYSAVE      TEST SAME AGENCY/MEDIA                       
         BNE   LR20                                                             
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR40                                                             
         CLC   BCLT,KEY+1          TEST SAME CLIENT                             
         BE    LR40                                                             
                                                                                
LR20     CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BNE   NOACTERR            ONLINE ERROR ROUTINE                         
         CLI   BOFFCD,0            TEST OFFICE CODE WAS ENTERED                 
         BNE   LRRPRT                                                           
         B     NOACTIV                                                          
         EJECT                                                                  
LR30     GOTO1 SEQ                 DO READ SEQUENTIAL                           
                                                                                
LR35     CLC   KEY(1),KEYSAVE      SAME TYPE/A-M                                
         BE    LR40                YES                                          
                                                                                
         CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BE    LRRPRT              GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                                                             
                                                                                
LR40     LA    R4,KEY                                                           
         USING BUYRECD,R4                                                       
                                                                                
         CLI   SVT3PR06,C'Y'                                                    
         BNE   LR45                                                             
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    LR30                                                             
         CLI   DUB+4,C'S'                                                       
         BE    LR30                                                             
         CLI   DUB+4,C'C'          CM FOR IHEART RADIO                          
         BE    LR30                                                             
                                                                                
LR45     DS    0H                                                               
         TM    KEY+10,X'80'                                                     
         BO    LR30                                                             
                                                                                
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR50                NO                                           
                                                                                
         CLC   BUYKCLT,BCLT        SAME CLIENT                                  
         BE    LR70                YES                                          
                                                                                
         CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BE    LRRPRT              GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                                                             
                                                                                
LR50     CLC   SVCLT,BUYKCLT       NEW CLIENT                                   
         BE    LR70                                                             
                                                                                
         MVC   SVCLT,BUYKCLT                                                    
         GOTO1 =A(FCLT),RR=SPTR25RR                                             
*NOP     BE    LR30                CLIENT WAS EXCLUDED                          
                                                                                
LR70     CLI   BOFFCD,0            THIS AN OFFICE REQUEST                       
         BE    LR80                                                             
                                                                                
         CLC   OCLT,BUYKCLT        THIS THIS CLT                                
         BE    LR80                                                             
                                                                                
         GOTO1 =A(NOFF),RR=SPTR25RR  GET NEW CLIENT CODE                        
         BNE   LRRPRT              ALL OF OFFICE CODE DONE?                     
                                                                                
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMD       A-M/CLT                                      
         MVC   BUYKCLT,OCLT                                                     
         GOTO1 HIGH                                                             
         B     LR35                                                             
                                                                                
LR80     OC    BMKTSTA,BMKTSTA     WAS STATION ENTERED                          
         BZ    LR90                                                             
         CLC   BMKTSTA,BUYMSTA                                                  
         BNE   LR30                                                             
                                                                                
LR90     OC    MKTFTR,MKTFTR       WAS MARKET FILTER ENTERED                    
         BZ    LR100                                                            
         CLC   MKTFTR,BUYMSTA                                                   
         BNE   LR30                                                             
                                                                                
LR100    CLI   BEST,0              WAS ESTIMATE ENTERED                         
         BE    LR110               NO                                           
         CLC   BEST,BUYKEST        TEST MATCH                                   
         BNE   LR30                                                             
                                                                                
LR110    OC    MGRPTBLE(2),MGRPTBLE  WAS MARKET GROUP ENTERED                   
         BZ    LR130                  NO                                        
                                                                                
         LA    R0,150                                                           
         LA    R1,MGRPTBLE                                                      
LR120    CLC   BUYMSTA(2),0(R1)    THIS A MARKET                                
         BE    LR130                                                            
         LA    R1,2(,R1)                                                        
         OC    0(2,R1),0(R1)       END OF TABLE                                 
         BZ    LR30                                                             
         BCT   R0,LR120                                                         
         B     LR30                                                             
                                                                                
LR130    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
                                                                                
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08'                                              
                                                                                
         TM    15(R6),X'80'        DELETED BUY                                  
         BO    LR30                YES - IGNORE                                 
                                                                                
         LA    R6,24(,R6)          POINT TO FIRST ELEMENT                       
         USING BDELEM,R6                                                        
                                                                                
         CLI   SLNFTR,0            WAS SPOT LEN FILTER ENTERED                  
         BE    LR140                                                            
         CLC   SLNFTR,BDSEC        SAME                                         
         BNE   LR30                NO                                           
                                                                                
* LOOK FOR TRAFFIC=NO                                                           
                                                                                
LR140    L     R6,AIO1                                                          
         LA    R6,24(,R6)                                                       
         MVI   ELCODE,X'66'                                                     
LR150    BAS   RE,NEXTEL                                                        
         BNE   LR160                                                            
                                                                                
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    LR30                BYPASS THIS BUY                              
         B     LR150                                                            
                                                                                
LR160    L     R6,AIO1                                                          
         LA    R6,24(,R6)                                                       
         MVC   SVSLN,BDSEC                                                      
         MVC   SVBDDAY,BDDAY       MOVE IN DAY 1=MON...7=SUN                    
                                                                                
         MVI   SVPRD,0                                                          
         MVI   SVPRD2,0                                                         
                                                                                
* LOOK FOR BILLBOARD                                                            
                                                                                
         NI    SVFLAG,X'FF'-(BB5+BB10) INIT BILLBOARD FLAGS                     
                                                                                
         L     R6,AIO1                                                          
         LA    R6,24(,R6)                                                       
         MVI   ELCODE,X'66'                                                     
LR170    BAS   RE,NEXTEL                                                        
         BNE   LR180                                                            
                                                                                
         CLC   =C'BB=5',3(R6)                                                   
         BNE   *+12                                                             
         OI    SVFLAG,BB5          BB=5                                         
         B     LR170                                                            
                                                                                
         CLC   =C'BB=10',3(R6)                                                  
         BNE   LR170                                                            
         OI    SVFLAG,BB10         BB=10                                        
         B     LR170                                                            
                                                                                
LR180    L     R6,AIO1                                                          
                                                                                
         MVI   SVPRD2,0            RESET PTR                                    
                                                                                
         CLI   3(R6),X'FF'         TEST POOL BUYREC                             
         BE    LR200                YES, FIX PRD 1ST                            
                                                                                
         MVC   SVPRD,BUYKPRD       SAVE PRODUCT                                 
                                                                                
         CLI   BDTIME,0            TEST P/B                                     
         BE    LR190                NO                                          
                                                                                
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4            PIGGYBACK ELEMENT                            
         LA    R6,24(,R6)          POINT TO FIRST ELEMENT                       
         BAS   RE,BUYEL                                                         
         BNE   LR190                                                            
         MVC   SVPRD2,PBPROD                                                    
         MVC   SVSLN2,PBTIME       P/B LENGTH                                   
                                                                                
LR190    DS    0H                                                               
         CLI   BPRD,0              TEST ONE PRD REQUEST                         
         BE    LR200                                                            
         CLC   BPRD,SVPRD          YES - TEST MATCH PROD                        
         BE    LR200                                                            
         CLC   BPRD,SVPRD2                                                      
         BNE   LR30                                                             
                                                                                
LR200    BAS   RE,GROT             GET ROTATION                                 
                                                                                
         NI    SVFLAG,X'FF'-X'80'  INIT POOL BUY FLAG                           
                                                                                
         L     R6,AIO1                                                          
         CLI   3(R6),X'FF'         TEST POOL BUYREC                             
         BNE   LR300                                                            
                                                                                
         OI    SVFLAG,X'80'        ITS A POOL BUY                               
                                                                                
         LA    R1,24(,R6)          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
LR210    ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    LR240                                                            
         CLI   0(R1),X'61'         TEST FOR MASTER CLIENT ELEMENT               
         BNE   LR210                                                            
                                                                                
         CLI   1(R1),6             THIS ELEM INCLUDE PROD CODE                  
         BL    LR240                                                            
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)          SAVE PROD CODE                               
         BZ    LR240                                                            
         STC   R0,SVPRD                                                         
                                                                                
         LA    R1,24(,R6)          POINT TO FIRST ELEMENT                       
                                                                                
LR215    ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    LR240                                                            
         CLI   0(R1),X'0B'         TEST FOR POL ORIG                            
         BL    LR215                                                            
         CLI   0(R1),X'0D'         TEST FOR POL FLIP                            
         BH    LR215                                                            
         CLI   1(R1),10            UNALLOCATED PRODUCT                          
         BNH   LR215                                                            
         STC   R0,10(,R1)          STORE NEW PRODUCT CODE                       
         B     LR215                                                            
                                                                                
* POL PROCESSING                                                                
                                                                                
LR240    XC    SVELDATE,SVELDATE                                                
                                                                                
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         L     R6,AIO1                                                          
         LA    R6,24(,R6)          POINT TO FIRST ELEMENT                       
                                                                                
         NI    SVFLAG,X'FF'-BUYFSW                                              
                                                                                
LR245    BAS   RE,BUYEL                                                         
         BNE   LR420                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   LR410                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   LR410                                                            
                                                                                
         BAS   RE,MPRD                                                          
         BNE   LR245               PRODS DO NOT MATCH                           
                                                                                
*TEMP    OI    SVFLAG,BUYFSW       GOOD BUY FOUND SW                            
         MVC   SVELDATE,2(R6)      SAVE BUY DATE                                
         B     LR260                                                            
                                                                                
LR250    DS    0H                                                               
         BAS   RE,BUYEL                                                         
         BNE   LR420                                                            
                                                                                
LR252    CLI   1(R6),10            TEST UNALL                                   
         BNH   LR410                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   LR410                                                            
                                                                                
         CLC   SVPRD,10(R6)                                                     
         BNE   LR255                                                            
         CLC   SVPRD2,14(R6)                                                    
         BNE   LR255                                                            
         CLC   SVELDATE,2(R6)                                                   
         BE    LR410                                                            
                                                                                
LR255    MVC   SVELDATE,2(R6)                                                   
         BAS   RE,MPRD             SEE IF PRODS MATCH                           
         BNE   LR250                                                            
                                                                                
*TEMP    OI    SVFLAG,BUYFSW       GOOD BUY FOUND SW                            
                                                                                
LR260    MVC   ELDATE,2(R6)        START DATE (COMPRESSED BUY DTE)              
         MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
         B     LR370                                                            
*                                                                               
* NON-POOL BUY                                                                  
*                                                                               
LR300    XC    SVELDATE,SVELDATE                                                
         XC    ELDATE,ELDATE                                                    
         XC    ELDATEX,ELDATEX                                                  
         XC    ELCOUNT,ELCOUNT                                                  
                                                                                
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         LA    R6,24(,R6)          POINT TO FIRST ELEMENT                       
         BAS   RE,BUYEL                                                         
         BNE   LR30                                                             
         MVC   SVELDATE,2(R6)                                                   
         B     LR330                                                            
                                                                                
LR310    CLI   0(R6),0             TEST EOR                                     
         BE    LR30                 YES, GET NEXT RECORD                        
                                                                                
LR320    LR    R5,R6               SAVE PT TO PREV ELEM                         
                                                                                
         BAS   RE,BUYEL                                                         
         BNE   LR350                                                            
                                                                                
LR330    CLC   SVELDATE,2(R6)      TEST SAME DATE                               
         BNE   LR340                                                            
                                                                                
         ZIC   R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AH    R0,ELCOUNT                                                       
         STH   R0,ELCOUNT                                                       
         B     LR320                                                            
                                                                                
LR340    MVC   ELDATE,SVELDATE                                                  
         MVC   SVELDATE,2(R6)      MOVE NEW DATE                                
         OC    ELCOUNT,ELCOUNT     TEST ANY SPOTS THIS DATE                     
         BZ    LR330                NO                                          
         B     LR360                                                            
                                                                                
LR350    OC    ELCOUNT,ELCOUNT     TEST ANY SPOTS THIS DATE                     
         BZ    LR30                NO                                           
         MVC   ELDATE,SVELDATE                                                  
                                                                                
LR360    MVC   ELDATEX,ELDATE      PRESET END DATE                              
                                                                                
LR370    CLI   SVT1PR01,0          TEST ADJUST ROTATOR DAYS                     
         BE    LR373               NO                                           
         CLC   SVT1PR01(1),SVBDDAY TEST DAYS MATCH                              
         BNE   LR373                                                            
                                                                                
* BACK UP ONE DAY *                                                             
                                                                                
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR02         GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
         MVC   ELDATEX,ELDATE      AND PRESET END DATE                          
         B     LR375                                                            
                                                                                
LR373    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,ELDATE),(0,WORK)                                  
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,1                IF MONDAY                                    
         BE    LR375                THEN DONE                                   
                                                                                
* CHANGE DATE TO PREV MONDAY                                                    
                                                                                
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,ELDATE)                                  
*NOP     MVC   ELDATEX,ELDATE      AND PRESET END DATE                          
                                                                                
LR375    CLC   ELDATE,ENDATE       TEST AFTER PERIOD END DTE                    
         BNH   LR380                                                            
         TM    SVFLAG,X'80'        POOL BUY                                     
         BO    LR410               GET NEXT ELEM                                
         XC    ELCOUNT,ELCOUNT                                                  
         CLI   0(R6),0             CHK EOR                                      
         BE    LR30                                                             
         B     LR330                                                            
                                                                                
LR380    TM    SVFLAG,X'80'        POOL BUY?                                    
         BZ    *+12                                                             
         CLI   0(R6),11             YES, TEST REGEL                             
         BNE   LR385                                                            
                                                                                
         LH    R0,ROTDAYS                                                       
         LTR   R0,R0                                                            
         BZ    LR385                                                            
                                                                                
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
                                                                                
LR385    CLC   ELDATEX,STDATE      TEST BEFORE PERIOD START DTE                 
         BNL   LR390                                                            
         TM    SVFLAG,X'80'        POOL BUY                                     
         BO    LR410               GET NEXT ELEM                                
         XC    ELCOUNT,ELCOUNT                                                  
         CLI   0(R6),0             CHK EOR                                      
         BE    LR30                                                             
         B     LR330                                                            
                                                                                
LR390    OI    SVFLAG,BUYFSW       GOOD BUY FOUND SW                            
         CLI   MODE,LISTRECS       TEST ONLINE LIST                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* PROCESS ELEMENTS FOR OFFLINE REPORT                                           
                                                                                
LR400    CLI   TBLSW,1             HAS WEEK TABLE BEEN BUILT                    
         BE    LR420               YES                                          
         BAS   RE,TBL              GO BUILD WEEK TABLE AND HEADINGS             
         MVI   TBLSW,1                                                          
         B     LR420                                                            
                                                                                
LR410    GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         CLI   0(R6),0             TEST EOR                                     
         BE    LR415                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    LR410                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    LR410                                                            
         B     LR252                                                            
                                                                                
LR415    TM    SVFLAG,BUYFSW       GOOD BUY FOUND ?                             
         BZ    LR30                 NO, GET NEXT REC                            
                                                                                
LR420    BAS   RE,CKDELEST         CHECK FOR DELETED ESTIMATE                   
         BNE   LR30                 YES BYPASS                                  
                                                                                
         TM    SVFLAG,X'80'        POOL BUY                                     
         BZ    LR450                                                            
         TM    SVFLAG,BUYFSW       WERE ANY GOOD BUYS FOUND                     
         BZ    LR30                 NO                                          
         NI    SVFLAG,X'FF'-BUYFSW                                              
                                                                                
LR450    DS    0H                                                               
         TM    SVFLAG,X'80'        POOL BUY                                     
         BO    LR460                                                            
         CLI   0(R6),0                                                          
         BE    *+6                                                              
         LR    R6,R5               PT TO ELEM THAT IS BEING PROCESSED           
                                                                                
LR460    CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         EJECT                                                                  
* FORMAT SORT RECS FOR OFFLINE REPORT HERE *                                    
*                                                                               
LRRSRT   L     R5,AIO2                                                          
         USING SORTREC,R5                                                       
         L     R4,AIO1                                                          
         USING BUYKEY,R4                                                        
                                                                                
         L     RE,AIO2                                                          
         LA    RF,2000             ZERO SORT RECORD AREA                        
         XCEF                                                                   
                                                                                
         NI    SVFLAG,X'FF'-PRDSW  INIT PROD CHANGED                            
                                                                                
         MVC   SRTEST,BUYKEST                                                   
         CLI   ESTSTAT,C'Y'        TEST ESTIMATE STATUS                         
         BE    *+8                  IT IS NOT 'NO'                              
         MVI   SRTEST,0            SORT ACROSS ALL ESTIMATES                    
                                                                                
         MVC   SRTCLT,BUYKCLT                                                   
                                                                                
         CLC   SVCLT,BUYKCLT                                                    
         BE    LRRS10                                                           
         MVC   SVCLT,BUYKCLT                                                    
         GOTO1 =A(FCLTR),RR=SPTR25RR                                            
*NOP     BE    LR30                CLIENT WAS EXCLUDED                          
                                                                                
LRRS10   L     R1,ASVCLIST                                                      
                                                                                
LRRS15   CLC   3(1,R1),SVPRD       MATCH PRD CODE                               
         BE    LRRS20                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    LRRS15                                                           
         LA    R1,=C'???'          SET UNKNOWN PRODUCT                          
                                                                                
LRRS20   MVC   SRTPROD,0(R1)                                                    
         MVC   SRTPRD,SVPRD                                                     
         MVC   SRTMKST,BUYMSTA                                                  
                                                                                
         TM    SVFLAG,BB5                                                       
         BZ    *+8                                                              
         OI    SRTFLAG,SRTBB5              BB=5                                 
                                                                                
         TM    SVFLAG,BB10                                                      
         BZ    *+8                                                              
         OI    SRTFLAG,SRTBB10             BB=10                                
                                                                                
         XC    SRTPROD2,SRTPROD2                                                
         XC    SRTSLN2,SRTSLN2                                                  
         CLI   SVPRD2,0            ANY PTR                                      
         BE    LRRS50              NO                                           
                                                                                
         L     R1,ASVCLIST                                                      
LRRS30   CLC   3(1,R1),SVPRD2      MATCH PRD CODE                               
         BE    LRRS40                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    LRRS30                                                           
         LA    R1,=C'???'          SET UNKNOWN PRODUCT                          
                                                                                
LRRS40   MVC   SRTPROD2,0(R1)                                                   
         MVC   SRTPRD2,SVPRD2                                                   
                                                                                
LRRS50   MVC   SRTSLN,SVSLN                                                     
         CLI   SVPRD2,0            ANY PTR                                      
         BE    LRRS70                                                           
         MVC   SRTSLN2,SVSLN2                                                   
                                                                                
LRRS70   CLC   ELDATEX,STDATE      BEFORE PERIOD START                          
         BL    LRRS110                                                          
         CLC   ELDATE,ENDATE                                                    
         BH    LRRS110                                                          
         CLC   ELDATE,STDATE                                                    
         BL    LRRS110                                                          
*                                                                               
LRRS100  MVC   SRTDT,ELDATE        START DATE                                   
         MVC   SRTDTX,ELDATEX      PLUS ROTATION                                
                                                                                
         LA    R5,L'SRTENT(,R5)                                                 
                                                                                
LRRS110  BAS   RE,BUYEL                                                         
         BNE   LRRS140                                                          
                                                                                
         CLI   1(R6),10            TEST UNALLOCATED PRODUCT                     
         BNH   LRRS110                                                          
                                                                                
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   LRRS110                                                          
                                                                                
         TM    SVFLAG,X'80'        POOL BUY                                     
         BZ    LRRS120              NO                                          
                                                                                
         CLC   SVPRD,10(R6)        SAME PRODUCT?                                
         BNE   *+14                                                             
         CLC   SVPRD2,14(R6)                                                    
         BE    LRRS120                                                          
                                                                                
         BAS   RE,MPRD                                                          
         BNE   LRRS110                                                          
                                                                                
         OI    SVFLAG,PRDSW        PROD CHANGED                                 
                                                                                
LRRS120  MVC   SVELDATE,2(R6)                                                   
         MVC   ELDATE,2(R6)        PRESET BUY DATES                             
         MVC   ELDATEX,2(R6)                                                    
                                                                                
         CLI   SVT1PR01,0          TEST ADJUST ROTATOR DAYS                     
         BE    LRRS122             NO                                           
         CLC   SVT1PR01(1),SVBDDAY TEST DAYS MATCH                              
         BNE   LRRS122                                                          
                                                                                
* BACK UP ONE DAY *                                                             
                                                                                
         GOTO1 DATCON,DMCB,(2,SVELDATE),WORK                                    
         ZIC   R0,SVT1PR02         GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,SVELDATE)                                  
         MVC   ELDATE,SVELDATE                                                  
         MVC   ELDATEX,ELDATE      AND PRESET END DATE                          
         B     LRRS124                                                          
                                                                                
LRRS122  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SVELDATE),(0,WORK)                                
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,1                IF MONDAY                                    
         BE    LRRS124              THEN DONE                                   
                                                                                
* CHANGE DATE TO PREV MONDAY                                                    
                                                                                
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,SVELDATE)                                
         MVC   ELDATE,SVELDATE                                                  
         MVC   ELDATEX,ELDATE      AND PRESET END DATE                          
                                                                                
LRRS124  TM    SVFLAG,X'80'        POOL BUY?                                    
         BZ    *+12                                                             
         CLI   0(R6),11            TEST REGEL                                   
         BNE   LRRS125                                                          
                                                                                
         LH    R0,ROTDAYS                                                       
         LTR   R0,R0                                                            
         BZ    LRRS125                                                          
                                                                                
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
                                                                                
LRRS125  LR    R1,R5                                                            
         S     R1,=A(L'SRTENT)     PT TO PREV SET OF DATES                      
                                                                                
PREV     USING SORTREC,R1                                                       
                                                                                
         CLC   PREV.SRTDT,ELDATE DATE IN TABLE?                                 
         BNE   LRRS130                                                          
                                                                                
         DROP  PREV                                                             
                                                                                
         TM    SVFLAG,PRDSW        HAS PRODUCT CHANGED                          
         BZ    LRRS110              NO, GET NEXT ELEM                           
                                                                                
         B     LRRS140             SAME DATE DIFFERENT PROD                     
                                                                                
LRRS130  TM    SVFLAG,PRDSW                                                     
         BZ    LRRS70                                                           
         MVC   SVPRD,10(R6)        PROD FROM POOL BUYS                          
                                                                                
LRRS140  L     R3,AIO2                                                          
         SR    R5,R3                                                            
         LTR   R5,R5                                                            
         BZ    LRRS150                                                          
         LA    R5,23(,R5)                                                       
         STH   R5,0(,R3)                                                        
                                                                                
****     MVC   P(3),=C'PUT'                                                     
*        GOTO1 HEXOUT,DMCB,AIO2,P+5,60                                          
*****    GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R3)                                     
                                                                                
LRRS150  TM    SVFLAG,X'80'        IS THIS POOL BUY                             
         BZ    LRRS160                                                          
                                                                                
         TM    SVFLAG,PRDSW        HAS PROD CHANGED?                            
         BZ    LR30                 NO                                          
         L     R5,AIO2                                                          
         NI    SVFLAG,X'FF'-PRDSW  RESET PROD CHANGED                           
         B     LRRS10                                                           
                                                                                
LRRS160  XC    ELCOUNT,ELCOUNT                                                  
         B     LR310                                                            
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE *                                                  
*                                                                               
LRRPRT   LM    R0,R1,=A(HDHK,HEADING)     HEADING ROUTINE FOR REPORT            
         A     R0,SPTR25RR                                                      
         ST    R0,HEADHOOK         STORE FOR CONTROLLER                         
         A     R1,SPTR25RR                                                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         L     RE,AIO2                                                          
         LA    RF,2000             ZERO TABLE AREA                              
         XCEF                                                                   
                                                                                
         OC    BCLT,BCLT           WAS SPECIFIC CLT ENTERED                     
         BNZ   *+10                 YES                                         
         XC    SVCLT,SVCLT                                                      
                                                                                
         NI    SVFLAG,X'FF'-ACTFSW  INIT ACTIVITY FOUND SWITCH                  
         NI    SVFLAG,X'FF'-ACTPSW  INIT ACTIVITY PRINT SWITCH                  
         MVI   BYTE,0                                                           
         MVI   EOFSORT,C'N'                                                     
         USING SORTREC,R5                                                       
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET' GET FIRST RECORD                         
         ICM   R5,15,4(R1)                                                      
         BZ    NOACTIV                                                          
         B     LRRP10              AT LEAST 1 SORT REC                          
                                                                                
NOACTIV  MVC   P(44),=C' *** NO ACTIVITY SINCE LAST INSTRUCTIONS ***'           
         XC    HEADHOOK,HEADHOOK   DON'T GO TO HEADHOOK ROUTINE                 
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EXIT                                                             
                                                                                
LRRP10   L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
                                                                                
         LH    RF,SRTLEN           GET RECORD LENGTH                            
         LR    R3,RF                                                            
         LR    RE,R5                                                            
         L     R2,AIO1                                                          
         LR    R5,R2                                                            
         MVCL  R2,RE               MOVE SORT RECORD TO AIO1                     
         LA    R2,P                ADDRESS OF PRINT AREA                        
         USING PRTLINE,R2                                                       
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET'   NEXT RECORD                            
         ICM   R6,15,4(R1)                                                      
         BNZ   LRRP20              TEST NOT EOF                                 
         MVI   EOFSORT,C'Y'                                                     
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
                                                                                
LRRP20   GOTO1 =A(INSRECAP),RR=SPTR25RR READ INSTRUCTION RECAP RECS             
         LH    RF,SRTLEN                                                        
         CHI   RF,23               ANY DATES                                    
         BH    LRRP25              YES, PROCESS THIS BUY                        
         LR    R5,R6               ADDRESS OF NEXT RECORD                       
         CLI   EOFSORT,C'Y'        END OF SORT                                  
         BE    LRRP300             YES, DONE                                    
         B     LRRP10                                                           
                                                                                
LRRP25   TM    FLAG,TRACESW                                                     
         BZ    LRRP27                                                           
         GOTO1 =A(TRACEBUY),RR=SPTR25RR TRACE BUYACT FOR THIS REC               
                                                                                
LRRP27   OI    SVFLAG,ACTFSW       AT LEAST ONE ENTRY TO PRINT                  
         CLI   EOFSORT,C'Y'        LAST RECORD                                  
         BE    LRRP30                                                           
                                                                                
         CLC   4(16,R6),4(R5)      TEST IDENTICAL KEY ACROSS ESTIMATE           
         BNE   LRRP30                                                           
         MVI   SPOOLOK,C'N'        FILL UP PRINT LINE, BUT DON'T PRINT          
         B     LRRP35              (PRINT LINES GET MERGED)                     
                                                                                
LRRP30   MVI   SPOOLOK,C'Y'        OK TO PRINT                                  
                                                                                
LRRP35   CLC   SVCLT,SRTCLT        EJECT ON CHANGE OF CLIENT                    
         BE    LRRP40                                                           
         MVC   SVCLT,SRTCLT                                                     
         GOTO1 =A(FCLTR),RR=SPTR25RR                                            
*NOP     BNE   *+6                                                              
*NOP     DC    H'0'                                                             
         MVI   FORCEHED,C'Y'                                                    
LRRP40   CLC   SRTPROD,PROD        EJECT ON CHANGE OF PRODUCT                   
         BE    LRRP45                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PROD,SRTPROD                                                     
LRRP45   CLC   SRTEST,EST          EJECT ON CHANGE OF ESTIMATE                  
         BE    LRRP50                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   EST,SRTEST                                                       
                                                                                
LRRP50   CLI   FORCEHED,C'Y'       IF FORCING HEADLINE, PRINT MARKET            
         BNE   LRRP51                                                           
         TM    SVFLAG,ACTPSW       PRINT SWITCH                                 
         BZ    LRRP60                                                           
         MVI   FORCEHED,C'N'       TURN IT OF TO PRINT LAST ACTIVITY            
         MVI   BYTE,C'Y'           SAVE FORCEHED                                
         B     LRRP52                                                           
                                                                                
LRRP51   CLC   SVMKTSTA(5),SRTMKST CHANGE IN MARKET AND OR STATION              
         BNE   LRRP52                                                           
         B     LRRP70                                                           
                                                                                
LRRP52   CLC   0(132,R2),SPACES    ANYTHING TO PRINT                            
         BNH   LRRP55                                                           
         TM    SVFLAG,ACTPSW                                                    
         BZ    LRRP55                                                           
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         NI    SVFLAG,X'FF'-ACTPSW                                              
                                                                                
LRRP55   CLI   BYTE,C'Y'           WAS FORCEHED TURNED ON BEFORE                
         BNE   LRRP58                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   BYTE,C'N'                                                        
         B     LRRP60                                                           
                                                                                
LRRP58   CLC   SVMKTSTA(2),SRTMKST CHANGE IN MARKET                             
         BE    LRRP70                                                           
                                                                                
* READ MARKET RECORD FOR NAME AND PRINT ON SEPARATE LINE *                      
                                                                                
LRRP60   MVC   SVMKTSTA(2),SRTMKST CHANGE IN MARKET                             
         SR    R0,R0                                                            
         ICM   R0,3,SRTMKST                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
*                                                                               
         BAS   RE,RMKTNM                                                        
*                                                                               
         MVC   PRTMKT,QMKT                                                      
         MVC   PRTMKTNM,MKTNM                                                   
*                                                                               
         MVI   PRTMKTSW,C'Y'       SET PRINTING MARKET NAME LINE                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     LET CONTROLLER BUILD PAGE                    
*                                                                               
         MVI   PRTMKTSW,C'N'       SET OFF PRINTING MARKET NAME LINE            
*                                                                               
LRRP70   MVC   SVMKTSTA,SRTMKST                                                 
         GOTO1 =A(FMTSTA),RR=SPTR25RR                                           
         MVC   PRTSTA,STAPRNT                                                   
         MVC   PRTAFF,STAAFFL                                                   
*                                                                               
         OC    STANET,STANET       CABLE HEAD STATION                           
         BZ    *+10                                                             
         MVC   PRTSTA(8),STANET                                                 
*                                                                               
         CLI   STADRSW,C'*'                                                     
         BNE   LRRP80                                                           
         CLC   =C'BCM',CONREC                                                   
         BNE   LRRP75                                                           
         CLI   SVTANOAD,C'Y'       TEST SUPPRESS ERROR                          
         BE    LRRP80                                                           
*                                                                               
LRRP75   MVC   PRTSTA-3+132(14),=C'* NO ADDRESS *'                              
*                                                                               
LRRP80   EDIT  (B1,SRTSLN),(3,PRTSLN),ALIGN=LEFT                                
*                                                                               
         OC    SRTPROD2,SRTPROD2                                                
         BZ    LRRP90                                                           
         MVC   PRTPTR(3),SRTPROD2                                               
         LA    R3,PRTPTR+3                                                      
         CLI   PRTPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,SRTSLN2),(3,1(R3)),ALIGN=LEFT                                
                                                                                
LRRP90   LH    R1,SRTLEN                                                        
         SH    R1,=H'23'           SUBT FIXED REC LEN                           
         SR    R0,R0                                                            
         D     R0,=A(L'SRTENT)                                                  
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         LA    R1,PRTDTES+1                                                     
                                                                                
         DROP  R2                                                               
                                                                                
         LA    R2,WKTABLE                                                       
         LA    R3,SRTDT                                                         
         LR    RF,R1                                                            
LRRP95   CLC   0(2,R2),0(R3)       LOOK FOR DATE IN TABLE                       
         BE    LRRP185                                                          
         BL    LRRP97                                                           
         DC    H'0'                I MUST'VE PASSED IT????                      
                                                                                
* OVERLAPPING BUY DATES                                                         
                                                                                
*TEMP    AHI   R2,-2               BACK UP ONE IN TABLE                         
*        AHI   R1,-4               BACK UP ONE IN PRINTLINE                     
*        LA    RF,WKTABLE                                                       
*        CR    R2,RF               CHK PAST START OF TABLE                      
*        BNL   LRRP95                                                           
*TEMP    DC    H'0'                BUY DATE OUT OF TABLE RANGE                  
                                                                                
LRRP97   CLC   2(2,R2),0(R3)                                                    
         BE    LRRP140                                                          
         BL    LRRP180                                                          
                                                                                
         MVI   0(R1),C'*'          SHOW ACTIVE DATE WITH ASTERISK               
         MVI   1(R1),C'M'          SHOW AS MEDIA ENTRY                          
         OI    SVFLAG,ACTPSW       ACTIVITY PRINT SWITCH                        
                                                                                
         TM    SRTFLAG,SRTBB5                                                   
         BZ    LRRP110                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   132(2,RF),SPACES                                                 
         BE    LRRP100                                                          
                                                                                
         CLC   =C'B5',132(RF)                                                   
         BE    LRRP130                                                          
                                                                                
         CLC   =C'B5',264(RF)                                                   
         BE    LRRP130                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP100  MVC   132(2,RF),=C'B5'                                                 
                                                                                
LRRP110  TM    SRTFLAG,SRTBB10                                                  
         BZ    LRRP130                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   132(2,RF),SPACES                                                 
         BE    LRRP120                                                          
                                                                                
         CLC   =C'B10',132(RF)                                                  
         BE    LRRP130                                                          
                                                                                
         CLC   =C'B10',264(RF)                                                  
         BE    LRRP130                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP120  MVC   132(3,RF),=C'B10'                                                
                                                                                
LRRP130  CLC   0(2,R3),2(R3)       SAME START AND END DATES?                    
         BE    LRRP210                                                          
                                                                                
         CLC   0(2,R2),2(R3)       CHECK END DATE                               
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   2(2,R2),2(R3)                                                    
         BH    LRRP185                                                          
                                                                                
LRRP140  CLI   4(R2),0             PAST THE END OF THE DATE TABLE?              
         BE    LRRP145              YES, BYPASS                                 
                                                                                
         MVI   4(R1),C'*'          SHOW ACTIVE DATE WITH ASTERISK               
         MVI   5(R1),C'M'          SHOW AS MEDIA ENTRY                          
         OI    SVFLAG,ACTPSW       ACTIVITY PRINT SWITCH                        
                                                                                
LRRP145  TM    SRTFLAG,SRTBB5                                                   
         BZ    LRRP155                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   136(2,RF),SPACES                                                 
         BE    LRRP150                                                          
                                                                                
         CLC   =C'B5',136(RF)                                                   
         BE    LRRP210                                                          
                                                                                
         CLC   =C'B5',272(RF)                                                   
         BE    LRRP210                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP150  MVC   136(2,RF),=C'B5'                                                 
                                                                                
LRRP155  TM    SRTFLAG,SRTBB10                                                  
         BZ    LRRP210                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   136(2,RF),SPACES                                                 
         BE    LRRP160                                                          
                                                                                
         CLC   =C'B10',136(RF)                                                  
         BE    LRRP210                                                          
                                                                                
         CLC   =C'B10',272(RF)                                                  
         BE    LRRP210                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP160  MVC   136(3,RF),=C'B10'                                                
         B     LRRP210                                                          
                                                                                
LRRP180  LA    R1,4(,R1)                                                        
         LA    R2,2(,R2)                                                        
         B     LRRP95                                                           
                                                                                
LRRP185  MVI   0(R1),C'*'          SHOW ACTIVE DATE WITH ASTERISK               
         MVI   1(R1),C'M'          SHOW AS MEDIA ENTRY                          
         OI    SVFLAG,ACTPSW       ACTIVITY PRINT SWITCH                        
                                                                                
         TM    SRTFLAG,SRTBB5                                                   
         BZ    LRRP195                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   132(2,RF),SPACES                                                 
         BE    LRRP190                                                          
                                                                                
         CLC   =C'B5',132(RF)                                                   
         BE    LRRP210                                                          
                                                                                
         CLC   =C'B5',264(RF)                                                   
         BE    LRRP210                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP190  MVC   132(2,RF),=C'B5'                                                 
                                                                                
LRRP195  TM    SRTFLAG,SRTBB10                                                  
         BZ    LRRP210                                                          
                                                                                
         LR    RF,R1                                                            
         CLC   132(2,RF),SPACES                                                 
         BE    LRRP200                                                          
                                                                                
         CLC   =C'B10',132(RF)                                                  
         BE    LRRP210                                                          
                                                                                
         CLC   =C'B10',264(RF)                                                  
         BE    LRRP210                                                          
                                                                                
         LA    RF,132(RF)                                                       
LRRP200  MVC   132(3,RF),=C'B10'                                                
                                                                                
LRRP210  DS    0H                                                               
         LR    RF,R3               BUY DATES                                    
         LA    R3,L'SRTENT(,R3)                                                 
         CLC   0(2,R2),0(RF)       IF BUY DATE SAME AS TABLE DATE               
         BE    LRRP215             THEN DO NOT BUMP IN TABLE                    
                                                                                
         LA    R1,P                ADDRESS OF PRINT AREA                        
         USING PRTLINE,R1                                                       
         LA    R1,PRTDTES+1                                                     
                                                                                
         DROP  R1                                                               
                                                                                
         LA    R2,WKTABLE          AWAYS START FROM THE TOP                     
*NOP     LA    R1,4(,R1)                                                        
*NOP     LA    R2,2(,R2)                                                        
                                                                                
LRRP215  LR    RF,R1                                                            
         BCT   R0,LRRP95                                                        
                                                                                
         LR    R5,R6               SAVE ADDRESS OF NEXT RECORD                  
         CLI   SPOOLOK,C'Y'        TEST OK TO PRINT                             
         BNE   LRRP10              NO, LEAVE                                    
         B     LRRP310                                                          
                                                                                
LRRP300  TM    SVFLAG,ACTFSW       WAS THERE ANYTHING TO PRINT                  
         BZ    NOACTIV                                                          
                                                                                
LRRP310  GOTO1 SPOOL,DMCB,(R8)     LET CONTROLLER BUILD PAGE                    
                                                                                
         CLI   EOFSORT,C'N'        TEST END OF SORT FILE                        
         BE    LRRP10                                                           
                                                                                
         TM    FLAG,TRACESW        TRACE REQUEST?                               
         BZ    LRRPX                                                            
                                                                                
         L     RF,ABUYACT          BUYACT TABLE                                 
         OC    0(L'BUYENTRY,RF),0(RF) ANY ENTRIES                               
         BZ    LRRPX                  NO                                        
                                                                                
         GOTO1 =A(PTRACE),RR=SPTR25RR  GO PRINT BUYACT                          
                                                                                
LRRPX    B     EXIT                                                             
                                                                                
BUYEL    CLI   0(R6),0                                                          
         BE    BUYELX                                                           
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
BUYELX   LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
*SEE IF PRODUCTS MATCH (FOR POOL BUYS)                                          
                                                                                
MPRD     NTR1                                                                   
         MVI   SVPRD2,0            RESET PTR                                    
         CLI   BPRD,0              TEST ONE PRD REQUEST                         
         BE    MPRDX               NO - ALL PRODUCTS                            
         CLC   BPRD,10(R6)         TEST MATCH PROD                              
         BE    MPRDX                YES                                         
                                                                                
         CLI   1(R6),18            TEST PIGGYBACK                               
         BL    MPRDNX               NO                                          
         CLC   BPRD,14(R6)         PRD/PTR MATCH?                               
         BE    MPRDX                                                            
MPRDNX   CR    RB,RC                                                            
         B     MPRDXIT                                                          
                                                                                
MPRDX    MVC   SVPRD,10(R6)        PRODUCT                                      
         MVC   SVSLN,11(R6)        LENGTH                                       
         CLI   1(R6),18            TEST PIGGYBACK                               
         BL    MPRDEQ               NO                                          
         MVC   SVPRD2,14(R6)                                                    
         MVC   SVSLN2,15(R6)       P/B LENGTH                                   
MPRDEQ   CR    RB,RB                                                            
MPRDXIT  XIT1                                                                   
         EJECT                                                                  
* GET ROTATION                                                                  
                                                                                
GROT     NTR1                                                                   
         XC    ROTDAYS,ROTDAYS                                                  
         ZIC   R0,SVBDDAY          BUY DAY                                      
         SLL   R0,25                                                            
                                                                                
         CLI   OWRSDAY,0           OUT OF WEEK ROTATION START DATE              
         BE    GROT10                                                           
         ZIC   RF,OWRSDAY                                                       
                                                                                
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         O     R0,=X'00800000'                                                  
         SLL   R0,1                                                             
         BCT   RF,*-14                                                          
                                                                                
GROT10   LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BZ    *+16                                                             
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-14                                                             
                                                                                
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON     S ON          
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
                                                                                
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE ESTIMATE *                                                           
                                                                                
         DS    0H                                                               
VEST     NTR1                                                                   
         MVI   BEST,0                                                           
         MVI   OWRSDAY,0           OUT OF WEEK ROTATION START DATE              
         MVC   QEST,SPACES                                                      
                                                                                
         MVI   ESTSTAT,C'Y'                                                     
         CLI   5(R2),0             ANY DATA                                     
         BE    VEXIT               NO                                           
         CLC   =C'NO',8(R2)                                                     
         BNE   VEST10                                                           
         MVI   ESTSTAT,C'N'                                                     
         B     VEXIT                                                            
VEST10   TM    4(R2),X'08'         TEST DATA NUMERIC                            
         BO    VEST20              YES                                          
         B     ESTERR                                                           
*                                                                               
VEST20   ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,VESTPACK                                                      
         CVB   R1,DUB                                                           
         CHI   R1,1                TEST ESTIMATE BETWEEN 1 AND 255              
         BL    ESTERR                                                           
         CHI   R1,255                                                           
         BH    ESTERR                                                           
         CLI   BPRD,0              TEST PRODUCT ENTERED                         
         BE    NOPRDERR            NO, ERROR                                    
         XC    KEY,KEY             ESTIMATE HEADER                              
         MVC   KEY+1(3),BAGYMD     AGENCY, MEDIA, BCLT                          
         MVC   KEY+4(3),QPRD       PRODUCT                                      
         STC   R1,KEY+7            THE ALLEGED ESTIMATE NUMBER                  
         STC   R1,BEST                                                          
                                                                                
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTERR                                                         
         EDIT  (B1,BEST),(3,QEST),ALIGN=LEFT                                    
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
                                                                                
         USING ESTHDRD,R6                                                       
                                                                                
         MVC   QESTDESC,EDESC                                                   
         MVC   OWRSDAY,EOWSDAY                                                  
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
VEXIT    B     EXIT                                                             
                                                                                
VESTPACK PACK  DUB,TRAEST(0)                                                    
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
* BUILD TABLE OF WEEKS                                                          
                                                                                
TBL      NTR1                                                                   
         XC    WKTABLE(WKTABLEX-WKTABLE),WKTABLE                                
         XC    WKHDA,WKHDA                                                      
         XC    WKHDB,WKHDB                                                      
         MVC   WKTABLE,STDATE                                                   
         GOTO1 DATCON,DMCB,(2,WKTABLE),(0,WORK)                                 
         CLC   WKTABLE,STDATE                                                   
         BE    TBL20                                                            
         BL    TBL14                                                            
                                                                                
TBL10    GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-7' SUB 1 WEEK                          
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WKTABLE+2)                             
         MVC   WORK(6),WORK+6                                                   
         CLC   WKTABLE+2(2),STDATE                                              
         BH    TBL10                                                            
         MVC   WKTABLE(2),WKTABLE+2                                             
         B     TBL20                                                            
                                                                                
TBL14    GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7' ADD 1 WEEK                           
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WKTABLE+2)                             
         GOTO1 (RF),(R1),(0,WORK+6),(2,WKTABLE+2)                               
         MVC   WORK(6),WORK+6                                                   
         MVC   WKTABLE(2),WKTABLE+2                                             
         CLC   WKTABLE+2(2),STDATE                                              
         BL    TBL14                                                            
                                                                                
TBL20    ZIC   R2,PERWKS                                                        
         LA    R3,WKTABLE                                                       
         LA    R4,WKHDA                                                         
         LA    R5,WKHDB                                                         
                                                                                
TBL22    GOTO1 DATCON,DMCB,(2,(R3)),(0,WORK)                                    
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'7' ADD 1 WEEK                           
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WORK+12)                               
         MVC   2(2,R3),WORK+12                                                  
         GOTO1 (RF),(R1),(2,(R3)),(5,WORK)                                      
         MVC   0(3,R4),WORK                                                     
         MVC   1(2,R5),WORK+3                                                   
         CLC   ENDATE,WORK+12                                                   
         BL    EXIT                                                             
                                                                                
         LA    R3,2(,R3)                                                        
         LA    R4,4(,R4)                                                        
         LA    R5,4(,R5)                                                        
         BCT   R2,TBL22                                                         
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
* READ MARKET RECORD FOR NAME *                                                 
                                                                                
RMKTNM   NTR1                                                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
                                                                                
         L     R4,AIO2                                                          
         LA    R5,1960(,R4)        END OF TABLE-1                               
RMKTNM10 LA    R1,4(,R4)           MKT NAME IF ANY                              
         OC    0(4,R4),0(R4)       END OF TABLE                                 
         BZ    RMKTNM20                                                         
         CLC   QMKT,0(R4)                                                       
         BE    RMKTNM30                                                         
         LA    R4,28(,R4)                                                       
         CR    R4,R5                                                            
         BL    RMKTNM10                                                         
                                                                                
RMKTNM20 L     R3,AIO3                                                          
         USING MKTRECD,R3                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO3                     
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(15),0(R3)                                                    
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
                                                                                
         MVC   0(4,R4),QMKT                                                     
         MVC   4(24,R4),0(R1)                                                   
                                                                                
RMKTNM30 MVC   MKTNM,0(R1)                                                      
                                                                                
RMKTNMX  B     EXIT                                                             
         EJECT                                                                  
* CHECK FOR DELETED ESTIMATE *                                                  
                                                                                
CKDELEST NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY             ESTIMATE HEADER                              
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,SVKEY+BUYKCLT-BUYKEY                                     
         L     RE,ASVCLIST                                                      
         LA    RF,220                                                           
                                                                                
CKDEL10  CLC   3(1,RE),SVKEY+BUYKPRD-BUYKEY    ???? PRD=POL                     
         BE    CKDEL14                                                          
         LA    RE,4(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BNH   CKDEL26                                                          
         BCT   RF,CKDEL10                                                       
         B     CKDEL26                                                          
                                                                                
CKDEL14  MVC   EKEYPRD,0(RE)                                                    
         MVC   EKEYEST,SVKEY+BUYKEST-BUYKEY                                     
                                                                                
         LA    RE,BLOCK                                                         
         LA    RF,120                                                           
CKDEL20  OC    0(4,RE),0(RE)       EMPTY ENTRY (END)                            
         BZ    CKDEL30                                                          
         CLC   KEY+4(4),0(RE)                                                   
         BE    CKDEL26                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,CKDEL20                                                       
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         B     CKDEL30                                                          
                                                                                
CKDEL26  MVC   KEY,SVKEY           RESTORE KEY                                  
         CR    RE,RE               SET COND CODE FOR GOOD RETURN                
         B     EXIT                                                             
                                                                                
CKDEL30  DS   0H                                                                
                                                                                
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
                                                                                
         GOTO1 HIGH                                                             
         MVC   WORK(96),KEY        SAVE BOTH KEY AND KEYSAVE                    
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         MVC   KEY,SVKEY           RESTORE ORIG KEY                             
         GOTO1 HIGH                SET FOR SEQ                                  
         CLC   KEY,SVKEY                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(5),WORK+48     WAS THIS AGYMD AND CLIENT                    
         BNE   CKDEL50              BYPASS STORE                                
                                                                                
         LA    RE,BLOCK                                                         
         LA    RF,120                                                           
CKDEL40  OC    0(6,RE),0(RE)       EMPTY ENTRY (END)                            
         BZ    CKDEL44                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,CKDEL40                                                       
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         LA    RE,BLOCK                                                         
CKDEL44  MVC   0(4,RE),WORK+4      SAVE WHATEVER PRD/EST WAS FOUND              
                                                                                
CKDEL50  CLC   WORK(13),WORK+48    WAS EST HDR FOUND                            
         BE    EXIT                 YES                                         
                                                                                
         CLC   AGENCY,=C'TH'       IS THIS ZENITH                               
         BE    CKDEL60                                                          
         CLC   AGENCY,=C'BS'       IS THIS BACKER                               
         BNE   EXIT                                                             
CKDEL60  MVI   SVKEY+BUYKEST-BUYKEY,0                                           
         L     RF,AIO1                                                          
         MVI   BUYKEST-BUYKEY(RF),0                                             
         B     EXIT                                                             
         EJECT                                                                  
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
                                                                                
GOFF     NTR1                                                                   
                                                                                
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
                                                                                
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
                                                                                
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
                                                                                
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO CLEAR LIST SCREEN                                                  
*                                                                               
         DS    0H                                                               
CLEAR    NTR1                                                                   
         LA    R4,TRASELH          CLEAR LIST LINES OF ALL ACTIVITY             
         LA    R1,TRAPFKSH                                                      
CLR02    LA    R5,8(R4)            SKIP OVER FIELD HEADER                       
         LA    R0,3                LENGTH OF SELECT FIELD                       
CLR04    CLI   0(R5),C' '          LOOK FOR NON-BLANK CHARACTER                 
         BE    CLR06                                                            
         CLI   0(R5),X'00'                                                      
         BE    CLR06                                                            
         XC    8(3,R4),8(R4)       CLEAR FIELD AND TRANSMIT                     
         OI    6(R4),X'80'                                                      
         B     CLR08                                                            
CLR06    LA    R5,1(R5)                                                         
         BCT   R0,CLR04                                                         
CLR08    LA    R4,11(R4)           LIST FIELD HEADER                            
         LA    R5,8(R4)                                                         
         LA    R0,74               LENGTH OF LIST FIELD                         
CLR14    CLI   0(R5),C' '                                                       
         BE    CLR16                                                            
         CLI   0(R5),X'00'                                                      
         BE    CLR16                                                            
         XC    8(74,R4),8(R4)      CLEAR FIELD AND TRANSMIT                     
         OI    6(R4),X'80'                                                      
         B     CLR18                                                            
CLR16    LA    R5,1(R5)                                                         
         BCT   R0,CLR14                                                         
CLR18    LA    R4,82(R4)           NEXT SELECT FIELD HEADER                     
         CR    R4,R1               DON'T SPILL OVER SCREEN                      
         BL    CLR02                                                            
         B     EXIT                                                             
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
ESTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTMSG),ESTMSG                                         
         B     ERREXIT                                                          
NOPRDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPRDMSG),NOPRDMSG                                     
         LA    R2,TRAPRDH                                                       
         B     ERREXIT                                                          
NOESTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOESTMSG),NOESTMSG                                     
         B     ERREXIT                                                          
OFFLNERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFLNMS),OFFLNMS                                       
         B     ERREXIT                                                          
INVOFERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOFMS),INVOFMS                                       
         B     ERREXIT                                                          
NOACTERR BAS   RE,CLEAR                                                         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOACTMSG),NOACTMSG                                     
         LA    R2,TRAMEDH                                                       
         B     ERREXIT                                                          
                                                                                
RUNERR   LA    R2,CONWHENH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RUNMSG),RUNMSG                                         
ERREXIT  GOTO1 ERREX2                                                           
         EJECT                                                                  
*                                                                               
*AGENCY TABLE TO  SEE IF NEED TO RUN REPORT IN LONG SOON                        
*                                                                               
AGYTAB   DS    0XL9  TABLE OF AGY/MED/CLT/PROD                                  
         DC    CL2'RP',CL1'T',CL3'HRM',CL3'TEL'                                 
         DC    CL2'RP',CL1'T',CL3'HRM',XL3'000'                                 
         DC    CL2'G7',CL1'T',XL3'000',XL3'000'                                 
         DC    CL2'G7',CL1'R',XL3'000',XL3'000'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',XL3'000'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'NOR'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'EAS'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'TEX'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'PHX'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'POR'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'DEN'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'SEA'                                 
         DC    CL2'BN',CL1'R',CL3'SA1',CL3'VON'                                 
         DC    CL2'H7',CL1'T',XL3'000',XL3'000'                                 
         DC    CL2'H7',CL1'R',XL3'000',XL3'000'                                 
         DC    CL2'FR',CL1'T',XL3'000',XL3'000'                                 
         DC    CL2'FR',CL1'R',XL3'000',XL3'000'                                 
         DC    XL1'FF',CL8' '                                                   
AGYTABCT EQU   (*-AGYTAB)/L'AGYTAB                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OFFLNMS  DC    C'* ERROR * OFFICE MUST BE * AND 1 OR 2 CHARACTER *'             
INVOFMS  DC    C'* ERROR * INVALID OFFICE *'                                    
RUNMSG   DC    C'* ERROR * CAN''T BE RUN ONLINE *'                              
NOPRDMSG DC    C'* ERROR * ESTIMATE REQUIRES PRODUCT *'                         
NOESTMSG DC    C'* ERROR * NO SUCH ESTIMATE FOR THIS PRODUCT *'                 
NOACTMSG DC    C'* NOTE * NO ACTIVITY SINCE LAST INSTRUCTIONS *'                
ESTMSG   DC    C'* ERROR * ESTIMATE MUST BE NUMERIC (1-255) OR ''NO'' *+        
               '                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=600'                                   
                                                                                
HEADING  SSPEC H1,3,C'MEDIA'                                                    
         SSPEC H1,39,C'TRAFFIC BUY ACTIVITY REPORT'                             
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'CLIENT'                                                   
         SSPEC H2,39,C'---------------------------'                             
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,3,C'PRODUCT'                                                  
         SSPEC H3,40,C'PERIOD'                                                  
         SSPEC H5,3,C'ESTIMATE'                                                 
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H8,22,C'STATION  AFF'                                            
         SSPEC H8,37,C'SLN PARTNER'                                             
         SSPEC H8,50,C'---------------------- ACTIVITY WEEKS ----------+        
               -----------'                                                     
         SSPEC H9,3,C'------------------ -------  ---   --- -------'            
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
                                                                                
         DS    0H                                                               
                                                                                
FCLT     NMOD1 0,**FCLT***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         XC    BLOCK(240),BLOCK    CLEAR EST TABLE AREA                         
         XC    BLOCK+240(240),BLOCK+240                                         
                                                                                
* SAVE CURRENT RECORD                                                           
                                                                                
FCLT10   DS   0H                                                                
         MVC   SVKEY,KEY           SAVE BUY KEY                                 
                                                                                
         L     R0,AIO3             SAVE BUY RECORD                              
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
                                                                                
         GOTO1 CLUNPK,DMCB,SVCLT,QCLT                                           
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
                                                                                
         GOTO1 VALICLT                                                          
         MVC   OCLT,BCLT                                                        
         MVC   SVCLT,BCLT                                                       
         XC    BCLT,BCLT                                                        
         XC    FILENAME,FILENAME                                                
                                                                                
         MVC   KEY,SVKEY           RESTORE BUY KEY                              
                                                                                
         CLI   TRACLT,C'$'         BY OFFICE LIST                               
         BNE   FCLT12                                                           
                                                                                
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
                                                                                
* GO VALIDATE CLIENT OFFICE IS IN LIST                                          
         OC    TRACLT,SPACES                                                    
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TRACLT                                                 
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1,R6                                                            
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   FCLT14              GET NEXT CLIENT                              
         B     FCLT13                                                           
                                                                                
FCLT12   CLI   TRACLT,C'*'         BY OFFICE                                    
         BNE   FCLT13                                                           
         CLC   SVCLTOFF,TRACLT+1                                                
         BNE   FCLT14                                                           
                                                                                
FCLT13   MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
                                                                                
         CLI   ERROR,SECLOCK       ERR IS SECURITY LOCK-OUT                     
         BE    FCLT14                                                           
                                                                                
         CLI   ERROR,INVCLI        CLIENT NOT FOUND                             
         BE    FCLT14                                                           
         DC    H'0'                                                             
                                                                                
FCLT14   DS    0H                                                               
         MVI   KEY+3,0             CLEAR PRD FIELD                              
         ZICM  R1,KEY+1,2                                                       
         LA    R1,1(R1)            BUMP CLT UP 1                                
         STCM  R1,3,KEY+1                                                       
                                                                                
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         GOTO1 HIGH                READ FOR NEXT BUY                            
                                                                                
         CLC   KEY(1),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
                                                                                
         MVC   SVCLT,KEY+1         SAVE CLIENT                                  
                                                                                
* DO GETREC - WILL SAVE REC AT FCLT10                                           
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         B     FCLT10                                                           
                                                                                
FCLT20   DS    0H                                                               
         L     R0,AIO1             MOVE BUY RECORD BACK                         
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
                                                                                
* SEE IF CLIENT USES PRODUCT EQUIVALENCY *                                      
* READ T0 PROFILE *                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ T1 PROFILE *                                                             
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT1PR01,SVT1PROF+0                                              
         MVC   SVT1PR02,SVT1PROF+1                                              
                                                                                
* READ TA PROFILE *                                                             
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
                                                                                
* READ T3 PROFILE *                                                             
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT3PR06,SVT1PROF+5                                              
                                                                                
         CLI   SVTAXMED,C'Y'       EXCLUDE CLIENT                               
         BE    FCLT14                                                           
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      MUST BE THERE                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CR    RB,RB                                                            
         B     FCLTX                                                            
                                                                                
FCLTNE   DS    0H                                                               
         CR    RC,RB                                                            
FCLTX    XIT1                                                                   
         DROP  RB,RC                                                            
* FIND CLIENT HEADER AND SAVE CLIST                                             
                                                                                
FCLTR    NMOD1 0,**FCLTR**                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         XC    BLOCK(240),BLOCK    CLEAR EST TABLE AREA                         
         XC    BLOCK+240(240),BLOCK+240                                         
                                                                                
*NOP     MVC   SVKEY,KEY                                                        
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+CKEYAM-CLTHDRD(1),BAGYMD                                     
         MVC   KEY+CKEYCLT-CLTHDRD(2),SVCLT                                     
                                                                                
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE  BYPASS IF NO CLIENT HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   FILENAME,=C'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM                 
                                                                                
         L     R4,AIO3                                                          
         ST    R4,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         MVC   CLTNM,CNAME-CLTHDRD(R4)                                          
                                                                                
         MVC   SVCLTOFF,COFFICE-CLTHDRD(R4)                                     
         CLI   CTRAFOFC-CLTHDRD(R4),0                                           
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC-CLTHDRD(R4)                                    
                                                                                
         CLI   TRACLT,C'$'         BY OFFICER OFFICE                            
         BNE   *+10                                                             
         MVC   BOFFCD,SVCLTOFF                                                  
                                                                                
         LA    R2,CLIST-CLTHDRD(R4)                                             
                                                                                
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
                                                                                
* SEE IF CLIENT USES PRODUCT EQUIVALENCY *                                      
* READ T0 PROFILE *                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         GOTO1 CLUNPK,DMCB,SVCLT,QCLT                                           
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ T1 PROFILE *                                                             
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT1PR01,SVT1PROF+0                                              
         MVC   SVT1PR02,SVT1PROF+1                                              
                                                                                
* READ TA PROFILE *                                                             
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
                                                                                
* READ T3 PROFILE *                                                             
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT3PR06,SVT1PROF+5                                              
                                                                                
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* LOOK FOR A BUY ACTIVITY RECORD FOR THIS BUY                                   
                                                                                
TRACEBUY NMOD1 0,**TRACE**                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         L     RF,ABUYACT          BUY ACTIVITY TABLE                           
         USING NBUYACT,RF                                                       
                                                                                
         XC    SVDATE,SVDATE             AND BUY DATE                           
                                                                                
         LA    R3,SRTDT            POINT TO BUY DATES                           
                                                                                
         CLC   SVCLT,SRTCLT                                                     
         BE    TRACE05                                                          
                                                                                
         MVC   SVCLT,SRTCLT                                                     
         GOTO1 =A(FCLT),RR=SPTR25RR                                             
         BNE   *+6                                                              
         DC    H'0'                EXCLUDED CLT ?                               
                                                                                
TRACE05  XC    DTCHG,DTCHG         DATE CHANGED (FROM F1 ELEM)                  
         XC    DTADD,DTADD         DATE ADDED                                   
                                                                                
         XC    KEY,KEY                                                          
                                                                                
         MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),SRTCLT                                                  
         MVC   KEY+5(5),SRTMKST                                                 
         MVC   KEY+11(1),SRTEST                                                 
                                                                                
         L     R1,ASVCLIST                                                      
         LA    RE,SRTPROD                                                       
         LA    RF,KEY+10           PRODUCT                                      
                                                                                
TRACE10  CLC   0(3,R1),0(RE)       PRODUCTS MATCH ?                             
         BE    TRACE20                                                          
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    TRACE10                                                          
         DC    H'0'                UNKNOWN PRODUCT                              
                                                                                
TRACE20  MVC   0(1,RF),3(R1)       MOVE PRD CODE                                
                                                                                
         CLC   0(3,RE),SRTPROD2    IS THIS PROD PARTNER?                        
         BE    TRACE30                                                          
                                                                                
         OC    SRTPROD2,SRTPROD2   ANY PROD2                                    
         BZ    TRACE30                                                          
                                                                                
         L     R1,ASVCLIST                                                      
         LA    RE,SRTPROD2                                                      
         LA    RF,KEY+12           PRODUCT PARTNER                              
         B     TRACE10                                                          
                                                                                
TRACE30  MVC   SYSDIR(3),=C'TRF'   SWITCH TO TRAFFIC                            
         MVC   SYSFIL(3),=C'TRF'                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE     ANY BUY ACTIVITY ?                           
         BE    TRACE35                                                          
                                                                                
* BUY ACTIVITY RECORD NOT FOUND (FIX BUY DATE)                                  
                                                                                
TRACE33  OC    0(2,R3),0(R3)       ANY BUY DATES?                               
         BZ    TRACEX               NO, DONE                                    
                                                                                
         MVC   SVDATE,0(R3)        SAVE BUY DATE                                
                                                                                
         GOTO1 DATCON,DMCB,(2,SRTDT),(0,WORK)                                   
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,1                IF MONDAY                                    
         BE    TRACE100            THEN DONE                                    
                                                                                
* CHANGE DATE TO PREV MONDAY                                                    
                                                                                
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDATE)                                  
         B     TRACE100                                                         
                                                                                
* BUY ACTIVITY RECORD FOUND, PROCESS DATA                                       
                                                                                
TRACE35  LH    RF,SRTLEN                                                        
         AHI   RF,-23              SUBT FIXED REC LEN                           
         SR    RE,RE                                                            
         D     RE,=A(L'SRTENT)                                                  
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R4,RF               NUMBER OF BUYS                               
                                                                                
         MVC   SVSLN,SRTSLN        SAVE PROD LENGTH                             
         MVC   SVSLN2,SRTSLN2                                                   
                                                                                
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'F1'                                                     
                                                                                
         BAS   RE,GETELA                                                        
         BE    *+6                                                              
         DC    H'0'                NO F1 ELEMENT??                              
                                                                                
         USING ACTVD,R6                                                         
                                                                                
         MVC   DTADD,ACTVADDT      DATE RECORD WAS ADDED                        
         MVC   DTCHG,ACTVCHDT      DATE RECORD WAS CHANGED                      
                                                                                
         L     R6,AIO3                                                          
                                                                                
         MVI   ELCODE,X'05'                                                     
                                                                                
* DATE MUST BE MONDAY                                                           
                                                                                
TRACE40  MVC   SVDATE,0(R3)        SAVE BUY DATE                                
                                                                                
         GOTO1 DATCON,DMCB,(2,SRTDT),(0,WORK)                                   
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,1                IF MONDAY                                    
         BE    TRACE50             THEN DONE                                    
                                                                                
* CHANGE DATE TO PREV MONDAY                                                    
                                                                                
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDATE)                                  
                                                                                
TRACE50  BAS   RE,GETELA                                                        
         BNE   TRACE100            PRINT BUY DETAILS                            
                                                                                
TRACE60  CLC   SVDATE,2(R6)                                                     
         BNE   TRACE80             GET NEXT ELEM                                
                                                                                
         CLC   SVSLN,4(R6)         DO PRODUCT LENGTHS MATCH                     
         BNE   TRACE80             NO, GET NEXT ELEM                            
         CLC   SVSLN2,5(R6)                                                     
         BE    TRACE200                                                         
                                                                                
TRACE80  BAS   RE,NEXTELA                                                       
         BE    TRACE60                                                          
                                                                                
* ADD BUY INFO TO BUY ACTIVITY TABLE                                            
* (NO BUY ACTIVITY RECORD FOUND FOR THIS BUY)                                   
                                                                                
TRACE100 XC    BUYENTRY,BUYENTRY                                                
                                                                                
         LA    RF,BUYENTRY         BIULD BUY ENTRY HERE                         
         MVC   NBUYCLT,SRTCLT                                                   
         MVC   NBUYPRD,SRTPROD                                                  
         MVC   NBUYEST,SRTEST                                                   
         MVC   NBUYMKST,SRTMKST                                                 
         MVC   NBUYSLN,SRTSLN                                                   
         MVC   NBUYPRD2,SRTPROD2                                                
         MVC   NBUYSLN2,SRTSLN2                                                 
         MVC   NBUYDTE,SVDATE                                                   
         MVC   NBUYADT,DTADD       DATE RECORD WAS ADDED (FROM F1 ELEM)         
         MVC   NBUYCDT,DTCHG       DATE RECORD WAS CHNGD (FROM F1 ELEM)         
                                                                                
         L     RF,ABUYACT          BUY ACTIVITY TABLE                           
         LA    R0,BUYACTLN         NUMBER OF ENTRIES                            
                                                                                
TRACE110 OC    NBUYENT,NBUYENT     EMPTY ENTRY                                  
         BZ    TRACE150             YES                                         
                                                                                
         CLC   BUYENTRY,0(RF)      IS THIS ENTRY ALREADY IN THE TABLE           
         BE    TRACE200             YES                                         
                                                                                
         LA    RF,NBUYNEXT                                                      
         BCT   R0,TRACE110                                                      
         DC    H'0'                MAKE TABLE BIGGER                            
                                                                                
TRACE150 MVC   0(L'NBUYENT,RF),BUYENTRY SAVE THIS BUY IN TABLE                  
                                                                                
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                MAKE TABLE BIGGER                            
                                                                                
         LA    RF,NBUYNEXT                                                      
         XC    NBUYENT,NBUYENT     CLEAR NEXT ENTRY                             
                                                                                
TRACE200 LA    R3,L'SRTENT(R3)                                                  
                                                                                
         CLC   KEY(13),KEYSAVE     WAS BUY ACTIVITY RECORD FOUND                
         BNE   TRACE33                                                          
                                                                                
         L     R6,AIO3                                                          
         BCT   R4,TRACE40                                                       
                                                                                
TRACEX   MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         XIT1                                                                   
                                                                                
GETELA   AH    R6,DATADISP                                                      
*                                                                               
FIRSTELA CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BER   RE                                                               
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
NEXTELA  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTELA                                                         
                                                                                
         DROP  R5,RF,RB,RC                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* PRINT BUYS THAT ARE MISSING BUY ACTIVITY RECORDS                              
                                                                                
PTRACE   NMOD1 0,**PTRA***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         MVI   PRTMKTSW,C'Y'       SET OFF PRINTING MARKET NAME LINE            
         MVI   FORCEHED,C'Y'                                                    
         MVI   LINE,1                                                           
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         L     R3,ABUYACT          BUY ACTIVITY TABLE                           
         USING NBUYACT,R3                                                       
                                                                                
         LA    R4,BUYACTLN         NUMBER OF ENTRIES                            
                                                                                
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P(38),=C'NO BUY ACTIVITY RECORDS/ELEMENTS FOUND'                 
         MVC   P+38(15),=C' FOR THESE BUYS'                                     
         MVC   P2(38),=C'--------------------------------------------'          
         MVC   P2+38(15),=C'---------------'                                    
         MVC   P+102(7),=C'REC ADD'                                             
         MVC   P+111(7),=C'REC CHA'                                             
         MVC   P2+102(16),=C'-------  -------'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         LA    R2,P                                                             
PTRACE10 MVC   0(4,R2),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,NBUYCLT,4(R2)                                        
                                                                                
         MVC   8(4,R2),=C'PRD='                                                 
         MVC   12(3,R2),NBUYPRD                                                 
                                                                                
         MVC   16(4,R2),=C'EST='                                                
         EDIT  (B1,NBUYEST),(3,20(R2)),ALIGN=LEFT                               
                                                                                
         MVC   25(4,R2),=C'M/S='                                                
         BAS   R5,MKTSTA                                                        
         MVC   29(4,R2),QMKT                                                    
         MVC   34(5,R2),SVSTAKEY                                                
                                                                                
         MVC   40(4,R2),=C'LEN='                                                
         EDIT  (B1,NBUYSLN),(2,44(R2)),ALIGN=LEFT                               
                                                                                
         MVC   47(5,R2),=C'DATE='                                               
         GOTO1 DATCON,DMCB,(2,NBUYDTE),(8,52(R2))                               
                                                                                
         OC    NBUYPRD2,NBUYPRD2                                                
         BZ    PTRACE20                                                         
         MVC   61(4,R2),=C'PTR='                                                
         MVC   65(3,R2),NBUYPRD2                                                
         MVC   69(4,R2),=C'LEN='                                                
         EDIT  (B1,NBUYSLN2),(2,73(R2)),ALIGN=LEFT                              
PTRACE20 DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0A2E'                                                 
         MVC   ELEM+2(1),BAGYMD                                                 
         MVC   ELEM+3(2),NBUYCLT                                                
         MVC   ELEM+5(5),NBUYMKST                                               
         MVC   ELEM+11(1),NBUYEST                                               
                                                                                
         CLC   SVCLT,NBUYCLT                                                    
         BE    PTRACE25                                                         
                                                                                
         MVC   SVCLT,NBUYCLT                                                    
         GOTO1 =A(FCLT),RR=SPTR25RR                                             
         BNE   *+6                                                              
         DC    H'0'                EXCLUDED CLT ?                               
                                                                                
PTRACE25 L     R1,ASVCLIST                                                      
         LA    RE,NBUYPRD                                                       
         LA    RF,ELEM+10          PRODUCT                                      
                                                                                
PTRACE30 CLC   0(3,R1),0(RE)       PRODUCTS MATCH ?                             
         BE    PTRACE40                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    PTRACE30                                                         
         DC    H'0'                UNKNOWN PRODUCT                              
                                                                                
PTRACE40  MVC   0(1,RF),3(R1)       MOVE PRD CODE                               
                                                                                
         CLC   0(3,RE),NBUYPRD2    IS THIS PROD PARTNER?                        
         BE    PTRACE50                                                         
                                                                                
         OC    NBUYPRD2,NBUYPRD2   ANY PRD2                                     
         BZ    PTRACE50                                                         
                                                                                
         L     R1,ASVCLIST                                                      
         LA    RE,NBUYPRD2                                                      
         LA    RF,ELEM+12          PRODUCT PARTNER                              
         B     PTRACE30                                                         
                                                                                
PTRACE50 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,ELEM,75(R2),13  PRINT BUYACT KEY                     
                                                                                
         GOTO1 DATCON,DMCB,(3,NBUYADT),(8,102(R2))                              
         GOTO1 DATCON,DMCB,(3,NBUYCDT),(8,111(R2))                              
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         LA    R3,NBUYNEXT                                                      
                                                                                
         OC    NBUYENT,NBUYENT     EMPTY ENTRY                                  
         BZ    PTRACEX              YES                                         
                                                                                
         BCT   R4,PTRACE10                                                      
                                                                                
PTRACEX  XIT1                                                                   
                                                                                
MKTSTA   MVC   SYSDIR(3),=C'TRF'   SWITCH TO TRAFFIC                            
         MVC   SYSFIL(3),=C'TRF'                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',NBUYMKST),QMKT,WORK                           
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES                                                 
         BE    MKTSTA10                                                         
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
                                                                                
MKTSTA10 CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         CLC   SVSTAKEY,WORK       SAME STATION                                 
         BER   R5                                                               
         MVC   SVSTAKEY,WORK                                                    
         BR    R5                                                               
                                                                                
         DROP  RB,RC,R3                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* SEE IF THERE IS AN INSTRUCTION RECAP RECORD FOR THIS BUY                      
                                                                                
INSRECAP NMOD1 0,**IREC***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         USING SORTREC,R5                                                       
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
                                                                                
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM,BAGYMD                                                    
         MVC   INSKCLT,SRTCLT                                                   
         MVC   INSKPRD,SRTPRD                                                   
         MVC   INSKMKT(5),SRTMKST  MARKET/STATION                               
                                                                                
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   INSREC05             NO                                          
                                                                                
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    INSREC05             NOT CABLE                                   
                                                                                
*                    X'E8' NOW                                                  
         CLI   SRTMKST+2,CABLESTA  THIS A CABLE STATION                         
         BL    INSREC05             NO                                          
         NI    INSKMKT+4,CABLENET  GET RID OF LOB                               
                                                                                
INSREC05 MVC   INSKCOPY,SRTEST                                                  
                                                                                
         MVC   SYSDIR(3),=C'TRF'   SWITCH TO TRAFFIC                            
         MVC   SYSFIL(3),=C'TRF'                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INSRECX                                                          
                                                                                
         DROP  R4                                                               
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
                                                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETELI                                                        
         B     *+8                                                              
INSREC10 BAS   RE,NEXTELI                                                       
         BNE   INSRECX                                                          
                                                                                
         ST    R6,FULL             SAVE ADDR OF THIS ELEMENT                    
                                                                                
         USING INSDTAEL,R6                                                      
                                                                                
         CLC   INSPRD1,SRTPRD      SAME PRODUCTS                                
         BNE   INSREC10                                                         
         CLC   INSPRD2,SRTPRD2                                                  
         BNE   INSREC10                                                         
         CLC   INSSLN1,SRTSLN      AND LENGTHS                                  
         BNE   INSREC10                                                         
         CLC   INSSLN2,SRTSLN2                                                  
         BNE   INSREC10                                                         
                                                                                
         ZIC   R1,INSDTALN                                                      
         AHI   R1,-INSBSCEL        SUBT FIXED REC LEN                           
         SR    R0,R0                                                            
         D     R0,=A(INSSUBEL)                                                  
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               NUMBER OF PATTS                              
         LA    R2,INSPTTN          POINT TO CURRENT PAT                         
                                                                                
         LH    RF,SRTLEN                                                        
         AHI   RF,-23              SUBT FIXED REC LEN                           
         SR    RE,RE                                                            
         D     RE,=A(L'SRTENT)                                                  
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R4,RF               NUMBER OF BUYS                               
         LR    R3,R4                                                            
         LA    R6,SRTENT           POINT TO CURRENT BUY DATES                   
                                                                                
INSREC15 DS   0H                                                                
         MVC   WORK(2),3(R2)       INSFTD                                       
         MVC   WORK+2(2),5(R2)     INSLTD                                       
                                                                                
INSREC16 CLC   =X'FFFFFF',0(R2)    IS PAT TBA                                   
         BE    INSREC18                                                         
         OC    0(3,R2),0(R2)       CHK FOR HIATUS                               
         BNZ   INSREC20                                                         
                                                                                
INSREC18 DS    0H                                                               
         LA    R2,INSSUBEL(R2)     BUMP TO NEXT SET OF PATTERNS                 
         BCT   R0,INSREC15                                                      
         B     INSREC70                                                         
                                                                                
* SEE IF BUY DATES ARE OUTSIDE OF PATTERN DATES                                 
                                                                                
INSREC20 CLC   0(2,R6),WORK+2      IF BUY ST DTE > PAT END DTE                  
         BH    INSREC50            YES, BYPASS                                  
         CLC   2(2,R6),WORK        IF BUY END DTE < PAT ST DTE                  
         BL    INSREC50            YES, BYPASS                                  
                                                                                
         CLC   0(4,R6),WORK        DO START AND END DATE MATCH                  
         BE    INSREC25             YES                                         
         CLC   0(2,R6),WORK        BUY ST DTE TO PAT ST DTE                     
         BL    INSREC30                                                         
         CLC   2(2,R6),WORK+2      BUY END DTE TO PAT END DTE                   
         BH    INSREC30                                                         
                                                                                
INSREC25 XC    0(4,R6),0(R6)       BUY DTES W/N PAT DTES (CLEAR DTES)           
         BAS   RE,SDATES           & SHIFT DTES OVER ADJUST REC LEN             
         BCTR  R3,0                ADJUST # OF BUYS TO PROCESS                  
         B     INSREC51                                                         
                                                                                
INSREC30 MVC   WORK+10(4),0(R6)    SAVE BUY START/END DATES                     
         XC    0(4,R6),0(R6)       CLEAR SRTENT                                 
         CLC   WORK+10(2),WORK     BUY ST DTE TO PAT ST DTE                     
         BNL   INSREC35            STDATE FALLS WITHIN                          
         MVC   0(2,R6),WORK+10     MOVE BACK BUY START DATE                     
                                                                                
         GOTO1 DATCON,DMCB,(2,WORK),(0,WORK+50) START DATE                      
         GOTO1 ADDAY,(R1),WORK+50,WORK+50,F'-1' ST DTE -1 AS END DTE            
         GOTO1 DATCON,(R1),(0,WORK+50),(2,2(R6))  END DATE                      
                                                                                
INSREC35 XC    WORK+20(4),WORK+20                                               
         CLC   WORK+12(2),WORK+2   BUY END DTE TO PAT END DTE                   
         BNH   INSREC50            END DATE FALLS W/N PAT END DATE              
                                                                                
         GOTO1 DATCON,DMCB,(2,WORK+2),(0,WORK+50) END DATE                      
         GOTO1 ADDAY,(R1),WORK+50,WORK+50,F'1' END DTE +1 START DATE            
         GOTO1 DATCON,(R1),(0,WORK+50),(2,WORK+20)  START DATE                  
         MVC   WORK+22(2),WORK+12                                               
         OC    0(4,R6),0(R6)       ANY BUY DATES                                
         BNZ   INSREC40                                                         
         MVC   0(4,R6),WORK+20     NO, MOVE IN NEW DATES                        
         B     INSREC50                                                         
                                                                                
INSREC40 DS    0H                                                               
         BAS   RE,MDATES           MURGE BUY DATES IN SRT RECORD                
         CLI   WORK+20,X'FF'       DATES ADDED?                                 
         BNE   INSREC50                                                         
         LA    R3,1(R3)            ADD 1 TO # OF BUYS                           
                                                                                
INSREC50 DS    0H                                                               
         LA    R6,L'SRTENT(R6)     POINT TO NEXT SET OF BUY DATES               
INSREC51 BCT   R4,INSREC20                                                      
                                                                                
* LOOP FOR MORE PATTERNS IF ANY                                                 
                                                                                
         LA    R6,SRTENT           POINT TO CURRENT BUY DATES                   
         OC    SRTENT,SRTENT       ANY BUYS                                     
         BZ    INSRECX              NO,DONE                                     
                                                                                
         LR    R4,R3               NUMBER OF BUYS TO PROCESS                    
                                                                                
         LA    R2,INSSUBEL(R2)     BUMP TO NEXT SET OF PATTERNS                 
                                                                                
INSREC55 CLC   =X'FFFFFF',0(R2)    IS PAT TBA                                   
         BE    INSREC60                                                         
         OC    0(3,R2),0(R2)       CHK FOR HIATUS                               
         BZ    INSREC60                                                         
         BCT   R0,INSREC15                                                      
                                                                                
INSREC60 LA    R2,INSSUBEL(R2)                                                  
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNP   INSREC70                                                         
*NOP     BCT   R0,INSREC55                                                      
         B     INSREC55                                                         
                                                                                
INSREC70 OC    SRTENT,SRTENT       ANY BUYS?                                    
         BZ    INSRECX                                                          
                                                                                
         L     R6,FULL             ADDR OF LAST ELEMENT PROCESSED               
         B     INSREC10                                                         
                                                                                
INSRECX  MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         XIT1                                                                   
                                                                                
GETELI   AH    R6,DATADISP                                                      
*                                                                               
FIRSTELI CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BER   RE                                                               
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
NEXTELI  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTELI                                                         
         EJECT                                                                  
* MERGE BUY DATES IN SORTED RECORD                                              
                                                                                
* (WORK+20 = BUY START DATE)                                                    
* (WORK+22 = BUY END DATE)                                                      
* (R3 NUMBER OF BUYS)                                                           
                                                                                
MDATES   NTR1                                                                   
                                                                                
         LA    R6,SRTENT                                                        
                                                                                
MDATE10  CLC   0(4,R6),WORK+20     DATES ALREADY IN TABLE?                      
         BNE   *+14                                                             
         XC    WORK+20(4),WORK+20  YES, DONE                                    
         B     MDATEX                                                           
                                                                                
         CLC   0(2,R6),WORK+22     IF BUY ST DTE > PAT END DTE                  
         BH    MDATE20             YES, BYPASS                                  
         CLC   2(2,R6),WORK+20     IF BUY END DTE < PAT ST DTE                  
         BL    MDATE20             YES, BYPASS                                  
                                                                                
         CLC   0(2,R6),WORK+20                                                  
         BH    MDATE20                                                          
         CLC   2(2,R6),WORK+22                                                  
         BL    MDATE20                                                          
         XC    WORK+20(4),WORK+20  DATES FALL W/N                               
         B     MDATEX              DONE                                         
                                                                                
MDATE20  DS    0H                                                               
         LA    R6,L'SRTENT(R6)                                                  
         BCT   R3,MDATE10                                                       
         MVC   0(4,R6),WORK+20                                                  
         XC    WORK+20(4),WORK+20                                               
         MVI   WORK+20,X'FF'                                                    
         LH    R2,SRTLEN           RECORD LEN                                   
         AHI   R2,L'SRTENT                                                      
         STH   R2,SRTLEN           ADJUST RECORD LENGTH                         
                                                                                
MDATEX   XIT1                                                                   
                                                                                
* SHIFT DATES IN SORTED RECORD                                                  
                                                                                
SDATES   NTR1                                                                   
         LR    RE,R6               CURRENT POSITON                              
         LH    RF,SRTLEN           RECORD LEN                                   
         LR    R2,RF                                                            
         AHI   R2,-L'SRTENT                                                     
         STH   R2,SRTLEN           ADJUST RECORD LENGTH                         
                                                                                
         AHI   RF,-23              MINUS FIXED REC LEN                          
         LA    R4,SRTENT                                                        
         LR    R2,R6               R2 PTS TO MOVE TO SPACE                      
         SR    RE,R4                                                            
         SR    RF,RE               RF=LENGTH TO MOVE                            
         LA    RE,L'SRTENT(R6)     RE=PTS TO DATA TO MOVE                       
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
                                                                                
*                                  CLEAR LAST ENTRY                             
         AR    R2,RF                                                            
         AHI   R2,-L'SRTENT        BUMP TO PREVIOUS ENTRY                       
         XC    0(L'SRTENT,R2),0(R2) AND CLEAR IT                                
                                                                                
         XIT1                                                                   
                                                                                
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* FORMAT STATION FOR PRINTING *                                                 
                                                                                
FMTSTA   NMOD1 0,**FSTA***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         MVC   SYSDIR(3),=C'TRF'   SWITCH TO TRAFFIC                            
         MVC   SYSFIL(3),=C'TRF'                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),QMKT,WORK                           
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES                                                 
         BE    FMTSTA06                                                         
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
                                                                                
FMTSTA06 CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         CLC   SVSTAKEY,WORK       SAME STATION                                 
         BE    FMTSTAX                                                          
         MVC   SVSTAKEY,WORK                                                    
                                                                                
         L     R2,VADUMMY                                                       
         LR    R3,R2                                                            
         AHI   R3,9000             ROOM FOR 1000 STATIONS                       
         CLI   MODE,PRINTREP       THIS SPOOLED REPORT                          
         BNE   FMTSTA20             NO, TABLE BUILD                             
                                                                                
FMTSTA10 OC    0(5,R2),0(R2)       END OF TABLE                                 
         BZ    FMTSTA20                                                         
         CLC   WORK(5),0(R2)                                                    
         BE    FMTSTA30                                                         
         LA    R2,9(,R2)                                                        
         CR    R2,R3                                                            
         BL    FMTSTA10                                                         
                                                                                
         SH    R2,=H'9'                                                         
                                                                                
FMTSTA20 MVC   0(5,R2),WORK                                                     
         MVC   5(4,R2),SPACES                                                   
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         MVI   5(R2),C'*'                                                       
                                                                                
         MVC   6(3,R2),SPACES      AFFILIATE                                    
                                                                                
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   FMTSTA30                                                         
                                                                                
* READ STATION MASTER RECORD *                                                  
                                                                                
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK                                                    
                                                                                
         CLI   STAKCALL+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAKCALL+4(1),QMED                                               
                                                                                
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
                                                                                
         L     R4,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,(R4)                     
         CLI   8(R1),0                                                          
         BE    FMTSTA26                                                         
         MVC   SNETWRK,=C'???'                                                  
FMTSTA26 MVC   6(3,R2),SNETWRK                                                  
         MVC   STAAFFL,6(R2)                                                    
                                                                                
FMTSTA30 MVC   STADRSW,5(R2)                                                    
         MVC   STAAFFL,6(R2)                                                    
                                                                                
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
                                                                                
FMTSTAX  MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         XIT1                                                                   
                                                                                
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PERIOD (ALSO NOT MORE THAN 15 WEEKS) *                               
                                                                                
VPER     NMOD1 0,**VPER***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY)                                 
         XC    PERDTS,PERDTS                                                    
                                                                                
         CLI   5(R2),0                                                          
         BE    VPER10                                                           
                                                                                
         GOTO1 DATVAL,(R1),(0,8(R2)),WORK                                       
                                                                                
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    DATERR                                                           
                                                                                
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
                                                                                
         CLI   0(R1),1             TEST MONDAY                                  
         BNE   DAYERR                                                           
                                                                                
         LA    R3,1(R2,R3)                                                      
                                                                                
         GOTO1 DATCON,(R1),(0,WORK),(2,STDATE)                                  
                                                                                
         OC    8(2,R3),8(R3)                                                    
         BZ    VPER20                                                           
                                                                                
         GOTO1 DATVAL,(R1),(0,8(R3)),WORK                                       
                                                                                
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERR                                                           
                                                                                
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
                                                                                
         CLI   0(R1),7             TEST SUNDAY                                  
         BNE   DAYERR                                                           
                                                                                
         GOTO1 DATCON,(R1),(0,WORK),(2,ENDATE)                                  
                                                                                
         CLC   STDATE,ENDATE                 START NOT GREATER END              
         BH    DATERR                                                           
                                                                                
         GOTO1 DATCON,(R1),(2,STDATE),(0,SPDATE)                                
         GOTO1 DATCON,(R1),(2,ENDATE),(0,EPDATE)                                
         GOTO1 ADDAY,(R1),SPDATE,DUB,F'104'    ADD 15 WEEKS FOR OFFLINE         
                                                                                
         CLC   DUB(6),EPDATE                                                    
         BL    PERERR                                                           
                                                                                
         B     VPER30                                                           
                                                                                
VPER10   MVC   STDATE,TODAY        START OF PERIOD IS TODAY                     
                                                                                
VPER20   GOTO1 DATCON,(R1),(2,STDATE),(0,SPDATE)                                
         GOTO1 GETDAY,(R1),SPDATE,WORK+6                                        
         CLI   0(R1),0             TEST DAY RETURNED                            
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   R0,0(R1)            GET DAY NUMBER                               
         BCTR  R0,0                GIVES DAYS TO MONDAY                         
         LCR   R0,R0               SET TO BACK UP TO PREVIOUS MONDAY            
                                                                                
         GOTO1 ADDAY,(R1),SPDATE,WORK,(R0)                                      
                                                                                
         MVC   SPDATE,WORK                                                      
                                                                                
         GOTO1 DATCON,(R1),(0,SPDATE),(2,STDATE)                                
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,SVTAMWKS                                                    
         BNZ   *+8                                                              
         LA    R0,15               STANDARD IS 15 WEEKS                         
                                                                                
         STC   R0,PERWKS                                                        
         MHI   R0,7                                                             
         BCTR  R0,0                                                             
                                                                                
         GOTO1 ADDAY,(R1),SPDATE,EPDATE,(R0)   ADD 15 WEEKS FOR OFFLINE         
                                                                                
VPER27   GOTO1 DATCON,(R1),(0,EPDATE),(2,ENDATE)                                
                                                                                
VPER30   MVC   DUB,SPDATE                                                       
         GOTO1 DATCON,(R1),(0,DUB),(5,SPDATE)                                   
         MVC   DUB,EPDATE                                                       
         GOTO1 DATCON,(R1),(0,DUB),(5,EPDATE)                                   
         B     EXIT1                                                            
EXIT1    XIT1                                                                   
                                                                                
PERERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PERMSG),PERMSG                                         
         B     ERREXIT1                                                         
DAYERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DAYMSG),DAYMSG                                         
ERREXIT1 GOTO1 ERREX2                                                           
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
                                                                                
DAYMSG   DC    C'* ERROR * PERIOD MUST BEGIN ON MONDAY AND END ON SUNDA+        
               Y *'                                                             
PERMSG   DC    C'* ERROR * PERIOD CAN''T BE MORE THAN 15 WEEKS *'               
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - MARKET, LEN                                        
                                                                                
VFTR     NMOD1 0,**VFTR***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
*                                                                               
         XC    FILTERS,FILTERS                                                  
         XC    MGRPTBLE(200),MGRPTBLE                                           
         XC    MGRPTBLE+100(100),MGRPTBLE+100                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
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
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         B     VFTRERRX                                                         
VFTR08   GOTO1 SCANNER,DMCB,TRAFLTRH,(5,BLOCK)                                  
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         MKT=                                         
         BE    VFTR16                                                           
         EX    R1,VFTRCLCB         MARKET=                                      
         BNE   VFTR20                                                           
VFTR16   LA    R5,22(,R4)                                                       
         TM    3(R4),X'80'         WAS MARKET ENTRY NUMERIC                     
         BZ    NUMERRA                                                          
         MVC   MKTFTR,BMKT                                                      
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      MARKET                                       
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALIMKT                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
         MVC   WORK(2),MKTFTR                                                   
         MVC   MKTFTR,BMKT                                                      
         MVC   BMKT,WORK                                                        
         MVI   MEDSOON,C'Y'        RUN IN MEDIUM SOON                           
         B     VFTR100                                                          
VFTR20   EX    R1,VFTRCLCC         SPOT LEN                                     
         BNE   VFTR30                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERRA                                                          
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      SPOT LEN                                     
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR100                                                          
                                                                                
VFTR30   EX    R1,VFTRCLCD         MARKET GROUP                                 
         BNE   VFTR40                                                           
         CLI   1(R4),2                                                          
         BL    MGRPERR                                                          
         CLI   1(R4),6                                                          
         BH    MGRPERR                                                          
         CLI   22(R4),C'A'                                                      
         BL    MGRPERR                                                          
         CLI   22(R4),C'Z'                                                      
         BH    MGRPERR                                                          
                                                                                
         MVC   MKTGID,22(R4)       MARKET GROUP ID                              
                                                                                
         ZIC   RF,1(R4)            LENGTH                                       
         BCTR  RF,0                                                             
                                                                                
         LA    R1,23(,R4)                                                       
                                                                                
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
                                                                                
         CLI   0(R1),C'A'          SEE IF 2 CHAR MARKET GROUP                   
         BL    VFTR32                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VFTR32                                                           
         LA    R1,1(R1)                                                         
                                                                                
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
                                                                                
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,SPTR25RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
                                                                                
VFTR31   CLC   22(2,R4),0(RE)      IS THIS IT                                   
         BE    VFTR31C                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,VFTR31                                                        
                                                                                
         B     MGRPERR                                                          
                                                                                
VFTR31C  MVC   MKTGID,2(RE)        MOVE HEX VALUE FROM TABLE                    
                                                                                
         LA    RE,DUB                                                           
         ZIC   RF,1(R4)            LENGTH OF SECOND HALF OF FIELD               
         SHI   RF,2                                                             
                                                                                
VFTR32   CLI   0(R1),C'0'                                                       
         BL    MGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    MGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VFTR32                                                        
                                                                                
         PACK  WORK(3),DUB(5)                                                   
*NOP     MVC   SVPMGRP(1),22(R4)                                                
         MVC   SVPMGRP(1),MKTGID                                                
         MVC   SVPMGRP+1(4),DUB                                                 
         MVC   SVDMGRP(6),22(R4)                                                
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
*NOP     MVC   KEY+8(1),22(R4)                                                  
         MVC   KEY+8(1),MKTGID                                                  
         MVC   KEY+9(2),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VFTR34                                                           
                                                                                
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   BDMGRPER                                                         
                                                                                
VFTR34   LA    R5,MGRPTBLE                                                      
         LA    R6,150                                                           
VFTR36   MVC   0(2,R5),KEY+11                                                   
         LA    R5,2(,R5)                                                        
         BCT   R6,*+6                                                           
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BE    VFTR36                                                           
                                                                                
         XC    FILENAME,FILENAME                                                
         MVI   MEDSOON,C'Y'        RUN IN MEDIUM SOON                           
         B     VFTR100                                                          
                                                                                
VFTR40   EX    R1,VFTRCLCE         BB - PRINT ALL BILLBOARDS                    
         BNE   VFTR44                                                           
                                                                                
         MVI   BBFILT,C'Y'         SET TO SHOW ALL BILLBOARDS                   
                                                                                
VFTR44   EX    R1,VFTRCLCF         SHOW DISK ADDRESS                            
         BNE   VFTR50                                                           
                                                                                
         MVI   DSKADSW,C'Y'        SHOW DISK ADDRESS                            
                                                                                
VFTR50   EX    R1,VFTRCLCT         TRACE                                        
         BNE   VFTR120                                                          
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   VFTR120                                                          
         OI    FLAG,TRACESW                                                     
                                                                                
VFTR100  LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
                                                                                
NUMERRA  MVI   ERROR,NOTNUM                                                     
         B     TRAPERRA                                                         
MISSERRA MVI   ERROR,MISSING                                                    
TRAPERRA GOTO1 ERREX                                                            
                                                                                
VFTR120  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
VFTRERRX GOTO1 ERREX2                                                           
                                                                                
MGRPERR  MVC   CONHEAD,MGRPERMS                                                 
         B     VFTRERRX                                                         
                                                                                
BDMGRPER MVC   CONHEAD,BDMGRPMS                                                 
         CLI   SVDMGRP+1,C'Z'      2 CHAR MKT GROUP                             
         BH    BDMGR10              NO                                          
         MVC   CONHEAD+26(6),SVDMGRP                                            
         B     *+10                                                             
BDMGR10  MVC   CONHEAD+26(5),SVDMGRP                                            
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         B     VFTRERRX                                                         
                                                                                
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'VALID FILTERS - MKT/LEN/MGRP/BB= *'                            
VFTRCLCA CLC   12(0,R4),=CL4'MKT'                                               
VFTRCLCB CLC   12(0,R4),=CL7'MARKET'                                            
VFTRCLCC CLC   12(0,R4),=CL4'LEN'                                               
VFTRCLCD CLC   12(0,R4),=CL5'MGRP'                                              
VFTRCLCE CLC   12(0,R4),=CL3'BB'                                                
VFTRCLCF CLC   12(0,R4),=CL6'DSKAD '                                            
VFTRCLCT CLC   12(0,R4),=CL5'TRACE'                                             
MGRPERMS DC    CL60'* ERROR * MKT GROUP MUST BE 1-2 CHAR AND 1-4 DIGI *C        
               TS'                                                              
BDMGRPMS DC    CL60'* ERROR * NO MARKET GROUP X0000  FOUND *'                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE OFFICE CODE (ONLY SUPPORTED IN OFFLINE REPORT) *                     
*              -READ CLIENT HEADER RECORDS TO BUILD                             
*               TABLE OF CLIENTS FOR REQUESTED OFFICE                           
                                                                                
VOFF     NMOD1 0,**VOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERRA                                                          
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         GOTO1 HIGH                                                             
*                                                                               
VOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   OFFERR                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   OFFERR                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
*                                                                               
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
*                                                                               
VOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         USING CLTHDRD,R6                                                       
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
                                                                                
         CLI   LAOFFICE,0          IS THIS CLT LIMITED ACCESS                   
         BE    VOFF40               NO                                          
         CLC   SVCLTOFF,LAOFFICE   SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFF70                                                           
                                                                                
VOFF40   BRAS  RE,COFF             CHECK OFFICE VALIDITY                        
         BNE   VOFF20                                                           
                                                                                
VOFF70   CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
         BNE   VOFF90              NO, OFFICE LIST                              
                                                                                
         CLC   BOFFCD,SVCLTOFF     SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFFX                                                            
                                                                                
VOFF90   MVC   BOFFCD,SVCLTOFF                                                  
                                                                                
VOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
                                                                                
         XC    FILENAME,FILENAME                                                
         CR    R1,R1                                                            
                                                                                
         XIT1                                                                   
OFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFMSG),OFFMSG                                         
         GOTO1 ERREX2                                                           
OFFMSG   DC    C'* ERROR * NO CLIENTS FOUND FOR OFFICE *'                       
         DS    0H                                                               
                                                                                
REPERRA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSGA),REPMSGA                                       
         GOTO1 ERREX2                                                           
REPMSGA  DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* GET NEXT CLIENT FOR THIS OFFICE CODE (ONLY IN OFFLINE REPORT) *               
*          END OF CLIENTS, RETURN NE COND CODE                                  
                                                                                
NOFF     NMOD1 0,**NOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERR                                                           
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),OCLT       LAST CLIENT THIS OFFICE                      
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         GOTO1 HIGH                                                             
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
*                                                                               
NOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   NEQXIT                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NEQXIT                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
*                                                                               
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     NOFF10                                                           
*                                                                               
NOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
*                                                                               
*NOP     CLI   TRACLT,C'$'         USING OFFICER AS FILTER                      
*****    BNE   NOFF34                                                           
         BRAS  RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20               NOT OK                                      
                                                                                
         CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
         BNE   NOFF50              NO, OFFICE LIST                              
                                                                                
         CLC   BOFFCD,SVCLTOFF     SAME OFFICE                                  
         BNE   NOFF20                                                           
         B     NOFFX                                                            
                                                                                
NOFF50   MVC   BOFFCD,SVCLTOFF                                                  
                                                                                
NOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
                                                                                
         XC    FILENAME,FILENAME                                                
         CR    R1,R1               SET CC EQ                                    
                                                                                
         XIT1                                                                   
                                                                                
NEQXIT   XC    FILENAME,FILENAME                                                
                                                                                
         LTR   RB,RB               SET CC NOT EQUAL                             
         XIT1                                                                   
         EJECT                                                                  
* CHECK OFFICE TO BE VALID *                                                    
                                                                                
         DS   0H                                                                
COFF     NMOD1 0,**COFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         GOTO1 CLUNPK,DMCB,KEY+2,DUB                                            
                                                                                
         USING CLTHDRD,R6                                                       
                                                                                
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
                                                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,DUB                                                       
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
                                                                                
REPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSG),REPMSG                                         
         GOTO1 ERREX2                                                           
REPMSG   DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
         DS   0H                                                                
HDHK     NMOD1 0,**HDHK***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
*                                                                               
         MVC   H1+10(L'QMED),QMED                                               
         MVC   H1+15(L'MEDNM),MEDNM                                             
         MVC   H3+47(8),SPDATE                                                  
         MVI   H3+55,C'-'                                                       
         MVC   H3+56(8),EPDATE                                                  
*                                                                               
         CLI   TRACLT,C'$'         TEST OFFICER CODE GIVEN                      
         BNE   HDHK10                                                           
         MVI   H4+60,C'('                                                       
         MVC   H4+61(3),TRACLT     SHOW OFFICE CODE                             
         MVI   H4+64,C')'                                                       
*                                                                               
HDHK10   CLI   BOFFCD,0            TEST OFFICE CODE GIVEN                       
         BE    HDHK16              *SKIP MGROUP PRINTING                        
         MVC   H4+47(6),=C'OFFICE'                                              
         BAS   RE,CNVOFF           CONVERT 1 BYTE OFFICE CODE                   
         BE    HDHK16              OFFICE PRINTED                               
*                                                                               
         GOTO1 =V(OFFOUT),DMCB,BOFFCD,HEXOUT,H4+56                              
         B     HDHK16              OFFICE PRINTED                               
*                                                                               
*** DO NOT PRINT MGROUP **** PROBLEM WITH MULTIPLE MGROUP REQUEST ***           
*                                                                               
**HK14   OC    MGRPTBLE(2),MGRPTBLE   WAS MARKET GROUP ENTERED                  
         BZ    HDHK16                                                           
         MVC   H5+47(6),=C'MGROUP'                                              
         CLI   SVDMGRP+1,C'Z'      2 CHAR MKT GROUP                             
         BH    HDHK15               NO                                          
         MVC   H5+56(6),SVDMGRP    SHOW MARKET GROUP                            
         B     *+10                                                             
HDHK15   MVC   H5+56(5),SVDMGRP    SHOW MARKET GROUP                            
*                                                                               
HDHK16   MVC   H2+10(L'QCLT),QCLT                                               
         MVC   H2+15(L'CLTNM),CLTNM                                             
         MVC   H3+10(L'PROD),PROD                                               
         CLI   EST,0               TEST ESTIMATE = 'NO'                         
         BNE   HDHK20                                                           
         MVC   H5+11(3),=C'NO '                                                 
         B     HDHK40                                                           
                                                                                
HDHK20   EDIT  (B1,EST),(3,QEST),ALIGN=LEFT                                     
         MVC   H5+11(L'QEST),QEST                                               
                                                                                
HDHK40   XC    KEY,KEY             PRODUCT HEADER                               
         CLC   PROD,=C'???'        IS IT UNKNOWN PRODUCT                        
         BE    HDHK80                                                           
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),PROD                                                    
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   H3+15(20),PNAME                                                  
         DROP  R1                                                               
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
* SEE IF PRODUCT IS EQUIVALENT OR BASE *                                        
                                                                                
         CLI   SVPROF13,C'Y'                                                    
         BNE   HDHK50                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,SVCLT                                                    
         MVC   PEQKPRD,PROD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE                                  
         BNE   HDHK44               NO                                          
         MVC   H4+9(6),=C'(BASE)'                                               
         B     HDHK50                                                           
                                                                                
HDHK44   XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,SVCLT                                                    
         MVC   PEQPEPRD,PROD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS AN EQUIV                                
         BNE   HDHK50               NO                                          
         MVC   H4+10(26),=C'EQUIVALENT TO BASE PRODUCT'                         
         MVC   H4+10+27(3),PEQPBPRD                                             
         DROP  R4                                                               
                                                                                
HDHK50   CLI   EST,0                                                            
         BE    HDHK80                                                           
         XC    KEY,KEY             ESTIMATE HEADER                              
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),PROD                                                    
         MVC   KEY+7(1),EST                                                     
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
                                                                                
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDHK60                                                           
         MVC   H5+15(11),=C'**UNKNOWN**'                                        
         B     HDHK80                                                           
*NOP     TM    KEY+13,X'80'        DELETED REC                                  
****     BZ    *+6                                                              
HDHK60   DS   0H                   SHOULD HAVE DROPPED ALL DELETED EST          
         MVC   AIO,AIO3                                                         
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         L     R3,AIO                                                           
         USING ESTHDRD,R3                                                       
         MVC   H5+15(20),EDESC                                                  
         GOTO1 DATCON,DMCB,(0,ESTART),(8,H6+15)                                 
         GOTO1 (RF),(R1),(0,EEND),(8,H6+24)                                     
         MVI   H6+23,C'-'                                                       
         DROP  R3                                                               
                                                                                
HDHK80   MVC   H9+49(60),WKHDA     MOVE IN MONTH HEADING                        
         MVC   H10+49(60),WKHDB     MOVE IN DAYS HEADING                        
                                                                                
         CLI   PRTMKTSW,C'Y'       IS THIS A MARKET NAME LINE                   
         BE    HDHKX                YES                                         
         MVC   P4,P3                                                            
         MVC   P3,P2                                                            
         MVC   P2,P1                                                            
         MVC   P1,SPACES                                                        
                                                                                
         LA    R2,P               ADDRESS OF PRINT AREA                         
         USING PRTLINE,R2                                                       
         MVC   PRTMKT,QMKT                                                      
         MVC   PRTMKTNM,MKTNM                                                   
                                                                                
HDHKX    XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
                                                                                
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,BOFFCD                                                    
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
                                                                                
         MVC   H4+56(2),OFCOFC2                                                 
         CR    RB,RB               SET EQ CC                                    
                                                                                
CNVOFFX  XIT1                                                                   
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
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
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAA1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB1D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* OFFLINE SORT RECORD DSECT                                                     
                                                                                
SORTREC  DSECT                                                                  
SRTCOM   DS   0CL23                                                             
SRTLEN   DS    F                   LENGTH 23 AND UP IN INCREMENTS OF 4          
SRTCLT   DS    XL2                                                              
SRTPROD  DS    CL3                                                              
SRTEST   DS    XL1                                                              
SRTMKST  DS    XL5                                                              
SRTSLN   DS    XL1                                                              
SRTPROD2 DS    CL3                                                              
SRTSLN2  DS    XL1                                                              
SRTUPDT  DS   0XL2                **NOT USED** DATE OF LAST UPDATE              
*                                                                               
SRTFLAG  DS    XL1                 00 = PROD AND PTR ARE IN ORDER               
*                                  80 = PROD AND PTR ARE INVERTED               
SRTBB5   EQU   X'40'               BB=5                                         
SRTBB10  EQU   X'20'               BB=10                                        
*                                                                               
SRTPRD   DS    XL1                                                              
SRTPRD2  DS    XL1                                                              
*                                  VARIABLE PART OF RECORD                      
SRTENT   DS   0XL4                                                              
SRTDT    DS    XL2                 BUY DATE                                     
SRTDTX   DS    XL2                  PLUS ROTATION                               
                                                                                
* BUYS WITHOUT BUY ACTIVITY RECORDS                                             
                                                                                
NBUYACT  DSECT                                                                  
NBUYENT  DS   0CL(L'BUYENTRY)                                                   
NBUYCLT  DS    XL2                                                              
NBUYPRD  DS    CL3                                                              
NBUYEST  DS    XL1                                                              
NBUYMKST DS    XL5                                                              
NBUYSLN  DS    XL1                                                              
NBUYPRD2 DS    CL3                                                              
NBUYSLN2 DS    XL1                                                              
NBUYDTE  DS    XL2                                                              
NBUYADT  DS    XL3                                                              
NBUYCDT  DS    XL3                                                              
NBUYNEXT EQU   *                                                                
*                                                                               
* AGENCY TABLE FOR LONG SOONS                                                   
*                                                                               
LSAGYTAB DSECT                                                                  
LSAGY    DS    CL2                                                              
LSQMED   DS    CL1                                                              
LSQCLT   DS    CL3                                                              
LSQPRD   DS    CL3                                                              
LSNEXT EQU     *                                                                
         EJECT                                                                  
* DSECT FOR DISPLAY RECORD ONLINE (ALL DATES FOR A STATION) *                   
                                                                                
DSPLINED DSECT                                                                  
*                                                                               
DSPLINE  DS    0CL24               ***** DSECT FOR DISPLAY LINE DATA            
*                                                                               
DSPPRD   DS    CL7                 PROD/SLN                                     
         DS    CL1                                                              
DSPPTR   DS    CL7                 PARTNER/SLN                                  
         DS    CL1                                                              
DSPDTE   DS    CL8                 ACTIVITY DATE                                
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSTMKT   DS    CL4                                                              
         DS    CL3                                                              
LSTMKTNM DS    CL20                                                             
         DS    CL2                                                              
LSTSTA   DS    CL7                                                              
         DS    CL2                                                              
LSTPRD   DS    CL7                                                              
         DS    CL2                                                              
LSTPTR   DS    CL7                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
         DS    CL2                                                              
LSTDTE   DS    CL8                                                              
LSTIND   DS    CL3                                                              
                                                                                
PRTLINE  DSECT                     ***** DSECT FOR PRINT LINE DATA              
         DS    CL2                                                              
PRTMKT   DS    CL4                                                              
         DS    CL1                                                              
PRTMKTNM DS    CL24                                                             
         ORG   *-11                                                             
PRTNEW   DS    CL1                                                              
PRTSTA   DS    CL7                                                              
         DS    CL2                                                              
PRTAFF   DS    CL3                                                              
         DS    CL3                                                              
PRTSLN   DS    CL3                                                              
         DS    CL1                                                              
PRTPTR   DS    CL7                                                              
         DS    CL1                                                              
PRTDTES  DS    CL60                                                             
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
                                                                                
SPTR25RR DS    F                                                                
ADSPLINE DS    A                                                                
ABUYACT  DS    A                                                                
ROTDAYS  DS    H                                                                
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
SVELDATE DS    H                                                                
ELCOUNT  DS    H                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
*                                                                               
PRTMKTSW DS    CL1                 Y = THIS A MARKET NAME PRINT LINE            
*                                                                               
MEDSOON  DS    CL1                 Y = RUN THIS REPORT IN MEDIUM SOON           
*                                                                               
PERWKS   DS    XL1                 WEEKS TO COVER IN PERIOD                     
*                                                                               
PROD     DS    CL3                                                              
PROD2    DS    CL3                                                              
TODAY    DS    XL2                                                              
SVDATE   DS    XL2                 ADJUSTED TO MONDAY BUY DATE                  
*                                                                               
PERDTS   DS    0XL20                                                            
STDATE   DS    XL2                 START OF PERIOD                              
ENDATE   DS    XL2                 END OF PERIOD                                
SPDATE   DS    CL8                 START OF PERIOD                              
EPDATE   DS    CL8                 END OF PERIOD                                
*                                                                               
SVSLNS   DS   0XL2                                                              
SVSLN    DS    XL1                                                              
SVSLN2   DS    XL1                                                              
SVPRD2   DS    XL1                                                              
SVPRD    DS    XL1                                                              
*                                                                               
OWRSDAY  DS    XL1                 EOWSDAY FROM EST=OUT OF WK ROT ST DY         
SVBDDAY  DS    XL1                 BIT 0=SPARE 1=MON...7=SUN                    
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVFLAG   DS    XL1                                                              
*                                  80=POOL BUY                                  
PRDSW    EQU   X'40'               PRODUCT CHANGED                              
BUYFSW   EQU   X'20'               BUY FOUND SWITCH                             
BB10     EQU   X'10'               BB=10                                        
BB5      EQU   X'08'               BB=5                                         
ACTFSW   EQU   X'04'               ACTIVITY FOUND SWITCH                        
ACTPSW   EQU   X'04'               ACTIVITY PRINT SWITCH                        
                                                                                
MKTGID   DS    CL1                 MARKET GROUP ID                              
                                                                                
SVMKTSTA DS    XL5                                                              
SVCLT    DS    XL2                                                              
OCLT     DS    XL2                                                              
WKHDA    DS    CL128                                                            
WKHDB    DS    CL128                                                            
TBLSW    DS    XL1                                                              
*                                                                               
SVT2PR05 DS    CL1                 COMBINE CABLE NETWORKS(T2 PROFILE)           
SVT1PR01 DS    CL1                 ROTATOR DATES TO ADJUST                      
SVT1PR02 DS    CL1                 NUMBER OF DAYS TO ADJUST BY                  
SVT3PR06 DS    CL1                 EXCLUDE DV/SM FROM TRAFFIC                   
                                                                                
SVSTAKEY DS    CL5                                                              
STADRSW  DS    CL1                                                              
STAAFFL  DS    CL3                                                              
                                                                                
SVMGRP   DS    XL3                                                              
SVPMGRP  DS    CL5                 MGRP WITH TRAILING ZEROS                     
SVDMGRP  DS    CL6                 DISPLAYABLE MGRP AS ENTERED                  
                                                                                
WKTABLE  DS    32XL2               31 WEEKS, WITH X'00' IN LAST BYTE            
WKTABLEX EQU   *                                                                
                                                                                
FILTERS  DS    0CL7                                                             
MKTFTR   DS    XL2                                                              
SLNFTR   DS    XL1                                                              
HOLDSIGN DS    CL1                                                              
BBFILT   DS    CL1                                                              
DSKADSW  DS    CL1                                                              
FLAG     DS    CL1                                                              
TRACESW  EQU   X'80'               TRACE TO BUY ACTIVITY RECORD                 
                                                                                
MGRPTBLE DS    XL300               ROOM FOR 150 MARKETS                         
                                                                                
EST      DS    XL1                                                              
ESTSTAT  DS    CL1                 'N' IF EST= 'NO', OTHERWISE 'Y'              
BOFFCD   DS    CL1                 OFFICE CODE                                  
LAOFFICE DS    CL1                 OFFICE FOR THIS LIMITED ACCESS               
ACTVDATE DS    XL2                                                              
SPOOLOK  DS    CL1                                                              
EOFSORT  DS    CL1                                                              
DTADD    DS    XL3                 DATE RECORD ADDED                            
DTCHG    DS    XL3                 DATE RECORD CHANGED                          
NUMSLN   DS    F                   NUMBER OF SPOT LENGTH PAIRS                  
SLNWKKEY DS    XL10                SLNS AND ACTIVITY WEEK BITS                  
SLNWKLST DS    16XL10              UP TO 16 PAIRS PER LIST RECORD               
         DS   0F                                                                
SAVES    DS   0XL8                                                              
SAVEBBS  DS    F                                                                
SAVEWKS  DS    F                                                                
                                                                                
BUYENTRY DS    CL24                 BUILD BUY ENTRY HERE                        
BUYACTBL DS    0XL(500*L'BUYENTRY) 100 BUY ENTRIES                              
BUYACTLN EQU   (L'BUYACTBL)/L'BUYENTRY NUMBER OF ENTRIES                        
                                                                                
* TA PROFILE DEFINITIONS                                                        
                                                                                
SVTAXMED EQU   SVT1PROF+0          EXCLUDE CLIENT FROM MEDIA BUY                
SVTAXTBY EQU   SVT1PROF+1             "      "      "  TRAFFIC                  
SVTAXGOL EQU   SVT1PROF+2             "      "      "  GOAL                     
SVTAMWKS EQU   SVT1PROF+3          WEEKS TO SHOW ON MEDIA BUY ACTIVITY          
SVTANOTR EQU   SVT1PROF+4          DON'T SHOW *T TRAFFIC ACTIVITY               
SVTANOAD EQU   SVT1PROF+15         SUPPRESS 'NO ADDR' ON BCM REPORT             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPTRA25   12/06/16'                                      
         END                                                                    
