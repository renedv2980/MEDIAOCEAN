*          DATA SET SPTRA92    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T21692A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'T21692 TWX TRANSACTION REPORT/DAILY EXCEPTION REPORT'           
***********************************************************************         
*        TITLE 'T21692 TWX TRANSACTION REPORT/DAILY EXCEPTION REPORT'           
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CML SEQ RECORD FOR ADDS (IN PAR RTN)                  
*                    READ IN PRDHDR FOR PROD NAMES IN OFFLINE LIST              
*             AIO3 - REC READ IN FOR CHANGE COMPARE                             
*                    REC READ IN FOR CCUSA CML CHECKING                         
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - GRAPH BUFFER BUILD                                                
*        R5 -                                                                   
*        R6 - WORK REG                                                          
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
*        DUMMY FOR 150,000 USED FOR DELIVERY NOTICE SORT AREA                   
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
* THIS REPORT READS THE SYSPRINT FILE PUT OUT BY JIM ZIEGLERS GRAFNET           
* TRANSMISSION REPORT AND SPECIFICALLY USES                                     
*                                                                               
* O  BUFFER (10)(02)(01) RECS FOR PRINT QUE LIST AND INSTR HEADER INFO          
* I  BUFFER (10)(02)     FOR DELIVERED OK OR ERR                                
* SENDGRAFBUFFER  FOR CURRENT DATE/TIME                                         
* READBUFFER      FOR CURRENT DATE/TIME                                         
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV 03    OCT17/89 FIX EDIT AM/FM                                  *         
*  LEV 04-07 NOV02/89 FIX MATCH UP BUGS                               *         
*  LEV 08-10 MAR21/90 MAKE DUMMY AREA LARGER (FROM 50,000 TO 100,000) *         
*                     AND SHRINK DREC                                 *         
*  LEV 11    JUN19/91 ELIMINATE OFFLINE TW PROFILE CK                 *         
*  LEV 12    JUL01/91 SET FOR 8 CHAR AGENCY ID (BELATEDLY)            *         
*  LEV 13-14 JUL02/91 DO AND UNDO CHANGE FOR BLOCK+1 = WAS FBM        *         
*  LEV 15    JUL30/91 ADD CK FOR MARKREPORTSENT IN ADDITION TO BLANK  *         
*  LEV 16    AUG23/91 ADD SPACE FOR DLN FROM 100 TO 150, USE QSORT    *         
*  LEV 17    NOV21/91 ADD SPACE FOR DLN FROM 150 TO 260, USE QSORT    *         
*  LEV 18 SM APR11/01 USE TRAFFIC OFFICE                              *         
*  LEV 19 BG MAY04/01 CHANGE DUMMY                                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21692 TWX TRANSACTION REPORT/DAILY EXCEPTION REPORT'           
T21692A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TWXR**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR92RR                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                                                              
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE 3                                                                
VK       LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
* READ TW PROFILE *                                                             
         SPACE                                                                  
VK20     CLI   OFFLINE,C'Y'        IF OFFLINE DON'T CK TW PROFILE               
         BE    VK26                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TW'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         OC    BCLT,BCLT                                                        
         BZ    VK24                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
VK24     GOTO1 GETPROF,DMCB,WORK,SVTWPROF,DATAMGR                               
         SPACE                                                                  
         CLI   SVTWPR1,C'Y'        IS TWX ALLOWED                               
         BNE   TWXERR                                                           
         SPACE                                                                  
VK26     LA    R2,TRAPRDH          PRODUCT                                      
         XC    QPRD,QPRD                                                        
         MVI   BPRD,0                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                NO                                           
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3         GET BIN PROD                                 
         SPACE                                                                  
VK30     LA    R2,TRAPERH          PERIOD                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK40                NO                                           
         BAS   RE,VPER                                                          
         B     VK50                                                             
         SPACE                                                                  
VK40     GOTO1 DATCON,DMCB,(4,RCDATE),(5,STDATE)                                
         MVC   ENDATE,STDATE                                                    
         SPACE                                                                  
VK50     GOTO1 DATVAL,DMCB,(0,STDATE),STDATEN                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,ENDATE),ENDATEN                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* OFFLINE REPORT ROUTINE *                                                      
         SPACE                                                                  
LRR      XC    RECCT,RECCT         ZERO RECORD CT                               
         XC    SRTCT,SRTCT         ZERO SORTED RECORD CT                        
         XC    SVPROD,SVPROD                                                    
         XC    SVCLT,SVCLT                                                      
         XC    SVTOTAL,SVTOTAL     CLIENT TOTAL                                 
         XC    PRDTOTAL,PRDTOTAL   PRODUCT TOTAL                                
         XC    CANTOTAL,CANTOTAL   CANCELLED TOTAL                              
         XC    DLNTOTAL,DLNTOTAL   DELIVERED TOTAL                              
         XC    MISTOTAL,MISTOTAL   MISSING TOTAL                                
         XC    TRNTOTAL,TRNTOTAL   TOTAL TRANSACTIONS                           
         L     R1,=A(HEADING)      HEADING LINE FOR REPORT                      
         A     R1,SPTR92RR                                                      
         ST    R1,SPECS                                                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK                                                      
         XC    BLOCK(133),BLOCK    SET NO REC READ YET                          
         MVI   FIRSTSW,0                                                        
         SPACE                                                                  
         LA    R0,600              CLEAR 150,000 BYTES                          
         L     R1,VADUMMY                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(,R1)                                                      
         BCT   R0,*-10                                                          
         SPACE                                                                  
         LA    R2,TWXFILE                                                       
         OPEN  ((R2),(INPUT))                                                   
         SPACE                                                                  
         LTR   RF,RF               OPEN OK                                      
         BZ    LRR04                                                            
         ABEND 991                                                              
         SPACE                                                                  
LRR04    GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         SPACE                                                                  
* READ TWX TRANSMISSION FILE, BUILD SORT FILE *                                 
         SPACE                                                                  
         BAS   RE,GTW                                                           
         SPACE                                                                  
* SORT DELIVERY RECS *                                                          
         SPACE                                                                  
         SR    R0,R0                                                            
         L     RE,VADUMMY                                                       
         SPACE                                                                  
LRR06    OC    0(L'DREC,RE),0(RE)                                               
         BZ    LRR08                                                            
         LA    RE,L'DREC(,RE)                                                   
         BCT   R0,LRR06                                                         
LRR08    LPR   R0,R0                                                            
         LTR   R0,R0                                                            
         BZ    LRR10                                                            
         SPACE                                                                  
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     RF,0(R1)            PICK UP A(QSORT)                             
         GOTO1 (RF),(R1),VADUMMY,(R0),L'DREC,18,0                               
         SPACE                                                                  
LRR10    XC    SREC,SREC                                                        
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   LRR12                                                            
         LH    R3,CANTOTAL        PRODUCT TOTAL ONLY UPDATED                    
         AH    R3,DLNTOTAL        WHEN BREAK IN TOTAL                           
         AH    R3,MISTOTAL        SO IF AT END MUST ADD TO GET TOTAL            
         STH   R3,PRDTOTAL                                                      
         B     LRREND                                                           
LRR12    LH    R3,TRNTOTAL         TOTAL NUMBER OF TRANSACTIONS                 
         LA    R3,1(R3)                                                         
         STH   R3,TRNTOTAL                                                      
         SPACE                                                                  
* MOVE SORT REC TO SREC SO CAN SEE IT IN DUMPS *                                
         SPACE                                                                  
         MVC   SREC,0(R6)                                                       
         MVI   FIRSTSW,1                                                        
         SPACE                                                                  
         OC    SVCLT,SVCLT         FIRST TIME AROUND                            
         BNZ   LRR15                                                            
         MVC   SVCLT,SCLT                                                       
         MVC   SVPROD,SPRD                                                      
         B     LRR18                                                            
*                                                                               
LRR15    CLC   SVCLT,SCLT          SAME CLIENT                                  
         BE    LRR17               YES -CHECK PRODUCT                           
         LH    R3,CANTOTAL                                                      
         AH    R3,DLNTOTAL                                                      
         AH    R3,MISTOTAL                                                      
         STH   R3,PRDTOTAL                                                      
         BAS   R3,PRNTOT           NO                                           
         MVC   P+30(25),=CL25'CLIENT TOTAL - '                                  
         LH    R3,TRNTOTAL                                                      
         S     R3,=F'1'            THIS REC DOESN'T BELONG TO THIS CLT          
         STH   R3,TRNTOTAL                                                      
         EDIT  (2,TRNTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,1                                                             
         STH   R3,TRNTOTAL        RESET TRANTOTAL                               
         MVC   SVCLT,SCLT          MOVE IN NEW CLIENT                           
         MVC   SVPROD,SPRD         AND PRODUCT                                  
         MVI   FORCEHED,C'Y'                                                    
         B     LRR18                                                            
*                                                                               
LRR17    CLC   SVPROD,SPRD         SAME PRODUCT                                 
         BE    LRR18                                                            
         LH    R3,DLNTOTAL                                                      
         AH    R3,CANTOTAL                                                      
         AH    R3,MISTOTAL                                                      
         STH   R3,PRDTOTAL                                                      
         BAS   R3,PRNTOT                                                        
         MVC   SVPROD,SPRD         MOVE IN NEW PRODUCT                          
         MVI   FORCEHED,C'Y'                                                    
         B     LRR18                                                            
         EJECT                                                                  
* SEE IF DELIVERY REC FOR THIS REPORT *                                         
         SPACE                                                                  
LRR18    L     RE,VADUMMY                                                       
         L     RF,=A(260000/L'DREC)                                             
         SPACE                                                                  
LRR20    CLI   0(RE),0             AT END OF ENTRIES                            
         BE    LRR24                NO DELIVERY MESSAGE FOUND                   
         CLC   SCRN,DCRN-DREC(RE)                                               
         BNE   LRR22                                                            
         CLC   SRPT,DRPT-DREC(RE)                                               
         BE    LRR26               DELIVERY/CAN FOUND                           
LRR22    LA    RE,L'DREC(,RE)                                                   
         BCT   RF,LRR20                                                         
         SPACE                                                                  
LRR24    LH    R3,MISTOTAL         MISSING MESSAGE TOTAL                        
         LA    R3,1(R3)                                                         
         STH   R3,MISTOTAL                                                      
         MVC   SELNK,SPACES                                                     
         B     LRR30                                                            
*                                                                               
LRR26    DS    0H                                                               
         CLC   =C'CAN',DDELMS-DREC(RE)                                          
         BNE   LRR28                                                            
         MVC   PCAN,=C'CANX'                                                    
         LH    R3,CANTOTAL                                                      
         LA    R3,1(R3)                                                         
         STH   R3,CANTOTAL                                                      
         B     LRR29                                                            
*                                                                               
LRR28    CLC   =C'DLN',DDELMS-DREC(RE)                                          
         BE    *+6                                                              
         DC    H'0'                CAN ONLY BE CAN OR DLN                       
         LH    R3,DLNTOTAL                                                      
         LA    R3,1(R3)                                                         
         STH   R3,DLNTOTAL                                                      
*                                                                               
LRR29    MVC   SDELMS,DDELMS-DREC(RE)                                           
         MVC   SENDETM,DTIME-DREC(RE)                                           
         MVC   SELNK,DELNK-DREC(RE)                                             
         SPACE                                                                  
*                                                                               
LRR30    GOTO1 DATCON,DMCB,(0,SRUNDT),(8,SRUNDT8)                               
         MVC   PDATE,SRUNDT8       MOVE IN DATE                                 
         MVC   PSTA(4),SSTA                                                     
         LA    R1,PSTA+4                                                        
         CLI   PSTA+3,C' '                                                      
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'-TV'                                                  
         CLI   SSTA+4,C'T'                                                      
         BE    LRR32                                                            
         MVC   0(3,R1),=C'-FM'                                                  
         CLI   SSTA+4,C'F'                                                      
         BE    LRR32                                                            
         MVC   0(3,R1),=C'-AM'                                                  
         CLI   SSTA+4,C'A'                                                      
         BE    LRR32                                                            
         MVC   0(3,R1),=C'-X '                                                  
         CLI   SSTA+4,C'X'        RADIO NETWORK                                 
         BE    LRR32                                                            
         MVC   0(3,R1),=C'   '    NETWORK                                       
         CLI   SSTA+4,C'N'         LEAVE AS CBS                                 
         BE    LRR32                                                            
         DC    H'0'                INVALID MEDIA                                
*                                                                               
LRR32    MVC   PCRN,SCRN           MOVE IN CUSTOMER REFERENCE                   
         XC    WORK(10),WORK                                                    
         EDIT  (C5,SRPT+3),(4,WORK+4),ALIGN=LEFT                                
         MVC   WORK(3),SRPT                                                     
         MVC   WORK+3(1),=C','                                                  
         MVC   PREPTID,WORK        MOVE IN REPORT ID                            
         SPACE                                                                  
         OC    SELNK,SELNK                                                      
         BZ    LRR40                                                            
         MVC   PELNK,SELNK         MOVE IN EASYLINK LEDGER                      
LRR40    MVC   PSENT,SENDSTM       MOVE IN TIME SENT                            
         MVI   PSENT+2,C'.'                                                     
         SPACE                                                                  
         MVC   PDELIV,SENDETM      MOVE IN DELIV TIME                           
         OC    PDELIV,PDELIV                                                    
         BZ    LRR44                                                            
         MVI   PDELIV+2,C'.'                                                    
         SPACE                                                                  
* ADJUST STATION RECEIVED TIME TO DDS FUNNY TIME (00.00 = 08.00) *              
         SPACE                                                                  
LRR44    MVC   WORK(2),=C'00'                                                   
         MVN   WORK(2),SENDETM     MOVE NUMERICS                                
         CLC   WORK(2),SENDETM     IF NOT NUMERIC, BYPASS                       
         BNE   LRR54                                                            
         PACK  DUB,SENDETM(2)                                                   
         CP    DUB,=P'7'           IF LESS THAN 8                               
         BH    *+10                                                             
         AP    DUB,=P'24'          ADD 24 HOURS                                 
         SP    DUB,=P'8'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  PDELIV(2),DUB                                                    
         SPACE                                                                  
LRR54    MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR10                                                            
         EJECT                                                                  
* E-O-F -- CHECK FOR ANY SORTED RECS *                                          
LRREND   CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   LRRENDA             NO                                           
         MVC   PCRN+132(30),=CL30'NO ACTIVITY FOR REPORT PERIOD'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LRRENDA  GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (TWXFILE,)                                                       
         BAS   R3,PRNTOT                                                        
         MVC   P+30(25),=CL25'CLIENT TOTAL - '                                  
         EDIT  (2,TRNTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
* PRINT TOTALS                                                                  
         SPACE                                                                  
PRNTOT   DS    0H                                                               
         MVC   P(132),SPACES                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    DLNTOTAL,DLNTOTAL                                                
         BZ    PRNT10                                                           
         MVC   P+30(25),=CL25'DELIVERIES RECEIVED - '                           
         EDIT  (2,DLNTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRNT10   OC    CANTOTAL,CANTOTAL                                                
         BZ    PRNT20                                                           
         MVC   P+30(25),=CL25'CANCELLATIONS RECEIVED - '                        
         EDIT  (2,CANTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRNT20   OC    MISTOTAL,MISTOTAL                                                
         BZ    PRNT30                                                           
         MVC   P+30(25),=CL25'NO NOTICE RECEIVED - '                            
         EDIT  (2,MISTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRNT30   DS    0H                                                               
         OC    PRDTOTAL,PRDTOTAL                                                
         BZ    PRNT40                                                           
         MVC   P+30(25),=CL25'PRODUCT TOTAL - '                                 
         EDIT  (2,PRDTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRNT40   DS    0H                                                               
         XC    CANTOTAL,CANTOTAL                                                
         XC    DLNTOTAL,DLNTOTAL                                                
         XC    MISTOTAL,MISTOTAL                                                
         XC    PRDTOTAL,PRDTOTAL                                                
         BR    R3                                                               
         EJECT                                                                  
* VALIDATE PERIOD                                                               
         SPACE                                                                  
         DS    0H                                                               
VPER     NTR1                                                                   
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         BZ    DATERR                                                           
         LA    R3,1(RE,R3)         POINT TO END DATE                            
         CLM   RE,1,5(R2)          ONLY 1 DATE ENTERED                          
         BNE   VPER10                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(5,STDATE)                                  
         MVC   ENDATE,STDATE                                                    
         MVC   WORK+6(6),WORK                                                   
         B     VPER20                                                           
VPER10   GOTO1 DATVAL,(R1),(R3),WORK+6                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(5,STDATE)                                  
         GOTO1 (RF),(R1),(0,WORK+6),(5,ENDATE)                                  
         CLC   WORK(6),WORK+6                                                   
         BH    DATSQER                                                          
         SPACE                                                                  
* DATES MUST BE WITHIN THIS MONTH AND NOT AFTER TODAY *                         
         SPACE                                                                  
VPER20   GOTO1 DATCON,DMCB,(4,RCDATE),(0,DUB)                                   
         CLC   DUB(6),WORK+6            NOT IN THE FUTURE                       
         BL    FUTDATER                                                         
         SPACE                                                                  
         MVC   DUB+4(2),=C'01'                                                  
         CLC   DUB(6),WORK         MUST BE WITHIN THIS MONTH                    
         BH    MONDATER                                                         
         B     EXIT                                                             
         EJECT                                                                  
* GET GRAPHNET TRANS RECS AND BUILD 1 TRANS REC *                               
         SPACE                                                                  
         DS    0H                                                               
GTW      NTR1                                                                   
         SPACE                                                                  
GTW10    BAS   RE,GETWX                                                         
         SPACE                                                                  
GTW20    CLC   =C'O  BUFFER',BLOCK+1  SENT MESSAGE                              
         BNE   GTW30                                                            
         SPACE                                                                  
         CLC   GMSGID(12),=C'(10)(02)(01)'                                      
         BNE   GTW10                                                            
         BAS   RE,BMS              GO BUILD MESSAGE IN AIO1                     
         SPACE                                                                  
         LA    R4,RSTABLE                                                       
         LA    R5,SREC                                                          
         BAS   RE,SBLD             BUILD SORT REC                               
         BNE   GTW20                WRONG OR BAD, BYPASS                        
         SPACE                                                                  
         B     GTW20                                                            
         SPACE                                                                  
GTW30    CLC   =C'MARKREPORTSENT',BLOCK+1                                       
         BNE   GTW40                                                            
         SPACE                                                                  
         MVC   SENDSTM,TIME        MOVE SENT TIME AND                           
         MVC   SENDATE,DATE        DATE INTO RECORD                             
         SPACE                                                                  
         BAS   RE,FTR                                                           
         BNE   GTW26                                                            
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LH    R1,SRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,SRTCT                                                         
         SPACE                                                                  
GTW26    XC    SREC,SREC                                                        
         B     GTW10                                                            
         SPACE                                                                  
GTW40    CLC   =C'I  BUFFER',BLOCK+1                                            
         BNE   GTW50                                                            
         SPACE                                                                  
         CLC   GMSGID(8),=C'(10)(02)'                                           
         BNE   GTW10                                                            
         CLC   GMSGID+8(3),=C'CAN'      ONLY WANT DELIVERED                     
         BE    GTW43                    OR CAN MESSAGES                         
         CLC   GMSGID+8(3),=C'DLN'                                              
         BNE   GTW10                                                            
         SPACE                                                                  
GTW43    BAS   RE,BMS              GO BUILD MESSAGE IN AIO1                     
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,DATE),WORK                                        
         CLC   STDATEN,WORK        IF BEFORE START DATE, IGNORE                 
         BH    GTW20                                                            
         BE    GTW46                                                            
         CLC   ENDATEN,WORK        IF EQ OR HI, OK                              
         BNL   GTW46                                                            
         GOTO1 ADDAY,DMCB,ENDATEN,WORK+6,F'1'                                   
         CLC   WORK(6),WORK+6      ALLOW NEXT DAY DELIVERY NOTICES              
         BNE   GTW20                                                            
         SPACE                                                                  
GTW46    LA    R4,RDTABLE                                                       
         LA    R5,DREC                                                          
         BAS   RE,DBLD             BUILD DELIVERY REC                           
         BNE   GTW20                WRONG OR BAD, BYPASS                        
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(4,DATE),(2,DDATE)                                   
         SPACE                                                                  
         BAS   RE,PUTD                                                          
         B     GTW20               NOW CK LAST REC READ                         
         SPACE                                                                  
GTW50    CLI   BLOCK+1,255                                                      
         BNE   GTW10                                                            
         B     EXIT                                                             
         EJECT                                                                  
* READ TWX TRANSMISSION FILE RECS *                                             
         DS    0H                                                               
GETWX    NTR1                                                                   
         SPACE                                                                  
GETWX10  GET   TWXFILE,BLOCK                                                    
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
         SPACE                                                                  
         CLC   BLOCK+1(132),SPACES BLANK LINE OR HEADER                         
         BE    GETWX10                                                          
         CLC   =C'DATE ',BLOCK+1   HEADER FROM PAGE BREAK                       
         BE    GETWX10                                                          
         CLC   =28C'-',BLOCK+55                                                 
         BE    GETWX10                                                          
         CLC   =C'SENDEOT ',BLOCK+1                                             
         BE    GETWX10                                                          
         SPACE                                                                  
         CLC   =C'INITIALIZE',BLOCK+1    FIRST OF EVERY DAY                     
         BE    GETWX14                                                          
         CLC   =C'SENDTOWEST',BLOCK+1    SENDING TO WESTERN UNION               
         BE    GETWX14                                                          
         CLC   =C'SENDWESTBUFFER',BLOCK+1 SEND BUFFER TO WESTERN UNION          
         BE    GETWX14                                                          
         CLC   =C'CHECKFORBID',BLOCK+1    THIS A HEADER LINE                    
         BE    GETWX14                                                          
         CLC   =C'FINDMATCHING',BLOCK+1    THIS A HEADER LINE                   
         BE    GETWX14                                                          
         CLC   =C'READBUFFER',BLOCK+1      THIS A HEADER LINE                   
         BE    GETWX14                                                          
         CLC   =C'PROCESSINBOUND ',BLOCK+1 THIS A HEADER LINE                   
         BE    GETWX14                                                          
         CLC   =C'ENDINBOUND ',BLOCK+1     THIS A HEADER LINE                   
         BNE   GETWX20                                                          
GETWX14  CLC   GDATE,SPACES                                                     
         BE    GETWX10                                                          
         MVC   TIME,GTIME                                                       
         MVC   DATE,GDATE                                                       
         B     GETWX10                                                          
         SPACE                                                                  
GETWX20  B     EXIT                                                             
         SPACE                                                                  
ENDTWX   MVI   BLOCK,255                                                        
         MVC   BLOCK+1(132),BLOCK                                               
         B     EXIT                                                             
         EJECT                                                                  
* BUILD MESSAGE AS SENT/RECEIVED TO/FROM WESTERN UNION *                        
         DS    0H                                                               
BMS      NTR1                                                                   
         L     R4,AIO1                                                          
         LA    R0,8                                                             
         LR    R1,R4                                                            
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(,R1)                                                      
         BCT   R0,*-10                                                          
         SPACE                                                                  
         LR    R6,R4                                                            
BMS10    MVC   0(81,R6),GMSGID                                                  
         LA    R6,81(,R6)                                                       
         LA    RF,GMSGID+81                                                     
BMS14    CLI   0(RF),C' '          MOVE ANY OVER 81 CHARACTERS                  
         BNH   BMS20                                                            
         MVC   0(1,R6),0(RF)                                                    
         LA    R6,1(,R6)                                                        
         LA    RF,1(,RF)                                                        
         B     BMS14                                                            
         SPACE                                                                  
BMS20    C     R6,AIO2                                                          
         BNL   EXIT                                                             
         SPACE                                                                  
         BAS   RE,GETWX                                                         
         CLC   =C'MARKREPORTSENT',BLOCK+1                                       
         BE    EXIT                                                             
         CLI   BLOCK+1,C' '                                                     
         BNH   BMS10                                                            
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* BUILD SORT REC FOR TWX MESSAGE SENT *                                         
         DS    0H                                                               
SBLD     NTR1                                                                   
         L     R2,AIO1                                                          
         LA    R0,2000                                                          
         USING RTABLED,R4                                                       
         SPACE                                                                  
         LA    R3,1                LINE CT                                      
         LA    R2,12(R2)                                                        
         B     SBLD30                                                           
*                                                                               
SBLD14   CLC   0(8,R2),=C'(0D)(25)'                                             
         BE    SBLD20                                                           
         LA    R2,1(,R2)                                                        
         C     R2,AIO2                                                          
         BL    SBLD14                                                           
         CR    RB,RD               SET BYPASS                                   
         B     EXIT                                                             
SBLD20   LA    R2,8(R2)                                                         
         LA    R3,1(R3)                                                         
         SPACE                                                                  
SBLD30   CLM   R3,1,RENT                                                        
         BL    SBLD14                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   R1,1,RLEN                                                        
         ICM   RE,3,RDISP                                                       
         ICM   RF,1,RPOS                                                        
         LA    RE,0(R5,RE)                                                      
         LA    RF,0(R2,RF)                                                      
         SPACE                                                                  
         EX    R1,SBLDMVC                                                       
SBLD40   LA    R4,RNEXT                                                         
         CLI   RENT,255            END                                          
         BL    SBLD30                                                           
         GOTO1 DATVAL,DMCB,(0,DATE),SRUNDT                                      
         OC    DMCB(4),DMCB        VALID DATE                                   
         BNZ   SBLDEQX                                                          
         DC    H'0'                                                             
SBLDEQX  CR    RB,RB                                                            
         B     EXIT                                                             
SBLDMVC  MVC   0(0,RE),0(RF)                                                    
         DROP  R4                                                               
         EJECT                                                                  
* BUILD SORT REC FOR TWX DELIVERY MESSAGES *                                    
         DS    0H                                                               
DBLD     NTR1                                                                   
         L     R2,AIO1                                                          
         LA    R0,2000                                                          
         USING RTABLED,R4                                                       
         SPACE                                                                  
         LA    R3,1                LINE CT                                      
         LA    R2,8(R2)                                                         
         B     DBLD30                                                           
*                                                                               
DBLD14   CLC   0(8,R2),=C'(0D)(25)'                                             
         BE    DBLD20                                                           
         LA    R2,1(,R2)                                                        
         C     R2,AIO2                                                          
         BL    DBLD14                                                           
         CR    RB,RD               SET BYPASS                                   
         B     EXIT                                                             
DBLD20   LA    R2,8(R2)                                                         
         LA    R3,1(R3)                                                         
         SPACE                                                                  
DBLD30   CLM   R3,1,RENT                                                        
         BL    DBLD14                                                           
         BE    DBLD36                                                           
         DC    H'0'                                                             
         SPACE                                                                  
DBLD36   SR    R1,R1                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   R1,1,RLEN                                                        
         ICM   RE,3,RDISP                                                       
         ICM   RF,1,RPOS                                                        
         LA    RE,0(R5,RE)                                                      
         LA    RF,0(R2,RF)                                                      
         SPACE                                                                  
         EX    R1,DBLDMVC                                                       
DBLD40   LA    R4,RNEXT                                                         
         CLI   RENT,255            END                                          
         BL    DBLD30                                                           
         GOTO1 DATVAL,DMCB,(0,DATE),SRUNDT                                      
         OC    DMCB(4),DMCB        VALID DATE                                   
         BNZ   DBLDEQX                                                          
         DC    H'0'                                                             
DBLDEQX  CR    RB,RB                                                            
         B     EXIT                                                             
         SPACE                                                                  
DBLDMVC  MVC   0(0,RE),0(RF)                                                    
         DROP  R4                                                               
         EJECT                                                                  
* PUT DELIVERY REC OUT INTO DUMMY *                                             
         SPACE                                                                  
PUTD     L     R1,VADUMMY                                                       
         LR    R0,R1                                                            
         A     R0,=F'260000'                                                    
PUTD10   OC    0(L'DREC,R1),0(R1)  EMPTY SPOT                                   
         BZ    PUTD20                                                           
         SPACE                                                                  
         CLC   DCRN,DCRN-DREC(RE)                                               
         BNE   PUTD14                                                           
         CLC   DRPT,DRPT-DREC(R1)  ALREADY HERE                                 
         BE    PUTD20              OVERLAY                                      
         SPACE                                                                  
PUTD14   LA    R1,L'DREC(,R1)                                                   
         CR    R1,R0                                                            
         BL    PUTD10                                                           
         DC    H'0'                                                             
PUTD20   MVC   0(L'DREC,R1),DREC                                                
         BR    RE                                                               
         EJECT                                                                  
* FILTER ON AGENCY OF ORIGIN, MEDIA, CLIENT, PRODUCT, DATES *                   
         DS    0H                                                               
FTR      NTR1                                                                   
         SPACE                                                                  
         CLC   SAGYORG(4),AGYORIG     SAME AGENCY                               
         BNE   EXIT                                                             
         SPACE                                                                  
         CLC   SMEDIA,QMED            SAME MEDIA                                
         BNE   EXIT                                                             
         SPACE                                                                  
         CLC   QCLT,SPACES                                                      
         BE    FTR10                                                            
         OC    QCLT,QCLT                                                        
         BZ    FTR10                                                            
         CLC   QCLT,SCLT                                                        
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   BPRD,0                                                           
         BE    FTR10                                                            
         CLC   QPRD,SPRD                                                        
         BNE   EXIT                                                             
         SPACE                                                                  
* SELECT ON EITHER RUN DATE OR SEND DATE *                                      
         SPACE                                                                  
FTR10    CLC   STDATEN,SRUNDT                                                   
         BH    FTR20                                                            
         CLC   ENDATEN,SRUNDT                                                   
         BL    FTR20                                                            
         B     FTREQ                                                            
         SPACE                                                                  
FTR20    GOTO1 DATVAL,DMCB,(0,SENDATE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    FTRNE                                                            
         SPACE                                                                  
         CLC   STDATEN,WORK                                                     
         BH    EXIT                                                             
         CLC   ENDATEN,WORK                                                     
         BL    EXIT                                                             
FTREQ    CR    RB,RB                                                            
         B     EXIT                                                             
FTRNE    LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
         DS    0H                                                               
HDHK     NTR1                                                                   
         SPACE                                                                  
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H3+12(3),SCLT                                                    
         CLI   SPRD+2,C'+'                                                      
         BE    HDHK05                                                           
         MVC   H4+12(3),SPRD                                                    
         B     *+10                                                             
HDHK05   MVC   H4+12(2),SPRD                                                    
         MVC   H3+35(8),STDATE                                                  
         MVC   H3+44(2),=C'TO'                                                  
         MVC   H3+47(8),ENDATE                                                  
         B     EXIT                                                             
         SPACE                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
FUTDATER LA    R1,FUTDATMS                                                      
         B     ERREXIT                                                          
         SPACE                                                                  
MONDATER LA    R1,MONDATMS                                                      
         B     ERREXIT                                                          
         SPACE                                                                  
TWXERR   LA    R1,TWXERRMS                                                      
         B     ERREXIT                                                          
         SPACE                                                                  
DATSQER  LA    R1,DATSEQMS                                                      
ERREXIT  MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
MONDATMS DC    CL60'* ERROR * DATES MUST BE WITHIN THIS MONTH *'                
FUTDATMS DC    CL60'* ERROR * DATES MUST NOT BE IN THE FUTURE *'                
DATSEQMS DC    CL60'* ERROR * DATES MUST BE IN SEQUENCE *'                      
TWXERRMS DC    CL60'* ERROR * CALL DDS TO ENABLE TWX SERVICE *'                 
         SPACE                                                                  
* DCB FOR OFF-LINE PRINT FILE FOR DATA SENT TO GRAPHNET                         
* SPOOL FROM PRTFILE                                                            
         SPACE                                                                  
TWXFILE  DCB   DDNAME=TWXFILE,DSORG=PS,RECFM=FB,BLKSIZE=26600,         X        
               LRECL=133,MACRF=GM,EODAD=ENDTWX                                  
         SPACE 2                                                                
* SORT ON MEDIA/CLIENT/PRODUCT/STATION/RUN DATE/RUN TIME/REPORT ID              
         SPACE                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,36,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=81'                                    
         EJECT                                                                  
* TABLE TO BUILD TABLE OF TWX'S DELIVERY STATUS *                               
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
RDTABLE  DC    AL1(01),AL1(00),AL1(02),AL2(DDELMS-DREC)                         
         DC    AL1(02),AL1(00),AL1(17),AL2(DCRN-DREC)                           
         DC    AL1(02),AL1(06),AL1(07),AL2(DRPT-DREC)                           
         DC    AL1(03),AL1(08),AL1(04),AL2(DTIME-DREC)                          
         DC    AL1(04),AL1(00),AL1(14),AL2(DELNK-DREC)                          
         DC    X'FF'                                                            
         SPACE                                                                  
* TABLE TO BUILD SORT RECS OF SENT TWX'S *                                      
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
RSTABLE  DC    AL1(03),AL1(06),AL1(17),AL2(SCRN-SREC)                           
         DC    AL1(03),AL1(00),AL1(04),AL2(SSTA-SREC)                           
         DC    AL1(03),AL1(12),AL1(07),AL2(SRPT-SREC)                           
         DC    AL1(03),AL1(25),AL1(07),AL2(SAGYORG-SREC)                        
         DC    AL1(03),AL1(33),AL1(00),AL2(SMEDIA-SREC)                         
         DC    AL1(03),AL1(34),AL1(02),AL2(SCLT-SREC)                           
         DC    AL1(03),AL1(37),AL1(02),AL2(SPRD-SREC)                           
         DC    AL1(07),AL1(41),AL1(07),AL2(SRUNDT8-SREC)                        
         DC    AL1(07),AL1(53),AL1(07),AL2(SRUNTM-SREC)                         
         DC    X'FF'                                                            
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,31,C'WESTERN UNION TRANSACTION REPORT'                        
         SSPEC H2,31,C'--------------------------------'                        
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
*                                                                               
         SSPEC H8,3,C'DATE'                                                     
         SSPEC H9,3,C'-----'                                                    
         SSPEC H8,10,C'STATION'                                                 
         SSPEC H9,10,C'-------'                                                 
         SSPEC H8,21,C'CUSTOMER REFERENCE'                                      
         SSPEC H9,21,C'------------------'                                      
         SSPEC H8,42,C'REPORT ID'                                               
         SSPEC H9,42,C'---------'                                               
         SSPEC H8,54,C'EASYLINK'                                                
         SSPEC H9,54,C'--------'                                                
         SSPEC H8,72,C'SENT'                                                    
         SSPEC H9,72,C'-----'                                                   
         SSPEC H8,80,C'RECEIVED'                                                
         SSPEC H9,80,C'--------'                                                
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAAFD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR92RR DS    F                                                                
TIME     DS    CL8                                                              
DATE     DS    CL8                                                              
STDATE   DS    CL8                                                              
ENDATE   DS    CL8                                                              
STDATEN  DS    CL6                                                              
ENDATEN  DS    CL6                                                              
SVCLT    DS    CL3                                                              
SVPROD   DS    CL3                                                              
RECCT    DS    H                   RECS READ FROM DAVID E'S FILE                
SRTCT    DS    H                   SORTED RECS                                  
MISTOTAL DS    H                   MISSING NOTICES                              
DLNTOTAL DS    H                   DELIVERED NOTICES                            
CANTOTAL DS    H                   CANCELLED NOTICES                            
TRNTOTAL DS    H                   TOTAL TRANSACTIONS                           
SVTOTAL  DS    H                   CLIENT TOTAL                                 
PRDTOTAL DS    H                   PRODUCT TOTAL                                
FIRSTSW  DS    CL1                                                              
SVTWPROF DS    CL16                                                             
         SPACE                                                                  
SVTWPR1  EQU   SVTWPROF+0          Y=ALLOW TWX INSTRUCTIONS                     
SVTWPR2  EQU   SVTWPROF+1          O/L GEN REQ C=COPY OF TWX                    
*                                              I=AUTO INSTR OF TWX              
SVTWPR3  EQU   SVTWPROF+2                                                       
         SPACE 3                                                                
* DELIVERY INFO RECS *                                                          
         SPACE                                                                  
DREC     DS    0CL51                                                            
DCRN     DS    CL18                CUSTOMER REFERENCE                           
DRPT     DS    CL8                 REPORT ID                                    
DELNK    DS    CL15                EASY LINK LEDGER NUMBER                      
DDATE    DS    XL2                 DATE DELIVERED                               
DTIME    DS    CL5                 TIME DELIVERED                               
DDELMS   DS    CL3                 DELIVERY MESSAGE/CAN OR DLN                  
         EJECT                                                                  
* TRANSMISSION INFO RECS *                                                      
         SPACE                                                                  
SREC     DS    0CL108                                                           
SRECSRT  DS    0CL81                                                            
SMEDIA   DS    CL1                 MEDIA                                        
SCLT     DS    CL3                 CLIENT                                       
SPRD     DS    CL3                 PRODUCT                                      
SSTA     DS    CL7                 STATION                                      
SRUNDT   DS    CL6                 RUN DATE                                     
SRUNTM   DS    CL8                 RUN TIME                                     
SRPT     DS    CL8                 REPORT ID                                    
SCRN     DS    CL18                CUSTOMER REFERENCE NUMBER                    
SENDATE  DS    CL8                 DELIVERY DATE                                
SENDSTM  DS    CL8                 DELIVERY START TIME                          
SENDETM  DS    CL8                 DELIVERY END TIME                            
SDELMS   DS    CL3                 DELIVERY MESSAGE                             
*                                  DLN = OK, CAN = NOT DELIVERED                
         SPACE                                                                  
* SELNK NOT INCLUDED IN SORT, ONLY BUILT FROM DELIVERY RECORD *                 
         SPACE                                                                  
SELNK    DS    CL15                EASY LINK LEDGER                             
         SPACE                                                                  
* SRUNDT8 NOT INCLUDED IN SORT, BUT REBUILT AFTER SORT *                        
         SPACE                                                                  
SRUNDT8  DS    CL8                 RUN DATE MONDA/YR                            
         SPACE                                                                  
* SAGYORG NOT INCLUDED IN SORT, OR REBUILT AFTER SORT,     *                    
*         ONLY USED AS FILTER WHEN BUILT FROM WESTRUN FILE *                    
         SPACE                                                                  
SAGYORG  DS    CL8                                                              
         EJECT                                                                  
* TABLE TO PULL SENT/DELIVERED INFO *                                           
         SPACE                                                                  
RTABLED  DSECT                                                                  
RENT     DS    XL1                                                              
RPOS     DS    XL1                                                              
RLEN     DS    XL1                                                              
RDISP    DS    XL2                                                              
RNEXT    EQU   *                                                                
         SPACE                                                                  
* GRAPHNET LINE DSECT                                                           
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
         DS    CL1                                                              
GSEND    DS    CL4                                                              
         DS    CL16                                                             
GTIME    DS    CL8                                                              
         DS    CL2                                                              
GDATE    DS    CL8                                                              
         SPACE                                                                  
         ORG   GTIME                                                            
GMSGID   DS    CL12                                                             
GSEQ     DS    CL7                                                              
         DS    CL25                                                             
GAGYID   DS    CL4                                                              
GMED     DS    CL1                                                              
GCLT     DS    CL3                                                              
         DS    CL12                                                             
GRPTID   DS    CL8                                                              
         EJECT                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PDATE    DS    CL5                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL4                                                              
PCRN     DS    CL18                                                             
         DS    CL3                                                              
PREPTID  DS    CL8                                                              
         DS    CL4                                                              
PELNK    DS    CL15                                                             
         DS    CL3                                                              
PSENT    DS    CL5                                                              
         DS    CL3                                                              
PDELIV   DS    CL5                                                              
         DS    CL3                                                              
PCAN     DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPTRA92   05/01/02'                                      
         END                                                                    
