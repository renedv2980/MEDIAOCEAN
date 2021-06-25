*          DATA SET SPTRA09    AT LEVEL 137 AS OF 05/01/02                      
*          DATA SET SPTRA09    AT LEVEL 010 AS OF 03/26/90                      
*PHASE T21609A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'T21609 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
***********************************************************************         
*        TITLE 'T21609 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
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
*        DUMMY FOR 100,000 USED FOR DELIVERY NOTICE SORT AREA                   
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
* LEV 136 BGRI DUMPED KHDUMMY                                                   
***********************************************************************         
         EJECT                                                                  
         TITLE 'T21609 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
T21609   CSECT                                                                  
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
         ST    R2,SPTR09RR                                                      
         SPACE 3                                                                
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                                                              
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY ROUTINE *                                                        
***********************************************************************         
VK       DS    0H                  DO NOTHING                                   
         MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OFFLINE REPORT ROUTINE *                                                      
***********************************************************************         
         SPACE                                                                  
LRR      XC    RECCT,RECCT         ZERO RECORD CT                               
         XC    SRTCT,SRTCT         ZERO SORTED RECORD CT                        
         XC    SVPROD,SVPROD                                                    
         XC    SVCLT,SVCLT                                                      
         XC    SVRPT,SVRPT                                                      
         XC    SVAGYID,SVAGYID                                                  
         XC    SVTOTAL,SVTOTAL     CLIENT TOTAL                                 
         XC    PRDTOTAL,PRDTOTAL   PRODUCT TOTAL                                
         XC    CANTOTAL,CANTOTAL   CANCELLED TOTAL                              
         XC    DLNTOTAL,DLNTOTAL   DELIVERED TOTAL                              
         XC    MISTOTAL,MISTOTAL   MISSING TOTAL                                
         XC    TRNTOTAL,TRNTOTAL   TOTAL TRANSACTIONS                           
         XC    BLOCK(133),BLOCK    SET NO REC READ YET                          
         MVI   FIRSTSW,0                                                        
         SPACE                                                                  
         LA    R0,400              CLEAR 100,000 BYTES                          
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
*                                                                               
*                                                                               
         BAS   RE,GTW                                                           
         GOTO1 SPOOL,DMCB,(R8)     PRINT NOTHING                                
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
         GOTO1 XSORT,DMCB,VADUMMY,(R0),L'DREC,18,0                              
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
*                                                                               
* MOVE SORT REC TO SREC SO CAN SEE IT IN DUMPS *                                
*                                                                               
         MVC   SREC,0(R6)                                                       
         MVI   FIRSTSW,1                                                        
         CLC   SVAGYID,SAGYIDN                                                  
         BE    LRR13                                                            
*                                                                               
         BAS   RE,GETHEAD          GET HEADER ROUTINE                           
*                                                                               
LRR13    OC    SVRPT,SVRPT         FIRST TIME AROUND                            
         BNZ   LRR15                                                            
         MVC   SVRPT,SRPT                                                       
         MVC   SVCLT,SCLT                                                       
         MVC   SVPROD,SPRD                                                      
         B     LRR18                                                            
*                                                                               
LRR15    DS    0H                                                               
         CLC   SVRPT,SRPT          SAME REPORT TYPE                             
         BNE   LRR16               YES -CHECK CLIENT                            
         CLC   SVCLT,SCLT          SAME CLIENT                                  
         BE    LRR17               YES -CHECK PRODUCT                           
LRR16    LH    R3,CANTOTAL                                                      
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
         MVC   SVRPT,SRPT          MOVE IN NEW REPORT TYPE                      
         MVC   SVCLT,SCLT          MOVE IN NEW CLIENT                           
         MVC   SVPROD,SPRD         AND PRODUCT                                  
         MVI   FORCEHED,C'Y'                                                    
         B     LRR18                                                            
*                                                                               
LRR17    CLC   SRPT,=C'TWX'                                                     
         BE    LRR17A                                                           
         CLC   SVPROD,SPRD         SAME NETWORK                                 
         BNE   LRR17B                                                           
LRR17A   CLC   SVPROD(3),SPRD      SAME PRODUCT                                 
         BE    LRR18                                                            
LRR17B   LH    R3,DLNTOTAL                                                      
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
LRR20    OC    0(L'DREC,RE),0(RE)                                               
         BZ    LRR24               NO DELIVERY MESSAGE FOUND                    
         CLC   SCRN,DCRN-DREC(RE)                                               
         BNE   LRR22                                                            
         CLC   SCRN+6(8),DRPT-DREC(RE)                                          
         BE    LRR26               DELIVERY/CAN FOUND                           
LRR22    LA    RE,L'DREC(,RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   LRR20                                                            
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
LRR30    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SRUNDT),(8,SRUNDT8)                               
         MVC   PDATE,SRUNDT8       MOVE IN DATE                                 
*                                  FOR TESTING                                  
*        EDIT  (B2,SAGYIDN),(5,PDATE)                                           
*                                                                               
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
*        DC    H'0'                INVALID MEDIA                                
*                                                                               
LRR32    MVC   PCRN,SCRN           MOVE IN CUSTOMER REFERENCE                   
         XC    WORK(10),WORK                                                    
         EDIT  (C5,SCRN+9),(4,WORK+4),ALIGN=LEFT                                
         MVC   WORK(3),SRPT                                                     
         MVC   WORK+3(1),=C','                                                  
         MVC   PREPTID,WORK        MOVE IN REPORT ID                            
         SPACE                                                                  
         OC    SELNK,SELNK                                                      
         BZ    LRR40                                                            
         CLC   =C'TWX',SRPT                                                     
         BE    LRR35                                                            
         MVC   PELNK(11),SELNK     MOVE IN EASYLINK LEDGER                      
         B     LRR40                                                            
LRR35    MVC   PELNK,SELNK         MOVE IN EASYLINK LEDGER                      
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
         CLC   SVRPT,=C'TWX'                                                    
         BE    PRNT35                                                           
         MVC   P+30(25),=CL25'NETWORK TOTAL - '                                 
         B     PRNT38                                                           
PRNT35   MVC   P+30(25),=CL25'PRODUCT TOTAL - '                                 
PRNT38   EDIT  (2,PRDTOTAL),(5,P+55)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRNT40   DS    0H                                                               
         XC    CANTOTAL,CANTOTAL                                                
         XC    DLNTOTAL,DLNTOTAL                                                
         XC    MISTOTAL,MISTOTAL                                                
         XC    PRDTOTAL,PRDTOTAL                                                
         BR    R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET GRAPHNET TRANS RECS AND BUILD 1 TRANS REC *                               
***********************************************************************         
         DS    0H                                                               
GTW      NTR1                                                                   
         SPACE                                                                  
GTW10    BAS   RE,GETWX                                                         
*                                                                               
GTW20    CLC   =C'O  BUFFER',BLOCK+1  SENT MESSAGE                              
         BNE   GTW30                                                            
         SPACE                                                                  
         CLC   GMSGID(12),=C'(10)(02)(01)'                                      
         BNE   GTW10                                                            
         BAS   RE,BMS              GO BUILD MESSAGE IN AIO1                     
*                                                                               
         BAS   RE,GETABLE          GET APPROPRIATE REPORT TYPE TABLE            
         BNE   GTW40               INVALID REPORT TYPE, DON'T PUT               
*                                    IN SORT                                    
         ZIC   R4,REPTADD,3        SCAN FOR TWX FOR NOW                         
         LA    R5,SREC                                                          
         BAS   RE,SBLD             BUILD SORT REC                               
         B     GTW20                                                            
         SPACE                                                                  
GTW30    CLC   =C'MARKREPORTSENT',BLOCK+1                                       
         BNE   GTW40                                                            
*                                                                               
         CLC   REPTYPE,=C'ERR'                                                  
         BE    GTW40                                                            
*                                                                               
         MVC   SENDSTM,TIME        MOVE SENT TIME AND                           
         MVC   SENDATE,DATE        DATE INTO RECORD                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LH    R1,SRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,SRTCT                                                         
*                                                                               
GTW26    XC    SREC,SREC                                                        
         B     GTW10                                                            
*                                                                               
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
*                                                                               
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
GTW50    CLI   BLOCK+1,X'FF'                                                    
         BNE   GTW10                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ TWX TRANSMISSION FILE RECS *                                             
***********************************************************************         
         DS    0H                                                               
GETWX    NTR1                                                                   
*                                                                               
GETWX10  GET   TWXFILE,BLOCK                                                    
         LH    R1,RECCT                                                         
         LA    R1,1(R1)                                                         
         STH   R1,RECCT                                                         
*                                                                               
         CLC   BLOCK+1(132),SPACES  BLANK LINE OR HEADER                        
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
GETWX20  DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* END OF FILE *                                                                 
***********************************************************************         
ENDTWX   MVI   BLOCK,X'FF'                                                      
         MVC   BLOCK+1(132),BLOCK                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD MESSAGE AS SENT/RECEIVED TO/FROM WESTERN UNION *                        
***********************************************************************         
         DS    0H                                                               
BMS      NTR1                                                                   
         L     R4,AIO1                                                          
         LA    R0,8                                                             
         LR    R1,R4                                                            
BMS05    XC    0(250,R1),0(R1)     CLEAR 2000 BYTES                             
         LA    R1,250(,R1)                                                      
         BCT   R0,BMS05                                                         
         SPACE                                                                  
         LR    R6,R4                                                            
BMS10    MVC   0(81,R6),GMSGID                                                  
         LA    R6,81(R6)                                                        
         LA    RF,GMSGID+81                                                     
BMS20    CLI   0(RF),C' '          MOVE ANY OVER 81 CHARACTERS                  
         BNH   BMS30                                                            
         MVC   0(1,R6),0(RF)                                                    
         LA    R6,1(,R6)                                                        
         LA    RF,1(,RF)                                                        
         B     BMS20                                                            
         SPACE                                                                  
BMS30    C     R6,AIO2             CHECK IF AIO2 OVERWRITTEN                    
         BL    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETWX            GET NEXT REC                                 
         CLI   BLOCK+1,C' '                                                     
         BNH   BMS10                                                            
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD SORT REC FOR MESSAGE SENT *                                             
***********************************************************************         
         DS    0H                                                               
SBLD     NTR1                                                                   
         CLC   REPTYPE,=C'ERR'                                                  
         BE    SBLD16                                                           
         L     R2,AIO1                                                          
         LA    R0,2000                                                          
         USING RTABLED,R4                                                       
         SPACE                                                                  
         LA    R3,1                LINE CT                                      
         LA    R2,12(R2)           BUMP PASS (10)(02)(01)                       
         B     SBLD30                                                           
*                                                                               
SBLD14   CLC   0(8,R2),=C'(0D)(25)'                                             
         BE    SBLD20                                                           
         LA    R2,1(,R2)                                                        
         C     R2,AIO2                                                          
         BL    SBLD14                                                           
SBLD16   CR    RB,RD               SET BYPASS                                   
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
         CLI   RENT,X'FF'          END                                          
         BL    SBLD30                                                           
*                                                                               
         BAS   RE,GETID            GET ID NUMBER                                
*                                                                               
SBLD60   GOTO1 DATVAL,DMCB,(0,DATE),SRUNDT                                      
         OC    DMCB(4),DMCB        VALID DATE                                   
         BNZ   SBLDEQX                                                          
         DC    H'0'                                                             
SBLDEQX  CR    RB,RB                                                            
         B     EXIT                                                             
SBLDMVC  MVC   0(0,RE),0(RF)                                                    
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ASSIGN AGENCY ID NUM *                                                        
***********************************************************************         
         DS    0H                                                               
GETID    NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIKEY,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKID(4),SAGYID                                                 
         OC    CTIKID,SPACES       BLANK PADDED                                 
         DROP  R6                                                               
*                                  GET AGENCY ID NUM                            
GETID05  GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO              KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         USING CTIKEY,R6           IT ISN'T A GENCON READ HIGH                  
*                                                                               
         CLC   CTIKEY,KEY                                                       
         BE    GETID10                                                          
*                                                                               
         MVC   SAGYIDN,=X'FFFF'    DID NOT FIND AN AGENCY ID NUM                
         B     GETIDX                                                           
GETID10  MVI   ELCODE,X'02'        GET ID NUM                                   
         BAS   RE,GETEL                                                         
         BE    GETID20                                                          
         DC    H'0'                MUST BE THERE                                
GETID20  MVC   SAGYIDN,2(R6)                                                    
*                                                                               
GETIDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ASSIGN AGENCY ID INFO *                                                       
***********************************************************************         
         DS    0H                                                               
GETINFO  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIKEY,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKNUM,SAGYIDN                                                  
         DROP  R6                                                               
*                                                                               
GETINF10 GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO              KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         USING CTIKEY,R6           IT ISN'T A GENCON READ HIGH                  
*                                                                               
         CLC   CTIKEY,KEY                                                       
         BE    GETINF20                                                         
*                                                                               
         XC    SAGYINFO,SPACES                                                  
         B     GETINFX                                                          
GETINF20 MVI   ELCODE,X'36'        GET ID NAME AND ADDRESS                      
         BAS   RE,GETEL                                                         
         BE    GETINF30                                                         
         XC    SAGYINFO,SPACES                                                  
         B     GETINFX                                                          
*                                                                               
GETINF30 MVC   SAGYINFO,2(R6)                                                   
         DROP  R6                                                               
*                                                                               
GETINFX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ASSIGN APPROPRIATE REPORT TYPE TABLE *                                        
***********************************************************************         
         DS    0H                                                               
GETABLE  NTR1                                                                   
         L     R2,AIO1                                                          
         LA    R0,2000                                                          
         SPACE                                                                  
         LA    R2,12(R2)           BUMP PASS (10)(02)(01)                       
         SR    R3,R3                                                            
*                                                                               
GETAB10  CLC   0(8,R2),=C'(0D)(25)'  BUMP PASS 2 (0D)(25)'S                     
         BE    GETAB20                                                          
         LA    R2,1(R2)                                                         
         C     R2,AIO2                                                          
         BL    GETAB10                                                          
         CR    RB,RD               SET BYPASS                                   
         B     GETABX                                                           
*                                                                               
GETAB20  LA    R2,8(R2)                                                         
         LA    R3,1(R3)                                                         
         CH    R3,=H'2'                                                         
         BL    GETAB10                                                          
*                                                                               
GETAB30  CLI   0(R2),X'5E'         BUMP PASS SEMICOLON                          
         BE    GETAB40                                                          
         LA    R2,1(R2)                                                         
         C     R2,AIO2                                                          
         BL    GETAB30                                                          
         CR    RB,RD               SET BYPASS                                   
         B     GETABX                                                           
*                                                                               
GETAB40  LA    R2,1(R2)            GET CUSTOMER REF NUM                         
         LA    R3,REPTABLE                                                      
*                                                                               
GETAB45  CLC   0(3,R3),6(R2)       FIND MATCHING REPORT TYPE                    
         BE    GETAB50                                                          
         LA    R3,L'REPTABLE(R3)   BUMP TO NEXT ROW                             
         CLI   0(R3),X'FF'                                                      
         BNE   GETAB45                                                          
         MVC   REPTYPE,=C'ERR'                                                  
         CR    RB,RD               SET BYPASS                                   
         B     GETABX                                                           
*                                                                               
GETAB50  DS    0H                                                               
         MVC   REPTYPE,0(R3)       GET REPORT TYPE                              
         MVC   REPTADD,3(R3)       GET REPORT TYPE TABLE ADDRESS                
         MVC   REPHEAD,6(R3)       GET REPORT TYPE HEADING LINE ADD             
         MVC   REPHDHK,9(R3)       GET REPORT TYPE HEADING ROUTINE ADD          
         CR    RB,RB               SET OK                                       
*                                                                               
GETABX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET HEADER LINE AND HEADER ROUTINE FOR A REPORT TYPE *                        
***********************************************************************         
         DS    0H                                                               
GETHEAD  NTR1                                                                   
         LA    R3,REPTABLE                                                      
*                                                                               
GETHEAD3 CLC   SRPT,0(R3)          FIND MATCHING REPORT TYPE                    
         BE    GETHEAD5                                                         
         LA    R3,L'REPTABLE(R3)   BUMP TO NEXT ROW                             
         CLI   0(R3),X'FF'                                                      
         BNE   GETHEAD3                                                         
         B     GETHEADX                                                         
*                                                                               
GETHEAD5 DS    0H                                                               
         MVC   REPTYPE,0(R3)       GET REPORT TYPE                              
         MVC   REPTADD,3(R3)       GET REPORT TYPE TABLE ADDRESS                
         MVC   REPHEAD,6(R3)       GET REPORT TYPE HEADING LINE ADD             
         MVC   REPHDHK,9(R3)       GET REPORT TYPE HEADING ROUTINE ADD          
         MVC   SVAGYID,SAGYIDN                                                  
*                                                                               
         ZIC   R1,REPHEAD,3        HEADING LINE FOR REPORT                      
         A     R1,SPTR09RR                                                      
         ST    R1,SPECS                                                         
         ZIC   R1,REPHDHK,3        HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK                                                      
*                                                                               
GETHEADX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD SORT REC FOR TWX DELIVERY MESSAGES *                                    
***********************************************************************         
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
         CLI   RENT,X'FF'          END                                          
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
***********************************************************************         
* PUT DELIVERY REC OUT INTO DUMMY *                                             
***********************************************************************         
PUTD     L     R1,VADUMMY                                                       
         LR    R0,R1                                                            
         A     R0,=F'100000'                                                    
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
***********************************************************************         
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
***********************************************************************         
         DS    0H                                                               
TWXHDHK  NTR1                      TWX REPORT                                   
         SPACE                                                                  
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         MVC   H3+12(3),SCLT                                                    
         CLI   SPRD+2,C'+'                                                      
         BE    TWXHDHK5                                                         
         MVC   H4+12(3),SPRD                                                    
         B     *+10                                                             
TWXHDHK5 MVC   H4+12(2),SPRD                                                    
         GOTO1 DATCON,DMCB,(5,WORK),(8,H3+40)                                   
*        MVC   H3+33(8),STDATE                                                  
*        MVC   H3+43(2),=C'TO'                                                  
*        MVC   H3+47(8),ENDATE                                                  
         B     EXIT                                                             
*                                                                               
         DS    0H                                                               
CWXHDHK  NTR1                      CWX REPORT                                   
         SPACE                                                                  
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         MVC   H3+12(3),SCLT                                                    
         CLI   SPRD+3,C'+'                                                      
         BE    CWXHDHK5                                                         
         MVC   H4+12(4),SPRD       PRINTS NETWORK INSTEAD                       
         B     *+10                                                             
CWXHDHK5 MVC   H4+12(3),SPRD                                                    
         GOTO1 DATCON,DMCB,(5,WORK),(8,H3+40)                                   
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
DATSEQER LA    R1,DATSEQMS                                                      
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
         GETEL R6,DATADISP,ELCODE        USED FOR GETEL OPERATIONS              
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
***********************************************************************         
* TABLE OF REPORT TYPES AND SYSTEMS                                             
*                                                                               
REPTABLE DS    0CL12                                                            
*              REP TYP TABLE ADDRESS  HEAD LINE   HEAD ROUT                     
*                                                                               
         DC    C'CWX',AL3(CWXTABLE),AL3(CWXHEAD),AL3(CWXHDHK)                   
         DC    C'NWX',AL3(CWXTABLE),AL3(CWXHEAD),AL3(CWXHDHK)                   
         DC    C'TWX',AL3(TWXTABLE),AL3(TWXHEAD),AL3(TWXHDHK)                   
         DC    X'FF'                                                            
         DC    C'QEZ',C'ADDS ',AL3(Q2QTABLE)                                    
         DC    C'PIO',C'PRINT',AL3(PIOTABLE)                                    
         DC    C'Q2Q',C'ADDS ',AL3(Q2QTABLE)                                    
         DC    X'FF'                                                            
***********************************************************************         
* TABLE TO BUILD TABLE OF DELIVERY STATUS *                                     
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
RDTABLE  DC    AL1(01),AL1(00),AL1(02),AL2(DDELMS-DREC)                         
         DC    AL1(02),AL1(00),AL1(17),AL2(DCRN-DREC)                           
         DC    AL1(02),AL1(06),AL1(07),AL2(DRPT-DREC)                           
         DC    AL1(03),AL1(08),AL1(04),AL2(DTIME-DREC)                          
         DC    AL1(04),AL1(00),AL1(14),AL2(DELNK-DREC)                          
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* TABLE TO BUILD SORT RECS OF SENT TWX'S *                                      
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
TWXTABLE DC    AL1(03),AL1(00),AL1(04),AL2(SSTA-SREC)                           
         DC    AL1(03),AL1(06),AL1(17),AL2(SCRN-SREC)                           
         DC    AL1(03),AL1(12),AL1(02),AL2(SRPT-SREC)                           
         DC    AL1(03),AL1(25),AL1(03),AL2(SAGYID-SREC)                         
         DC    AL1(03),AL1(29),AL1(00),AL2(SMEDIA-SREC)                         
         DC    AL1(03),AL1(30),AL1(02),AL2(SCLT-SREC)                           
         DC    AL1(03),AL1(33),AL1(02),AL2(SPRD-SREC)                           
         DC    AL1(07),AL1(41),AL1(07),AL2(SRUNDT8-SREC)                        
         DC    AL1(07),AL1(53),AL1(07),AL2(SRUNTM-SREC)                         
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* TABLE TO BUILD SORT RECS OF SENT CWX'S *                                      
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
CWXTABLE DC    AL1(03),AL1(18),AL1(17),AL2(SCRN-SREC)                           
         DC    AL1(03),AL1(24),AL1(02),AL2(SRPT-SREC)                           
         DC    AL1(03),AL1(37),AL1(03),AL2(SAGYID-SREC)                         
         DC    AL1(03),AL1(41),AL1(00),AL2(SMEDIA-SREC)                         
         DC    AL1(03),AL1(42),AL1(02),AL2(SCLT-SREC)                           
         DC    AL1(03),AL1(45),AL1(03),AL2(SPRD-SREC)                           
         DC    AL1(06),AL1(91),AL1(07),AL2(SRUNDT8-SREC)                        
         DC    AL1(06),AL1(103),AL1(07),AL2(SRUNTM-SREC)                        
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* TABLE TO BUILD SORT RECS OF SENT PIO'S *                                      
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
PIOTABLE DC    AL1(03),AL1(06),AL1(17),AL2(SCRN-SREC)                           
         DC    AL1(03),AL1(00),AL1(04),AL2(SSTA-SREC)                           
         DC    AL1(03),AL1(12),AL1(02),AL2(SRPT-SREC)                           
         DC    AL1(03),AL1(29),AL1(00),AL2(SMEDIA-SREC)                         
         DC    AL1(03),AL1(30),AL1(02),AL2(SCLT-SREC)                           
         DC    AL1(03),AL1(33),AL1(02),AL2(SPRD-SREC)                           
         DC    AL1(07),AL1(41),AL1(07),AL2(SRUNDT8-SREC)                        
         DC    AL1(07),AL1(53),AL1(07),AL2(SRUNTM-SREC)                         
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* TABLE TO BUILD SORT RECS OF SENT Q2Q'S/EAS'S*                                 
         SPACE                                                                  
*                ENTRY    POS    LEN-1     TO FIELD                             
Q2QTABLE DC    AL1(03),AL1(06),AL1(17),AL2(SCRN-SREC)                           
         DC    AL1(03),AL1(00),AL1(04),AL2(SSTA-SREC)                           
         DC    AL1(03),AL1(12),AL1(02),AL2(SRPT-SREC)                           
         DC    AL1(03),AL1(29),AL1(00),AL2(SMEDIA-SREC)                         
         DC    AL1(03),AL1(30),AL1(02),AL2(SCLT-SREC)                           
         DC    AL1(03),AL1(33),AL1(02),AL2(SPRD-SREC)                           
         DC    AL1(07),AL1(41),AL1(07),AL2(SRUNDT8-SREC)                        
         DC    AL1(07),AL1(53),AL1(07),AL2(SRUNTM-SREC)                         
         DC    X'FF'                                                            
         SPACE                                                                  
         EJECT                                                                  
TWXHEAD  SSPEC H1,3,C'SPOT TRAFFIC SYSTEM'                                      
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,33,C'EASYLINK TRANSACTION REPORT'                             
         SSPEC H2,33,C'---------------------------'                             
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
CWXHEAD  SSPEC H1,3,C'NET TRAFFIC SYSTEM'                                       
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'NETWORK'                                                  
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,33,C'EASYLINK TRANSACTION REPORT'                             
         SSPEC H2,33,C'---------------------------'                             
         SSPEC H4,73,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
*                                                                               
         SSPEC H8,3,C'DATE'                                                     
         SSPEC H9,3,C'-----'                                                    
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
*        PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAE9D                                                       
*        PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR09RR DS    F                                                                
TIME     DS    CL8                                                              
DATE     DS    CL8                                                              
STDATE   DS    CL8                                                              
ENDATE   DS    CL8                                                              
STDATEN  DS    CL6                                                              
ENDATEN  DS    CL6                                                              
SVAGYID  DS    XL2                                                              
SVRPT    DS    CL3                                                              
SVCLT    DS    CL3                                                              
SVPROD   DS    CL3                                                              
SAGYINFO DS    CL66                AGENCY FULL NAME AND ADDRESS                 
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
SREC     DS    0CL106                                                           
SRECSRT  DS    0CL79                                                            
SAGYIDN  DS    XL2                 AGENCY ID NUMBER                             
SRPT     DS    CL3                 REPORT ID                                    
SMEDIA   DS    CL1                 MEDIA                                        
SCLT     DS    CL3                 CLIENT                                       
SPRD     DS    CL4                 TWX PRODUCT, CWX/NWX NETWORK                 
SSTA     DS    CL7                 TWX STATION                                  
SRUNDT   DS    CL6                 RUN DATE                                     
SRUNTM   DS    CL8                 RUN TIME                                     
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
*                                                                               
* SRUNDT8 NOT INCLUDED IN SORT, BUT REBUILT AFTER SORT *                        
*                                                                               
SRUNDT8  DS    CL8                 RUN DATE MONDA/YR                            
* SAGYID NOT INCLUDED IN SORT, USES THIS TO GET ID NUM FOR SAGYIDN              
*                                                                               
SAGYID   DS    CL4                 AGENCY ID                                    
         EJECT                                                                  
* TABLE TO PULL APPROPRIATE REPORT TYPE INFO *                                  
REPTYPE  DS    CL3                 REPORT TYPE                                  
REPTADD  DS    AL3                 REPORT TYPE TABLE ADDRESS                    
REPHEAD  DS    AL3                 REPORT TYPE HEADING                          
REPHDHK  DS    AL3                 REPORT TYPE HEAD HOOK                        
*                                                                               
* TABLE TO PULL SENT/DELIVERED INFO *                                           
*                                                                               
RTABLED  DSECT                                                                  
RENT     DS    XL1                                                              
RPOS     DS    XL1                                                              
RLEN     DS    XL1                                                              
RDISP    DS    XL2                                                              
RNEXT    EQU   *                                                                
*                                                                               
* TABLE TO PULL REPORT SYSTEM INFO *                                            
*                                                                               
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
**PAN#1  DC    CL21'137SPTRA09   05/01/02'                                      
         END                                                                    
