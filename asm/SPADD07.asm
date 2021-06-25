*          DATA SET SPADD07    AT LEVEL 006 AS OF 03/25/15                      
*PHASE T21207A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'T21207 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
***********************************************************************         
*        TITLE 'T21207 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPADD00-T21200)                   
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
*        R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER                              
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
*        VALI ROUTINES ARE IN BASE (SPADD00-T21200)                             
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
* THIS REPORT READS THE SYSPRINT FILE PUT OUT BY JIM ZIEGLERS GRAFNET           
* TRANSMISSION REPORT AND SPECIFICALLY USES                                     
*                                                                               
* O  BUFFER (10)(02)(01) RECS FOR PRINT QUE LIST AND INSTR HEADER INFO          
* I  BUFFER (10)(02)     FOR DELIVERED OK OR ERR                                
* MARKREPORTSENT         FOR CURRENT DATE/TIME                                  
*                                                                               
* REPORT TYPES SUPPORTED BY THIS TRANSACTION PROGRAM:                           
* CWX, NWX, PIO, Q2Q, QEZ, QXX, TWX                                             
*                                                                               
***********************************************************************         
*                                                                               
* LEV 17    NOV22/91 MAKE DLN ENTRIES SHORTER AND AREA LONGER                   
*                                                                               
***********************************************************************         
         EJECT                                                                  
         TITLE 'T21207 TRANSACTION REPORT/DAILY EXCEPTION REPORT'               
T21207   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ADDS**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING MYAREAD,R5                                                       
         ST    R2,RELO                                                          
*                                                                               
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
VK       DS    0H                                                               
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK10                                                             
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
*                                                                               
VK20     LA    R2,TRAPERH          PERIOD                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK40                NO                                           
         BAS   RE,VPER                                                          
         B     VK50                                                             
*                                                                               
VK40     GOTO1 DATCON,DMCB,(4,RCDATE),(5,STDATE)                                
         MVC   ENDATE,STDATE                                                    
         MVC   8(8,R2),STDATE                                                   
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
VK50     GOTO1 DATVAL,DMCB,(0,STDATE),STDATEN                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(0,ENDATE),ENDATEN                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VK60     XC    FREPTYP,FREPTYP                                                  
         OC    TRATYP,TRATYP                                                    
         BZ    VKX                                                              
*                                                                               
         LA    R2,TRATYPH                                                       
         LA    R1,FREPTYP          BUILD REPORT TYPE FILTER                     
         ZIC   RF,5(R2)                                                         
         LA    RF,0(RF,R1)                                                      
         LA    RE,8(R2)                                                         
VK70     MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,4(RE)                                                         
         CR    R1,RF                                                            
         BL    VK70                                                             
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OFFLINE REPORT ROUTINE *                                                      
***********************************************************************         
LRR      XC    RECCT,RECCT         ZERO RECORD CT                               
         XC    SRTCT,SRTCT         ZERO SORTED RECORD CT                        
         XC    SVMEDIA,SVMEDIA                                                  
         XC    SVNET,SVNET                                                      
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
*                                                                               
         LA    R0,600              CLEAR 150,000 BYTES                          
         L     R1,VADUMMY                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(,R1)                                                      
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,TWXFILE                                                       
         OPEN  ((R2),(INPUT))                                                   
*                                                                               
         LTR   RF,RF               OPEN OK                                      
         BZ    LRR05                                                            
         ABEND 991                                                              
*                                                                               
LRR05    GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
* READ TWX TRANSMISSION FILE, BUILD SORT FILE *                                 
*                                                                               
         BAS   RE,GTW                                                           
*                                                                               
* SORT DELIVERY RECS *                                                          
*                                                                               
         SR    R0,R0                                                            
         L     RE,VADUMMY                                                       
*                                                                               
LRR10    OC    0(L'DREC,RE),0(RE)                                               
         BZ    LRR15                                                            
         LA    RE,L'DREC(,RE)                                                   
         BCT   R0,LRR10                                                         
LRR15    LPR   R0,R0                                                            
         LTR   R0,R0                                                            
         BZ    LRR20                                                            
         GOTO1 XSORT,DMCB,VADUMMY,(R0),L'DREC,18,0                              
*                                                                               
LRR20    XC    SREC,SREC                                                        
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   LRR23                                                            
         LH    R3,CANTOTAL        PRODUCT TOTAL ONLY UPDATED                    
         AH    R3,DLNTOTAL        WHEN BREAK IN TOTAL                           
         AH    R3,MISTOTAL        SO IF AT END MUST ADD TO GET TOTAL            
         STH   R3,PRDTOTAL                                                      
         B     LRREND                                                           
LRR23    LH    R3,TRNTOTAL         TOTAL NUMBER OF TRANSACTIONS                 
         LA    R3,1(R3)                                                         
         STH   R3,TRNTOTAL                                                      
*                                                                               
* MOVE SORT REC TO SREC SO CAN SEE IT IN DUMPS *                                
*                                                                               
         MVC   SREC,0(R6)                                                       
         MVI   FIRSTSW,1                                                        
*                                                                               
LRR25    OC    SVAGYID,SVAGYID     FIRST TIME AROUND                            
         BNZ   LRR28                                                            
         MVC   SVAGYID,SAGYIDN                                                  
         MVC   SVMEDIA,SMEDIA                                                   
         MVC   SVRPT,SRPT                                                       
         MVC   SVCLT,SCLT                                                       
*        MVC   SVNET,SNET                                                       
         MVC   SVPROD,SPRD                                                      
         B     LRR35                                                            
*                                                                               
LRR28    DS    0H                                                               
         CLC   SVAGYID,SAGYIDN     SAME AGENCY DESTINATION ?                    
         BNE   LRR35                                                            
*                                                                               
         CLC   SVRPT,SRPT          SAME REPORT TYPE ?                           
         BE    LRR33                                                            
         CLC   =C'Q2Q',SRPT        FOR Q2Q/QXX/QEZ, NO PAGE BREAKS              
         BE    LRR30               EXCEPT FOR DIFFERENT AGENCY ID               
         CLC   =C'QXX',SRPT                                                     
         BE    LRR30                                                            
         CLC   =C'QEZ',SRPT                                                     
         BNE   LRR35                                                            
*                                                                               
LRR30    CLC   =C'Q2Q',SVRPT                                                    
         BE    LRR50                                                            
         CLC   =C'QXX',SVRPT                                                    
         BE    LRR50                                                            
         CLC   =C'QEZ',SVRPT                                                    
         BE    LRR50                                                            
         B     LRR35                                                            
*                                                                               
LRR33    CLC   SVCLT,SCLT          SAME CLIENT ?                                
         BE    LRR43               YES -CHECK PRODUCT                           
         B     LRR38               KEEP SAME QUEUE                              
LRR35    DS    0H                                                               
         CLI   PQSW,1              FIRST TIME, NO TOTALS                        
         BE    LRR40                                                            
*                                                                               
         MVI   NEWQUE,C'Y'         NEW QUEUE AFTER PRINTING TOTAL               
LRR38    DS    0H                                                               
         LH    R3,CANTOTAL                                                      
         AH    R3,DLNTOTAL                                                      
         AH    R3,MISTOTAL                                                      
         STH   R3,PRDTOTAL                                                      
         BAS   RE,PRNTOT           NO                                           
*                                                                               
         CLC   =C'QXX',SVRPT       NO CLIENT TOTAL FOR ADDS                     
         BE    LRR39                                                            
         CLC   =C'Q2Q',SVRPT                                                    
         BE    LRR39                                                            
         CLC   =C'QEZ',SVRPT                                                    
         BE    LRR39                                                            
*                                                                               
         MVC   P+30(25),=CL25'CLIENT TOTAL - '                                  
         LH    R3,TRNTOTAL                                                      
         BCTR  R3,0                THIS REC DOESN'T BELONG TO THIS CLT          
         STH   R3,TRNTOTAL                                                      
         EDIT  (2,TRNTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
*                                                                               
LRR39    DS    0H                                                               
         LA    R3,1                                                             
         STH   R3,TRNTOTAL         RESET TRANTOTAL                              
         MVC   SVAGYID,SAGYIDN     MOVE IN NEW AGENCY ID NUM                    
         MVC   SVMEDIA,SMEDIA      MOVE IN NEW MEDIA                            
         MVC   SVRPT,SRPT          MOVE IN NEW REPORT TYPE                      
         MVC   SVCLT,SCLT          MOVE IN NEW CLIENT                           
*        MVC   SVNET,SNET          MOVE IN NEW NETWORK                          
         MVC   SVPROD,SPRD         MOVE IN NEW PROD                             
         MVI   FORCEHED,C'Y'                                                    
         CLI   NEWQUE,C'N'         DON'T START NEW QUEUE                        
         BE    LRR50               CHECK PRODUCT                                
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BAS   RE,PRTLINE                                                       
*                                                                               
LRR40    DS    0H                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK         EXTENDED SPOOLKEY (128 BYTES)                
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVC   PLDESC,=CL11'XACT REP'                                           
         OI    GENSTAT3,NOCLRSPK   GENCON DON'T CLEAR SPOOLKEY                  
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'12'                                                   
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,SPUINIT    USER VALUES PRESENT                          
         DROP  R1                                                               
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'A'                                                    
         BAS   RE,GETRTYPE         GET REPORT TYPE AND ID                       
         MVC   REMOTJID,REPJID                                                  
         MVC   REMOTSYS,REPSYSN                                                 
         MVC   REMOTDST,SAGYIDN                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
         MVI   PQSW,2              SO I WON'T REOPEN                            
         MVI   NEWQUE,C'N'                                                      
*                                                                               
         L     R1,=A(HEADING)      HEADING LINE FOR REPORT                      
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         ZIC   R1,REPHDHK,3        HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK                                                      
         B     LRR50                                                            
*                                                                               
LRR43    CLC   =C'TWX',SRPT        TWX/PIO HAVE PRODUCT FIELD INSTEAD           
         BE    LRR45                 OF NETWORK FIELD                           
         CLC   =C'PIO',SRPT                                                     
         BE    LRR45                                                            
*        CLC   SVNET,SNET          SAME NETWORK ?                               
*        BE    LRR50                                                            
*        MVC   SVNET,SNET          MOVE IN NEW NETWORK                          
*        B     LRR48                                                            
         B     LRR50               DON'T PAGE BREAK ON NEW NETWORK              
LRR45    CLC   SVPROD,SPRD         SAME PRODUCT ?                               
         BE    LRR50                                                            
         MVC   SVPROD,SPRD         MOVE IN NEW PRODUCT                          
LRR48    LH    R3,DLNTOTAL                                                      
         AH    R3,CANTOTAL                                                      
         AH    R3,MISTOTAL                                                      
         STH   R3,PRDTOTAL                                                      
         BAS   RE,PRNTOT                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     LRR50                                                            
         EJECT                                                                  
* SEE IF DELIVERY REC FOR THIS REPORT *                                         
*                                                                               
LRR50    L     RE,VADUMMY                                                       
LRR53    OC    0(L'DREC,RE),0(RE)                                               
         BZ    LRR58               NO DELIVERY MESSAGE FOUND                    
         CLC   SCRN,DCRN-DREC(RE)                                               
         BNE   LRR55                                                            
         CLC   SCRN+6(8),DRPT-DREC(RE)                                          
         BE    LRR60               DELIVERY/CAN FOUND                           
LRR55    LA    RE,L'DREC(,RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   LRR53                                                            
*                                                                               
LRR58    LH    R3,MISTOTAL         MISSING MESSAGE TOTAL                        
         LA    R3,1(R3)                                                         
         STH   R3,MISTOTAL                                                      
         MVC   SELNK,SPACES                                                     
         B     LRR70                                                            
*                                                                               
LRR60    DS    0H                                                               
* KEEP ONE-TO-ONE CORRESPODENCE BETWEEN SEND REC AND DELV REC                   
         MVI   DCRN-DREC(RE),C'X'  MARKED SO IT CAN'T BE MATCHED AGAIN          
*                                                                               
         CLI   DDELMS-DREC(RE),C'C' THIS A CANCELATION                          
         BNE   LRR63                                                            
         MVC   PCAN,=C'CANX'                                                    
         LH    R3,CANTOTAL                                                      
         LA    R3,1(R3)                                                         
         STH   R3,CANTOTAL                                                      
         MVC   SDELMS,=C'CAN'                                                   
         B     LRR67                                                            
*                                                                               
LRR63    CLI   DDELMS-DREC(RE),C'D' THIS A NORMAL DELIVIERY                     
         BE    *+6                                                              
         DC    H'0'                CAN ONLY BE CAN OR DLN                       
         LH    R3,DLNTOTAL                                                      
         LA    R3,1(R3)                                                         
         STH   R3,DLNTOTAL                                                      
         MVC   SDELMS,=C'DLN'                                                   
         SPACE                                                                  
* RESTORE TIME FROM BINARY TO HH:MM                                             
         SPACE                                                                  
LRR67    SR    R0,R0                                                            
         ICM   R0,3,DTIME-DREC(RE)                                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         MVC   SENDETM(2),WORK+1                                                
         MVI   SENDETM+2,C'.'                                                   
         MVC   SENDETM+3(2),WORK+3                                              
         MVC   SELNK,DELNK-DREC(RE)                                             
*                                                                               
LRR70    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SENDATE),(8,SRUNDT8)                              
         MVC   PDATE,SRUNDT8       MOVE IN DATE                                 
*                                                                               
LRR73    CLC   =C'TWX',SRPT        STATION FOR TWX ONLY                         
         BE    LRR80                                                            
         CLC   =C'NWX',SRPT        NETWORK FOR NWX/CWX ONLY                     
         BE    LRR85                                                            
         CLC   =C'CWX',SRPT        NETWORK FOR NWX/CWX ONLY                     
         BE    LRR85                                                            
         CLC   =C'QXX',SRPT        REP FOR QXX ONLY                             
         BE    LRR75                                                            
         CLC   =C'Q2Q',SRPT        REP FOR Q2Q ONLY                             
         BE    LRR75                                                            
         CLC   =C'QEZ',SRPT        REP FOR QEZ ONLY                             
         BNE   LRR87                                                            
LRR75    MVC   PQMED,SMEDIA                                                     
         MVC   PQCLT,SCLT                                                       
         MVC   PQPRD,SPRD                                                       
         CLI   SPRD+2,C'+'                                                      
         BNE   LRR78                                                            
         MVI   PQPRD+2,C' '                                                     
LRR78    MVC   PQREP(8),SSTA                                                    
         MVC   PQREFNUM,SREFNUM                                                 
         B     LRR87                                                            
*                                                                               
LRR80    MVC   PSTA(4),SSTA                                                     
         LA    R1,PSTA+4                                                        
         CLI   PSTA+3,C' '                                                      
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'-TV'                                                  
         CLI   SSTA+4,C'T'                                                      
         BE    LRR87                                                            
         MVC   0(3,R1),=C'-FM'                                                  
         CLI   SSTA+4,C'F'                                                      
         BE    LRR87                                                            
         MVC   0(3,R1),=C'-AM'                                                  
         CLI   SSTA+4,C'A'                                                      
         BE    LRR87                                                            
         MVC   0(3,R1),=C'-X '                                                  
         CLI   SSTA+4,C'X'         RADIO NETWORK                                
         BE    LRR87                                                            
         MVC   0(3,R1),=C'   '     NETWORK                                      
         CLI   SSTA+4,C'N'         LEAVE AS CBS                                 
         BE    LRR87                                                            
*        DC    H'0'                INVALID MEDIA                                
*                                                                               
LRR85    MVC   PSTA(3),SNET        MOVE IN NETWORK                              
*                                                                               
LRR87    CLC   =C'PIO',SRPT                                                     
         BNE   LRR95                                                            
         XC    PPADNUM(PPEND),PPADNUM                                           
         MVC   PPADNUM,SADNUM                                                   
         MVC   PPPUBNUM,SPUBNUM                                                 
*                                                                               
         MVC   PPPUBNUM(L'PPPUBNUM-1),PPPUBNUM+1                                
         MVI   PPPUBNUM+L'PPPUBNUM-1,C' '                                       
LRR90    CLI   PPPUBNUM,C' '        LEFT ALIGN FIELD                            
         BNE   LRR95                                                            
         MVC   PPPUBNUM(L'PPPUBNUM-1),PPPUBNUM+1                                
         B     LRR90                                                            
*                                                                               
LRR95    MVC   PCRN,SCRN           MOVE IN CUSTOMER REFERENCE                   
         MVC   PREPTID(3),SCRN+6                                                
         MVC   PREPTID+3(1),=C','                                               
         XC    WORK(5),WORK                                                     
         EDIT  (C5,SCRN+9),(5,WORK),ALIGN=LEFT                                  
         MVC   PREPTID+4,WORK      MOVE IN REPORT ID                            
*                                                                               
         CLC   SELNK,SPACES                                                     
         BNE   LRR100                                                           
         MVC   PELNK,=15C'*'       NO EASYLINK LEDGER                           
         B     LRR120                                                           
*                                                                               
LRR100   DS    0H                                                               
         CLC   =C'QXX',SRPT        NO EASYLINK LEDGER FOR QXX, Q2Q              
         BE    LRR120                                                           
         CLC   =C'Q2Q',SRPT                                                     
         BE    LRR120                                                           
         CLC   =C'TWX',SRPT                                                     
         BE    LRR110                                                           
         MVC   PELNK(11),SELNK     MOVE IN EASYLINK LEDGER                      
         B     LRR120                                                           
*                                                                               
LRR110   MVC   PELNK,SELNK         MOVE IN EASYLINK LEDGER                      
LRR120   MVC   PSENT,SENDSTM       MOVE IN TIME SENT                            
         MVI   PSENT+2,C'.'                                                     
*                                                                               
         MVC   PDELIV,SENDETM      MOVE IN DELIV TIME                           
         OC    PDELIV,PDELIV                                                    
         BNZ   LRR130                                                           
         MVC   PDELIV,=15C'*'                                                   
         B     LRR150                                                           
*                                                                               
LRR130   MVI   PDELIV+2,C'.'                                                    
         CLC   =C'QXX',SRPT        NO TIME CORRECTION NEEDED                    
         BE    LRR150                                                           
         CLC   =C'Q2Q',SRPT                                                     
         BE    LRR150                                                           
*                                                                               
* ADJUST STATION RECEIVED TIME TO DDS FUNNY TIME (00.00 = 08.00) *              
*                                                                               
LRR140   MVC   WORK(2),=C'00'                                                   
         MVN   WORK(2),SENDETM     MOVE NUMERICS                                
         CLC   WORK(2),SENDETM     IF NOT NUMERIC, BYPASS                       
         BNE   LRR150                                                           
         PACK  DUB,SENDETM(2)                                                   
         CP    DUB,=P'7'           IF LESS THAN 8                               
         BH    *+10                                                             
         AP    DUB,=P'24'          ADD 24 HOURS                                 
         SP    DUB,=P'8'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  PDELIV(2),DUB                                                    
*                                                                               
LRR150   DS    0H                                                               
         CLC   =C'PIO',SRPT                                                     
         BNE   LRR160                                                           
         MVC   PPCRN(PPCOPY),PCRN    GET PIO FIELDS                             
         MVC   PSTA(PPEND),PPADNUM                                              
         B     LRR200                                                           
*                                                                               
LRR160   DS    0H                                                               
         CLC   =C'QXX',SRPT                                                     
         BE    LRR190                                                           
LRR170   CLC   =C'Q2Q',SRPT                                                     
         BE    LRR190                                                           
LRR180   CLC   =C'QEZ',SRPT                                                     
         BNE   LRR200                                                           
LRR190   MVC   PQCRN(PQCOPY),PCRN    GET QXX/Q2Q/QEZ FIELDS                     
         MVC   PQSENT(20),PSENT                                                 
         MVC   PSTA(PQEND),PQMED                                                
*                                                                               
LRR200   MVI   SPACING,2                                                        
         BAS   RE,PRTLINE                                                       
         B     LRR20                                                            
         EJECT                                                                  
* E-O-F -- CHECK FOR ANY SORTED RECS *                                          
LRREND   CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   LRRENDA             NO                                           
         MVC   PCRN+132(30),=CL30'NO ACTIVITY FOR REPORT PERIOD'                
         BAS   RE,PRTLINE                                                       
*                                                                               
LRRENDA  GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (TWXFILE,)                                                       
****** IF SRPT IS Q2Q,QXX OR QEZ DON'T PRINT CLIENT/PROD TOTALS                 
         CLC   =C'QXX',SRPT                                                     
         BE    EXIT                                                             
         CLC   =C'Q2Q',SRPT                                                     
         BE    EXIT                                                             
         CLC   =C'QEZ',SRPT                                                     
         BE    EXIT                                                             
         BAS   RE,PRNTOT                                                        
         MVC   P+30(25),=CL25'CLIENT TOTAL - '                                  
         EDIT  (2,TRNTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                                  
***********************************************************************         
PRNTOT   NTR1                                                                   
         MVC   SAGYIDN,SVAGYID     RESTORE AGENCY ID NUM                        
         MVC   SMEDIA,SVMEDIA      RESTORE MEDIA                                
         MVC   SRPT,SVRPT          RESTORE REPORT TYPE                          
         MVC   SCLT,SVCLT          RESTORE CLIENT                               
*        MVC   SNET,SVNET          RESTORE NETWORK                              
         MVC   SPRD,SVPROD         RESTORE PROD                                 
*                                                                               
         MVC   P(132),SPACES                                                    
         BAS   RE,PRTLINE                                                       
*                                                                               
         OC    DLNTOTAL,DLNTOTAL                                                
         BZ    PRNT10                                                           
         MVC   P+30(25),=CL25'DELIVERIES RECEIVED - '                           
         EDIT  (2,DLNTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
*                                                                               
PRNT10   OC    CANTOTAL,CANTOTAL                                                
         BZ    PRNT20                                                           
         MVC   P+30(25),=CL25'CANCELLATIONS RECEIVED - '                        
         EDIT  (2,CANTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
*                                                                               
PRNT20   OC    MISTOTAL,MISTOTAL                                                
         BZ    PRNT30                                                           
         MVC   P+30(25),=CL25'NO NOTICE RECEIVED - '                            
         EDIT  (2,MISTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
*                                                                               
PRNT30   DS    0H                                                               
         OC    PRDTOTAL,PRDTOTAL                                                
         BZ    PRNT40                                                           
         CLC   =C'TWX',SVRPT                                                    
         BE    PRNT35                                                           
         CLC   =C'PIO',SVRPT                                                    
         BE    PRNT35                                                           
*        MVC   P+30(25),=CL25'NETWORK TOTAL - '                                 
*        B     PRNT38                                                           
         B     PRNT40                                                           
PRNT35   MVC   P+30(25),=CL25'PRODUCT TOTAL - '                                 
PRNT38   EDIT  (2,PRDTOTAL),(5,P+55)                                            
         BAS   RE,PRTLINE                                                       
*                                                                               
PRNT40   DS    0H                                                               
         XC    CANTOTAL,CANTOTAL                                                
         XC    DLNTOTAL,DLNTOTAL                                                
         XC    MISTOTAL,MISTOTAL                                                
         XC    PRDTOTAL,PRDTOTAL                                                
         MVC   SREC,0(R6)          RESTORE SREC                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
***********************************************************************         
         DS    0H                                                               
VPER     NTR1                                                                   
         LA    R3,8(R2)            START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         BZ    ERDATE                                                           
         LA    R3,1(RE,R3)         POINT TO END DATE                            
         CLM   RE,1,5(R2)          ONLY 1 DATE ENTERED                          
         BNE   VPER10                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(5,STDATE)                                  
         MVC   ENDATE,STDATE                                                    
         MVC   WORK+6(6),WORK                                                   
         B     VPER20                                                           
VPER10   GOTO1 DATVAL,(R1),(R3),WORK+6                                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERDATE                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(5,STDATE)                                  
         GOTO1 (RF),(R1),(0,WORK+6),(5,ENDATE)                                  
         CLC   WORK(6),WORK+6                                                   
         BH    ERDATSEQ                                                         
*                                                                               
* DATES MUST BE WITHIN THIS MONTH AND NOT AFTER TODAY *                         
*                                                                               
VPER20   GOTO1 DATCON,DMCB,(4,RCDATE),(0,DUB)                                   
         CLC   DUB(6),WORK+6            NOT IN THE FUTURE                       
         BL    ERFUTDAT                                                         
*                                                                               
         MVC   DUB+4(2),=C'01'                                                  
         CLC   DUB(6),WORK         MUST BE WITHIN THIS MONTH                    
         BH    ERMONDAT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET GRAPHNET TRANS RECS AND BUILD 1 TRANS REC *                               
***********************************************************************         
         DS    0H                                                               
GTW      NTR1                                                                   
*                                                                               
GTW10    BAS   RE,GETWX                                                         
*                                                                               
GTW11    CLC   =C'SENDTOWEST',BLOCK+1                                           
         BNE   GTW20                                                            
*                                                                               
         BAS   RE,SBLDDDS          GET ++DDS LINES AND BUILD SORT REC           
*                                                                               
GTW20    CLC   =C'O  BUFFER',BLOCK+1  SENT MESSAGE                              
         BNE   GTW30                                                            
*                                                                               
         CLC   =C'(10)(02)(01)',GMSGID                                          
         BNE   GTW10                                                            
         BAS   RE,BMS              GO BUILD MESSAGE IN AIO1                     
*                                                                               
         BAS   RE,GETABLE          GET APPROPRIATE REPORT TYPE TABLE            
         BNE   GTW40               INVALID REPORT TYPE, DON'T PUT               
*                                    IN SORT                                    
         BAS   RE,SBLD             BUILD SORT REC                               
         B     GTW20                                                            
*                                                                               
GTW30    CLC   =C'MARKREPORTSENT',BLOCK+1                                       
         BNE   GTW40                                                            
*                                                                               
         CLC   =C'ERR',REPTYPE                                                  
         BE    GTW40                                                            
*                                                                               
         CLC   =C'Q2Q',SRPT                                                     
         BE    GTW32                                                            
         CLC   =C'QXX',SRPT                                                     
         BE    GTW32                                                            
         CLC   =C'QEZ',SRPT                                                     
         BNE   GTW34                                                            
*                                                                               
GTW32    MVC   SREFNUM(6),=C'*TEST*' GET REP OFF FOR Q2Q/QXX/QEZ                
         MVC   SSTA,BLOCK+40                                                    
*                                                                               
GTW34    MVC   SENDSTM,BLOCK+51    MOVE SENT TIME AND                           
         MVC   DATE,BLOCK+61       MOVE SENT DATE AND                           
         GOTO1 DATVAL,DMCB,(0,DATE),SENDATE                                     
*                                                                               
         BAS   RE,FTR              FILTER                                       
         BNE   GTW35                                                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LH    R1,SRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,SRTCT                                                         
*                                                                               
GTW35    XC    SREC,SREC                                                        
         B     GTW10                                                            
*                                                                               
*                                                                               
GTW40    CLC   =C'I  BUFFER',BLOCK+1                                            
         BNE   GTW50                                                            
*                                                                               
         CLC   =C'(10)(02)',GMSGID                                              
         BNE   GTW10                                                            
         CLC   =C'CAN',GMSGID+8      ONLY WANT DELIVERED                        
         BE    GTW43                    OR CAN MESSAGES                         
         CLC   =C'DLN',GMSGID+8                                                 
         BNE   GTW10                                                            
*                                                                               
GTW43    BAS   RE,BMS              GO BUILD MESSAGE IN AIO1                     
         BAS   RE,GETABLE                                                       
*                                                                               
         BAS   RE,DBLD             BUILD DELIVERY REC                           
         BNE   GTW20                WRONG OR BAD, BYPASS                        
*                                                                               
         BAS   RE,DFTR             DELIVERY REC FILTER                          
         BNE   GTW20                                                            
*                                                                               
         BAS   RE,PUTD                                                          
         B     GTW20               NOW CK LAST REC READ                         
*                                                                               
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
         CLC   =C'SENDTOWEST',BLOCK+1                                           
         BE    GETWXX                                                           
         CLC   =C'SENDWESTBUFFER',BLOCK+1                                       
         BE    GETWXX                                                           
         CLC   =C'O  BUFFER',BLOCK+1                                            
         BE    GETWXX                                                           
         CLC   =C'I  BUFFER',BLOCK+1                                            
         BE    GETWXX                                                           
         CLC   =C'MARKREPORTSENT',BLOCK+1                                       
         BNE   GETWX10                                                          
*                                                                               
GETWXX   B     EXIT                                                             
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
*                                                                               
         LR    R6,R4                                                            
BMS10    MVC   0(81,R6),GMSGID                                                  
         LA    R6,81(R6)                                                        
         LA    RF,GMSGID+81                                                     
*                                                                               
BMS20    CLI   0(RF),C' '          MOVE ANY OVER 81 CHARACTERS                  
         BNH   BMS30                                                            
         MVC   0(1,R6),0(RF)                                                    
         LA    R6,1(,R6)                                                        
         LA    RF,1(,RF)                                                        
         B     BMS20                                                            
*                                                                               
BMS30    C     R6,AIO2             CHECK IF AIO2 OVERWRITTEN                    
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GET   TWXFILE,BLOCK       GET NEXT REC                                 
         LH    R1,RECCT                                                         
         LA    R1,1(R1)                                                         
         STH   R1,RECCT                                                         
*                                                                               
         CLI   BLOCK+1,C' '                                                     
         BNH   BMS10                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD SORT REC FOR MESSAGE SENT WITH DDS *                                    
***********************************************************************         
         DS    0H                                                               
SBLDDDS  NTR1                                                                   
         B     EXIT                                                             
***********************************************************************         
* BUILD SORT REC FOR MESSAGE SENT *                                             
***********************************************************************         
         DS    0H                                                               
SBLD     NTR1                                                                   
         CLC   =C'ERR',REPTYPE                                                  
         BE    SBLDX                                                            
         L     R2,AIO1                                                          
*                                                                               
         LA    R2,12(R2)           BUMP PASS (10)(02)(01)                       
         GOTO1 BUMPFLD,DMCB,2,(R2) BUMP PASS 2 (0D)(25)'S                       
         CLI   DMCB+3,0                                                         
         BE    SBLDX               SET BYPASS                                   
         L     R2,DMCB                                                          
*                                                                               
         LR    RF,R2                                                            
SBLD30   CLI   0(R2),X'5E'         BUMP PASS SEMICOLON                          
         BE    SBLD40                                                           
         LA    R2,1(R2)                                                         
         C     R2,AIO2                                                          
         BL    SBLD30                                                           
         B     SBLDX               SET BYPASS                                   
*                                                                               
SBLD40   CLC   =C'TWX',REPTYPE     ONLY TWX NEED STATION FIELD                  
         BNE   SBLD45                                                           
         LR    R1,R2                                                            
         SR    R1,RF               GET LENGTH OF FIELD                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SSTA(0),0(RF)       STATION/REP                                  
*                                                                               
SBLD45   LA    R2,1(R2)            BUMP PASS SEMICOLON                          
         MVC   SCRN(18),0(R2)      CUSTOMER REFERENCE NUMBER                    
         MVC   SAGYID(4),2(R2)     \                                            
         PACK  DUB,SAGYID(4)        \ CONVERT EBCDIC ID NUM TO BIN              
         CVB   R6,DUB               /                                           
         STH   R6,SAGYIDN          /                                            
         MVC   SRPT,6(R2)          REPORT TYPE                                  
         LA    R2,18(R2)           BUMP PASS CUSTOMER REF #                     
*                                                                               
SBLD45A  CLI   0(R2),C':'          BUMP UNTIL A COLON IS ENCOUNTERED            
         BE    SBLD46                                                           
         LA    R2,1(R2)                                                         
         B     SBLD45A                                                          
*                                                                               
SBLD46   LA    R2,1(R2)            BUMP PASS COLON                              
*                                                                               
         LR    RF,R2               GET BILLING INFO                             
         LA    R2,8(R2)            BUMP PASS AGENCY ORIGIN                      
SBLD47   CLI   0(RF),C'+'                                                       
         BE    SBLD48                                                           
         LA    RF,1(RF)                                                         
         CR    RF,R2                                                            
         BL    SBLD47                                                           
*                                                                               
         CLI   0(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SMEDIA(1),0(R2)                                                  
         CLI   1(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SCLT(3),1(R2)                                                    
*                                                                               
***PIO                                                                          
SBLD47A  CLC   =C'PIO',REPTYPE                                                  
         BNE   SBLD48                                                           
         CLI   4(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SPRD,4(R2)                                                       
*                                                                               
* WILL GET AD NUM, PRINT NOTHING FOR NOW                                        
*        MVC   SADNUM,=C'*TEST*'                                                
*                                                                               
         GOTO1 BUMPFLD,DMCB,9,(R2) BUMP PASS 9 (0D)(25)'S                       
         CLI   DMCB+3,0                                                         
         BE    SBLDX               SET BYPASS                                   
         L     R2,DMCB                                                          
         MVC   SPUBNUM,58(R2)      GET PUB NUMBER                               
         B     SBLDX                                                            
*                                                                               
***TWX                                                                          
SBLD48   CLC   =C'TWX',REPTYPE                                                  
         BNE   SBLD50                                                           
         CLI   4(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SPRD,4(R2)                                                       
         B     SBLDX                                                            
*                                                                               
***CWX/NWX                                                                      
SBLD50   CLC   =C'CWX',REPTYPE                                                  
         BE    SBLD55                                                           
         CLC   =C'NWX',REPTYPE                                                  
         BNE   SBLD60                                                           
SBLD55   CLI   4(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SNET,4(R2)                                                       
         CLI   SNET+3,C'+'                                                      
         BNE   SBLD58                                                           
         MVI   SNET+3,C' '                                                      
*                                                                               
SBLD58   MVC   SRPT,=C'CWX'        SORT BOTH NWX AND CWX AS CWX                 
         B     SBLDX                                                            
*                                                                               
***Q2Q/QXX/QEZ                                                                  
SBLD60   CLC   =C'Q2Q',REPTYPE                                                  
         BE    SBLD63                                                           
SBLD63   CLC   =C'QXX',REPTYPE                                                  
         BE    SBLD65                                                           
SBLD65   CLC   =C'QEZ',REPTYPE                                                  
         BNE   SBLD70                                                           
         CLI   4(R2),C'+'                                                       
         BE    *+10                                                             
         MVC   SPRD,4(R2)                                                       
         B     SBLDX                                                            
*                                                                               
SBLD70   DS    0H                  ADD MORE REPORT TYPES HERE                   
*                                                                               
SBLDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FILTER ON MEDIA, CLIENT, PRODUCT, DATES - SORT RECS                           
***********************************************************************         
         DS    0H                                                               
FTR      NTR1                                                                   
*                                                                               
         OC    TRAMED,TRAMED                                                    
         BZ    FTR03                                                            
         CLC   SMEDIA,TRAMED          SAME MEDIA                                
         BNE   FTRNE                                                            
*                                                                               
FTR03    OC    TRACLT,TRACLT                                                    
         BZ    FTR05                                                            
         CLC   SCLT,TRACLT                                                      
         BNE   FTRNE                                                            
*                                                                               
FTR05    OC    TRAPRD,TRAPRD                                                    
         BZ    FTR10                                                            
         CLC   =C'TWX',SRPT                                                     
         BE    FTR08                                                            
         CLC   =C'PIO',SRPT                                                     
         BE    FTR08                                                            
*                                                                               
         CLC   SNET,TRAPRD         FILTER ON PRODUCT/NETWORK                    
         BNE   FTRNE                                                            
         B     FTR10                                                            
FTR08    CLC   SPRD,TRAPRD                                                      
         BNE   FTRNE                                                            
*                                                                               
FTR10    CLC   STDATEN,SENDATE                                                  
         BH    FTRNE                                                            
         CLC   ENDATEN,SENDATE                                                  
         BL    FTRNE                                                            
*                                                                               
FTR20    OC    TRATYP,TRATYP       NO REPORT TYPE FILTER,                       
         BZ    FTREQ                 TAKE EVERYTHING                            
*                                                                               
         LA    R1,FREPTYP          BUILD REPORT TYPE FILTER                     
FTR30    CLC   SRPT,0(R1)                                                       
         BE    FTREQ                                                            
         LA    R1,3(R1)                                                         
         OC    0(1,R1),0(R1)                                                    
         BZ    FTRNE                                                            
         B     FTR30                                                            
*                                                                               
FTREQ    CR    RB,RB                                                            
         B     FTRX                                                             
FTRNE    LTR   RB,RB                                                            
FTRX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FILTER ON REPORT TYPE - DELIVERY RECS                                         
***********************************************************************         
         DS    0H                                                               
DFTR     NTR1                                                                   
         OC    TRATYP,TRATYP       NO REPORT TYPE FILTER,                       
         BZ    DFTREQ                TAKE EVERYTHING                            
*                                                                               
         LA    R1,FREPTYP          BUILD REPORT TYPE FILTER                     
DFTR10   CLC   DRPT(3),0(R1)                                                    
         BE    DFTREQ                                                           
         LA    R1,3(R1)                                                         
         OC    0(1,R1),0(R1)                                                    
         BZ    DFTRNE                                                           
         B     DFTR10                                                           
*                                                                               
DFTREQ   CR    RB,RB                                                            
         B     DFTRX                                                            
DFTRNE   LTR   RB,RB                                                            
DFTRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUMP PASS A NUMBER OF (0D)(25)'S IN OUTPUT BUFFER                  
*                                                                               
* ON ENTRY:    P1                  NUMBER OF (0D)(25)'S TO BUMP                 
*              P2                  ADDRESS OF CURRENT POSITION                  
*                                                                               
* ON EXIT:     P2                  ADDRESS OF NEW POSITION                      
***********************************************************************         
         DS    0H                                                               
BUMPFLD  NTR1                                                                   
         L     R3,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
BUMPF10  CLC   =C'(0D)(25)',0(R2)                                               
         BE    BUMPF20                                                          
         LA    R2,1(R2)                                                         
         C     R2,AIO2                                                          
         BL    BUMPF10                                                          
         B     BUMPERR             SET BYPASS                                   
*                                                                               
BUMPF20  LA    R2,8(R2)                                                         
         BCT   R3,BUMPF10                                                       
*                                                                               
         ST    R2,DMCB                                                          
         B     EXIT                                                             
*                                                                               
BUMPERR  XC    DMCB,DMCB                                                        
         B     EXIT                                                             
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
*                                                                               
         LA    R2,12(R2)             BUMP PASS (10)(02)(01)                     
         SR    R3,R3                                                            
*                                                                               
GETAB10  CLC   =C'(0D)(25)',0(R2)  BUMP PASS 2 (0D)(25)'S                       
         BE    GETAB20                                                          
         LA    R2,1(R2)                                                         
         C     R2,AIO2                                                          
         BL    GETAB10                                                          
         CR    RB,RD                 SET BYPASS                                 
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
         MVC   REPHDHK,3(R3)       GET REPORT TYPE HEADING ROUTINE ADD          
         MVC   REPSYS,6(R3)        GET REPORT TYPE SYSTEM NAME                  
         MVC   REPJID,13(R3)       GET REPORT TYPE JOB ID                       
         CR    RB,RB               SET OK                                       
*                                                                               
GETABX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET HEADER LINE AND HEADER ROUTINE FOR A REPORT TYPE *                        
***********************************************************************         
         DS    0H                                                               
GETRTYPE NTR1                                                                   
         LA    R3,REPTABLE                                                      
*                                                                               
GETR3    CLC   SRPT,0(R3)          FIND MATCHING REPORT TYPE                    
         BE    GETR5                                                            
         LA    R3,L'REPTABLE(R3)   BUMP TO NEXT ROW                             
         CLI   0(R3),X'FF'                                                      
         BNE   GETR3                                                            
         B     GETRX                                                            
*                                                                               
GETR5    DS    0H                                                               
         MVC   REPTYPE,0(R3)       GET REPORT TYPE                              
         MVC   REPHDHK,3(R3)       GET REPORT TYPE HEADING ROUTINE ADD          
         MVC   REPSYS,6(R3)        GET REPORT TYPE SYSTEM NAME                  
         MVC   REPJID,13(R3)       GET REPORT TYPE JOB ID                       
         MVC   SVAGYID,SAGYIDN                                                  
*                                                                               
         LA    R3,SYSLST+6                                                      
GETR10   CLI   0(R3),0                                                          
         BNE   GETR15                                                           
         DC    H'0'                MUST FIND SYSTEM NUMBER                      
GETR15   CLC   REPSYS,2(R3)                                                     
         BE    GETR20                                                           
         LA    R3,L'SYSLST(R3)     BUMP TO NEXT ROW                             
         B     GETR10                                                           
*                                                                               
GETR20   MVC   REPSYSN,0(R3)       GET SYSTEM NUMBER                            
*                                                                               
GETRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD SORT REC FOR DELIVERY MESSAGES *                                        
***********************************************************************         
         DS    0H                                                               
DBLD     NTR1                                                                   
         L     R2,AIO1                                                          
*                                                                               
         LA    R2,8(R2)            BUMP PASS (10)(02)                           
         MVC   DDELMS,0(R2)                                                     
*                                                                               
         GOTO1 BUMPFLD,DMCB,1,(R2)                                              
         CLI   DMCB+3,0                                                         
         BE    DBLDNEX                                                          
         L     R2,DMCB                                                          
         MVC   DCRN,0(R2)                                                       
         MVC   DRPT,6(R2)                                                       
*                                                                               
         GOTO1 BUMPFLD,DMCB,1,(R2)                                              
         CLI   DMCB+3,0                                                         
         BE    DBLDNEX                                                          
         L     R2,DMCB                                                          
         SPACE                                                                  
* CONVERT TIME TO BINARY TO SAVE SPACE                                          
         SPACE                                                                  
         MVC   FULL(2),8(R2)                                                    
         MVC   FULL+2(2),11(R2)                                                 
         PACK  DUB,FULL                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,DTIME                                                       
*                                                                               
         CLC   =C'QXX',REPTYPE     NO EASYLINK LEDGER FOR QXX, Q2Q              
         BE    DBLDEQX                                                          
         CLC   =C'Q2Q',REPTYPE                                                  
         BE    DBLDEQX                                                          
         GOTO1 BUMPFLD,DMCB,1,(R2)                                              
         CLI   DMCB+3,0                                                         
         BE    DBLDNEX                                                          
         L     R2,DMCB                                                          
         MVC   DELNK,0(R2)                                                      
         B     DBLDEQX                                                          
*                                                                               
DBLDEQX  CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
DBLDNEX  CR    RB,RD                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PUT DELIVERY REC OUT INTO DUMMY *                                             
***********************************************************************         
PUTD     L     R1,VADUMMY                                                       
         LR    R0,R1                                                            
         A     R0,=F'250000'                                                    
PUTD10   OC    0(L'DREC,R1),0(R1)  EMPTY SPOT                                   
         BZ    PUTD20                                                           
*                                                                               
         CLC   DCRN,DCRN-DREC(RE)                                               
         BNE   PUTD14                                                           
         CLC   DRPT,DRPT-DREC(R1)  ALREADY HERE                                 
         BE    PUTD20              OVERLAY                                      
*                                                                               
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
*                                                                               
         MVC   H1+2(4),=C'SPOT'                                                 
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+2(5),=C'MEDIA'                                                
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         MVC   H3+2(6),=C'CLIENT'                                               
         MVC   H3+12(3),SCLT                                                    
         CLC   STDATE,ENDATE                                                    
         BE    TWXHDHK3                                                         
         MVC   H3+35(8),STDATE                                                  
         MVC   H3+44(2),=C'TO'                                                  
         MVC   H3+47(8),ENDATE                                                  
         B     TWXHDHK4                                                         
TWXHDHK3 MVC   H3+41(8),STDATE                                                  
TWXHDHK4 MVC   H4+2(7),=C'PRODUCT'                                              
         CLI   SPRD+2,C'+'                                                      
         BE    TWXHDHK5                                                         
         MVC   H4+12(3),SPRD                                                    
         B     *+10                                                             
TWXHDHK5 MVC   H4+12(2),SPRD                                                    
         MVC   H8+9(7),=C'STATION'                                              
         MVC   H9+9(7),=C'-------'                                              
         B     EXIT                                                             
         EJECT                                                                  
         DS    0H                                                               
CWXHDHK  NTR1                      TWX REPORT                                   
*                                                                               
         MVC   H1+2(7),=C'NETWORK'                                              
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+2(5),=C'MEDIA'                                                
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         MVC   H3+2(6),=C'CLIENT'                                               
         MVC   H3+12(3),SCLT                                                    
         CLC   STDATE,ENDATE                                                    
         BE    CWXHDHK3                                                         
         MVC   H3+35(8),STDATE                                                  
         MVC   H3+44(2),=C'TO'                                                  
         MVC   H3+47(8),ENDATE                                                  
         B     CWXHDHK5                                                         
CWXHDHK3 MVC   H3+41(8),STDATE                                                  
CWXHDHK5 MVC   H8+9(7),=C'NETWORK'                                              
         MVC   H9+9(7),=C'-------'                                              
         B     EXIT                                                             
         EJECT                                                                  
         DS    0H                                                               
Q2QHDHK  NTR1                      TWX REPORT                                   
*                                                                               
         MVC   H1+2(4),=C'ADDS'                                                 
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         CLC   STDATE,ENDATE                                                    
         BE    Q2QHDHK3                                                         
         MVC   H3+35(8),STDATE                                                  
         MVC   H3+44(2),=C'TO'                                                  
         MVC   H3+47(8),ENDATE                                                  
         B     Q2QHDHK4                                                         
Q2QHDHK3 MVC   H3+41(8),STDATE                                                  
*                                                                               
Q2QHDHK4 MVC   H8+2(90),SPACES     CLEAR HEADING                                
         MVC   H9+2(90),SPACES                                                  
         MVC   H8+2(4),=C'DATE'    SPECIAL HEADING FOR Q2Q                      
         MVC   H9+2(4),=18C'-'                                                  
         MVC   H8+9(3),=C'MED'                                                  
         MVC   H9+9(3),=18C'-'                                                  
         MVC   H8+15(3),=C'CLT'                                                 
         MVC   H9+15(3),=18C'-'                                                 
         MVC   H8+21(3),=C'PRD'                                                 
         MVC   H9+21(3),=18C'-'                                                 
         MVC   H8+27(7),=C'REF NUM'                                             
         MVC   H9+27(7),=18C'-'                                                 
         MVC   H8+37(3),=C'REP'                                                 
         MVC   H9+37(3),=18C'-'                                                 
         MVC   H8+47(18),=C'CUSTOMER REFERENCE'                                 
         MVC   H9+47(18),=18C'-'                                                
         MVC   H8+68(9),=C'REPORT ID'                                           
         MVC   H9+68(9),=18C'-'                                                 
         MVC   H8+80(4),=C'SENT'                                                
         MVC   H9+80(4),=18C'-'                                                 
         MVC   H8+88(8),=C'RECEIVED'                                            
         MVC   H9+88(8),=18C'-'                                                 
         B     EXIT                                                             
         EJECT                                                                  
         DS    0H                                                               
PIOHDHK  NTR1                      TWX REPORT                                   
*                                                                               
         MVC   H1+2(5),=C'PRINT'                                                
         BAS   RE,GETINFO          GET FULL NAME AND ADDRESS                    
         MVC   H1+72(33),SAGYINFO  AGENCY NAME                                  
         MVC   H2+2(5),=C'MEDIA'                                                
         MVC   H2+12(1),SMEDIA                                                  
         MVC   H2+72(33),SAGYINFO+33   AGENCY ADDRESS                           
         MVC   H3+2(6),=C'CLIENT'                                               
         MVC   H3+12(3),SCLT                                                    
         CLC   STDATE,ENDATE                                                    
         BE    PIOHDHK3                                                         
         MVC   H3+35(8),STDATE                                                  
         MVC   H3+44(2),=C'TO'                                                  
         MVC   H3+47(8),ENDATE                                                  
         B     PIOHDHK4                                                         
PIOHDHK3 MVC   H3+41(8),STDATE                                                  
PIOHDHK4 MVC   H4+2(7),=C'PRODUCT'                                              
         CLI   SPRD+2,C'+'                                                      
         BE    PIOHDHK5                                                         
         MVC   H4+12(3),SPRD                                                    
         B     *+10                                                             
PIOHDHK5 MVC   H4+12(2),SPRD                                                    
         MVC   H8+2(90),SPACES     CLEAR HEADING                                
         MVC   H9+2(90),SPACES                                                  
*                                                                               
         MVC   H8+2(4),=C'DATE'    SPECIAL HEADING FOR PIO                      
         MVC   H9+2(4),=18C'-'                                                  
*        SKIP THIS FOR NOW...                                                   
*        MVC   H8+9(6),=C'AD NUM'                                               
*        MVC   H9+9(6),=18C'-'                                                  
*                                                                               
         MVC   H8+18(7),=C'PUB NUM'                                             
         MVC   H9+18(7),=18C'-'                                                 
         MVC   H8+41(18),=C'CUSTOMER REFERENCE'                                 
         MVC   H9+41(18),=18C'-'                                                
         MVC   H8+62(9),=C'REPORT ID'                                           
         MVC   H9+62(9),=18C'-'                                                 
         MVC   H8+74(8),=C'EASYLINK'                                            
         MVC   H9+74(8),=18C'-'                                                 
         MVC   H8+92(4),=C'SENT'                                                
         MVC   H9+92(4),=18C'-'                                                 
         MVC   H8+100(8),=C'RECEIVED'                                           
         MVC   H9+100(8),=18C'-'                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE *                                                                  
***********************************************************************         
         DS    0H                                                               
PRTLINE  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
RELO     DS    F                                                                
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     TRAPERR                                                          
ERDATE   MVI   GERROR1,DATERR                                                   
         B     TRAPERR                                                          
ERDATSEQ MVI   GERROR1,DATSEQER                                                 
         B     TRAPERR                                                          
ERFUTDAT MVI   GERROR1,FUTDATER                                                 
         B     TRAPERR                                                          
ERMONDAT MVI   GERROR1,MONDATER                                                 
         B     TRAPERR                                                          
TRAPERR  GOTO1 MYERR                                                            
         GETEL R6,DATADISP,ELCODE        USED FOR GETEL OPERATIONS              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* DCB FOR OFF-LINE PRINT FILE FOR DATA SENT TO GRAPHNET                         
* SPOOL FROM PRTFILE                                                            
*                                                                               
TWXFILE  DCB   DDNAME=TWXFILE,DSORG=PS,RECFM=FB,BLKSIZE=26600,         X        
               LRECL=133,MACRF=GM,EODAD=ENDTWX                                  
*                                                                               
* SORT ON MEDIA/CLIENT/PRODUCT/STATION/RUN DATE/RUN TIME/REPORT ID              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,57,A),FORMAT=CH'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=82'                                    
         EJECT                                                                  
***********************************************************************         
* TABLE OF REPORT TYPES AND SYSTEMS                                             
*                                                                               
REPTABLE DS    0CL16                                                            
*              REP TYP,HEADER ROUT,SYSTEM NAME,JOB ID                           
*                                                                               
         DC    C'CWX',AL3(CWXHDHK),C'NETWORK',C'NTR'                            
         DC    C'NWX',AL3(CWXHDHK),C'NETWORK',C'NTR'                            
         DC    C'PIO',AL3(PIOHDHK),C'PRINT  ',C'PTR'                            
         DC    C'QEZ',AL3(Q2QHDHK),C'SPOT   ',C'STR'                            
         DC    C'Q2Q',AL3(Q2QHDHK),C'SPOT   ',C'STR'                            
         DC    C'QXX',AL3(Q2QHDHK),C'SPOT   ',C'STR'                            
         DC    C'TWX',AL3(TWXHDHK),C'SPOT   ',C'STR'                            
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*  IF FASYSLST IS CHANGED, REPTABLE MAY HAVE TO BE UPDATED TO                   
*  REFLECT THE CHANGES                                                          
*                                                                               
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
HEADING  SSPEC H6,3,PAGE                                                        
         SSPEC H1,33,C'EASYLINK TRANSACTION REPORT'                             
         SSPEC H2,33,C'---------------------------'                             
         SSPEC H4,73,RUN                                                        
*                                                                               
         SSPEC H8,3,C'DATE'                                                     
         SSPEC H9,3,C'----'                                                     
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
*                                                                               
         PRINT OFF                                                              
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
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE SPADDFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPADDD7D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPADDWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
MYAREAD  DSECT                                                                  
DATE     DS    CL8                                                              
STDATE   DS    CL8                                                              
ENDATE   DS    CL8                                                              
STDATEN  DS    CL6                                                              
ENDATEN  DS    CL6                                                              
SVAGYID  DS    XL2                                                              
SVMEDIA  DS    CL1                                                              
SVRPT    DS    CL3                                                              
SVCLT    DS    CL3                                                              
SVNET    DS    CL4                                                              
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
NEWQUE   DS    CL1                 NEW QUEUE FLAG                               
REPSYSN  DS    XL1                 REPORT TYPE SYSTEM NUMBER                    
FREPTYP  DS    CL40                FILTER FOR REPORT TYPE                       
SVTWPROF DS    CL16                                                             
*                                                                               
SVTWPR1  EQU   SVTWPROF+0          Y=ALLOW TWX INSTRUCTIONS                     
SVTWPR2  EQU   SVTWPROF+1          O/L GEN REQ C=COPY OF TWX                    
*                                              I=AUTO INSTR OF TWX              
SVTWPR3  EQU   SVTWPROF+2                                                       
* 3                                                                             
* DELIVERY INFO RECS *                                                          
*                                                                               
DREC     DS    0CL46                                                            
DCRN     DS    CL18                CUSTOMER REFERENCE                           
DRPT     DS    CL8                 REPORT ID                                    
DELNK    DS    CL15                EASY LINK LEDGER NUMBER                      
DTIME    DS    XL2   WAS 5         TIME DELIVERED                               
DDELMS   DS    CL1   WAS 3         DELIVERY MESSAGE/CAN OR DLN                  
         EJECT                                                                  
* TRANSMISSION INFO RECS *                                                      
* CHANGE SORTCARD AND RECCARD IF SREC IS CHANGED                                
*                                                                               
         DS    0H                                                               
SREC     DS    0CL109                                                           
SRECSRT  DS    0CL82                                                            
SAGYIDN  DS    H                   AGENCY ID NUMBER                             
SRPT     DS    CL3                 REPORT ID                                    
SMEDIA   DS    CL1                 MEDIA                                        
SCLT     DS    CL3                 CLIENT                                       
SNET     DS    CL4                 CWX/NWX NETWORK                              
         ORG   SNET                                                             
SPRD     DS    CL3                 TWX/PIO PRODUCT                              
         DS    CL1                 SPARE                                        
SADNUM   DS    CL6                 PIO AD NUMBER                                
SPUBNUM  DS    CL20                PIO PUB NUMBER                               
         ORG   SADNUM                                                           
SREFNUM  DS    CL7                 ADDS AVAIL REF NUM                           
SSTA     DS    CL8                 TWX STATION/ADDS REP OFF                     
         DS    CL11                SPARE                                        
SCRN     DS    CL18                CUSTOMER REFERENCE NUMBER                    
SENDATE  DS    CL6                 DELIVERY DATE                                
SENDSTM  DS    CL8                 DELIVERY START TIME                          
SENDETM  DS    CL8                 DELIVERY END TIME (RECEIPT)                  
SDELMS   DS    CL3                 DELIVERY MESSAGE                             
*                                  DLN = OK, CAN = NOT DELIVERED                
*                                                                               
* SELNK NOT INCLUDED IN SORT, ONLY BUILT FROM DELIVERY RECORD *                 
*                                                                               
SELNK    DS    CL15                EASY LINK LEDGER                             
*                                                                               
* SRUNDT8 NOT INCLUDED IN SORT, BUT REBUILT AFTER SORT *                        
*                                                                               
SRUNDT8  DS    CL8                 RUN DATE MONDA/YR                            
* SAGYID NOT INCLUDED IN SORT,USED TO BUILD AGENCY ID NUMBER                    
*                                                                               
SAGYID   DS    CL4                 AGENCY ID NUM                                
         EJECT                                                                  
* TABLE TO PULL APPROPRIATE REPORT TYPE INFO *                                  
REPTYPE  DS    CL3                 REPORT TYPE                                  
REPHDHK  DS    AL3                 REPORT TYPE HEAD HOOK                        
REPSYS   DS    CL7                 REPORT TYPE SYSTEM NAME                      
REPJID   DS    CL3                 REPORT TYPE JOB NAME                         
*                                                                               
* Q2Q SPECIAL PRINT LINE                                                        
*                                                                               
PQMED    DS    CL1                                                              
         DS    CL5                                                              
PQCLT    DS    CL3                                                              
         DS    CL3                                                              
PQPRD    DS    CL3                                                              
         DS    CL3                                                              
PQREFNUM DS    CL7                                                              
         DS    CL3                                                              
PQREP    DS    CL8                                                              
         DS    CL2                                                              
PQCRN    DS    CL18                                                             
         DS    CL3                                                              
PQREPTID DS    CL8                                                              
         DS    CL4                                                              
PQCOPY   EQU   *-PQCRN             COPY THESE FROM P                            
PQSENT   DS    CL5                                                              
         DS    CL3                                                              
PQDELIV  DS    CL5                                                              
         DS    CL3                                                              
PQCAN    DS    CL4                                                              
PQEND    EQU   *-PQMED             LENGTH OF Q2QLINE                            
*                                                                               
* PIO SPECIAL PRINT LINE                                                        
*                                                                               
PPADNUM  DS    CL6                                                              
         DS    CL3                                                              
PPPUBNUM DS    CL20                                                             
         DS    CL3                                                              
PPCRN    DS    CL18                                                             
         DS    CL3                                                              
PPREPTID DS    CL8                                                              
         DS    CL4                                                              
PPELNK   DS    CL15                                                             
         DS    CL3                                                              
PPSENT   DS    CL5                                                              
         DS    CL3                                                              
PPDELIV  DS    CL5                                                              
         DS    CL3                                                              
PPCAN    DS    CL4                                                              
PPCOPY   EQU   *-PPCRN             COPY THESE FROM P                            
PPEND    EQU   *-PPADNUM           LENGTH OF PIOLINE                            
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
*                                                                               
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
         DS    CL21                                                             
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
*                                                                               
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
**PAN#1  DC    CL21'006SPADD07   03/25/15'                                      
         END                                                                    
