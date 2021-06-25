*          DATA SET SPLDEXTPT  AT LEVEL 097 AS OF 09/28/00                      
*PHASE SPEXTPTA                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE QSORT                                                                  
         TITLE 'SPLDEXT - RETRIEVE LOST RECORDS'                                
***********************************************************************         
*                                                                     *         
*        USING KEYS FROM INPUT FILE, FIND RECORDS ON                  *         
*        OLD DUMP TAPE AND ADD THEM TO DATASET                        *         
*        TO BE ADDED TO LOAD TAPE OVER THE WEEKEND                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* PARAMETER LIST                                                      *         
*                                                                     *         
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                     *         
*                               X'01'= RECORD IN CORE                 *         
*                               X'FF'= END OF FILE                    *         
*               RETURN VALUE    X'00'= KEEP RECORD                    *         
*                               X'FF'= PURGE RECORD                   *         
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ        *         
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                     *         
*                               X'40'= TAPE OUTPUT                    *         
*                               X'20'= RECORD IS I/S FILE RECORD      *         
* P3=A(PARAM CARD)                                                    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPLDEXTPT - FIND LOST RECORDS - INIT'                           
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 0,SPLDEXT                                                        
*                                                                               
         L     RC,=A(WORKD)        ESTABLISH WORKING STORAGE                    
         USING WORKD,RC                                                         
*                                                                               
         ST    R1,APARM            SAVE A(PARAMETER LIST)                       
         MVC   PLIST,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     R9,VLDDEFN          ESTABLISH LOAD CONTROLS                      
         USING LDDEFND,R9                                                       
*                                                                               
         TITLE 'SPLDEXTPT - FIND LOST RECORDS - DMCTL'                          
***********************************************************************         
*                                                                     *         
*        CONTROL FLOW LOGIC                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXCTL   DS    0H                                                               
*                                                                               
         CLI   PRMMODE,PRMMINIQ                                                 
         BE    DMXINIT             INITIALIZE                                   
*                                                                               
         CLI   PRMMODE,PRMMRECQ    NEW RECORD IN CORE                           
         BE    DMXREC              PROCESS                                      
*                                                                               
         CLI   PRMMODE,PRMMEOFQ                                                 
         BE    DMXEOF              END-OF-FILE                                  
*                                                                               
         B     DMXIT                                                            
*                                                                               
*        EXITS                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         CLOSE FILEOUT             CLOSE OUTPUT TAPE                            
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID2,SPACES         CLEAR HEADLINES                              
         MVC   MID3,SPACES                                                      
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         MVC   P(25),=C'RECORDS ADDED TO FILE = '                               
         EDIT  (P8,RECCNTR),(15,P+25),COMMAS=YES,ALIGN=LEFT                     
*                                                                               
         GOTO1 VPRINTER            PRINT RECORD COUNT                           
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),PRMRKPQ                                                    
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),PRMRPRGQ                                                   
         B     DMXIT                                                            
*                                                                               
DMXPGEOF DS    0H                  PURGE AND CAUSE INPUT EOF EXIT               
*                                                                               
         CLOSE FILEOUT             CLOSE OUTPUT TAPE                            
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID2,SPACES         CLEAR HEADLINES                              
         MVC   MID3,SPACES                                                      
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         MVC   P(25),=C'RECORDS ADDED TO FILE = '                               
         EDIT  (P8,RECCNTR),(15,P+25),COMMAS=YES,ALIGN=LEFT                     
*                                                                               
         GOTO1 VPRINTER            PRINT RECORD COUNT                           
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         L     R1,APARM                                                         
         MVI   0(R1),PRMREOJQ                                                   
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
*                                                                               
         TITLE 'SPLDEXTPT - FIND LOST RECORDS - DMXINIT'                        
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
*                                                                               
         L     RF,=V(HEXIN)                                                     
         ST    RF,VHEXIN                                                        
*                                                                               
         L     RF,=V(QSORT)                                                     
         ST    RF,VQSORT                                                        
*                                                                               
         ZAP   RECCNTR,=P'0'                                                    
*                                                                               
         OPEN  (CARD,INPUT)        OPEN FILE OF LOST KEYS                       
*                                                                               
         SR    R2,R2               INIT RECORD COUNTER                          
*                                                                               
         MVC   MID1(60),=CL60'RECORDS AS READ'                                  
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         L     R4,=A(PTTAB)        TABLE TO SAVE KEYS                           
         USING PTTENT,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
TABINILP DS    0H                                                               
*                                                                               
         XC    PTTENT(PTTENTL),PTTENT INIT TABLE ENTRY                          
*                                                                               
         L     R1,=A(CARD)                                                      
         GET   (1),QAREA           READ IN REQUEST CARD                         
*                                                                               
         AHI   R2,1                BUMP RECORD COUNTER                          
*                                                                               
         GOTO1 VHEXIN,DMCB,QAREA+39,WRKKEY,26,0   CONVERT TO HEX                
*                                                                               
         CLI   DMCB+15,0           NO ERRORS TOLERATED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WRKKEY+10(1),WRKKEY+11  MOVE OVER LINE NUMBER                    
         MVC   PTTKEY,WRKKEY       SAVE NEW KEY                                 
*                                                                               
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  P+4(4),DUB                                                       
*                                                                               
         MVC   P+12(26),QAREA+39                                                
         MVC   P+52(11),PTTKEY                                                  
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
TABINICN DS    0H                                                               
*                                                                               
         LA    R4,PTTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TABINILP                                                         
*                                                                               
TABINIDN DS    0H                                                               
*                                                                               
         ST    R2,PTTKEY#          SAVE RECORD COUNTER                          
*                                                                               
         MVI   0(R4),X'FF'         SET END OF TABLE                             
*                                                                               
         CLOSE CARD                CLOSE INPUT FILE                             
*                                                                               
         L     R4,=A(PTTAB)                                                     
         L     R0,PTTKEY#          NUMBER OF RECORDS TO SORT                    
*                                                                               
         GOTO1 VQSORT,DMCB,(R4),(R0),11,11,0  SORT TABLE                        
*                                                                               
         L     R0,PTTKEY#          NUMBER OF KEYS                               
*                                                                               
         MVC   MID1(60),=CL60'RECORDS AS SORTED'                                
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         SR    R2,R2               RECORD COUNTER                               
TABPRTLP DS    0H                  PRINT SORTED TABLE                           
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  P+4(4),DUB                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,PTTKEY,P+10,11,0,0  PRINT KEY                       
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
TABPRTCN DS    0H                                                               
*                                                                               
         LA    R4,PTTENTL(R4)      NEXT ENTRY IN TABLE                          
         BCT   R0,TABPRTLP                                                      
*                                                                               
TABINITX DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,0           INIT END OF FILE SWITCH                      
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))    OPEN OUTPUT FILE                           
*                                                                               
         L     R4,=A(PTTAB)        POINT TO START  OF KEY TABLE                 
         ST    R4,CURRECA          A(CURRENT TABLE ENTRY)                       
*                                                                               
         MVC   MID1(60),=CL60'RECORDS FROM TAPE'                                
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
         TITLE 'SPLDEXTPT - FIND LOST RECORDS - DMXREC'                         
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         SR    R6,R6               INIT ELEMENT POINTER                         
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
         L     R4,CURRECA          POINT TO CURRENT KEY IN TABLE                
         USING PTTENT,R4                                                        
*                                                                               
         NOP   DMXREC1                                                          
         MVI   *-3,X'F0'                                                        
         MVC   P+10(10),=CL10'FIRST REC'                                        
         GOTO1 VHEXOUT,DMCB,0(R3),P+30,13,0,0                                   
         GOTO1 VHEXOUT,DMCB,0(R4),P+60,11,0,0                                   
         GOTO1 VPRINTER                                                         
DMXREC1  DS    0H                                                               
*                                                                               
*        RESTORE RECORDS                                                        
*                                                                               
RESKEYLP DS    0H                                                               
*                                                                               
         CLC   0(11,R3),PTTKEY     SKIP IF TABLE NOT REACHED                    
         BL    RESTOREX                                                         
*                                                                               
         BE    RESKEYFD            KEEP IF KEYS MATCH                           
*                                                                               
RESKEYCN DS    0H                                                               
*                                                                               
         LA    R4,PTTENTL(R4)      BUMP TO NEXT KEY                             
         CLI   0(R4),X'FF'         CONTINUE IF NOT EOT                          
         BNE   RESKEYLP                                                         
*                                                                               
RESKEYDN DS    0H                  TABLE EXHAUSTED                              
*                                                                               
         NOP   DMXREC9                                                          
         MVI   *-3,X'F0'                                                        
         MVC   P+10(10),=CL10'LAST REC'                                         
         GOTO1 VHEXOUT,DMCB,0(R3),P+30,13,0,0                                   
         GOTO1 VHEXOUT,DMCB,0(R4),P+60,11,0,0                                   
         GOTO1 VPRINTER                                                         
DMXREC9  DS    0H                                                               
         B     DMXPGEOF            ELSE STOP RUN                                
*                                                                               
RESKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD                                  
*                                                                               
         L     R5,=A(NEWREC)       POINT TO COPY                                
         NI    15(R5),X'FF'-X'80'  TURN OFF DELETE BIT                          
*                                                                               
         BRAS  RE,WRITE            WRITE TO OUTPUT DATASET                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R5),P+10,16,0,0  PRINT KEY                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R4),P+80,11,0,0  PRINT KEY                         
*                                                                               
         GOTO1 VPRINTER            PRINT KEYS                                   
*                                                                               
RESTOREX DS    0H                                                               
*                                                                               
         ST    R4,CURRECA          SAVE A(CURRENT RECORD)                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
RECCTR   DC    PL2'100'            RECORD COUNTER                               
*                                                                               
         TITLE 'STLDEXTCZ  - FIND LOST RECORDS - COPY'                          
***********************************************************************         
*                                                                     *         
*        COPY CURRENT RECORD TO WORKAREA                              *         
*                                                                     *         
*NTRY    R3==> RECORD TO BE COPIED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COPY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R0,R3               POINT TO INCOMING RECORD                     
         L     RE,=A(NEWREC)       POINT TO NEW RECORD                          
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
         LR    RF,R1               COPY LENGTH                                  
*                                                                               
         AHI   RF,4                ALLOW FOR LENGTH BYTES                       
         AHI   RE,-4               BACK UP TO RRLEN FIELD                       
         STCM  RF,3,0(RE)          SET OUTPUT FILE LENGTH                       
         AHI   RE,4                REPOINT TO RECORD                            
         AHI   RF,-4               RESTORE TRUE RECORD LENGTH                   
*                                                                               
         MVCL  RE,R0               COPY INCOMING RECORD                         
*                                                                               
         L     R3,=A(NEWREC)                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
*                                                                               
         LA    RF,0(R1,R3)         POINT TO END OF NEW RECORD                   
         MVI   0(RF),0             FORCE ENDING NULLS                           
*                                                                               
COPYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - FIND LOST RECORDS - WRITE'                         
***********************************************************************         
*                                                                     *         
*        WRITE WORKAREA RECORD TO OUTPUT DATASET                      *         
*                                                                     *         
*NTRY    NEWREC HAS RECORD TO GO                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WRITE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         L     R2,=A(RRLEN)        POINT TO RECORD                              
*                                                                               
         CLC   SAVEKEY,4(R2)       SKIP IF DUPLICATE RECORD                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY,4(R2)       SAVE CURRENT KEY                             
*                                                                               
         PUT   (1),(R2)            WRITE TO OUTPUT FILE                         
*                                                                               
         AP    RECCNTR,=P'1'       COUNT NEW RECORDS                            
*                                                                               
WRITEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - FIND LOST RECORDS - WORKD'                         
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL128                                                            
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
SAVEKEY  DC    XL13'00'            LAST KEY WRITTEN                             
WRKKEY   DC    XL13'00'            KEY WORKAREA                                 
*                                                                               
BYTE     DS    X                                                                
RECCNTR  DS    PL8                 RECORD COUNTER                               
*                                                                               
DUB      DS    D                                                                
HALF     DS    H                                                                
EOFTOSW  DC    X'00'               END OF FILE SWITCH FOR TO FILE               
*                                                                               
CURRECA  DS    A                   A(CURRENT ENTRY IN TABLE)                    
PTTKEY#  DS    F                   NUMBER OF KEYS IN TABLE                      
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
PLIST    DS    0CL24               PARAMETER LIST - SAVED                       
PRMMODE  DS    0XL1                CALLING MODE                                 
PRMMINIQ EQU   X'00'                 X'00'= INITIALISE                          
PRMMRECQ EQU   X'01'                 X'01'= RECORD IN CORE                      
PRMMEOFQ EQU   X'FF'                 X'FF'= END OF FILE                         
*                                                                               
PRMRTNCD DS    0XL1                RETURN CODE                                  
PRMRKPQ  EQU   X'00'               X'00'= KEEP RECORD                           
PRMRPRGQ EQU   X'FF'               X'FF'= PURGE RECORD                          
PRMREOJQ EQU   X'FF'               X'FF'/C'EOJ'=PURGE & CAUSE EOJ               
*                                                                               
AREC     DS    A                   A(CURRENT RECORD)                            
*                                                                               
VTAPEOUT DS    A                   V(TAPEOUT DCB)                               
APARAMC  DS    A                   A(PARAMETER CARD)                            
VLDDEFN  DS    A                   A(FILE DEFINITION)                           
VPRINTER DS    A                   V(PRINTER)                                   
VCPRINT  DS    A                   V(CPRINT)                                    
VHEXOUT  DS    A                   V(HEXOUT)                                    
VHEXIN   DS    A                   V(HEXIN)                                     
VQSORT   DS    A                   V(QSORT)                                     
*                                                                               
         EJECT                                                                  
         DS    D                                                                
QRECORD  DS    0CL80                                                            
QAREA    DS    0CL80   COLUMN                                                   
QPROG    DS    0CL2    ------                                                   
QCODE    DS    CL2        1        PROGRAM CODE                                 
QAGY     DS    CL2        3        AGENCY CODE                                  
QMED     DS    CL1        5        MEDIA CODE (R/T)                             
QCLT     DS    CL3        6        CLIENT CODE                                  
QPGR     DS    CL1        9        PROCESS BY DIVISION                          
QMGR     DS    CL1       10        PROCESS BY DISTRICT                          
QCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
QBYID    EQU   QCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
QPRD     DS    CL3       12        PRODUCT MNEMONIC                             
QMKT     DS    CL4       15        MARKET NUMBER                                
QSTA     DS    CL5       19        STATION CALL LETTERS                         
QEST     DS    CL3       24        ESTIMATE NUMBER                              
QESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
QDEMOVRD DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
QCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
QSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
QENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   QSTAUTO+2                                                        
QDEMNOS  DS    CL4                 DEMO OVERRIDE NUMBERS                        
QSTART   DS    CL6       38        REQUEST START DATE                           
QEND     DS    0CL6      44        REQUEST END DATE                             
QTODAY   DS    CL6       44                                                     
QBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
QHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
QRERATE  DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
QCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
QAFFIL   DS    CL1       58        AFFILIATION FILTER                           
QPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
QDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
QDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
QOPT1    DS    CL1       62        OPTION 1                                     
QOPT2    DS    CL1       63        OPTION 2                                     
QOPT3    DS    CL1       64        OPTION 3                                     
QOPT4    DS    CL1       65        OPTION 4                                     
QOPT5    DS    CL1       66        OPTION 5                                     
QGRP     DS    CL2       67        GROUP                                        
QFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
QUESTOR  DS    CL12      69        REQUESTOR NAME                               
         ORG   QUESTOR                                                          
QNWAGY   DS    CL2                 NEW AGENCY POWER CODE                        
QNWCLT   DS    CL3                 NEW CLIENT CODE                              
         DS    CL3                 SPARE                                        
QNWEST   DS    CL3                 NEW ESTIMATE CODE                            
         DS    CL1                 SPARE                                        
*                                                                               
         EJECT                                                                  
         ORG   QAREA+57                                                         
QCMRCL   DS    CL8       58        COMMERCIAL FILTER                            
         ORG   QAREA+29                                                         
QREP     DS    CL3       30        DISCREPANCY REP                              
QREPTYPE DS    CL1       33                                                     
QTIME    DS    CL2       34                                                     
QCONT    DS    CL1       36                                                     
         SPACE 2                                                                
         ORG   QAREA+49                                                         
QPRD2    DS    CL3       50        PRODUCT 2                                    
QPRD3    DS    CL3       53        PRODUCT 3                                    
QAMTTYPE DS    CL1       56        AMOUNT TYPE                                  
QAMT     DS    CL10      57        AMOUNT                                       
         DS    CL2       67                                                     
QIND     DS    CL12      69        INVOICE NUMBER                               
         SPACE 2                                                                
         TITLE 'STLDEXTMVC - FIND LOST RECORDS - PTTAB'                         
***********************************************************************         
*                                                                     *         
*        DATA FOR THIS CONVERSION                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
         DS    0D                                                               
         DC    C'**PTTAB*'                                                      
PTTAB    DS    0X                  TABLE ENTRY                                  
*                                                                               
         DS    1500XL(PTTENTL)     TRANSFER TABLE                               
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
CARD     DCB   DDNAME=CARDIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=TABINIDN                                                   
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
         TITLE 'STLDEXTMVC - FIND LOST RECORDS - BSRPRMD'                       
***********************************************************************         
*                                                                     *         
*        DSECT FOR BINSRCH PARAMETERES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BSRPRMD  DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
         TITLE 'STLDEXTMVC - FIND LOST RECORDS - PTTABD'                        
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DATA TO MOVE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PTTABD   DSECT                                                                  
PTTENT   DS    0X                  ENTRY IN TABLE                               
PTTKEY   DS    XL11                SPOTFILE KEY                                 
PTTENTL  EQU   *-PTTENT            TABLE ENTRY LENGTH                           
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         EJECT                                                                  
*SPGENBUY                                                                       
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENCLT                                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENEST                                                                       
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENGOAL                                                                      
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097SPLDEXTPT 09/28/00'                                      
         END                                                                    
*                                                                               
