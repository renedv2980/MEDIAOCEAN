*          DATA SET REREP2002  AT LEVEL 048 AS OF 05/01/02                      
*PHASE RE2002C,*                                                                
         TITLE 'REREP2002 (RE2002) --- DISCREPANCY REPORT'                      
*********************************************************************           
*                                                                   *           
*        REREP2002 --- REPPACK DISCREPANCY REPORT                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* MAR07/89 (MRR) --- ADD PRODUCT CODE TO THE REPORT IF IT EXISTS AND*           
*                     SHOW THE CONTRACT TYPE.                       *           
*                                                                   *           
* NOV01/89 (MRR) --- HEADLINE DRESS-UP, CHANGE CONTRACT READER ORDER*           
*                     TO STATION HIGH                               *           
*                                                                   *           
* AUG06/90 (BU ) --- INSTALL OPTION TO SELECT ONLY CONTRACTS WITH   *           
*                    ZERO ORDERED $/NON-ZERO INVOICE $ + VICE VERSA *           
*                                                                   *           
* NOV15/91 (BU ) --- INSTALL NEW VALUENEW ADDRESSING                *           
*                                                                   *           
* FEB21/92 (MRR) --- >SUPPORT ACCOUNTING OPTION                     *           
*                                                                   *           
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH NEW VALU2NEW              *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* FEB03/93 (BU ) --- SKIP DETAIL LINE PRINT WHEN NO DATA IS FOUND.  *           
*                                                                   *           
* MAR08/94 (BU ) --- ADD INTERFACE CODE REPORT SEQUENCE             *           
*                                                                   *           
* APR13/94 (BU ) --- SKIP GRP/SUBGRP TOTALS IF INTERFACE REPORT ONLY*           
*                                                                   *           
* APR29/94 (BU ) --- ADD OFFICE ORDER HIGH TO 'I' SEQUENCE KEY      *           
*                                                                   *           
* JUL25/94 (BU ) --- SKIP FORECAST CONTRACTS                        *           
*                                                                   *           
* AUG24/94 (BU ) --- ADD 'INVTYPE' TO TSARLINE OUTPUT               *           
*                                                                   *           
* AUG30/94 (BU ) --- ADD INVOICE NUMBER TO DISPLAY                  *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* OCT23/96 (BU ) --- CHANGE ABORT IF NO X'08' ELEMENT TO SKIP       *           
*                    DISPLAY CODE IF 'COMPETING' STATION.           *           
*                                                                   *           
* JAN22/97 (DBU) --- PRINT STATION/REPORT TOTALS AS POSITIVE NUMBERS*           
*                    IF THEY EXCEED THE CAPACITY OF A FULL WORD     *           
*                                                                   *           
* JAN24/97 (DBU) --- ADD TRAFFIC NUMBER FIELD                       *           
*                                                                   *           
* MAY21/97 (BU ) --- UPGRADE FOR YEAR 2000                          *           
*                                                                   *           
* DEC29/97 (BU ) --- ADD NEW EXCLUSION LIMIT OF $5K                 *           
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* FEB10/98 (BU ) --- CONSIDER NEGATIVE DIFFERENCES                  *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE2002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2002,R5,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         STM   R2,RC,SAVEREGS                                                   
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(STALAST),AL3(STADONE)    STATION BREAK                       
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         LA    R2,STATOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,OVERTOTS                                                      
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,GRPTOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,INTTOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         MVI   DOREP,1             SET REPORT TO '1'                            
         XC    REP#1(2),REP#1      CLEAR REPORTS TO BE DONE                     
         CLI   QOPTION3,C'B'       BOTH REPORTS TO BE DONE?                     
         BNE   INIT0004            NO                                           
         MVI   REP#1,1             YES - TURN ON BOTH REPORTS                   
         MVI   REP#2,2                                                          
         B     INIT0016                                                         
INIT0004 EQU   *                                                                
         CLI   QOPTION3,C'I'       INTERFACE REPORT TO BE DONE?                 
         BNE   INIT0006            NO                                           
         MVI   REP#2,2             YES - TURN ON INTERFACE REPORT               
         B     INIT0016                                                         
INIT0006 EQU   *                                                                
*                                  ANYTHING ELSE GETS STATION REPORT            
         MVI   REP#1,1                                                          
INIT0016 EQU   *                                                                
         MVC   MYHED4,SPACES                                                    
         MVI   RCSUBPRG,0                                                       
         CLC   QEND,SPACES         CHECK FOR 1 MONTH REQUEST                    
         BNE   INIT0020                                                         
         MVI   RCSUBPRG,1                                                       
INIT0020 MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   COUNTER,1                                                        
         LA    RF,HHOOK                                                         
         ST    RF,HEADHOOK                                                      
         L     R1,ATSARDWA         A(TSAR CONTROL BOLOCK AREA)                  
         USING TSARD,R1                                                         
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,LBFKEY           SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,LBUFFREC         SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF                                                         
         MVC   SVTSRBLK,0(R1)      SAVE THE TSAR BLOCK                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         B     MODEEXIT                                                         
*                                                                               
       ++INCLUDE REREPTSAR                                                      
         EJECT                                                                  
*   STAINIT: FIRST STATION MODE SETTING                                         
*                                                                               
STAINIT  NTR1                                                                   
         MVI   STATACTV,C'N'       SET STATION=NOT ACTIVE                       
         MVC   MYHED,SPACES        CLEAR THE ALTERNATE FIELD                    
         MVC   MYHED+20(20),RSTAMKT                                             
         MVC   MYHED+90(41),SPACES                                              
         CLI   QOPTIONS,C'D'                                                    
         BNE   STIN0020                                                         
         MVC   HEAD4+90(29),=C'DETAILS SHOW DIFFERENCES ONLY'                   
         B     STIN0080                                                         
STIN0020 LA    R3,OPT2TAB          OPTION TO SUPRESS MIN. AMT DISCREPS.         
STIN0040 CLI   0(R3),X'FF'                                                      
         BE    STIN0080                                                         
         CLC   0(1,R3),QOPTIONS                                                 
         BE    STIN0060                                                         
         LA    R3,L'OPT2TAB(R3)                                                 
         B     STIN0040                                                         
         SPACE 1                                                                
STIN0060 MVC   HEAD4+90(33),=C'DETAILS EXCLUDE DIFFERENCES UNDER'               
         LA    R2,MYHED+124                                                     
         EDIT  (4,1(R3)),(6,0(R2)),FLOAT=$,ALIGN=LEFT                           
STIN0080 B     MODEEXIT                                                         
         EJECT                                                                  
*   STADONE:  LAST STATION MODE SETTING                                         
*                                                                               
STADONE  NTR1                                                                   
         CLI   STATACTV,C'N'       STATION=NOT ACTIVE?                          
         BE    STAD0020            YES - DON'T PRINT ANYTHING                   
**>>>    OC    STATOTS,STATOTS                                                  
**>>>    BZ    STAD0020                                                         
         MVI   SPACING,1                                                        
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BAS   RE,PRINTIT                                                       
         MVC   P+52(16),=C'*STATION TOTALS*'                                    
         LA    R2,STATOTS                                                       
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
STAD0020 MVI   FORCEHED,C'Y'                                                    
         MVI   COUNTER,0                                                        
         B     MODEEXIT                                                         
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING                                         
*                                                                               
RPTDONE  NTR1                                                                   
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,2                                                        
         CLI   DATAFLAG,C'Y'       ANY DATA FOUND?                              
         BNE   RPTD0020            NO  - DON'T PRINT ANYTHING                   
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          DON'T NEED ALL THE HEADLINES                 
         MVC   P+50(19),=C'***REPORT TOTALS***'                                 
         MVC   MYHED(132),SPACES                                                
         LA    R2,OVERTOTS                                                      
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
         B     RPTD0040                                                         
RPTD0020 EQU   *                   NO DATA FOUND: PUT OUT TOTAL SHEET           
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          DON'T NEED ALL THE HEADLINES                 
         MVC   P+50(19),=C'***NO DATA FOUND***'                                 
         MVC   MYHED(132),SPACES                                                
         BAS   RE,PRINTIT                                                       
RPTD0040 EQU   *                                                                
         BAS   RE,TSARBACK                                                      
         B     MODEEXIT                                                         
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
         BAS   RE,CHKFCST          CHECK FOR FORECAST ORDER                     
         BNZ   MODEEXIT            FCST ORDER: SKIP IT                          
         CLI   FCGTAGY,C'Y'        AGENCY INFO REQUESTED?                       
         BE    POST0240                                                         
         CLI   FCGTSTAT,C'Y'       STATION RECORD RETRIEVED?                    
         BE    POST0010            YES - SET HEADINGS                           
         CLC   RCONKSTA,OLDSTAT    NO  - SAME STATION IN PROGRESS?              
         BE    POST0018            YES - SKIP HEADING SET                       
         MVI   FCGTSTAT,C'Y'       NO  - NEED WHOLE STATION RECORD              
         MVC   OLDSTAT,RCONKSTA SAVE NEW STATION CALLS                          
         MVI   MODE,0              GO BACK AND GET IT                           
         B     MODEEXIT                                                         
POST0010 EQU   *                                                                
         MVI   FCGTSTAT,C'N'       TURN OFF FORCE STATION GET                   
         MVC   MYHED+90(15),=C'INTERFACE CODE='                                 
         LA    RE,RSTAELEM         FIND EXTENDED DESCRIP ELT                    
POST0015 EQU   *                                                                
         ZIC   RF,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,RF                                                            
         CLI   0(RE),0             END OF RECORD REACHED?                       
         BNE   POST0017            NO                                           
         CLI   RSTAGRUP,C'C'       COMPETING STATION?                           
         BE    POST0018            YES - SKIP ABORT: NO INTERFACE CODE          
         DC    H'0'                YES - SHOULDN'T HAVE BEEN                    
POST0017 EQU   *                                                                
         CLI   0(RE),8             PROPER ELEMENT?                              
         BNE   POST0015            NO  - GO BACK FOR NEXT                       
         MVC   MYHED+105(10),RSTAOSI-RSTAXXEL(RE)                               
*                                  INSERT INTERFACE CODE                        
POST0018 EQU   *                                                                
*                                  TEST ANY ACTIVITY ON CONTRACT                
         MVI   ACTIVE,C'Y'                                                      
         MVI   ORDFLAG,0            INITIALIZE OPTIONAL FLAG FIELDS             
         MVI   INVFLAG,0                                                        
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
*                                                                               
POST0020 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0040            FOUND - BEGIN TO PULL FIGURES                
         BH    POST0140            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0020            GO BACK FOR NEXT                             
*                                                                               
POST0040 CLI   0(R4),0             ANY MORE MONTHS?                             
         BE    POST0140                                                         
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST0140            TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
*                                                                               
*  IF BEST $ OPTION, EACH SET OF BUCKETS MUST BE CHECKED FULLY,                 
*     FOR BOTH ORDERED AND INVOICED $.                                          
*                                                                               
         CLI   QOPTION2,C'Z'       BEST $ OPTION?                               
         BNE   POST0080            NO  - REGULAR PROCESSING                     
         OC    TOTORD(4,R6),TOTORD(R6)   ORDERED IN BUCKET?                     
         BZ    POST0060            NO - CHECK INVOICED                          
         MVI   ORDFLAG,1           YES - SET ORDERED FLAG                       
POST0060 OC    CUASATIN(4,R6),CUASATIN(R6)     ANY INVOICED?                    
         BZ    POST0120            NO  - CHECK ALL BUCKETS                      
         MVI   INVFLAG,1           YES - SET INVOICED FLAG                      
         B     POST0120            CHECK ALL BUCKETS                            
*                                                                               
*   REGULAR PROCESSING ONLY REQUIRES THAT $ BE FOUND IN >>EITHER<<              
*       ORDERED OR INVOICED BUCKET.                                             
*                                                                               
POST0080 EQU   *                                                                
         CLI   QACCTOPT,C'T'                 ACCT OPT WITH PENNIES              
         BNE   POST0100                                                         
         OC    PRASATOR(4,R6),PRASATOR(R6)   ANY DOLLARS ACTIVE?                
         BNZ   POST0220                                                         
         B     POST0120                                                         
POST0100 EQU   *                                                                
         OC    TOTORD(4,R6),TOTORD(R6)       ANY ORDERED?                       
         BNZ   POST0220                                                         
         OC    CUASATIN(4,R6),CUASATIN(R6)   ANY INVOICES?                      
         BNZ   POST0220                                                         
POST0120 LA    R4,NEXTBUCK(R4)                                                  
         B     POST0040            GO BACK FOR NEXT                             
         SPACE 1                                                                
*   'BEST $' CONTRACTS:                                                         
*              CONTRACT MAY BE PROCESSED IF THE REQUEST IS FOR                  
*              BEST-DOLLAR CONTRACTS,                                           
*              IF CONTRACT ORDERED $ = ZERO/INVOICE $ NOT = ZERO                
*                              OR VICE VERSA                                    
*                   AND                                                         
*              IF CONTRACT IS WITHIN C REQUEST START AND END DATE               
*              IT MIGHT APPLY SO PRINT IT                                       
POST0140 EQU   *                                                                
         SPACE 1                                                                
         CLI   QOPTION2,C'Z'       ZERO BEST-$ OPTION?                          
         BNE   POST0160                                                         
         CLC   ORDFLAG(2),ORDINV   FLAGS = X'0100'?                             
         BE    POST0220            YES - PROCESS THIS CONTRACT FURTHER          
         CLC   ORDFLAG(2),INVORD   FLAGS = X'0001'?                             
         BE    POST0220            YES - PROCESS THIS CONTRACT FURTHER          
         B     MODEEXIT            NO  - DROP THIS CONTRACT                     
POST0160 GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         MVC   WORK+6(6),WORK+12                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
         GOTO1 GETBROAD,DMCB,WORK,WORK+12                                       
         CLC   QSTART(4),WORK+18                                                
         BH    MODEEXIT                                                         
         LA    R3,QEND                                                          
         CLC   QEND,SPACES                                                      
         BNE   POST0180                                                         
         LA    R3,QSTART                                                        
POST0180 CLC   0(4,R3),WORK+6                                                   
         BL    MODEEXIT                                                         
POST0200 MVI   ACTIVE,C'N'                                                      
         B     POST0220                                                         
*                                                                               
POST0220 EQU   *                                                                
         MVI   MODE,0                                                           
         MVI   FCGTAGY,C'Y'                                                     
         MVI   FCGTADV,C'Y'                                                     
         MVI   FCGTPROD,C'Y'                                                    
         B     MODEEXIT                                                         
*                                                                               
POST0240 EQU   *                                                                
         MVI   FCGTAGY,C'N'                                                     
         MVI   FCGTADV,C'N'                                                     
         MVI   FCGTPROD,C'N'                                                    
         BAS   RE,POSTTR#          CHECK FOR TRFI #                             
*                                                                               
         MVC   P+22(4),RCONKADV    LOAD ADVERTISER CODE AND NAME                
         MVC   PSECOND+22(20),RADVNAME                                          
*                                                                               
         MVC   P+44(3),RCONPRD     LOAD PRODUCT CODE AND NAME                   
         MVC   PSECOND+44(20),RPRDNAME                                          
*                                                                               
         MVC   P(4),RAGYKAGY       LOAD AGENCY+CITY AND NAME                    
         LA    R1,P+3                                                           
         BAS   RE,FLOAT                                                         
         CLC   RAGYKAOF,SPACES                                                  
         BE    POST0260                                                         
         MVI   0(R1),C'-'                                                       
         MVC   1(2,R1),RAGYKAOF                                                 
POST0260 EQU   *                                                                
         MVC   PSECOND(20),RAGYNAM1                                             
*                                                                               
         MVC   P+66(2),RCONKOFF    LOAD REP OFFICE AND SALESPERSON              
         MVC   PSECOND+66(3),RCONSAL                                            
*                                                                               
         UNPK  P+70(8),RCONKCON(5) LOAD CONTRACT NUMBER AND TYPE                
         MVI   P+77,C' '                                                        
         LA    R1,P+70                                                          
POST0280 CLI   0(R1),C'0'                                                       
         BNE   POST0300                                                         
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         B     POST0280                                                         
POST0300 EQU   *                                                                
         CLC   =C'ACC',RCONBUYR                                                 
         BNE   POST0320                                                         
         MVI   P+77,C'*'                                                        
         B     POST0340                                                         
POST0320 EQU   *                                                                
         MVC   P+77(1),RCONTYPE                                                 
POST0340 EQU   *                                                                
         EJECT                                                                  
*              NOW LOOP THROUGH THE MONTH TABLE                                 
         SPACE 3                                                                
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
*                                                                               
POST0360 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0380            FOUND - BEGIN TO PULL FIGURES                
         BH    POST0780            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0360            GO BACK FOR NEXT                             
*                                                                               
POST0380 CLI   0(R4),0             ANY MORE MONTHS?                             
         BE    POST0780                                                         
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST0780            TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         SPACE 2                                                                
         CLI   RCSUBPRG,1          SUPPRESS MONTH IF ONLY ONE REQUESTED         
         BE    POST0400                                                         
         GOTO1 DATCON,DMCB,(0,(R4)),(6,P+79)                                    
         MVC   P+82(2),P+83                                                     
         MVI   P+84,C' '                                                        
POST0400 EQU   *                                                                
         BAS   RE,POSTINV#         CHECK FOR/DISP INV #                         
         CLI   QACCTOPT,C'T'       ACCT OPT WITH PENNIES                        
         BNE   POST0460                                                         
         XC    CONTOTS(8),CONTOTS                                               
         TM    FLAG6(R4),X'01'                                                  
         BO    POST0420                                                         
         MVC   CONTOTS(4),PRASATOR(R6)                                          
         B     POST0440                                                         
POST0420 EQU   *                                                                
         MVC   CONTOTS+4(4),PRASATOR(R6)                                        
POST0440 EQU   *                                                                
         OC    CONTOTS(8),CONTOTS                                               
         BZ    POST0760                                                         
         L     R3,CONTOTS                                                       
         A     R3,CONTOTS+4                                                     
         ST    R3,CONTOTS+8                                                     
         B     POST0500                                                         
POST0460 EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)                                            
         MVC   CONTOTS+4(4),CUASATIN(R6)                                        
POST0480 EQU   *                                                                
         MVC   INVBYTE,FLAG6(R4)   SET CURRENT AS AT INVOICE BYTE               
         L     R3,CONTOTS                                                       
         S     R3,CONTOTS+4                                                     
         ST    R3,CONTOTS+8                                                     
POST0500 EQU   *                                                                
         LA    R2,CONTOTS          ADD DOWN                                     
         LA    R6,STATOTS                                                       
         LA    R7,OVERTOTS                                                      
         LA    R3,3                                                             
         SPACE 2                                                                
POST0520 EQU   *                                                                
         L     R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         AP    0(8,R6),DUB         TO STATION TOTALS                            
         AP    0(8,R7),DUB         TO OVERALL TOTALS                            
         LA    R6,8(R6)                                                         
         LA    R7,8(R7)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,POST0520                                                      
         SPACE 1                                                                
         LA    R2,CONTOTS                                                       
         OC    CONTOTS+8(4),CONTOTS+8                                           
         BNZ   POST0540                                                         
         CLI   QOPTIONS,C'D'       OPTION TO SUPPRESS ZERO DISCREPS.            
         BE    POST0640                                                         
         CLI   QOPTIONS,C' '       ANY OPTIONS                                  
         BE    POST0660            NO, BUT 0 DIFF-CHECK IF ACC CONTRACT         
POST0540 LA    RE,OPT2TAB          OPTION TO SUPRESS MIN. AMT DISCREPS.         
POST0560 CLI   0(RE),X'FF'                                                      
         BE    POST0700                                                         
         CLC   0(1,RE),QOPTIONS                                                 
         BE    POST0580                                                         
         LA    RE,L'OPT2TAB(RE)                                                 
         B     POST0560                                                         
         SPACE 1                                                                
POST0580 L     R1,CONTOTS+8        DISCREPANCY AMOUNT                           
         L     RF,1(RE)            MINIMUM AMOUNT IN DOLLARS                    
         CLI   QACCTOPT,C'P'                                                    
         BE    POST0600                                                         
         CLI   QACCTOPT,C'T'                                                    
         BNE   POST0620                                                         
POST0600 L     RF,5(RE)            MINIMUM AMOUNT IN PENNIES                    
POST0620 EQU   *                                                                
         LPR   R1,R1               LOAD THE POSITIVE VALUE OF DIFF              
         CR    RF,R1                                                            
         BH    POST0640                                                         
         B     POST0700                                                         
         SPACE 1                                                                
POST0640 XC    CONTOTS,CONTOTS                                                  
         B     POST0760                                                         
         SPACE 2                                                                
POST0660 LA    R2,CONTOTS                                                       
         CLC   =C'ACC',RCONBUYR                                                 
         BNE   POST0680                                                         
         GOTO1 DATCON,DMCB,0(R4),(3,WORK)                                       
         CLC   WORK(2),RCONDATE                                                 
         BL    POST0680                                                         
         CLC   WORK(2),RCONDATE+3                                               
         BNH   POST0700                                                         
*                                                                               
POST0680 OC    0(12,R2),0(R2)                                                   
         BNZ   POST0700                                                         
         CLI   ACTIVE,C'N'                                                      
         BNE   POST0760                                                         
         SPACE 1                                                                
POST0700 EQU   *                                                                
         MVC   SAVTOTS,CONTOTS                                                  
         XC    MYFLAG,MYFLAG                                                    
         BAS   RE,FORMAT                                                        
*                                                                               
         MVC   PSECOND+85(47),DASHES   PRINT DASHES ON 2ND PRINT LINE           
         CLC   PTHIRD,SPACES           BUT IF THERE IS A TRF#                   
         BE    POST0710                PRINT DASHES ON 3RD PRINT LINE           
         MVC   PTHIRD+85(47),DASHES                                             
         MVC   PSECOND+85(47),SPACES                                            
         MVI   PSECOND+85,X'00'                                                 
POST0710 EQU   *                                                                
         CLI   RCSUBPRG,1          IF SINGLE MONTH REQUESTED                    
         BNE   POST0720                                                         
         SR    R1,R1               PRINT 20 ON A PAGE                           
         IC    R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
         CLI   COUNTER,21                                                       
         BL    POST0720                                                         
         MVI   COUNTER,1                                                        
         MVI   FORCEHED,C'Y'                                                    
POST0720 CLI   LINE,57             KEEP PRINT LINES TOGETHER                    
         BL    POST0740                                                         
         MVI   FORCEHED,C'Y'                                                    
POST0740 EQU   *                                                                
         BAS   RE,TSARLINE         PUT OUT TSAR INFO                            
         BAS   RE,PRINTIT                                                       
         MVI   DATAFLAG,C'Y'       SET DATA FOUND FLAG                          
         MVI   STATACTV,C'Y'       SET STATION=ACTIVE                           
         SPACE 2                                                                
POST0760 LA    R4,NEXTBUCK(R4)                                                  
         B     POST0380            GO BACK FOR NEXT                             
POST0780 MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         SPACE 2                                                                
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   POSTINV#:  CHECK FOR X'18' ELEMENT.  IF NOT FOUND, DO NOTHING.              
*      IF FOUND, SCAN INVOICE ELEMENTS WITHIN LOOKING FOR DATE MATCH            
*        FOR INVOICE MONTH.  IF FOUND, INSERT THE INVOICE NUMBER                
*        INTO THE SECOND PRINTLINE, UNDER THE CONTRACT NUMBER                   
*   R4 POINTS TO INVOICE DATE.                                                  
*                                                                               
POSTINV# NTR1                                                                   
         MVC   WORK(4),0(R4)       LOAD DATE                                    
         MVC   WORK+4(2),=C'01'    STUFF DAY                                    
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
*                                  CONVERT DATE TO BINARY FORMAT                
         LA    R2,RCONELEM         SET A(DESCRIPT ELEMENT)                      
PINV0020 EQU   *                                                                
         CLI   0(R2),X'0'          END OF RECORD?                               
         BE    PINV0100            YES - RETURN CC = ZERO                       
         CLI   0(R2),X'18'         INV ELEMENT?                                 
         BE    PINV0040            YES - CHECK IT OUT                           
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     PINV0020            GO BACK FOR NEXT                             
PINV0040 EQU   *                                                                
         USING RCONDVEL,R2         NEW LENGTH:  CHECK FOR FCST                  
         ZIC   R3,RCONDVI#         LOAD # INV ELEMENTS                          
         LTR   R3,R3               ANY ELEMENTS?                                
         BZ    PINV0100            NO  - FINISHED                               
         LA    R2,RCONDVIL         SET A(1ST INV ELT LEN)                       
*                                                                               
         DROP  R2                                                               
*                                                                               
PINV0060 EQU   *                                                                
         CLC   1(2,R2),WORK+6      INVOICE DATE FOUND                           
         BE    PINV0080            YES - DISPLAY INVOICE NUMBER                 
         ZIC   RF,0(R2)            NO  - GET INV ELT LENGTH                     
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         BCT   R3,PINV0060         LOOP THROUGH ALL ELEMENTS                    
         B     PINV0100            NOT FOUND - EXIT                             
PINV0080 EQU   *                                                                
         LA    R3,3(R2)            SET A(INVOICE NUMBER IN ELT)                 
         ZIC   RF,0(R2)            SET L(ENTIRE ELEMENT)                        
         SH    RF,=H'4'            SUBTRACT L(CONTROL) + 1 FOR EX               
         EX    RF,PINV0120         MOVE INV# BY LENGTH                          
PINV0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
PINV0120 MVC   PSECOND+70(0),0(R3)                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*   POSTINV#:  CHECK FOR X'18' ELEMENT.  IF NOT FOUND, DO NOTHING.              
*   IF FOUND, INSERT THE TRAFFIC NUMBER INTO THIRD PRINTLINE                    
*                                                                               
POSTTR#  NTR1                                                                   
         LA    R2,RCONELEM         SET A(DESCRIPT ELEMENT)                      
PTRF0020 EQU   *                                                                
         CLI   0(R2),X'0'          END OF RECORD?                               
         BE    PTRFEX              YES - RETURN CC = ZERO                       
         CLI   0(R2),X'1F'         TRF ELEMENT?                                 
         BE    PTRF0040            YES - CHECK IT OUT                           
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     PTRF0020            GO BACK FOR NEXT                             
PTRF0040 EQU   *                                                                
         USING RCONXEL,R2                                                       
         OC    RCONTRF,RCONTRF     TRUFFIC NUM?                                 
         BZ    PTRFEX              NO                                           
         MVC   PTHIRD+70(L'RCONTRF),RCONTRF  MOVE TRFI # INTO PRINT L.          
         DROP  R2                                                               
PTRFEX   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*        TSARLINE - ADDS PRINT LINE DATA TO TSAR BUFFER PRIOR TO                
*          PRINTING.  ADDS GROUP/SUBGRP AND INTERFACE CODE TO                   
*          BEGINNING OF LINE TO SERVE AS KEY.                                   
*                                                                               
TSARLINE NTR1                                                                   
         XC    BUFFREC,BUFFREC     CLEAR BUFFER RECORD                          
         MVC   BFGRPSUB,RCONKGRP   INSERT GROUP/SUBGROUP                        
         LA    RE,RSTAELEM         FIND EXTENDED DESCRIP ELT                    
TSAL0010 EQU   *                                                                
         ZIC   RF,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,RF                                                            
         CLI   0(RE),0             END OF RECORD REACHED?                       
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAVE BEEN                    
         CLI   0(RE),8             PROPER ELEMENT?                              
         BNE   TSAL0010            NO  - GO BACK FOR NEXT                       
         MVC   BFINTFAC,RSTAOSI-RSTAXXEL(RE)                                    
*                                  INSERT INTERFACE CODE                        
         MVC   BFSTA,RSTAKSTA      INSERT STATION                               
         MVC   BFAGYOFF,RCONKAGY   INSERT AGENCY/AGY OFF                        
         MVC   BFADVCD,RCONKADV    INSERT ADVERTISER CODE                       
         MVC   BFPRODCD,RCONPRD    INSERT PRODUCT CODE (IF ANY)                 
         MVC   BFOFFICE,RCONKOFF   INSERT OFFICE                                
         MVC   BFOFFHI,RCONKOFF    INSERT OFFICE INTO KEY                       
         MVC   BFCONTYP(8),P+70    INSERT CONTRACT + TYPE                       
         MVC   BFMONTH,P+79        INSERT DATE                                  
         MVC   BFINV#,PSECOND+70   INSERT INVOICE NUMBER                        
         MVC   BFTRF#,PTHIRD+70    INSERT TRAFFIC NUMBER                        
         MVC   BFGRPNAM,RGRPNAME   INSERT GROUP NAME                            
         MVC   BFSUBNAM,RGRPSBNM   INSERT SUBGROUP NAME                         
         MVC   BFAGYNAM,RAGYNAM1   INSERT AGENCY NAME                           
         MVC   BFADVNAM,RADVNAME   INSERT ADVERTISER NAME                       
         MVC   BFPRDNAM,RPRDNAME   INSERT PRODUCT NAME                          
         MVC   BFSPERSN,RCONSAL    INSERT SALESPERSON NAME                      
         MVC   BFBUYER,RCONBUYR    INSERT BUYER NAME (1ST 3 CHARS)              
         MVC   BFCONTOT,SAVTOTS    SAVE CONTRACT TOTALS                         
         MVC   BFINVBYT,INVBYTE    SAVE INVENTORY BYTE FOR TSAR                 
         L     RF,RECCTR           INCREMENT COUNTER TO MAKE UNIQUE             
         LA    RF,1(RF)                                                         
         ST    RF,RECCTR                                                        
         MVC   BFCTR,RECCTR                                                     
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVC   0(TSARDL,R1),SVTSRBLK                                            
*                                  RELOAD TSAR BLOCK                            
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD     SET ACTION TO 'ADD'                          
         GOTO1 ATSAROFF            ADD RECORD TO BUFFER                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         XIT1                                                                   
*              FORMAT A LINE OF ACCUMULATORS                                    
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         LA    R3,P+86                                                          
         LA    R4,3                                                             
         LR    R6,R2                                                            
         SPACE 2                                                                
FOMA0020 TM    MYFLAG,TOTS         STAT/REPORT TOTALS?                          
         BZ    FOMA0030            NO                                           
         OC    0(8,R2),0(R2)                                                    
         BNZ   FOMA0060                                                         
         B     FOMA0035                                                         
FOMA0030 EQU   *                                                                
         OC    0(4,R2),0(R2)                                                    
         BNZ   FOMA0060                                                         
FOMA0035 EQU   *                                                                
         CLI   QACCTOPT,C'T'                                                    
         BE    FOMA0120                                                         
         CH    R4,=H'2'            LOOKING AT INVOICE AMOU                      
         BNE   FOMA0040                                                         
         TM    INVBYTE,X'01'       PRINT 0 INV. AMT. ON ACC. CON                
         BO    FOMA0060                                                         
FOMA0040 CH    R4,=H'1'            DONT PRINT ZERO DIFF EVER                    
         BE    FOMA0120                                                         
*                                  ONLY PRINT ZERO AMTS IF ACC CON              
         CLC   =C'ACC',RCONBUYR                                                 
         BNE   FOMA0120                                                         
FOMA0060 EQU   *                                                                
         CLI   QACCTOPT,C'P'       PENNIES OPTION                               
         BE    FOMA0080                                                         
         CLI   QACCTOPT,C'T'       PENNIES OPTION                               
         BE    FOMA0080                                                         
         TM    MYFLAG,TOTS         STAT/REPORT TOTALS?                          
         BZ    FOMA0070            NO                                           
         EDIT  (P8,(R2)),(14,(R3)),MINUS=YES,COMMAS=YES                         
         B     FOMA0120                                                         
FOMA0070 EQU   *                                                                
         EDIT  (4,(R2)),(14,(R3)),MINUS=YES,COMMAS=YES                          
         B     FOMA0120                                                         
FOMA0080 EQU   *                                                                
         TM    MYFLAG,TOTS         STAT/REPORT TOTALS?                          
         BZ    FOMA0090            NO                                           
         EDIT  (P8,(R2)),(14,(R3)),2,MINUS=YES,COMMAS=YES                       
         B     FOMA0120                                                         
FOMA0090 EQU   *                                                                
         EDIT  (4,(R2)),(14,(R3)),2,MINUS=YES,COMMAS=YES                        
FOMA0120 EQU   *                                                                
         LA    R3,16(R3)                                                        
*                                                                               
         TM    MYFLAG,TOTS         STAT/REPORT TOTALS?                          
         BZ    FOMA0130            NO                                           
         LA    R2,8(R2)                                                         
         B     FOMA0140                                                         
FOMA0130 EQU   *                                                                
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
FOMA0140 EQU   *                                                                
         BCT   R4,FOMA0020                                                      
*                                                                               
         TM    MYFLAG,TOTS         STAT/REPORT TOTALS?                          
         BZ    XIT                 NO                                           
         LR    R2,R6               RESTORE A(FIRST BUCKET)                      
         BAS   RE,CLTOTALS         CLEAR BUCKETS                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKFCST:  IF FORECAST ORDER, RETURN NON-ZERO CC FOR SKIP.                   
*                                                                               
CHKFCST  NTR1                                                                   
         LA    R2,RCONELEM         SET A(DESCRIPT ELEMENT)                      
CFCS0020 EQU   *                                                                
         CLI   0(R2),X'0'          END OF RECORD?                               
         BE    CFCS0100            YES - RETURN CC = ZERO                       
         CLI   0(R2),X'12'         SAR ELEMENT?                                 
         BE    CFCS0040            YES - CHECK IT OUT                           
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CFCS0020            GO BACK FOR NEXT                             
CFCS0040 EQU   *                                                                
         CLI   1(R2),120           CHECK ELEMENT LENGTH                         
         BNH   CFCS0100            OLD LENGTH: RETURN CC = ZERO                 
         USING RSARXEL,R2          NEW LENGTH:  CHECK FOR FCST                  
         TM    RSARXFLG,X'08'      BIT ON?                                      
         BNO   CFCS0100            NO  - NOT FCST:  CC = ZERO                   
         LTR   RB,RB               YES - FCST: CC NOT = ZERO                    
         B     CFCS0120                                                         
CFCS0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CFCS0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
PRINTIT  NTR1                                                                   
         MVC   HEAD4,MYHED4                                                     
         MVC   HEAD5,MYHED                                                      
         CLI   DOREP,1             FIRST REPORT IN PROGRESS?                    
         BNE   PRTI0020            NO  - CHECK SECOND REPORT                    
         CLI   REP#1,1             YES - WAS FIRST REPORT ASKED FOR?            
         BNE   PRTI0060            NO  - SKIP THE OUTPUT                        
         B     PRTI0040            YES - PRINT OUT THE DATA                     
PRTI0020 EQU   *                                                                
         CLI   DOREP,2             SECOND REPORT IN PROGRESS?                   
         BE    PRTI0030            NO                                           
         DC    H'0'                MUST BE 1 OR 2                               
PRTI0030 EQU   *                                                                
         CLI   REP#2,2             WAS SECOND REPORT ASKED FOR?                 
         BNE   PRTI0060            NO  - SKIP THE OUTPUT                        
PRTI0040 EQU   *                                                                
         GOTO1 REPORT                                                           
PRTI0060 EQU   *                                                                
         MVC   P,SPACES            CLEAR THE PRINT, IN CASE                     
         MVC   PSECOND,SPACES         REPORT NOT ASKED FOR                      
         MVC   PTHIRD,SPACES         REPORT NOT ASKED FOR                       
         B     XIT                                                              
         EJECT                                                                  
FLOAT    OI    0(R1),C' '                                                       
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-12                                                          
         LA    R1,1(R1)                                                         
         BR    RE                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*   RETURN TSAR TABLE, AND PRINT REPORT FROM IT                                 
*                                                                               
TSARBACK NTR1                                                                   
*                                                                               
         XC    CONTOTS(12),CONTOTS                                              
         XC    SAVTOTS(12),SAVTOTS                                              
         LA    R2,STATOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,OVERTOTS                                                      
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,GRPTOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         LA    R2,INTTOTS                                                       
         BAS   RE,CLTOTALS         CLEAR TOTALS                                 
         MVI   DOREP,2             SET TO SECOND REPORT                         
*                                  CLEAR TOTALS BUCKETS                         
         MVI   RCSUBPRG,3          USE INTERFACE HEADINGS                       
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVC   0(TSARDL,R1),SVTSRBLK                                            
*                                  RELOAD TSAR BLOCK                            
         LA    R0,BUFFREC                                                       
         ST    R0,TSAREC                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         XC    BUFFREC,BUFFREC     CLEAR BUFFER RECORD                          
         XC    OLDBFREC,OLDBFREC   CLEAR OLD BUFFER RECORD                      
TSBA0020 EQU   *                                                                
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    TSBA0080            YES - FINISHED                               
         BAS   RE,INFCPRNT         NO  - MOVE IT TO PRINT                       
         MVI   TSOFFACT,TSANXT     SET TO READ NEXT                             
         B     TSBA0020            GO BACK FOR NEXT LINE                        
TSBA0080 EQU   *                                                                
*                                                                               
         BAS   RE,STATTOTL         PRODUCE LAST STATION   TOTALS                
         BAS   RE,INFCTOTL         PRODUCE LAST INTERFACE TOTALS                
         BAS   RE,GSUBTOTL         PRODUCE LAST GROUP/SUBGRP TOTALS             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,2                                                        
         CLI   DATAFLAG,C'Y'       ANY DATA FOUND?                              
         BNE   TSBA0120            NO  - DON'T PRINT ANYTHING                   
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+50(19),=C'***REPORT TOTALS***'                                 
         MVC   MYHED(132),SPACES                                                
         LA    R2,OVERTOTS                                                      
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
         B     TSBA0160                                                         
TSBA0120 EQU   *                   NO DATA FOUND: PUT OUT TOTAL SHEET           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+50(19),=C'***NO DATA FOUND***'                                 
         MVC   MYHED(132),SPACES                                                
         BAS   RE,PRINTIT                                                       
TSBA0160 EQU   *                                                                
         DROP  R1                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FORMAT THE DATA INTO 3 PRINTLINES, AND PRINT IT OUT                         
*                                                                               
INFCPRNT NTR1                                                                   
         BAS   RE,GRSUBSET         SET GROUP/SUBGROUP HEADER                    
         OC    OLDBFREC(25),OLDBFREC                                            
*                                  FIRST TIME THROUGH?                          
         BZ    IFCP0060            YES                                          
         LA    RF,OLDBFREC         A(PREVIOUS BUFFER RECORD)                    
         CLC   BFGRPSUB,BFGRPSUB-BFREC(RF)                                      
*                                  SAME GROUP/SUPGROUP?                         
         BE    IFCP0010            YES                                          
         BAS   RE,STATTOTL         NO  - BREAK STATION                          
         BAS   RE,INFCTOTL               BREAK INTERFACE CODE                   
         BAS   RE,GSUBTOTL               BREAK GROUP/SUBGROUP                   
         MVI   FORCEHED,C'Y'       FORCE PAGEBREAK                              
         B     IFCP0060                                                         
IFCP0010 EQU   *                                                                
         CLC   BFINTFAC,BFINTFAC-BFREC(RF)                                      
*                                  YES - SAME INTERFACE CODE?                   
         BE    IFCP0012            YES                                          
         BAS   RE,STATTOTL         NO  - BREAK STATION                          
         BAS   RE,INFCTOTL               BREAK INTERFACE CODE                   
         MVI   FORCEHED,C'Y'       FORCE PAGEBREAK                              
         B     IFCP0060                                                         
IFCP0012 EQU   *                                                                
         CLC   BFSTA,BFSTA-BFREC(RF)                                            
*                                  YES - SAME STATION?                          
         BE    IFCP0060            YES                                          
         BAS   RE,STATTOTL         NO  - BREAK STATION                          
         MVI   FORCEHED,C'Y'       FORCE PAGEBREAK                              
*                                                                               
IFCP0060 EQU   *                                                                
         MVC   MYHED+21(10),BFINTFAC                                            
         MVC   MYHED+90(8),=C'STATION='                                         
         MVC   MYHED+98(5),BFSTA                                                
         MVC   MYHED+103(1),MYHED+102                                           
         MVI   MYHED+104,C'M'                                                   
         MVI   MYHED+102,C'-'                                                   
         MVC   INVBYTE,BFINVBYT    RESTORE FOR 'FORMAT' ROUTINE                 
         MVC   RCONBUYR(3),BFBUYER RESTORE FOR 'FORMAT' ROUTINE                 
*                                                                               
*   FIRST MONTH OF A CONTRACT WILL CONTAIN THE CONTRACT#/TYPE.                  
*     FOLLOWING MONTHS WILL NOT.  THIS PERMITS IDENTIFICATION AND               
*     SUPPRESSION OF REDUNDANT DESCRIPTIVE INFORMATION.                         
*                                                                               
         CLC   BFCONTYP(8),SPACES  ANY ENTRY IN FIELD?                          
         BE    IFCP0100            NO  - SKIP REDISPLAY FOR ORDER               
         MVC   OLDBFREC,BUFFREC    SAVE NEW BUFFER RECORD                       
         MVC   RAGYKAGY(6),BFAGYOFF     RESET AGENCY CODE                       
         MVC   P(4),RAGYKAGY       LOAD AGENCY+CITY AND NAME                    
         LA    R1,P+3                                                           
         BAS   RE,FLOAT                                                         
         CLC   RAGYKAOF,SPACES                                                  
         BE    IFCP0080                                                         
         MVI   0(R1),C'-'                                                       
         MVC   1(2,R1),RAGYKAOF                                                 
IFCP0080 EQU   *                                                                
         MVC   P+70(8),BFCONTYP    INSERT CONTRACT # + TYPE                     
         MVC   P+22(4),BFADVCD     INSERT ADVERTISER CODE                       
         MVC   P+44(3),BFPRODCD    INSERT PRODUCT CODE                          
         MVC   P+66(2),BFOFFICE    INSERT OFFICE                                
         MVC   PSECOND(20),BFAGYNAM                                             
*                                  INSERT AGENCY NAME                           
         MVC   PSECOND+22(20),BFADVNAM                                          
*                                  INSERT ADVERTISER NAME                       
         MVC   PSECOND+44(20),BFPRDNAM                                          
*                                  INSERT PRODUCT NAME                          
         MVC   PSECOND+66(3),BFSPERSN                                           
*                                  INSERT SALESPERSON NAME                      
IFCP0100 EQU   *                                                                
         MVC   PSECOND+70(12),BFINV# INSERT INVOICE NUMBER                      
         OC    BFTRF#,BFTRF#                                                    
         BZ    *+10                                                             
         MVC   PTHIRD+70(10),BFTRF#    INSERT TRAFFIC NUMBER                    
*                                                                               
         MVC   PSECOND+85(47),DASHES   PRINT DASHES ON 2ND PRINT LINE           
         CLC   PTHIRD,SPACES           BUT IF THERE IS A TRF#                   
         BE    IFCP0110                PRINT DASHES ON 3RD PRINT LINE           
         MVC   PTHIRD+85(47),DASHES                                             
         MVC   PSECOND+85(47),SPACES                                            
         MVI   PSECOND+85,X'00'                                                 
IFCP0110 EQU   *                                                                
         MVC   P+79(5),BFMONTH     INSERT MONTH                                 
         MVC   CONTOTS,BFCONTOT    RESTORE COUNTERS                             
         LA    R2,CONTOTS                                                       
         LA    R6,STATOTS          ACCUMULATE STATION TOTALS                    
         LA    R0,3                                                             
IFCP0120 EQU   *                                                                
         L     R1,0(R2)                                                         
         CVD   R1,DUB              CONVERT TOTAL TO PACKED FORMAT               
         AP    0(8,R6),DUB         ADD SUM OF CONTRACTS                         
         LA    R2,4(R2)            BUMP TO NEXT BUCKETS                         
         LA    R6,8(R6)                                                         
         BCT   R0,IFCP0120         DO THREE BUCKETS                             
         LA    R2,CONTOTS                                                       
         LA    R6,INTTOTS          ACCUMULATE INTERFACE CODE TOTALS             
         LA    R0,3                                                             
IFCP0130 EQU   *                                                                
         L     R1,0(R2)                                                         
         CVD   R1,DUB              CONVERT TOTAL TO PACKED FORMAT               
         AP    0(8,R6),DUB         ADD SUM OF CONTRACTS                         
         LA    R2,4(R2)            BUMP TO NEXT BUCKETS                         
         LA    R6,8(R6)                                                         
         BCT   R0,IFCP0130         DO THREE BUCKETS                             
         LA    R2,CONTOTS                                                       
         LA    R6,OVERTOTS         ACCUMULATE INTRFCE CODE GRAND TOTS           
         LA    R0,3                                                             
IPCP0140 EQU   *                                                                
         L     R1,0(R2)                                                         
         CVD   R1,DUB              CONVERT TOTAL TO PACKED FORMAT               
         AP    0(8,R6),DUB         ADD SUM OF CONTRACTS                         
         LA    R2,4(R2)            BUMP TO NEXT BUCKETS                         
         LA    R6,8(R6)                                                         
         BCT   R0,IPCP0140         DO THREE BUCKETS                             
         LA    R2,CONTOTS                                                       
         LA    R6,GRPTOTS          ACCUMULATE GROUP/SUBGROUP TOTS               
         LA    R0,3                                                             
IPCP0160 EQU   *                                                                
         L     R1,0(R2)                                                         
         CVD   R1,DUB              CONVERT TOTAL TO PACKED FORMAT               
         AP    0(8,R6),DUB         ADD SUM OF CONTRACTS                         
         LA    R2,4(R2)            BUMP TO NEXT BUCKETS                         
         LA    R6,8(R6)                                                         
         BCT   R0,IPCP0160         DO THREE BUCKETS                             
         LA    R2,CONTOTS                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GSUBTOTL:                                                                   
*                                                                               
GSUBTOTL NTR1                                                                   
         OC    REP#1,REP#1         STATION REPORT REQUESTED?                    
         BZ    GSUB0020            NO  - ONLY 'INTERFACE' =                     
*                                     SKIP THIS DISPLAY                         
         MVI   SPACING,1                                                        
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BAS   RE,PRINTIT                                                       
         MVC   P+45(23),=C'*GROUP/SUBGROUP TOTALS*'                             
         LA    R2,GRPTOTS                                                       
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
GSUB0020 EQU   *                                                                
         LA    R2,GRPTOTS                                                       
         BAS   RE,CLTOTALS                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   INFCTOTL:                                                                   
*                                                                               
INFCTOTL NTR1                                                                   
         MVI   SPACING,1                                                        
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BAS   RE,PRINTIT                                                       
         MVC   P+45(23),=C'*INTERFACE CODE TOTALS*'                             
         LA    R2,INTTOTS                                                       
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
         LA    R2,INTTOTS                                                       
         BAS   RE,CLTOTALS                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STATTOTL:                                                                   
*                                                                               
STATTOTL NTR1                                                                   
         MVI   SPACING,1                                                        
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BAS   RE,PRINTIT                                                       
         MVC   P+52(16),=C'*STATION TOTALS*'                                    
         LA    R2,STATOTS                                                       
         OI    MYFLAG,TOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRINTIT                                                       
         NI    MYFLAG,X'FF'-TOTS                                                
         LA    R2,STATOTS                                                       
         BAS   RE,CLTOTALS                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GRSUBSET: SET GROUP/SUBGROUP FOR HEADER 4                                   
*                                                                               
GRSUBSET NTR1                                                                   
         MVC   MYHED4+1(8),=C'GROUP  -'                                         
         MVC   MYHED4+7(1),BFGRPSUB     INSERT GROUP                            
         MVC   MYHED4+9(10),BFGRPNAM    INSERT GROUP NAME                       
         CLI   BFGRPSUB+1,C' '          ANY SUBGROUP?                           
         BE    GRSU0020                 NO  - FINISHED                          
         MVC   MYHED4+21(11),=C'SUBGROUP  -'                                    
         MVC   MYHED4+30(1),BFGRPSUB+1  INSERT SUBGROUP                         
         MVC   MYHED4+32(10),BFSUBNAM   INSERT SUBGRP NAME                      
GRSU0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CLEAR  TOTALS                                                                 
*                                                                               
CLTOTALS NTR1                                                                   
         LA    R6,3                COUNTER                                      
CL10     EQU   *                                                                
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,CL10                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         CLI   QACCTOPT,C'T'                                                    
         BNE   HHOOK05                                                          
         MVC   HEAD5+040(28),=C'FOR THE ACCOUNTING PERIOD OF'                   
         L     R4,AMONARCH                                                      
         USING MONARCHD,R4                                                      
         GOTO1 DATCON,DMCB,(X'12',KEYMONTH),(5,HEAD5+066)                       
         DROP  R4                                                               
         B     HHOOK10                                                          
HHOOK05  EQU   *                                                                
         MVC   HEAD5+052(23),=C'(BROADCAST MONTH BASIS)'                        
HHOOK10  EQU   *                                                                
*                                                                               
         XIT                                                                    
         EJECT                                                                  
*              WORK SPACE ETC                                                   
         SPACE 3                                                                
MYFLAG   DS    X                                                                
TOTS     EQU   X'80'               ST/REPORT/GROUP TOTALS                       
         DS    0F                                                               
SAVEREGS DS    11F                                                              
*                                                                               
COUNTER  DS    CL1                                                              
         DS    0F                                                               
GENTOTS  EQU   *                                                                
CONTOTS  DS    0XL12                                                            
         DC    3F'0'             BINARY CONTRACT TOTALS                         
STATOTS  DS    3PL8               PACKED STATION TOTALS                         
OVERTOTS DS    3PL8                                                             
GRPTOTS  DS    3PL8                                                             
INTTOTS  DS    3PL8                                                             
SAVTOTS  DS    CL12                                                             
         DS    PL8                                                              
LGENTOTS EQU   *-GENTOTS                                                        
         SPACE 1                                                                
REP#1    DC    XL1'0'                                                           
REP#2    DS    XL1'0'                                                           
DOREP    DS    XL1                                                              
OLDSTAT  DS    CL5                 PREVIOUS STATION                             
STATACTV DS    CL1                                                              
*                                                                               
ACTIVE   DS    CL1                                                              
ELCODE   DS    CL1                                                              
MYHED4   DS    CL132               FOR GROUP/SUBGROUP                           
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
         SPACE 2                                                                
OPT2TAB  DS    0CL9                                                             
         DC    C'E',AL4(100),AL4(10000)                                         
         DC    C'F',AL4(500),AL4(50000)                                         
         DC    C'G',AL4(1000),AL4(100000)                                       
         DC    C'H',AL4(1500),AL4(150000)                                       
         DC    C'I',AL4(5000),AL4(500000)                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
DASHES   DC   15C'-',CL1' ',15C'-',CL1' ',15C'-'                                
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
ORDFLAG  DC    X'00'               FLAGS FOR 'BEST $' OPTION                    
INVFLAG  DC    X'00'                                                            
ORDINV   DC    XL2'0100'           ORD $/NO INV $                               
INVORD   DC    XL2'0001'           INV $/NO ORD $                               
DATAFLAG DC    CL1'N'              Y  =  DATA FOUND                             
INVBYTE  DS    CL1                                                              
RECCTR   DS    F                   RECORD COUNTER                               
*                                                                               
*                                                                               
BFREC    EQU   *                   BUFFER RECORD                                
BFGRPSUB DS    CL2                 GROUP/SUBGROUP                               
BFINTFAC DS    CL10                INTERFACE CODE                               
BFSTA    DS    CL5                 STATION CODE                                 
BFOFFHI  DS    CL2                 OFFICE CODE HIGH                             
BFAGYOFF DS    CL6                 AGENCY/AGYOFF                                
BFADVCD  DS    CL4                 ADVERTISER                                   
BFPRODCD DS    CL3                 PRODUCT                                      
BFCTR    DS    CL4                 COUNTER                                      
LBFKEY   EQU   *-BFREC             L(BFKEY)                                     
BFOFFICE DS    CL2                 OFFICE                                       
BFCONTYP DS    CL9                 CONTRACT NUMBER + TYPE                       
BFMONTH  DS    CL5                 MONTH                                        
BFINV#   DS    CL12                INVOICE NUMBER                               
BFTRF#   DS    CL10                TRAFFIC NUMBER                               
BFGRPNAM DS    CL10                GROUP NAME                                   
BFSUBNAM DS    CL10                SUBGROUP NAME                                
BFAGYNAM DS    CL20                AGENCY NAME                                  
BFADVNAM DS    CL20                ADVERTISER NAME                              
BFPRDNAM DS    CL20                PRODUCT NAME                                 
BFSPERSN DS    CL3                 SALESPERSON                                  
BFBUYER  DS    CL3                 BUYER NAME                                   
BFCONTOT DS    CL12                TOTALS FOR ORDER                             
BFINVBYT DS    CL1                 INVENTORY BYTE FOR REDISPLAY                 
BFEND    DS    0CL1                                                             
LBUFFREC EQU   *-BFREC             L(BFREC)                                     
*                                                                               
         ORG   BFREC                                                            
BUFFREC  DS    CL(BFEND-BFREC)                                                  
OLDBFREC DS    CL(BFEND-BFREC)                                                  
SVTSRBLK DS    CL(TSARDL)                                                       
         ORG                                                                    
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***       RINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REMONARCHD                                                     
       ++INCLUDE DDTSARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048REREP2002 05/01/02'                                      
         END                                                                    
