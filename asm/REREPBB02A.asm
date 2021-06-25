*          DATA SET REREPBB02A AT LEVEL 205 AS OF 09/09/99                      
*PHASE REBB02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'REREPBB02  (REBB02) --- NBC BACK BILLING UPLOAD'                
*                                                                               
********************************************************************            
*                                                                  *            
*                                                                  *            
*        REREPKB02  -- NBC BACK BILLING UPLOAD:  ACCEPT E-MAIL FILE*            
*                      AND SAVE AS CSV FROM EXCEL IN BUHR.BOOK2    *            
*                      CONVERT FROM BUHR.BOOK2 TO EITHER TAPE, OR  *            
*                      DIRECTLY INTO MAIN FILE.                    *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* AUG17/99 (ROB) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =    Y   =   DISPLAY INPUT RECORDS               *            
*     QUESTOR+1   =    Y   =   DISPLAY OUTPUT RECORDS              *            
*     QUESTOR+2   =    Y   =   CUT OFF AFTER N RECORDS             *            
*     QUESTOR+3   =    Y   =   DISPLAY OUTPUT RECORDS              *            
*     QUESTOR+11  =    Y   =   DISPLAY PERFORMANCE TIME STAMP      *            
*                                                                  *            
*                                                                  *            
*  QOPTION1   =   NOT USED                                                      
*                                                                               
*  QOPTION2   =   DIRECT/MERGE FLAG    D = DIRECT UPDATE (DEFAULT) *            
*                                      M = TAPE O/P TO BE MERGED   *            
*  QOPTION3   =   UPDATE/TEST FLAG     U = UPDATE FILE             *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REBB02   CSECT                                                                  
         NMOD1 0,**REBB**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0900                                                         
*                                                                               
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   VREPFACS,4(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
         OC    VREPFACS,VREPFACS                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC),RR=Y                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK) RESOLVE BUCKET ACTIVITY DATE          
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,1                                                             
         ZIC   R3,0(R1)                                                         
         SR    R2,R3                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEWBUCK+4)                             
*                                                                               
*                                  GET NEXT CONTRACT NUMBER                     
         ZAP   LASTCON,=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),QREP      ALPHA REP CODE                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*                                                                               
         MVO   WORK+5(5),KEY+23(4)                                              
         SP    LASTCON,WORK+5(5)   GET POSITIVE                                 
         AP    LASTCON,=P'1'       NEXT K NUMBER +1 (SKIP 1 NUMBER)             
         MVO   WORK+10(5),LASTCON                                               
         MVC   RCONKCON,WORK+10    TO K KEY                                     
*                                                                               
         L     R5,ARECAREA                                                      
MAIN0060 DS    0H                                                               
         GET   INTAPE,(R5)         READ TAPE RECORD INTO RDA                    
*                                     TAPE WILL BE PARSED ON ',' SEPS           
         L     RF,REDCTR           INCREMENT RECORD COUNTER                     
         LA    RF,1(RF)                                                         
         ST    RF,REDCTR                                                        
         CLI   QUESTOR+0,C'Y'      DISPLAY INPUT RECORDS?                       
         BNE   MAIN0070            NO                                           
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'INPUT RECORD:         '                               
         GOTO1 REPORT                                                           
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
*                                                                               
MAIN0070 EQU   *                                                                
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
*                                                                               
*                                  REINITIALIZE RECORDS                         
*                                                                               
         MVI   RCSUBPRG,1          SET HEADINGS                                 
*                                                                               
         MVI   NEWBUCK+3,0                                                      
*                                                                               
*   SKELETONIZE RECORDS                                                         
*                                                                               
         XC    RCONREC(255),RCONREC                                             
         AP    LASTCON,=P'1'       NEXT K NUMBER                                
         MVO   WORK(5),LASTCON                                                  
         MVC   RCONKCON,WORK       TO K KEY                                     
*                                                                               
         MVI   RCONKTYP,X'0C'      INSERT RECORD TYPE                           
*                                                                               
         MVC   RCONKREP,QREP                                                    
*                                                                               
         MVC   RCONELEM(2),=X'013C' INSERT ELEMENT 1 CODE                       
*                                                                               
         LA    R1,1+(RCONCMEL-RCONREC)                                          
         STCM  R1,3,RCONLEN        REC LENGTH                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,RCONCREA)                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,RCONHDRD)                                
*                                                                               
         MVI   RCONTYPE,C'S'                                                    
*                                                                               
         MVC   RCONPRD,SPACES                                                   
*                                                                               
         MVC   RCONBUYR(6),=C'ACC-BB'                                           
         OC    RCONBUYR,SPACES                                                  
*                                                                               
         LA    R4,AGBLDTAB         SET A(BUILD TABLE)                           
*                                  R5-->A(INPUT DATA)                           
         LR    RF,R5               CALCULATE A(END OF RECORD)                   
         ZICM  RE,0(R5),2          GET L(RECORD INPUT)                          
         AR    RF,RE                                                            
         ST    RF,ENDOFREC         SAVE END OF RECORD                           
*                                                                               
         LR    RF,R5               RESET A(RECORD INPUT)                        
         LA    RF,4(RF)            SKIP CONTROL OF I/P                          
         ST    RF,AIPFIELD         SET A(1ST INPUT FIELD)                       
         ST    RF,AIPFIEL2         SET A(1ST INPUT FIELD): 2ND COPY             
MAIN0100 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    MAIN0180            YES - DISPLAY AND OUTPUT RECORD              
*                                                                               
         LA    R2,RECORD           SET A(RECORD)                                
         BAS   RE,LOADDATA                                                      
         LA    R4,LAGBLDNT(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0100            GO BACK FOR NEXT TABLE ENTRY                 
MAIN0180 EQU   *                                                                
*                                                                               
*  FURTHER UPDATES TO RECORD HERE                                               
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW05ELT,0             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW1EELT,0             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW1FELT,0             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW20ELT,0             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,GETSTA                                                        
         CLI   RSTAREC,0                                                        
         BE    *+10                                                             
         MVC   RCONKGRP,RSTAGRUP                                                
*                                                                               
         BAS   RE,GETSAL                                                        
         CLI   RSALREC,0                                                        
         BE    *+10                                                             
         MVC   RCONTEM,RSALTEAM                                                 
*                                                                               
         BAS   RE,GETAGY                                                        
*                                                                               
         BAS   RE,GETADV                                                        
*                                                                               
         LA    RF,RCONREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 AGYRECOP,DMCB,(0,0) PROCESS DETAIL RECORD                        
         B     MAIN0060            GO BACK FOR NEXT                             
*                                                                               
MAIN0800 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=Y                                      
*                                  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   MAIN0820            NO  - DIRECT UPDATE                          
         CLOSE FILOUTA             YES - CLOSE OUTPUT FILES                     
         CLOSE FILOUTB                                                          
MAIN0820 EQU   *                                                                
         CLOSE (INTAPE,REWIND)                                                  
*                                                                               
MAIN0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
*   AGYRECOP:  SUBROUTINE TO GENERATE OUTPUT FOR THIS RUN.                      
*                                                                               
AGYRECOP NTR1                                                                   
         CLI   QUESTOR+3,C'Y'      DISPLAY OUTPUT?                              
         BNE   AGOP0030            NO                                           
         MVC   P+1(24),=C'RECORD1 CREATED FROM I/P'                             
         GOTO1 REPORT                                                           
         MOVE  (REC,1024),RECORD    MOVE REC TO O/P AREA                        
         ZICM  RF,RCONLEN,2        SET O/P LENGTH                               
         LA    RF,4(RF)            BUMP LENGTH BY 2                             
         STCM  RF,3,REC-4                                                       
         GOTO1 =A(DISPPUT),DMCB,(RC),RR=Y                                       
*                                  DISPLAY AGENCY RECORD                        
AGOP0030 EQU   *                                                                
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   AGOP0040            NO  - PUT DIRECTLY TO FILE                   
         GOTO1 PUTRECS             YES                                          
         B     AGOP0200                                                         
AGOP0040 EQU   *                                                                
         MVC   KEY(27),RECORD      CHECK: IS RECORD ALREADY ON FILE?            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGOP0060            NO                                           
         DC    H'0'                                                             
AGOP0060 EQU   *                                                                
         LA    RF,RECORD           SET A(IO AREA)                               
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0070            NO  - SKIP O/P                               
         GOTO1 AREC                                                             
AGOP0070 EQU   *                                                                
         MVC   P+1(8),=C'ADD CON:'                                              
AGOP0075 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,P+11,4                                      
         MVC   P+20(2),RCONKREP    DISPLAY REP                                  
         MVC   P+24(5),RCONKSTA    DISPLAY STA                                  
         MVC   P+31(6),RCONKAGY    AGENCY                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
AGOP0080 EQU   *                                                                
         L     RF,TOTCTR           BUMP TOTAL COUNT                             
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         B     AGOP0200                                                         
*&&DO                                                                           
AGOP0100 EQU   *                                                                
*                                                                               
         MVC   KEYDA,KEY+28        SAVE DISK ADDRESS                            
         MVC   KEY(27),RECORD                                                   
         MVI   KEY,X'8A'           INSERT PASSIVE KEY TYPE                      
         MVC   KEY+1(18),OLDNAME   INSERT ORIG NAME                             
         GOTO1 HIGH                IS KEY ON FILE?                              
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    AGOP0120            YES                                          
         MVC   KEY(27),KEYSAVE     NO  - RESTORE KEY                            
         MVC   KEY+28(4),KEYDA     INSERT NEW D/A                               
         B     AGOP0180                                                         
AGOP0120 EQU   *                                                                
         OI    KEY+27,X'80'        YES - TURN ON DELETE BIT                     
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0140            NO  - SKIP O/P                               
         GOTO1 WRITE               REWRITE THE KEY 'DELETED'                    
AGOP0140 EQU   *                                                                
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         MVC   KEY+1(18),RAGYNAM1  INSERT NEW NAME                              
         MVC   KEY+28(4),KEYDA     INSERT NEW D/A                               
         GOTO1 HIGH                CHECK FOR EXISTENCE                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGOP0180            NO                                           
         MVC   KEY+28(4),KEYDA     YES - INSERT NEW D/A                         
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0160            NO  - SKIP O/P                               
         GOTO1 WRITE               REWRITE KEY                                  
AGOP0160 EQU   *                                                                
         B     AGOP0200                                                         
AGOP0180 EQU   *                                                                
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0200            NO  - SKIP O/P                               
         GOTO1 ADD                 ADD NEW KEY                                  
*&&                                                                             
AGOP0200 EQU   *                                                                
*&&DO                                                                           
         L     RF,TOTCTR           BUMP TOTAL COUNT                             
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         CLI   QUESTOR+3,C'Y'      DISPLAY OUTPUT?                              
         BNE   AGOP0210            NO                                           
         MVC   P+1(24),=C'RECORD2 CREATED FROM I/P'                             
         GOTO1 REPORT                                                           
         MOVE  (REC,300),RECORD2   MOVE AGENCY2 REC TO O/P AREA                 
         ZICM  RF,RAGY2LEN,2       SET O/P LENGTH                               
         LA    RF,4(RF)            BUMP LENGTH BY 2                             
         STCM  RF,3,REC-4                                                       
         GOTO1 =A(DISPPUT),DMCB,(RC),RR=Y                                       
*                                  DISPLAY AGENCY RECORD                        
AGOP0210 EQU   *                                                                
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   AGOP0220            NO  - PUT DIRECTLY TO FILE                   
         GOTO1 PUTRECS             YES                                          
         B     AGOP0400                                                         
AGOP0220 EQU   *                                                                
         MVC   KEY(27),RECORD2     CHECK: IS RECORD ALREADY ON FILE?            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGOP0260            NO                                           
         LA    RF,RECORD3          SET NEW IOAREA                               
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                READ ORIGINAL RECORD                         
         LA    RF,RECORD2                                                       
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0240            NO  - SKIP O/P                               
         GOTO1 PREC                REWRITE RECORD                               
AGOP0240 EQU   *                                                                
         B     AGOP0400                                                         
AGOP0260 EQU   *                                                                
         LA    RF,RECORD2          SET A(IO AREA)                               
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0400            NO  - SKIP O/P                               
         GOTO1 AREC                                                             
         B     AGOP0400                                                         
AGOP0280 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY OUTPUT?                              
         BNE   AGOP0400            NO                                           
         MVC   P+1(38),=C'CORPORATE1 ALREADY ON FILE - NO UPDATE'               
         GOTO1 REPORT                                                           
         B     AGOP0400                                                         
AGOP0300 EQU   *                                                                
         MVC   P+1(17),=C'DUPLICATE AGENCY:'                                    
         MVC   P+20(4),RAGYKAGY    DISPLAY AGENCY CODE                          
         MVC   P+26(2),RAGYKAOF    DISPLAY AGENCY OFFICE CODE                   
         MVC   P+32(33),RAGYNAM2   AGENCY NAME                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         L     RF,DUPECTR          BUMP DUPLICATE RECORDS FOUND                 
         LA    RF,1(RF)                                                         
         ST    RF,DUPECTR                                                       
*&&                                                                             
AGOP0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  AGENCY BUILD TABLE:  A SERIES OF ENTRIES WHICH CORRESPOND WITH               
*        THE DATA DELIVERED FROM THE UPLOAD RECORD.  EACH ENTRY                 
*        IS SET UP AS:                                                          
*        POS   1      =  INSERT IN OUTPUT RECORD INDICATOR:                     
*                        X'00'  =  DON'T INSERT INTO RECORD                     
*                        X'01'  =  X'0C' RECORD                                 
*                        X'02   =  CREATE BUCKET ELEM                           
*        POS   2 - 3  =  DISPLACEMENT INTO RECORD, IF NOT IN AN                 
*                        ELEMENT - IF ELEMENT, DISPLACEMENT THERE               
*        POS   4      =  ELEMENT, IF NOT A DISPLACEMENT                         
*        POS   5      =  FILL CHARACTER                                         
*        POS   6      =  MAX CHARACTER                                          
*        POS   7      =  0  =  FIXED                                            
*                        1  = VARIABLE                                          
AGRECTYP EQU   0                   DISP(RECORD TYPE INDICATOR)                  
AGRECDIS EQU   1                   DISP(DISP INTO RECORD/ELEMENT)               
AGRECELT EQU   3                   DISP(ELEMENT CODE)                           
AGRECFIL EQU   4                   DISP(FILL CHARACTER)                         
AGRECMAX EQU   5                   DISP(MAX COUNT)                              
AGRECFIX EQU   6                   DISP(FIXED/VARIABLE FLAG)                    
*                                                                               
*  BUILD TABLE COMPONENT                                                        
*                                                                               
AGBLDTAB EQU   *                                                                
         DC    X'0',AL2(0),X'00',X'00',AL1(5),X'00'                             
*                                  GROSS/NET                                    
LAGBLDNT EQU   *-AGBLDTAB                                                       
         DC    X'1',AL2(RCONKSTA-RCONREC),X'00',C' ',AL1(5),X'00'               
*                                  STATION                                      
         DC    X'1',AL2(RCONKAGY-RCONREC),X'00',C' ',AL1(4),X'00'               
*                                  AGENCY CODE                                  
         DC    X'1',AL2(RCONKAOF-RCONREC),X'00',C' ',AL1(2),X'00'               
*                                  AGENCY OFFICE                                
         DC    X'1',AL2(RCONKADV-RCONREC),X'00',C' ',AL1(4),X'00'               
*                                  ADVERTISER CODE                              
         DC    X'1',AL2(RCONSAL-RCONREC),X'00',C' ',AL1(3),X'00'                
*                                  SALESMAN                                     
         DC    X'1',AL2(RCONKOFF-RCONREC),X'00',C' ',AL1(2),X'00'               
*                                  OFFICE                                       
         DC    X'4',AL2(RCONDATE-RCONREC),X'00',C' ',AL1(2),X'00'               
*                                  FLIGHT                                       
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'  BUCKET AMT (36)             
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
         DC    X'2',AL2(0),X'FF',C' ',AL1(2),X'00'                              
*                                                                               
         DC    X'FFFF'             DELIMITER                                    
         DS    0H                  ALIGNMENT                                    
*                                                                               
*   LOADDATA:  USING TABLE, ITEM IS                                             
*        A.  INITIALIZED TO FILL CHARACTER                                      
*        B.  UNSTRUNG FROM DATA INPUT UNTIL ',' DELIMITER IS                    
*            REACHED.                                                           
*        C.  ELEMENTS ARE INITIALIZED/BUILT AS APPROPRIATE                      
*                                                                               
*        REGISTER USAGE:                                                        
*            R2  -->  A(RECORD BEING BUILT)                                     
*            R4  -->  BUILD TABLE                                               
*            R5  -->  A(INPUT RECORD) - PASSED IN AIPFIELD                      
*                                                                               
LOADDATA NTR1                                                                   
         L     R5,AIPFIELD         SET A(FIELD IN PROGRESS                      
         CLI   AGRECTYP(R4),5      TRAP                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R6,R5               FIND ACTUAL LEN OF DATA                      
LDAT0008 DS    0H                                                               
         C     R6,ENDOFREC         END OF RECORD REACHED?                       
         BNL   LDAT0010                                                         
         CLI   0(R6),C','          DELIMITER?                                   
         BE    LDAT0010                                                         
         LA    R6,1(R6)                                                         
         B     LDAT0008                                                         
LDAT0010 DS    0H                                                               
         SR    R6,R5               DATA LENGTH                                  
*                                                                               
         CLI   AGRECTYP(R4),2      BUCKET                                       
         BNE   LDAT0040                                                         
         LTR   R6,R6               EMPTY BUCKET?                                
         BZ    LDAT0200                                                         
         XC    WORK2,WORK2                                                      
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R5)                                                   
         LA    R6,1(R6)                                                         
         OC    WORK2(80),SPACES         MAKE SPACE PADDED CARD                  
         GOTO1 =V(SCANNER),DMCB,(C'C',WORK2),WORK,C',=,:',RR=Y                  
         CLI   4(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         ZIC   R1,NEWBUCK+3                                                     
         LA    R1,1(R1)                                                         
         CH    R1,=H'12'                                                        
         BNH   *+8                                                              
         LA    R1,1                                                             
         STC   R1,NEWBUCK+3                                                     
         MVC   WORK2(LNEWBUCK),NEWBUCK                                          
         MVI   WORK2,X'04'                                                      
         CLI   WORK+12,C'B'                                                     
         BE    LDAT0020                                                         
         MVI   WORK2,X'54'                                                      
         CLI   WORK+12,C'A'                                                     
         BE    LDAT0020                                                         
         MVI   WORK2,X'64'                                                      
         CLI   WORK+12,C'G'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
LDAT0020 EQU   *                                                                
         ICM   RF,15,WORK+8                                                     
         MH    RF,=H'100'          MAKE DOLLARS                                 
         STCM  RF,15,WORK2+6                                                    
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),(R2),WORK2,0                   
         CLI   12(R1),0                                                         
         BE    LDAT0200                                                         
         DC    H'0'                                                             
*                                                                               
LDAT0040 EQU   *                                                                
         CLI   AGRECTYP(R4),4                                                   
         BNE   LDAT0080                                                         
         LTR   R6,R6               NO DATA?                                     
         BZ    LDAT0200                                                         
         XC    WORK,WORK                                                        
         GOTO1 =V(PERVAL),DMCB,((R6),0(R5)),WORK                                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCONDATE,WORK+28                                                 
*                                                                               
         XC    WORK,WORK           FIGURE BUCKET YEAR                           
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,DUB)                                 
         GOTO1 GETBROAD,DMCB,DUB,WORK,GETDAY,ADDAY                              
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,7                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,WORK+18)                              
         MVC   NEWBUCK+2(1),WORK+18                                             
         B     LDAT0200                                                         
*                                                                               
LDAT0080 EQU   *                                                                
         TM    AGRECTYP(R4),1                                                   
         BZ    LDAT0200                                                         
         ZIC   RF,AGRECMAX(R4)     GET FIELD LENGTH                             
         LR    R3,R2               SET FIELD DISPLACEMENT                       
         ZICM  RE,AGRECDIS(R4),2   GET DISP(REC/ELT)                            
         AR    R3,RE               SET A(RECEIVING FIELD)                       
         BCTR  RF,0                                                             
         EX    RF,*+8              YES - SET FIELD TO BIN ZERO                  
         B     *+10                                                             
         XC    0(0,R3),0(R3)       SET FIELD TO BIN ZERO                        
         LA    RF,1(RF)                                                         
*                                                                               
         CR    RF,R6               MAX LEN VS. DATA LEN                         
         BL    *+6                                                              
         LTR   RF,R6               USE DATA LEN                                 
         BZ    LDAT0110            NO DATA, LEAVE BLANK                         
         BCTR  RF,0                FOR EX                                       
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R5)       MOVE FIELD TO RECORD                         
LDAT0110 DS    0H                                                               
         CLI   AGRECFIL(R4),C' '   SPACE PAD?                                   
         BNE   LDAT0200                                                         
         ZIC   RF,AGRECMAX(R4)     MAX FIELD LENGTH                             
         BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+4                                                           
         OC    0(0,R3),SPACES      PADDING                                      
*                                                                               
LDAT0200 DS    0H                                                               
         LA    R5,1(R6,R5)         NEXT FIELD                                   
         ST    R5,AIPFIELD         SET TO A(NEXT FIELD)                         
         XIT1                                                                   
*                                                                               
* GETSTA - READ STATION RECORD INTO RECORD3                                     
*                                                                               
GETSTA   NTR1                                                                   
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETSTA20                                                         
         MVI   RSTAREC,0                                                        
         MVC   P(20),=C'***MISSING RECORD***'                                   
         MVC   P+24(8),=C'STATION:'                                             
         MVC   P+33(5),RCONKSTA                                                 
         GOTO1 REPORT                                                           
         B     GETSTAX                                                          
GETSTA20 LA    RF,RSTAREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
GETSTAX  XIT1                                                                   
*                                                                               
* GETSAL - READ SALESMAN RECORD INTO RECORD3                                    
*                                                                               
GETSAL   NTR1                                                                   
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETSAL20                                                         
         MVI   RSALREC,0                                                        
         MVC   P(20),=C'***MISSING RECORD***'                                   
         MVC   P+24(9),=C'SALESMAN:'                                            
         MVC   P+34(3),RCONSAL                                                  
         GOTO1 REPORT                                                           
         B     GETSALX                                                          
GETSAL20 LA    RF,RSALREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
GETSALX  XIT1                                                                   
*                                                                               
* GETADV - READ ADVERTISER RECORD TO RECORD3                                    
*                                                                               
GETADV   NTR1                                                                   
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   RADVKREP,RCONKREP                                                
         MVC   RADVKADV,RCONKADV                                                
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETADV20                                                         
         MVI   RADVREC,0                                                        
         MVC   P(20),=C'***MISSING RECORD***'                                   
         MVC   P+24(11),=C'ADVERTISER:'                                         
         MVC   P+36(4),RCONKADV                                                 
         GOTO1 REPORT                                                           
         B     GETADVX                                                          
GETADV20 LA    RF,RADVREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
GETADVX  XIT1                                                                   
*                                                                               
* GETAGY - READ AGENCY RECORD TO RECORD3                                        
*                                                                               
GETAGY   NTR1                                                                   
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   RAGYKAGY(6),RCONKAGY                                             
         MVC   RAGYKREP,RCONKREP                                                
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETAGY20                                                         
         MVI   RAGYREC,0                                                        
         MVC   P(20),=C'***MISSING RECORD***'                                   
         MVC   P+24(7),=C'AGENCY:'                                              
         MVC   P+32(6),RCONKAGY                                                 
         GOTO1 REPORT                                                           
         B     GETAGYX                                                          
GETAGY20 LA    RF,RAGYREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
GETAGYX  XIT1                                                                   
*                                                                               
*                                                                               
NEW05ELT DC    X'0516',CL20'IMP ENTERPRISE HIST'                                
LNEW05   EQU   *-NEW05ELT                                                       
NEW1EELT DC    X'1E1610',19X'00'                                                
LNEW1E   EQU   *-NEW1EELT                                                       
NEW1FELT DC    X'1F18',4X'00',X'40',17X'00'                                     
LNEW1F   EQU   *-NEW1FELT                                                       
NEW20ELT DC    X'202F00001001',41X'00'                                          
LNEW20   EQU   *-NEW20ELT                                                       
NEWBUCK  DC    X'FF0A',8X'00'                                                   
LNEWBUCK EQU   *-NEWBUCK                                                        
         EJECT                                                                  
*                                                                               
*   SCANQUOT:  QUOTE-DELIMITED FIELD HAS BEEN ENCOUNTERED, WHICH                
*        HAS AN IMBEDDED COMMA.  SCAN FIELD AND CHANGE IMBEDDED                 
*        COMMA TO X'FF', WHICH WILL BE RESET TO COMMA WHEN                      
*        FIELD IS MOVED OUT.                                                    
*        R5  ->  FIRST '"' OF PAIR.  SKIP IT AND TERMINATE UPON                 
*                ENCOUNTERING NEXT '"'                                          
*                                                                               
SCANQUOT NTR1                                                                   
*                                                                               
*   TEST DISPLAY                                                                
*        MVC   P+1(08),=C'SCANQUOT'                                             
*        MVC   P+12(50),0(R5)                                                   
*        GOTO1 REPORT                                                           
*   TEST DISP END                                                               
*                                                                               
         LA    R5,1(R5)            SKIP FIRST '"'                               
SQUT0020 EQU   *                                                                
         CLI   0(R5),C'"'          QUOTE FOUND?                                 
         BE    SQUT0060            YES - FINISHED                               
         CLI   0(R5),C','          NO  - COMMA ENCOUNTERED?                     
         BNE   SQUT0040            NO                                           
         MVI   0(R5),X'FF'         YES - RESET IT TO X'FF'                      
SQUT0040 EQU   *                                                                
         LA    R5,1(R5)            BUMP TO NEXT POSITION                        
         B     SQUT0020            GO BACK TO CHECK NEXT POSITION               
SQUT0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
                                                                                
*                                                                               
*   INITIALIZATIONS ....                                                        
INITIAL  NTR1                                                                   
         OPEN  (INTAPE,(INPUT))                                                 
*                                                                               
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   INIT0010            NO  - DIRECT UPDATE                          
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
INIT0010 EQU   *                                                                
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET INITIAL IO AREA                          
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',500000,500000                                   
*                                  GET .5MEG STORAGE SPACE                      
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
*                                                                               
INIT0120 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  DISPIPUT:  DISPLAY THE RECORD LOCATED AT ARECAREA.                           
*                                                                               
DISPIPUT NTR1                                                                   
         GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD)                                    
         ZICM  RF,0(R4),2          SET L(INPUT RECORD)                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT                                                           
DPUT0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'INPUT RECORDS READ     :'                             
         EDIT  REDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OUTPUT RECORDS WRITTEN :'                             
         EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CORPORATE CODES ADDED  :'                             
         EDIT  CORPCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CORPORATES OVERWRITTEN :'                             
         EDIT  CORPOVER,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DUPLICATES ENCOUNTERED :'                             
         EDIT  DUPECTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
DITO0010 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         GOTO1 REPORT                                                           
DIPU0001 EQU   *                                                                
         MVC   P+1(8),=C'CONTRACT'                                              
         CLI   REC,X'0C'           AGENCY?                                      
         BE    DIPU0020            YES                                          
         DC    H'0'                                                             
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
PUTR0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
VREPFACS DS    A                                                                
AIPFIELD DS    A                   INPUT FIELD ADDRESS                          
AIPFIEL2 DS    A                   INPUT FIELD ADDRESS                          
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
ASTAAREA DS    A                                                                
ASTANEXT DS    A                                                                
REDCTR   DS    F                                                                
TOTCTR   DS    F                                                                
STARCTR  DS    F                                                                
DISPCTR  DS    F                                                                
CORPCTR  DS    F                                                                
DUPECTR  DS    F                                                                
CORPOVER DS    F                                                                
AIOAREA  DS    F                                                                
ENDOFREC DS    A                   END OF RECORD                                
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
CALLOV   DS    A                                                                
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
OLDNAME  DS    CL18                OLD AGENCY NAME                              
OLDNAM2  DS    CL20                OLD ADVERT NAME                              
KEYDA    DS    CL4                 KEY DISK ADDRESS                             
WORK2    DS    CL256                                                            
REQWORKA DS    CL128                                                            
ELTBILD1 DS    CL64                                                             
ELTBILD2 DS    CL64                                                             
ELTBILD3 DS    CL64                                                             
RUNID    DS    CL5                                                              
USERID   DS    CL4                                                              
FOXZEROS DC    C'0000000'                                                       
FOXALPHA DC    20X'C0'                                                          
REPUSE   DS    CL2                 PLUG-IN REP VALUE                            
QUOTES   DS    CL1                                                              
SAVOFFIC DS    CL2                                                              
**>PDETL DS    CL1                                                              
TCODESAV DS    CL10                                                             
LASTCON  DS    CL5                 PACKED                                       
         DS    0H                                                               
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32670,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=VB,LRECL=2000,             X        
               BLKSIZE=8000,MACRF=GM,EODAD=MAIN0800                             
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2048              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENPTP                POINT PERSON RECORD                          
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REGENCTL                STATION CTL RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT                                     
         ORG                                                                    
         EJECT                                                                  
RECORD3  DS    CL1024              ADD'L IO BUFFER                              
         EJECT                                                                  
         ORG   RECORD3                                                          
       ++INCLUDE REGENOFF          OFFICE RECORD                                
         ORG   RECORD3                                                          
       ++INCLUDE REGENTEM          OFFICE RECORD                                
         ORG   RECORD3                                                          
       ++INCLUDE REGENSTA          OFFICE RECORD                                
         ORG   RECORD3                                                          
       ++INCLUDE REGENADV          OFFICE RECORD                                
         ORG   RECORD3                                                          
       ++INCLUDE REGENAGY          OFFICE RECORD                                
         ORG   RECORD3                                                          
       ++INCLUDE REGENSAL          OFFICE RECORD                                
         EJECT                                                                  
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
         EJECT                                                                  
         CSECT                                                                  
*                                                                               
*********************************************************************           
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'205REREPBB02A09/09/99'                                      
         END                                                                    
