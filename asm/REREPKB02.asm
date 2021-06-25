*          DATA SET REREPKB02  AT LEVEL 117 AS OF 05/01/02                      
*PHASE REKB02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
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
         TITLE 'REREPKB02  (REKB02A) --- NBC AGENCY/ADVERT UPLOAD'              
*                                                                               
********************************************************************            
*                                                                  *            
*                                                                  *            
*        REREPKB02  -- NBC AGY/ADVERT UPLOAD:  ACCEPT E-MAIL FILE  *            
*                      AND SAVE AS CSV FROM EXCEL IN BUHR.BOOK2    *            
*                      CONVERT FROM BUHR.BOOK2 TO EITHER TAPE, OR  *            
*                      DIRECTLY INTO MAIN FILE.                    *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* SEP24/98 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
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
*  QOPTION1   =   AGENCY/ADVERT UPLOAD A = AGENCY CREATE           *            
*                                      V = ADVERT CREATE           *            
*  QOPTION2   =   DIRECT/MERGE FLAG    D = DIRECT UPDATE (DEFAULT) *            
*                                      M = TAPE O/P TO BE MERGED   *            
*  QOPTION3   =   UPDATE/TEST FLAG     U = UPDATE FILE             *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKB02   CSECT                                                                  
         NMOD1 0,**REKB**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0900                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC),RR=Y                                       
*                                                                               
         L     R5,ARECAREA                                                      
         ZIC   R4,QSTATION         GET SKIP COUNT FROM REQ CARD                 
         SLL   R4,28               DROP ZONE BITS                               
         SRL   R4,28               RESET COUNT                                  
         LTR   R4,R4               ANYTHING THERE?                              
         BZ    MAIN0060            NO  - DON'T SKIP ANYTHING                    
MAIN0040 EQU   *                   YES                                          
         GET   INTAPE,(R5)         READ TAPE RECORD INTO RDA                    
         BCT   R4,MAIN0040         SKIP N RECORDS                               
MAIN0060 EQU   *                                                                
*                                                                               
         XC    0(250,R5),0(R5)     CLEAR RECORD DELIVERY AREA                   
*                                                                               
*   TEST INPUT CUTOFF                                                           
         CLI   QUESTOR+2,C'Y'      CUT OFF AFTER N RECORDS?                     
         BNE   TEST0020            NO                                           
*                                                                               
         CLC   REDCTR,=F'099'      CUT OFF AFTER N RECORDS                      
         BE    MAIN0800                                                         
TEST0020 EQU   *                                                                
*   TEST INPUT CUTOFF END                                                       
*                                                                               
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
         CLI   QUESTOR+11,C'Y'     DISPLAY PERFORMANCE TIME STAMP?              
         BNE   MAIN0080            NO                                           
*                                                                               
         C     RF,=F'20'           DISPLAY EVERY N RECORDS                      
         BNE   MAIN0080                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  REDCTR,(10,P+15)    DISPLAY TOTAL COUNTER                        
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+55,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
*                                                                               
MAIN0080 EQU   *                                                                
*                                                                               
*                                  REINITIALIZE RECORDS                         
         XCEFL RECORD,1024                                                      
         XCEFL RECORD2,1024                                                     
*                                                                               
         MVI   RCSUBPRG,1          SET HEADINGS TO 'AGENCY'                     
         CLI   QOPTION1,C'A'       AGENCY UPDATE?                               
         BNE   MAIN0400            NO  - PROCESS ADVERTISER                     
*                                                                               
         XC    NEW20ELT+2(LNEW20-2),NEW20ELT+2                                  
         XC    NEW40ELT+2(LNEW40-2),NEW40ELT+2                                  
*                                                                               
*   SKELETONIZE RECORDS                                                         
*                                                                               
         MVI   RAGYKTYP,X'0A'      INSERT RECORD TYPE                           
         MVI   RAGK2TYP,X'1A'      INSERT RECORD TYPE                           
         MVC   RAGYELEM(2),=X'017A' INSERT ELEMENT 1 CODE                       
         MVC   RAGYLEN,=X'009C'    INSERT RECORD LENGTH                         
         MVC   RAGY2FXE(2),=X'1034' INSERT ELEMENT 10 CODE                      
         MVC   RAGY2LEN,=X'0056'   INSERT RECORD LENGTH                         
         MVC   RAGYKREP,QOFFICE    INSERT REP CODE FROM REQ CARD                
         MVC   RAGK2REP,QOFFICE    INSERT REP CODE FROM REQ CARD                
         GOTO1 DATCON,DMCB,(5,WORK),(2,RAGY2LCD)                                
*                                  INSERT LAST CHANGED DATE                     
         LA    R4,AGBLDTAB         SET A(AGENCY BUILD TABLE)                    
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
         CLI   0(R4),0             END OF TABLE?                                
         BE    MAIN0180            YES - DISPLAY AND OUTPUT RECORD              
*                                                                               
*   DATA FOR ANY FIELD MAY BE INSERTED INTO EITHER AGENCY                       
*        OR AGENCY2 RECORDS.                                                    
*                                                                               
         TM    AGRECTYP(R4),X'01'  INSERT INTO TYPE 1 RECORD?                   
         BNO   MAIN0120            NO                                           
         LA    R2,RECORD           SET A(AGENCY RECORD 1)                       
         BAS   RE,LOADDATA                                                      
MAIN0120 EQU   *                                                                
         TM    AGRECTYP(R4),X'02'  INSERT INTO TYPE 2 RECORD?                   
         BNO   MAIN0160            NO                                           
         TM    AGRECTYP(R4),X'01'  WAS FIRST RECORD UPDATED BY FIELD?           
         BNO   MAIN0140            NO                                           
         MVC   AIPFIELD,AIPFIEL2   YES - RESET A(FIELD)                         
MAIN0140 EQU   *                                                                
         LA    R2,RECORD2          SET A(AGENCY RECORD 2)                       
         BAS   RE,LOADDATA                                                      
         MVC   AIPFIEL2,AIPFIELD   RESET A(FIELD2)                              
MAIN0160 EQU   *                                                                
         LA    R4,LAGBLDNT(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0100            GO BACK FOR NEXT TABLE ENTRY                 
MAIN0180 EQU   *                                                                
         OI    RAGYFLAG,X'90'      TURN ON 'NBC UPLOAD' FLAG                    
*                                     AND USE EXPANDED ADDRS                    
         MVI   RAGYPROS,C'N'       TURN ON PROFILES                             
         MVC   RAGYPROS+1(9),RAGYPROS                                           
         CLC   RAGYNAM1,SPACES     AGENCY NAME 1 ENTERED?                       
         BNE   MAIN0200            YES                                          
         MVC   RAGYNAM1,RAGYNAM2   NO  - LOAD CONTRACT NAME TO SCREEN           
MAIN0200 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RAGY2REC,NEW20ELT,0            
*                                  INSERT NEW 20 ELT                            
         CLI   NEW40ELT+1,2        IS COMMENT ELT EMPTY?                        
         BE    MAIN0220            YES - DON'T INSERT IT                        
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RAGY2REC,NEW40ELT,0            
*                                  INSERT NEW 40 ELT                            
MAIN0220 EQU   *                                                                
         CLC   RAGYKAOF,=C'  '     ANY AGENCY OFFICE?                           
         BE    MAIN0222            NO                                           
         MVC   SAVOFFIC,RAGYKAOF   YES - SAVE AGENCY OFFICE                     
         MVC   RAGYKAOF,SPACES     CLEAR OFFICE FOR CORPORATE LOOK              
         MVC   RAGK2AOF,SPACES     CLEAR OFFICE FOR CORPORATE LOOK              
         GOTO1 AGYRECOP,DMCB,(1,0) PROCESS CORPORATE RECORD                     
         MVC   RAGYKAOF,SAVOFFIC   REINSERT OFFICE FOR DETAIL REC               
         MVC   RAGK2AOF,SAVOFFIC   REINSERT OFFICE FOR DETAIL REC               
MAIN0222 EQU   *                                                                
         GOTO1 AGYRECOP,DMCB,(0,0) PROCESS DETAIL RECORD                        
         B     MAIN0060            GO BACK FOR NEXT                             
         EJECT                                                                  
*                                                                               
*   ADVERTISER RECORD PROCESSING                                                
*                                                                               
***>>>                                                                          
MAIN0400 EQU   *                                                                
*                                                                               
*   SKELETONIZE RECORDS:  SET TO BINARY ZERO                                    
*                                                                               
         MVI   RCSUBPRG,2          SET HEADINGS TO ADVERTISER                   
         XC    TCODESAV,TCODESAV   CLEAR T-CODE SAVE AREA                       
         MVI   RADVKTYP,X'08'      INSERT RECORD TYPE                           
         MVC   RADVELEM(2),=X'0142' INSERT ELEMENT 1 CODE                       
         MVC   RAGYLEN,=X'0064'    INSERT RECORD LENGTH                         
         MVC   RADVKREP,QOFFICE    INSERT REP CODE FROM REQ CARD                
         MVC   RADVCITY,SPACES     CLEAR TO SPACES                              
         MVC   RADVCLSS,SPACES     CLEAR TO SPACES                              
         GOTO1 DATCON,DMCB,(5,WORK),(2,RADVLCD)                                 
*                                  INSERT LAST CHANGED DATE                     
         LA    R4,ADBLDTAB         SET A(ADVERT BUILD TABLE)                    
*                                  R5-->A(INPUT DATA)                           
         LR    RF,R5               CALCULATE A(END OF RECORD)                   
         ZICM  RE,0(R5),2          GET L(RECORD INPUT)                          
         AR    RF,RE                                                            
         ST    RF,ENDOFREC         SAVE END OF RECORD                           
*                                                                               
         LR    RF,R5               RESET A(RECORD INPUT)                        
         LA    RF,4(RF)            SKIP CONTROL OF I/P                          
         ST    RF,AIPFIELD         SET A(1ST INPUT FIELD)                       
MAIN0420 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    MAIN0500            YES - DISPLAY AND OUTPUT RECORD              
         TM    AGRECTYP(R4),X'01'  INSERT INTO TYPE 1 RECORD?                   
         BNO   MAIN0440            NO                                           
         LA    R2,RECORD           SET A(AGENCY RECORD 1)                       
         BAS   RE,LOADDATA                                                      
         B     MAIN0480                                                         
MAIN0440 EQU   *                                                                
         DC    H'0'                ALL SHOULD BE SET                            
MAIN0480 EQU   *                                                                
         LA    R4,LAGBLDNT(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0420            GO BACK FOR NEXT TABLE ENTRY                 
MAIN0500 EQU   *                                                                
         OI    RADVFLGS,X'10'      TURN ON 'NBC UPLOAD' FLAG                    
         MVC   RADVCLSS,SPACES     CLEAR CLASS FIELD                            
*                                                                               
         CLC   =C'T=',TCODESAV     IS A TCODE PRESENT?                          
         BNE   MAIN0520            NO                                           
         MVC   RADVTCOD(5),TCODESAV+2                                           
*                                  YES - SAVE IT                                
MAIN0520 EQU   *                                                                
         GOTO1 ADVRECOP,DMCB,(0,0) PROCESS DETAIL RECORD                        
         B     MAIN0060            GO BACK FOR NEXT                             
         EJECT                                                                  
***>>>                                                                          
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
*        IF CORPORATE RECORD IS ALREADY ON FILE, THE ROUTINE                    
*        IS EXITED.  REGULAR RECORD IS UPDATED OR ADDED AS                      
*        APPROPRIATE.                                                           
*                                                                               
AGYRECOP NTR1                                                                   
         MVC   CORPDETL,0(R1)      SAVE CORP (1)/DETAIL (0) FLAG                
         CLI   CORPDETL,1          CORPORATE RECORD?                            
         BNE   AGOP0010            NO                                           
         OI    RAGYFLAG,X'08'      YES - TURN ON INDICATOR                      
AGOP0010 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY OUTPUT?                              
         BNE   AGOP0030            NO                                           
         MVC   P+1(24),=C'RECORD1 CREATED FROM I/P'                             
         CLI   CORPDETL,1          CORPORATE RECORD?                            
         BNE   AGOP0020            NO                                           
         MVC   P+1(24),=C'CORPORATE1      FROM I/P'                             
AGOP0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         MOVE  (REC,300),RECORD    MOVE AGENCY REC TO O/P AREA                  
         ZICM  RF,RAGYLEN,2        SET O/P LENGTH                               
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
         XC    OLDNAME,OLDNAME     CLEAR OLD AGENCY NAME                        
         MVC   KEY(27),RECORD      CHECK: IS RECORD ALREADY ON FILE?            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGOP0060            NO                                           
         CLI   CORPDETL,1          CORPORATE RECORD?                            
         BE    AGOP0280            YES - LEAVE PREVIOUS RECORD                  
*                                  NO  - CHECK IF PREVIOUS WAS                  
*                                     CREATED FROM AGY/OFF RECORD               
         LA    RF,RECORD3          SET NEW IOAREA                               
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                READ ORIGINAL RECORD                         
         LA    RF,RECORD3          SET NEW IOAREA                               
         TM    RAGYFLAG-RAGYREC(RF),X'08'                                       
*                                  CREATED FROM AGY/OFF RECORD?                 
         BO    AGOP0050            YES - REPLACE IT                             
         B     AGOP0300            NO  - FLAG IT ON REPORT AS DUPE              
AGOP0050 EQU   *                                                                
         L     RF,CORPOVER         BUMP CORPORATES OVERWRITTEN                  
         LA    RF,1(RF)                                                         
         ST    RF,CORPOVER                                                      
         LA    RF,RECORD3          RESET A(ORIGINAL RECORD)                     
         MVC   OLDNAME,RAGYNAM1-RAGYREC(RF)                                     
*                                  SAVE ORIG NAME FOR 8A KEY CHECK              
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0100            NO  - SKIP REWRITE                           
         GOTO1 PREC                REWRITE RECORD                               
         B     AGOP0100                                                         
AGOP0060 EQU   *                                                                
         LA    RF,RECORD           SET A(IO AREA)                               
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0070            NO  - SKIP O/P                               
         GOTO1 AREC                                                             
AGOP0070 EQU   *                                                                
         MVC   P+1(17),=C'ADD       AGENCY:'                                    
         CLI   CORPDETL,1          CORPORATE RECORD?                            
         BNE   AGOP0075            NO                                           
         MVC   P+5(04),=C'CORP'    YES - INSERT CORPORATE IND                   
         L     RF,CORPCTR          BUMP CORPORATE RECORDS OUTPUT                
         LA    RF,1(RF)                                                         
         ST    RF,CORPCTR                                                       
AGOP0075 EQU   *                                                                
         MVC   P+20(4),RAGYKAGY    DISPLAY AGENCY CODE                          
         MVC   P+26(2),RAGYKAOF    DISPLAY AGENCY OFFICE CODE                   
         MVC   P+32(33),RAGYNAM2   AGENCY NAME                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   KEY(27),RECORD      SET UP X'8A' KEY                             
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(18),RAGYNAM1  INSERT AGENCY NAME                           
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   AGOP0080            NO  - SKIP O/P                               
         GOTO1 ADD                 ADD NEW KEY:  D/A S/B SET                    
AGOP0080 EQU   *                                                                
         L     RF,TOTCTR           BUMP TOTAL COUNT                             
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         B     AGOP0200                                                         
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
AGOP0200 EQU   *                                                                
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
AGOP0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADVRECOP:  SUBROUTINE TO GENERATE OUTPUT FOR THIS RUN.                      
*                                                                               
ADVRECOP NTR1                                                                   
         CLI   QUESTOR+3,C'Y'      DISPLAY OUTPUT?                              
         BNE   ADOP0020            NO                                           
         MVC   P+1(24),=C'ADV REC CREATED FROM I/P'                             
         GOTO1 REPORT                                                           
         MOVE  (REC,300),RECORD    MOVE AGENCY REC TO O/P AREA                  
         ZICM  RF,RADVLEN,2        SET O/P LENGTH                               
         LA    RF,4(RF)            BUMP LENGTH BY 2                             
         STCM  RF,3,REC-4                                                       
         GOTO1 =A(DISPPUT),DMCB,(RC),RR=Y                                       
*                                  DISPLAY ADVERT RECORD                        
ADOP0020 EQU   *                                                                
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   ADOP0040            NO  - PUT DIRECTLY TO FILE                   
         GOTO1 PUTRECS             YES                                          
         B     ADOP0200                                                         
ADOP0040 EQU   *                                                                
         MVC   KEY(27),RECORD      CHECK: IS RECORD ALREADY ON FILE?            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   ADOP0060            NO                                           
         B     ADOP0400                                                         
***                                                                             
*   DUPLICATE KEYS ARE NOT BEING REWRITTEN:  THEY ARE BEING FLAGGED.            
***                                                                             
         LA    RF,RECORD3          SET NEW IOAREA                               
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                READ ORIGINAL RECORD                         
         MVC   OLDNAM2,RADVNAME-RADVREC(RF)                                     
*                                  SAVE ORIG NAME FOR 88 KEY CHECK              
         LA    RF,RECORD           RESET ORIGINAL IO AREA                       
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0100            NO  - SKIP REWRITE                           
         GOTO1 PREC                REWRITE RECORD                               
         B     ADOP0100                                                         
ADOP0060 EQU   *                                                                
         LA    RF,RECORD           SET A(IO AREA)                               
         ST    RF,AIOAREA                                                       
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0070            NO  - SKIP O/P                               
         GOTO1 AREC                                                             
ADOP0070 EQU   *                                                                
         MVC   P+1(21),=C'ADD       ADVERTISER:'                                
         MVC   P+25(4),RADVKADV    DISPLAY ADVERT CODE                          
         MVC   P+40(20),RADVNAME   AGENCY NAME                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   KEY(27),RECORD      SET UP X'88' KEY                             
         MVI   KEY,X'88'                                                        
         MVC   KEY+1(20),RADVNAME  INSERT ADVERT NAME                           
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0080            NO  - SKIP O/P                               
         GOTO1 ADD                 ADD NEW KEY:  D/A S/B SET                    
ADOP0080 EQU   *                                                                
         L     RF,TOTCTR           BUMP TOTAL COUNT                             
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         B     ADOP0200                                                         
ADOP0100 EQU   *                                                                
*                                                                               
         MVC   KEYDA,KEY+28        SAVE DISK ADDRESS                            
         MVC   KEY(27),RECORD                                                   
         MVI   KEY,X'88'           INSERT PASSIVE KEY TYPE                      
         MVC   KEY+1(20),OLDNAM2   INSERT ORIG NAME                             
         GOTO1 HIGH                IS KEY ON FILE?                              
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    ADOP0120            YES                                          
         MVC   KEY(27),KEYSAVE     NO  - RESTORE KEY                            
         MVC   KEY+28(4),KEYDA     INSERT NEW D/A                               
         B     ADOP0180                                                         
ADOP0120 EQU   *                                                                
         OI    KEY+27,X'80'        YES - TURN ON DELETE BIT                     
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0140            NO  - SKIP O/P                               
         GOTO1 WRITE               REWRITE THE KEY 'DELETED'                    
ADOP0140 EQU   *                                                                
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         MVC   KEY+1(20),RADVNAME  INSERT NEW NAME                              
         MVC   KEY+28(4),KEYDA     INSERT NEW D/A                               
         GOTO1 HIGH                CHECK FOR EXISTENCE                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   ADOP0180            NO                                           
         MVC   KEY+28(4),KEYDA     YES - INSERT NEW D/A                         
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0160            NO  - SKIP O/P                               
         GOTO1 WRITE               REWRITE KEY                                  
ADOP0160 EQU   *                                                                
         B     ADOP0200                                                         
ADOP0180 EQU   *                                                                
         CLI   QOPTION3,C'U'       HARD UPDATE?                                 
         BNE   ADOP0200            NO  - SKIP O/P                               
         GOTO1 ADD                 ADD NEW KEY                                  
ADOP0200 EQU   *                                                                
         XIT1                                                                   
ADOP0400 EQU   *                                                                
         MVC   P+1(21),=C'DUPLICATE ADVERTISER:'                                
         MVC   P+25(4),RADVKADV    DISPLAY ADVERT CODE                          
         MVC   P+40(20),RADVNAME   AGENCY NAME                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  AGENCY BUILD TABLE:  A SERIES OF ENTRIES WHICH CORRESPOND WITH               
*        THE DATA DELIVERED FROM THE UPLOAD RECORD.  EACH ENTRY                 
*        IS SET UP AS:                                                          
*        POS   1      =  INSERT IN OUTPUT RECORD INDICATOR:                     
*                        X'01'  =  X'0A' RECORD                                 
*                        X'02'  =  X'1A' RECORD                                 
*                        CAN BE IN BOTH                                         
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
*   AGENCY BUILD TABLE COMPONENT                                                
*                                                                               
AGBLDTAB EQU   *                                                                
         DC    X'3',AL2(RAGYKAGY-RAGYREC),X'00',C' ',AL1(4),X'00'               
*                                  AGENCY CODE                                  
LAGBLDNT EQU   *-AGBLDTAB                                                       
         DC    X'3',AL2(RAGYKAOF-RAGYREC),X'00',C' ',AL1(2),X'00'               
*                                  AGENCY OFFICE CODE                           
         DC    X'1',AL2(RAGYNAM1-RAGYREC),X'00',C' ',AL1(20),X'00'              
*                                  AGENCY NAME 1:  SCREEN                       
         DC    X'1',AL2(RAGYNAM2-RAGYREC),X'00',C' ',AL1(33),X'00'              
*                                  AGENCY NAME 1:  CONTRACT                     
         DC    X'2',AL2(RAGY2CTX-RAGY2CMT),X'40',C' ',AL1(60),X'01'             
*                                  COMMENT                                      
         DC    X'2',AL2(RAGY2AD1-RAGY2AE1),X'20',C' ',AL1(34),X'00'             
*                                  ADDRESS 1                                    
         DC    X'2',AL2(RAGY2AD2-RAGY2AE1),X'20',C' ',AL1(34),X'00'             
*                                  ADDRESS 2                                    
         DC    X'1',AL2(RAGYCITY-RAGYREC),X'00',C' ',AL1(20),X'00'              
*                                  CITY                                         
         DC    X'1',AL2(RAGYSTAT-RAGYREC),X'00',C' ',AL1(2),X'00'               
*                                  STATE                                        
         DC    X'1',AL2(RAGYZIP-RAGYREC),X'00',C' ',AL1(10),X'00'               
*                                  ZIP                                          
         DC    X'0000'             DELIMITER                                    
         DS    0H                  ALIGNMENT                                    
*                                                                               
*   ADVERT BUILD TABLE COMPONENT                                                
*                                                                               
ADBLDTAB EQU   *                                                                
         DC    X'1',AL2(RADVKADV-RADVREC),X'00',C' ',AL1(4),X'00'               
*                                  ADVERT CODE                                  
         DC    X'1',AL2(RADVNAME-RADVREC),X'00',C' ',AL1(20),X'00'              
*                                  ADVERT NAME :                                
         DC    X'1',AL2(RADVREC-RADVREC),X'60',X'00',AL1(07),X'00'              
*                                  0 BYTES DISPLACEMENT                         
*                                  NBC TCODE - MISC INFO                        
         DC    X'1',AL2(RADVCLSS-RADVREC),X'00',C' ',AL1(02),X'00'              
*                                  ADDRESS 1                                    
         DC    X'1',AL2(RADVCATG-RADVREC),X'00',C' ',AL1(02),X'00'              
*                                  CITY                                         
         DC    X'0000'             DELIMITER                                    
         DS    0H                  ALIGNMENT                                    
         EJECT                                                                  
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
         CLI   AGRECELT(R4),0      ELEMENT BEING BUILT?                         
         BZ    LDAT0040            NO  - USE RECORD ADDRESS PASSED IN           
         LA    R2,NEW20ELT         YES - SET A(20 ELEMENT)                      
         CLI   AGRECELT(R4),X'20'  20 ELEMENT?                                  
         BE    LDAT0040            YES                                          
         LA    R2,NEW40ELT         NO  - SET A(40 ELEMENT)                      
         CLI   AGRECELT(R4),X'40'  40 ELEMENT?                                  
         BE    LDAT0040            YES                                          
         LA    R2,TCODESAV         NO  - SET A(TCODE SAVE)                      
         CLI   AGRECELT(R4),X'60'  TCODE SAVE?                                  
         BE    LDAT0040            YES                                          
         DC    H'0'                NO  - UNIDENTIFIED                           
LDAT0040 EQU   *                                                                
         ZIC   RF,AGRECMAX(R4)     GET FIELD LENGTH                             
         BCTR  RF,0                SET FOR EX                                   
         LR    R3,R2               SET FIELD DISPLACEMENT                       
         ZICM  RE,AGRECDIS(R4),2   GET DISP(REC/ELT)                            
         AR    R3,RE               SET A(RECEIVING FIELD)                       
         CLI   AGRECFIL(R4),0      FILL = BINARY ZERO?                          
         BNE   LDAT0060            NO  -                                        
         EX    RF,LDAT0800         YES - SET FIELD TO BIN ZERO                  
         B     LDAT0080                                                         
LDAT0060 EQU   *                                                                
         MVI   0(R3),C' '          INSERT FIRST SPACE                           
         BCTR  RF,0                SET DOWN FOR EX                              
         EX    RF,LDAT0820         SET FIELD TO SPACES                          
LDAT0080 EQU   *                                                                
         ZIC   RF,AGRECMAX(R4)     GET MAXIMUM FIELD LENGTH                     
         SR    R6,R6               CLEAR COUNTER                                
         MVI   QUOTES,C'N'         SET 'QUOTES STRING' TO 'NO'                  
*                                                                               
*   TEST                                                                        
         CLC   =C'ABSM',0(R5)                                                   
         BNE   LDAT0100                                                         
         SR    R6,R6                                                            
*   TEST END                                                                    
LDAT0100 EQU   *                                                                
         CLI   0(R5),C'"'          QUOTES STRING FOUND?                         
         BNE   LDAT0120            NO                                           
         CLI   QUOTES,C'Y'         FIRST QUOTES FOUND?                          
         BE    LDAT0180            YES - DON'T REDO                             
*                                     DON'T MOVE THIS FIELD OUT                 
         MVI   QUOTES,C'Y'         SET 'QUOTES STRING' TO 'YES'                 
         BAS   RE,SCANQUOT         SCAN/PREP THIS FIELD                         
         B     LDAT0180            DON'T MOVE THIS FIELD OUT                    
LDAT0120 EQU   *                                                                
         CLI   0(R5),C','          SEPARATOR FOUND?                             
         BE    LDAT0200            YES - FIELD FINISHED                         
         C     R5,ENDOFREC         END OF RECORD REACHED?                       
         BE    LDAT0200            YES - FIELD/RECORD FINISHED                  
         CLI   0(R5),X'FF'         RESET ',' FOUND?                             
*                                     (SEE SCANQUOT)                            
         BNE   LDAT0140            NO                                           
         MVI   0(R5),C','          YES - RESET IT BEFORE MOVE                   
LDAT0140 EQU   *                                                                
         CR    R6,RF               LENGTH COUNT VS MAX COUNT                    
         BH    LDAT0160            RECEIVING FLD LEN EXCEEDED                   
         MVC   0(1,R3),0(R5)                                                    
         LA    R3,1(R3)            BUMP RECEIVING FIELD                         
         LA    R6,1(R6)            BUMP LENGTH COUNT                            
LDAT0160 EQU   *                                                                
         LA    R5,1(R5)            BUMP SENDING   FIELD                         
         B     LDAT0100            GO BACK FOR NEXT CHARACTER                   
LDAT0180 EQU   *                                                                
*                                  SKIP '"' AND DON'T COUNT IN LEN              
         LA    R5,1(R5)            BUMP SENDING   FIELD                         
         B     LDAT0100            GO BACK FOR NEXT FIELD                       
LDAT0200 EQU   *                                                                
         LA    R5,1(R5)            PASS SEPARATOR                               
         ST    R5,AIPFIELD         SET TO A(NEXT FIELD)                         
         CLI   AGRECELT(R4),X'40'  40 ELEMENT?                                  
         BNE   LDAT0220            NO                                           
         LA    R6,2(R6)            YES - ADD L(CONTROL)                         
         STC   R6,NEW40ELT+1       INSERT LENGTH INTO ELT                       
LDAT0220 EQU   *                                                                
LDAT0240 EQU   *                                                                
         XIT1                                                                   
LDAT0800 EQU   *                                                                
         XC    0(0,R3),0(R3)       SET FIELD TO BIN ZERO                        
LDAT0820 EQU   *                                                                
         MVC   1(0,R3),0(R3)       SET FIELD TO SPACES                          
*                                                                               
NEW20ELT DC    X'2046',68X'00'                                                  
LNEW20   EQU   *-NEW20ELT                                                       
NEW40ELT DC    X'403E',60X'00'                                                  
LNEW40   EQU   *-NEW40ELT                                                       
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
         MVC   P+1(8),=C'AGENCY  '                                              
         CLI   REC,X'0A'           AGENCY?                                      
         BE    DIPU0020            YES                                          
         CLI   REC,X'1A'           AGENCY?                                      
         BE    DIPU0020            YES                                          
         MVC   P+1(8),=C'ADVERT  '                                              
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
CORPDETL DS    CL1                                                              
TCODESAV DS    CL10                                                             
         DS    0H                                                               
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=VB,LRECL=255,              X        
               BLKSIZE=6233,MACRF=GM,EODAD=MAIN0800                             
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
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERTISER RECORD                            
         ORG                                                                    
         EJECT                                                                  
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY2         AGENCY2 RECORD                               
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL1024              ADD'L IO BUFFER                              
         EJECT                                                                  
         ORG   RECORD3                                                          
       ++INCLUDE REGENOFF          OFFICE RECORD                                
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
         CSECT                                                                  
*                                                                               
*********************************************************************           
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117REREPKB02 05/01/02'                                      
         END                                                                    
