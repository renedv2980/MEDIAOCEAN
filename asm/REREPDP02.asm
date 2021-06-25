*          DATA SET REREPDP02  AT LEVEL 030 AS OF 07/14/04                      
*PHASE REDP02A,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
         TITLE 'REREPDP02 - DARE RECORD PURGE'                                  
*********************************************************************           
*                                                                   *           
*        REREPDP02 --- DARE RECORD PURGE                            *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JUN02/03 (BU ) --- INITIAL ENTRY                                  *           
* SEP02/03 (BU ) --- SUBREP PROCESSING: SINGLE REP ONLY             *           
* JUL06/04 (BU ) --- ACCEPT AS-AT DATE                              *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*    QOPTION1     = 6     BACK DATE UP SIX MONTHS                   *           
*                 = 1     BACK DATE UP ONE YEAR                     *           
*                         DEFAULT IS BACK UP TWO QUARTERS           *           
*                                                                   *           
*                                                                   *           
*    QUESTOR      = Y     DISPLAY PASSIVE BLOCK                     *           
*    QUESTOR+1    = Y     DISPLAY RECORDS/KEYS PROCESSED            *           
*    QUESTOR+2    = Y     SHORT RUN BASED ON RECORD COUNT           *           
*    QUESTOR+3    = Y     DISPLAY CONTRACTS NOT FOUND               *           
*    QUESTOR+4    = Y     DISPLAY DARE ORDER #S W/OUT CON#S         *           
*    QUESTOR+5    = Y     DISPLAY MAKEGOOD INFORMATION              *           
*    QUESTOR+6    = Y     DISPLAY MAKEGOOD RECORDS                  *           
*    QUESTOR+7    = Y     DISPLAY BADHHDR RECS, MISSING 1D ELTS     *           
*    QUESTOR+8    = Y     DISPLAY DARE ELT PRE/POST RECORDS         *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
REDP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REDP02,R7,R9,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
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
         XIT1                                                                   
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         XIT1                                                                   
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
*                                                                               
*   ALL PROCESSING IS DONE FROM REQFRST                                         
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
***      DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(REQDONE)    END OF REPORT                       
         DC    AL1(RUNLAST),AL3(RUNDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
*                                                                               
*TEST*   GOTOX LOADER,DMCB,=CL8'T00AACA',0                                      
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   VREPFACS,4(R1)      LOADED REPFACS                               
         OC    VREPFACS,VREPFACS   REPFACS FOUND?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL IT                                
*                                                                               
*   SET CUTOFF DATE                                                             
*                                                                               
         CLC   QSTART,SPACES       ANY DATE ENTERED?                            
         BNH   INIT0020            NO                                           
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(3,STARTBIN)                                
*                                  CUTOFF DATE: BINARY                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,STARTCOM)                                
*                                  CUTOFF DATE: COMPRESSED                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         B     INIT0180                                                         
*                                                                               
INIT0020 EQU   *                                                                
*   DATE SETUP:  DEFAULT PURGE DATE IS QUARTER BASED.  OPTIONALLY,              
*        THERE IS A SIX-MONTH PURGE, WHICH MAY BE ACCESSED BY                   
*        SETTING QOPTION1 TO '6', AND A 1-YEAR PURGE WHICH MAY BE               
*        ACCESSED BY SETTING QOPTION1 TO '1'.                                   
*                                                                               
         CLI   QOPTION1,C'6'       USE SIX-MONTH AGING?                         
         BNE   INIT0060            NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+20)                                 
*                                  GET TODAY'S DATE                             
         MVC   P+1(27),=C'DATE: BACKING UP SIX MONTHS'                          
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*   PURGE WILL DROP ALL DARE THAT TERMINATED MORE THAN SIX MONTHS               
*        PREVIOUSLY.  THIS IS A SAMPLE OF THE CALCULATION:                      
*        1. DATE OF RUN        =  JAN15/2003                                    
*        2. DAY SET TO 01      =  JAN01/2003                                    
*        3. MONTH BACKED UP 6  =  JUL01/2003                                    
*        4. YEAR  BACKED UP 1  =  JUL01/2002  (IF NEEDED)                       
*   ALL DARE RECORDS WITH AN LTC PRIOR TO JUL01/2002 WILL BE DROPPED.           
*                                                                               
         MVI   WORK+22,1           SET DAY TO 01                                
         ZIC   RE,WORK+20          STRIP YEAR                                   
         ZIC   RF,WORK+21          STRIP MONTH                                  
         CLI   WORK+21,7           MONTH JULY OR AFTER?                         
         BNL   INIT0040            YES                                          
         AH    RF,=H'12'           NO  - ADJUST FOR SUBTRACTION                 
         BCTR  RE,0                BACK YEAR UP 1                               
         STC   RE,WORK+20          REPLACE YEAR                                 
INIT0040 EQU   *                                                                
         SH    RF,=H'6'            BACK UP SIX MONTHS                           
         STC   RF,WORK+21          REPLACE MONTH                                
         MVC   STARTBIN,WORK+20    SAVE BINARY CUTOFF DATE                      
         GOTO1 DATCON,DMCB,(3,WORK+20),(2,STARTCOM)                             
*                                  SET CUTOFF DATE: COMPRESSED                  
         B     INIT0180                                                         
INIT0060 EQU   *                                                                
         CLI   QOPTION1,C'1'       USE ONE-YEAR  AGING?                         
         BNE   INIT0080            NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+20)                                 
*                                  GET TODAY'S DATE                             
*                                                                               
*                                                                               
*   BACK UP ONE YEAR                                                            
*                                                                               
*                                                                               
         MVC   P+1(27),=C'DATE: BACKING UP  12 MONTHS'                          
         MVC   P+30(3),WORK+20                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVI   WORK+22,1           SET DAY TO 01                                
         ZIC   RE,WORK+20          STRIP YEAR                                   
         BCTR  RE,0                BACK UP 1 YEAR                               
         STC   RE,WORK+20                                                       
         MVC   STARTBIN,WORK+20    SAVE BINARY CUTOFF DATE                      
         GOTO1 DATCON,DMCB,(3,WORK+20),(2,STARTCOM)                             
*                                  SET CUTOFF DATE: COMPRESSED                  
         B     INIT0180                                                         
INIT0080 EQU   *                                                                
*                                                                               
*   A NOTE ABOUT AS-AT DATE USE:  THIS IS FOR TESTING ONLY.  WHEN               
*        AN AS-AT DATE IS ENTERED, IT ALSO SETS "TODAY'S DATE" TO               
*        THE AS-AT DATE, TO GET THE PROPER AGING.  THIS COULD                   
*        RESULT IN THE INCORRECT DELETION OF LIVE DATA, IF IT                   
*        WERE TO BE ENTERED FOR A LIVE RUN.                                     
*                                                                               
         CLC   QASAT,SPACES        ANY AS AT DATE ENTERED?                      
         BNH   INIT0090            NO                                           
         GOTO1 DATCON,DMCB,(0,QASAT),(X'20',WORK+20)                            
*                                  YES  - CONVERT IT                            
         B     INIT0095                                                         
INIT0090 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',WORK+20)                             
*                                  GET TODAY'S DATE IN EBCDIC                   
INIT0095 EQU   *                                                                
*                                                                               
*   TEST DATE                                                                   
***>>>   MVC   WORK+20(6),=C'030521'                                            
*   TEST DATE END                                                               
*                                                                               
*   BACK UP ON A QUARTERLY BASIS:  TWO QUARTERS                                 
*                                                                               
*                                                                               
         MVC   P+1(27),=C'DATE: BACKING UP  QUARTERLY'                          
         MVC   P+30(3),WORK+20                                                  
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(0,WORK+20),WORK+32,0,0                        
*                                  GET TODAY'S BROADCAST MONTH                  
         LA    RF,MOTABLE          SET A(ADJUSTMENT TABLE)                      
INIT0100 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
*   TEST                                                                        
         ST    RF,SAVERF                                                        
         MVC   P+1(23),=C'END BCST: XX  TABLE: XX'                              
         MVC   P+11(2),WORK+40                                                  
         MVC   P+22(2),0(RF)                                                    
         MVC   P+30(12),WORK+32                                                 
         GOTO1 REPORT                                                           
         L     RF,SAVERF                                                        
*   TEST                                                                        
*                                                                               
         CLC   WORK+40(2),0(RF)    END BCST DATE MO VS TABLE                    
         BL    INIT0120            TABLE ENTRY FOUND                            
         LA    RF,LMOTABLE(RF)     BUMP TABLE                                   
         B     INIT0100            GO BACK FOR NEXT                             
SAVERF   DS    F                                                                
INIT0120 EQU   *                                                                
         MVC   WORK+40(2),2(RF)    REPLACE MONTH                                
         MVC   WORK+48(1),4(RF)    SAVE YEAR ADJUST FLAG                        
         GOTO1 DATCON,DMCB,(0,WORK+38),(3,WORK)                                 
         CLI   WORK+48,0           ANY YEAR ADJUSTMENT?                         
         BE    INIT0140            NO                                           
         ZIC   RF,WORK             GET YEAR FROM DATE                           
         BCTR  RF,0                BACK UP 1 YEAR                               
         STC   RF,WORK                                                          
INIT0140 EQU   *                                                                
         MVI   WORK+2,15           SET DAY TO MIDMONTH                          
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+20)                                 
*                                  CONVERT TO EBCDIC                            
         GOTO1 =V(GETBROAD),DMCB,(0,WORK+20),WORK+32,0,0                        
*                                  GET MONTH'S BROADCAST MONTH                  
         GOTO1 DATCON,DMCB,(0,WORK+32),(3,STARTBIN)                             
*                                  CONVERT TO BINARY                            
         GOTO1 DATCON,DMCB,(0,WORK+32),(2,STARTCOM)                             
*                                  CONVERT TO COMPRESSED                        
         B     INIT0160                                                         
*                                                                               
*   MOTABLE IS SET UP IN THIS FASHION:                                          
*        POS 0    =   TODAY'S BROADCAST MONTH                                   
*        POS 1    =   CUTOFF DATE BROADCAST MONTH                               
*        POS 2    =   ADJUST YEAR FLAG:  0 = NO, 1 = YES                        
*   EVERYTHING PRIOR TO THE CUTOFF DATE IS DROPPED.  THEREFORE,                 
*        THE CUTOFF DATE IS SET TO THE FIRST DAY OF THE MONTH                   
*        FOLLOWING THE QUARTER TO BE DROPPED.                                   
*                                                                               
MOTABLE  DC    C'04',C'07',X'01'                                                
LMOTABLE EQU   *-MOTABLE                                                        
         DC    C'07',C'10',X'01'                                                
         DC    C'10',C'01',X'00'                                                
         DC    C'13',C'04',X'00'                                                
         DC    X'0000'                                                          
         DS    0H                                                               
INIT0160 EQU   *                                                                
         B     INIT0180                                                         
INIT0180 EQU   *                                                                
         MVC   P+1(12),=C'CUTOFF DATE='                                         
         GOTO1 DATCON,DMCB,(3,STARTBIN),(5,P+15)                                
         GOTO1 DATCON,DMCB,(2,STARTCOM),(5,P+25)                                
         GOTO1 REPORT                                                           
         GOTO1 DATCON,DMCB,(3,STARTBIN),(0,WORK)                                
*                                                                               
*   A NOTE ABOUT AS-AT DATE USE:  THIS IS FOR TESTING ONLY.  WHEN               
*        AN AS-AT DATE IS ENTERED, IT ALSO SETS "TODAY'S DATE" TO               
*        THE AS-AT DATE, TO GET THE PROPER AGING.  THIS COULD                   
*        RESULT IN THE INCORRECT DELETION OF LIVE DATA, IF IT                   
*        WERE TO BE ENTERED FOR A LIVE RUN.                                     
*                                                                               
         CLC   QASAT,SPACES        ANY AS AT DATE ENTERED?                      
         BNH   INIT0190            NO                                           
         GOTO1 DATCON,DMCB,(0,QASAT),(X'20',WORK+6)                             
*                                  YES  - CONVERT IT                            
         B     INIT0195                                                         
INIT0190 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,WORK+6),(0,WORK+6)                                
INIT0195 EQU   *                                                                
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
         MVC   P+1(21),=C'CUTOFF AGE = XXX DAYS'                                
         MVC   HALF,DMCB+8         GET # DAYS INCLUSIVE                         
         EDIT  HALF,(3,P+14)                                                    
         GOTO1 REPORT                                                           
         CLC   HALF,=H'180'        MINIMUM CUTOFF AGE?                          
         BNH   INIT0800            NO  - DON'T PURGE                            
*                                                                               
*   DETERMINE MASTER/SUBSIDIARY SITUATION                                       
*                                                                               
***>>>   DC    H'0'                KILL IT FOR TEST                             
*                                                                               
         GOTO1 MASTSUB                                                          
*                                                                               
         LA    RF,KEYTYPTB         SET A(KEYTYPE IN PROGRESS)                   
         ST    RF,AKEYTABL                                                      
*                                                                               
         MVC   KEYTYPE,0(RF)       PROCESS X'4100' RECORDS FIRST                
INIT0200 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),KEYTYPE      SET FIRST KEY                                
         CLI   SUBREPS,0           MASTER RUN? (SUBREPS TABLED?)                
         BNE   INIT0240            YES - MASTER RUN                             
*                                     DON'T PUT REP IN KEY                      
         MVC   KEY+7(2),RCREPFL    NO  - INSERT REP CODE                        
         B     INIT0240                                                         
INIT0220 EQU   *                                                                
         MVI   KEY+RDARKRT-RDARREC,X'FF'                                        
         XC    KEY+RDARKSEQ-RDARREC(2),KEY+RDARKSEQ-RDARREC                     
*                                  CLEAR REC SUBTYPE                            
INIT0240 EQU   *                                                                
         GOTO1 HIGH                READ FIRST RECORD                            
         B     INIT0280                                                         
INIT0260 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT RECORD                             
INIT0280 EQU   *                                                                
***      MVC   P+1(09),=C'READING :'                                            
***      MVC   P+12(27),KEY                                                     
***      GOTO1 HEXOUT,DMCB,KEY+RDARKORD-RDARREC,P+44,4,=C'TOG'                  
***      GOTO1 REPORT                                                           
*                                                                               
         L     RF,PROCCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
         L     RF,TOTREAD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TOTREAD                                                       
         CLC   PROCCTR,=F'5000'    DISPLAY EVERY 5K DARE READS                  
         BNE   INIT0300                                                         
         XC    PROCCTR,PROCCTR     CLEAR COUNTER                                
         MVC   P+1(05),=C'READ:'                                                
         EDIT  TOTREAD,(8,P+10)                                                 
         EDIT  CONUPCTR,(8,P+20)                                                
         EDIT  MKGSREAD,(8,P+30)                                                
         EDIT  MKGSCHGD,(8,P+40)                                                
         MVC   P+50(27),KEY                                                     
         GOTO1 REPORT                                                           
INIT0300 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
***      MVC   P+1(13),=C'KEY RETURNED:'                                        
***      MVC   P+15(34),KEY                                                     
***      GOTO1 REPORT                                                           
*   TEST DISP END                                                               
*                                                                               
         CLI   SUBREPS,0           MASTER RUN?                                  
         BNE   INIT0320            YES - MASTER RUN                             
         CLC   KEY(2),KEYTYPE      NO  - SUBREP: SAME RECORD TYPE?              
         BNE   INIT0500            NO  - END OF DATA FOR REP                    
         CLC   KEY+7(2),RCREPFL    YES - SAME REP CODE?                         
         BNE   INIT0500            NO  - END OF DATA FOR REP                    
         GOTO1 DARCOUNT            COUNT RECORDS READ                           
         B     INIT0360            YES - PROCESS THIS RECORD FURTHER            
INIT0320 EQU   *                                                                
         CLC   KEY(2),KEYTYPE      MASTER RUN: SAME KEY TYPE?                   
         BNE   INIT0500            NO  - END OF DATA FOR MASTER                 
         GOTO1 DARCOUNT            COUNT RECORDS READ                           
         LA    RF,SUBREPS          SET A(SUBREP TABLE)                          
INIT0340 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE REACHED?                        
         BE    INIT0220            YES - NOT IN TABLE: SKIP RECORD              
         CLC   KEY+7(2),0(RF)      RECORD'S REP IN TABLE?                       
         BE    INIT0360            YES - PROCESS THIS RECORD FURTHER            
         LA    RF,2(RF)            NO  - BUMP TO NEXT TABLE ENTRY               
         B     INIT0340            GO BACK FOR NEXT                             
INIT0360 EQU   *                                                                
         CLI   KEY+RDARKRT-RDARREC,X'10'                                        
*                                  AGENCY HEADER SUBTYPE RECORD?                
         BE    INIT0380            YES                                          
         BAS   RE,BADHDR           NO  - DISPLAY AND COUNT BAD HDRS             
         B     INIT0220            SKIP THIS DARE RECORD                        
INIT0380 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P+1(08),=C'CHEKREC:'                                             
         GOTO1 CHEKREC             PROCESS THIS DARE RECORD?                    
         BNZ   INIT0220            NO  - SKIP IT:  CC NOT ZERO                  
*                                                                               
         CLI   QUESTOR+2,C'Y'      SHORT RUN REQUESTED?                         
         BNE   INIT0400            NO                                           
*                                                                               
*   TEST: SHORT RUNS FOR TESTING PURPOSES                                       
*                                                                               
         CLC   KEYTYPE,=X'4100'    DARE RECORDS: IN PROGRESS?                   
         BNE   TEST0040            NO                                           
         CLC   DAR41DEL,TESTCNT    YES - N RECS DELETED?                        
         BE    INIT0500            YES - FORCE FINISH                           
         B     INIT0400            NO  - PROCEED                                
TEST0040 EQU   *                                                                
         CLC   KEYTYPE,=X'4101'    DARE RECORDS: PREV COPY IN PROGRESS?         
         BNE   TEST0060            NO                                           
         CLC   DAR4101D,TESTCNT    YES - N RECS DELETED?                        
         BE    INIT0500            YES - FORCE FINISH                           
         B     INIT0400            NO  - PROCEED                                
TEST0060 EQU   *                                                                
         CLC   DAR51DEL,TESTCNT    N RECS DELETED?                              
         BE    INIT0500            YES - FORCE FINISH                           
INIT0400 EQU   *                                                                
         GOTO1 DARPROC             COUNT RECORDS PROCESSED                      
*                                                                               
*   TEST END                                                                    
*                                                                               
         GOTO1 DELORD              DELETE DARE ORDER                            
*                                                                               
         MVC   KEYSAV2,KEY         SAVE DARE KEY FOR RESTART                    
*                                                                               
         GOTO1 MARKCON             MARK CONTRACT HEADER                         
         GOTO1 PROCMKG             PROCESS MAKEGOODS                            
         MVC   KEY,KEYSAV2         RESTART DARE KEY                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         B     INIT0280            GO BACK FOR NEXT                             
*                                     NEXT DARE RECORD READ DURING              
*                                     PROCESSING OF RECORD:                     
*                                     DON'T READ SEQ OR RECORD WILL             
*                                     BE SKIPPED                                
INIT0500 EQU   *                                                                
         L     RF,AKEYTABL         SET A(KEYTYPE TABLE)                         
         LA    RF,LKEYTYP(RF)                                                   
         ST    RF,AKEYTABL                                                      
         CLC   =X'FFFF',0(RF)      DELIMITER OF TABLE REACHED?                  
         BE    INIT0900            YES - FINISHED                               
         MVC   KEYTYPE,0(RF)       NO  - INSERT NEXT KEYTYPE                    
         B     INIT0200                                                         
INIT0800 EQU   *                                                                
         MVC   P+1(36),=C'MINIMUM # DAYS NOT REACHED: NO PURGE'                 
         GOTO1 REPORT                                                           
         MVC   P+1(36),=C'***CHECK DATE CALCULATION ROUTINE***'                 
         GOTO1 REPORT                                                           
         MVC   P+1(36),=C'******NO DELETIONS TODAY************'                 
         GOTO1 REPORT                                                           
INIT0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*   BADHDR:  COUNT DARE RECORDS THAT DON'T BEGIN WITH X'10'                     
*                                                                               
BADHDR   NTR1                                                                   
         L     RF,BADHDRCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,BADHDRCT                                                      
         CLI   QUESTOR+7,C'Y'      DISPLAY BAD HEADER KEYS?                     
         BNE   BHDR0080            NO                                           
         MVC   P+1(11),=C'BAD HEADER:'                                          
         MVC   P+15(27),KEY                                                     
         GOTO1 REPORT                                                           
BHDR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   DARCOUNT:  COUNT DARE RECORDS READ BY TYPE                                  
*                                                                               
DARCOUNT NTR1                                                                   
         CLC   KEYTYPE,=X'4100'                                                 
         BNE   DCNT0040                                                         
         L     RF,DAR41CTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR41CTR                                                      
         B     DCNT0100                                                         
DCNT0040 EQU   *                                                                
         CLC   KEYTYPE,=X'4101'                                                 
         BNE   DCNT0060                                                         
         L     RF,DAR4101C                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR4101C                                                      
         B     DCNT0100                                                         
DCNT0060 EQU   *                                                                
         L     RF,DAR51CTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR51CTR                                                      
         B     DCNT0100                                                         
DCNT0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   DARPROC :  COUNT DARE RECORDS PROCESSED BY TYPE                             
*                                                                               
DARPROC  NTR1                                                                   
         CLC   KEYTYPE,=X'4100'                                                 
         BNE   DPRO0040                                                         
         L     RF,DAR41DEL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR41DEL                                                      
         B     DPRO0100                                                         
DPRO0040 EQU   *                                                                
         CLC   KEYTYPE,=X'4101'                                                 
         BNE   DPRO0060                                                         
         L     RF,DAR4101D                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR4101D                                                      
         B     DPRO0100                                                         
DPRO0060 EQU   *                                                                
         L     RF,DAR51DEL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAR51DEL                                                      
         B     DPRO0100                                                         
DPRO0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*                                                                               
RPTDONE  NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RUNDONE:  DISPLAY MESSAGE                                                   
RUNDONE  NTR1                                                                   
         MVC   P+1(08),=C'RUN DONE'                                             
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 4100 ORDERS READ:'                               
         EDIT  DAR41CTR,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 4101 ORDERS READ:'                               
         EDIT  DAR4101C,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 5100 ORDERS READ:'                               
         EDIT  DAR51CTR,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 4100 ORDERS DELD:'                               
         EDIT  DAR41DEL,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 4101 ORDERS DELD:'                               
         EDIT  DAR4101D,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE 5100 ORDERS DELD:'                               
         EDIT  DAR51DEL,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'TOTAL DARE KEYS  DELD:'                               
         EDIT  DARTOTKD,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'TOTAL DARE RECS  DELD:'                               
         EDIT  DARTOTRD,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'CONTRACT RECS MODIFIED:'                              
         EDIT  CONUPCTR,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'CONTRACTS NOT ON FILE :'                              
         EDIT  NOTFILED,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'DARE ORDERS W/OUT CON#:'                              
         EDIT  NOCON#,(6,P+28)                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'CONTRACTS W/OUT 1D ELT:'                              
         EDIT  NODARE1D,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'MAKEGOOD RECS READ    :'                              
         EDIT  MKGSREAD,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'MAKEGOOD RECS CHANGED :'                              
         EDIT  MKGSCHGD,(6,P+28)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'BAD HEADERS FOUND     :'                              
         EDIT  BADHDRCT,(6,P+28)                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
*   REQDONE:  DISPLAY MESSAGE                                                   
REQDONE  NTR1                                                                   
         MVC   P+1(08),=C'REQ DONE'                                             
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* MASTSUB - CHECK REP:  IF MASTER, SET SUBSIDIARY TABLE                         
*                                                                               
***********************************************************************         
MASTSUB  NTR1                                                                   
*                                                                               
         XC    SUBREPS,SUBREPS     CLEAR SUB REP TABLE                          
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET RECORD TYPE                              
         MVC   KEY+25(2),RCREPFL   SET REP CODE                                 
         GOTO1 HIGH                RETRIEVE KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREP              RETRIEVE RECORD                              
*                                                                               
         MVC   P+1(07),=C'REP IS:'                                              
         MVC   P+10(33),RREPNAME                                                
         GOTO1 REPORT                                                           
         CLC   =X'FFFF',RREPMAST   MASTER REP?                                  
         BE    MSUB0020            YES                                          
         MVC   P+1(19),=C'RUN IS FOR A SUBREP'                                  
         GOTO1 REPORT                                                           
         B     MSUB0100            NO                                           
MSUB0020 EQU   *                                                                
         MVC   P+1(19),=C'RUN IS FOR A MASTER'                                  
         GOTO1 REPORT                                                           
         LA    R2,SUBREPS          SET A(SUBREP TABLE)                          
         LA    R1,RREPELEM         GET SUB REPS FROM REP RECORD                 
*                                                                               
MSUB0040 ZIC   R0,1(R1)            SEARCH FOR '02' ELEMENT                      
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BE    MSUB0100            YES                                          
         CLI   0(R1),2             SUBSIDIARY ELEMENT?                          
         BNE   MSUB0040            NO  - SKIP IT                                
*                                                                               
*  '02' ELEMENT FOUND:  EXTRACT COUNT, STORE CODES IN TABLE                     
*                                                                               
         ZIC   RF,2(R1)            RREPSCNT: # SUB REPS FOR MASTER              
         LR    R5,RF               SAVE FOR LOOP                                
         LA    R4,10(R1)           A(SUBSID REP CODES)                          
*                                                                               
*  SORT THE CODES IN THE REP RECORD BEFORE TABLING                              
*                                                                               
         GOTO1 XSORT,DMCB,(0,0(R4)),(RF),2,2,0                                  
*                                                                               
***      MVC   P+1(12),=C'SORTED REPS:'                                         
***      MVC   P+15(24),0(R4)                                                   
***      GOTO1 REPORT                                                           
MSUB0060 EQU   *                                                                
         MVC   0(2,R2),0(R4)       MOVE CODE TO TABLE                           
*                                                                               
         LA    R4,2(R4)            NEXT CODE IN REP REC                         
         LA    R2,2(R2)            NEXT TABLE ENTRY                             
         BCT   R5,MSUB0060         LOAD ALL CODES                               
*                                                                               
MSUB0080 EQU   *                                                                
         MVC   0(2,R2),=X'FFFF'    END MARKER - FOR REP TABLE                   
MSUB0100 EQU   *                                                                
**       MVC   P+1(13),=C'SUBREP TABLE:'                                        
**       MVC   P+15(40),SUBREPS                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHEKREC - CHECK LTC OF DARE ORDER                                             
*        RETURN NOT ZERO TO SKIP RECORD                                         
*                                                                               
***********************************************************************         
CHEKREC  NTR1                                                                   
         GOTO1 GETDARE             RETRIEVE DARE AGENCY HEADER                  
*&&DO                                                                           
         MVC   P+1(09),=C'CHECKING:'                                            
         MVC   P+12(27),KEY                                                     
         GOTO1 HEXOUT,DMCB,RDARKORD,P+44,4,=C'TOG'                              
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,P+56)                                
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*   TEST                                                                        
*                                                                               
         CLC   =X'31960000',RDARKORD                                            
         BNE   TEST0020                                                         
         LA    R4,RDARREC          A(DARE RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RDARLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0020 EQU   *                                                                
*                                                                               
*   TEST END                                                                    
*&&                                                                             
         MVC   SAVEREP,RDARKREP    SAVE RECORD'S REP CODE                       
         MVC   SAVECON#,RDARREP#   SAVE RECORD'S CONTRACT #                     
*                                                                               
         CLC   RDARESEN,STARTCOM   EST END DATE PRE- CUTOFF DATE?               
         BNL   CREC0900            NO  - DON'T PROCESS THIS                     
         SR    R0,R0               SET CC ZERO                                  
         B     CREC0990            EXIT                                         
CREC0900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CREC0990 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DELETE - ORDER FROM HEADER TO TRAILER                                         
***********************************************************************         
DELORD   NTR1                                                                   
*                                                                               
*   FIRST KEY AND RECORD ALREADY READ BY FILTERING ROUTINE                      
*                                                                               
         B     DORD0040                                                         
DORD0020 DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   DORD0160                                                         
                                                                                
         GOTO1 GETDARE                                                          
*                                                                               
DORD0040 DS    0H                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER?                               
         BNE   DORD0060                                                         
         MVC   KEYSAV3,KEY         SAVE KEY FOR RESTART                         
         GOTOR DELPAS              DELETE PASSIVES KEYS TO THIS ORDER           
         MVC   KEY,KEYSAV3         RESET KEY FOR RESTART                        
         GOTO1 HIGH                RESTART KEY                                  
*                                                                               
DORD0060 DS    0H                                                               
         OI    RDARCNTL,X'80'                                                   
         L     RF,DARTOTRD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DARTOTRD                                                      
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY RECORDS PROCESSED?                   
         BNE   DORD0080            NO                                           
         GOTO1 DELDISP             DISPLAY RECORD PROCESSED                     
DORD0080 DS    0H                                                               
*                                                                               
         CLI   QOPTION2,C'U'       HARD UPDATE?                                 
         BNE   DORD0100            NO                                           
         GOTO1 PREC                                                             
DORD0100 DS    0H                                                               
         OI    KEY+27,X'80'                                                     
         L     RF,DARTOTKD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DARTOTKD                                                      
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY RECORDS PROCESSED?                   
         BNE   DORD0120            NO                                           
         GOTO1 DELKDISP            DISPLAY KEY PROCESSED                        
*                                                                               
DORD0120 DS    0H                                                               
         CLI   QOPTION2,C'U'       HARD UPDATE?                                 
         BNE   DORD0140            NO                                           
         GOTO1 WRITE                                                            
DORD0140 DS    0H                                                               
                                                                                
         GOTO1 SEQ                                                              
                                                                                
         B     DORD0020                                                         
*                                                                               
* DELETE ACTIVE/CONFIRM ORDER AS WELL                                           
*                                                                               
DORD0160 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DELDISP  NTR1                                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(09),=C'KEY TYPE:'                                            
         GOTO1 HEXOUT,DMCB,RDARKTYP,P+12,2,=C'TOG'                              
         MVC   P+21(09),=C'DARE ORD#'                                           
         GOTO1 HEXOUT,DMCB,RDARKORD,P+32,4,=C'TOG'                              
         MVC   P+42(09),=C'REC TYPE:'                                           
         GOTO1 HEXOUT,DMCB,RDARKRT,P+52,1,=C'TOG'                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,RDARREC          A(DARE RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RDARLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
         EJECT                                                                  
DELKDISP NTR1                                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(09),=C'PREV KEY:'                                            
         MVC   P+12(34),KEY                                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
DELPAS   NTR1                                                                   
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AWORK:    KEY BUILD AREA                                               
*        RDARREC:  CURRENT LOCATION OF AGENCY ORDER RECORD                      
*                                                                               
         LA    RF,RCONREC                                                       
         ST    RF,AWORK                                                         
         L     R4,AWORK                                                         
         XCEF  (R4),1600                                                        
*                                                                               
         PRINT GEN                                                              
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'81',DBCOMFCS),AWORK,RDARREC          
*                                                                               
         L     R4,AWORK            A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
         CLI   QOPTION2,C'U'       HARD UPDATE?                                 
         BNE   DELP0080            NO  - GO DISPLAY BUFFER                      
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',DBCOMFCS),AWORK,(R4),   X        
               DUMMYDA                                                          
*                                  YES - DELETE OLD KEYS                        
         PRINT NOGEN                                                            
*                                                                               
         B     DELP0990                                                         
DELP0080 EQU   *                                                                
         CLI   QUESTOR,C'Y'        DISPLAY PASSIVE BLOCK?                       
         BNE   DELP0990            NO                                           
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'PASSIVE KEYS'                                         
         GOTO1 REPORT                                                           
         L     R4,AWORK            A(DARE RECORD)                               
         SR    RF,RF                                                            
         A     RF,=F'1600'         SET TO 1600 BYTES                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
DELP0990 XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  MARK CONTRACT DR ELEMENT FOR REMOVED DARE ORDER                              
***********************************************************************         
MARKCON  NTR1                                                                   
                                                                                
         OC    SAVECON#,SAVECON#   DARE ORDER HAS CONTRACT?                     
         BNZ   MCON0005            YES                                          
         L     RF,NOCON#                                                        
         LA    RF,1(RF)                                                         
         ST    RF,NOCON#                                                        
         CLI   QUESTOR+4,C'Y'      DISPLAY KEYS?                                
         BNE   MCON0900            NO                                           
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'DARE ORDER W/OUT CON#'                                
         GOTO1 HEXOUT,DMCB,RDARKORD,P+24,4,=C'TOG'                              
         MVC   P+34(2),SAVEREP                                                  
         GOTO1 REPORT                                                           
         B     MCON0900                                                         
MCON0005 EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY              SET A(KEY)                                   
         USING RCONKEY,R2                                                       
         MVI   0(R2),X'8C'         INSERT KEY TYPE                              
         MVC   21(02,R2),SAVEREP   INSERT REP CODE OF ORDER                     
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),SAVECON#                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         MVC   CCONNUM,WORK        SAVE FOR MAKEGOODS                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         CLI   QUESTOR+2,C'Y'      DISPLAY KEYS?                                
         BNE   MCON0010            NO                                           
         GOTO1 REPORT                                                           
         MVC   P+1(13),=C'CONTRACT KEY:'                                        
         MVC   P+15(27),KEY                                                     
         GOTO1 REPORT                                                           
MCON0010 EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    MCON0015            KEY FOUND ON FILE                            
         L     RF,NOTFILED                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NOTFILED                                                      
         CLI   QUESTOR+3,C'Y'      DISPLAY KEYS?                                
         BNE   MCON0900            NO                                           
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'CONTRACT NOT ON FILE:'                                
         GOTO1 HEXOUT,DMCB,RDARKORD,P+24,4,=C'TOG'                              
         MVC   P+34(2),SAVEREP                                                  
         GOTO1 HEXOUT,DMCB,SAVECON#,P+38,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     MCON0900                                                         
MCON0015 EQU   *                                                                
         GOTO1 GETCON                                                           
*                                                                               
*   TEST DISPLAY                                                                
         CLI   QUESTOR+8,C'Y'                                                   
         BNE   MCON0017                                                         
         MVC   P+1(14),=C'CONTRACT PRE ='                                       
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(CONTRACT RECORD)                           
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
MCON0017 EQU   *                                                                
*   TEST DISP END                                                               
*                                                                               
*                                                                               
*                                                                               
         LA    R6,RCONELEM                                                      
MCON0020 CLI   0(R6),0                                                          
         BE    MCON0100            NO DARE ELEMENT:  LIST                       
         CLI   0(R6),X'1D'         DARE AGENCY ORDER ELEMENT?                   
         BE    MCON0040            YES                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MCON0020                                                         
*                                                                               
MCON0040 DS    0H                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,MCON0060                                                      
         B     MCON0080                                                         
MCON0060 MVC   ELEM(0),0(R6)       SAVE THEN DELETE ELEMENT                     
MCON0080 DS    0H                                                               
*                                                                               
         USING RCONDREL,R6                                                      
         MVI   RCONDRFG,0          CLEAR DARE FLAGS                             
         DROP  R6                                                               
*                                                                               
*                                                                               
*   TEST DISPLAY                                                                
         CLI   QUESTOR+8,C'Y'                                                   
         BNE   MCON0090                                                         
         MVC   P+1(14),=C'CONTRACT POST='                                       
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(CONTRACT RECORD)                           
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
MCON0090 EQU   *                                                                
*   TEST DISP END                                                               
*                                                                               
         L     RF,CONUPCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,CONUPCTR                                                      
*                                                                               
         CLI   QOPTION2,C'U'       HARD UPDATE?                                 
         BNE   MCON0900            NO                                           
         GOTO1 PREC                                                             
         B     MCON0900                                                         
MCON0100 DS    0H                                                               
         L     RF,NODARE1D                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NODARE1D                                                      
         CLI   QUESTOR+7,C'Y'      DISPLAY INFORMATION?                         
         BNE   MCON0110            NO                                           
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACT MISSING 1D ELT:'                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+28,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RDARKORD,P+38,4,=C'TOG'                              
         GOTO1 REPORT                                                           
MCON0110 DS    0H                                                               
*                                                                               
MCON0900 DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNDARE MAKEGOOD RECORDS, IF ANY                                               
* CONTRACT RECORD SHOULD BE IN AIO AREA                                         
***********************************************************************         
PROCMKG  NTR1                                                                   
         OC    SAVECON#,SAVECON#   DARE ORDER HAS CONTRACT?                     
         BZ    PMKG0990            NO  - NO MAKEGOODS                           
         XC    KEY,KEY                                                          
MGKEYD   USING RMKGKEY,KEY                                                      
         MVI   MGKEYD.RMKGKTYP,X'11'                                            
         MVC   MGKEYD.RMKGKREP,RCONKREP                                         
         MVC   MGKEYD.RMKGKOFF,RCONKOFF                                         
         MVC   MGKEYD.RMKGKSTA,RCONKSTA                                         
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         MVC   MGKEYD.RMKGKCON,WORK                                             
*                                                                               
PMKG0010 DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PMKG0040                                                         
*                                                                               
PMKG0020 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PMKG0040 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   PMKG0990                                                         
*                                                                               
         L     RF,MKGSREAD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,MKGSREAD                                                      
*                                                                               
         CLI   QUESTOR+5,C'Y'      DISPLAY MAKEGOODS?                           
         BNE   PMKG0060            NO                                           
         MVC   P+1(09),=C'MAKEGOOD:'                                            
         GOTO1 HEXOUT,DMCB,SAVECON#,P+12,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,CCONNUM,P+22,4,=C'TOG'                               
         MVC   P+32(27),KEY                                                     
         GOTO1 REPORT                                                           
PMKG0060 EQU   *                                                                
         OC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
         BZ    PMKG0070            GROUP COMMENT: PROCESS                       
         XC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
*                                  NOT GROUP COMMENT: SKIP READ                 
         ZIC   RF,MGKEYD.RMKGKGR2  BUMP 2ND CHAR OF GROUP                       
         LA    RF,1(RF)                                                         
         STC   RF,MGKEYD.RMKGKGR2  REPLACE GROUP                                
         B     PMKG0010            READ HIGH FOR NEXT KEY                       
*                                                                               
         DROP  MGKEYD                                                           
*                                                                               
PMKG0070 EQU   *                                                                
         GOTO1 GETMKG                                                           
*                                                                               
         L     RF,MKGSCHGD         COUNT MAKEGOODS CHANGED                      
         LA    RF,1(RF)                                                         
         ST    RF,MKGSCHGD                                                      
*                                                                               
         CLI   QUESTOR+6,C'Y'      DISPLAY MAKEGOOD RECORDS?                    
         BNE   PMKG0080            NO                                           
*                                                                               
         CLC   MKGSCHGD,=F'21'     DISPLAY FIRST 20 CHANGES                     
         BNL   PMKG0080                                                         
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'MAKEGOOD REC PRE :'                                   
         GOTO1 REPORT                                                           
         LA    R4,RMKGREC          A(MKGD RECORD)                               
         ICM   RF,3,RMKGLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
PMKG0080 EQU   *                                                                
         MVI   RMKGSFG1,0                                                       
         XC    RMKGDARN,RMKGDARN                                                
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'02',RMKGREC),0,0            
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'05',RMKGREC),0,0            
*                                                                               
*                                                                               
         CLI   QUESTOR+6,C'Y'      DISPLAY MAKEGOOD RECORDS?                    
         BNE   PMKG0100            NO                                           
*                                                                               
         CLC   MKGSCHGD,=F'21'     DISPLAY FIRST 20 CHANGES                     
         BNL   PMKG0100                                                         
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'MAKEGOOD REC POST:'                                   
         GOTO1 REPORT                                                           
         LA    R4,RMKGREC          A(MKGD RECORD)                               
         ICM   RF,3,RMKGLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
PMKG0100 EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'U'       HARD UPDATE?                                 
         BNE   PMKG0020            NO                                           
         GOTO1 PREC                                                             
         B     PMKG0020                                                         
*                                                                               
PMKG0990 DS    0H                                                               
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
GETDARE  LA    RF,RDARREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETMKG   LA    RF,RMKGREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         XIT1                                                                   
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BNZ   DM020                                                            
         XIT1                                                                   
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
         SPACE 3                                                                
CONFLAG  DS    CL1                                                              
PRTFLAG  DC    CL1'X'                                                           
*                                                                               
*   PRTFLAG SETTING:  X  = NO: FIRST PASS                                       
*                     N  = NO: ALL OTHER PASSES                                 
*                     Y  = YES: PRINT TOTALS                                    
*                                                                               
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
KEYTYPE  DS    XL2                                                              
*                                                                               
KEYTYPTB DC    X'4100'             DARE ORDERS: PRIMARY                         
LKEYTYP  EQU   *-KEYTYPTB          SET L(TABLE ENTRY)                           
         DC    X'4101'             DARE ORDERS: PREV COPY                       
         DC    X'5100'             DARE ORDERS: CONFIRMED                       
         DC    X'FFFF'             DELIMITER                                    
AKEYTABL DS    A                   A(KEYTYPE IN PROGRESS)                       
*                                                                               
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
KEYSAV3  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
*                                                                               
STARTBIN DS    CL3                 START DATE: BINARY                           
STARTCOM DS    CL2                 START DATE: COMPRESSED                       
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEWORK DS    CL6                                                              
SUBREPS  DS    CL40                SUB REP TABLE: 20 ENTRIES                    
SAVEREP  DS    CL2                                                              
SAVECON# DS    CL4                                                              
CCONNUM  DS    CL4                                                              
*                                                                               
ELEM     DS    CL64                                                             
*                                                                               
DBCOMFCS DS    A                                                                
VREPFACS DS    A                                                                
AWORK    DS    A                                                                
DUMMYDA  DS    A                                                                
*                                                                               
PROCCTR  DS    F                                                                
TOTREAD  DS    F                                                                
DAR41CTR DS    F                                                                
DAR4101C DS    F                                                                
DAR51CTR DS    F                                                                
DAR41DEL DS    F                                                                
DAR4101D DS    F                                                                
DAR51DEL DS    F                                                                
DARTOTKD DS    F                                                                
DARTOTRD DS    F                                                                
MKGSREAD DS    F                                                                
MKGSCHGD DS    F                                                                
NODARE1D DS    F                                                                
CONUPCTR DS    F                                                                
NOCON#   DS    F                                                                
NOTFILED DS    F                                                                
BADHDRCT DS    F                                                                
TESTCNT  DC    F'100'                                                           
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
         ORG   RBUYREC                                                          
       ++INCLUDE REGENDAR                                                       
*                                                                               
         ORG   RBUYREC                                                          
       ++INCLUDE REGENMKG                                                       
*                                                                               
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
QREC2D   DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE REPFACSQ                                                       
         DS    0F                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030REREPDP02 07/14/04'                                      
         END                                                                    
