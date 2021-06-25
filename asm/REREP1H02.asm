*          DATA SET REREP1H02  AT LEVEL 178 AS OF 05/25/07                      
*PHASE RE1H02G,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1H02 - GENERAL CONTRACT FIXER   '                          
*********************************************************************           
*                                                                   *           
*        REREP1H02 --- GENERAL CONTRACT FIXER                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JUL18/06 (BU ) --- INITIAL ENTRY:  SCAN MAKEGOOD RECORDS.         *           
*                    LOCATE SELF-APPLIED, NOT APPROVED.             *           
*                    PRODUCE DOWNLOADABLE REPORT                    *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1H02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1H02,R7,R8,R9,RR=RE                                        
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
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
         DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)                                          
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
         GOTO1 INITALL,DMCB,(RC)                                                
*                                                                               
         CLI   QOPTION1,C'S'       SELF-APPLIED REPORT?                         
         BNE   INIT0020            NO                                           
         MVI   RCSUBPRG,0          SET HEADINGS FOR SELF-APPLIED                
         GOTO1 PASSSELF,DMCB,(RC)  YES - PRODUCE SELF-APPLIED REP               
         B     INIT0040                                                         
*                                                                               
INIT0020 EQU   *                                                                
         CLI   QOPTION1,C'O'       OPEN MAKEGOOD REPORT?                        
         BE    *+6                 YES -                                        
         DC    H'0'                UNRECOGNIZED REPORT                          
         MVI   RCSUBPRG,1          SET HEADINGS FOR OPEN MKGD                   
         GOTO1 PASSOPMG,DMCB,(RC)  PRODUCE OPEN MAKEGOOD REP                    
INIT0040 EQU   *                                                                
         GOTO1 SHOWRECS,DMCB,(RC)                                               
*                                                                               
***      GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
         XIT1                                                                   
*                                                                               
INITALL  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'N'       SUPPRESS PAGE BREAK                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                  OPEN SORT FILE                               
         XC    SORTREC,SORTREC                                                  
*                                  OPEN SORT FILE                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
PASSSELF NTR1                                                                   
         XC    PULSECTR,PULSECTR                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'11'           SET TO MAKEGOOD RECS                         
         MVC   RMKGKREP-RMKGKEY+KEY(2),RCREPFL                                  
*                                  INSERT REP CODE                              
PSEL0010 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     PSEL0040                                                         
PSEL0020 EQU   *                                                                
         GOTO1 SEQ                                                              
PSEL0040 EQU   *                                                                
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
         BNE   PSEL0050                                                         
         XC    PULSECTR,PULSECTR                                                
         CLC   =C'PULSE',QUESTOR   DISPLAY PROCESS CTS?                         
         BNE   PSEL0050            NO                                           
         MVC   P+01(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+12)                                               
         MVC   P+24(09),=C'MAKEGOODS'                                           
         MVC   P+36(27),KEY                                                     
         GOTO1 REPORT                                                           
PSEL0050 EQU   *                                                                
         CLI   KEY,X'11'           MAKEGOOD RECORD?                             
         BH    PSEL0900            NO  - FINISHED                               
         CLC   RMKGKREP-RMKGKEY+KEY(2),RCREPFL                                  
*                                  FOR THIS REP?                                
         BNE   PSEL0900            NO  - FINISHED                               
*                                                                               
         CLI   RMKGKPLN-RMKGKEY+KEY,X'00'                                       
*                                  GROUP COMMENT RECORD?                        
         BNE   PSEL0020            NO  - SKIP IT                                
         CLC   QSTATION,SPACES     ANY STATION FILTER?                          
         BNH   PSEL0052            NO                                           
         CLC   QSTATION,RMKGKSTA-RMKGKEY+KEY                                    
*                                  YES - SAME STATION?                          
         BNE   PSEL0020            NO  - SKIP IT                                
PSEL0052 EQU   *                                                                
         CLC   QOFFICE,SPACES      ANY OFFICE FILTER?                           
         BNH   PSEL0054            NO                                           
         CLC   QOFFICE,RMKGKOFF-RMKGKEY+KEY                                     
*                                  YES - SAME OFFICE?                           
         BNE   PSEL0020            NO  - SKIP IT                                
PSEL0054 EQU   *                                                                
         CLC   KEY(19),PASSKEY     SAME MAKEGOOD THRU CONTRACT?                 
         BE    PSEL0056            YES - DON'T CLEAR TARGETS                    
         XC    TARGETS,TARGETS     NO  - DIFFERENT CONTRACT                     
PSEL0056 EQU   *                                                                
         BAS   RE,GETMKG           RETRIEVE MKGD RECORD                         
*                                                                               
         TM    RMKGSFG3,RMGF3SAQ   80 = SELF-APPLY DONE                         
         BNO   PSEL0020            NOT SELF-APPLY: SKIP IT                      
         TM    RMKGSFG3,RMGF3ARQ   40 = REP APPROVED?                           
         BO    PSEL0020            YES - SKIP IT                                
*                                                                               
         MVC   PASSKEY,RMKGKEY     SAVE KEY FOR SUBRECS                         
*                                                                               
         LA    R6,RMKGREC          GET SALESPERSON CODE                         
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL            RETRIEVE SALESPERSON CODE ELEMENT            
         BE    *+6                                                              
         DC    H'0'                                                             
SC       USING RMKGXEL,R6                                                       
         MVC   SALPER,SC.RMKGXSAL                                               
         DROP  SC                                                               
*&&DO                                                                           
*   TEST                                                                        
**       CLC   RMKGKCON,=X'53884629'                                            
**       BNE   TEST0020                                                         
         MVC   P+1(10),=C'S/A FOUND:'                                           
         MVC   P+12(27),RMKGKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'PASSKEY  :'                                           
         MVC   P+12(27),PASSKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'TABLE    :'                                           
         MVC   P+12(32),TARGETS                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
TEST0020 EQU   *                                                                
*   END TEST                                                                    
*&&                                                                             
*                                      REVERSE CONTRACT NUMBER                  
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RMKGKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+23(4),WORK+15       PRIME THE VALUE                         
         PACK  WORK+23(1),WORK+18(1)    REVERSE THE COMPLIMENT                  
         PACK  WORK+24(1),WORK+17(1)                                            
         PACK  WORK+25(1),WORK+16(1)                                            
         PACK  WORK+26(1),WORK+15(1)                                            
*                                                                               
*   NEED TO CYCLE SUBRECORDS FOR THIS CONTRACT / GROUP                          
*                                                                               
PSEL0060 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT SUBREC                             
         CLC   KEY(21),PASSKEY     SAME MAKEGOOD THRU GROUP?                    
         BE    PSEL0065            YES - PROCESS GROUP  -                       
*                                                                               
*   DON'T PERMIT DUPLICATES WITHIN A GROUP.  PERMIT DUPLICATES                  
*        WITHIN A CONTRACT                                                      
*                                                                               
****     CLC   KEY(19),PASSKEY     SAME MAKEGOOD THRU CONTRACT?                 
****     BE    PSEL0040            YES - DON'T CLEAR TARGETS                    
*                                     BEGIN CHECK FROM TOP AGAIN                
         XC    TARGETS,TARGETS     NO  - DIFFERENT CONTRACT / GROUP             
         B     PSEL0040            CLEAR TARGETS, THEN                          
*                                     BEGIN CHECK FROM TOP AGAIN                
*                                                                               
PSEL0065 EQU   *                                                                
*                                                                               
         GOTO1 GETMKG              YES - DETAIL IN GROUP                        
*&&DO                                                                           
*   TEST                                                                        
         MVI   SHOWFLAG,C'N'                                                    
         CLC   RMKGKCON,=X'97464729'                                            
         BNE   TEST0040                                                         
         MVI   SHOWFLAG,C'Y'                                                    
TEST0040 EQU   *                                                                
*   END TEST                                                                    
*&&                                                                             
*&&DO                                                                           
*   TEST                                                                        
         CLC   RMKGKCON,=X'97464729'                                            
         BNE   TEST0030                                                         
         MVC   P+1(10),=C'MG DETAIL:'                                           
         MVC   P+12(27),RMKGKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'PASSKEY  :'                                           
         MVC   P+12(27),PASSKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'TABLE    :'                                           
         MVC   P+12(32),TARGETS                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
TEST0030 EQU   *                                                                
*   END TEST                                                                    
*&&                                                                             
*                                                                               
*   R5 STILL COVERING RECORD - CYCLE MISSED LINE ELEMENTS                       
*                                                                               
         TM    RMKGKRTY,X'10'      'CHOICE' MG?                                 
         BNO   PSEL0068            NO  - SINGLE OR 'ALL'                        
         LA    R6,RMKGREC          YES - WAS THIS LINE SELECTED?                
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            RETRIEVE STATUS CONTROL ELEMENT              
         BNE   PSEL0220            NOT FOUND - RECORD FINISHED                  
SC       USING RMKGSTEL,R6                                                      
         TM    SC.RMKGSTCH,X'01'   SELECTED?                                    
         BO    PSEL0068            YES - PROCESS IT                             
         B     PSEL0220            NO  - RECORD FINISHED                        
         DROP  SC                                                               
PSEL0068 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            RETRIEVE MISSED LINE ELEMENT                 
         BNE   PSEL0220            NOT FOUND - RECORD FINISHED                  
         B     PSEL0075                                                         
PSEL0070 EQU   *                                                                
         BAS   RE,NEXTEL           RETRIEVE MISSED LINE ELEMENT                 
         BNE   PSEL0220            NOT FOUND - RECORD FINISHED                  
PSEL0075 EQU   *                                                                
EM       USING RMKGMGEL,R6                                                      
         TM    RMKGKRTY,X'10'      SINGLE OR 'AND' OFFER?                       
         BO    PSEL0077            YES - DON'T CHECK FOR ZERO SPOTS             
         OC    EM.RMKGMGSP,EM.RMKGMGSP                                          
*                                  NO  - MISSED SPOTS = ZERO?                   
         BZ    PSEL0070            YES - SKIP ELEMENT                           
PSEL0077 EQU   *                                                                
*                                                                               
         LA    R2,TARGETS          LOOK FOR DUPES                               
PSEL0080 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    PSEL0100            YES - NOT FOUND                              
         CLC   EM.RMKGMGLI,0(R2)   MISSED LINE IN TABLE?                        
         BE    PSEL0070            YES - CHECK NEXT ELEMENT                     
         LA    R2,1(R2)            NO  - BUMP TO NEXT SLOT                      
         B     PSEL0080            GO BACK FOR NEXT                             
PSEL0100 EQU   *                                                                
*                                                                               
*   DON'T PUT ANYTHING INTO TABLE:  ACCEPT ALL                                  
*                                                                               
****>>   MVC   0(1,R2),EM.RMKGMGLI    INSERT LINE INTO TARGET                   
*                                                                               
*   GENERATE A SORT RECORD FOR MKG / BUYLINE #                                  
*                                                                               
PSEL0200 EQU   *                                                                
*                                                                               
         MVC   SOFF,PASSKEY+08     INSERT OFFICE CODE                           
         MVC   SSTATION,PASSKEY+10 INSERT STATION                               
         MVC   SCON,WORK+23        INSERT MG CONTRACT CON #                     
         MVC   SMKGGRP,PASSKEY+19  INSERT MG GROUP CODE                         
         MVC   SMISSLIN,EM.RMKGMGLI       INSERT MG LINE #                      
         DROP  EM                                                               
         MVC   SMKGOFFR,RMKGKMLN   INSERT MAKEGOOD OFFER LINE #S                
         MVC   SSALPER,SALPER      INSERT SALESPERSON CODE                      
*                                                                               
*   TEST                                                                        
         CLI   SHOWFLAG,C'N'                                                    
         BE    TESTS050                                                         
         MVC   P+1(07),=C'05 ELT:'                                              
         GOTO1 HEXOUT,DMCB,(R6),P+10,10,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,RMKGKEY,P+34,27,=C'TOG'                              
***      MVC   P+10(10),0(R6)                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'SORT  :'                                              
         MVC   P+10(16),SORTREC                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
TESTS050 EQU   *                                                                
*   END TEST                                                                    
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
*                                                                               
         B     PSEL0070            INNER LOOP - GO BACK FOR NEXT                
*                                     MISSED BUY ELT FOR MKG                    
PSEL0220 EQU   *                                                                
         B     PSEL0060            GET NEXT RECORD - OUTER LOOP                 
*                                     DETAILS OF SELF-APPLIED                   
PSEL0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***>>>>  OPEN MAKEGOOD REPORT                                                   
PASSOPMG NTR1                                                                   
         XC    PULSECTR,PULSECTR                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'11'           SET TO MAKEGOOD RECS                         
         MVC   RMKGKREP-RMKGKEY+KEY(2),RCREPFL                                  
*                                  INSERT REP CODE                              
POMG0010 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     POMG0040                                                         
POMG0020 EQU   *                                                                
         GOTO1 SEQ                                                              
POMG0040 EQU   *                                                                
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
         BNE   POMG0050                                                         
         XC    PULSECTR,PULSECTR                                                
         CLC   =C'PULSE',QUESTOR   DISPLAY PROCESS CTS?                         
         BNE   POMG0050            NO                                           
         MVC   P+01(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+12)                                               
         MVC   P+24(09),=C'MAKEGOODS'                                           
         MVC   P+36(27),KEY                                                     
         GOTO1 REPORT                                                           
POMG0050 EQU   *                                                                
         CLI   KEY,X'11'           MAKEGOOD RECORD?                             
         BH    POMG0900            NO  - FINISHED                               
         CLC   RMKGKREP-RMKGKEY+KEY(2),RCREPFL                                  
*                                  FOR THIS REP?                                
         BNE   POMG0900            NO  - FINISHED                               
*                                                                               
         CLI   RMKGKPLN-RMKGKEY+KEY,X'00'                                       
*                                  GROUP COMMENT RECORD?                        
         BNE   POMG0020            NO  - SKIP IT                                
         CLC   QSTATION,SPACES     ANY STATION FILTER?                          
         BNH   POMG0052            NO                                           
         CLC   QSTATION,RMKGKSTA-RMKGKEY+KEY                                    
*                                  YES - SAME STATION?                          
         BNE   POMG0020            NO  - SKIP IT                                
POMG0052 EQU   *                                                                
         CLC   QOFFICE,SPACES      ANY OFFICE FILTER?                           
         BNH   POMG0054            NO                                           
         CLC   QOFFICE,RMKGKOFF-RMKGKEY+KEY                                     
*                                  YES - SAME OFFICE?                           
         BNE   POMG0020            NO  - SKIP IT                                
POMG0054 EQU   *                                                                
         CLC   KEY(19),PASSKEY     SAME MAKEGOOD THRU CONTRACT?                 
         BE    POMG0056            YES - DON'T CLEAR TARGETS                    
         XC    TARGETS,TARGETS     NO  - DIFFERENT CONTRACT                     
POMG0056 EQU   *                                                                
         BAS   RE,GETMKG           RETRIEVE MKGD RECORD                         
*                                                                               
         TM    RMKGSCST,RMKGSCRQ   01 = CREATED BY REP?                         
         BO    POMG0020            YES - SKIP IT                                
*                                  NO  - CREATED BY STATION                     
         OC    RMKGSFG1,RMKGSFG1   ANY CURRENT DARE STATUS?                     
         BZ    POMG0020            NO  - NOT DARE: SKIP IT                      
*                                                                               
         TM    RMKGSFG1,RMGF1MSN   40 = DARE MAKEGOOD SENT?                     
         BO    POMG0058            YES - PROCESS IT                             
         TM    RMKGSFG1,RMGF1MCR   02 = DARE MAKEGOOD RESENT?                   
         BNO   POMG0020            NO  - SKIP IT                                
*                                                                               
POMG0058 EQU   *                                                                
         MVC   PASSKEY,RMKGKEY     SAVE KEY FOR SUBRECS                         
*                                                                               
         LA    R6,RMKGREC          GET SALESPERSON CODE                         
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL            RETRIEVE SALESPERSON CODE ELEMENT            
         BE    *+6                                                              
         DC    H'0'                                                             
SC       USING RMKGXEL,R6                                                       
         MVC   SALPER,SC.RMKGXSAL                                               
         DROP  SC                                                               
*&&DO                                                                           
*   TEST                                                                        
**       CLC   RMKGKCON,=X'53884629'                                            
**       BNE   TEST0020                                                         
         MVC   P+1(10),=C'S/A FOUND:'                                           
         MVC   P+12(27),RMKGKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'PASSKEY  :'                                           
         MVC   P+12(27),PASSKEY                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'TABLE    :'                                           
         MVC   P+12(32),TARGETS                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
TEST0020 EQU   *                                                                
*   END TEST                                                                    
*&&                                                                             
*                                      REVERSE CONTRACT NUMBER                  
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RMKGKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+23(4),WORK+15       PRIME THE VALUE                         
         PACK  WORK+23(1),WORK+18(1)    REVERSE THE COMPLIMENT                  
         PACK  WORK+24(1),WORK+17(1)                                            
         PACK  WORK+25(1),WORK+16(1)                                            
         PACK  WORK+26(1),WORK+15(1)                                            
*                                                                               
*                                                                               
*   GENERATE A SORT RECORD FOR MKG                                              
*                                                                               
POMG0200 EQU   *                                                                
*                                                                               
         MVC   SOFF,PASSKEY+08     INSERT OFFICE CODE                           
         MVC   SSTATION,PASSKEY+10 INSERT STATION                               
         MVC   SCON,WORK+23        INSERT MG CONTRACT CON #                     
         MVC   SMKGGRP,PASSKEY+19  INSERT MG GROUP CODE                         
         MVC   SMKGOFFR,RMKGKMLN   INSERT MAKEGOOD OFFER LINE #S                
         MVC   SSALPER,SALPER      INSERT SALESPERSON CODE                      
*&&DO                                                                           
*   TEST                                                                        
         CLI   SHOWFLAG,C'N'                                                    
         BE    TESTM050                                                         
         MVC   P+1(07),=C'05 ELT:'                                              
         GOTO1 HEXOUT,DMCB,(R6),P+10,10,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,RMKGKEY,P+34,27,=C'TOG'                              
***      MVC   P+10(10),0(R6)                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'SORT  :'                                              
         MVC   P+10(16),SORTREC                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
TESTM050 EQU   *                                                                
*   END TEST                                                                    
*&&                                                                             
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
         B     POMG0020            GET NEXT RECORD                              
*                                                                               
POMG0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***>>>>  OPEN MAKEGOOD REPORT                                                   
SHOWRECS NTR1                                                                   
*                                                                               
DSAL     EQU   1                                                                
DSALNAME EQU   7                                                                
DOFF     EQU   30                                                               
DSTAT    EQU   36                                                               
DCON     EQU   43                                                               
DMISS    EQU   55                                                               
DGROUP   EQU   61                                                               
DMASTER  EQU   65                                                               
DLINE    EQU   71                                                               
DSUBTYP  EQU   77                                                               
DMULTI   EQU   81                                                               
DSENT2ST EQU   87                                                               
DCONDATE EQU   92                                                               
*                                                                               
SREC0020 EQU   *                                                                
         BAS   RE,GETSORT          RETURN SORT RECORD                           
*                                                                               
         CLI   SOFF,X'FF'          EOF                                          
         BE    SREC0900            YES - FINISHED                               
*                                                                               
*   OUTPUT THE FIELDS FROM SORT RECORD TO PRINTER                               
*                                                                               
         MVC   P+DSAL(3),SSALPER   SALESPERSON                                  
*                                                                               
         CLC   LASTSAL,SSALPER     SALESPERSON ALREADY FOUND?                   
         BE    SREC0025            YES                                          
         XC    KEY,KEY             NO  - RETRIEVE NEW SALESPERSON               
         MVI   KEY,6               SET KEY FOR SALESPERSON                      
         MVC   KEY+22(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+24(3),SSALPER   INSERT REP CODE                              
         GOTO1 HIGH                GET KEY                                      
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETSAL           RETRIEVE S/P  RECORD                         
         MVC   LASTSNAM,RSALNAME   SAVE SALESPERSON NAME                        
         MVC   LASTSAL,SSALPER     SAVE SALESPERSON CODE                        
SREC0025 EQU   *                                                                
*                                                                               
         MVC   P+DSALNAME(20),LASTSNAM                                          
*                                                                               
         MVC   P+DOFF(2),SOFF      OFFICE                                       
         MVC   P+DSTAT(5),SSTATION STATION                                      
         GOTO1 HEXOUT,DMCB,SCON,P+DCON,4,=C'TOG'                                
*                                  CONTRACT NUMBER                              
         CLI   QOPTION1,C'S'       SELF-APPLIED REPORT?                         
         BNE   SREC0027            NO                                           
         EDIT  SMISSLIN,(2,P+DMISS),FILL=0                                      
SREC0027 EQU   *                                                                
         MVC   P+DGROUP(2),SMKGGRP MAKEGOOD GROUP                               
         CLC   =C'BILLUHR',QUESTOR SPECIAL REQUESTOR?                           
         BNE   SREC0030            NO                                           
         EDIT  (1,SMKGOFFR),(2,P+DMASTER),FILL=0                                
         EDIT  (1,SMKGOFFR+1),(2,P+DLINE),FILL=0                                
         EDIT  (1,SMKGOFFR+2),(2,P+DSUBTYP),FILL=0                              
SREC0030 EQU   *                                                                
         MVI   P+DMULTI,C'N'       SET 'NOT MULTI'                              
         CLC   LASTCON,SCON        SAME CON / BUYLINE # / GROUP?                
         BE    SREC0060            YES - ALREADY FOUND:                         
*                                     NO REDISPLAY                              
         CLC   LASTCON(5),SCON     NO  - SAME CON / BUYLINE #:                  
*                                     DIFFERENT GROUP                           
         BNE   SREC0040                                                         
*                                                                               
*   INDICATES MULTIPLE OFFERS ARE REFERENCING THE SAME LINE                     
*                                                                               
         MVI   P+DMULTI,C'Y'       SET MULTI                                    
SREC0040 EQU   *                                                                
         MVC   LASTCON,SCON        SAVE LAST CON / BUYLINE # / GROUP            
         CLI   QOPTION1,C'S'       SELF-APPLIED REPORT?                         
         BNE   SREC0050            NO                                           
*                                                                               
         XC    KEY,KEY             YES - RETRIEVE CONTRACT RECORD               
         MVI   KEY,X'8C'           SET KEY FOR CONTRACT 8C KEY                  
         MVC   KEY+21(2),RCREPFL   INSERT REP CODE                              
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),SCON                                                  
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   KEY+23(4),WORK      INSERT CONTRACT 9'S COMP                     
         GOTO1 HIGH                GET KEY                                      
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETCON           RETRIEVE CON  RECORD                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'                                                   
         BZ    SREC0043                                                         
         MVI   P+DSENT2ST,C'Y'                                                  
*                                                                               
SREC0043 EQU   *                                                                
         LA    R6,RCONREC                                                       
         USING RCONREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,P+DCONDATE)                          
         MVI   P+DCONDATE+8,C'-'                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,P+DCONDATE+9)                      
         DROP  R6                                                               
*                                                                               
SREC0045 EQU   *                                                                
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         B     SREC0020            GO BACK FOR NEXT                             
SREC0050 EQU   *                                                                
         GOTO1 LOCALREP,DMCB,ALLTEXT2                                           
         B     SREC0020            GO BACK FOR NEXT                             
SREC0060 EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE                              
         B     SREC0020            SKIP THE LINE                                
SREC0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SORTGEN  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*&&DO                                                                           
         MVC   P+1(08),=C'SORT OUT'                                             
         MVC   P+10(16),SORTREC                                                 
         GOTO1 REPORT                                                           
*&&                                                                             
SGEN0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GETSORT  NTR1                                                                   
         CLI   SOFF,X'FF'          EOF REACHED?                                 
         BE    GS025               YES                                          
         MVI   SOFF,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6               TEST RETURN ZERO=EOF                         
         BZ    GS025               YES -                                        
         MVC   SORTREC,0(R6)                                                    
*&&DO                                                                           
         MVC   P+1(08),=C'SORT IN '                                             
         MVC   P+10(16),SORTREC                                                 
         GOTO1 REPORT                                                           
*&&                                                                             
GS025    EQU   *                                                                
         XIT1                                                                   
*                                                                               
LOCALREP NTR1                                                                   
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LREP30              NO - JUST PRINT LINE                         
*                                                                               
         MVI   LINE,1              SET LINECOUNT TO A PERPETUAL 1               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
LREP10   EQU   *                                                                
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LREP20                                                           
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LREP10                                                           
LREP20   EQU   *                                                                
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LREPGOOD                                                         
LREP30   GOTO1 REPORT                                                           
*                                                                               
LREPGOOD EQU   *                                                                
         XIT1                                                                   
POST     NTR1                                                                   
*                                                                               
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         USING ALTHDR,RF           ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETMKG   LA    RF,RMKGREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETSAL   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTCON   LA    RF,RCONREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
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
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    EQU   *                                                                
         MVC   P+1(14),=C'REC NOT FOUND:'                                       
         MVC   P+22(27),KEY                                                     
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*&&DO                                                                           
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*&&                                                                             
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
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
*                                                                               
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'U'       HARD UPDATE RUN?                             
         BNE   MODEEXIT            NO  - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
ALLTEXT  EQU   *                                                                
         DC    C'O',AL1(01),C'T',AL1(03)   OFFSET / SP CODE                     
         DC    C'O',AL1(03),C'T',AL1(20)   OFFSET / SP NAME                     
         DC    C'O',AL1(03),C'T',AL1(02)   OFFSET / OFFICE                      
         DC    C'O',AL1(04),C'T',AL1(05)   OFFSET / STATION                     
         DC    C'O',AL1(02),C'N',AL1(08)   OFFSET / CONTRACT NUMBER             
         DC    C'O',AL1(04),C'N',AL1(02)   OFFSET / BUYLINE #                   
         DC    C'O',AL1(04),C'T',AL1(02)   OFFSET / MAKEGOOD GROUP              
         DC    C'O',AL1(18),C'T',AL1(01)   OFFSET / MULTI FLAG                  
         DC    C'O',AL1(05),C'T',AL1(01)   OFFSET / SEND                        
         DC    C'O',AL1(04),C'T',AL1(17)   OFFSET / CONTRACT DATE               
         DC    X'0000'                                                          
         DS    0H                                                               
*                                                                               
ALLTEXT2 EQU   *                                                                
         DC    C'O',AL1(01),C'T',AL1(03)   OFFSET / SP CODE                     
         DC    C'O',AL1(03),C'T',AL1(20)   OFFSET / SP NAME                     
         DC    C'O',AL1(03),C'T',AL1(02)   OFFSET / OFFICE                      
         DC    C'O',AL1(04),C'T',AL1(05)   OFFSET / STATION                     
         DC    C'O',AL1(02),C'N',AL1(08)   OFFSET / CONTRACT NUMBER             
*                                                                               
*   ORIGINAL                                                                    
*                                                                               
****>>   DC    C'O',AL1(04),C'N',AL1(02)   OFFSET / BUYLINE #                   
****>>   DC    C'O',AL1(04),C'T',AL1(02)   OFFSET / MAKEGOOD GROUP              
*                                                                               
*   REPLACED BY                                                                 
         DC    C'O',AL1(10),C'T',AL1(02)   OFFSET / MAKEGOOD GROUP              
*                                                                               
         DC    C'O',AL1(18),C'T',AL1(01)   OFFSET / MULTI FLAG                  
         DC    X'0000'                                                          
         DS    0H                                                               
*                                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
*                                                                               
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
PULSECTR DS    F                   CONTRACTS PROCESSED CTR                      
TOTALCTR DS    F                                                                
AIOAREA  DS    A                                                                
*                                                                               
SORTREC  DS    0CL32                                                            
SSALPER  DS    CL3                 SALESPERSON CODE                             
SOFF     DS    CL2                 OFFICE                                       
SSTATION DS    CL5                 STATION                                      
SCON     DS    CL4                 CONTRACT                                     
SMISSLIN DS    CL1                 MISSED LINE NUMBER                           
SMKGGRP  DS    CL2                 MAKEGOOD GROUP                               
SMKGOFFR DS    CL3                 MAKEGOOD OFFER LINE                          
         DS    CL12                SPARE                                        
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=032'                                   
*                                                                               
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
PASSKEY  DS    CL27                SELF-APPLIED KEY AREA                        
LASTCON  DS    CL7                 LAST CON # / BUYLINE / GROUP                 
LASTSAL  DS    CL3                 LAST SALESPERSON                             
LASTSNAM DS    CL20                LAST S/P NAME                                
TARGETS  DS    CL100               POSSIBLE TARGET LINES                        
ELCODE   DS    CL1                                                              
SHOWFLAG DC    CL1'N'                                                           
COMMAND  DS    CL8                                                              
SALPER   DS    CL3                 MAKEGOOD SALESPERSON CODE                    
         SPACE 2                                                                
FILLER   DS    6000C                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
*   MAKEGOOD RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,               
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENMKG                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'178REREP1H02 05/25/07'                                      
         END                                                                    
