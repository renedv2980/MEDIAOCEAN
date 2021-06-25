*          DATA SET RECNT5B    AT LEVEL 043 AS OF 12/08/04                      
*PHASE T8025BA,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T8025B - RECNT5B - VCI EC: BDE INTERFACE'                       
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT5B --- VCI      ELECTRONIC CONTRACT INTERFACE         *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
* NOTE:  FIELD SEPARATORS ARE TO BE ASCII TILDAS.                   *           
*                                                                   *           
* AUG02/95 (BU ) --- REP ORDER COMMENT, AGENCY LIABILITY, CREDIT    *           
*                    ADVISORY ADDED                                 *           
*                                                                   *           
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                             *           
*                                                                   *           
* 29MAY96 (SKU) --- EXPAND EST NUM                                  *           
*                                                                   *           
* 26JUL96 (SKU) --- CHANGE TO USE THMS FOR TIME STAMPING            *           
*                                                                   *           
* 24JUL97 (SKU) --- 4K CONTRACT SUPPORT                             *           
*                                                                   *           
* 12MAY98 (BU ) --- UPGRADE PER SPEC RECV'D 12MAY98 - TELECNFRNCE   *           
*                   11MAY98/1130AM:  L. PUTNAM, PAUL?? OF VCI       *           
*                                                                   *           
*                                                                   *           
* 26JUL99 (BU ) --- DON'T SEND CANCELLED BUYS                       *           
*                                                                   *           
* 23MAY01 (BU ) --- FIX FORMAT OF ESTIMATE NUMBER                   *           
*                                                                   *           
* 25MAY01 (BU ) --- ADD PROGRAM NAME AS COMMENT                     *           
*                                                                   *           
* 29MAY01 (BU ) --- MOVE PROGRAM NAME TO LINE INSTRUCTION, FIX      *           
*                   BUG ALSO.                                       *           
*                                                                   *           
* 16APR02 (BU ) --- CHANGE EDICT TO BDE                             *           
*                                                                   *           
* 08DEC04 (BU ) --- A DAY STRING > 11 CHARS COULD NOT BE HANDLED    *           
*                   BY DAYUNPK.  DAYUNPK MADE LOCAL AND MODIFIED.   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T8025B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T8025B*,R9,RR=R8                              
         ST    R8,RELX                                                          
         LR    R3,RC               SET A(MODULE WORK SPACE)                     
         USING LOCALWRK,R3         SET DSECT FOR AREA                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPULAR                                                       
         ST    R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*********************************************************************           
*        MAIN LINE PROCESSING                                                   
*********************************************************************           
MAIN     EQU   *                                                                
         BAS   RE,INIT             SET INITAL ADDRS AND VALUES                  
         BAS   RE,GETCON           READ CONTRACT RECORD                         
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(GETSTA),DMCB,(RC),RCONKSTA,RR=YES                             
*                                  READ STATION RECORD                          
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(FINDID),DMCB,(RC),RR=YES      FIND SEND ID                    
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(PQOPENA),DMCB,(RC),(R8),RR=YES                                
*                                  OPEN PRINT QUEUE                             
*                                                                               
         GOTO1 =A(EDICT),DMCB,(RC),(R8),RR=YES                                  
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
         BAS   RE,CALCTOTS         CYCLE BUY RECS FOR                           
*                                     TOT SPOTS/TOTAL DOLLARS                   
         XC    EXTRAKEY,EXTRAKEY   CLEAR FLAG FOR RE-READING BUYS               
         BAS   RE,DESCRECS         GENERATE DESCRIPTIVE RECORDS:                
*                                     START/AGENCY/ADVERTISER/SALES             
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         HEADER                                       
*                                                                               
MN400    EQU   *                                                                
         BAS   RE,BLD03REC         LINE ADD                                     
*                                                                               
MN500    EQU   *                                                                
         BAS   RE,BLD10REC         'END'                                        
*                                                                               
MN600    EQU   *                                                                
         GOTO1 =A(DATETIME),DMCB,(RC),RR=Y                                      
*                                  DATE AND TIME STAMP                          
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE SCANS ALL THE BUY RECORDS, AND CALCULATES THE                 
*          TOTAL # OF SPOTS AND TOTAL DOLLARS OF THE ORDER FOR THE              
*          HEADER RECORD.  ***NOTE***  AS THESE ARE ORIGINAL ORDERS             
*          ONLY, THERE IS NO - REPEAT, NO - PROVISION TO BACK OUT               
*          MAKEGOOD MISSED SPOTS, BECAUSE THERE SHOULDN'T BE ANY!!              
*          THIS, OF COURSE, ONLY PERTAINS TO THOSE ORDERS CREATED               
*          UNDER THE OLD MAKEGOOD STRUCTURE, WHERE MISSED SPOTS WERE            
*          NOT SUBTRACTED FROM THE EFFECTIVE DATE ELEMENTS, AND THE             
*          DATES OF THE ELEMENTS ADJUSTED ACCORDINGLY.  FOR NEWER               
*          ORDERS, MAKEGOODS ARE NOT A CONSIDERATION AT ALL.  B.UHR.            
*                                                                               
CALCTOTS NTR1                                                                   
CTOT0020 EQU   *                                                                
         GOTO1 =A(READBUY),DMCB,(RC),RR=Y                                       
         BZ    CTOT0500            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    CTOT0020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    CTOT0020            DON'T ADD INTO TOTALS                        
         LA    R2,RBUYELEM         SET A(01 DESCRIPTOR ELEMENT)                 
CTOT0040 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    CTOT0020            YES - GO BACK FOR ANOTHER RECORD             
         CLI   0(R2),3             EFFECTIVE DATE ELEMENT?                      
         BNE   CTOT0040            NO  - GO BACK FOR NEXT ELEMENT               
*                                                                               
         USING RBUYDTEL,R2                                                      
*                                                                               
         ZIC   RF,RBUYDTNW         SET NUMBER PER WEEK                          
         SR    RE,RE                                                            
         ZIC   R1,RBUYDTWK         SET NUMBER OF WEEKS                          
         MR    RE,R1               #/WK * # SPOTS/WK = # SPOTS                  
         L     RE,TLSPOTS          ACCUMULATE NUMBER OF SPOTS                   
         AR    RE,RF                                                            
         ST    RE,TLSPOTS          REPLACE TOTAL SPOTS                          
         SR    RE,RE                                                            
         ZICM  R1,RBUYCOS,4        LOAD SPOT COST                               
         MR    RE,R1               # SPOTS * $/SPOT = TOTAL $$                  
         L     RE,TOTALAMT         ACCUMULATE TOTAL DOLLARS                     
         AR    RE,RF                                                            
         ST    RE,TOTALAMT         REPLACE TOTAL DOLLARS                        
         B     CTOT0040            GO BACK FOR NEXT X'03' ELT                   
CTOT0500 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        DESCRECS  ---  GENERATES START/AGENCY/ADVERT/SALES RECS                
*********************************************************************           
DESCRECS NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,1               INSERT RECORD TYPE                           
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     REP FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                REP NOT FOUND?????                           
         GOTO1 VGETREC,DMCB,RREPREC                                             
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(5),=C'START'                                             
         MVI   RCSTART+5,X'A1'     INSERT TILDA                                 
         MVC   RCSTART+6,RREPABBR  INSERT SHORT NAME (FILENAME)                 
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0A'           INSERT RECORD TYPE: AGENCY                   
         MVC   KEY+19(4),RCONKAGY  INSERT AGENCY CODE                           
         MVC   KEY+23(2),RCONKAOF  INSERT AGENCY OFFICE CODE                    
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     AGENCY FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                AGENCY NOT FOUND?????                        
         GOTO1 VGETREC,DMCB,RAGYREC                                             
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(6),=C'AGENCY'                                            
         MVI   RCSTART+6,X'A1'     INSERT TILDA                                 
         LA    R7,RCSTART+7        SET A(INSERT FIELD ADDR)                     
         GOTO1 LOADDATA,DMCB,(33,RAGYNAM2),(X'A1',0)                            
*                                  INSERT AGENCY NAME (33C),TILDA               
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
*                                                                               
***      GOTO1 LOADDATA,DMCB,(20,RAGYADD1),(X'A1',0)                            
*                                  INSERT AGENCY ADD1 (20C),TILDA               
***      L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
***      GOTO1 LOADDATA,DMCB,(20,RAGYADD2),(X'A1',0)                            
*                                  INSERT AGENCY ADD2 (20C),TILDA               
***      L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
***      MVI   0(R7),X'A1'         INSERT TILDA:  NO 'CITY'                     
***      LA    R7,1(R7)            SKIP TILDA                                   
***      GOTO1 LOADDATA,DMCB,(2,RAGYSTAT),(X'A1',0)                             
*                                  INSERT AGENCY STATE (2C),TILDA               
***      L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
***      GOTO1 LOADDATA,DMCB,(10,RAGYZIP),(X'40',0)                             
*                                  INSERT AGENCY ZIP (10C),SPACE                
         GOTO1 LOADDATA,DMCB,(06,RAGYKAGY),(X'40',0)                            
*                                  INSERT AGY/OFF CODE (MAX 6C),SPACE           
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         MVC   SAVRISK,RAGYRISK    SAVE AGENCY RISK VALUE                       
         MVC   SAVLIAB,RAGYLIAB    SAVE AGENCY LIABILITY CODE                   
*                                                                               
         GOTO1 DISPFC              RETRIEVE LIABILITY COMMENT                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'08'           INSERT RECORD TYPE: ADVERT                   
         MVC   KEY+21(4),RCONKADV  INSERT ADVERT CODE                           
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     ADVERT FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                ADVERT NOT FOUND?????                        
         GOTO1 VGETREC,DMCB,RADVREC                                             
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(10),=C'ADVERTISER'                                       
         MVI   RCSTART+10,X'A1'     INSERT TILDA                                
         LA    R7,RCSTART+11       SET A(INSERT FIELD ADDR)                     
         GOTO1 LOADDATA,DMCB,(20,RADVNAME),(X'A1',0)                            
*                                  INSERT ADVERT NAME (20C),TILDA               
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
**       MVI   0(R7),X'A1'         INSERT TILDA:  NO ADDRESS 1                  
**       LA    R7,1(R7)            SKIP TILDA                                   
**       MVI   0(R7),X'A1'         INSERT TILDA:  NO ADDRESS 2                  
**       LA    R7,1(R7)            SKIP TILDA                                   
**       GOTO1 LOADDATA,DMCB,(20,RADVCITY),(X'A1',0)                            
*                                  INSERT ADVERT CITY (20C),TILDA               
**       L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
**       MVI   0(R7),X'A1'         INSERT TILDA:  NO 'STATE'                    
**       LA    R7,1(R7)            SKIP TILDA                                   
         GOTO1 LOADDATA,DMCB,(04,RADVKADV),(X'40',0)                            
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'06'           INSERT RECORD TYPE: S/P                      
         MVC   KEY+22(2),RCONKREP  INSERT REP CODE                              
         MVC   KEY+24(3),RCONSAL   INSERT S/P    CODE                           
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     S/P    FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                S/P    NOT FOUND?????                        
         GOTO1 VGETREC,DMCB,RSALREC                                             
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(05),=C'SALES'                                            
         MVI   RCSTART+05,X'A1'     INSERT TILDA                                
         LA    R7,RCSTART+06       SET A(INSERT FIELD ADDR)                     
         GOTO1 LOADDATA,DMCB,(20,RSALNAME),(X'A1',0)                            
*                                  INSERT S/P    NAME (20C),TILDA               
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'04'           INSERT RECORD TYPE: OFFICE                   
         MVC   KEY+23(2),RCONKREP  INSERT REP CODE                              
         MVC   KEY+25(2),RSALOFF   INSERT S/P    OFFICE                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     S/P    FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                S/P    NOT FOUND?????                        
         GOTO1 VGETREC,DMCB,ROFFREC                                             
         GOTO1 LOADDATA,DMCB,(20,ROFFNAME),(X'A1',0)                            
*                                  INSERT REP OFF NAME (20C),TILDA              
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         MVC   0(3,R7),=C'100'     INSERT S/P PERCENT:  CONSTANT                
         LA    R7,3(R7)            PASS S/P PERCENT                             
         MVI   0(R7),X'A1'         INSERT TILDA                                 
         LA    R7,1(R7)            SKIP TILDA                                   
         GOTO1 LOADDATA,DMCB,(03,RCONSAL),(X'A1',0)                             
*                                  INSERT S/P CODE (3C), TILDA                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 LOADDATA,DMCB,(02,RCONKOFF),(X'40',0)                            
*                                  INSERT S/P OFFC (2C), SPACE                  
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIABILITY COMMENT RECORDS                        *         
*        COMMENTS ARE SAVED IN IO3 UNTIL NEEDED                       *         
***********************************************************************         
DISPFC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTREC,R6                                                       
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE(4),=C'LIAB'  COMMENT CODE                               
         LA    R2,RCMTKCDE+4                                                    
         EDIT  SAVLIAB,(2,(R2)),FILL=0                                          
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R6                                                               
*                                                                               
DISPFCED EQU   *                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DIFC0100            KEY NOT FOUND                                
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RCMTREC,R6                                                       
         MVI   ELCODE,2            RETRIEVE FIRST COMMENT TEXT ELT              
         BAS   RE,GETEL                                                         
         BNE   DIFC0100            NO ELEMENT FOUND                             
         USING RCMTELM2,R6                                                      
*                                                                               
         L     R2,AIO3             SET A(SAVE AREA)                             
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R2),(RF)          CLEAR SAVE AREA                              
DIFC0020 EQU   *                                                                
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DIFC0060            MORE THAN 3 CHARACTERS FOUND                 
         CLI   RCMT2TXT,C' '       3 OR LESS: 1ST CHAR A SPACE?                 
         BNE   DIFC0060            NO  - MUST BE NON-SPACE                      
DIFC0040 EQU   *                                                                
         BAS   RE,NEXTEL           R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    DIFC0020            ANOTHER ELEMENT FOUND                        
         B     DIFC0100            NONE FOUND - FINISHED                        
*                                                                               
DIFC0060 DS    0H                                                               
         ZIC   R1,RCMT2LEN         GET ELEMENT LENGTH                           
         SH    R1,=H'3'            SUBTRACT CONTROL + EX ADJUSTMENT             
         EX    R1,DIFC0080         MOVE BY LENGTH                               
         LA    R2,70(R2)           BUMP TO NEXT SAVE AREA                       
         B     DIFC0040            GO BACK FOR NEXT                             
DIFC0080 MVC   0(0,R2),RCMT2TXT    EX STATEMENT                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
DIFC0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
*                                                                               
*********************************************************************           
*    LOADDATA  ---  STRINGS DATA INTO RECORD AREA.  BACKS UP TO                 
*        FIND LAST NON-SPACE CHARACTER.  INSERTS INDICATED                      
*        CHARACTER AFTER THAT.  PASSES BACK A(NEXT INSERTION).                  
*           P1   =  1ST CHARACTER = L(DATA)                                     
*                   2-4 CHARACTER = A(DATA)                                     
*           P2   =  1ST CHARACTER = DELIMITER CHARACTER                         
*           R7   =  A(INSERTION POINT)                                          
*         ABUILD = RETURNED INSERTION POINT                                     
*         R1 WILL REMAIN POINTING TO PARAMETER LIST                             
*                                                                               
*********************************************************************           
*                                                                               
LOADDATA NTR1                                                                   
         LR    R2,R7               SET A(INSERTION POINT)                       
         ZIC   R4,0(R1)            SET L(DATA TO TRANSFER)                      
         ZICM  R5,1(R1),3          SET A(DATA TO TRANSFER)                      
         BCTR  R4,0                DECREMENT FOR MOVE                           
         EX    R4,LDAT0500         MOVE DATA BY LENGTH                          
         LA    R4,1(R4)            SET BACK TO FULL LENGTH                      
         AR    R2,R4               SET A(1 POS PAST LAST CHAR)                  
LDAT0020 EQU   *                                                                
         BCTR  R2,0                BACK UP 1 POSITION                           
         CLI   0(R2),C' '          TRAILING SPACE FOUND?                        
         BE    LDAT0020            YES - BACK UP ANOTHER                        
         CLI   0(R2),X'00'         TRAILING LOW-VALUES FOUND?                   
*                                     (PROBABLY NOT NECESSARY)                  
         BE    LDAT0020            YES - BACK UP ANOTHER                        
         LA    R2,1(R2)            CHARACTER ENCOUNTERED: SKIP PAST             
         MVC   0(1,R2),4(R1)       INSERT SEPARATOR CHARACTER                   
         LA    R2,1(R2)            BUMP PAST                                    
         ST    R2,ABUILD           SET NEW INSERTION ADDRESS                    
         XIT1                                                                   
*                                                                               
LDAT0500 MVC   0(0,R2),0(R5)                                                    
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        BLD01REC  ---  BUILDS THE HEADER TRANSACTION RECORD                    
*********************************************************************           
BLD01REC NTR1                                                                   
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(06),=C'HEADER'                                           
         MVI   RCSTART+06,X'A1'     INSERT TILDA                                
         LA    R7,RCSTART+7        SET A(INSERT FIELD ADDR)                     
         GOTO1 DATCON,DMCB,(5,WORK),(10,WORK2)                                  
*                                  TODAY'S DATE->MM/DD/YY EBCDIC                
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT TODAY'S DATE (8C),TILDA               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 DATCON,DMCB,(0,WORK),(10,WORK2)                                  
*                                  START DATE->MM/DD/YY EBCDIC                  
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT START DATE (8C),TILDA                 
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 DATCON,DMCB,(0,WORK+6),(10,WORK2)                                
*                                  END   DATE->MM/DD/YY EBCDIC                  
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT END   DATE (8C),TILDA                 
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         EDIT  TLSPOTS,(8,WORK2),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT TOTAL SPOTS (8C),TILDA                
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         EDIT  TOTALAMT,(12,WORK2),ALIGN=LEFT,ZERO=NOBLANK                      
         GOTO1 LOADDATA,DMCB,(12,WORK2),(X'A1',0)                               
*                                  INSERT TOTAL $$ (8C),TILDA                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 FINDEST,DMCB,(RC)                                                
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
*                                     NOT CHANGED IF NOT FOUND                  
         BNZ   BLD10020            EST# FOUND                                   
         MVI   0(R7),X'A1'         INSERT TILDA:  NO ESTIMATE # FOUND           
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
BLD10020 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK2,4,=C'TOG'                             
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT CONTRACT# (8C),TILDA                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         MVI   0(R7),X'A1'         INSERT TILDA:  NO ORDER DESCRIPT             
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         GOTO1 LOADDATA,DMCB,(20,PRODNAME),(X'A1',0)                            
*                                  INSERT PRODUCT NAME (20C),TILDA              
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         MVI   0(R7),X'A1'         INSERT TILDA:  NO SPECIAL BILL               
*                                     HANDLING REQUIREMENTS                     
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         GOTO1 LOADDATA,DMCB,(4,RCONKSTA),(X'40',0)                             
*                                  INSERT STATION CALL (20C),SPACE              
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
*                                  PUT OUT CONTRACT COMMENTS                    
*                                                                               
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(14),=C'HEADER COMMENT'                                   
         MVI   RCSTART+14,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+15       SET A(INSERT FIELD ADDR)                     
         GOTO1 FCOMMNTS,DMCB,(RC)                                               
*                                  PUT OUT CONTRACT COMMENTS                    
*                                                                               
*                                  PUT OUT REP ORDER COMMENTS                   
*                                                                               
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(19),=C'HEADER INSTRUCTIONS'                              
         MVI   RCSTART+19,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+20       SET A(INSERT FIELD ADDR)                     
         GOTO1 RCOMMNTS,DMCB,(RC)                                               
*                                  PUT OUT REP ORDER COMMENTS                   
         L     RF,AIO3             SET A(LIABILITY COMMENT AREA)                
         OC    0(20,RF),0(RF)      ANY LIABILITY COMMENTS?                      
         BZ    BLD10060            NO                                           
*                                                                               
         GOTO1 LCOMMNTS,DMCB,(RC)                                               
*                                  YES - PUT OUT LIABILITY COMMENTS             
BLD10060 EQU   *                                                                
         CLI   SAVRISK,0           ANY RISK COMMENT?                            
         BE    BLD10100            NO                                           
         GOTO1 XCOMMNTS,DMCB,(RC)  YES - PUT OUT RISK DESCRIPTION               
BLD10100 EQU   *                                                                
*                                                                               
         MVI   RCSTART,C' '        SPACE FILL THE RECORD                        
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(10),=C'HEADER END'                                       
*                                     PUT OUT HEADER END RECORD                 
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
*********************************************************************           
*        FINDEST   ---  FIND THE X'A2' ELEMENT - EASI CODE ELT      *           
*********************************************************************           
FINDEST  NTR1                                                                   
         LA    R4,RCONELEM         SET A(DESCRIPTOR ELEMENT)                    
FEST0100 EQU   *                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R4),0             END OF RECORD?                               
         BE    FEST0200            YES - X'A2' NOT FOUND                        
         CLI   0(R4),X'A2'         EASI CODE ELT?                               
         BNE   FEST0100            NO  - GO BACK FOR NEXT                       
         USING RCONIEL,R4          YES - EDIT THE ESTIMATE NUMBER               
*                                                                               
         OC    RCONXEST,RCONXEST                                                
         BZ    FEST0110                                                         
****     EDIT  (4,RCONXEST),(8,WORK2),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   WORK2(10),RCONXEST  UNLOAD ESTIMATE                              
         OC    WORK2(10),SPACES                                                 
         B     FEST0120                                                         
*                                                                               
FEST0110 DS    0H                                                               
         EDIT  RCONIEST,(8,WORK2),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
FEST0120 DS    0H                                                               
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT ESTIMATE # (8C),TILDA                 
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     FEST0300            EXIT                                         
FEST0200 EQU   *                                                                
         ST    R7,ABUILD           ENSURE A(NEXT SPOT) IS SET                   
         SR    R0,R0               SET CC = ZERO                                
FEST0300 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
*        FCOMMNTS  ---  FIND THE X'02' ELEMENTS - COMMENT ELTS      *           
*********************************************************************           
FCOMMNTS NTR1                                                                   
         MVI   COMMFLAG,C'N'       SET COMMENT FLAG TO 'NO'                     
         LA    R5,RCONELEM         SET A(DESCRIPTOR ELEMENT)                    
FCOM0100 EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R5),0             END OF RECORD?                               
         BE    FCOM0200            YES - FINISHED                               
         CLI   0(R5),X'02'         COMMENT ELT?                                 
         BNE   FCOM0100            NO  - GO BACK FOR NEXT                       
         GOTO1 =A(CHKCMTS),DMCB,(RC),(R5),1,RR=Y                                
         BZ    FCOM0120            STORED COMMENT FOUND - PUT IT OUT            
         ZIC   R4,1(R5)            YES - GET ELEMENT LENGTH                     
         SH    R4,=H'3'            SUBTRACT 2 FOR CTRL, 1 FOR EX                
         EX    R4,FCOM0500         MOVE COMMENT BY LENGTH                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         MVI   COMMFLAG,C'Y'       SET COMMENT FLAG TO 'YES'                    
         MVI   RCSTART+15,C' '                                                  
         MVC   RCSTART+16(100),RCSTART+15                                       
         B     FCOM0100            GO BACK FOR NEXT ELEMENT                     
FCOM0120 EQU   *                                                                
         L     R2,AIO4             SET A(COMMENT AREA)                          
FCOM0130 EQU   *                                                                
         CLI   0(R2),X'FF'         LAST COMMENT?                                
         BE    FCOM0100            YES - GO LOOK FOR ANOTHER X'02'              
         CLI   0(R2),0             ANYTHING IN FIELD?                           
         BE    FCOM0100            NO  - GO LOOK FOR ANOTHER X'02'              
         ZIC   RF,0(R2)            TAKE LENGTH                                  
         SH    RF,=H'2'            SUBTRACT 1 FOR CNTRL, 1 FOR EX               
         EX    RF,FCOM0600         MOVE COMMENT BY LENGTH                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         MVI   COMMFLAG,C'Y'       SET COMMENT FLAG TO 'YES'                    
         MVI   RCSTART+15,C' '                                                  
         MVC   RCSTART+16(100),RCSTART+15                                       
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         B     FCOM0130            GO BACK FOR NEXT LINE                        
FCOM0200 EQU   *                                                                
         CLI   COMMFLAG,C'Y'       COMMENTS FOUND?                              
         BE    FCOM0300            YES                                          
         GOTO1 PRINTREC,DMCB,(RC)  NO  - PUT OUT AN EMPTY RECORD                
FCOM0300 EQU   *                                                                
         XIT1                                                                   
FCOM0500 MVC   RCSTART+15(0),2(R5) MOVE COMMENT BY LENGTH                       
FCOM0600 MVC   RCSTART+15(0),1(R2) MOVE COMMENT BY LENGTH                       
         EJECT                                                                  
*********************************************************************           
*        RCOMMNTS  ---  FIND THE X'82' ELEMENTS - COMMENT ELTS      *           
*********************************************************************           
RCOMMNTS NTR1                                                                   
         LA    R5,RCONELEM         SET A(DESCRIPTOR ELEMENT)                    
RCOM0100 EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R5),0             END OF RECORD?                               
         BE    RCOM0200            YES - FINISHED                               
         CLI   0(R5),X'82'         REP ORDER COMMENT ELT?                       
         BNE   RCOM0100            NO  - GO BACK FOR NEXT                       
         GOTO1 =A(CHKCMTS),DMCB,(RC),(R5),2,RR=Y                                
         BZ    RCOM0120            STORED COMMENT FOUND - NEXT                  
         ZIC   R4,1(R5)            YES - GET ELEMENT LENGTH                     
         SH    R4,=H'3'            SUBTRACT 2 FOR CTRL, 1 FOR EX                
         EX    R4,RCOM0500         MOVE COMMENT BY LENGTH                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         MVI   COMMFLAG,C'Y'       SET COMMENT FLAG TO 'YES'                    
         MVI   RCSTART+20,C' '                                                  
         MVC   RCSTART+21(95),RCSTART+20                                        
         B     RCOM0100            GO BACK FOR NEXT ELEMENT                     
RCOM0120 EQU   *                                                                
         L     R2,AIO4             SET A(COMMENT AREA)                          
RCOM0130 EQU   *                                                                
         CLI   0(R2),X'FF'         LAST COMMENT?                                
         BE    RCOM0100            YES - GO LOOK FOR ANOTHER X'02'              
         CLI   0(R2),0             ANYTHING IN FIELD?                           
         BE    RCOM0100            NO  - GO LOOK FOR ANOTHER X'02'              
         ZIC   RF,0(R2)            TAKE LENGTH                                  
         SH    RF,=H'2'            SUBTRACT 1 FOR CNTRL, 1 FOR EX               
         EX    RF,RCOM0600         MOVE COMMENT BY LENGTH                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         MVI   COMMFLAG,C'Y'       SET COMMENT FLAG TO 'YES'                    
         MVI   RCSTART+20,C' '                                                  
         MVC   RCSTART+21(100),RCSTART+20                                       
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         B     RCOM0130            GO BACK FOR NEXT LINE                        
RCOM0200 EQU   *                                                                
         CLI   COMMFLAG,C'Y'       COMMENTS FOUND?                              
         BE    RCOM0300            YES                                          
*                                                                               
*                                  DON'T PUT OUT EMPTY RECORD                   
****>>   GOTO1 PRINTREC,DMCB,(RC)  NO  - PUT OUT AN EMPTY RECORD                
RCOM0300 EQU   *                                                                
         XIT1                                                                   
RCOM0500 MVC   RCSTART+20(0),2(R5) MOVE COMMENT BY LENGTH                       
RCOM0600 MVC   RCSTART+20(0),1(R2) MOVE COMMENT BY LENGTH                       
         EJECT                                                                  
*********************************************************************           
*        LCOMMNTS  ---  PUT OUT THE LIABILITY COMMENTS              *           
*********************************************************************           
LCOMMNTS NTR1                                                                   
         L     R5,AIO3             SET A(LIABILITY COMMENT STORAGE)             
LCOM0100 EQU   *                                                                
         OC    0(10,R5),0(R5)      ANYTHING IN FIELD?                           
         BZ    LCOM0300            NO  - FINISHED                               
         LR    R6,R5               SAVE ORIGINAL FIELD START                    
         LA    R4,RCSTART+20       FIRST POSITION OF COMMENT O/P                
         LA    RF,70               MAX SIZE OF COMMENT                          
LCOM0120 EQU   *                                                                
         CLI   0(R6),X'00'         BINARY ZERO IN FIELD?                        
         BE    LCOM0160            YES - COMMENT FINISHED                       
         MVC   0(1,R4),0(R6)       NO  - MOVE COMMENT TO O/P                    
         LA    R4,1(R4)            BUMP TO NEXT CHAR                            
         LA    R6,1(R6)            BUMP TO NEXT CHAR                            
         BCT   RF,LCOM0120                                                      
LCOM0160 EQU   *                                                                
         GOTO1 PRINTREC,DMCB,(RC)  OUTPUT THE LIABILITY COMMENT                 
         MVI   RCSTART+20,C' '                                                  
         MVC   RCSTART+21(95),RCSTART+20                                        
         LA    R5,70(R5)           BUMP TO NEXT POSSIBLE COMMENT                
         B     LCOM0100            GO BACK FOR NEXT ELEMENT                     
LCOM0300 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        XCOMMNTS  ---  PUT OUT THE RISK COMMENT                    *           
*********************************************************************           
XCOMMNTS NTR1                                                                   
         LA    RE,RISKTAB                                                       
         ZIC   RF,SAVRISK          GET RISK                                     
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         MH    RF,=H'40'                                                        
         AR    RE,RF               BUMP TO CORRECT SPACE                        
         MVC   RCSTART+20(40),0(RE)                                             
         GOTO1 PRINTREC,DMCB,(RC)  OUTPUT THE RISK DESCRIPTION                  
         MVI   RCSTART+20,C' '                                                  
         MVC   RCSTART+21(95),RCSTART+20                                        
         XIT1                                                                   
       ++INCLUDE RERISKTAB                                                      
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE LINE TRANSACTION RECORD.                           
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE LINE RECORD PER EFFECTIVE DATE IS GENERATED.                     
*      2.  COMMENTS ARE GEN'D AS LINE COMMENTS RECORDS                          
*                                                                               
BLD03REC NTR1                                                                   
BLD30020 EQU   *                                                                
         MVI   ORBITCOM,C'N'       SET ORBIT COMMENTS NEEDED FLAG               
         MVI   ALTWKCOM,C'N'       ALTERNATING WEEKS COMMENT FLAG               
         XC    COMFLAGS,COMFLAGS   CLEAR FLAGS FOR THIS BUY                     
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    COLLAPS2,COLLAPS2   USED IN COLLAPSE BUY ROUTINE                 
         XC    TESTCTR,TESTCTR     **TEST                                       
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         GOTO1 =A(READBUY),DMCB,(RC),RR=Y                                       
         BZ    BLD30260            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
BLD30040 EQU   *                                                                
         GOTO1 BLD3CTCM,DMCB,(RC)  SET MG + COMMENT FLAGS                       
         MVC   RCSTART(4),=C'LINE' INSERT RECORD ID                             
         MVI   RCSTART+4,X'A1'     INSERT TILDA                                 
*                                                                               
         XC    FIRSTSW,FIRSTSW     INITIALIZE 'ALTERNATE WEEK' IND              
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        GET EFFECTIVE DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                 NOT FOUND                                    
*                                                                               
         DC    H'0'                NO EFFECTIVE DATE ELEMENT?                   
*                                                                               
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         USING RBUYDTCD,R6                                                      
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK INDIC SET?                    
         BNO   BLD30060            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
*                                                                               
         DROP  R6                                                               
*                                                                               
*   LOOP THROUGH MULTIPLE EFFECTIVE DATES                                       
*                                                                               
BLD30060 EQU   *                                                                
         LA    R7,RCSTART+5        SET A(INSERT FIELD ADDR)                     
         ST    R7,ABUILD           SAVE INSERT FIELD ADDR                       
         L     R6,AEFFDATE         RESET A(EFF DATE ELEMENT)                    
         ZIC   RF,1(R6)            L(EFFECTIVE DATE ELEMENT)                    
         BCTR  RF,0                DECREMENT 1                                  
         EX    RF,BLD30080         MOVE EFF DATE TO TABLE                       
         B     BLD30100                                                         
BLD30080 MVC   BLDTABLE(0),0(R6)                                                
BLD30100 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      ANY MORE '03' ELEMENTS?                      
         BE    BLD30220            NO                                           
         GOTO1 DTEINFO,DMCB,(RC),(R6)                                           
         CLI   SPOTSWK,0           ANY SPOTS IN THIS WEEK?                      
         BZ    BLD30200            NO  - DON'T PROCESS BUY                      
*                                                                               
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30120            NO                                           
         MVI   ALTWKCOM,C'Y'       SET ALT WEEK COMMENT NEEDED                  
BLD30120 EQU   *                                                                
         L     R7,ABUILD           SET PAST START TIMES                         
         MVI   WORK2,C'2'                                                       
         GOTO1 LOADDATA,DMCB,(1,WORK2),(X'A1',0)                                
*                                  INSERT PRIORITY (1C),TILDA                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
*                                                                               
         CLI   DTSTRNGS,1          MULTIPLE DAY/TIME STRINGS?                   
         BE    BLD30140            NO                                           
         OI    COMFLAGS,X'20'      YES - SET 'COMMENT NEEDED' FLAG              
*                                     FOR MULTI D/T STRINGS                     
         MVI   0(R7),X'A1'         INSERT TILDA - NO START TIME                 
         LA    R7,1(R7)                                                         
         MVI   0(R7),X'A1'         INSERT TILDA - NO END   TIME                 
         LA    R7,1(R7)                                                         
         MVI   0(R7),X'A1'         INSERT TILDA - NO DAYS                       
         LA    R7,1(R7)                                                         
         MVI   ORBITCOM,C'Y'       SET ORBIT COMMENTS NEEDED FLAG               
         B     BLD30160                                                         
BLD30140 EQU   *                                                                
         GOTO1 =A(DYTIME),DMCB,(RC),RR=YES                                      
         L     R7,ABUILD           RESET A(NEXT ADDRESS)                        
*                                                                               
BLD30160 EQU   *                                                                
         MVC   WORK2(12),SPACES                                                 
         EDIT  SPOTSWK,(3,WORK2),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 LOADDATA,DMCB,(12,WORK2),(X'A1',0)                               
*                                  INSERT SPOTS/WK (12C),TILDA                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         MVC   WORK2(12),SPACES                                                 
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    BLD30180            YES                                          
         ZICM  RF,RBUYDUR,2        NO  - SECONDS                                
         MVC   WORK2(12),SPACES                                                 
         EDIT  RBUYDUR,(5,WORK2),ALIGN=LEFT,ZERO=NOBLANK                        
*                                  EDIT LENGTH IN SECONDS                       
         GOTO1 LOADDATA,DMCB,(12,WORK2),(X'A1',0)                               
*                                  INSERT LENGTH (12C),TILDA                    
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         B     BLD30190                                                         
BLD30180 EQU   *                                                                
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         M     R0,=F'60'           CALCULATE NUMBER OF SECONDS                  
         MVC   WORK2(12),SPACES                                                 
         EDIT  (R1),(5,WORK2),ALIGN=LEFT,ZERO=NOBLANK                           
*                                  EDIT LENGTH IN SECONDS                       
         GOTO1 LOADDATA,DMCB,(12,WORK2),(X'A1',0)                               
*                                  INSERT LENGTH (12C),TILDA                    
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
BLD30190 EQU   *                                                                
         MVC   WORK2(24),SPACES                                                 
         EDIT  RBUYCOS,(12,WORK2),ALIGN=LEFT,ZERO=NOBLANK                       
*                                  EDIT BUY COST/RATE                           
         GOTO1 LOADDATA,DMCB,(24,WORK2),(X'A1',0)                               
*                                  INSERT COST/RATE (24C), TILDA                
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         OC    RBUYSEC,RBUYSEC     ANY SECTION CODE ENTERED?                    
         BZ    BLD30192            NO  - ADD TILDA                              
         CLC   RBUYSEC,SPACES      ANY SECTION CODE ENTERED?                    
         BE    BLD30192            NO  - ADD TILDA                              
         GOTO1 LOADDATA,DMCB,(03,RBUYSEC),(X'A1',0)                             
*                                  INSERT SECTION CODE (03C), TILDA             
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         B     BLD30194                                                         
BLD30192 EQU   *                                                                
         MVI   0(R7),X'A1'         INSERT TILDA - NO SECTION CODE               
         LA    R7,1(R7)                                                         
BLD30194 EQU   *                                                                
*                                                                               
*    THIS IS WHERE DAYPART ID CODE GOES, WHEN AVAILABLE                         
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
*  COMMENT/INSTRUCTION RECORDS MUST FOLLOW EACH LINE RECORD                     
*                                                                               
**       CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
**       BE    BLD30200            NO                                           
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
BLD30200 EQU   *                                                                
         XC    BLDTABLE,BLDTABLE   COLLAPSE TABLE                               
         XC    FIRSTSW,FIRSTSW                                                  
         B     BLD30060            GO BACK FOR NEXT EFF DATE ELT                
BLD30220 EQU   *                                                                
         B     BLD30020            ACCESS NEXT BUY RECORD                       
*                                                                               
BLD30260 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETRIEVE AUXILIARY DATA, SET MG AND/OR COMMENT FLAG.                        
*        SET MULTI D/T STRING FLAGS IF PATTERN RECORDS NEEDED.                  
*                                                                               
BLD3CTCM NTR1                                                                   
         XC    DTSTRNGS,DTSTRNGS                                                
         XC    COMFLAGS,COMFLAGS                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   BCTC0005            NO ORDER COMMENTS FOUND                      
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BCTC0005 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'84'        LOOK FOR ORD COMMENTS                        
         BAS   RE,GETEL                                                         
         BNE   BCTC0010            NO ORDER COMMENTS FOUND                      
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BCTC0010 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'04'        LOOK FOR CONTRACT COMMENT                    
         BAS   RE,GETEL                                                         
         BNE   BCTC0020            NO CONTRACT COMMENT FOUND                    
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BCTC0020 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'02'        COUNT D/T STRINGS                            
         BAS   RE,GETEL                                                         
         BNE   BCTC0060            NOT FOUND - DONE                             
         B     BCTC0050                                                         
BCTC0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT D/T STRING                          
         BNE   BCTC0060            NOT FOUND - DONE                             
BCTC0050 EQU   *                                                                
         ZIC   RF,DTSTRNGS                                                      
         LA    RF,1(RF)            INCREMENT                                    
         STC   RF,DTSTRNGS                                                      
         CLI   DTSTRNGS,1          MORE THAN ONE DAY/TIME STRING?               
         BNH   BCTC0040            NO  - LOOK FOR ANOTHER                       
BCTC0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE LINE COMMENT/INSTRUCTIONS RECORDS                   
*        BUY COMMENTS (X'04' ELTS) ARE SENT AS 'LINE COMMENTS'                  
*        ORDER COMMENTS (X'84' ELTS) ARE SENT AS 'LINE INSTRUCTIONS'            
*                                                                               
BLD04REC NTR1                                                                   
         MVC   SAVELCOD,ELCODE     SAVE CURRENT ELEMENT CODE                    
         MVC   SAVRCSTR(RCRECLEN),RCSTART                                       
*                                  SAVE THE BUY RECORD                          
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(12),=C'LINE COMMENT'                                     
         MVI   RCSTART+12,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+13       SET A(INSERT FIELD ADDR)                     
         ST    R7,ABUILD                                                        
         TM    COMFLAGS,X'40'      COMMENTS EXIST?                              
         BNO   BLD40340            NO  - DON'T PUT OUT COMMENTS                 
****>>>  BNO   BLD40280            NO  - PUT OUT EMPTY HEADER -                 
*                                     OLD CODE: COMMENT MANDATORY               
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'04'        LOOK FOR BUY COMMENT                         
BLD40160 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLD40200                                                         
BLD40180 EQU   *                                                                
         L     R7,ABUILD                                                        
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLD40200 EQU   *                                                                
         BNE   BLD40260            NOT FOUND:  COMMENT TYPE DONE                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         CLI   ELCODE,X'84'        ORDER COMMENT?                               
         BNE   BLD40210            NO                                           
         BCTR  RF,0                YES - SUBTRACT 1 FOR                         
*                                     STATION/REP COMMENT INDICATOR             
         MVI   WORK2,C' '          CLEAR WORK2 FOR COMMENT                      
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         EX    RF,BLD4023A         MOVE 1ST COMMENT (ORDER)                     
         GOTO1 LOADDATA,DMCB,(100,WORK2),(X'40',0)                              
*                                  INSERT COMMENT (100C),SPACE                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 PRINTREC,DMCB,(RC)                                               
         LA    R7,RCSTART+18       YES - RESET A(INSERT FIELD ADDR)             
         ST    R7,ABUILD                                                        
         MVI   RCSTART+18,C' '     CLEAR COMMENT RECORD                         
         MVC   RCSTART+19(150),RCSTART+18                                       
         B     BLD40180            GO BACK FOR NEXT ELEMENT                     
BLD40210 EQU   *                                                                
         MVI   WORK2,C' '          CLEAR WORK2 FOR COMMENT                      
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         EX    RF,BLD4023C         MOVE 1ST COMMENT (ORDER)                     
         GOTO1 LOADDATA,DMCB,(100,WORK2),(X'40',0)                              
*                                  INSERT COMMENT (100C),SPACE                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 PRINTREC,DMCB,(RC)                                               
         LA    R7,RCSTART+13       YES - RESET A(INSERT FIELD ADDR)             
         ST    R7,ABUILD                                                        
         MVI   RCSTART+13,C' '     CLEAR COMMENT RECORD                         
         MVC   RCSTART+14(150),RCSTART+13                                       
         B     BLD40180                                                         
*                                                                               
BLD4023A MVC   WORK2(0),3(R6)      ORDER COMMNTS START 3 CHARS OVER             
BLD4023C MVC   WORK2(0),2(R6)      BUY   COMMNTS START 2 CHARS OVER             
*                                                                               
BLD40260 EQU   *                                                                
         CLI   ELCODE,X'84'        ORDER COMMENTS DONE?                         
         BE    BLD40340            YES - COMMENTS FINISHED                      
         LA    R6,RBUYREC          NO  - RESET A(BUY REC)                       
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(17),=C'LINE INSTRUCTIONS'                                
         MVI   RCSTART+17,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+18       SET A(INSERT FIELD ADDR)                     
         ST    R7,ABUILD                                                        
         BAS   RE,PROGCOMT         CHECK FOR PROGRAM NAME COMMENT               
         MVI   ELCODE,X'84'        NO  - GO BACK AND DO THEM                    
         B     BLD40160                                                         
BLD40280 EQU   *                                                                
*                                                                               
*    THIS LABEL IS UNREFERENCED.  'EMPTY' COMMENT RECORDS ARE NO                
*        LONGER GENERATED. B.UHR PER L.PUTNAM   6/22/95                         
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                  PUT OUT 'EMPTY' COMMENT RECORD               
BLD40340 EQU   *                                                                
         CLI   ROTATCOM,C'Y'       ROTATOR INSTRUCTION NEEDED?                  
         BNE   BLD40400            NO                                           
***>>>   BAS   RE,BLD05ROT         YES - PUT OUT ROTATOR INSTRUCTION            
*    NO WARNING FOR OUT OF WEEK ROTATOR IS BEING ISSUED.                        
*                                                                               
BLD40400 EQU   *                                                                
         CLI   ALTWKCOM,C'Y'       ALT WEEK INSTRUCTION NEEDED?                 
         BNE   BLD40440            NO                                           
         BAS   RE,BLD06ALT         YES - PUT OUT ALT WK INSTRUCTION             
BLD40440 EQU   *                                                                
         CLI   ORBITCOM,C'Y'       ORBIT INSTRUCTION NEEDED?                    
         BNE   BLD40480            NO                                           
         BAS   RE,BLD07ORB         YES - PUT OUT ORBIT INSTRUCTIONS             
BLD40480 EQU   *                                                                
         MVI   RCSTART,C' '        CLEAR RECORD                                 
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(08),=C'LINE END'                                         
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                  PUT OUT LINE END RECORD                      
         MVC   RCSTART(RCRECLEN),SAVRCSTR                                       
*                                  RESTORE THE BUY RECORD                       
****>>>  XC    COMFLAGS,COMFLAGS   TURN OFF COMMENTS FLAG                       
*                                     DON'T RESET HERE!!                        
         MVC   ELCODE,SAVELCOD     RESTORE CURRENT ELEMENT CODE                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   OUTPUT PROGRAM NAME COMMENT, IF ANY                                         
*                                                                               
PROGCOMT NTR1                                                                   
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROG NAME ELT                       
         BAS   RE,GETEL            GET PROG NAME ELEMENT                        
         BNE   PCOM0020            NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
*                                                                               
         MVI   WORK2,C' '          CLEAR WORK2 FOR COMMENT                      
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         EX    RF,BLD4023C         MOVE 1ST COMMENT (ORDER)                     
         GOTO1 LOADDATA,DMCB,(100,WORK2),(X'40',0)                              
*                                  INSERT COMMENT (100C),SPACE                  
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         GOTO1 PRINTREC,DMCB,(RC)                                               
         LA    R7,RCSTART+18       RESET A(INSERT FIELD ADDR)                   
         ST    R7,ABUILD                                                        
         MVI   RCSTART+18,C' '     CLEAR COMMENT RECORD                         
         MVC   RCSTART+19(145),RCSTART+18                                       
PCOM0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE OUT OF WEEK ROTATOR COMMENT                         
*                                                                               
*                                                                               
BLD05ROT NTR1                                                                   
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(17),=C'LINE INSTRUCTIONS'                                
         MVI   RCSTART+17,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+18       SET A(INSERT FIELD ADDR)                     
         MVC   0(29,R7),=C'OUT-OF-WEEK ROTATOR:  WARNING'                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE ALTERNATING WEEK COMMENT                            
*                                                                               
*                                                                               
BLD06ALT NTR1                                                                   
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(17),=C'LINE INSTRUCTIONS'                                
         MVI   RCSTART+17,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+18       SET A(INSERT FIELD ADDR)                     
         MVC   0(29,R7),=C'ALTERNATING WEEK BUY: WARNING'                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE ORBIT COMMENT AND ORBIT DETAILS COMMENTS            
*                                                                               
*                                                                               
BLD07ORB NTR1                                                                   
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(17),=C'LINE INSTRUCTIONS'                                
         MVI   RCSTART+17,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+18       SET A(INSERT FIELD ADDR)                     
         MVC   0(31,R7),=C'BUY IS AN ORBIT: DETAILS FOLLOW'                     
         GOTO1 PRINTREC,DMCB,(RC)                                               
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'02'        COUNT D/T STRINGS                            
         BAS   RE,GETEL                                                         
         BNE   BORB0160            NOT FOUND - DONE                             
         B     BORB0050                                                         
BORB0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT D/T STRING                          
         BNE   BORB0160            NOT FOUND - DONE                             
BORB0050 EQU   *                                                                
         MVI   RCSTART,C' '        SPACE FILL RECORD                            
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(17),=C'LINE INSTRUCTIONS'                                
         MVI   RCSTART+17,X'A1'    INSERT TILDA                                 
         LA    R7,RCSTART+18       SET A(INSERT FIELD ADDR)                     
         MVC   0(13,R7),=C'ORBIT DETAIL:'                                       
         LA    R7,14(R7)                                                        
         BAS   RE,DYTIME2                                                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
         B     BORB0040            GO BACK FOR NEXT                             
BORB0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE 'END' TRANSACTION RECORD                            
*                                                                               
*                                                                               
BLD10REC NTR1                                                                   
         MVI   RCSTART,C' '        CLEAR RECORD                                 
         MVC   RCSTART+1(RCRECLEN-1),RCSTART                                    
         MVC   RCSTART(3),=C'END'                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                  PUT OUT 'END' RECORD                         
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*        NOTE!  BE CAREFUL WHEN USING LOCALWRK ENTRIES.  R3 REFERS  *           
*            TO LOCALWRK, AND IS CHANGED IN THIS ROUTINE.  THEREFORE*           
*            ENTRIES INTENDED FOR LOCALWRK CAN BE MISPLACED.                    
*                                                                               
*********************************************************************           
INIT     NTR1                                                                   
*                                                                               
         GOTO1 =A(RESOLVE),DMCB,(RC),RR=YES                                     
*                                                                               
         LA    RE,ADCONS           RELOCATE A-TYPES                             
         LA    RF,OUTDAY                                                        
         LA    R0,NADCONS                                                       
INIT10   EQU   *                                                                
         L     R1,0(RE)                                                         
         A     R1,RELX                                                          
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVC   AREGENSC,VREGENSC   SAVE A(STANDARD COMMENT RTN)                 
         XC    TRANSCNT,TRANSCNT   CLEAR ACCUMULATORS                           
         XC    TOTALAMT,TOTALAMT                                                
         XC    TLSPOTS,TLSPOTS                                                  
         XC    TRANSCNT,TRANSCNT                                                
         XC    EXTRAKEY,EXTRAKEY                                                
*                                                                               
         MVI   NOPQCLOS,0          ASSUME WE HAVE OUTPUT                        
         MVI   FOXZEROS,X'F0'                                                   
         MVC   FOXZEROS+1(L'FOXZEROS-1),FOXZEROS                                
         XC    LATREND,LATREND     SET EARLIEST END DATE                        
         MVC   ERLYSTRT(3),=X'FFFFFF'                                           
*                                  SET LATEST START DATE                        
         MVI   INTTYPE,C'E'        SET INPUT TYPE TO 'EC'                       
         LA    RF,SPOOLEND-SPOOLD  CLEAR THE SPOOL AREA                         
         LA    RE,SPOOLD                                                        
         XCEF                                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         MVC   SPOOLDM,DATAMGR     INITIALIZE SPOOLER DATAMGR                   
         L     RF,AFACILS          A(FACILITIES LIST)                           
         LM    R2,R4,8(RF)         A(TERMINAL INPUT AREA??)                     
         ST    R3,ATIA                                                          
         MVC   SPOOLBUF,ATIA                                                    
         MVC   SCANNER(16),24(R4)                                               
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON     SET A(DATCON)                                
         MVC   RCCOMFAC,ACOMFACS                                                
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   DMFILE,C'R'                                                      
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        GETCON --- READ CONTRACT RECORD                                        
*********************************************************************           
GETCON   NTR1                                                                   
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
*   RETRIEVE SUPPORT INFORMATION FROM PRODUCT RECORD                            
*                                                                               
*                                                                               
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BNE   GCON0060            YES - GET RECORD                             
         LA    R6,RCONREC          NO  - RETRIEVE X'05' ELEMENT                 
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    GCON0050                                                         
         DC    H'0'                NO PRODUCT CODE NAME ELEMENT                 
GCON0050 EQU   *                                                                
         MVC   PRODNAME,2(R6)      LOAD PRODUCT NAME                            
*                                     FIXED LEN ELEMENT OF 20 CHARS             
         B     GCON0080            EXIT                                         
GCON0060 EQU   *                                                                
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,X'09'       OFFICE RECORD                                
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER                            
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RPRDKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0070                                                         
         DC    H'0',C'MISSING PRODUCT RECORD'                                   
         DS    0H                                                               
GCON0070 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RPRDREC                                             
         MVC   PRODNAME,RPRDNAME   SAVE PRODUCT NAME                            
*                                                                               
GCON0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
PRINTREC NTR1                                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         LA    R2,2                LOOP CONTROL FOR PRINTING                    
         LA    R4,RCSTART          A(OUTPUT RECORD)                             
PRC20040 EQU   *                                                                
         MVC   P(250),0(R4)                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
BLD3PTRN NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE DAY AND TIME TO JDS FORMAT FOR ORBIT COMMENT RECORDS              
*        R6 --> DAY/TIME ELEMENT TO BE CONVERTED                                
*                                                                               
DYTIME2  NTR1                                                                   
         USING RBUYDYEL,R6                                                      
*                                                                               
         MVI   ROTATCOM,C'N'       SET 'ROTATOR COMMENT' = NO                   
         ZIC   RF,RBUYDYIN         IS START DAY MONDAY?                         
         SRL   RF,4                DROP END DAY FROM STRING                     
         CH    RF,=H'1'            MONDAY?                                      
         BE    DYT20020            YES                                          
         MVI   ROTATCOM,C'Y'       NO  - SET 'NEED ROTATOR COMMENT'             
DYT20020 EQU   *                                                                
         EDIT  RBUYDYT1,(4,WORK)                                                
         OC    WORK(4),FOXZEROS    INSERT ZEROS FOR SPACES                      
         MVC   WORK2(2),WORK       FORMAT TIME STRING                           
         MVI   WORK2+2,C':'        INSERT SEPARATOR                             
         MVC   WORK2+3(2),WORK+2                                                
         MVI   WORK2+5,C':'        INSERT SEPARATOR                             
         MVC   WORK2+6(2),FOXZEROS                                              
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'40',0)                                
*                                  INSERT START TIME (8C),SPACE                 
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         OC    RBUYDYT2,RBUYDYT2   ANY END TIME?                                
         BNZ   DYT20040            YES                                          
*                                  NO  - LOAD START AS END TIME                 
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'40',0)                                
*                                  INSERT END TIME (8C),SPACE                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         B     DYT20060                                                         
DYT20040 EQU   *                                                                
         EDIT  RBUYDYT2,(4,WORK)                                                
         OC    WORK(4),FOXZEROS    INSERT ZEROS FOR SPACES                      
         MVC   WORK2(2),WORK       FORMAT TIME STRING                           
         MVI   WORK2+2,C':'        INSERT SEPARATOR                             
         MVC   WORK2+3(2),WORK+2                                                
         MVI   WORK2+5,C':'        INSERT SEPARATOR                             
         MVC   WORK2+6(2),FOXZEROS                                              
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'40',0)                                
*                                  INSERT END TIME (8C),SPACE                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
DYT20060 EQU   *                                                                
         MVC   WORK2(80),SPACES                                                 
         LA    RF,RBUYDAYS         A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),RBUYDYIN    INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =A(DAYUNPK2),DMCB,,(0,XDATE),RR=YES                              
*                                                                               
         GOTO1 =A(EXPLDAYS),DMCB,(RC),RR=Y                                      
*                                  EXPLODE DAYS TO VCI FORMAT                   
         GOTO1 LOADDATA,DMCB,(80,WORK2),(X'40',0)                               
*                                  INSERT PRIORITY (1C),SPACE                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
DYT20200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*    GET BUY DATE INFORMATION                                                   
*                                                                               
DTEINFO  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            RESET A(EFF DATE ELEMENT)                    
         USING RBUYDTEL,R6                                                      
         MVC   STARTDTE,RBUYDTST   SAVE START DATE                              
         MVC   SPOTSWK,RBUYDTNW    SET NUMBER SPOTS/WEEK                        
         XC    COLLAPSE,COLLAPSE   CLEAR COLLAPSE FLAG                          
         GOTO1 =A(CHKDATE),DMCB,(RC),(R6),RR=Y                                  
         MVI   NOFWKS,0            CLEAR NUMBER OF WEEKS                        
*                                                                               
*   SHIFT 'USING' FROM EFFECTIVE DATE ELEMENT TO BLDTABLE AREA,                 
*      WHERE A 'REVISED/COLLAPSED' EFFECTIVE DATE ELEMENT MAY HAVE              
*      BEEN BUILT.  IF IT HASN'T, IT STILL LOOKS LIKE THE ONE PASSED            
*      IN AT THE ORIGINAL CALL.                                                 
*                                                                               
         LA    R6,BLDTABLE                                                      
*                                                                               
         TM    BLDTABLE+1,X'40'    ALTERNATE WEEK?                              
         BNO   DINF0020            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
DINF0020 EQU   *                                                                
         MVC   ENDDTE,RBUYDTED     FORCE END DATE IN                            
         OC    RBUYDTED,RBUYDTED   ANY END DATE?                                
         BNZ   DINF0070            YES - END DATE EXISTS                        
*                                     USE FORCED DATE                           
         MVC   HOLDDATE(3),CKSTRDTE                                             
*                                  TEMPORARY HOLD AREA                          
         ZIC   R0,RBUYSTED         GET START/END DAY OF WEEK                    
*                                  START DAY  STAYS IN R0                       
         SRDL  R0,4                SHIFT END DATE INTO R1                       
         SRL   R1,28               SHIFT END DATE TO LOW-ORDER                  
         SR    R1,R0               SUBTRACT START FROM END                      
         BZ    DINF0060            SAME DAY:  NO ADJUSTMENT                     
         BP    DINF0040                                                         
         LA    R1,7(R1)            NEGATIVE:  MAKE POSITIVE                     
DINF0040 EQU   *                                                                
         ST    R1,DAYBUMP          SET DAYS TO ADD                              
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         L     RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,FRSTDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
DINF0060 EQU   *                                                                
         MVC   ENDDTE,FRSTDATE     SAVE NEW END DATE                            
DINF0070 EQU   *                                                                
         XC    DAYBUMP,DAYBUMP                                                  
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
DINF0080 EQU   *                                                                
         L     RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,FRSTDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   ENDDTE(3),FRSTDATE                                               
*                                  END DATE REACHED?                            
         BL    DINF0120            YES                                          
         ZIC   RF,RBUYDTNW         ACCUM # SPOTS/WK                             
         ZIC   RF,NOFWKS           BUMP NUMBER OF WEEKS                         
         LA    RF,1(RF)                                                         
         STC   RF,NOFWKS                                                        
*                                                                               
*    I DON'T KNOW IF ABOVE ACCUMULATIONS ARE NECESSARY IN THIS                  
*        FASHION.  DURING TESTING, CHECK RESULTS OBTAINED.                      
*                                                                               
         LA    RF,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK?                            
         BNO   DINF0100            NO                                           
         LA    RF,14               YES                                          
DINF0100 EQU   *                                                                
         ST    RF,DAYBUMP                                                       
*                                                                               
*   USE PREVIOUS 1ST DATE TO CALCULATE NEW FIRST DATE                           
         MVC   SECDATE(3),FRSTDATE                                              
         GOTO1 DATCON,DMCB,(3,SECDATE),(0,FRSTDATE)                             
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         B     DINF0080            GO BACK FOR NEXT                             
DINF0120 EQU   *                                                                
         MVC   WORK2(16),FOXZEROS                                               
         XC    HOLDDATE,HOLDDATE                                                
         CLC   ERLYSTRT,STARTDTE   EARLIER START DATE?                          
         BL    DINF0140            NO                                           
         MVC   ERLYSTRT,STARTDTE   YES - SAVE IT                                
DINF0140 EQU   *                                                                
         CLC   ENDDTE,LATREND      LATER   END   DATE?                          
         BL    DINF0160            NO                                           
         MVC   LATREND,ENDDTE      YES - SAVE IT                                
DINF0160 EQU   *                                                                
*                                                                               
*    INSERT START DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,STARTDTE),(10,WORK2)                              
         L     R7,ABUILD           RESET A(INSERT POINT)                        
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT LINE START DATE (8C),TILDA            
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(10,WORK2)                                
         GOTO1 LOADDATA,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT LINE END   DATE (8C),TILDA            
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        ADDRESS                                                                
*                                                                               
RELX     DS    F                                                                
*YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                        
*BYEAR    DS    X                   BINARY YEAR                                 
*STATFLAG DS    X                                                               
*HASCANLN EQU   X'80'               ORDER HAS CANCELLED BUYLINES                
         SPACE 1                                                                
ADCONS   DS    0A                  A/V TYPES                                    
         DC    V(OUTDAY)                                                        
         DC    V(REGENPBY)                                                      
         DC    V(REGENBUC)                                                      
         DC    V(REGENTL2)                                                      
         DC    V(UNTIME)                                                        
NADCONS  EQU   (*-ADCONS)/4                                                     
         SPACE 2                                                                
DASH     DC    51C'-'                                                           
         SPACE 2                                                                
*RMSCMODE DC    X'00'               REGENSC MODES                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
RCMTD    DSECT                                                                  
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
* INCLUDE CTGENFILE                                                             
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
LOCALWRK DSECT                                                                  
       ++INCLUDE RECNTVCI                                                       
*                                                                               
*      LOCAL VARIABLES                                                          
*                                                                               
FOXZEROS DS    CL24                STRING OF X'F0'                              
EXTRAKEY DS    CL34                BUY KEY SAVE AREA                            
COLLAPSE DS    XL1                                                              
COLLAPS2 DS    XL1                                                              
MULTEFDT DS    XL1                                                              
SVBYLN#  DS    XL1                                                              
ZEROSPTS DS    XL1                                                              
TRANSCNT DS    XL1                                                              
COMFLAGS DS    XL1                 COMMENT FLAGS                                
*                                  X'80'  =  MG SWITCH                          
*                                  X'40'  =  BUY COMMENTS EXIST                 
*                                  X'20'  =  NEED MULT D/T COMMENT              
COMMFLAG DS    XL1                 COMMENTS PRESENT FLAG                        
ROTATCOM DS    CL1                 OUT OF WEEK ROTATOR COMMENT NEEDED           
ORBITCOM DS    CL1                 ORBIT COMMENT NEEDED.                        
ALTWKCOM DS    CL1                 ALTERNATING WEEKS COMMENT NEEDED.            
FIRSTSW  DS    XL1                                                              
TRANSCT  DS    XL1                 TRANSACTION COUNTER                          
ABUILD   DS    A                   A(INSERT POINT)                              
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
TESTCTR  DS    F                                                                
BLDTABLE DS    XL24                (???? CHECK LENGTH)                          
SVSL#    DS    XL1                 SAVE SUBLINE #                               
SAVRISK  DS    XL1                 CREDIT RISK OF AGENCY                        
SAVLIAB  DS    XL1                 LIABILITY OF AGENCY                          
CNVDATE  DS    CL6                                                              
HOLDDATE DS    CL6                                                              
DATEWORK DS    CL12                DATE WORK AREA                               
         ORG   DATEWORK                                                         
FRSTDATE DS    CL6                                                              
SECDATE  DS    CL6                                                              
THIRDATE DS    CL3                                                              
CMTCTR   DS    XL1                 COMMENT COUNTER                              
NOFWKS   DS    XL1                 NUMBER OF WEEKS CTR                          
ALTWKS   DS    XL1                 ALTERNATING WEEKS                            
STARTDTE DS    XL3                 START DATE                                   
ENDDTE   DS    XL3                 END   DATE                                   
ERLYSTRT DS    XL3                 EARLIEST START DATE                          
LATREND  DS    XL3                 EARLIEST END DATE                            
SVCNTWK  DS    XL1                 SAVE NUMBER OF WEEKS                         
SPOTSWK  DS    XL1                 SPOTS PER WEEK                               
CKSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSPTSWK DS    XL1                 FOR CHECKDATE ROUTINE                        
TLSPOTS  DS    F                   TOTAL SPOTS                                  
DAYBUMP  DS    F                                                                
DAYCNT   DS    F                                                                
SPOTLEFT DS    F                                                                
SPOTMAX  DS    F                                                                
SPOTMAX2 DS    F                                                                
AEFFDATE DS    A                                                                
*AIO4     DS    A                   A(CONTRACT COMMENT)                         
AREGENSC DS    A                   A(STANDARD CONTRACT ROUTINE                  
*********************************************************************           
*    FIELDS USED TO BUILD DAY/TIME STRINGS + COMMENTS OF GROUPS                 
*        OF D/T STRINGS FOR THE 0105 RECORDS                                    
*                                                                               
DTLENGTH DS    XL1                 LENGTH OF DAY/TIME STRING                    
DTBUILD  DS    16B                 CONSTRUCT DAT/TIME STRING                    
DTCOMNT  DS    CL60                BUILD 0105 COMMENT DAY/TIMES                 
DTCOMEND EQU   *                                                                
DTCOMLEN EQU   *-DTCOMNT                                                        
DTSTRNGS DS    XL1                 NUMBER OF DAY/TIME STRINGS                   
*********************************************************************           
*    FIELDS USED TO HOLD DAYS AND TIMES BEFORE TRANSLATING TO                   
*        VALID BIAS FORMAT                                                      
XDATE    DS    CL12                TRANSLATE DATE                               
XTIME    DS    CL4                 TRANSLATE TIME                               
*********************************************************************           
         DS    0H                                                               
SUPPRECS DS    0CL100              SUPPORT RECORD INFORMATION                   
AGYNAME  DS    CL20                                                             
ADVNAME  DS    CL20                                                             
SALNAME  DS    CL20                                                             
OFFNAME  DS    CL20                                                             
PRODNAME DS    CL20                                                             
         DS    0F                                                               
BLOCK    DS    480C                DEMO WORK AREA                               
*                                                                               
NEW15ELT DS    CL20                AREA FOR DATE/TIME ELT                       
SAVRCSTR DS    250C                SAVE AREA FOR BUY RECORD                     
SAVELCOD DS    CL1                 SAVE AREA FOR CURRENT ELEMENT                
HEXSTTIM DS    CL6                 START TIME FOR BUY LINE                      
HEXENTIM DS    CL6                 END   TIME FOR BUY LINE                      
LOCALEND EQU   *                                                                
         EJECT                                                                  
*                                                                               
*********************************************************************           
*- GETSTA -- READ STATION RECORD                                    *           
*********************************************************************           
         CSECT                                                                  
GETSTA   NMOD1 0,*GSTA*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            SET A(STATION IN CONTRACT REC)               
GSTA05   DS    0H                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R4)      INSERT STATION                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSTA10                                                           
         DC    H'0',C'MISSING STA REC'                                          
GSTA10   GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                                                               
*- PICK UP RECORD DATA                                                          
*                                                                               
*    NEW DATA TO DEFINE THE OUTPUT FOR ELECTRONIC CONTRACTING MAY               
*        HAVE TO BE IDENTIFIED.  THIS WILL PERMIT THE SPOOLING OF               
*        THE EC REPORT IN THE RIGHT DIRECTION.                                  
*                                                                               
GSTA20   DS    0H                                                               
*                                                                               
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   GSTA55              NOT THERE                                    
*                                                                               
         USING RSTAXXEL,R6                                                      
*                                                                               
GSTA55   EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
*                                  IF RECORD NOT FOUND, JOB BLOWS UP            
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*- FINDID -- FIGURE OUT WHERE THE CONTRACT IS SUPPOSED TO GO FROM               
*            SOMETHING IN THIS MESS......                                       
*                                                                               
*  INPUT       RSTAREC                                                          
*                                                                               
*  RETURN                                                                       
*        BYTE  PARAMETER INTO 'PQOPEN'                                          
*        CC    ZERO  = GOOD EXIT.  SEND ID AND FORMAT FILLED IN                 
*        CC    NON-0 = COPY NOT REQUIRED.  EXIT ASAP.                           
*                                                                               
*********************************************************************           
*- SENDER = STATION - STATION WILL INITIATE THE EC/E2 COMMAND.                  
*                                                                               
*  ID = TERMINAL                                                                
*                                                                               
*********************************************************************           
FINDID   NMOD1 0,*FIND*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   SENDID,TWAUSRID                                                  
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
*                                  ALWAYS RETURN A ZERO CC                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   PQOPENA:                                                                    
*                                                                               
* 'BYTE' CONTAINS A PARAMETER TO THIS ROUTINE.  IF BYTE IS A 'W',               
* THEN CLASS 'G' REPORTS WILL BE CREATED WITH STATUS 'KEEP'.  THIS              
* WILL CAUSE THEM TO BE PICKED UP BY WESTERN UNION, NOT GRAPHNET.               
*                                                                               
PQOPENA  NMOD1 0,*PQOP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,X'40'   ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         MVC   SPOOLKEY+12(3),=C'ECR'                                           
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   1(8,R3),CONCNUM                                                  
         SPACE 2                                                                
VPQ10    CLI   1(R3),C'0'                                                       
         BNE   VPQ20                                                            
         MVC   1(7,R3),2(R3)                                                    
         B     VPQ10                                                            
         SPACE 2                                                                
VPQ20    DS    0H                                                               
         GOTO1 SQUASHER,DMCB,SPOOLKEY+1,11                                      
         MVI   SPOOLKEY+16,68      68 LINES TO A PAGE                           
         MVI   SPMODE,0                                                         
         SPACE 1                                                                
         CLI   INTTYPE,C'E'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
*   CLASS                                                                       
*                                                                               
***      MVI   PLCLASS,C' '        CLASS 'SPACE/BLANK'                          
         MVI   PLCLASS,C'G'        CLASS 'G' - BDE                              
         SPACE 1                                                                
         OC    SENDID,SENDID       IF SEND ID, STORE IN PLUSER                  
         BZ    VPQ30                                                            
         MVC   PLUSER(2),SENDID                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
VPQ30    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         LA    RE,TWASPKEY                                                      
         DROP  RF                                                               
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'      ORDER WORKSHEETS                             
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'EC'                                                     
         SPACE 1                                                                
         CLC   SENDID(2),=X'0406'  FOR GRAPHNET COPY, USE                       
         BNE   VPQ50                                                            
         MVC   QLRETND,=H'26'      PRTD/SENT RETENTION OF 26, NOT 2             
         DROP  RE                                                               
VPQ50    GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         XIT1                                                                   
         SPACE 3                                                                
OKMESS   DC    C'XXX,12345 HAS BEEN SPOOLED. PAGES=NNN,LINES=NNNN'              
CONXMSG  DC    C'**** CONTRACT CONFIRMED ****                    '              
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE DMPRTQL                                                        
*          DATA SET RECNT64    AT LEVEL 072 AS OF 08/24/93                      
*   RESOLVE:  FILL IN THE ADDRESSES OF CALLED ROUTINES                          
* DERIVED FROM THE RECNT80 MODULE                                               
*                                                                               
         CSECT                                                                  
RESOLVE  NMOD1 0,*RESO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,CORES                                                         
         LA    R4,CLIST                                                         
         SPACE 1                                                                
RESO0020 XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R4)     OVERLAY NUMBER                               
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   RESO0020            GO BACK FOR NEXT                             
         SPACE 1                                                                
         MVC   BOOKVAL(56),CORES   GET CORE RESIDENT PHASE                      
         MVC   UPVAL,CORES+56                                                   
         SPACE 1                                                                
         XIT1                                                                   
CLIST    DC    X'00'               BOOKVAL                                      
         DC    X'01'               CENTER                                       
         DC    X'02'               CHOPPER                                      
         DC    X'03'               DAYVAL                                       
         DC    X'E0'          ***DEMOCON***  (WAS DEMCON)                       
         DC    X'05'               DEMEX                                        
         DC    X'06'               DEMOTAB                                      
         DC    X'07'               DEMVAL                                       
         DC    X'08'               DEMUP                                        
*         DC    X'09'                                                           
*         DC    X'0A'                                                           
*         DC    X'0B'                                                           
         DC    X'0C'               SPOOL                                        
         DC    X'0D'               SQUASHER                                     
         DC    X'0E'               TIMVAL                                       
         DC    X'0F'               UNDAY                                        
         DC    X'10'               UNDERLIN                                     
*         DC    X'11'               UNTIME                                      
*         DC    X'12'               XSORT                                       
         DC    X'13'               UPVAL                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
CORES    DS    0F                                                               
VBOOKVAL DS    V                                                                
VCENTER  DS    V                                                                
VCHOPPER DS    V                                                                
VDAYVAL  DS    V                                                                
VDEMCON  DS    V                                                                
VDEMEX   DS    V                                                                
VDEMOTAB DS    V                                                                
VDEMVAL  DS    V                                                                
VDEMUP   DS    V                                                                
*VINVEDIT DS    V                  NOT USED - FROM AVAILS...                    
*VPAVCOND DS    V                                                               
*VPAVEXPL DS    V                                                               
VSPOOL   DS    V                                                                
VSQUASH  DS    V                                                                
VTIMVAL  DS    V                                                                
VUNDAY   DS    V                                                                
VUNDERLN DS    V                                                                
VUNTIME  DS    V                                                                
VXSORT   DS    V                                                                
VUPVAL   DS    V                                                                
*                                                                               
*   FILL IN CONTRACT '15' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
         CSECT                                                                  
DATETIME NMOD1 0,**DTTM**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   KEY,RCONREC         REESTABLISH CONTRACT RECORD                  
*                                     DUE TO INTERVENING BUY RECORDS            
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         XC    NEW15ELT,NEW15ELT   CLEAR DATE/TIME ELT AREA                     
         MVC   NEW15ELT(2),=X'1514'                                             
*                                  SET ELT TYPE (X'15'), LEN=20                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'15'        LOOK FOR EC CONTROL ELT                      
         BAS   RE,GETEL                                                         
         BNE   DTIM0020            NOT FOUND:  USE NEW ELT                      
         MVC   NEW15ELT,0(R6)      FOUND:  INSERT INTO ELT AREA                 
DTIM0020 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'15',RCONREC)                                    
*                                  DELETE ANY OLD X'15' ELEMENT(S)              
         LA    R6,NEW15ELT                                                      
         USING RCONECEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,DUB),(2,RCONECDT)                                 
*                                  INSERT DATE INTO ELT                         
         ZICM  RF,RCONECCT,2       BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         STCM  RF,3,RCONECCT       PUT COUNTER BACK                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RCONECTM                                                    
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,NEW15ELT                                   
*                                  ADD UPDATED X'15' ELEMENT                    
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                  REWRITE UPDATED CONTRACT RECORD              
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ROUTINE COLLAPSES EFFECTIVE DATE ELEMENTS, IF AT ALL POSSIBLE,              
*        TO ATTEMPT TO LIMIT AMOUNT OF LINES BEING TRANSMITTED.                 
*        LINES ARE CHECKED FOR CONTINUITY, EITHER SINGLE WEEK OR                
*        ALTERNATING WEEK.                                                      
*   REASON BEHIND THIS:  A USER WILL (FROM TIME TO TIME) ENTER A                
*        BUYLINE OF 10 WEEKS AS 10 UNIQUE EFFECTIVE DATES.  THIS                
*        WOULD GENERATE 1 LINE WITH 10 SUBLINES, AND COULD FLOOD                
*        THE BIAS SYSTEM WITH MORE LINES/SUBLINES THAN IT COULD                 
*        HANDLE.                                                                
*                                                                               
CHKDATE  NMOD1 0,**CHDT**                                                       
         L     RC,0(R1)            RESET A (WORKSPACE)                          
         L     R6,4(R1)            A(1ST EFFECTIVE DATE ELT)                    
         USING RBUYDTEL,R6                                                      
         MVC   CKSTRDTE,STARTDTE   SAVE 1ST EFFECTIVE START DATE                
CHKD0020 EQU *                                                                  
         MVI   ELCODE,X'03'        SET ELEMENT CODE                             
         L     R6,AEFFDATE         A(EFF DATE ELEMENT IN PROGRESS)              
         BAS   RE,NEXTEL           GET NEXT 03 ELEMENT                          
         BNE   CHKD0120            NO MORE - EXIT                               
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         MVC   SVSTRDTE,RBUYDTST   SAVE EFFECT START DATE                       
         MVC   SVSPTSWK,RBUYDTNW   SAVE SPOTS PER WEEK                          
         CLI   COLLAPS2,1          STOP COLLAPSE?                               
         BE    CHKD0160            YES                                          
         CLC   SVSPTSWK,SPOTSWK    HAS THIS WEEK BEEN MADE GOOD?                
         BNE   CHKD0160            YES - CAN'T COLLAPSE WEEKS                   
*                                                                               
*   ABOVE TEST IS STRANGE:  ARE SPOTS/WEEK CHANGED IF LINE IS MADE              
*        GOOD?  DOES IT GET TO THE EFFECTIVE DATE ELEMENT, OR TOTAL             
*        SPOTS FOR LINE?  THIS MAY NOT BE A VALID TEST.....                     
*                                                                               
         CLC   NOFWKS,RBUYDTWK     NUMBER OF WEEKS SAME:                        
*                                     1ST EFF DATE VS THIS ONE                  
         BNE   CHKD0160            NO  - CAN'T COLLAPSE                         
*********************************************************************           
*     FOLLOWING TEST INVALID:  REMOVED                              *           
****>>   CLI   FIRSTSW,0           ALTERNATE WEEK PATTERN?                      
****>>   BNZ   CHKD0040            YES                                          
*                                                                   *           
*********************************************************************           
*                                                                               
* BUMP PREVIOUS EFF DATE ELEMENT START DATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,CKSTRDTE),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         LA    RF,7                LOOK FOR SEQUENTIAL                          
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE(3),SVSTRDTE                                             
*                                  SEQUENTIAL BUY WEEK?                         
         BE    CHKD0060            YES - SET SWITCH AND EXIT                    
         B     CHKD0160            NO  - NOT ALT WEEK PATTERN.                  
*                                     DON'T COLLAPSE ANY FURTHER                
CHKD0040 EQU   *                                                                
         LA    RF,14               LOOK FOR ALTERNATING                         
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE(3),SVSTRDTE                                             
*                                  ALTERNATING BUY WEEK?                        
         BNE   CHKD0180            NO  - NOT SEQ OR ALTERNATING                 
*                                                                               
CHKD0060 EQU   *                                                                
         MVI   COLLAPSE,1          INDICATE COLLAPSE BUY                        
         MVC   CKSTRDTE,THIRDATE   SET TO NEW DATE                              
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,CHKD0080         MOVE TO BLDTABLE                             
         B     CHKD0100                                                         
CHKD0080 MVC   BLDTABLE(0),0(R6)                                                
CHKD0100 EQU   *                                                                
         SR    RE,RE               CALCULATE NUMBER OF SPOTS                    
         ZIC   RF,NOFWKS           NUMBER OF WEEKS *                            
         ZIC   R1,SPOTSWK             SPOTS PER WEEK=                           
         MR    RE,R1                     SPOTS FOR EFF DATE ELT                 
         L     RE,TLSPOTS          ADD IT TO ACCUMULATOR                        
         AR    RE,RF                                                            
         ST    RE,TLSPOTS                                                       
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         ZIC   RF,SVSPTSWK                                                      
         ZIC   R1,SPOTSWK                                                       
         AR    RF,R1                                                            
         STC   RF,SVSPTSWK         TOTAL SPOTS                                  
         B     CHKD0020        GO BACK FOR NEXT ELEMENT                         
CHKD0120 EQU   *                                                                
         MVI   COLLAPSE,X'FF'      SET COLLAPSE TO 'NO MORE'                    
CHKD0160 EQU   *                                                                
         CLI   COLLAPSE,1          PASS PRODUCED COLLAPSE?                      
         BE    CHKD0180            YES                                          
         MVI   COLLAPS2,1          NO  - DON'T COLLAPSE BUY FURTHER             
CHKD0180 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EXPLDAYS NMOD1 0,**EXPL**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,XDATE            A(DDS FORMAT DAY STRING)                     
         BAS   RE,XDATECLN         CLEAN UP XDATE FIELD                         
         LA    R2,DAYTAB           A(CONVERSION FORMAT TABLE)                   
         LA    R4,WORK2            A(FINAL RECEIVING FIELD)                     
EXPL0040 EQU   *                                                                
         ZIC   R5,0(R2)            LENGTH OF COMPARE                            
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,EXPL0900         COMPARE CODE BY LENGTH                       
         BE    EXPL0080            DAY CODE FOUND                               
         AR    R2,R5                                                            
         LA    R2,1(R2)            NOT FOUND: SKIP LEN CODE+DAY                 
         B     EXPL0040                                                         
EXPL0080 EQU   *                                                                
         EX    RF,EXPL0905         MOVE DAY CODE BY LENGTH                      
EXPL0120 EQU   *                                                                
         BAS   RE,EXPLDAY2         CHECK FOR THREE-CHAR CODE                    
         BZ    EXPL0800            3-CHAR CODE FOUND: FINISHED                  
*                                     FOUND ONLY FOR SINGLE DAY ENTRY           
         AR    R4,R5               ADD CHAR LEN -> NEXT O/P SLOT                
         AR    R1,R5               BUMP PAST FOUND I/P DAY                      
         CLI   0(R1),C' '          SPACE = LAST CHAR                            
         BE    EXPL0800            FINISHED                                     
         CLI   0(R1),C','          SEPARATOR FOUND?                             
         BNE   EXPL0200            NO                                           
         MVI   0(R4),C','          YES - INSERT INTO O/P STRING                 
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         B     EXPL0040            GO BACK FOR NEXT DAY CODE                    
EXPL0200 EQU   *                                                                
         CLI   0(R1),C'-'          'THROUGH' INDICATOR?                         
         BE    *+6                 YES                                          
         DC    H'0'                UNKNOWN CHARACTER IN STRING                  
         LA    R1,1(R1)            SKIP 'THROUGH' INDICATOR                     
         AR    R2,R5                                                            
         LA    R2,1(R2)            BUMP PAST FOUND ITEM                         
EXPL0240 EQU   *                                                                
         MVI   0(R4),C','          INSERT SEPARATOR INTO O/P STRING             
         LA    R4,1(R4)            BUMP PAST IT                                 
EXPL0280 EQU   *                                                                
         ZIC   R5,0(R2)            TAKE LENGTH OF NEXT COMPARE                  
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,EXPL0900         COMPARE BY LENGTH                            
         BE    EXPL0320            DAY CODE FOUND                               
         EX    RF,EXPL0905         NOT FOUND:  MOVE DAY FROM TABLE              
         AR    R4,R5               BUMP PAST IT                                 
         MVI   0(R4),C','          INSERT SEPARATOR                             
         LA    R4,1(R4)            BUMP PAST IT                                 
         AR    R2,R5                                                            
         LA    R2,1(R2)            BUMP PAST DAYTAB ENTRY                       
         B     EXPL0280            GO BACK FOR NEXT                             
EXPL0320 EQU   *                                                                
         EX    RF,EXPL0905         MOVE BY LENGTH                               
         B     EXPL0120            GO BACK FOR NEXT COMPARE                     
EXPL0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
EXPL0900 CLC   0(0,R1),1(R2)       COMPARE BY LENGTH                            
EXPL0905 MVC   0(0,R4),1(R2)       COMPARE BY LENGTH                            
*                                                                               
DAYTAB   DC    X'1',C'M'                                                        
         DC    X'2',C'TU'                                                       
         DC    X'1',C'W'                                                        
         DC    X'2',C'TH'                                                       
         DC    X'1',C'F'                                                        
         DC    X'2',C'SA'                                                       
         DC    X'2',C'SU'                                                       
         DC    X'1',C'M'                                                        
         DC    X'2',C'TU'                                                       
         DC    X'1',C'W'                                                        
         DC    X'2',C'TH'                                                       
         DC    X'1',C'F'                                                        
         DC    X'2',C'SA'                                                       
         EJECT                                                                  
*                                                                               
*    EXPLDAY2:  FOR SINGLE DAY ENTRY, IE: MONDAY, A THREE CHARACTER             
*        CODE IS RETURNED.  IF FOUND, A CC OF ZERO IS RETURNED.                 
*             R1  ->  DAY-STRING BEING CHECKED.                                 
*                                                                               
EXPLDAY2 NTR1                                                                   
         LA    R2,THREEDAY                                                      
EXP20020 EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    EXP20100            CODE NOT FOUND: CC NOT ZERO                  
         CLC   0(3,R2),0(R1)                                                    
         BE    EXP20120                                                         
         LA    R2,3(R2)            BUMP TO NEXT ENTRY                           
         B     EXP20020            GO BACK FOR NEXT                             
EXP20100 EQU   *                                                                
         LTR   RB,RB                                                            
         B     EXP20140                                                         
EXP20120 EQU   *                                                                
         SR    R0,R0                                                            
EXP20140 EQU   *                                                                
         XIT1                                                                   
*                                                                               
THREEDAY DC    C'MONTUEWEDTHUFRISATSUN'                                         
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
*   DAYUNPK IS RETURNING A TWO-CHARACTER DAY CODE WHEN THE                      
*        LAST DAY OF THE STRING IS MONDAY, WEDNESDAY, OR FRIDAY.                
*        THIS CAUSES A PROBLEM, AND MUST BE CLEANED UP.                         
*        I DID IT HERE BECAUSE CHANGES TO DAYUNPK WOULD BE                      
*        POSSIBLY DANGEROUS.                                                    
*                                                                               
XDATECLN NTR1                                                                   
         LA    R2,XDATE+11         SET A(LAST CHARACTER)                        
         LA    R0,11               LOOP CONTROL                                 
XCLN0010 EQU   *                                                                
         CLI   0(R2),X'40'         ANY VALUE IN POSITION?                       
         BH    XCLN0020            YES - CHECK THIS AND PREVIOUS                
         BCTR  R2,0                NO  - BACK UP 1 POSITION                     
         BCT   R0,XCLN0010         GO BACK FOR NEXT                             
         DC    H'0'                NO CHARACTERS IN FIELD?                      
XCLN0020 EQU   *                                                                
         BCTR  R2,0                BACK UP ONE MORE POSITION                    
         CLC   =C'MO',0(R2)        TWO CHARACTER MONDAY?                        
         BE    XCLN0040            YES - CLEAN IT UP                            
         CLC   =C'WE',0(R2)        TWO CHARACTER WEDNESDAY?                     
         BE    XCLN0040            YES - CLEAN IT UP                            
         CLC   =C'FR',0(R2)        TWO CHARACTER FRIDAY?                        
         BNE   XCLN0060            NO  - NO CLEANUP NEEDED                      
XCLN0040 EQU   *                                                                
         MVI   1(R2),X'40'         INSERT SPACE OVER LAST CHAR                  
XCLN0060 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHKCMTS:  CHECKS ELEMENT PASSED IN.  IF 'C=' OR 'SC=',                      
*        COMMENT IS RETRIEVED AND FORMATTED FOR OUTPUT.                         
*   P1   =   A(WORKSPACE)                                                       
*   P2   =   A(COMMENT ELEMENT CONTROL)                                         
*   P3   =   TYPE OF RECORD:  1  =  HEADER COMMENT (COMMENT)                    
*                             2  =  REP ORDER COMMENT (INSTRUCTION)             
         DS    0F                                                               
CHKCMTS  NMOD1 0,**CMTS**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(COMMENT ELEMENT)                     
         L     R5,8(R1)            SET INDICATOR FLAG                           
         LA    R7,2(R7)            PASS CONTROL OF ELEMENT                      
         CLC   0(2,R7),=C'C='      STANDARD COMMENT?                            
         BE    CCMT0040            YES                                          
         CLC   0(3,R7),=C'SC='     SFM COMMENT?                                 
         BNE   CCMT0400            NO  - EXIT                                   
CCMT0040 EQU   *                                                                
         L     R2,AIO4             SET A(IO AREA 4)                             
         ICM   RF,15,=AL4(CONLENQ)                                              
*                                                                               
         PRINT GEN                                                              
         XCEF  0(R2),(RF)          CLEAR THE AREA                               
         GOTO1 AREGENSC,DMCB,(3,0(R7)),(R2),DATAMGR,RCONREC,GETTXT              
                                                                                
         PRINT NOGEN                                                            
         SR    R0,R0               SET CC = ZERO                                
         B     CCMT0800            EXIT                                         
*                                  COMMENT FOUND OR NOT WILL BE                 
*                                     DETERMINED AFTER CALL                     
CCMT0400 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
CCMT0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TRANSLATE DAY AND TIME TO JDS FORMAT.                                       
*                                                                               
DYTIME   NMOD1 0,**DTIM**                                                       
         L     RC,0(R1)                                                         
         MVI   ROTATCOM,C'N'       SET 'ROTATOR COMMENT' = NO                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   DYTI0200            NOT FOUND - EXIT                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         ZIC   RF,RBUYDYIN         IS START DAY MONDAY?                         
         SRL   RF,4                DROP END DAY FROM STRING                     
         CH    RF,=H'1'            MONDAY?                                      
         BE    DYTI0020            YES                                          
         MVI   ROTATCOM,C'Y'       NO  - SET 'NEED ROTATOR COMMENT'             
DYTI0020 EQU   *                                                                
         EDIT  RBUYDYT1,(4,WORK)                                                
         OC    WORK(4),FOXZEROS    INSERT ZEROS FOR SPACES                      
         MVC   WORK2(2),WORK       FORMAT TIME STRING                           
         MVI   WORK2+2,C':'        INSERT SEPARATOR                             
         MVC   WORK2+3(2),WORK+2                                                
         MVI   WORK2+5,C':'        INSERT SEPARATOR                             
         MVC   WORK2+6(2),FOXZEROS                                              
         GOTO1 LOADDTA2,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT START TIME (8C),TILDA                 
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         OC    RBUYDYT2,RBUYDYT2   ANY END TIME?                                
         BNZ   DYTI0040            YES                                          
*                                  NO  - LOAD START AS END TIME                 
         GOTO1 LOADDTA2,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT END TIME (8C),TILDA                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
         B     DYTI0060                                                         
DYTI0040 EQU   *                                                                
         EDIT  RBUYDYT2,(4,WORK)                                                
         OC    WORK(4),FOXZEROS    INSERT ZEROS FOR SPACES                      
         MVC   WORK2(2),WORK       FORMAT TIME STRING                           
         MVI   WORK2+2,C':'        INSERT SEPARATOR                             
         MVC   WORK2+3(2),WORK+2                                                
         MVI   WORK2+5,C':'        INSERT SEPARATOR                             
         MVC   WORK2+6(2),FOXZEROS                                              
         GOTO1 LOADDTA2,DMCB,(8,WORK2),(X'A1',0)                                
*                                  INSERT END TIME (8C),TILDA                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
DYTI0060 EQU   *                                                                
         MVC   WORK2(80),SPACES                                                 
         LA    RF,RBUYDAYS         A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),RBUYDYIN    INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =A(DAYUNPK2),DMCB,,(0,XDATE),RR=YES                              
*                                                                               
         GOTO1 =A(EXPLDAYS),DMCB,(RC),RR=Y                                      
*                                                                               
*                                  EXPLODE DAYS TO VCI FORMAT                   
         GOTO1 LOADDTA2,DMCB,(80,WORK2),(X'A1',0)                               
*                                  INSERT PRIORITY (1C),TILDA                   
         L     R7,ABUILD           NEXT ADDRESS RETURNED IN ABUILD              
DYTI0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*    LOADDTA2  ---  STRINGS DATA INTO RECORD AREA.  BACKS UP TO                 
*        FIND LAST NON-SPACE CHARACTER.  INSERTS INDICATED                      
*        CHARACTER AFTER THAT.  PASSES BACK A(NEXT INSERTION).                  
*           P1   =  1ST CHARACTER = L(DATA)                                     
*                  2-4 CHARACTER = A(DATA)                                      
*           P2   =  1ST CHARACTER = DELIMITER CHARACTER                         
*           R7   =  A(INSERTION POINT)                                          
*         ABUILD = RETURNED INSERTION POINT                                     
*         R1 WILL REMAIN POINTING TO PARAMETER LIST                             
* LOCALIZED ROUTINE....                                                         
*********************************************************************           
*                                                                               
LOADDTA2 NTR1                                                                   
         LR    R2,R7               SET A(INSERTION POINT)                       
         ZIC   R4,0(R1)            SET L(DATA TO TRANSFER)                      
         ZICM  R5,1(R1),3          SET A(DATA TO TRANSFER)                      
         BCTR  R4,0                DECREMENT FOR MOVE                           
         EX    R4,LDT20500         MOVE DATA BY LENGTH                          
         LA    R4,1(R4)            SET BACK TO FULL LENGTH                      
         AR    R2,R4               SET A(1 POS PAST LAST CHAR)                  
LDT20020 EQU   *                                                                
         BCTR  R2,0                BACK UP 1 POSITION                           
         CLI   0(R2),C' '          TRAILING SPACE FOUND?                        
         BE    LDT20020            YES - BACK UP ANOTHER                        
         CLI   0(R2),X'00'         TRAILING LOW-VALUES FOUND?                   
*                                     (PROBABLY NOT NECESSARY)                  
         BE    LDT20020            YES - BACK UP ANOTHER                        
         LA    R2,1(R2)            CHARACTER ENCOUNTERED: SKIP PAST             
         MVC   0(1,R2),4(R1)       INSERT SEPARATOR CHARACTER                   
         LA    R2,1(R2)            BUMP PAST                                    
         ST    R2,ABUILD           SET NEW INSERTION ADDRESS                    
         XIT1                                                                   
*                                                                               
LDT20500 MVC   0(0,R2),0(R5)                                                    
         EJECT                                                                  
*                                                                               
*        CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
*        BE    PSTCMTX                                                          
*                                                                               
*STCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
*        BCTR  R4,0                                                             
*        BCTR  R4,0                                                             
*        EX    R4,*+8                                                           
*        B     *+10                                                             
*        MVC   P+30(0),1(R2)                                                    
*        BAS   RE,PRINT                                                         
*                                                                               
*        ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
*        AR    R2,R4                                                            
*        CLI   0(R2),X'FF'         IF X'FF', DONE                               
*        BE    PSTCMTX                                                          
*                                                                               
*        LR    R4,RC               BOUNDARY CHECK FOR R2                        
*        A     R4,=AL4(IO4-GENOLD+L'IO4+1)                                      
*        CR    R4,R2                                                            
*        BH    PSTCMT10                                                         
*        B     PSTCMTX                                                          
*                                                                               
*STCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
*        CLI   RMSCMODE,3          K CMTS OR ORD CMTS??                         
*        BNE   PSTCMTX             ANYTHING ELSE, DON'T PRINT                   
*                                                                               
*        MVC   P+30(60),0(R3)                                                   
*        OC    P+30(60),SPACES                                                  
*                                                                               
*        BAS   RE,PRINT                                                         
*                                                                               
*STCMTX  B     XIT                                                              
         EJECT                                                                  
****>>>>                                                                        
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
EDICT    NMOD1 0,*EDIC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
                                                                                
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
***>>>   MVC   P+9(11),=C'EDICT=*BIAS'                                          
         MVC   P+9(6),=C'EDICT='                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAUSRID                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         L     R6,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R6)   IT ISN'T A GENCON READ HIGH                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         LA    R6,28(R6)           OFFSET TO FIRST ELEMENT                      
*                                                                               
EDICT03  DS    0H                                                               
         CLI   0(R6),0             NOT FOUND, EXIT                              
         BNE   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         CLI   0(R6),CTDSCELQ                                                   
         BE    EDICT05                                                          
         ZIC   RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         AR    R6,RF                                                            
         B     EDICT03                                                          
*                                                                               
EDICT05  DS    0H                                                               
         USING CTDSCD,R6                                                        
         CLI   CTDSCLEN,3          MUST BE AT LEAST 3 CHAR LONG                 
         BL    EDICTX                                                           
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+15(0),CTDSC                                                    
         DROP  R6                                                               
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
         MVC   P+54(2),RCONTEM     TEAM                                         
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
                                                                                
         MVI   EDIPROG,C'E'        FOR TYPE E (ELECTRONIC CONTRACT)             
         MVC   EDIPROG+1(2),REPALPHA                                            
                                                                                
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
                                                                                
         MVC   EDIRCNRP,RCONKREP   REP CODE                                     
         MVC   EDIRCNOF,RCONKOFF   OFF CODE                                     
         MVC   EDIRCNSP,RCONSAL    SALESPERSON CODE                             
         MVC   EDIRCNAG,RCONKAGY   AGENCY CODE                                  
         MVC   EDIRCNAO,RCONKAOF   CITY CODE                                    
         MVC   EDIRCNAD,RCONKADV   ADVERTISER CODE                              
         MVC   EDIRCNCT,RCONKCON   CONTRACT TYPE                                
* FLIGHT START AND END DATES                                                    
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,EDIRCNFS)                            
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,EDIRCNFE)                          
* LATEST VERSION NUMBER                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDICT20                                                          
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    EDICT10                                                          
         EDIT  (1,RCONSSV),(3,EDIRCNVN),ALIGN=LEFT                              
         B     EDICT20                                                          
EDICT10  EDIT  (1,RCONSRV),(3,EDIRCNVN),ALIGN=LEFT                              
                                                                                
EDICT20  MVC   EDIRCNST,RCONKSTA   STATION CALLS                                
* CONTRACT NUMBER                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,EDIRCNHN),ALIGN=LEFT                                
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(14),=C'SUB DONOVAN EC'                                 
                                                                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(06),=C'FIL EC'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON,EDISYST+11,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(07),=C'EXT TXT'                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
**       MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         DROP  R5,R6                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****>>>>                                                                        
*                                                                               
READBUY  NMOD1 0,*RBUY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   KEY,EXTRAKEY        INITIALIZE OR RESET KEY                      
         OC    EXTRAKEY,EXTRAKEY   ANY PRIOR KEY?                               
         BNZ   REBU0020            YES - DO SEQ READ                            
         LA    RF,KEY                                                           
         USING RBUYREC,RF                                                       
         MVI   RBUYKEY,X'0B'       INSERT ID                                    
         MVC   RBUYKREP,REPALPHA                                                
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RE                                                       
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  RE,RF                                                            
         GOTO1 VHIGH                                                            
         B     REBU0040                                                         
REBU0020 EQU   *                                                                
         GOTO1 VSEQ                                                             
REBU0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH CONTRACT #                   
         BE    REBU0060            BUY FOUND                                    
         SR    R0,R0               NO MORE BUYS:  CC = ZERO                     
         LTR   R0,R0                                                            
         B     REBU0080            GO BACK                                      
REBU0060 EQU   *                                                                
         MVC   EXTRAKEY(27),KEY    SAVE KEY FOUND                               
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         LTR   RB,RB               BUY FOUND:  CC NOT = ZERO                    
REBU0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* CODAY - PARAMETER LIST                                              *         
*                                                                     *         
*    WORD 0  BYTE 0     X'FF'= REPPAK (11 BYTE OUTPUT)                *         
*                       X'80'= REPPAK (11 BYTE OUTPUT)                *         
*                       START DAY FOR TRUE 'OUT-OF-WEEKS'             *         
*                       IN FIRST NIBBLE                               *         
*            BYTE 1-3   A(1 BYTE DAY CODE)                            *         
*                       X'40'=MON,X'01'=SUN                           *         
*                       X'80' = OLD STYLE OUT OF WEEK                 *         
*                                                                               
*    WORD 1  BYTE 0     X'07' = FORCE MTWTFSS OUTPUT                  *         
*                       X'80' = OUTPUT IN FRENCH                      *         
*            BYTE 1-3   A(OUTPUT AREA)                                *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
DAYUNPK2 NMOD1 WORKX-WORKD,DAYUNPK,CLEAR=YES                                    
         USING WORKD,RC                                                         
         MVC   CDPARS,0(R1)        SAVE PARAMETERS                              
*&&DO                                                                           
*   TEST                                                                        
***      L     R1,CDPAR1                                                        
         ZICM  R1,CDPAR1+1                                                      
         CLI   3(R1),X'7C'                                                      
         BNE   TEST0040                                                         
         MVC   DIE(2),=X'0000'                                                  
*&&                                                                             
TEST0040 EQU   *                                                                
*   TEST                                                                        
*                                                                               
DIE      EQU   *                                                                
         TM    CDPAR2,X'80'        FRENCH OUTPUT?                               
         BZ    *+16                                                             
         MVC   DTAB,DTAB2          OVERWRITE LONG FORM                          
         MVC   DTAB1,DTAB3         OVERWRITE SHORT FORM                         
*                                                                               
         L     R1,0(R1)            GET BDAY ADDRESS                             
         TM    0(R1),X'7F'         TEST ANY DAY BITS ON                         
         BNZ   CDY00               YES                                          
         L     RE,CDPAR2           INPUT NOT VALID                              
         MVC   0(7,RE),=C'???????' SET THAT INPUT WAS WRONG                     
         B     DDEXIT                                                           
*                                                                               
CDY00    SR    R2,R2                                                            
         IC    R2,0(R1)            INSERT DAY CDE                               
         LA    R4,CDAYWK+6         POINT TO SUNDAY POSITION                     
         LA    R5,7                SET FOR SUNDAY                               
*                                                                               
CDY01    SR    R3,R3               CLEAR R3                                     
         SRDL  R2,1                SHIFT BIT TO R3                              
         LTR   R3,R3               TEST IF ON                                   
         BZ    *+8                 NO                                           
         STC   R5,0(R4)            YES-STORE DAY NUMBER                         
         BCTR  R4,0                BUMP TO NEXT LOWER DAY                       
         BCT   R5,CDY01            BUMP CDE TO NXT LWR DAY                      
         LTR   R2,R2               TEST IF OUT-OF-WK ROTATOR                    
         BZ    CDY03               NO                                           
         LA    R4,CDAYWK           YES-POINT TO MONDAY                          
         LA    R5,CDAYWK+7         POINT TO NEXT MONDAY                         
         LA    R6,6                SET BCT REG FOR MON THRU SAT                 
*                                                                               
CDY02    CLI   0(R4),0             TEST IF END OF ROTATION                      
         BE    CDY03               YES                                          
         MVC   0(1,R5),0(R4)       NO-MOVE TO NEXT WEEK                         
         MVI   0(R4),0             ZERO THIS WEEK                               
         LA    R5,1(R5)            BUMP NXT WEEK                                
         LA    R4,1(R4)            AND THIS WEEK POINTERS                       
         BCT   R6,CDY02            TEST NEXT DAY                                
*                                                                               
CDY03    LA    R8,13               SET BCT REG FOR MAX 13 DAY SCAN              
         LA    RE,DYLTL            POINT TO DAY LITERAL FORMAT AREA             
         MVI   DSCMSW,128          SET FOR 1ST TIME                             
         LA    R4,CDAYWK            POINT TO THIS MONDAY                        
         LA    R5,DTAB-3            BASE ADDRESS FOR DAYTAB                     
         SR    R7,R7                                                            
*                                  SET UP FOR TRUE OUT-OF-WEEKS                 
*                                  ----------------------------                 
         CLI   CDPAR1,X'FF'        NOT IF OLD REP CALL                          
         BE    CDY04                                                            
         MVC   DDBYTE,CDPAR1       START DAY IN FIRST NIBBLE                    
         NI    DDBYTE,X'7F'        STRIP OUT ANY REP INDICATOR                  
         SR    R1,R1                                                            
         IC    R1,DDBYTE                                                        
         SRA   R1,4                NO START DAY MEANS NOT OUT OF WEEK           
         BZ    CDY04                                                            
         MVC   CDAYWK+7(7),CDAYWK  COPY DAYS                                    
         LA    R4,CDAYWK-1(1)      POINT TO FIRST USED DAY                      
         LA    R8,7                ONLY SCAN 7 DAY POSITIONS                    
*                                                                               
CDY04    CLI   0(R4),0             TEST FOR ACTIVE DAY                          
         BNE   CDY045              YES                                          
         TM    DSCMSW,16           TEST IF 2ND TIME THRU ON ROTATION            
         BZ    CDY041              NO                                           
         LA    RE,2(RE)            YES-BUMP POINTER BY 2                        
         B     CDY043              SET FOR COMMA                                
*                                                                               
CDY041   TM    DSCMSW,1            TEST IF 3RD TIME THRU ON ROTATION            
         BZ    CDY044              NO                                           
         LA    RE,1(RE)            BUMP OVER END DAY OF ROTATION                
         CLI   0(RE),C' '          TEST IF TRUNCATED                            
         BZ    *+8                 YES                                          
         LA    RE,1(RE)            NO - BUMP OVER 2ND CHAR OF DAY               
*                                                                               
CDY043   MVI   DSCMSW,2            SET FOR COMMA                                
*                                                                               
CDY044   LA    R4,1(R4)            BUMP TO NEXT DAY                             
         BCT   R8,CDY04                                                         
         B     CDY048              END-COMPUTE LENGTH OF LTL                    
*                                                                               
CDY045   SR    R6,R6                                                            
         IC    R6,0(R4)            INSERT DAY NUMBER                            
         MH    R6,=H'3'            MULT BY DAY ABBREV LGTH                      
         LA    R6,0(5,R6)          POINT TO TABLE ENTRY                         
         LA    R2,1                SET MOVE LGTH                                
         TM    DSCMSW,128                                                       
         BZ    *+12                IF 1ST TIME                                  
         MVI   DSCMSW,0                                                         
         LA    R2,2                MOVE 3 CHARS                                 
         TM    DSCMSW,16           IF LOOKING FOR NXT DAY OF ROTATION           
         BZ    *+20                                                             
         MVI   DSCMSW,1            SET FOR SUBSEQUENT DAYS                      
         LA    RE,2(RE)            BUMP OVER 1ST DAY OF ROTATION                
         MVI   0(RE),C'-'          MOVE IN DASH                                 
         LA    RE,1(RE)                                                         
         CLI   DSCMSW,1            TEST IF DASH OR COMMA WANTED                 
         BNP   CDY046                                                           
         MVI   0(RE),C','                                                       
         TM    CDPAR1,X'80'        REPPAK?                                      
         BNZ   *+8                                                              
         MVI   0(RE),C'/'          SLASH(INSTEAD OF COMMA)                      
         LA    RE,1(RE)                                                         
CDY046   EX    2,MVDAY             MVC  0(0,14),0(6)                            
         TM    CDPAR2,X'80'        FRENCH REPORT?                               
         BNZ   CDY0460             YES                                          
         CLI   0(RE),C'T'          TEST IF TUE/THU                              
         BE    CDY047                                                           
         CLI   0(RE),C'S'          TEST IF SAT/SUN                              
         BE    CDY047                                                           
         B     CDY0461                                                          
*                                                                               
CDY0460  CLI   0(RE),C'M'          TEST IF MAR                                  
         BE    CDY047                                                           
         CLI   0(RE),C'J'          TEST IF JEU                                  
         BE    CDY047                                                           
         CLI   0(RE),C'S'          TEST IF SAM                                  
         BE    CDY047                                                           
         CLI   0(RE),C'D'          TEST IF DIM                                  
         BE    CDY047                                                           
*                                                                               
CDY0461  BCTR  RE,0                1 CHARACTER IS UNIQUE                        
         CLI   DSCMSW,1            TEST IF ROTATOR                              
         BNE   *+12                                                             
         MVI   2(RE),C' '          BLANK  CHARACTER 2                           
         LA    RE,1(RE)            DONT MOVE R14                                
CDY047   CLI   DSCMSW,1            TEST IF LOOKINGFOR  ROTATOR END              
         BE    CDY043+4            YES- RETURN                                  
         MVI   DSCMSW,16           NO-SET FOR 2ND DAY OF ROTATION               
         B     CDY043+4            RETURN FOR NXT DAY                           
*                                                                               
CDY048   LA    RE,DYLTL            POINT TO FORMATTED LITERAL                   
         SR    RF,RF               ZERO LGTH COUNTER                            
         TM    0(RE),X'BF'         EXIT ON FIRST BLANK OR ZERO                  
         BZ    CDY05                                                            
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
* LITERAL FORMATTED - MOVE TO CENTER OF OUTPUT FIELD                            
*                                                                               
CDY05    EQU   *                                                                
         LA    R1,DYLTL(RF)        POINT TO END OF LTRL                         
         BCTR  R1,0                POINT TO 2ND LAST CHAR                       
         BCTR  R1,0                                                             
*                                                                               
         TM    CDPAR2,X'80'        FRENCH REPORT?                               
         BNZ   CDY050              YES                                          
         CLI   0(R1),C'F'                                                       
         BE    CDY055                                                           
         CLI   0(R1),C'W'                                                       
         BE    CDY055                                                           
         CLI   0(R1),C'M'                                                       
         BNE   CDY056                                                           
*                                                                               
CDY050   CLI   0(R1),C'V'                                                       
         BE    CDY055                                                           
         CLC   0(2,R1),=C'ME'                                                   
         BE    CDY055                                                           
         CLI   0(R1),C'L'                                                       
         BNE   CDY056                                                           
*                                                                               
CDY055   BCTR  RF,0                                                             
*                                                                               
CDY056   EQU   *                                                                
         L     R1,CDPAR2           OUTPUT AREA                                  
         TM    CDPAR1,X'80'        REPPAK?                                      
         BZ    SPOTCD                                                           
         MVC   0(12,1),=12C' '                                                  
         STC   RF,CDPAR1           LENGTH                                       
         CH    RF,=H'12'           MAX                                          
         BNH   CODAYX                                                           
         LA    RF,12               MAX LENGTH                                   
         B     CODAYX                                                           
SPOTCD   XC    0(8,R1),0(R1)       SPOTPAK                                      
         CLI   CDPAR2,7                                                         
         BE    FORCE7                                                           
         CH    RF,=H'8'            MAX                                          
         BNH   CODAYX                                                           
         EJECT                                                                  
         BAS   R9,COMPR1           COMPRESS THREE TO TWO                        
         CH    RF,=H'8'            NOW DOES IT FIT                              
         BNH   CODAYX              YES, SEND IT BACK                            
         BAS   R9,COMPR6           COMPRESS TWO TO ONE                          
         CH    RF,=H'8'            DOES IT FIT                                  
         BNH   CODAYX              YES                                          
FORCE7   BAS   R9,SHORT            DO SHORT METHOD (M.W.F..)                    
         SPACE 1                                                                
CODAYX   MVI   0(R1),C'?'          PRESET FOR STRATA EXPRESSION                 
         LTR   RF,RF                                                            
         BZ    DDEXIT                                                           
         BCTR  RF,0                                                             
         EX    RF,MVLTL            MVC  0(0,1),DYLTL                            
*                                                                               
DDEXIT   XIT1                                                                   
*                                                                               
MVDAY    MVC   0(0,RE),0(R6)       MOVE DAY ABBRV                               
MVLTL    MVC   0(0,R1),DYLTL                                                    
*                                                                               
DTAB     DC    C'MONTUEWEDTHUFRISATSUN'                                         
DTAB1    DC    C'MOWEFR'                                                        
DTAB2    DC    C'LUNMARMERJEUVENSAMDIM'                                         
DTAB3    DC    C'LUMEVE'                                                        
*                                                                               
         EJECT                                                                  
*              COMPRESS ALL THREE CHARACTER CODES TO TWO                        
         SPACE 1                                                                
COMPR1   LA    R3,DTAB                                                          
         LA    R4,DYLTL                                                         
COMPR2   TM    0(R4),X'C0'         - / OR  ,                                    
         BNO   COMPR4              ARE SKIPPED                                  
         LA    R5,7                                                             
         CLC   0(3,R4),0(R3)       OUTPUT MATCH DAY TABLE                       
         BE    *+16                                                             
         LA    R3,3(R3)            NEXT DAY                                     
         BCT   R5,*-14                                                          
         B     COMPR4              NO MATCH                                     
         SPACE 1                                                                
         LA    RE,DYLTL(RF)        END                                          
         SR    RE,4                LESS ADDRESS OF MATCHING                     
         SH    RE,=H'4'            GET LENGTH FOR EX                            
         BM    COMPR3                                                           
         EX    RE,*+8                                                           
         B     COMPR3                                                           
         MVC   2(0,R4),3(R4)       SHIFT LEFT                                   
COMPR3   BCTR  RF,0                NEW LENGTH                                   
         LA    RE,DYLTL(RF)        NEW END                                      
         MVI   0(RE),X'40'         CLEAR LAST CHARACTER                         
         B     COMPR1              START OVER AGAIN                             
         SPACE 1                                                                
COMPR4   LA    R4,1(R4)                                                         
         CLI   0(R4),X'41'         END OF DYLTL                                 
         BL    0(R9)                                                            
         LA    R3,DTAB                                                          
         B     COMPR2                                                           
         EJECT                                                                  
*              COMPRESS TWO CHARACTER CODES TO ONE                              
         SPACE 1                                                                
COMPR6   LA    R3,DTAB1                                                         
         LA    R4,DYLTL                                                         
COMPR7   TM    0(R4),X'C0'         - / OR ,                                     
         BNO   COMPR9              ARE SKIPPED                                  
         LA    R5,3                                                             
         CLC   0(2,R4),0(R3)       OUTPUT MATCH DAY TABLE                       
         BE    *+16                                                             
         LA    R3,2(R3)            NEXT DAY                                     
         BCT   R5,*-14                                                          
         B     COMPR9              NO MATCH                                     
         SPACE 1                                                                
         LA    RE,DYLTL(RF)                                                     
         SR    RE,4                LESS ADDRESS OF MATCHING                     
         SH    RE,=H'3'            GET LENGTH FOR EX                            
         BM    COMPR8                                                           
         EX    RE,*+8                                                           
         B     COMPR8                                                           
         MVC   1(0,R4),2(R4)       SHIFT LEFT                                   
COMPR8   BCTR  RF,0                                                             
         LA    RE,DYLTL(RF)                                                     
         MVI   0(RE),X'40'         CLEAR LAST CHARACTER                         
         B     COMPR6                                                           
         SPACE 1                                                                
COMPR9   LA    R4,1(R4)                                                         
         CLI   0(R4),X'41'                                                      
         BL    0(R9)                                                            
         LA    R3,DTAB1                                                         
         B     COMPR7                                                           
         EJECT                                                                  
SHORT    MVC   DYLTL(7),=7C'.'                                                  
         SR    R2,R2                                                            
         L     R1,CDPAR1                                                        
         IC    R2,0(R1)                                                         
         LA    R6,DTAB+18          START AT SUNDAY WORK BACK                    
         LA    R4,DYLTL+6                                                       
         LA    R5,7                                                             
         SPACE 1                                                                
SHORT1   SR    R3,R3                                                            
         SRDL  R2,1                                                             
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   0(1,R4),0(R6)       FIRST LETTER TO DYLTL                        
         BCTR  R4,0                                                             
         SH    R6,=H'3'                                                         
         BCT   R5,SHORT1                                                        
*                                                                               
         CLI   DDBYTE,0            OUT OF WEEK ROTATOR                          
         BE    SHORT2                                                           
         ZIC   15,DDBYTE                                                        
         SRA   15,4                                                             
         BZ    SHORT2                                                           
         MVC   DDWORK(7),DYLTL                                                  
         MVC   DDWORK+7(7),DYLTL                                                
         LA    15,DDWORK-1(15)                                                  
         MVC   DYLTL(7),0(15)                                                   
*                                                                               
SHORT2   LA    RF,7                SET LENGTH                                   
         L     R1,CDPAR2                                                        
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
WORKD    DSECT                                                                  
CDPARS   DS    0CL8                                                             
CDPAR1   DS    F                                                                
CDPAR2   DS    F                                                                
DYLTL    DS    D                                                                
DDWORK   DS    XL16                                                             
CDAYWK   DS    CL14                                                             
DSCMSW   DS    C                                                                
DDBYTE   DS    C                                                                
         DS    CL4                                                              
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043RECNT5B   12/08/04'                                      
         END                                                                    
