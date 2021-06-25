*          DATA SET REREP1902  AT LEVEL 149 AS OF 05/01/02                      
*PHASE RE1902C,*                                                                
         TITLE 'REREP1902 - RE1902 - INVOICE CONTROL LIST REPORT'               
*                                                                               
***********************************************************************         
*                                                                     *         
*- REREP1902 --- INVOICE CONTROL LIST REPORT                          *         
*                 (SHOW STATIONS NOT REPORTING $ FOR A MONTH)         *         
*                                                                     *         
***********************************************************************         
*  MOD LOG                                                            *         
*  -------                                                            *         
*                                                                     *         
*  MAY25/90 (MRR) --- INITIAL DEVELOPMENT                             *         
*                                                                     *         
*  JUL31/91 (BU ) --- FILTER CONTRACTS ON TYPE                        *         
*                                                                     *         
*  OCT08/91 (SKU) --- ADD INTERFACE # COLUMN, TAKE OUT FOOTHOOK       *         
*                                                                     *         
*  MAR25/92 (MRR) --- GET TABLE AREA VIA COVAIL                       *         
*                                                                     *         
*  NOV10/93 (BU ) --- CHECK 12 MONTH PERIOD FOR CONVERTED STATIONS    *         
*                                                                     *         
*  JAN26/94 (BU ) --- FIX ' I' DISPLAY ERROR                          *         
*                                                                     *         
*  APR13/94 (BU ) --- INSTALL DOWNLOADING                             *         
*                                                                     *         
*  AUG23/96 (SEP) --- REFLECTS INTNL FAX AND MAILBOXES                *         
*                                                                   *           
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                               
* READ ALL THE STATIONS FOR A REP AS FILTERED BY THE REQUEST (GROUP,            
* SUB-GROUP OR STATION).  BUILD A TABLE ENTRY AS FOLLOWS:                       
*                                                                               
* GROUP/SUB-GROUP       CL2    +00                                              
* STATION               CL5     02                                              
* FLAG1                 CL1     07     CONTRACTS IN PERIOD                      
* FLAG2                 CL1     08     ONE CONTRACT WIL X'04' FOUND             
* MARKET NAME           CL20    09                                              
* TWX #                 CL12    29     WILL BE PRINTED AS TELEPHONE #           
* FAX #                 CL12    41                                              
* INTERFACE #           CL10    53                                              
* MONTH ARRAY           CL24    63     12 MONTH CONVERTED ARRAY                 
*                               87     LENGTH OF ENTRY                          
*                                                                               
* THE TABLE WILL BE SORTED ON THE FIRST TWO DATA ITEMS AND THEN                 
* FLAG1 AND FLAG2 WILL BE FILLED IN AS CONTRACTS ARE                            
* PROCESSED.                                                                    
*                                                                               
***********************************************************************         
*                                                                               
*  QOPTION1 - OPTION 1                                                          
*                                                                               
*                                                                               
*  ALL PROCESSING DONE IN REQ FIRST MODE.                                       
*                                                                               
*  R8 IS USED TO POINT TO THE STATION TABLE AND COVERS THE STABLE               
*  DSECT.  THE USING STATEMENT IS AT THE TOP AND IS USED BY THE                 
*  ENTIRE PROGRAM.  I.E. LEAVE IT AND R8 ALONE.                                 
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
RE1902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1902,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         USING STABLE,R8                                                        
*                                                                               
         STM   R2,RC,SAVEREGS      SAVE REGS 2-C FOR HEADLINE                   
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   MAIN                                                             
*                                                                               
         L     RE,ADCONLST                                                      
         USING ADCONSD,RE                                                       
         L     RF,COVAIL                                                        
         DROP  RE                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
         OC    P2,P2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ATABLE,P2                                                        
         L     RF,=F'1000000'                                                   
         L     RE,ATABLE                                                        
         LR    R2,RE                                                            
         XCEF                                                                   
         A     R2,=F'1000000'                                                   
         ST    R2,ATABLEX                                                       
*                                                                               
         B     MAINEXIT                                                         
*                                                                               
MAIN     EQU   *                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
*                                                                               
         L     R8,ATABLE                                                        
         BAS   RE,INITIAL          STARTUP                                      
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,BLDTBL           READ STATIONS, BUILD TABLE, SORT             
         BNZ   ERROR                                                            
*                                                                               
*- FOR EACH STATION IN THE TABLE,                                               
*        READ CONTRACTS USING X'0C' KEY                                         
*        APPLY DATE RANGE FILTERS                                               
*        MARK TABLE ENTRY                                                       
*                                                                               
         L     R8,ATABLE            POINT TO THE START                          
         CLI   0(R8),0                                                          
         BNE   MAIN50                                                           
         MVC   P(L'NODATA),NODATA                                               
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         B     OKEXIT                                                           
MAIN50   EQU   *                                                                
         GOTO1 GETGROUP,DMCB,0(R8)                                              
*                                 RETRIEVE GROUP NAME                           
MAIN100  EQU   *                                                                
*                                                                               
         L     RF,GRPSTN           BUMP STATION COUNT                           
         A     RF,ONE                                                           
         ST    RF,GRPSTN                                                        
         BAS   RE,READCON          READ CONTRACTS                               
         BNZ   ERROR                                                            
*                                                                               
         CLI   QOPTION3,C' '       OPTION3 SET?                                 
         BE    MAIN140             NO  - DON'T FILTER ON ARRAY VALUES           
         XC    SETVAL01,SETVAL01                                                
         MVI   SETVAL01,C'Y'       SET VALUE FOR 'ORDERED ONLY'                 
         CLI   QOPTION3,C'O'       'ORDERED ONLY'?                              
         BE    MAIN110             YES - CHECK ARRAY                            
         XC    SETVAL01,SETVAL01                                                
         MVI   SETVAL01+1,C'Y'     SET VALUE FOR 'INVOICED ONLY'                
         CLI   QOPTION3,C'I'       'INVOICED ONLY'?                             
         BE    MAIN110             YES - CHECK ARRAY                            
         MVC   SETVAL01,=C'YY'     SET VALUE FOR 'ORD+INV BOTH'                 
         CLI   QOPTION3,C'B'       'ORD+INV BOTH'?                              
         BE    MAIN110             YES - CHECK ARRAY                            
         DC    H'0'                UNRECOGNIZED OPTION                          
MAIN110  EQU   *                                                                
         LA    RF,STMONS           A(STATION MONTH VALUES)                      
         LA    RE,12                                                            
MAIN120  EQU   *                                                                
         CLC   0(2,RF),SETVAL01    MONTH OF ARRAY = FILTER?                     
         BE    MAIN130             YES - CONTINUE                               
         XC    0(2,RF),0(RF)       CLEAR MONTH OF ARRAY                         
MAIN130  EQU   *                                                                
         LA    RF,2(RF)            BUMP TO NEXT BUCKET                          
         BCT   RE,MAIN120          GO BACK FOR NEXT                             
         B     MAIN150             SKIP COUNTING FOR FILTERED                   
*                                     STATIONS:  NOT DISPLAYED                  
MAIN140  EQU   *                                                                
         CLI   STFLAG1,1           1=CONTRACT FOUND                             
         BNE   MAIN150             NO CONTRACTS - DON'T COUNT IT                
         L     RF,GRPSTNW          BUMP STATIONS W/CONTRACTS COUNT              
         A     RF,ONE                                                           
         ST    RF,GRPSTNW                                                       
*                                                                               
         CLI   STFLAG2,1           1=STATION CONVERTED, DON'T COUNT             
         BE    MAIN150                                                          
         L     RF,GRPSTNN          BUMP UNCONVERTED STATION COUNT               
         A     RF,ONE                                                           
         ST    RF,GRPSTNN                                                       
MAIN150  EQU   *                                                                
         OC    STMONS,STMONS       ANY ACTIVITY IN REPORT YEAR?                 
         BZ    MAIN200             NO  - DON'T PRINT                            
         BAS   RE,DODETAIL                                                      
*                                                                               
MAIN200  EQU   *                                                                
         CLI   STLNTRY(R8),0       END OF TABLE?                                
         BE    MAIN500                                                          
         CLC   STLNTRY(2,R8),0(R8) GROUP/SUB-GROUP CHANGE                       
         BE    MAIN300                                                          
         OC    STNGRPFL,STNGRPFL   ANY STATIONS IN GROUP PRINTED?               
         BZ    MAIN250             NO                                           
         BAS   RE,DOGROUP          YES - PRODUCE GROUP TOTALS                   
MAIN250  EQU   *                                                                
         GOTO1 GETGROUP,DMCB,STLNTRY(R8)                                        
MAIN300  EQU   *                                                                
         LA    R8,STLNTRY(R8)      NEXT STATION IN LIST                         
         B     MAIN100                                                          
*                                                                               
MAIN500  EQU   *                                                                
         OC    GRPSTNW,GRPSTNW     ANY STATIONS W/CONTRACTS IN GROUP?           
         BZ    MAIN550             NO                                           
         BAS   RE,DOGROUP          DO LAST GROUP                                
MAIN550  EQU   *                                                                
         BAS   RE,DORUN                                                         
         BAS   RE,DORECAP                                                       
*                                                                               
*- ALL DONE, NO ERRORS                                                          
OKEXIT   GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    MAINEXIT            YES - SKIP MESSAGE                           
         MVC   P(L'OKMSG),OKMSG                                                 
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         B     MAINEXIT                                                         
*                                                                               
*- ALL DONE, ERRORS FOUND                                                       
ERROR    GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVC   P(L'ERRMSG),ERRMSG                                               
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         B     MAINEXIT                                                         
*                                                                               
*        MAIN CONTROL XMOD                                                      
*                                                                               
MAINEXIT EQU   *                                                                
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
EXXMOD   XMOD1                                                                  
*                                                                               
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         MVI   RCSUBPRG,0          SET BASIC SUBPROGRAM                         
         CLI   QOPTION3,C' '       SHOW ALL MONTH TYPES?                        
         BE    INIT0000            YES                                          
         MVI   QOPTION1,C'N'       NO  - TURN OTHER OPTS                        
         MVI   QOPTION2,C'N'          TO 'SUPPRESS'                             
INIT0000 EQU   *                                                                
         LA    RF,COMMANDL                                                      
         LA    RE,COMMAND                                                       
         XCEF                                                                   
*                                                                               
         MVC   YRDATES-8(8),=C'*YRDATE*'                                        
*                                                                               
         LA    RE,IOAREA                                                        
         LH    RF,=Y(IOAREAL)                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,=F'1000000'                                                   
         L     RE,ATABLE                                                        
         XCEF                                                                   
*                                                                               
         LA    R0,HHOOK                                                         
         ST    R0,HEADHOOK                                                      
         L     RF,ADCONLST         A(EXTENDED ADCON LIST)                       
         USING ADCONSD,RF                                                       
         DROP  RF                                                               
*                                                                               
         MVC   QCONTYP2,QCONTYPE   ESTABLISH INCLUDE/EXCLUDE TYPE               
         OC    QCONTYP2(1),=X'40'  SET UPPER CASE BIT                           
         CLI   QCONTYPE,C' '       CONTRACT TYPE ENTERED?                       
         BE    INIT0010            NO                                           
         MVI   RCSUBPRG,1          SET HEADING ROUTINE TO 'INCLUDE'             
         TM    QCONTYPE,X'40'      INCLUDE OR EXCLUDE RUN?                      
         BO    INIT0010            ON:  UPPER=INCLUDE                           
         MVI   RCSUBPRG,2          SET HEADING ROUTINE TO 'EXCLUDE'             
INIT0010 EQU   *                                                                
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    RE,IOAREA                                                        
         ST    RE,AIOAREA                                                       
*                                                                               
*- CONVERT PERIOD START YYMM INTO BINARY                                        
*                                                                               
         MVC   QSTART+4,=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,PERIOD)                                
*                                                                               
*- LOAD 12-MONTH ARRAY TABLE                                                    
*                                                                               
         MVC   YRDATES+22(2),PERIOD INSERT CURRENT YYMM IN LAST SLOT            
         LA    R2,11               SET LOOP CONTROL                             
         LA    RE,YRDATES+22                                                    
         LA    RF,YRDATES+20                                                    
INIT0020 EQU   *                                                                
         MVC   0(2,RF),0(RE)       MOVE PERIOD DOWN 1 BUCKET                    
         ZIC   R0,1(RF)            DROP DATE DOWN 1 MONTH                       
         C     R0,=F'1'            MONTH = 01?                                  
         BE    INIT0030            YES                                          
         BCTR  R0,0                NO  - DECREMENT 1 MONTH                      
         STC   R0,1(RF)            STORE IT BACK                                
         B     INIT0040                                                         
INIT0030 EQU   *                                                                
         MVI   1(RF),12            SET MONTH = 12                               
         ZIC   R0,0(RF)            SET YEAR DOWN BY 1                           
         BCTR  R0,0                                                             
         STC   R0,0(RF)                                                         
INIT0040 EQU   *                                                                
         LA    R1,2                DROP BACK 2 POS IN                           
         SR    RF,R1                  BOTH BUCKETS                              
         SR    RE,R1                                                            
         BCT   R2,INIT0020         GO BACK FOR NEXT BUCKET                      
*                                                                               
INIT0060 EQU   *                                                                
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK AREA)                   
         USING TSARD,R1                                                         
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,4                SET KEY LENGTH (1 WORD COUNTER)              
         STC   R0,TSKEYL                                                        
         LA    R0,92               SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
                                                                                
INITOK   SR    R0,R0               GOOD CC                                      
         B     TESTEXIT                                                         
*                                                                               
INITXC   XC    0(0,RE),0(RE)                                                    
*                                                                               
       ++INCLUDE REREPTSAR                                                      
         EJECT                                                                  
*                                                                               
*- READ STATIONS ON FILE AND PUT INTO TABLE.                                    
*                                                                               
BLDTBL   NTR1                                                                   
         L     R8,ATABLE            POINT TO THE START OF THE TABLE             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20,QREP                                                      
         MVC   KEY+22,QSTATION                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     BLDTBL20                                                         
*                                                                               
BLDTBL10 EQU   *                                                                
         GOTO1 SEQ                                                              
*                                                                               
BLDTBL20 EQU   *                                                                
         CLI   KEY,X'02'           END OF STATION KEYS?                         
         BNE   BLDTBL90                                                         
         CLC   QREP(2),KEY+20      OUT OF THE REP?                              
         BNE   BLDTBL90                                                         
         CLC   QSTATION,SPACES     DO WE HAVE A STATION FILTER                  
         BE    BLDTBL25            NO                                           
         CLC   QSTATION,KEY+22     YES, CHECK IT                                
         BNE   BLDTBL90                                                         
BLDTBL25 EQU   *                                                                
*                                                                               
         GOTO1 GREC                GET THE STATION RECORD                       
*                                                                               
         CLC   QGROUP(1),SPACES    GROUP FILTER?                                
         BE    BLDTBL30            NO                                           
*                                                                               
         CLC   QGROUP(1),RSTAGRUP  CHECK GROUP                                  
         BNE   BLDTBL10            SKIP RECORD IF NOT EQUAL                     
*                                                                               
         CLC   QSBGROUP(1),SPACES  SUB-GROUP FILTER?                            
         BE    BLDTBL30            NO                                           
*                                                                               
         CLC   QSBGROUP(1),RSTAGRUP+1  CHECK SUB-GROUP                          
         BNE   BLDTBL10                SKIP RECORD IF NOT EQUAL                 
*                                                                               
BLDTBL30 EQU   *                                                                
*                                                                               
*- RECORD PASSES FILTERS.  ADD TO TABLE.                                        
*                                                                               
         L     RE,ATABLEX                                                       
         CR    R2,RE                                                            
         BL    BLDTBL50                                                         
*                                                                               
         MVC   P(L'TABMAX),TABMAX  TABLE OVERFLOW                               
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         B     BLDTBLER                                                         
*                                                                               
BLDTBL50 EQU   *                                                                
         MVC   STGROUP(2),RSTAGRUP                                              
         MVC   STSTN(5),RSTAKSTA                                                
         MVC   STMKT(20),RSTAMKT                                                
*                                                                               
         MVC   STEND,RSTAEND        MOVE LEAVE DATE INTO SORT REC               
*                                                                               
         MVI   ELCODE,X'07'                                                     
         LA    R6,RSTAREC                                                       
         BAS   RE,GETEL                                                         
         BNE   BLDTBL60                                                         
         USING RSTATWXL,R6                                                      
         MVC   STTWX(12),RSTATWX                                                
         DROP  R6                                                               
BLDTBL60 EQU   *                                                                
         MVI   ELCODE,X'08'                                                     
         LA    R6,RSTAREC                                                       
         BAS   RE,GETEL                                                         
         BNE   BLDTBL70                                                         
         USING RSTAXXEL,R6                                                      
         MVC   STFAX(12),RSTAOFAX                                               
         MVC   STINT(10),RSTAOSI                                                
         MVC   STINTFCE(10),RSTAOSI                                             
*                                  INSERT INTERFACE INTO KEY                    
         DROP  R6                                                               
BLDTBL70 EQU   *                                                                
*                                                                               
         L     RF,TABCOUNT                                                      
         A     RF,ONE                                                           
         ST    RF,TABCOUNT                                                      
         LA    R8,STLNTRY(R8)      NEXT TBL ENTRY                               
         B     BLDTBL10                                                         
*                                                                               
*- SORT                                                                         
*                                                                               
BLDTBL90 EQU   *                                                                
         CLC   TABCOUNT,ONE                                                     
         BE    BLDTBLOK                                                         
         L     R2,TABCOUNT                                                      
         XC    DMCB,DMCB                                                        
         MVC   P1+1(3),ATABLE+1                                                 
         GOTO1 XSORT,DMCB,,(R2),A(STLNTRY),A(STLKEY),0                          
*                                                                               
*- DONE.                                                                        
*                                                                               
BLDTBLOK EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
BLDTBLER EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- GETGROUP --- READ THE GROUP/SUBGROUP RECORD AND STORE THE NAME               
*                                                                               
GETGROUP NTR1                                                                   
*                                                                               
         L     R2,0(R1)            GET PASSED ADDR                              
*                                                                               
         XC    STNGRPFL,STNGRPFL   CLEAR 'STATIONS PRINTED' FLAG                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPREC,R6                                                       
*                                                                               
         MVI   RGRPKEY,X'07'                                                    
         MVC   RGRPKREP(2),QREP                                                 
         MVC   RGRPKGRP(2),0(R2)                                                
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GG10                                                             
         DC    H'0'                                                             
*                                                                               
GG10     EQU   *                                                                
         GOTO1 GREC                                                             
         LA    R6,RGRPREC                                                       
         MVC   GROUPN,RGRPNAME                                                  
*                                                                               
GGGOOD   EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- READCON -- READ CONTRACTS FOR STATION CALL LETTERS FOR THE CURRENT           
*              STATION IN THE TABLE AS POINTED TO BY R2.                        
*                                                                               
*             READ CONTRACTS UNTIL EOF ON THIS STATION OR FIND A                
*             CONTRACT WITH A X'04' ACUTAL DOLLARS (INVOICE) FOR THE            
*             REQUESTED MONTH.  ACCUMULATE COUNTS AND DOLLARS UNTIL             
*             THE ABOVE CONDITION IS MET.  PASS BACK A 1 IN THE 1ST             
*             BYTE OF DMCB IF EOF CONDITION WAS MET ELSE PASS BACK              
*             A ZERO.  ZERO THE COUNTS AND DOLLARS IFF X'04' FOUND.             
*                                                                               
*                                                                               
READCON  NTR1                                                                   
*                                                                               
*- BUILD 1ST KEY                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           CONTRACT '0C' MAIN POINTER                   
         MVC   KEY+2(2),QREP                                                    
         MVC   KEY+4(2),STGROUP    GROUP/SUBGROUP TO FIND                       
         MVC   KEY+6(5),STSTN      CALL LETTERS TO FIND                         
*                                                                               
CON050   EQU   *                                                                
         GOTO1 HIGH                1ST AND SKIP READ                            
         B     CON120                                                           
*                                                                               
CON100   GOTO1 SEQ                 NEXT CONTRACT                                
*                                                                               
CON120   EQU   *                                                                
         CLC   KEY(11),KEYSAVE     = THRU CALL LETTERS?                         
         BE    CON130                                                           
*                                                                               
         SR    R0,R0               EXIT W/GOOD CC                               
         B     TESTEXIT                                                         
*                                                                               
CON130   EQU   *                                                                
*                                                                               
         GOTO1 GREC                                                             
*                                                                               
         CLI   QCONTYPE,C' '       CONTRACT TYPE REQUESTED?                     
         BE    CON140              NO  - NO TYPE CHECK                          
         TM    QCONTYPE,X'40'      UPPER OR LOWER CASE?                         
         BO    CON135              ON:  UPPER=INCLUDE TYPE                      
         CLC   QCONTYP2,RCONTYPE   OFF: LOWER=EXCLUDE TYPE                      
         BNE   CON140              NOT EQUAL - PROCESS IT                       
         B     CON100              EQUAL     - SKIP IT                          
CON135   EQU   *                                                                
         CLC   QCONTYPE,RCONTYPE   ON:  UPPER=INCLUDE TYPE                      
         BNE   CON100              NOT EQUAL - SKIP IT                          
*                                  EQUAL     - PROCESS IT                       
CON140   EQU   *                                                                
         XC    DOLLARS,DOLLARS                                                  
*                                                                               
CON200   EQU   *                   ACCUM THE ORDERED DOLLARS                    
         MVI   ELCODE,X'03'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   CON290              NO ORDERED ELEMENT                           
CON210   EQU   *                                                                
         BAS   RE,SETARAY1         TRY TO SET ORD ARRAY FLAG                    
         CLC   2(2,R6),PERIOD      FOR REPORT PERIOD?                           
         BE    CON230              YES -                                        
CON220   EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   CON290                                                           
         B     CON210                                                           
CON230   EQU   *                                                                
         MVI   STFLAG1,1                                                        
         ZICM  RE,6(R6)            GET THE DOLLARS                              
         L     RF,DOLLARS                                                       
         AR    RF,RE                                                            
         ST    RF,DOLLARS                                                       
         B     CON220                                                           
CON290   EQU   *                                                                
         ZICM  RE,DOLLARS,4                                                     
         L     RF,STNCOND                                                       
         AR    RF,RE                                                            
         ST    RF,STNCOND                                                       
         L     RF,STNCONC                                                       
         A     RF,ONE                                                           
         ST    RF,STNCONC                                                       
*                                                                               
CON300   EQU   *                   CHECK FOR ACTUALIZED/INVOICED                
         MVI   ELCODE,X'04'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   CON390              NO ACTUAL ELEMENT                            
CON310   EQU   *                                                                
         BAS   RE,SETARAY2         TRY TO SET INV ARRAY FLAG                    
         CLC   2(2,R6),PERIOD      FOR THIS PERIOD                              
         BNE   CON320              NO, GET NEXT                                 
         MVI   STFLAG2,1           TELL THE TABLE                               
*                                                                               
*   PROCESS ALL X'04' ELEMENTS TO SET MONTH ARRAY!!  ONLY TERMINATE             
*      PROCESSING IF 11 MONTHS PRIOR TO REPORT MONTH HAVE BEEN                  
*      FILLED.                                                                  
*                                                                               
         CLC   STMONS(24),=C'YYYYYYYYYYYYYYYYYYYYYYYY'                          
         BNE   CON320                                                           
         SR    R0,R0                                                            
         B     CONGOOD                                                          
CON320   EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   CON390                                                           
         B     CON310                                                           
CON390   EQU   *                                                                
         B     CON100                                                           
*                                                                               
*        READCON EXIT                                                           
*                                                                               
CONGOOD  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
CONBAD   EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- SETARAY1 --- APPLY 'ORDERED/03' BUCKET AGAINST MONTH ARRAY                   
*                                                                               
SETARAY1 NTR1                                                                   
         LA    RF,YRDATES          A(MONTH ARRAY TABLE, 1ST POSITION)           
         LA    RE,STMONS           A(MONTHLY FLAGS, 1ST POSITION)               
         LA    R1,12               LOOP CONTROL                                 
SAR10010 EQU   *                                                                
         CLC   2(2,R6),0(RF)       03 DATE FOUND IN ARRAY?                      
         BE    SAR10020            YES - SET FLAGS                              
         LA    RF,2(RF)            NO  - BUMP TO NEXT DATE                      
         LA    RE,2(RE)            BUMP TO NEXT INDICATORS                      
         BCT   R1,SAR10010         GO BACK FOR NEXT                             
         B     SAR10030            DATE NOT IN TABLE                            
SAR10020 EQU   *                                                                
         MVI   0(RE),C'Y'          SET 'CONTRACT FOUND'                         
SAR10030 EQU   *                                                                
         XIT1                      TEST COMPLETED                               
         SPACE 3                                                                
*                                                                               
*- SETARAY2 --- APPLY 'INVOICED/04' BUCKET AGAINST MONTH ARRAY                  
*                                                                               
SETARAY2 NTR1                                                                   
         LA    RF,YRDATES          A(MONTH ARRAY TABLE, 1ST POSITION)           
         LA    RE,STMONS           A(MONTHLY FLAGS, 1ST POSITION)               
         LA    R1,12               LOOP CONTROL                                 
SAR20010 EQU   *                                                                
         CLC   2(2,R6),0(RF)       04 DATE FOUND IN ARRAY?                      
         BE    SAR20020            YES - SET FLAGS                              
         LA    RF,2(RF)            NO  - BUMP TO NEXT DATE                      
         LA    RE,2(RE)            BUMP TO NEXT INDICATORS                      
         BCT   R1,SAR20010         GO BACK FOR NEXT                             
         B     SAR20030            DATE NOT IN TABLE                            
SAR20020 EQU   *                                                                
         MVI   1(RE),C'Y'          SET 'INVOICED FOUND'                         
SAR20030 EQU   *                                                                
         XIT1                      TEST COMPLETED                               
         EJECT                                                                  
*                                                                               
*- DODETAIL --- PRINT A DETAIL LINE                                             
*               ROLL STATION TOTALS                                             
*                                                                               
DODETAIL NTR1                                                                   
*                                                                               
*   START TEST                                                                  
*        MVC   P+1(07),=C'ARRAY: '                                              
*        MVC   P+10(24),STMONS                                                  
*        GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*   END   TEST                                                                  
*                                                                               
         MVI   STNGRPFL,C'Y'       SET 'STATIONS PRINTED'                       
         LA    R5,P                                                             
         USING LINE1D,R5                                                        
*                                                                               
         MVC   L1STN(4),STSTN                                                   
         CLI   STSTN+4,C' '                                                     
         BE    DDET60                                                           
         CLI   STSTN+4,0                                                        
         BE    DDET60                                                           
         LA    R6,L1STN+3                                                       
         CLI   0(R6),C' '                                                       
         BE    DDET50                                                           
         CLI   0(R6),0                                                          
         BE    DDET50                                                           
         LA    R6,1(R6)                                                         
DDET50   EQU   *                                                                
         MVI   0(R6),C'-'                                                       
         MVC   1(1,R6),STSTN+4     MOVE THE BAND                                
DDET60   EQU   *                                                                
*   START TEST                                                                  
*        MVC   P+1(07),=C'LEAVE= '                                              
*        MVC   P+10(24),STEND                                                   
*        GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*   END   TEST                                                                  
         CLI   STEND,0                                                          
         BE    DDET61                                                           
         GOTO1 DATCON,DMCB,(3,STEND),(5,L1LDATE)                                
DDET61   EQU   *                                                                
         MVC   L1MARKET,STMKT                                                   
         MVC   L1TWX,STTWX                                                      
         MVC   L1INT,STINT                                                      
*                                                                               
         CLI   STFAX,0             IF HEX00 IS INTERNATIONAL NUM                
         BE    DDETINTL                                                         
*                                                                               
         MVC   L1FAX,STFAX                                                      
         B     DDET69                                                           
DDETINTL EQU   *                                                                
         UNPK  INTLNUM(16),STFAX+3(8)                                           
         MVC   L1FAX(16),INTLNUM+1                                              
         EDIT  (1,STFAX+1),(2,L1FAX+1)                                          
         MVC   L1FAX+15(1),SPACES                                               
         CLC   STFAX+3(5),=X'0000000000'                                        
         BNE   DDET69                                                           
         MVC   L1FAX(16),SPACES                                                 
DDET69   EQU   *                                                                
         LA    R3,STMONS           DISPLAY MONTH ARRAY DATA                     
         LA    R2,L1MONS+1                                                      
         LA    RF,12               LOOP CONTROL                                 
DDET70   EQU   *                                                                
         CLI   0(R3),C'Y'          ORDERED IN MONTH?                            
         BNE   DDET80              NO                                           
         MVI   0(R2),C'O'          YES                                          
DDET80   EQU   *                                                                
         CLI   1(R3),C'Y'          INVOICED IN MONTH?                           
         BNE   DDET90              NO                                           
         MVI   1(R2),C'I'          YES                                          
DDET90   EQU   *                                                                
         LA    R3,2(R3)            BUMP TO NEXT MONTH                           
         LA    R2,4(R2)            BUMP TO NEXT PRINT POSITION                  
         BCT   RF,DDET70           GO BACK FOR NEXT                             
*                                                                               
         GOTO1 LOCALREP,DMCB,ALLTEXT YES                                        
         DROP  R5                                                               
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
         L     RF,GRPCONC                                                       
         A     RF,STNCONC                                                       
         ST    RF,GRPCONC                                                       
         L     RF,GRPCOND                                                       
         A     RF,STNCOND                                                       
         ST    RF,GRPCOND                                                       
*                                                                               
DDETGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- DOGROUP --- PRINT THE GROUP DATA LINE                                        
*                                                                               
DOGROUP  NTR1                                                                   
*                                                                               
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DOGR010             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DOGR010  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
*                                                                               
         MVC   P(17),=C'* FOR THE GROUP -'                                      
         MVC   P+18(10),GROUPN                                                  
         MVC   P+29(10),GROUPN+10                                               
         MVI   P+40,C'*'                                                        
*                                                                               
         GOTO1 TSARLINE,DMCB,1     ADD LINE TO TSAR BUFFER                      
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    DOGR020             YES - SKIP GRP/SUBGRP                        
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DOGR020             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DOGR020  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
*                                                                               
         GOTO1 DOTOT,DMCB,GRPSTN                                                
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,NUMCNTS                                                       
         LA    R3,GRPSTN                                                        
         LA    R4,RUNSTN                                                        
DOGR100  EQU   *                                                                
         L     RF,0(R4)                                                         
         A     RF,0(R3)                                                         
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,DOGR100                                                       
*                                                                               
         XC    GRPSTN(GRPCNTSL),GRPSTN                                          
*                                                                               
DOGRGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- DORUN --- PRINT THE RUN DATA LINE                                            
*                                                                               
DORUN    NTR1                                                                   
*                                                                               
         MVC   GROUPN(20),SPACES                                                
         MVC   GROUPN(10),=C'RUN TOTALS'                                        
*                                                                               
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DORN010             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DORN010  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
*                                                                               
         MVC   P(23),=C'** FOR THE TOTAL RUN **'                                
         GOTO1 TSARLINE,DMCB,1     ADD LINE TO TSAR BUFFER                      
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    DORN020             YES - SKIP GRP/SUBGRP DISP                   
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DORN020             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DORN020  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
*                                                                               
         GOTO1 DOTOT,DMCB,RUNSTN                                                
*                                                                               
DORNGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- DOTOT --- PRINT THE GROUP/SUBGROUP OR RUN TOTAL LINE                         
*                                                                               
*        P1 = A(TOTALS)                                                         
*                                                                               
DOTOT    NTR1                                                                   
*                                                                               
         L     R2,0(R1)            R2 -> TOTALS                                 
         LR    R4,R2               SAVE R2 IN R4 FOR 2ND LINE                   
         LA    R2,4(R2)            WE ARE NOT PRINTING 1ST NUMBER               
         LA    R3,P                                                             
*                                                                               
         MVC   0(L'TOTPT1,R3),TOTPT1                                            
         LA    R3,L'TOTPT1(R3)                                                  
*                                                                               
         MVC   0(L'TOTPT2,R3),TOTPT2                                            
         EDIT  (4,0(R2)),(5,(R3)),COMMAS=YES,ZERO=NOBLANK                       
         LA    R2,4(R2)                                                         
         LA    R3,L'TOTPT2(R3)                                                  
*                                                                               
         MVC   0(L'TOTPT3,R3),TOTPT3                                            
         EDIT  (4,0(R2)),(5,(R3)),COMMAS=YES,ZERO=NOBLANK                       
*                                                                               
         GOTO1 TSARLINE,DMCB,1     ADD LINE TO TSAR BUFFER                      
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    DTOT010             YES - SKIP GRP/SUBGRP DISP                   
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DTOT010             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DTOT010  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
*                                                                               
         ZICM  RF,4(R4),4           ANY STATIONS?                               
         BZ    DTOT160              NO  - BUT PUT OUT A TSARLINE                
*                                     FOR RECAP SYMMETRY                        
         MVC   P(L'TOTPT4),TOTPT4                                               
         ZICM  RF,8(R4),4           ANY UNCONVERTED STN?                        
         BNZ   DTOT100              YES                                         
         SR    R3,R3                                                            
         B     DTOT150                                                          
DTOT100  EQU   *                                                                
         CLC   4(4,R4),8(R4)                                                    
         BNE   DTOT110                                                          
         LA    R3,100                                                           
         B     DTOT150                                                          
DTOT110  EQU   *                                                                
         L     R2,8(R4)                                                         
         MH    R2,=H'100'                                                       
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         D     R2,4(R4)                                                         
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
DTOT150  EQU   *                                                                
         EDIT  (R3),(3,P),ZERO=NOBLANK                                          
DTOT160  EQU   *                                                                
         GOTO1 TSARLINE,DMCB,2     ADD LINE TO TSAR BUFFER                      
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    DTOT170             YES - SKIP GRP/SUBGRP DISP                   
         CLI   QOPTION1,C'Y'       DISPLAY GRP/SUBGRP CONV?                     
         BNE   DTOT170             NO                                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DTOT170  EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINTLINE IN CASE 'NO'                 
DTOT190  EQU   *                                                                
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        TSARLINE - ADDS PRINT LINE TO TSAR BUFFER PRIOR TO                     
*          PRINTING.  APPENDS A FULL-WORD COUNTER TO FRONT OF                   
*          LINE TO SERVE AS KEY.                                                
*                                                                               
TSARLINE NTR1                                                                   
         L     RF,0(R1)            GET SPACE COUNT                              
         STC   RF,BUFFREC+4        INSERT INTO BUFFREC                          
         MVC   BUFFREC+5(87),P     INSERT PRINTLINE                             
         L     RF,BUFFREC          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUFFREC          STORE IT BACK                                
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         LA    R0,BUFFREC                                                       
         ST    R0,TSAREC                                                        
         MVI   TSOFFACT,TSAADD     SET ACTION TO 'ADD'                          
         GOTO1 ATSAROFF            ADD RECORD TO BUFFER                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         XIT1                                                                   
         DS    0F                                                               
BUFFREC  DS    CL92                RECORD BUFFER                                
         ORG   BUFFREC                                                          
         DS    CL5                                                              
BUFFREC2 DS    CL87                                                             
         EJECT                                                                  
*                                                                               
*        DORECAP - RETURN ALL RECORDS WRITTEN TO TSAR BUFFER,                   
*           WRITE THEM OUT                                                      
*        RECAPS ARE COMPOSED OF THREE LINES:                                    
*           1ST:   GROUP NAME                                                   
*           2ND:   STATION COUNT AND UNCONVERTED COUNT                          
*           3RD:   PERCENT NOT CONVERTED                                        
*        ALL FIGURES ARE IN PRINTABLE FORM                                      
*                                                                               
DORECAP  NTR1                                                                   
         CLI   QOPTION2,C'Y'       DISPLAY RECAP?                               
         BNE   DORE0080            NO  - GET OUT                                
         MVI   RCSUBPRG,3          SET HEADING ROUTINE TO 'RECAP'               
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         XC    BUFFREC,BUFFREC                                                  
DORE0020 EQU   *                                                                
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    DORE0080            YES - FINISHED                               
         MVI   TSOFFACT,TSANXT     SET TO READ NEXT                             
         CLC   BUFFREC2+11(9),=C'TOTAL RUN'                                     
         BNE   DORE0030            NOT TOTALS                                   
         MVC   P+1(10),BUFFREC2+11                                              
         B     DORE0040                                                         
DORE0030 EQU   *                                                                
         MVC   P+1(10),BUFFREC2+29 MOVE GROUP TO PRINT                          
DORE0040 EQU   *                                                                
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    DORE0100            ERROR - DUMP IT                              
         MVC   P+18(3),BUFFREC2+13 MOVE STATION COUNT TO PRINT                  
         MVC   P+29(3),BUFFREC2+51 MOVE UNCONVERTED COUNT TO PRINT              
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    DORE0100            ERROR - DUMP IT                              
         MVC   P+39(4),BUFFREC2+1  MOVE PERCENT TO PRINT                        
         LR    R2,R1               SAVE R1: A(TSAROFF BLOCK)                    
         GOTO1 LOCALREP,DMCB,CAPTEXT                                            
         LR    R1,R2               RESET R1                                     
         B     DORE0020            GO BACK FOR NEXT LINE                        
DORE0080 EQU   *                                                                
*                                                                               
         DROP  R1                                                               
         XIT1                                                                   
*                                                                               
DORE0100 EQU   *                                                                
         DC    H'0'                DUMP IT OUT                                  
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF            ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF               A(SAVED REGISTERS)                           
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
*- LINE 3 - MONTH                                                               
*                                                                               
         LA    R2,HEAD3+48                                                      
         MVC   0(L'BCLABEL,R2),BCLABEL                                          
         LA    R2,L'BCLABEL(R2)                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(9,0(R2))                                 
*                                                                               
         CLI   RCSUBPRG,3          RECAP REPORT?                                
         BE    HHOOK020            YES - DON'T SET GROUP NAME                   
*- LINE 4.  LEFT=GROUP/SUB-GROUP                                                
*                                                                               
         MVC   HEAD4+1(6),=C'GROUP-'                                            
         MVC   HEAD4+7(10),GROUPN                                               
         MVC   HEAD4+18(10),GROUPN+10                                           
*                                                                               
*- LINE 6.  SET FOR CONTRACT TYPE PROCESSING, IF ENTERED                        
*                                                                               
         CLI   QCONTYPE,C' '       CONTRACT TYPE ENTERED?                       
         BE    HHOOK020            NO                                           
         MVC   HEAD6+68(1),QCONTYP2     SET TYPE INTO HEADING                   
*                                                                               
HHOOK020 EQU   *                                                                
         CLI   RCSUBPRG,3          RECAP REPORT?                                
         BE    HHOOKEXT            YES - DON'T SET UP MONTHS                    
         LA    R2,HEAD6+79         A(1ST DATE HEADING)                          
         LA    R3,HEAD7+79                                                      
         CLI   RCSUBPRG,0          BASIC REPORT?                                
         BE    HHOOK030            YES - USE HEADING LINES SET UP               
         LA    R2,HEAD8+79         A(1ST DATE HEADING)                          
         LA    R3,HEAD9+79                                                      
HHOOK030 EQU   *                                                                
         LA    R4,YRDATES                                                       
         XC    WORK(20),WORK       CLEAR WORK SPACE                             
         MVI   WORK+2,1            SET DAY TO 01                                
         LA    R5,12               LOOP CONTROL                                 
HHOOK040 EQU   *                                                                
         MVC   WORK(2),0(R4)       INSERT YYMM FROM TABLE                       
         GOTO1 DATCON,DMCB,(3,WORK),(5,WORK+3)                                  
         MVC   0(3,R2),WORK+3      MOVE MONTH TO HEADING 8                      
         MVC   0(2,R3),WORK+9      MOVE YEAR TO HEADING 9                       
         LA    R2,4(R2)            BUMP HEADINGS                                
         LA    R3,4(R3)                                                         
         LA    R4,2(R4)            BUMP DATE TABLE                              
         BCT   R5,HHOOK040         GO BACK FOR NEXT                             
*                                                                               
HHOOKEXT EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 5                                                                
*                                                                               
*        FHOOK --- FOOTHOOK ROUTINE                                             
*                                                                               
*FHOOK    NTR1                                                                  
*                                                                               
*         USING FHOOK,RF            ESTABLISH ADDRESSABILITY                    
*         LA    R1,SAVEREGS-FHOOK                                               
*         AR    R1,RF               A(SAVED REGISTERS)                          
*         LM    R2,RC,0(R1)                                                     
*         DROP  RF                                                              
*         MVC   FOOT1+68(1),QCONTYP2     SET TYPE INTO FOOTING                  
*                                                                               
*FHOOKEXT EQU   *                                                               
*         SR    R0,R0                                                           
*         B     TESTEXIT                                                        
         SPACE 5                                                                
SAVEREGS DS    11F                 REGS 2 THRU C FOR HEADHOOK                   
         EJECT                                                                  
*                                                                               
*        LOCALREP --- LOCAL REPORT INTERFACE                                    
*                                                                               
LOCALREP NTR1                                                                   
*                                                                               
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
*                                                                               
LREPGOOD EQU   *                                                                
         XIT1                                                                   
         SPACE 2                                                                
         DROP  R5                                                               
         SPACE 3                                                                
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'O',AL1(01),C'T',AL1(10),C'O',AL1(02),C'T',AL1(06)              
         DC    C'O',AL1(02),C'T',AL1(20),C'O',AL1(02),C'T',AL1(12)              
         DC    C'O',AL1(02),C'T',AL1(12),C'O',AL1(04)                           
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    C'T',AL1(03),C'O',AL1(02)                                        
         DC    X'0000'                                                          
CAPTEXT  DC    C'O',AL1(01),C'T',AL1(10)                                        
         DC    C'O',AL1(07),C'N',AL1(04)                                        
         DC    C'O',AL1(07),C'N',AL1(04)                                        
         DC    C'O',AL1(05),C'N',AL1(04)                                        
         DC    X'0000'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        COMMON ROUTINES                                                        
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 2                                                                
         GETEL R6,34,ELCODE                                                     
       ++INCLUDE RGENIO            DATA MANAGER INTERFACE                       
         EJECT                                                                  
*                                                                               
*        LOCAL CONSTANTS, ET AL                                                 
*                                                                               
ONE      DC    F'1'                                                             
RELO     DS    F                                                                
ATABLE   DS    A                   A(TABLE)                                     
ATABLEX  DS    A                   A(END OF THE TABLE)                          
QCONTYP2 DS    CL1                 ALTERNATE CONTRACT TYPE FLAG                 
INTLNUM  DS    CL16                TO HOLD THE UNPACKED INTL FAX NUM            
*                                                                               
OKMSG    DC    C'** END OF REPORT **'                                           
ERRMSG   DC    C'** ERRORS FOUND.  REPORT TERMINATED **'                        
TABMAX   DC    C'STATION TABLE OVERFLOW'                                        
UNDERLIN DC    10C'-'                                                           
TOTPT1   DC    C'THERE WERE '                                                   
TOTPT2   DC    C'9,999 STATIONS THAT HAD CONTRACTS AND '                        
TOTPT3   DC    C'9,999 STATIONS DID NOT CONVERT.'                               
TOTPT4   DC    C'999% OF YOUR STATIONS DID NOT CONVERT.'                        
NODATA   DC    C'NO STATIONS FOUND FOR THIS REQUEST.'                           
BCLABEL  DC    C'FOR THE BROADCAST MONTH OF '                                   
         SPACE 2                                                                
SETVAL01 DS    CL2                 FILTER VALUE OPTION3 SETS                    
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        START OF LOCAL WORK AREA                                               
*                                                                               
         DC    C'**COMMAND**'                                                   
COMMAND  DS    CL8                 FOR RGENIO                                   
AIOAREA  DS    A                   A(IO BUFFER)                                 
*                                                                               
DOLLARS  DS    F                   DOLLARS FOR CURRENT CONTRACT                 
*                                                                               
TABCOUNT DS    F                   COUNT IN THE TABLE                           
STNCONC  DS    F                   NUMBER CONTRACTS FOR THIS STATION            
STNCOND  DS    F                   ORDERED DOLLARS FOR THIS STATION             
*                                                                               
GRPCNTS  EQU   *                   COUNTS FOR THE GROUP                         
GRPSTN   DS    F                   STATIONS FOR THIS GROUP                      
GRPSTNW  DS    F                   STATIONS WITH CONTRACTS/GROUP                
GRPSTNN  DS    F                   STATIONS NOT CONVERTED/GROUP                 
GRPCONC  DS    F                   NUM CONTRACTS AT RISK                        
GRPCOND  DS    F                   ORDERED DOLLARS AT RISK                      
GRPCNTSX EQU   *                                                                
GRPCNTSL EQU   GRPCNTSX-GRPCNTS                                                 
*                                                                               
RUNCNTS  EQU   *                   COUNTS FOR THE RUN                           
RUNSTN   DS    F                   STATIONS FOR THE RUN                         
RUNSTNW  DS    F                   STATIONS WITH CONTRACTS/RUN                  
RUNSTNN  DS    F                   STATIONS NOT CONVERTED/RUN                   
RUNCONC  DS    F                   NUM CONTRACTS AT RISK                        
RUNCOND  DS    F                   ORDERED DOLLARS AT RISK                      
RUNCNTSX EQU   *                                                                
RUNCNTSL EQU   RUNCNTSX-RUNCNTS                                                 
*                                                                               
NUMCNTS  EQU   RUNCNTSL/4          NUMBER OF COUNTS                             
*                                                                               
TRACE    DS    X                   ^0 = TRACE MODE                              
*                                                                               
ELCODE   DS    X                   FOR GETEL                                    
*                                                                               
PERIOD   DS    XL3                 YMD START                                    
*                                                                               
         DC    C'*YRDATE*'                                                      
YRDATES  DS    12XL2               DATE ARRAY                                   
*                                                                               
GROUPN   DS    CL20                GROUP/SUB-GROUP NAME                         
STNGRPFL DS    CL1                 Y = STATIONS PRINTED                         
*                                                                               
*                                                                               
COMMANDX EQU   *                                                                
COMMANDL EQU   COMMANDX-COMMAND                                                 
*                                                                               
         DC    C'**IO**'                                                        
IOAREA   DS    6008X               IO BUFFER                                    
IOAREAX  EQU   *                                                                
IOAREAL  EQU   IOAREAX-IOAREA                                                   
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENGRP                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
*                                                                               
         ORG                                                                    
*                                                                               
VDATAMGR EQU   DATAMGR                                                          
*                                                                               
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
*                                                                               
*- STATION TABLE DSECT.                                                         
*                                                                               
STABLE   DSECT                                                                  
STENTRY  DS    0C                                                               
STKEY    EQU   *                                                                
STGROUP  DS    CL1          +00    GROUP CODE                                   
STSGROUP DS    CL1           01    SUB-GROUP CODE                               
STINTFCE DS    CL10          02                                                 
STSTN    DS    CL5           12    STATION CALLS                                
STKEYX   EQU   *                                                                
STLKEY   EQU   STKEYX-STKEY                                                     
STFLAG1  DS    CL1           17    1=CONTRACT FOUND FOR THIS STATION            
*                                     FOR THIS MONTH                            
STFLAG2  DS    CL1           18    1=CONVERTED/ACTUALIZED CONTRACT FND          
STMKT    DS    CL20          19    MARKET NAME (FROM STN RECORD)                
STTWX    DS    CL12          39    TWX #, PRINTED AS TELEPHONE #                
STFAX    DS    CL12          51    FAX #                                        
STINT    DS    CL10          63    INTERFACE #                                  
STMONS   DS    CL24          73    CONVERTED MONTH ARRAY                        
*                                  12 TWO-BYTE ENTRIES:                         
*                                     BYTE 1  =  CONTRACT FOR MONTH             
*                                     BYTE 2  =  CONVERTED/ACTUALIZED           
*                                                FOR MONTH                      
STEND    DS    CL3           97    LEAVE DATE IF ENTERED                        
*        --    ----          100   -- END OF TABLE --                           
*                                                                               
STLNTRY  EQU   *-STENTRY           ENTRY LENGTH                                 
         SPACE 2                                                                
*                                                                               
*- REPORT PRINT LINE DSECT                                                      
*                                                                               
LINE1D   DSECT                     LINE 1                                       
         DS    C                                                                
L1INT    DS    CL10                                                             
         DS    CL1                                                              
L1STN    DS    CL6                                                              
         DS    CL1                                                              
L1MARKET DS    CL19                                                             
         DS    CL1                                                              
L1LDATE  DS    CL8                                                              
         DS    CL1                                                              
L1TWX    DS    CL12                                                             
         DS    CL1                                                              
L1FAX    DS    CL16                                                             
         DS    CL1                                                              
**L1MONS DS    CL60                ORIG                                         
L1MONS   DS    CL48                                                             
*                                                                               
         SPACE 5                                                                
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE REXADDRD                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'149REREP1902 05/01/02'                                      
         END                                                                    
