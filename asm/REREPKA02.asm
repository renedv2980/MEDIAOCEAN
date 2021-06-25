*          DATA SET REREPKA02  AT LEVEL 084 AS OF 10/16/07                      
*PHASE REKA02A,*                                                                
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
         TITLE 'REREPKA02  (REKA02A) --- KATZ TAPE ACTUALIZATION'               
*                                                                               
********************************************************************            
*                                                                  *            
*                                                                  *            
*        REREPKA02  -- KATZ ACTUALIZATION:  INVOICE INFORMATION    *            
*                      FROM TAPE.                                  *            
*                      ACCEPT KATZ TAPE, UPDATE EXISTING CONTRACTS *            
*                      AND ADD 'NEW' CONTRACTS (ADDLINES)          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN24/97 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
* APR16/97 (BU ) --- ADD UP REJECTED DOLLARS FOR DISPLAY PURPOSES  *            
*                                                                  *            
* APR30/97 (BU ) --- USE DEFAULT VALUES FOR ADDLINES W/MISSING     *            
*                    AGENCY/AGENCY OFFICES                         *            
*                                                                  *            
* MAY13/97 (BU ) --- ADD 'RS' TO KRG COMPANIES.  MODIFY DISPLAYS.  *            
*                                                                  *            
* JUN10/97 (BU ) --- REARRANGE 'CLOSEOUT' PROCESSING               *            
*                                                                  *            
* APR10/98 (BU ) --- FIX KATZ LOW-POWER STATION SETUP              *            
*                                                                  *            
* SEP09/99 (BU ) --- BAD DATA:  YEAR ENTERED AS '19'. RESET TO     *            
*                    99.                                           *            
*                                                                  *            
* APR24/00 (BU ) --- ADD KATZ INTERACTIVE MEDIA                    *            
*                                                                  *            
* MAR20/01 (BU ) --- ADD KATZ DEDICATED   MEDIA                    *            
*                                                                  *            
* SEP25/01 (BU ) --- ADD KATZ WEST SIDE   MEDIA                    *            
*                                                                  *            
* JAN14/02 (BU ) --- ADD 1E ELEMENT TO RECORD                      *            
*                    IF CLEAR CHANNEL, INITIALIZE COMP S/P         *            
*                                                                  *            
* APR13/04 (BU ) --- FIX EOF POINTER SEARCH PROBLEM.  INITIALIZE   *            
*                    DMCBW3 VALUE                                  *            
*                                                                  *            
* OCT16/07 (BU ) --- FIX ERROR IN NEWORDER CODE: AGENCY /ADVERT    *            
*                    DEFAULT VALUE PROBLEM.                        *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*  TO DISPLAY A RANGE OF CONTRACTS BEING CONVERTED (SERIAL COUNT   *            
*     FROM X TO Y)                                                 *            
*                                                                  *            
*     QRECORD+20-25  =  SIX=DIGIT LOW-COUNTER VALUE FOR DISPLAYS   *            
*     QRECORD+26-31  =  SIX=DIGIT HI -COUNTER VALUE FOR DISPLAYS   *            
*                       BOTH VALUES MUST BE ENTERED FOR DISPLAYS   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =    Y   =   DISPLAY INPUT RECORDS               *            
*     QUESTOR+0   =    *   =   DISPLAY INPUT RECORDS (ADDLINES)    *            
*     QUESTOR+1   =    Y   =   DISPLAY OUTPUT RECORDS              *            
*     QUESTOR+2   =    Y   =   DISPLAY COUNT ON DEFAULT LISTING    *            
*     QUESTOR+3   =    Y   =   DISPLAY ALL NEWORDERS               *            
*     QUESTOR+4   =    Y   =   DISPLAY NEXT CON# BY COMPANY        *            
*     QUESTOR+5   =    Y   =   DISPLAY STATION RECORDS             *            
*     QUESTOR+6   =    Y   =   DISPLAY SHORT 'NOT ON FILE'         *            
*     QUESTOR+10  =    Y   =   DISPLAY DEFAULT ERROR RECORDS       *            
*     QUESTOR+11  =    Y   =   DISPLAY PERFORMANCE TIME STAMP      *            
*                                                                  *            
*                                                                  *            
*  QOPTION1   =   COMPANY DESIGNATION: R = KATZ RADIO              *            
*                                      T = KATZ TELEVISION         *            
*                                      S = SELTEL                  *            
*  QOPTION2   =   DIRECT/MERGE FLAG    D = DIRECT UPDATE (DEFAULT) *            
*                                      M = TAPE O/P TO BE MERGED   *            
*  QOPTION3   =   UPDATE/TEST FLAG     U = UPDATE FILE             *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKA02   CSECT                                                                  
         NMOD1 0,**REKA**,R8,R9,RR=R5                                           
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
MAIN0040 EQU   *                                                                
         L     R5,ARECAREA                                                      
*                                                                               
         USING KCONTAPE,R5         SET DSECT FOR CONTRACT TAPE                  
*                                                                               
         GET   INTAPE,(R5)         READ TAPE RECORD INTO RDA                    
*                                                                               
         L     RF,REDCTR           INCREMENT RECORD COUNTER                     
         LA    RF,1(RF)                                                         
         ST    RF,REDCTR                                                        
*                                                                               
**       C     RF,=F'15800'        SKIP FIRST N RECORDS                         
**       BNH   MAIN0040                                                         
**       C     RF,=F'500'          END AFTER N RECORDS                          
**       BH    MAIN0200                                                         
*                                                                               
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
*                                                                               
         CLI   QUESTOR+11,C'Y'     DISPLAY PERFORMANCE TIME STAMP?              
         BNE   MAIN0060            NO                                           
*                                                                               
         C     RF,=F'1000'         DISPLAY EVERY N RECORDS                      
         BNE   MAIN0060                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  REDCTR,(10,P+15)    DISPLAY TOTAL COUNTER                        
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+55,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
*                                                                               
MAIN0060 EQU   *                                                                
         XC    REPUSE,REPUSE       CLEAR TEST REP TO USE                        
         CLI   QOPTION1,C'R'       KATZ RADIO RUN?                              
         BNE   MAIN0080            NO                                           
         MVC   RUNID,=C'KRGNY'     SAVE RUN ID                                  
         MVC   POWERCDE,=C'K3'     SET POWER CODE                               
         LA    R3,RADIODIV         YES                                          
         B     MAIN0140                                                         
MAIN0080 EQU   *                                                                
         CLI   QOPTION1,C'T'       KATZ TV    RUN?                              
         BNE   MAIN0100            NO                                           
         MVC   RUNID,=C'KTVNY'     SAVE RUN ID                                  
         MVC   POWERCDE,=C'MR'     SET POWER CODE                               
         LA    R3,TVDIV            YES                                          
         B     MAIN0140                                                         
MAIN0100 EQU   *                                                                
         CLI   QOPTION1,C'S'       SELTEL     RUN?                              
         BNE   MAIN0120            NO                                           
         MVC   RUNID,=C'SELNY'     SAVE RUN ID                                  
         MVC   POWERCDE,=C'SZ'     SET POWER CODE                               
         LA    R3,SELDIV           YES                                          
         B     MAIN0140                                                         
MAIN0120 EQU   *                                                                
         CLI   QOPTION1,C'X'       TEST RUN - TTV5/V5?                          
         BNE   MAIN0130            NO                                           
         MVC   RUNID,=C'TTV5 '     SAVE RUN ID                                  
         MVC   POWERCDE,=C'V5'     SET POWER CODE                               
         LA    R3,TESTDIV          YES                                          
         B     MAIN0140                                                         
MAIN0130 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED COMPANY                         
MAIN0140 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE REACHED?                        
         BNE   MAIN0160            NO  -                                        
         MVC   P+1(22),=C'UNRECOGNIZED DIVISION:'                               
         GOTO1 REPORT                                                           
         GOTO1 =A(ERRIPUT),DMCB,(RC),RR=Y                                       
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0160 EQU   *                                                                
         CLC   0(2,R3),KCONTREP    RECORD DIVISION IN TABLE?                    
         BE    MAIN0180            YES                                          
*                                                                               
         LA    R3,LDIVSET(R3)      NO - BUMP TO NEXT ENTRY                      
         B     MAIN0140            GO BACK FOR NEXT                             
MAIN0180 EQU   *                                                                
*                                                                               
         CLC   2(2,R3),=C'**'      DON'T PROCESS DIVISION INDICATOR?            
         BNE   MAIN0190            NO  - PROCESS THIS RECORD                    
         L     RF,STARCTR          YES - COUNT IT UP                            
         LA    RF,1(RF)                                                         
         ST    RF,STARCTR                                                       
         B     MAIN0040            GO BACK FOR NEXT RECORD                      
MAIN0190 EQU   *                                                                
         MVC   REPUSE,2(R3)        SAVE REP CODE TO USE                         
         GOTO1 CONTPROC,DMCB,(RC)                                               
*                                  PROCESS CONTRACT RECORD                      
         B     MAIN0040            GO BACK FOR NEXT RECORD                      
*                                                                               
MAIN0200 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=Y                                      
*                                  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   MAIN0240            NO  - DIRECT UPDATE                          
         CLOSE FILOUTA             YES - CLOSE OUTPUT FILES                     
         CLOSE FILOUTB                                                          
MAIN0240 EQU   *                                                                
         CLOSE (INTAPE,REWIND)                                                  
*                                                                               
MAIN0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    DIVISION TABLES:  KATZ RADIO/TV/SELTEL.  THESE TABLES EXIST                
*        TO PERMIT VALIDATION OF INCOMING DIVISION DESIGNATIONS.                
*    CHARS 1  -  2  =  VALIDATION REP CODE                                      
*    CHARS 3  -  4  =  REP CODE FOR LOOK-UPS:  TO FACILITATE USING              
*        PRODUCTION TAPES FROM KATZ FOR REFERENCING DDS TEST FILES              
*                                                                               
RADIODIV DC    C'BFBF'             BANNER                                       
LDIVSET  EQU   *-RADIODIV                                                       
         DC    C'CRCR'             CHRISTAL                                     
         DC    C'EAEA'             EASTMAN                                      
         DC    C'KFKF'             KATZ HISPANIC                                
         DC    C'KUKU'             KATZ RADIO                                   
         DC    C'S3S3'             SENTRY                                       
         DC    C'K4K4'             SYNDICATION                                  
         DC    C'K6K6'             DIMENSIONS(NOTNY)                            
         DC    C'RSRS'             ABC                                          
         DC    C'QDQD'             SPORTS SPECTRUM                              
         DC    C'NUNU'             CLEAR CHANNEL                                
         DC    C'G8G8'             INTERACTIVE MEDIA                            
         DC    C'J0J0'             DEDICATED   MEDIA                            
         DC    C'WCWC'             WEST SIDE   MEDIA                            
         DC    X'0000'                                                          
*                                  KATZ TELEVISION                              
TVDIV    DC    C'AMAM'             AMERICAN                                     
         DC    C'CQCQ'             CONTINENTAL                                  
         DC    C'NKNK'             NATIONAL                                     
         DC    C'8K8K'             SYNDICATED                                   
         DC    X'0000'                                                          
*                                  SELTEL                                       
SELDIV   DC    C'SZSZ'             SELTEL DOMESTIC                              
         DC    C'S2S2'             SELTEL INTERNATIONAL                         
         DC    X'0000'                                                          
*                                  SELTEL INTERNATIONAL/TTV5-V5                 
TESTDIV  DC    C'S2**'             SELINT (STRIPPED FOR TESTING)                
         DC    C'SZ**'             SELNY: SKIP ALL RECORDS                      
         DC    C'BF**'             BANNER                                       
         DC    C'CR**'             CHRISTAL                                     
         DC    C'EAV5'             EASTMAN                                      
         DC    C'KF**'             KATZ HISPANIC                                
         DC    C'KU**'             KATZ RADIO                                   
         DC    C'S3**'             SENTRY                                       
         DC    C'K4**'             SYNDICATION                                  
         DC    C'K6**'             DIMENSIONS(NOTNY)                            
         DC    C'RS**'             ABC                                          
         DC    X'0000'                                                          
         EJECT                                                                  
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
         L     RF,ARECAREA                                                      
         A     RF,=F'30000'        TAPE BUFFER AREA                             
         ST    RF,ASTAAREA         SET A(STATION AREA)                          
         ST    RF,ASTANEXT         SET A(NEXT AVAILABLE SLOT)                   
*                                                                               
         CLC   QRECORD+20(12),SPACES                                            
*                                  ANY DISPLAY VALUES?                          
         BE    INIT0060            NO                                           
         PACK  DUB,QRECORD+20(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,LOWCTR           SAVE LOW VALUE                               
         PACK  DUB,QRECORD+26(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,HIGHCTR          SAVE HIGH VALUE                              
INIT0060 EQU   *                                                                
*                                  DETERMINE MONDAY ACTIVITY DATE               
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK+6)                                  
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
         CLI   DMCB,1              MONDAY DATE?                                 
         BE    INIT0080            YES - USE DATE AS IS                         
         ZIC   RF,DMCB             NO  - GET DAY OF WEEK                        
         BCTR  RF,0                SUBTRACT 1 FOR ADJUSTMENT                    
         LNR   RF,RF               NEGATE THE VALUE                             
         ST    RF,DMCB+8           PUT INTO PARAM 3                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12                                        
         MVC   WORK+6(6),WORK+12                                                
INIT0080 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,ACTVDATE)                              
*                                  CALCULATE 04 INVOICE BUCKET                  
*                                     ACTIVITY DATE BASED ON TODAY              
*                                                                               
INIT0120 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  RETRIEVE EACH TAPE RECORD.                         *              
*      FOR EXISTING ORDERS, UPDATE INVOICE INFORMATION.          *              
*      FOR ADDLINES, GENERATE A NEW CONTRACT (AND PASSIVES?)     *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XCEFL RCONREC,2048        CLEAR OUT THE CONTRACT RECORD                
*                                                                               
         CLI   QUESTOR+0,C'Y'      DISPLAY INPUT RECORD?                        
         BE    CPRO0010            YES                                          
         CLI   QUESTOR+0,C'*'      DISPLAY INPUT RECORD (ADDLINE)?              
         BNE   CPRO0020            NO                                           
         CLC   =C'NEW',KCONKCON    YES - ADDLINE?                               
         BNE   CPRO0020            NO                                           
CPRO0010 EQU   *                                                                
         GOTO1 =A(READIPUT),DMCB,(RC),RR=Y                                      
CPRO0020 EQU   *                                                                
         CLC   =C'NEW',KCONKCON    ADDLINE?                                     
         BE    CPRO0060            YES                                          
         GOTO1 OLDORDER            NO  - UPDATE EXISTING ORDER                  
         B     CPRO0100            EXIT                                         
CPRO0060 EQU   *                                                                
         GOTO1 NEWORDER            NO  - CREATE ADDLINE ORDER                   
CPRO0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   OLDORDER:  RETRIEVE AN OLD ORDER FROM FILE, AND ADD THE INVOICE             
*        BUCKET FOR THE MONTH SPECIFIED.                                        
*                                                                               
OLDORDER NTR1                                                                   
*                                                                               
*   SKIP NEXT TESTS (TESTING DISPLAYS)                                          
*                                                                               
         B     OLDR0002                                                         
*                                                                               
*   QUICK-SKIP                                                                  
*                                                                               
         CLC   KCONTREP(2),=C'AM'                                               
         BE    OLDR0000            SCAN FOR AMERICAN ORDERS                     
         CLC   KCONTREP(2),=C'CQ'                                               
         BE    OLDR0001            SCAN FOR CONTINENTAL ORDERS                  
         B     OLDR0900            SKIP EVERYTHING ELSE                         
OLDR0000 EQU   *                   FILTER AMERICAN ORDERS                       
         CLC   KCONKCON(8),=C'04287433'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04291327'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04288441'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04276930'                                         
         BE    OLDR0002                                                         
         B     OLDR0900            SKIP EVERYTHING ELSE                         
OLDR0001 EQU   *                   FILTER CONTINENTAL ORDERS                    
         CLC   KCONKCON(8),=C'04327220'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04301184'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04319986'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04346691'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04287399'                                         
         BE    OLDR0002                                                         
         CLC   KCONKCON(8),=C'04321727'                                         
         BE    OLDR0002                                                         
         B     OLDR0900            SKIP EVERYTHING ELSE                         
OLDR0002 EQU   *                                                                
*   QUICK-SKIP END                                                              
*                                                                               
         CLI   QUESTOR+0,C'?'      DISPLAY INPUT RECORD?                        
         BNE   OLDR0003            YES                                          
         GOTO1 =A(READIPUT),DMCB,(RC),RR=Y                                      
OLDR0003 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
         MVC   KEY+21(2),REPUSE    INSERT REP CODE                              
         GOTO1 =V(HEXIN),DMCB,KCONKCON,WORK+32,8,=C'TOG'                        
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),WORK+32(4)                                            
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT COMP CON# INTO KEY                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND ON FILE?                      
         BE    OLDR0040            YES                                          
         MVC   P+1(18),=C'ORDER NOT ON FILE:'                                   
         CLI   QUESTOR+6,C'Y'      SHORT DISPLAY?                               
         BNE   OLDR0030            NO                                           
         MVC   P+25(20),KCONTREP   YES - SHOW REP/CON# ONLY                     
OLDR0030 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 =A(ERRIPUT),DMCB,(RC),RR=Y                                       
         B     OLDR0900            EXIT                                         
OLDR0040 EQU   *                                                                
         GOTO1 GREC                RETRIEVE RECORD                              
         MVC   WORK(2),KCONRFLT+4  SET YEAR FIRST                               
         MVC   WORK+2(4),KCONRFLT  SET MONTH/DAY LAST                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
*                                  CONVERT DATE TO BINARY FORMAT                
         MVC   FLTSTART,WORK+6     SAVE START DATE FOR TABLING                  
         MVC   NEW04ELT+NEWSVDAT(2),WORK+6                                      
*                                  INSERT SERVICE MONTH INTO NEW ELT            
         MVC   NEW04ELT+NEWACDAT(2),ACTVDATE                                    
*                                  INSERT ACTIVITY DATE                         
         ZIC   RF,WORK+7           GET MONTH NUMBER FROM DATE FIELD             
         BCTR  RF,0                MAKE MONTH ZERO RELATIVE                     
         MH    RF,=H'12'           * 12 TO DISPLACE TO MONTH'S DATA             
         LA    R2,KCONDOLS         A(1ST MONTH'S DATA)                          
         AR    R2,RF               DISPLACE TO MONTH'S DATA                     
         MVC   WORK(12),0(R2)      MOVE FIELD TO WORK AREA                      
         GOTO1 =V(CASHVAL),DMCB,(2,WORK),12,0,RR=Y                              
         CLI   DMCB,X'FF'          ERROR ON VALUE?                              
         BNE   OLDR0080            NO  - ACCEPTED AS IS                         
         MVC   P+1(20),=C'MONTHLY DATA INVALID'                                 
         GOTO1 REPORT                                                           
         GOTO1 =A(ERRIPUT),DMCB,(RC),RR=Y                                       
         B     OLDR0900            EXIT                                         
OLDR0080 EQU   *                                                                
         MVC   NEW04ELT+NEWDOLRS(4),DMCB+4                                      
         L     RF,DMCB+4           ACCUMULATE RUN TOTAL DOLLARS                 
         CVD   RF,WORK+24          CONVERT DOLLARS TO PACKED                    
         AP    TOTDOLRS(8),WORK+24(8)                                           
         CLI   QUESTOR+0,C'$'      DISPLAY INPUT/DOLLAR COUNT?                  
         BNE   OLDR0100            NO                                           
         CLC   REDCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    OLDR0100                                                         
         CLC   REDCTR,HIGHCTR                                                   
         BH    OLDR0100                                                         
         MVC   P+1(07),=C'OLD $$='                                              
         EDIT  (4,DMCB+4),(15,P+10),2,COMMAS=YES,CR=YES                         
         EDIT  (P8,TOTDOLRS),(15,P+30),2,COMMAS=YES,CR=YES                      
         GOTO1 REPORT                                                           
OLDR0100 EQU   *                                                                
*                                                                               
*   DATA UPDATE WILL BE ACCOMPLISHED, OPTIONALLY, IN ONE OF TWO WAYS:           
*        1.  DATA SENT TO TAPE FILES (QOPTION2 = M)                             
*            A.  REVISED/ADDED DATA WILL BE MERGED AT LOAD TIME                 
*            B.  ORIGINAL DATA AFFECTED WILL BE MARKED AS 'DELETED'             
*                DURING PROCESSING, AND REWRITTEN TO FILE.                      
*        2.  DATA UPDATED DIRECTLY  (QOPTION2 = D OR DEFAULT)                   
*            A.  REVISED RECORDS WILL BE REWRITTEN TO FILE, PROBABLY            
*                TO OVERFLOW.                                                   
*            B.  NEW RECORDS WILL BE ADDED TO FILE, DEFINITELY TO O/F           
*                                                                               
         CLI   QOPTION2,C'M'       O/P TO TAPE FILE?                            
         BNE   OLDR0120            NO  - NO NEED TO SAVE ORIGINAL               
         LA    RF,RECORD3          SAVE ORIGINAL RECORD IN RECORD3              
         LA    R1,2000                FOR FILE-MARKING AS 'DELETED'             
         LA    RE,RCONREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
OLDR0120 EQU   *                                                                
         LA    R6,RCONREC          DELETE OLD 04 ELTS FOR MONTH                 
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNZ   OLDR0280            NO ELT FOUND - SKIP DELETE                   
         B     OLDR0200                                                         
OLDR0160 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNZ   OLDR0240            NO ELT FOUND                                 
OLDR0200 EQU   *                                                                
         CLC   0(4,R6),NEW04ELT    ELT CODE/LEN/DATE = NEW ELT?                 
         BNE   OLDR0160            NO  - LEAVE ALONE                            
         MVI   0(R6),X'FF'         YES - SET FOR DELETE                         
         B     OLDR0160                                                         
OLDR0240 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DELETE OLD INVOICE ELTS FOR MONTH            
OLDR0280 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW04ELT,0             
*                                  INSERT INVOICE ELT FOR MONTH                 
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK)                                    
*                                  GET TODAY'S DATE                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'27'        LOOK FOR 'ACTUAL HIST ELT'                   
         BAS   RE,GETEL                                                         
         BNZ   OLDR0320            NOT FOUND - MUST BE ADDED                    
*                                                                               
         MVC   2(2,R6),NEW04ELT+2  INSERT MONTH OF SERVICE INTO ELT             
         MVC   4(2,R6),WORK        INSERT YMD OF TODAY'S DATE                   
*                                     AS DATE OF ACTUALIZATION                  
         B     OLDR0360                                                         
OLDR0320 EQU   *                                                                
*                                                                               
*   NO 27 ELT IN RECORD:  BUILD IT AND INSERT IT                                
*                                                                               
         MVC   NEW27ELT+2(2),NEW04ELT+2                                         
*                                  INSERT MONTH OF SERVICE INTO ELT             
         MVC   NEW27ELT+4(2),WORK  INSERT YMD OF TODAY'S DATE                   
*                                     AS DATE OF ACTUALIZATION                  
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW27ELT,0             
*                                  INSERT HISTORY ELT FOR MONTH                 
OLDR0360 EQU   *                                                                
         XC    NEW04ELT+2(8),NEW04ELT                                           
*                                  CLEAR THE NEW 04 ELT                         
         CLI   QOPTION2,C'M'       O/P TO TAPE FILE?                            
         BNE   OLDR0520            NO  - DIRECT UPDATE                          
         LA    RF,REC              YES - PUT RECORD TO TAPE FILE                
         LA    R1,2000                                                          
         LA    RE,RCONREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECS             GENERATE THE OUTPUT RECORDS FOR CON          
         L     RF,CONCTR           INCREMENT CONTRACTS O/P                      
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT RECORDS                       
         BNE   OLDR0400            NO                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
*                                  YES - DISPLAY RECORD JUST PUT                
OLDR0400 EQU   *                                                                
         LA    RF,RCONREC          RESTORE ORIGINAL RECORD IN RCONREC           
         LA    R1,2000                                                          
         LA    RE,RECORD3                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                     ADDED X'04' ELEMENT FOR REWRITE           
         OI    RCONCNTL,X'80'      SET DELETE BIT ON ORIGINAL ORDER             
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT RECORDS                       
         BNE   OLDR0440            NO                                           
         GOTO1 DISPCON,DMCB,(RC),(0,0)                                          
*                                  DISPLAY ORIG/DELETED RECORD                  
*                                     FROM CONTRACT RECORD                      
OLDR0440 EQU   *                                                                
         CLI   QOPTION3,C'U'       UPDATE REQUEST?                              
         BNE   OLDR0480            NO  - DON'T REWRITE REC AS DELETED           
         GOTO1 PREC                YES - REWRITE RECORD IN IO AREA              
OLDR0480 EQU   *                                                                
         GOTO1 =A(CLOSEOUT),DMCB,(RC),RR=Y                                      
*                                  CHECK CLOSEOUT DATE TABLE                    
         B     OLDR0900            EXIT ROUTINE                                 
OLDR0520 EQU   *                                                                
         L     RF,CONCTR           INCREMENT CONTRACTS O/P                      
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT RECORDS                       
         BNE   OLDR0560            NO                                           
         GOTO1 DISPCON,DMCB,(RC),(1,0)                                          
*                                  DISPLAY UPDATED ORIGINAL RECORD              
*                                     FROM CONTRACT RECORD                      
OLDR0560 EQU   *                                                                
         CLI   QOPTION3,C'U'       UPDATE REQUEST?                              
         BNE   OLDR0600            NO  - DON'T REWRITE REC AS DELETED           
         GOTO1 PREC                YES - REWRITE RECORD IN IO AREA              
OLDR0600 EQU   *                                                                
         GOTO1 =A(CLOSEOUT),DMCB,(RC),RR=Y                                      
*                                  CHECK CLOSEOUT DATE TABLE                    
         B     OLDR0900            EXIT ROUTINE                                 
OLDR0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*                .0.1.2.3.4.5.6.7.8.9.                                          
NEW04ELT DC    X'040A0000000000000000'                                          
NEW27ELT DC    X'270600000000'                                                  
*                                                                               
NEWSVDAT EQU   2                   DISPLACEMENT TO SERVICE DATE                 
NEWACDAT EQU   4                   DISPLACEMENT TO ACTIVITY DATE                
NEWDOLRS EQU   6                   DISPLACEMENT TO AMOUNT FIELD                 
*                                                                               
*   NEWORDER:  GENERATE A NEW CONTRACT FROM THE INFORMATION SUPPLIED            
*        IN THIS RECORD.                                                        
*                                                                               
NEWORDER NTR1                                                                   
*                                                                               
         L     RF,NEWORDRS         INCREMENT NEW ORDER COUNTER                  
         LA    RF,1(RF)                                                         
         ST    RF,NEWORDRS                                                      
*                                                                               
         MVI   RCONKTYP,X'0C'      INSERT RECORD TYPE                           
         MVC   RCONKREP,REPUSE     INSERT REP CODE                              
*                                                                               
         GOTO1 =A(VALORDER),DMCB,(RC),RR=Y                                      
*                                  VALIDATE ORDER'S FIELDS                      
*                                     AND LOAD KEY PORTION OF RECORD            
*                                        WHERE APPLICABLE                       
*                                     IF ERROR, RETURN CC NOT ZERO              
         BNZ   NORD0560            EXIT ERROR                                   
*                                                                               
*   WHAT SHOULD DEFAULTS BE?                                                    
*                                                                               
         MVI   ACEGRAPH,X'40'      DEFAULT STATION TO 'GRAPH'                   
*                                                                               
         MVC   RCONLEN,=X'005E'    SET INITIAL LENGTH OF RECORD                 
         MVC   RCONELEM(2),=X'013C'                                             
*                                  SET DESCRIP ELT CODE/LEN                     
         MVC   RCONBUYR,SPACES                                                  
         MVC   RCONBUYR(4),=C'ACC-'                                             
         MVC   RCONBUYR+4(16),KCONRACC                                          
*                                  INSERT ACCOUNT PERSON'S NAME                 
*                                     LEAVE LOWER CASE CHARS                    
*                                                                               
         MVI   RCONMOD,X'FF'       SET 'UNCONFIRMED'                            
         MVC   RCONMODR+1(1),ACGREASY                                           
*                                  INSERT ACE/GRAPH/OTHER INDICATOR             
         OC    ELTBILD3,ELTBILD3   ANY PRODUCT X'05' ELEMENT?                   
         BZ    NORD0020            NO                                           
         GOTO1 HELOCON3            YES - ADD ELEMENT TO RECORD                  
*                                                                               
NORD0020 EQU   *                                                                
         XC    ELTBILD1,ELTBILD1   CLEAR 1ST ELEMENT BUILD AREA                 
         LA    R6,ELTBILD1                                                      
         USING RCONSEND,R6                                                      
*                                                                               
         MVI   ELTBILD1,X'20'      SET ELEMENT CODE                             
         MVI   ELTBILD1+1,X'29'    SET LENGTH                                   
*                                                                               
         OI    RCONSENF,X'10'      ALWAYS 'STATION VERSION NOT ADV'D'           
         MVI   RCONSRV,1           INSERT REP VERSION NUMBER                    
*                                     WILL ALWAYS BE 1                          
***>>>   GOTO1 DATCON,DMCB,(5,WORK),(2,RCONSRDT)                                
*   NOT SENT:                      USE TODAY'S DATE AS DATE SENT                
*                                                                               
***>>>   MVC   RCONSRTI,=C'120000'                                              
*   NOT SENT:                      FORCE TIME SENT VALUE                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,ELTBILD2                                                      
         USING RCONXEL,R6                                                       
*                                                                               
         XC    ELTBILD2,ELTBILD2   CLEAR 2ND ELEMENT BUILD AREA                 
         MVI   ELTBILD2,X'1F'      INSERT CODE                                  
         MVI   ELTBILD2+1,24       INSERT LENGTH                                
*                                                                               
         OI    RCONCONF,X'80'      SET 'NOT CONFIRMED' BIT                      
         GOTO1 HELOCON1                                                         
*                                  INSERT X'20' ELT INTO RECORD                 
         DROP  R6                                                               
         GOTO1 HELOCON2                                                         
*                                  INSERT X'1F' ELT INTO RECORD                 
         LA    R6,ELTBILD4                                                      
*                                                                               
         XC    ELTBILD4,ELTBILD4   CLEAR 4TH ELEMENT BUILD AREA                 
         MVI   ELTBILD4,X'1E'      INSERT CODE                                  
         MVI   ELTBILD4+1,22       INSERT LENGTH                                
         CLC   =C'NU',RCONKREP     CLEAR CHANNEL ORDER?                         
         BNE   NORD0040            NO  - INSERT AS IS                           
         MVC   RCONRPSP-RCONRFEL(3,R6),RCONSAL                                  
*                                  YES - INSERT S/P AS COMP S/P                 
         MVC   RCONRSPO-RCONRFEL(2,R6),RCONKOFF                                 
*                                  INSERT S/P OFFICE                            
NORD0040 EQU   *                                                                
*                                                                               
         GOTO1 HELOCON4                                                         
*                                  INSERT X'1E' ELT INTO RECORD                 
*                                                                               
         CLI   RCONMOD,X'FF'       CONFIRMED ORDER?                             
         BE    NORD0080            NO  - NO MOD DATE                            
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+6)                                  
         MVC   RCONMODD,WORK+6     INSERT LAST MOD DATE                         
NORD0080 EQU   *                                                                
****>>>  OI    RCONMODR+1,X'01'    SET 'KATZ HOUSE ACCOUNT'                     
         GOTO1 DATCON,DMCB,(5,WORK2),(0,WORK2)                                  
*                                  SET UP EBCDIC DATE                           
         GOTO1 DATCON,DMCB,(5,WORK2),(3,WORK2+6)                                
*                                  USE TODAY'S DATE FOR DATES                   
         MVC   RCONCREA,WORK2+6    INSERT CREATE/1ST BUY DATE                   
         MVC   RCONHDRD,WORK2+6    INSERT HEADER CREATION DATE                  
*                                     SAME AS CREATE DATE HERE...               
         GOTO1 GETDAY,DMCB,(0,WORK2),WORK2+6                                    
         CLI   DMCB,1              MONDAY?                                      
         BE    NORD0120            YES - USE AS IS                              
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                BACK UP 1 DAY                                
         LNR   RF,RF                                                            
         ST    RF,DMCB+8           PUT INTO PAR 3                               
*                                  USE TODAY'S DATE FOR DATES                   
         GOTO1 ADDAY,DMCB,WORK2,WORK2+6,,                                       
         MVC   WORK2(6),WORK2+6    RESET NEW DATE                               
NORD0120 EQU   *                                                                
*                                                                               
         MVI   RCONRTGS,C'A'       INSERT RATING SERVICE - FORCE                
*                                     TO ARBITRON                               
         MVI   RCONWKS,1           INSERT 1 WEEK OF SCHEDULE                    
*                                                                               
*   SET UP X'04' ELEMENT FROM RECORD                                            
*                                                                               
         MVC   WORK2(2),KCONRFLT+4 SET YEAR FIRST                               
         MVC   WORK2+2(4),KCONRFLT SET MONTH/DAY LAST                           
         GOTO1 DATCON,DMCB,(0,WORK2),(3,WORK2+6)                                
*                                  CONVERT DATE TO BINARY FORMAT                
         MVC   NEW04ELT+NEWSVDAT(2),WORK2+6                                     
*                                  INSERT SERVICE MONTH INTO NEW ELT            
         MVC   NEW04ELT+NEWACDAT(2),ACTVDATE                                    
*                                  INSERT ACTIVITY DATE                         
         ZIC   RF,WORK2+7          GET MONTH NUMBER FROM DATE FIELD             
         BCTR  RF,0                MAKE MONTH ZERO RELATIVE                     
         MH    RF,=H'12'           * 12 TO DISPLACE TO MONTH'S DATA             
         LA    R2,KCONDOLS         A(1ST MONTH'S DATA)                          
         AR    R2,RF               DISPLACE TO MONTH'S DATA                     
         MVC   WORK2(12),0(R2)      MOVE FIELD TO WORK AREA                     
         GOTO1 =V(CASHVAL),DMCB,(2,WORK2),12,0,RR=Y                             
         CLI   DMCB,X'FF'          ERROR ON VALUE?                              
         BNE   NORD0160            NO  - ACCEPTED AS IS                         
         MVC   P+1(20),=C'MONTHLY DATA INVALID'                                 
         GOTO1 REPORT                                                           
         GOTO1 =A(ERRIPUT),DMCB,(RC),RR=Y                                       
         B     NORD0560            EXIT                                         
NORD0160 EQU   *                                                                
         MVC   NEW04ELT+NEWDOLRS(4),DMCB+4                                      
         L     RF,DMCB+4           ACCUMULATE RUN TOTAL DOLLARS                 
         CVD   RF,WORK+24          CONVERT DOLLARS TO PACKED                    
         AP    TOTDOLRS(8),WORK+24(8)                                           
         CLI   QUESTOR+0,C'$'      DISPLAY INPUT/DOLLAR COUNT?                  
         BNE   NORD0180            NO                                           
         CLC   REDCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    NORD0180                                                         
         CLC   REDCTR,HIGHCTR                                                   
         BH    NORD0180                                                         
         MVC   P+1(07),=C'NEW $$='                                              
         EDIT  (4,DMCB+4),(15,P+10),2,COMMAS=YES,CR=YES                         
         EDIT  (P8,TOTDOLRS),(15,P+30),2,COMMAS=YES,CR=YES                      
         GOTO1 REPORT                                                           
NORD0180 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW04ELT,0             
*                                  INSERT INVOICE ELT FOR MONTH                 
         LA    R0,2                SET LOOP CONTROL                             
         LA    R1,KCONRCOM         SET A(COMMENTS)                              
NORD0200 EQU   *                                                                
         XC    ELTBILD1,ELTBILD1   CLEAR ELEMENT BUILD AREA                     
         CLC   0(60,R1),SPACES     ANY COMMENT IN FIELD?                        
         BE    NORD0360            NO                                           
*                                  YES - DETERMINE L(COMMENT)                   
*                                     SCAN FROM END BACKWARDS                   
         LA    R2,59(R1)           YES - SET A(LAST CHAR IN COMT FLD)           
         LA    R3,60               SET LOOP CONTROL                             
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(08),=C'COMMENT='                                             
         MVC   P+10(60),0(R1)                                                   
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*&&                                                                             
NORD0240 EQU   *                                                                
         CLI   0(R2),C' '          FIELD CONTAINS SPACE?                        
         BNE   NORD0280            NO  - USE THIS LENGTH                        
         BCTR  R2,0                YES - BACK UP 1 CHARACTER                    
         BCT   R3,NORD0240         GO BACK FOR NEXT POSITION                    
         DC    H'0'                SHOULD NEVER HAPPEND                         
NORD0280 EQU   *                                                                
         MVI   ELTBILD1,X'02'      SET ELEMENT CODE                             
         EX    R3,NORD0320         MOVE COMMENT BY LENGTH                       
         LA    R3,3(R3)            ADD LENGTH FOR CONTROL, EX                   
         GOTO1 HELOCON1                                                         
*                                  INSERT X'02' ELT INTO RECORD                 
         B     NORD0360            CONTINUE TO CHECK                            
NORD0320 EQU   *                                                                
         MVC   ELTBILD1+2(0),0(R1) MOVE COMMENT BY LENGTH                       
NORD0360 EQU   *                                                                
         LA    R1,60(R1)           BUMP TO NEXT COMMENT                         
         BCT   R0,NORD0200         GO BACK FOR NEXT                             
*                                                                               
*   NO 27 ELT IN RECORD:  BUILD IT AND INSERT IT                                
*                                                                               
         MVC   NEW27ELT+2(2),NEW04ELT+2                                         
*                                  INSERT MONTH OF SERVICE INTO ELT             
         GOTO1 DATCON,DMCB,(5,WORK2),(2,WORK2)                                  
*                                  GET TODAY'S DATE COMPRESSED                  
         MVC   NEW27ELT+4(2),WORK2 INSERT YMD OF TODAY'S DATE                   
*                                     AS DATE OF ACTUALIZATION                  
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW27ELT,0             
*                                  INSERT HISTORY ELT FOR MONTH                 
         XC    NEW04ELT+2(8),NEW04ELT                                           
*                                  CLEAR THE NEW 04 ELT                         
         CLI   QOPTION2,C'M'       OUTPUT TO TAPE FOR MERGER?                   
         BNE   NORD0440            NO  - DIRECT UPDATE                          
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RCONREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECS             GENERATE THE OUTPUT RECORDS FOR BUY          
*                                  INSERT X'12' ELT INTO RECORD                 
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT RECORDS?                      
         BNE   NORD0400            NO                                           
         BAS   RE,DISPPUT          YES - DISPLAY OUTPUT RECORDS                 
NORD0400 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY ALL NEWORDERS?                       
         BNE   NORD0420            NO                                           
         BAS   RE,DISPNEW          YES - DISPLAY NEWORDER RECORDS               
NORD0420 EQU   *                                                                
         GOTO1 =A(CLOSEOUT),DMCB,(RC),RR=Y                                      
*                                  CHECK CLOSEOUT DATE TABLE                    
         B     NORD0560                                                         
NORD0440 EQU   *                                                                
         L     RF,PUTCTR           INCREMENT ADDED REC COUNT                    
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR                                                        
         CLI   QOPTION3,C'U'       UPDATE REQUEST?                              
         BNE   NORD0480            NO  - DON'T ADD NEW RECORD                   
         XC    KEY,KEY             CLEAR KEY                                    
         GOTO1 AREC                YES - ADD RECORD IN IO AREA                  
NORD0480 EQU   *                                                                
         GOTO1 =A(CLOSEOUT),DMCB,(RC),RR=Y                                      
*                                  CHECK CLOSEOUT DATE TABLE                    
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT RECORDS                       
         BNE   NORD0520            NO                                           
         GOTO1 DISPCON,DMCB,(RC),(2,0)                                          
*                                  DISPLAY NEWLY ADDED RECORD                   
NORD0520 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY NEWORDER RECORDS                     
         BNE   NORD0560            NO                                           
         GOTO1 DISPCONN,DMCB,(RC),(2,0)                                         
*                                  DISPLAY NEWLY ADDED RECORD                   
NORD0560 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  DISPIPUT:  DISPLAY THE RECORD LOCATED AT ARECAREA.                           
*                                                                               
DISPIPUT NTR1                                                                   
         CLC   CONCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DPUT0090                                                         
         CLC   CONCTR,HIGHCTR                                                   
         BH    DPUT0090                                                         
         GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD)                                    
         SR    RF,RF                                                            
         LA    RF,379              SET L(RECORD)                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT                                                           
DPUT0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  READIPUT:  DISPLAY THE RECORD JUST READ LOCATED AT ARECAREA.                 
*                                                                               
READIPUT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   REDCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    READ0090                                                         
         CLC   REDCTR,HIGHCTR                                                   
         BH    READ0090                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'TAPE ACT INPUT     :'                                 
         MVC   P+16(3),=C'OLD'                                                  
         CLC   =C'NEW',KCONKCON    OLD/NEW  TEST                                
         BNE   READ0020            OLD                                          
         MVC   P+16(3),=C'NEW'                                                  
READ0020 EQU   *                                                                
         EDIT  REDCTR,(7,P+25)                                                  
         GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD)                                    
         SR    RF,RF                                                            
         LA    RF,379              SET L(RECORD)                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT                                                           
READ0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  ERRIPUT:  DISPLAY THE RECORD W/ERROR LOCATED AT ARECAREA                     
*                                                                               
ERRIPUT  NTR1                                                                   
         GOTO1 REPORT                                                           
         CLI   QUESTOR+6,C'Y'      SHORT DISPLAY?                               
         BE    ERRI0040            YES - DON'T DISPLAY RECORD                   
*                                                                               
         L     R4,ARECAREA         A(RECORD)                                    
         SR    RF,RF                                                            
         LA    RF,379              SET L(RECORD)                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT                                                           
ERRI0040 EQU   *                                                                
*                                                                               
*   GET DOLLARS FROM REJECTED RECORD, AND ACCUMULATE THEM                       
*                                                                               
         MVC   WORK+20(2),KCONRFLT  SET MONTH FIRST                             
         MVI   WORK+22,C'/'         INSERT SEPARATOR                            
         MVC   WORK+23(2),KCONRFLT+2                                            
*                                   INSERT DAY NEXT                             
         MVI   WORK+25,C'/'         INSERT SEPARATOR                            
         MVC   WORK+26(2),KCONRFLT+4       SET YEAR LAST                        
         GOTO1 =V(DATVAL),DMCB,WORK+20,WORK+32,RR=Y                             
         OC    DMCB(4),DMCB         ZERO = ERROR RETURN                         
         BNZ   ERRI0060                                                         
         MVC   P+1(20),=C'DATE FIELD IN ERROR '                                 
         MVC   P+25(26),=C'*** RECORD IS BYPASSED ***'                          
         L     RF,TOTALBAD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALBAD                                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     ERRI0200                                                         
ERRI0060 EQU   *                                                                
         MVC   WORK(2),KCONRFLT+4  SET YEAR FIRST                               
         MVC   WORK+2(4),KCONRFLT  SET MONTH/DAY LAST                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
*                                  CONVERT DATE TO BINARY FORMAT                
         ZIC   RF,WORK+7           GET MONTH NUMBER FROM DATE FIELD             
         BCTR  RF,0                MAKE MONTH ZERO RELATIVE                     
         MH    RF,=H'12'           * 12 TO DISPLACE TO MONTH'S DATA             
         LA    R2,KCONDOLS         A(1ST MONTH'S DATA)                          
         AR    R2,RF               DISPLACE TO MONTH'S DATA                     
         MVC   WORK(12),0(R2)      MOVE FIELD TO WORK AREA                      
         GOTO1 =V(CASHVAL),DMCB,(2,WORK),12,0,RR=Y                              
         CLI   DMCB,X'FF'          ERROR ON VALUE?                              
         BNE   ERRI0080            NO  - ACCEPTED AS IS                         
         MVC   P+1(20),=C'REJECTED $$$ INVALID'                                 
         GOTO1 REPORT                                                           
         B     ERRI0200                                                         
ERRI0080 EQU   *                                                                
         L     RF,DMCB+4           ACCUMULATE REJECTED DOLLARS                  
         CVD   RF,WORK+24          CONVERT DOLLARS TO PACKED                    
         AP    REJDOLRS(8),WORK+24(8)                                           
ERRI0200 EQU   *                                                                
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
         MVC   P+1(24),=C'TAPE RECORDS  READ     :'                             
         EDIT  REDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         OC    STARCTR,STARCTR     ANY SKIPPED RECORDS?                         
         BZ    DITO0010            NO                                           
         MVC   P+1(24),=C'TAPE RECORDS  SKIPPED  :'                             
         EDIT  STARCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
DITO0010 EQU   *                                                                
         MVC   P+1(24),=C'OLD CONTRACTS REWRITTEN:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADD-LINES READ         :'                             
         EDIT  NEWORDRS,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADD-LINES CREATED      :'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' W/DEFAULTS            :'                             
         MVI   P+29,C'('                                                        
         EDIT  DEFORDRS,(12,P+30),COMMAS=YES                                    
         MVI   P+42,C')'                                                        
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   NEEDED AGENCY OFFICE:'                             
         MVI   P+29,C'('                                                        
         EDIT  NEEDAOF,(12,P+30),COMMAS=YES                                     
         MVI   P+42,C')'                                                        
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADD-LINES REJECTED     :'                             
         EDIT  ERRORDRS,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REJECTION COUNTS       :'                             
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   NEED PRODUCT CODE   :'                             
         EDIT  NEEDPROD,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   PRODUCT NOT ON FILE :'                             
         EDIT  NOPROD,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   STATION NOT ON FILE :'                             
         EDIT  ODDSTATN,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   SALESPERSON UNKNOWN :'                             
         EDIT  NOSALPER,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   INVALID FLIGHT DATES:'                             
         EDIT  BADDATES,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'   TOTALLY UNPROCESSED :'                             
         EDIT  TOTALBAD,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ACCEPTED $$ FOR RUN:    '                             
         EDIT  (P8,TOTDOLRS),(14,P+30),2,COMMAS=YES                             
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REJECTED $$ FOR RUN:    '                             
         EDIT  (P8,REJDOLRS),(14,P+30),2,COMMAS=YES                             
         GOTO1 REPORT                                                           
         AP    TOTDOLRS(8),REJDOLRS(8)                                          
         MVC   P+1(24),=C'TOTAL DOLLARS FOR RUN:  '                             
         EDIT  (P8,TOTDOLRS),(14,P+30),2,COMMAS=YES                             
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         GOTO1 =A(CYCLE16),DMCB,(RC),RR=Y                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   HELLO CALLS:  LABEL NAME INDICATES RECORD AND ELEMENT ADDRESS:              
*        HELO  =  HELLO CALL                                                    
*        CON   =  CONTRACT RECORD, BUY   =  BUY RECORD                          
*        1     =  ELTBILD1, ETC                                                 
*                                                                               
HELOCON1 NTR1                                                                   
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON4 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD4,0             
         XC    ELTBILD4,ELTBILD4                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         CLC   REDCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   REDCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
DIPU0001 EQU   *                                                                
         MVC   P+1(8),=C'CONTRACT'                                              
         CLI   REC,X'0C'           CONTRACT?                                    
         BE    DIPU0020            YES                                          
         MVC   P+1(8),=C'BUY REC '                                              
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
******************************************************************              
*  DISPNEW:  DISPLAY NEWORDER JUST 'PUT' TO OUTPUT.              *              
*                                                                *              
******************************************************************              
*                                                                               
DISPNEW  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   P+1(8),=C'NEWORDER'                                              
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
******************************************************************              
*  DISPCON:  DISPLAY RECORD REWRITTEN FROM CONTRACT RECORD AREA  *              
*                                                                *              
******************************************************************              
*                                                                               
DISPCON  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   DISPOPT,4(R1)       SAVE FLAG ENTERED                            
*                                                                               
         CLC   REDCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DCON0060                                                         
         CLC   REDCTR,HIGHCTR                                                   
         BH    DCON0060                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'ORIGINAL/DELETED:'                                    
         CLI   DISPOPT,0           ENTERED FROM 'ORIG/DEL'?                     
         BE    DCON0020            YES                                          
         MVC   P+1(17),=C'UPDATED ORIGINAL:'                                    
         CLI   DISPOPT,1           ENTERED FROM 'UP/ORIG'?                      
         BE    DCON0020            YES                                          
         MVC   P+1(17),=C'NEW/ADDED RECORD:'                                    
*                                  NO  - ENTERED FROM 'ADD-LINE'                
DCON0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,RECORD           A(RECORD LENGTH FIELD)                       
         ZICM  RF,RCONLEN,2        INSERT LENGTH                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DCON0060 EQU   *                                                                
         XIT1                                                                   
*                                                                               
DISPOPT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPCONN: DISPLAY RECORD REWRITTEN FROM CONTRACT RECORD AREA  *              
*                                                                *              
******************************************************************              
*                                                                               
DISPCONN NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(17),=C'NEW/ADDED RECORD:'                                    
*                                  ENTERED FROM 'ADD-LINE'                      
         GOTO1 REPORT                                                           
         LA    R4,RECORD           A(RECORD LENGTH FIELD)                       
         ZICM  RF,RCONLEN,2        INSERT LENGTH                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
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
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
ASTAAREA DS    A                                                                
ASTANEXT DS    A                                                                
STACTR   DS    F                                                                
CONCTR   DS    F                                                                
REDCTR   DS    F                                                                
DISPCTR  DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
PUTCTR2  DS    F                                                                
BUYCTR   DS    F                                                                
ERRORDRS DS    F                                                                
DEFORDRS DS    F                                                                
NEWORDRS DS    F                                                                
NEEDAOF  DS    F                                                                
NOPROD   DS    F                                                                
STARCTR  DS    F                                                                
NEEDPROD DS    F                                                                
ODDSTATN DS    F                                                                
NOSALPER DS    F                                                                
BADDATES DS    F                                                                
REQSTCTR DS    F                                                                
TOTALBAD DS    F                                                                
AIOAREA  DS    F                                                                
SAVER0   DS    F                                                                
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
FMCONNUM DS    F                   2ND CONTRACT NUMBER                          
FMCONREV DS    F                   2ND CONTRACT NUMBER REVERSED                 
F1CONREV DS    F                   1ST CONTRACT NUMBER REVERSED                 
CALLOV   DS    A                                                                
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
REQWORKA DS    CL128                                                            
ELTBILD1 DS    CL64                                                             
ELTBILD2 DS    CL64                                                             
ELTBILD3 DS    CL64                                                             
ELTBILD4 DS    CL64                                                             
TOTDOLRS DC    PL8'0'              PACKED ACCUMULATOR: $$ ACCEPTED              
REJDOLRS DC    PL8'0'              PACKED ACCUMULATOR: $$ REJECTED              
ACEGRAPH DS    XL1                 ACE/GRAPHNET INDICATOR                       
RUNID    DS    CL5                                                              
USERID   DS    CL4                                                              
POWERCDE DS    CL2                 POWER CODE FOR SEQUENTIAL READING            
*                                     NEEDED FOR MASTER POWER CODE              
FOXZEROS DC    C'0000000'                                                       
FOXALPHA DC    20X'C0'                                                          
NOSTAFLG DS    CL1                                                              
ACTVDATE DS    CL2                                                              
FLTSTART DS    CL3                 FLIGHT START DATE                            
FLTEND   DS    CL3                 FLIGHT END   DATE                            
CONERR   DS    CL1                                                              
TERMERR  DS    CL1                                                              
NOAGYOFF DS    CL1                                                              
FLGERR   DS    CL1                                                              
ACGREASY DS    CL1                 ACE/GRAPHNET FLAG                            
*                                  X'80' =  ONLINE/ACE                          
*                                  X'40' =  OFFLINE/GRAPHNET                    
*                                  X'00' =  OFFLINE/OTHER                       
REPUSE   DS    CL2                 PLUG-IN REP VALUE                            
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=379,              X        
               BLKSIZE=15160,MACRF=GM,EODAD=MAIN0200                            
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
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         ORG                                                                    
         EJECT                                                                  
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENPTP          POINT PERSON RECORD                          
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENCTL          STATION CTL RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL2048                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE REKTZTPE                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
         CSECT                                                                  
*                                                                               
*   VALIDATE THE CODES PASSED FROM KATZ TAPE.  IF NOT ON FILE,                  
*       PRINT ERROR MESSAGE, SET CC NOT ZERO ON RETURN.                         
*       SHOW ALL ERRORS ON PASS                                                 
*                                                                               
VALORDER NMOD1 0,*VALO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES      CLEAR BOTH PRINT LINES                       
         BAS   RE,SETERROR         SET UP ERROR DISPLAYS                        
*                                     JUST-IN-CASE                              
         MVI   CONERR,C'N'         TURN OFF CONTRACT ERROR FLAG                 
         MVI   TERMERR,C'N'        TURN OFF CON TERMINAL ERROR FLAG             
         MVI   FLGERR,C'N'         TURN OFF CONTRACT ERROR FLAG                 
         MVI   NOAGYOFF,C'N'       TURN OFF AGENCY OFFICE FLAG                  
         CLI   KCONRTYP,C'K'       KATZ 'NETWORK' CONTYPE?                      
         BNE   VORD0020            NO                                           
         MVI   KCONRTYP,C'N'       YES - SET TO 'NETWORK' DDS                   
         B     VORD0040                                                         
VORD0020 EQU   *                                                                
         CLI   KCONRTYP,C' '       ANY CONTRACT TYPE ENTERED?                   
         BNE   VORD0030            YES - CHECK IT FURTHER                       
         MVI   KCONRTYP,C'S'       NO  - SET IT TO SPOT                         
         B     VORD0040                                                         
VORD0030 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           INSERT CONTRACT TYPE RECORD                  
         MVC   KEY+24(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+26(1),KCONRTYP  INSERT CONTRACT TYPE                         
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     CONTRACT TYPE ON FILE?                       
         BE    VORD0040            YES                                          
*                                                                               
         MVI   PSECOND+67,C'S'     INDICATE DEFAULT USED ON REPORT              
         MVI   KCONRTYP,C'S'       SET DEFAULT                                  
         MVI   CONERR,C'Y'         TURN ON CONTRACT DEFAULT FLAG                
VORD0040 EQU   *                                                                
         MVC   RCONTYPE,KCONRTYP   CONTRACT TYPE OK: PUT IN RECORD              
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           INSERT AGENCY RECORD                         
         MVC   KEY+25(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+19(4),KCONRAGY  INSERT AGENCY CODE                           
         CLI   KCONRAGY+3,C'A'     4TH CHARACTER VALID?                         
         BL    VORD0060            NO  - LESS THAN 'A'                          
         CLI   KCONRAGY+3,C'Z'     4TH CHARACTER VALID?                         
         BNH   VORD0080            NO  - MORE THAN 'Z'                          
         CLI   KCONRAGY+3,C'0'     4TH CHARACTER VALID?                         
         BL    VORD0060            NO  - LESS THAN '0'                          
         CLI   KCONRAGY+3,C'9'     4TH CHARACTER VALID?                         
         BNH   VORD0080            NO  - MORE THAN '9'                          
VORD0060 EQU   *                                                                
         MVI   KEY+22,C' '         SPECIAL CHAR:  SPACE FILL IT                 
VORD0080 EQU   *                                                                
         MVC   KEY+23(2),KCONRAGY+5                                             
*                                  INSERT AGENCY OFFICE                         
*                                                                               
         GOTO1 HIGH1                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE     AGENCY ON FILE?                              
         BE    VORD0100            YES                                          
         CLC   KCONRAGY+5(2),SPACES                                             
*                                  NO  - AGENCY/POSSIBLY AGY OFF NOT            
*                                     FOUND ON FILE:                            
*                                     WAS ANY AGENCY OFFICE ENTERED?            
         BE    VORD0090            NO  - USE DEFAULT VALUES                     
         MVC   KEY(27),KEYSAVE     YES - RESET KEY TO LOOK FOR ONLY             
*                                     THE AGENCY ITSELF                         
         MVC   KEY+23(2),SPACES    CLEAR OUT AGENCY OFFICE                      
         GOTO1 HIGH                REREAD FOR AGENCY ONLY                       
         CLC   KEY(27),KEYSAVE     AGENCY ON FILE?                              
         BNE   VORD0090            NO  - SET DEFAULT                            
         MVC   RCONKAGY(6),KEY+19  INSERT AGENCY/NO OFFICE INTO RECORD          
*                                     THROW OUT ANY UNRECOGNIZED OFFICE         
         B     VORD0105            CHECK TO SEE IF OFFICE IS NEEDED             
*                                                                               
VORD0090 EQU   *                                                                
         MVC   PSECOND+25(4),=C'XXXX'                                           
         MVC   RCONKAGY(6),=C'XXXX   '                                          
*                                  SET DEFAULT                                  
         MVI   CONERR,C'Y'         TURN ON CONTRACT DEFAULT ERROR               
         B     VORD0120            SKIP AGY/OFFICE TEST                         
VORD0100 EQU   *                                                                
         MVC   RCONKAGY(6),KEY+19  INSERT AGENCY/AGYOFF INTO RECORD             
         CLC   KCONRAGY+5(2),SPACES                                             
*                                  ANY AGENCY OFFICE?                           
         BNE   VORD0120            YES - AGY+OFF SENT, FOUND ON FILE            
*                                     PROCEED TO NEXT FIELD                     
*   NO AGENCY OFFICE SENT:  AGENCY FOUND ON FILE.                               
*      DOES AGENCY ON FILE HAVE OFFICES, REQUIRING ONE BE SENT?                 
*                                                                               
VORD0105 EQU   *                                                                
         MVC   KEY+23(2),=X'0001'  FORCE NEXT OFFICE, IF PRESENT                
VORD0110 EQU   *                                                                
         GOTO1 SEQ1                NO  - READ NEXT AGENCY RECORD                
*                                                                               
         CLC   KEY(23),KEYSAVE     SAME TYPE/AGENCY?                            
         BNE   VORD0120            NO  - NO AGENCY OFFICE SENT:                 
*                                     NONE FOUND - OKAY TO USE                  
*                                  YES - MAY NOT BE SAME REP                    
*                                                                               
*   NOTE:  THIS IS A REAL PAIN RIGHT HERE.  BECAUSE THE READ MAY BE             
*        FOR A SUBREP OF A MASTER, THERE IS THE NEED TO CHECK BOTH              
*        THE SUBREP CODE AS WELL AS THE MASTER REP CODE FOUND.                  
*        IN AGENCY RECORD KEYS, THE POWER CODE IS UNDER THE                     
*        AGENCY / AGENCY OFFICE CODES.  THIS MEANS THAT THE AGENCY              
*        KEYS FOR DIFFERENT REPS ARE GROUPED TOGETHER.  THIS                    
*        CREATES A PROBLEM, AS A SEQUENTIAL READ THAT RETURNS                   
*        AN AGENCY CODE FOR A REP OTHER THAN THE SUBREP WILL MESS               
*        UP DATAMANAGER'S ABILITY TO REPLACE THE 'MASTER' REP                   
*        WITH THE PROPER 'SUBSIDIARY' REP.  RATHER THAN TRY TO                  
*        BE TOO COMPLICATED, I HAVE SIMPLY COMPARED FOR, FIRST,                 
*        THE SUBREP CODE, AND, SECOND, FOR THE MASTER REP CODE.                 
*                                                                               
         CLC   KEY+25(2),REPUSE    SAME REP (USE SUBREP CODE HERE)              
         BE    VORD0115            NO  - GO BACK AND CHECK NEXT                 
         CLC   KEY+25(2),POWERCDE  SAME REP (USE MASTER REP CODE HERE)          
         BNE   VORD0110            NO  - GO BACK AND CHECK NEXT                 
*                                                                               
VORD0115 EQU   *                                                                
*                                                                               
*   MISSING AGENCY OFFICE NOW DEFAULTS TO XXXX, THROWING OUT ORIGINAL           
*        AGENCY CODE.                                                           
*                                                                               
         MVC   PSECOND+25(4),=C'XXXX'                                           
         MVC   RCONKAGY(6),=C'XXXX   '                                          
*                                  SET DEFAULT                                  
         MVI   CONERR,C'Y'         TURN ON CONTRACT DEFAULT ERROR               
         L     RF,NEEDAOF          COUNTER DEFINES MISSING AGENCY               
         LA    RF,1(RF)              OFFICE CODES WHICH DEFAULTED TO            
         ST    RF,NEEDAOF             AGENCY CODE OF XXXX, OFF=SPACES           
         MVC   PSECOND+30(2),=C'**' REQUIRES AGENCY OFFICE                      
         MVI   NOAGYOFF,C'Y'       TURN ON AGENCY OFFICE FLAG                   
         B     VORD0120            SKIP AGY/OFFICE TEST                         
*********************************************************************           
***      MVC   PSECOND+30(2),=C'**' REQUIRES AGENCY OFFICE                      
***      MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
***      MVI   FLGERR,C'1'         SET DISPLAY FLAG                             
***      L     RF,NEEDAOF          INCREMENT COUNTER                            
***      LA    RF,1(RF)                                                         
***      ST    RF,NEEDAOF                                                       
*********************************************************************           
VORD0120 EQU   *                                                                
         XC    ELTBILD3,ELTBILD3   CLEAR X'05' BUILD AREA                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           INSERT ADVERTISER RECORD                     
         MVC   KEY+25(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+21(4),KCONRADV  INSERT ADVERTISER CODE                       
         MVC   RCONKADV,KEY+21     INSERT ADVERTISER INTO RECORD                
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     ADVERTISER CODE ON FILE?                     
         BE    VORD0160            YES - LEAVE ADVERTISER IN RECORD             
*                                                                               
         MVC   PSECOND+34(4),=C'XXXX'                                           
         MVC   RCONKADV(4),=C'XXXX'                                             
*                                  OVERRIDE ADVERTISER WITH DEFAULT             
         MVI   CONERR,C'Y'         TURN ON CONTRACT DEFAULT ERROR               
VORD0160 EQU   *                                                                
         CLI   KCONRTYP,C'N'       NETWORK CONTRACT?                            
         BNE   VORD0200            NO                                           
         CLC   KCONRPRD(2),=C'C='  YES - PRODUCT CODE DELIVERED?                
         BE    VORD0200            YES                                          
*                                                                               
*                                  NO KEYWORD:  CHECK WHAT IS ENTERED           
         XC    KEY,KEY             YES - VALIDATION NEEDED                      
         MVI   KEY,X'09'           INSERT PRODUCT RECORD                        
         MVC   KEY+25(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+18(4),KCONRADV  INSERT ADVERTISER CODE                       
         MVC   KEY+22(3),KCONRPRD  INSERT PRODUCT CODE                          
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     ADVERTISER CODE ON FILE?                     
         BNE   VORD0180            NO                                           
         MVC   RCONPRD,KCONRPRD    YES - INSERT PROD CODE INTO RECORD           
         B     VORD0240                                                         
*                                                                               
VORD0180 EQU   *                                                                
         MVC   P+44(12),=C'NWK CONTRACT'                                        
         MVC   PSECOND+44(21),=C'PROD CODE NOT ON FILE'                         
         MVC   PSECOND+70(4),KCONRADV                                           
         MVI   PSECOND+74,C'/'                                                  
         MVC   PSECOND+75(5),KCONRPRD                                           
         MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
         MVI   FLGERR,C'2'         SET DISPLAY FLAG                             
         L     RF,NEEDPROD         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NEEDPROD                                                      
VORD0200 EQU   *                                                                
         CLC   KCONRPRD(2),=C'C='  YES - PRODUCT CODE?                          
         BE    VORD0220            YES - VALIDATE CODE ENTERED                  
*                                                                               
*   BUILD PRODUCT X'05' ELEMENT FOR ADDING AT A LATER TIME                      
*                                                                               
         MVC   RCONPRD,SPACES      NO  - SET PROD CODE TO SPACES                
         XC    ELTBILD3,ELTBILD3                                                
         MVI   ELTBILD3,5          SET ELEMENT ID                               
         MVI   ELTBILD3+1,22       SET ELEMENT LENGTH                           
         MVC   ELTBILD3+2(20),KCONRPRD                                          
*                                  MOVE PRODUCT CODE TO ELEMENT                 
         CLC   KCONRPRD,SPACES     ANY PRODUCT FIELD ENTERED?                   
         BNE   VORD0210            YES                                          
         MVC   ELTBILD3+2(15),=C'DEFAULT PRODUCT'                               
VORD0210 EQU   *                                                                
         B     VORD0240                                                         
VORD0220 EQU   *                                                                
         XC    KEY,KEY             YES - VALIDATION NEEDED                      
         MVI   KEY,X'09'           INSERT PRODUCT RECORD                        
         MVC   KEY+25(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+18(4),KCONRADV  INSERT ADVERTISER CODE                       
         MVC   KEY+22(3),KCONRPRD+2                                             
*                                  INSERT PRODUCT CODE                          
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     ADVERTISER CODE ON FILE?                     
         BE    VORD0230            YES                                          
*                                                                               
         MVC   PSECOND+44(21),=C'PROD CODE NOT ON FILE'                         
         MVC   PSECOND+70(4),KCONRADV                                           
         MVI   PSECOND+74,C'/'                                                  
         MVC   PSECOND+75(5),KCONRPRD                                           
         MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
         MVI   FLGERR,C'3'         SET DISPLAY FLAG                             
         L     RF,NOPROD           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NOPROD                                                        
         B     VORD0240                                                         
VORD0230 EQU   *                                                                
         MVC   RCONPRD,KCONRPRD+2  INSERT PRODUCT CODE INTO RECORD              
VORD0240 EQU   *                                                                
         MVC   RCONCTGY,=C'AC'     SET DEFAULT CATEGORY CODE                    
         CLC   KCONRCTG,SPACES     ANY CATEGORY CODE ENTERED?                   
         BE    VORD0280            NO  -                                        
         MVI   KEY,X'0F'           INSERT CATEGORY RECORD                       
         MVC   KEY+23(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+25(2),KCONRCTG  INSERT CATEGORY CODE                         
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     CATEGORY CODE ON FILE?                       
         BNE   VORD0260            NO                                           
         MVC   RCONCTGY,KCONRCTG   INSERT CATEGORY CODE GIVEN                   
         B     VORD0280                                                         
*                                                                               
VORD0260 EQU   *                                                                
         MVC   PSECOND+58(2),=C'AC'                                             
         MVI   CONERR,C'Y'         TURN ON SPECIAL CONTRACT ERROR FLAG          
VORD0280 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           INSERT STATION RECORD                        
         MVC   KEY+20(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+22(4),KCONRSTA  INSERT STATION CALL LETTERS                  
*                                                                               
*   KATZ MADE UP THEIR OWN RECORD FORMAT.  LOW-POWER STATION                    
*        DOES NOT CONFORM TO KAAA-A FORMAT, BUT HAS BEEN ENTERED                
*        AS KAAAL, REQUIRING SPECIAL HANDLING AFTER THE FACT.                   
*                                                                               
         CLI   KCONRSTA+4,C'L'     LOW-POWER STATION?                           
         BNE   VORD0300            NO                                           
         MVI   KEY+26,C'L'         YES - INSERT IT                              
         B     VORD0310            NO                                           
VORD0300 EQU   *                                                                
         MVC   KEY+26(1),KCONRSTA+5                                             
VORD0310 EQU   *                                                                
*                                  INSERT STATION MEDIA                         
         CLI   KEY+26,C'T'         TV?                                          
         BNE   VORD0320            NO                                           
         MVI   KEY+26,C' '         YES - CHANGE TO SPACE                        
VORD0320 EQU   *                                                                
         CLI   KCONRSTA+3,C'A'     4TH CHARACTER VALID?                         
         BL    VORD0360            NO  - LESS THAN 'A'                          
         CLI   KCONRSTA+3,C'Z'     4TH CHARACTER VALID?                         
         BNH   VORD0380            YES -                                        
VORD0360 EQU   *                                                                
         MVI   KEY+25,C' '         SPECIAL CHAR:  SPACE FILL IT                 
VORD0380 EQU   *                                                                
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     STATION ON FILE?                             
         BE    VORD0400            YES                                          
*                                                                               
         MVC   PSECOND+7(07),=C'UNKNOWN'                                        
*        MVC   P+1(27),KEY                                                      
*        GOTO1 REPORT                                                           
*        MVC   P+1(27),KEYSAVE                                                  
*        GOTO1 REPORT                                                           
         MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
         MVI   FLGERR,C'4'         SET DISPLAY FLAG                             
         L     RF,ODDSTATN         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,ODDSTATN                                                      
VORD0400 EQU   *                                                                
         CLI   TERMERR,C'Y'        REJECTED FOR ERROR?                          
         BE    VORD0420            YES - DON'T DO UNNRECESSARY READ             
*                                                                               
         MVC   FULL,AIOAREA        SAVE CURRENT A(IOAREA)                       
         LA    RF,RECORD2          SET ALTERNATE IO AREA                        
         ST    RF,AIOAREA                                                       
*                                                                               
         GOTO1 GREC1               RETRIEVE RECORD                              
         MVC   RCONKGRP,RSTAGRUP   INSERT STATION GROUP INTO RECORD             
         LA    R1,RSTAELEM                                                      
         MVI   ACGREASY,0          SET FLAG TO 'OFF-LINE'/OTHER                 
VORD0410 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD REACHED?                       
         BE    VORD0414            YES - CONSIDER IT 'OFF-LINE'                 
         CLI   0(R1),5             EXTENDED DESCRIPT ELT?                       
         BE    VORD0412            YES                                          
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     VORD0410            GO BACK FOR NEXT ELT                         
VORD0412 EQU   *                                                                
         OC    10(2,R1),10(R1)     ANY RECEIVING ID?                            
         BZ    VORD0414            NO  - CONSIDER 'OFF-LINE'                    
         MVI   ACGREASY,X'40'      SET TO 'OFF-LINE' (GRAPH)                    
         CLC   10(2,R1),=X'0406'   GRAPHNET?                                    
         BE    VORD0414            YES - CONSIDER 'OFF-LINE'                    
         MVI   ACGREASY,X'80'      SET TO 'ON-LINE' (ACE)                       
VORD0414 EQU   *                                                                
         MVC   AIOAREA,FULL        RESTORE ORIGINAL A(IOAREA)                   
         MVC   RCONKSTA,KEY+22     INSERT STATION INTO RECORD                   
VORD0420 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           INSERT SALESPERSON RECORD                    
         MVC   KEY+22(2),REPUSE    INSERT REP CODE                              
         MVC   KEY+24(3),KCONRSAL  INSERT S/P CODE                              
         GOTO1 HIGH1                                                            
         CLC   KEY(27),KEYSAVE     S/P CODE ON FILE?                            
         BE    VORD0440            YES                                          
*                                                                               
         MVC   PSECOND+61(3),=C'***'                                            
         MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
         L     RF,NOSALPER         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NOSALPER                                                      
         MVI   FLGERR,C'5'         SET DISPLAY FLAG                             
VORD0440 EQU   *                                                                
         MVC   RCONSAL,KEY+24      INSERT S/P INTO RECORD                       
         CLI   TERMERR,C'Y'        REJECTED FOR ERROR?                          
         BE    VORD0460            YES - DON'T DO UNNRECESSARY READ             
*                                                                               
         MVC   FULL,AIOAREA        SAVE CURRENT A(IOAREA)                       
         LA    RF,RECORD2          SET ALTERNATE IO AREA                        
         ST    RF,AIOAREA                                                       
*                                                                               
         GOTO1 GREC1               RETRIEVE RECORD                              
         MVC   RCONKOFF,RSALOFF    INSERT S/P'S OFFICE INTO RECORD              
         MVC   RCONTEM,RSALTEAM    INSERT S/P'S TEAM INTO RECORD                
         MVC   AIOAREA,FULL        RESTORE ORIGINAL A(IOAREA)                   
VORD0460 EQU   *                                                                
         GOTO1 =V(DATVAL),DMCB,(0,KCONRFLT),WORK,RR=Y                           
         OC    DMCB,DMCB           ERROR?                                       
         BZ    VORD0480            YES                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,FLTSTART)                                
         GOTO1 =V(DATVAL),DMCB,(0,KCONRFLT+7),WORK,RR=Y                         
         OC    DMCB,DMCB           ERROR?                                       
         BZ    VORD0480            YES                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,FLTEND)                                  
         B     VORD0520                                                         
VORD0480 EQU   *                                                                
         MVC   PSECOND(13),=C'DATES INVALID'                                    
         MVI   TERMERR,C'Y'        TURN ON CON TERMINAL ERROR FLAG              
         L     RF,BADDATES         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BADDATES                                                      
         MVI   FLGERR,C'6'         SET DISPLAY FLAG                             
VORD0520 EQU   *                                                                
         MVC   RCONDATE(3),FLTSTART                                             
*                                  INSERT FLIGHT START INTO RECORD              
         MVC   RCONDATE+3(3),FLTEND                                             
*                                  INSERT FLIGHT END   INTO RECORD              
         CLI   TERMERR,C'Y'        CONTRACT TERMINAL ERROR FLAG ON?             
         BE    VORD0530            YES                                          
         CLI   CONERR,C'N'         CONTRACT ERROR FLAG OFF?                     
         BE    VORD0580            YES - NO SPECIAL DISPLAY                     
         B     VORD0560            NO  - DISPLAY SPECIAL MESSAGE                
VORD0530 EQU   *                                                                
         L     RF,ERRORDRS         INCREMENT ORDERS IN ERROR COUNTER            
         LA    RF,1(RF)                                                         
         ST    RF,ERRORDRS                                                      
*                                                                               
*                                                                               
****     CLI   QUESTOR+9,C'Y'      DISPLAY REJECT ERROR RECORDS?                
****     BNE   VORD0540            NO                                           
         MVC   P+14(8),=C'********'                                             
*                                  INDICATE NO CONTRACT NUMBER                  
         GOTO1 REPORT              YES - DISPLAY ORDER INFO IN ERROR            
         MVC   P+01(22),=C'NO CONTRACT GENERATED.'                              
         MVC   P+25(13),=C'ADDLINE ERROR'                                       
         EDIT  ERRORDRS,(6,P+39),ALIGN=LEFT                                     
         GOTO1 REPORT                                                           
*        MVI   P+1,C'*'                                                         
*        MVC   P+2(100),P+1                                                     
*        GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
VORD0540 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DISPLAY COUNT ON DEFAULT PRINTOUT?           
         BNE   VORD0550            NO                                           
         MVC   PSECOND+8(1),FLGERR                                              
VORD0550 EQU   *                                                                
*                                                                               
*   GET DOLLARS FROM REJECTED RECORD, AND ACCUMULATE THEM                       
*                                                                               
         MVC   WORK(2),KCONRFLT+4  SET YEAR FIRST                               
         MVC   WORK+2(4),KCONRFLT  SET MONTH/DAY LAST                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
*                                  CONVERT DATE TO BINARY FORMAT                
         ZIC   RF,WORK+7           GET MONTH NUMBER FROM DATE FIELD             
         BCTR  RF,0                MAKE MONTH ZERO RELATIVE                     
         MH    RF,=H'12'           * 12 TO DISPLACE TO MONTH'S DATA             
         LA    R2,KCONDOLS         A(1ST MONTH'S DATA)                          
         AR    R2,RF               DISPLACE TO MONTH'S DATA                     
         MVC   WORK(12),0(R2)      MOVE FIELD TO WORK AREA                      
         GOTO1 =V(CASHVAL),DMCB,(2,WORK),12,0,RR=Y                              
         CLI   DMCB,X'FF'          ERROR ON VALUE?                              
         BNE   VORD0552            NO  - ACCEPTED AS IS                         
         MVC   P+60(20),=C'REJECTED $$$ INVALID'                                
         B     VORD0554                                                         
VORD0552 EQU   *                                                                
         L     RF,DMCB+4           ACCUMULATE REJECTED DOLLARS                  
         CVD   RF,WORK+24          CONVERT DOLLARS TO PACKED                    
         AP    REJDOLRS(8),WORK+24(8)                                           
VORD0554 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VORD0640                                                         
VORD0560 EQU   *                                                                
         L     RF,DEFORDRS         INCREMENT DEFAULT ORDERS COUNTER             
         LA    RF,1(RF)                                                         
         ST    RF,DEFORDRS                                                      
****     CLI   QUESTOR+10,C'Y'     DISPLAY DEFAULT ERROR RECORDS?               
****     BNE   VORD0580            NO                                           
         MVC   PTHIRD+01(22),=C'CONTRACT GENERATED WIT'                         
         MVC   PTHIRD+23(16),=C'H DEFAULT VALUES'                               
         EDIT  DEFORDRS,(6,PTHIRD+42)                                           
         CLI   NOAGYOFF,C'N'       AGENCY OFFICE FLAG SET?                      
         BE    VORD0570            NO                                           
         MVI   NOAGYOFF,C'N'       YES - TURN IT OFF                            
         MVC   PTHIRD+55(38),=C'ORIGINAL AGENCY REQUIRES AGENCY OFFICE'         
VORD0570 EQU   *                                                                
****     L     R4,ARECAREA         A(RECORD)                                    
****     SR    RF,RF                                                            
****     LA    RF,379              SET L(RECORD)                                
****     GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
****     GOTO1 REPORT                                                           
VORD0580 EQU   *                                                                
*                                                                               
*   ASSIGN NEXT NUMBER TO NEW CONTRACT TO BE GENERATED.                         
*                                                                               
         GOTO1 =A(NEXTCON#),DMCB,(RC),RR=Y                                      
*                                  GET COMPANY'S NEXT CONTRACT NUMBER           
*                                     RETURNED IN 'FULL'                        
         L     R1,FULL             RETURN VALUE FROM NEXTCON                    
         EDIT  FULL,(8,WORK),FILL=0                                             
         GOTO1 =V(HEXIN),DMCB,WORK,RCONKCON,8,=C'TOG'                           
*                                                                               
         CLI   CONERR,C'N'         NO ERRORS FOUND?                             
         BE    VORD0600            YES - NO PRINT                               
         MVC   P+14(8),WORK        INSERT CONTRACT NUMBER                       
         GOTO1 REPORT                                                           
*        MVI   P+1,C'*'                                                         
*        MVC   P+2(100),P+1                                                     
*        GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                  RETURN CC = ZERO:  ACCEPTED                  
VORD0600 EQU   *                                                                
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES      CLEAR PRINTLINES                             
         MVC   PTHIRD,SPACES       CLEAR PRINTLINES                             
         SR    R0,R0               SET CC ZERO                                  
VORD0640 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SETERROR:  SET UP PRINT LINES IN EVENT ORDER IS FOUND IN ERROR.             
*        THIS WILL PRECLUDE HAVING TO SAVE OFF THE ORDER, THEN SET              
*        UP PRINT LINES FROM A SECONDARY AREA.                                  
*                                                                               
SETERROR NTR1                                                                   
         MVC   P+2(2),KCONTREP     INSERT REP CODE                              
         MVC   P+6(7),KCONRSTA     INSERT STATION CALL LETTERS                  
         MVC   P+25(7),KCONRAGY    INSERT AGENCY+AGENCY OFFICE                  
         MVC   P+34(4),KCONRADV    INSERT ADVERTISER                            
         MVC   P+44(12),KCONRPRD   INSERT PRODUCT NAME OR CODE                  
         MVC   P+58(2),KCONRCTG    INSERT CATEGORY CODE                         
         MVC   P+61(3),KCONRSAL    INSERT SALESPERSON CODE                      
         MVC   P+66(1),KCONRTYP    INSERT CONTRACT TYPE                         
         MVC   P+69(13),KCONRFLT   INSERT FLIGHT DATES                          
         LA    R1,KCONDOLS         SCAN FOR DOLLARS                             
         LA    R0,12               SCAN 12 MONTHLY AMOUNTS                      
SERR0020 EQU   *                                                                
         CLC   7(5,R1),=C' 0.00'   EMPTY DOLLAR FIELD?                          
         BNE   SERR0040            NO  - DISPLAY IT AS AMOUNT                   
         LA    R1,12(R1)           BUMP TO NEXT FIELD                           
         BCT   R0,SERR0020         GO BACK FOR NEXT                             
         MVC   P+84(12),KCONDOLS   NO DOLLARS FOUND:                            
*                                     USE FIRST FIELD                           
         B     SERR0060                                                         
SERR0040 EQU   *                                                                
         MVC   P+84(12),0(R1)      MOVE DOLLARS FOUND TO FIELD                  
SERR0060 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DISPLAY COUNT ON DEFAULT PRINTOUT?           
         BNE   SERR0080            NO                                           
         EDIT  REDCTR,(6,PSECOND+2),ALIGN=LEFT                                  
*                                  INSERT RECORD COUNTER                        
SERR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*********************************************************************           
READ1    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY1                                                         
SEQ1     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY1                                                         
HIGH1    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY1                                                         
ADD1     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY1                                                         
WRITE1   MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY1                                                         
DIRCTRY1 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         B     RGENIOD1                                                         
GREC1    MVC   COMMAND(8),GETREC                                                
         B     FILE1                                                            
PREC1    MVC   COMMAND(8),PUTREC                                                
         B     FILE1                                                            
AREC1    MVC   COMMAND(8),ADDREC                                                
         B     FILE1                                                            
FILE1    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
*                  DATA MANAGER ERRORS AND EXIT                                 
                                                                                
RGENIOD1 OC    DMCB+8(1),DMCB+8                                                 
*                                                                               
         XIT1                      RETURN                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHECK STATION/REP/MONTH JUST UPDATED.  IF NOT IN TABLE, ADD IT              
*        TO TABLE.                                                              
*        ARGUMENT TO TABLE IS:                                                  
*        CHARS 1  -  5  =  STATION CALL LETTERS                                 
*        CHARS 6  -  7  =  REP CODE                                             
*        CHARS 8  -  9  =  FIRST MONTH TO CLOSE                                 
*        CHARS 10 -  11 =  LAST  MONTH TO CLOSE                                 
*                                                                               
CLOSEOUT NMOD1 0,*TURN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ASTAAREA         SET A(STATION TABLE AREA)                    
         L     R6,STACTR           SET CURRENT STATION COUNTER                  
         XC    WORK,WORK           CLEAR WORKAREA                               
         MVC   WORK(5),RCONKSTA    INSERT STATION CALL LETTERS                  
         MVC   WORK+5(2),RCONKREP  INSERT REP CODE                              
         MVC   WORK+7(2),FLTSTART  INSERT YM OF INVOICED MONTH (1ST)            
         MVC   WORK+9(2),FLTSTART  INSERT YM OF INVOICED MONTH (LAST)           
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(1,WORK),(R2),(R6),11,(0,7),5000,      X        
               RR=RELO                                                          
*                                  FIND STA/REP.  IF NOT FOUND,                 
*                                     INSERT NEW CODE INTO TABLE                
         CLI   DMCB,0              RECORD FOUND?                                
         BE    CLDA0040            YES                                          
         MVC   STACTR,DMCB+8       SAVE NEW COUNT                               
         B     CLDA0100                                                         
CLDA0040 EQU   *                                                                
         L     RF,DMCB             LOAD A(ENTRY FOUND)                          
         CLC   FLTSTART(2),7(RF)   INV MONTH BEFORE EARLIEST?                   
         BNL   CLDA0060            NO                                           
         MVC   7(2,RF),FLTSTART    YES - INSERT NEW EARLIEST DATE               
         B     CLDA0100                                                         
CLDA0060 EQU   *                                                                
         CLC   9(2,RF),FLTSTART    LATEST BEFORE INV MONTH?                     
         BNL   CLDA0100            NO                                           
         MVC   9(2,RF),FLTSTART    YES - INSERT NEW LATEST DATE                 
CLDA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEXTCON#:  RETRIEVE THE NEXT AVAILABLE CONTRACT NUMBER FROM                 
*        THE SPECIFIC REP.  TABLE AND INCREMENT EACH REP SEPARATELY.            
*        NEXT CONTRACT NUMBER WILL BE RETURNED IN 'FULL' AS BINARY              
*        VALUE, WHICH MUST BE CONVERTED FOR USE.                                
*                                                                               
NEXTCON# NMOD1 0,*CON#*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   REPCONID,=C'*REPCON*'                                            
*                                  TABLE INITIALIZED?                           
         BE    NCON0020            YES                                          
         MVC   REPCONID,=C'*REPCON*'                                            
*                                  NO  - SET ID                                 
         XC    REPCON#S,REPCON#S                                                
NCON0020 EQU   *                                                                
         LA    R2,REPCON#S         FIND REP IN TABLE                            
NCON0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    NCON0060            YES -                                        
         CLC   0(2,R2),REPUSE      TABLE REP = RECORD REP?                      
         BE    NCON0120            YES - USE NEXT NUMBER                        
         LA    R2,L'REPCON#S(R2)   NO  - BUMP TO NEXT ENTRY                     
         B     NCON0040            GO BACK FOR NEXT                             
NCON0060 EQU   *                                                                
         MVC   0(2,R2),REPUSE      INSERT NEW REP INTO TABLE                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           BUILD HI-KEY RETRIEVER                       
*                                     (9'S COMPLEMENT)                          
         MVC   KEY+21(2),REPUSE    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH2               GET HIGH KEY                                 
         CLC   KEY(23),KEY         SAME RECORD TYPE/REP?                        
         BE    *+6                 YES                                          
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                  COMPLEMENT CONTRACT NUMBER                   
*                                     (GET ORIGINAL NUMBER)                     
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEY+23(4)                                             
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
*                                                                               
         SP    WORK+5(5),WORK+15(5)                                             
*                                                                               
         MVO   WORK+15(5),WORK+5(5)                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,WORK+15,WORK,4,=C'TOG'                           
*                                  CONVERT CON# TO EBCDIC                       
         PACK  WORK+24(8),WORK(8)  PACK CONTRACT NUMBER                         
*                                                                               
         CVB   RF,WORK+24          PACK THE NUMBER DOWN                         
         STCM  RF,15,2(R2)         INSERT NUMBER INTO TABLE                     
*                                                                               
*   TEST                                                                        
         CLI   QUESTOR+4,C'Y'      DISPLAY NEXT CONTRACT?                       
         BNE   NCON0120            NO                                           
         MVC   P+1(17),=C'REP XX NEXT CON#='                                    
         MVC   P+5(2),REPUSE                                                    
         MVC   P+19(8),WORK                                                     
         MVC   P+30(4),2(R2)                                                    
         GOTO1 REPORT                                                           
*   TEST NEXT CON# END                                                          
*      NOTE:  THIS DISPLAY STEPS ON THE PRINTLINE BEING CONSTRUCTED             
*                                                                               
NCON0120 EQU   *                                                                
         ZICM  RF,2(R2),4          GET NEXT CON# FROM TABLE                     
         LA    RF,1(RF)            BUMP CON#                                    
         ST    RF,FULL             RETURN VALUE                                 
         STCM  RF,15,2(R2)         PUT BACK IN TABLE                            
         XIT1                                                                   
*                                                                               
*   REPCON TABLE CONSISTS OF:                                                   
*        8-CHAR DUMP ID.  IF NOT '*REPCON*', SET THUSLY, AND                    
*              TABLE IS INITIALIZED TO ZERO                                     
*        20 6-CHARACTER INDICATORS CONSISTING OF                                
*              2-CHAR REP ID                                                    
*              FULL-WORD COUNTER CONTAINING LAST-USED CONTRACT #                
*                                                                               
REPCONID DS    CL8                                                              
REPCON#S DS    20CL6                                                            
         EJECT                                                                  
*********************************************************************           
READ2    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY2                                                         
SEQ2     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY2                                                         
HIGH2    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY2                                                         
ADD2     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY2                                                         
WRITE2   MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY2                                                         
DIRCTRY2 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         B     RGENIOD2                                                         
GREC2    MVC   COMMAND(8),GETREC                                                
         B     FILE2                                                            
PREC2    MVC   COMMAND(8),PUTREC                                                
         B     FILE2                                                            
AREC2    MVC   COMMAND(8),ADDREC                                                
         B     FILE2                                                            
FILE2    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
*                  DATA MANAGER ERRORS AND EXIT                                 
                                                                                
RGENIOD2 OC    DMCB+8(1),DMCB+8                                                 
*                                                                               
         XIT1                      RETURN                                       
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CYCLE16:  PROCESS EACH STATION TO CLOSE OUT INVOICE MONTHS.                 
*                                                                               
CYCLE16  NMOD1 0,*CY16*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
*                                  INITIALIZE NEW ACTIVITY ELEMENT              
         MVC   NEW23ELT+2(3),WORK  INSERT TODAY'S DATE                          
*                                                                               
         MVI   RCSUBPRG,2          SET UP ALTERNATE HEADINGS                    
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,RECORD2                                                       
         ST    RF,AIOAREA          RESET A(IO AREA)                             
         GOTO1 REPORT                                                           
         MVC   P+1(41),=C'STATIONS PROCESSED IN ACTUALIZATION: XXXX'            
         EDIT  STACTR,(4,P+38)                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         L     R2,ASTAAREA                                                      
CYCL0040 EQU   *                                                                
         OC    0(9,R2),0(R2)       ANY ENTRY  IN TABLE?                         
         BZ    CYCL0400            NO  - FINISHED                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'CLOSE STA:'                                           
*        MVC   P+10(11),0(R2)                                                   
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*   ONE-TIME CORRECTION                                                         
         OC    7(2,R2),7(R2)       START DATE = ZERO?                           
         BNZ   CYCL0070                                                         
         MVC   7(2,R2),9(R2)       YES - INSERT END DATE                        
CYCL0070 EQU   *                                                                
         MVC   P+1(5),0(R2)        YES - DISPLAY TABLE ENTRY                    
         MVC   P+10(2),5(R2)                                                    
         MVC   WORK(2),9(R2)       GET CLOSE DATE                               
         MVI   WORK+2,1            FORCE TO 1ST OF MONTH                        
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+20)                                    
***<<<   GOTO1 HEXOUT,DMCB,9(R2),P+20,2,=C'TOG'                                 
CYCL0080 EQU   *                                                                
         MVC   WORK+6(2),7(R2)     GET BCST MON START DATE                      
         MVI   WORK+8,15           SET 15TH OF MONTH                            
         GOTO1 DATCON,DMCB,(3,WORK+6),(0,WORK)                                  
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
*                                  GET BROADCAST MONTH                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,FSTRCOMP)                              
*                                  SET START DATE: BCST MONTH                   
         MVC   WORK+6(2),9(R2)     GET BCST MON END   DATE                      
         MVI   WORK+8,15           SET 15TH OF MONTH                            
         GOTO1 DATCON,DMCB,(3,WORK+6),(0,WORK)                                  
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
*                                  GET BROADCAST MONTH                          
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,FENDCOMP)                             
*                                  SET END   DATE: BCST MONTH                   
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'02'           INSERT KEY TYPE                              
         MVC   KEY+20(2),5(R2)     INSERT REP CODE                              
         MVC   KEY+22(5),0(R2)     INSERT STATION CALL LETTERS                  
         GOTO1 HIGH3                                                            
         CLC   KEY(27),KEYSAVE     STATION FOUND?                               
         BE    CYCL0120            YES                                          
         MVC   P+40(22),=C'STATION NOT FOUND     '                              
         GOTO1 REPORT                                                           
         B     CYCL0360                                                         
CYCL0120 EQU   *                                                                
         GOTO1 GREC3               RETRIEVE RECORD                              
         MVC   WORK(2),RSTACLDT    GET CLOSE DATE                               
         MVI   WORK+2,1            FORCE TO 1ST OF MONTH                        
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+30)                                    
****>>>  GOTO1 HEXOUT,DMCB,RSTACLDT,P+30,2,=C'TOG'                              
         CLC   RSTACLDT,9(R2)      CLOSE DATE VS LATEST MONTH INV'D             
         BNL   CYCL0280            MONTH ALREADY CLOSED                         
         MVC   RSTACLDT,9(R2)      INSERT NEW CLOSE DATE                        
         LA    R6,RSTAREC          FIND ACTIVITY ELEMENT X'23'                  
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNZ   CYCL0160            NO ELT FOUND - INSERT FRESH ELT              
*                                  ELT FOUND: OVERRIDE DATE/FLAG                
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
*                                  GET TODAY'S DATE                             
         MVC   2(3,R6),WORK        INSERT TODAY'S DATE INTO ELEMENT             
         MVI   5(R6),X'80'         RESET 'CLOSED BY' FLAG                       
         B     CYCL0200                                                         
CYCL0160 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,NEW23ELT,0             
*                                  INSERT ACTIVITY ELT FOR STATION              
CYCL0200 EQU   *                                                                
         MVC   P+40(22),=C'CLOSE DATE CHANGED    '                              
         CLI   QOPTION3,C'U'       UPDATE REQUEST?                              
         BNE   CYCL0240            NO  - DON'T REWRITE REC WITH                 
*                                     UPDATED CLOSE DATE                        
         GOTO1 PREC                YES - REWRITE RECORD IN IO AREA              
CYCL0240 EQU   *                                                                
         B     CYCL0320                                                         
CYCL0280 EQU   *                                                                
         MVC   P+40(22),=C'CLOSE DATE NOT CHANGED'                              
CYCL0320 EQU   *                                                                
         GOTO1 REPORT                                                           
         CLI   QUESTOR+5,C'Y'      DISPLAY STATION RECORD?                      
         BNE   CYCL0360            NO                                           
         GOTO1 DISPSTA,DMCB,(RC)   DISPLAY STATION RECORD                       
CYCL0360 EQU   *                                                                
         LA    R2,11(R2)           BUMP TO NEXT ENTRY                           
         B     CYCL0040            GO BACK FOR NEXT                             
CYCL0400 EQU   *                                                                
         XIT1                                                                   
FSTRCOMP DS    XL2                 FLIGHT START:  COMPRESSED                    
FENDCOMP DS    XL2                 FLIGHT END  :  COMPRESSED                    
RECCOUNT DS    F                                                                
RECCNT2  DS    F                                                                
*                   .0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9                    
NEW23ELT DC    XL20'2314000000800040404040404040400000000000'                   
         EJECT                                                                  
******************************************************************              
*  DISPSTA:  DISPLAY STATION RECORD UPDATED                      *              
*                                                                *              
******************************************************************              
*                                                                               
DISPSTA  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
         LA    R4,RECORD2          A(RECORD LENGTH FIELD)                       
         ZICM  RF,RSTALEN,2        INSERT LENGTH                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
READ3    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY3                                                         
SEQ3     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY3                                                         
HIGH3    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY3                                                         
ADD3     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY3                                                         
WRITE3   MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY3                                                         
DIRCTRY3 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         B     RGENIOD3                                                         
GREC3    MVC   COMMAND(8),GETREC                                                
         B     FILE3                                                            
PREC3    MVC   COMMAND(8),PUTREC                                                
         B     FILE3                                                            
AREC3    MVC   COMMAND(8),ADDREC                                                
         B     FILE3                                                            
FILE3    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
*                  DATA MANAGER ERRORS AND EXIT                                 
                                                                                
RGENIOD3 OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084REREPKA02 10/16/07'                                      
         END                                                                    
