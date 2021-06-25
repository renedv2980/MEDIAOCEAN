*          DATA SET RECNT7ES   AT LEVEL 015 AS OF 05/10/04                      
*          DATA SET RECNT7EC   AT LEVEL 056 AS OF 05/10/01                      
*PHASE T8027EB,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T8027E - RECNT7E - ENTERPRISE EC CHANGES'                       
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT7E --- ENTERPRISE ELECTRONIC CONTRACT INTERFACE       *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* OCT07/93 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* NOV24/93 (BU ) --- CHANGE REPORT CLASS TO 'G'                     *           
*                                                                   *           
* DEC01/93 (SKU) --- ADD EDICT HEADER CARDS                         *           
*                                                                   *           
* FEB01/94 (SKU) --- CHANGE EDICT PROG TYPE TO 'E' AND POWER CODE   *           
*                                                                   *           
* FEB04/94 (BU ) --- CORRECT TIME DISPLAY FOR 2400-2459 HOURS       *           
*                                                                   *           
* FEB28/94 (BU ) --- CORRECT ERROR IN SPOT/WEEK COMPARISON ON       *           
*                    COLLAPSING.                                    *           
*                                                                   *           
* MAR14/94 (SKU) --- REMOVE REP AND OFF BEFORE *HDR* FOR BILLING    *           
*                                                                   *           
* MAR31/94 (BU ) --- CONTRACT TYPE ALWAYS TO BE 'S'                 *           
*                                                                   *           
* APR01/94 (BU ) --- TIME-STAMP:  ADJUST HOURS...                   *           
*                                                                   *           
* JUN29/94 (BU ) --- FIX ALTERNATING WEEK PROBLEM.                  *           
*                                                                   *           
* SEP14/94 (BU ) --- ADJUST MIDNIGHT TIME PRESENTATION              *           
*                                                                   *           
* JAN04/95 (SKU) --- FORCE CURSOR TO CONCACT INCASE OF REC FULL ERR *           
*                                                                   *           
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                             *           
*                                                                   *           
* 26JUL96 (SKU) --- CHANGE TO USE THMS FOR TIME STAMPING            *           
*                                                                   *           
* NOV0896 (BU ) --- ADD ESTIMATE NUMBER FIELD TO OUTPUT             *           
*                                                                   *           
* JUL0997 (BU ) --- UPGRADE FOR YR 2000                             *           
*                                                                   *           
* JUL3197 (SKU) --- NEED TO CLEAR TOTLDOLS BEFORE USING             *           
*                                                                   *           
* OCT0797 (BU ) --- THREE-CHAR STATION CALLS                        *           
*                                                                   *           
* MAR0998 (BU ) --- TWO STATIONS GENERATE 'LOCAL' ORDERS: WKYC/WJBK *           
*                                                                   *           
* APR2098 (BU ) --- MAKE IT THREE:  KDFW/FK                         *           
*                                                                   *           
* APR2498 (BU ) --- ACTIVATE 'LOCAL' E/C FROM STATION RECORD        *           
*                                                                   *           
* FEB2299 (BU ) --- CONTRACT TYPE:  SEND RCONTYP                    *           
*                                                                   *           
* FEB2299 (BU ) --- CONTRACT TYPE:  SET IT BACK                     *           
*                                                                   *           
* MAR1599 (BU ) --- NEW 'LOCAL' SETTING FROM TWAFLAGS               *           
*                                                                   *           
* JUL2699 (BU ) --- SKIP CANCELLED BUYLINES                         *           
*                                                                   *           
* OCT279  (BU ) --- ENHANCED EC:  ADD NBC FIELDS                    *           
*                                                                   *           
* APR2400 (BU ) --- EC CHANGES VERSION                              *           
*                                                                   *           
* APR0501 (BU ) --- TRADE FLAG CORRECTLY SET                        *           
*                                                                   *           
* MAY10/01 (BU ) --- PROGRAM NAME: SEPARATE BUY COMMENT RECORD      *           
*                                                                   *           
* MAY10/01 (BU ) ---ENHANCED EC:  ADD NBC FIELDS: FIELDS ADDED NEW  *           
*                   TO THIS VERSION OF EC CHANGES                   *           
*                                                                   *           
* FEB25/04 (BU ) ---SEND ZEROS FOR 'VARIOUS' TIMES                  *           
*                                                                   *           
* MAY07/04 (BU ) ---FORMAT 'Z' - SEND 'BDE'                         *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T8027E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T8027E*,R9,RR=R8                              
         ST    R8,RELX                                                          
         LR    R3,RC               SET A(MODULE WORK SPACE)                     
         USING LOCALWRK,R3         SET DSECT FOR AREA                           
         LR    R7,R3                                                            
         A     R7,=F'4096'         SET NEXT LOCALWRK USING                      
         USING LOCALWRK+4096,R7                                                 
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
*                                  READ STATION RECORD INTO AIO3                
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(FINDID),DMCB,(RC),RR=YES      FIND SEND ID                    
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(PQOPENA),DMCB,(RC),(R8),RR=YES                                
*                                  OPEN PRINT QUEUE                             
*                                                                               
****>>>                                                                         
*                                                                               
         GOTO1 =A(EDICT),DMCB,(RC),(R8),RR=YES                                  
*                                  CREATE EDICT HEADER CARDS                    
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
         GOTO1 =A(TOTLBUYS),DMCB,(RC),RR=Y                                      
*                                                                               
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         'EORTYP' CONTROL RECORD                      
*                                                                               
MN300    EQU   *                                                                
         BAS   RE,BLD02REC         'E1RTYP' ORDER HEADER                        
*                                                                               
MN400    EQU   *                                                                
         XC    MGGROUP#,MGGROUP#   RESET COUNTER TO ZERO                        
*                                                                               
         BAS   RE,BLD03REC         '0203' LINE ADD                              
*                                                                               
MN600    EQU   *                                                                
         BAS   RE,DATETIME         DATE AND TIME STAMP                          
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
                                                                                
*********************************************************************           
*        BLD01REC  ---  BUILDS THE 'EORTYP' CONTROL RECORD                      
*********************************************************************           
BLD01REC NTR1                                                                   
         MVI   EORTYP,C' '         SPACE FILL THE RECORD                        
         MVC   EORTYP+1(200),EORTYP                                             
         MVC   EORTYP+201(119),EORTYP+200                                       
         MVI   EORTYP,C'C'         INSERT RECORD TYPE                           
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',WORK)                                
*                                  GET DATE IN EBCDIC FORMAT                    
         MVC   WORK+6(2),WORK      MOVE YY TO END                               
         MVC   EODATE,WORK+2       INSERT DATE                                  
*                                                                               
*******************************************************************             
*   ORIGINAL TIME CALCULATION TAKEN OUT, REPLACED BY THMS CALL                  
*                                                                               
*        TIME  DEC                                                              
*                                  MASSAGE THE DATE TO HHMMSS                   
*                                     NORMAL CLOCK TIME                         
*        STCM  R0,14,WORK          GET HHMMSS - LEAVE FRACTIONS                 
*                                      OF A SECOND                              
*        GOTO1 HEXOUT,DMCB,WORK,EOTIME,3,=C'TOG'                                
*                                  INSERT HHMMSS                                
*        PACK  DUB,EOTIME(2)       PACK HOURS                                   
*        CVB   R0,DUB              CONVERT RESULT TO BINARY                     
*        LA    R0,DDSTMADJ(R0)     ADJUST TO REGULAR TIME FROM DDS              
*        EDIT  (R0),(2,EOTIME),FILL=0                                           
*******************************************************************             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK           GET HHMMSS - LEAVE FRACTIONS                 
*                                      OF A SECOND                              
         PRINT GEN                                                              
         GOTO1 HEXOUT,DMCB,WORK,EOTIME,3,=C'TOG'                                
*                                  INSERT HHMMSS                                
         PRINT NOGEN                                                            
*                                                                               
         MVC   EOCLNT(4),RCONKSTA  INSERT STATION NAME                          
         MVC   EOCLNT+4(3),=C'-TV' INDICATE TELEVISION                          
         CLI   EOCLNT+3,C' '       LAST CHAR OF STN = SPACE?                    
         BNE   BLD10020            NO                                           
         MVC   EOCLNT+3(4),=C'-TV ' YES - MOVE '-T' TO LEFT                     
BLD10020 EQU   *                                                                
         MVC   EOREP(7),REPNAME    INSERT REP NAME                              
         MVC   EORCN,FOXZEROS      ZERO OUT                                     
         MVC   EORTN,FOXZEROS         CONTROL RECORD                            
         MVC   EORCV,FOXZEROS            COUNTERS                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
*                                     IF PRESENT                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        BLD02REC  ---  BUILDS THE 'E1RTYP' CONTRACT HEADER RECORD              
*********************************************************************           
BLD02REC NTR1                                                                   
         MVI   E1RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E1RTYP+1(200),E1RTYP                                             
         MVC   E1RTYP+201(119),E1RTYP+200                                       
         MVI   E1RTYP,C'H'         INSERT RECORD TYPE                           
         GOTO1 HEXOUT,DMCB,RCONKCON,E1RREF,4,=C'TOG'                            
*                                  INSERT CONTRACT NUMBER                       
         MVC   E1RREV(3),FOXZEROS  CLEAR REVISION #/BUYLINE#                    
         MVI   E1SUB,C' '          SPACE-FILL SUBCLASS                          
         MVI   E1ADSR,C'R'         ADVERTISER = REP CODE                        
         MVC   E1ADID(4),RCONKADV  INSERT ADVERTISER CODE                       
         MVC   E1ADNM(20),ADVNAME  INSERT ADVERTISER NAME                       
         MVI   E1TRAN,C'A'         INSERT TRANSACTION TYPE                      
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,DUB                       
         GOTO1 DATCON,DMCB,(0,WORK2),(X'20',WORK)                               
         MVC   WORK+6(2),WORK      MOVE YR TO END                               
         MVC   E1STDT,WORK+2       INSERT MMDDYY FLIGHT START                   
         GOTO1 DATCON,DMCB,(0,WORK2+6),(X'20',WORK)                             
         MVC   WORK+6(2),WORK      MOVE YR TO END                               
         MVC   E1ENDT,WORK+2       INSERT MMDDYY FLIGHT END                     
         MVI   E1CLS,C'1'          INSERT CLASS = NATL                          
         CLC   RCONKREP,=C'JB'     WJBK LOCAL REP?                              
         BNE   BLD20020            NO                                           
         CLC   =C'WJBK',RCONKSTA   YES - WJBK?                                  
         BNE   BLD20020            NO                                           
         MVI   E1CLS,C'2'          YES - SET CLASS TO 'LOCAL'                   
BLD20020 EQU   *                                                                
         CLC   RCONKREP,=C'87'     WKYCL LOCAL REP?                             
         BNE   BLD20040            NO                                           
         CLC   =C'WKYC',RCONKSTA   YES - WKYC?                                  
         BNE   BLD20040            NO                                           
         MVI   E1CLS,C'2'          YES - SET CLASS TO 'LOCAL'                   
BLD20040 EQU   *                                                                
         CLC   RCONKREP,=C'FK'     KDFWL LOCAL REP?                             
         BNE   BLD20060            NO                                           
         CLC   =C'KDFW',RCONKSTA   YES - KDFW?                                  
         BNE   BLD20060            NO                                           
         MVI   E1CLS,C'2'          YES - SET CLASS TO 'LOCAL'                   
BLD20060 EQU   *                                                                
         TM    LOCXOPT,X'10'       'LOCAL' ORDER FROM STATION RECORD?           
         BNO   BLD20080            NO                                           
         MVI   E1CLS,C'2'          YES - SET CLASS TO 'LOCAL'                   
BLD20080 EQU   *                                                                
         TM    TWAFLAGS,TWAFLHMQ   'LOCAL' ORDER FROM EITHER                    
*                                     PROF 38 OR NBC SIGNON?                    
         BNO   BLD20090            NO                                           
         MVI   E1CLS,C'2'          YES - SET CLASS TO 'LOCAL'                   
BLD20090 EQU   *                                                                
         MVC   E1PROD,PRODNAME     INSERT PRODUCT NAME                          
         MVI   E1RPSR,C'R'         INSERT REP   SOURCE                          
         MVI   E1OFSR,C'R'         INSERT OFF   SOURCE                          
         MVC   E1REP,=C'001'       INSERT REP # (ALWAYS 001)                    
         MVI   E1SLSR,C'R'         INSERT S/P SOURCE                            
         MVC   E1OFF(2),RCONKOFF   INSERT OFFICE CODE                           
         MVC   E1SLS(3),RCONSAL    INSERT SALESPERSON CODE                      
         MVC   E1SLNM(20),SALNAME  INSERT SALESPERSON NAME                      
         MVI   E1CNTP,C'S'         INSERT CONTRACT TYPE (ALWAYS S)              
         LA    R6,RCONREC          A(COMMENT RECORD)                            
         MVI   ELCODE,X'1E'        LOOK FOR RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL            GET ELEMENT                                  
         BNE   BLD20095            NOT FOUND                                    
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   BLD20095            NO                                           
         MVI   E1CNTP,C'T'         INSERT 'TRADE ORDER'                         
BLD20095 EQU   *                                                                
***      MVC   E1CNTP,RCONTYPE     INSERT CONTRACT TYPE                         
         MVI   E1AGSR,C'R'         INSERT AGENCY SOURCE                         
         MVC   E1AFID,RCONKAGY     INSERT AGENCY CODE                           
         MVC   E1AFNM(20),AGYNAME  INSERT AGENCY NAME                           
*                                                                               
*   E1AGZP HAS BEEN RENAMED.  ZIP CODE IS NO LONGER BEING XFERRED               
*                                                                               
***      MVC   E1AGZP,ZIPCODE      INSERT ZIP CODE                              
         MVC   E1BYNUM(20),RCONBUYR                                             
*                                  INSERT BUYER NAME                            
         EDIT  TOTLDOLS,(13,E1CKSUM),FILL=0                                     
         EDIT  TOTLSPTS,(05,E1TLSP),FILL=0                                      
*                                                                               
*   RETRIEVE A2 ELEMENT TO GET ESTIMATE NUMBER.  CHECK ORIGINAL                 
*        FIELD, THEN EXPANDED FIELD.                                            
*                                                                               
         LA    R6,RCONREC          A(COMMENT RECORD)                            
         MVI   ELCODE,X'A2'        LOOK FOR EASI CODE ELEMENT                   
         BAS   RE,GETEL            GET ELEMENT                                  
         BNE   BLD20140            NOT FOUND                                    
         USING RCONIEL,R6                                                       
         OC    RCONIEST,RCONIEST   ORIGINAL FIELD:  EMPTY?                      
         BZ    BLD20100            YES - CHECK NEW FIELD                        
         CLC   RCONIEST,SPACES     ORIGINAL FIELD: SPACES?                      
         BE    BLD20100            YES - CHECK NEW FIELD                        
         MVC   E1AGES(4),RCONIEST  NO  - USE 4-CHAR EST #                       
         B     BLD20120                                                         
BLD20100 EQU   *                                                                
         OC    RCONXEST,RCONXEST   NEW FIELD:  EMPTY?                           
         BZ    BLD20120            YES - NO ESTIMATE NUMBER IN ORDER            
         CLC   RCONXEST,SPACES     NEW FIELD: SPACES?                           
         BE    BLD20120            YES - NO ESTIMATE NUMBER IN ORDER            
         MVC   E1AGES(10),RCONXEST NO  - USE 10-CHAR EST #                      
BLD20120 EQU   *                                                                
         MVC   E1AGAD(4),RCONIADV  INSERT EASI ADV CODE                         
         OC    E1AGAD,SPACES       CLEAR BINARY TO SPACES                       
         MVC   E1AGP1,RCONIPRD     INSERT 1ST PRODUCT CODE                      
         OC    E1AGP1,SPACES       CLEAR BINARY TO SPACES                       
         MVC   E1AGP2,RCONIPR2     INSERT 2ND PRODUCT CODE                      
         OC    E1AGP2,SPACES       CLEAR BINARY TO SPACES                       
BLD20140 EQU   *                                                                
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
         BAS   RE,BLD02COM         ADD CONTRACT COMMENTS                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   IF CONTRACT COMMENTS ARE PRESENT, COMMENT RECORDS ARE GENERATED.            
*                                                                               
BLD02COM NTR1                                                                   
         LA    R6,RCONREC          A(COMMENT RECORD)                            
         MVI   ELCODE,X'02'        LOOK FOR CONTRACT COMMENT                    
BLDC0020 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLDC0060                                                         
BLDC0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLDC0060 EQU   *                                                                
         BNE   BLDC0120            NOT FOUND:  COMMENT TYPE DONE                
         MVI   E3RTYP+15,C' '      SPACE FILL THE RECORD                        
*                                     AFTER THE CONTROL INFO                    
         MVC   E3RTYP+16(185),E3RTYP+15                                         
         MVC   E3RTYP+201(119),E3RTYP+200                                       
         MVI   E3RTYP,C'M'         INDICATE COMMENT TYPE                        
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,2                                                             
         SR    RF,RE               SUBTRACT L(CONTROL)                          
*                                                                               
         EX    RF,BLDC0900         MOVE COMMENT                                 
*                                                                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
         B     BLDC0040            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLDC0120 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
BLDC0900 MVC   E3COMM(0),2(R6)                                                  
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE 'E2RTYP' BUYLINE RECORD.                           
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE '0203' RECORD PER EFFECTIVE DATE IS GENERATED.                   
*      2.  COMMENTS ARE GEN'D AS '0204' RECORDS                                 
*                                                                               
BLD03REC NTR1                                                                   
         XC    EXTRAKEY,EXTRAKEY   CLEAR EXTRAKEY AREA                          
*                                                                               
*                                                                               
*   NOTE:  SVBYLN# IS NOW BEING USED TO SET THE DATED REVISION #                
*        IN THE OUTPUT.  E2DRNR WILL DIFFERENTIATE THE 'SUBLINE #S'             
*                                                                               
*   NOTE:  THE COMMENT BELOW.  THIS MUST BE RECTIFIED IN EC CHANGES.            
*        BUYLINE NUMBERING MUST BE IN AGREEMENT.                                
*   NOTE RE:  SVBYLN#.  OUTPUT BUYLINE NUMBERS WILL NOT NECESSARILY             
*      CORRESPOND WITH DDS NUMBERS.  IF A BUYLINE CONTAINS MULTIPLE             
*      EFFECTIVE DATE ELEMENTS THAT CANNOT BE REDUCED TO A SINGLE               
*      ENTRY, EACH WILL GENERATE A CONSECUTIVELY NUMBERED OUTPUT                
*      RECORD.  FOR EXAMPLE:  BUY # 1 CONTAINS 2 EFF DATE ELEMENTS,             
*      BUY # 2 CONTAINS 1 EFF DATE ELEMENT.  THE OUTPUT WILL CONSIST            
*      OF THREE RECORDS:  # 1 =  BUY # 1, EFF DATE # 1                          
*                         # 2 =  BUY # 1, EFF DATE # 2                          
*                         # 3 =  BUY # 2, EFF DATE # 1.                         
*      ENTERPRISE HAS INDICATED THAT THEY HAVE A METHOD TO KEEP TRACK           
*      OF WHAT BELONGS TO WHAT.   BILL UHR (OCT27/93, AFTER A CONVERSE          
*      WITH DRAKE RINESMITH OF ENTERPRISE.)                                     
*                                                                               
BLD30020 EQU   *                                                                
         XC    ANXTMG,ANXTMG       CLEAR A(NEXT MG ELT)                         
         MVI   SETMKG,0            CLEAR 'SET M/G TABLE' FLAG                   
         XC    SVBYLN#,SVBYLN#     CLEAR DATED REV# COUNT                       
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30460            NO MORE BUYS - EXIT                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'56'        ANY M/G MISSED SPOT ELTS?                    
         BAS   RE,GETEL                                                         
         BNE   BLD30040            NO                                           
         MVI   SETMKG,1            YES - SET 'SET M/G TABLE' FLAG               
BLD30040 EQU   *                                                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    BLD30020            YES - SKIP IT                                
***>>    GOTO1 =A(MERGE03S),DMCB,(RC),(1,LTRUNTAB),RR=Y                         
*                                  REMERGE ANY LATE-RUN SPOTS                   
*                                     TO RESET TO ORIGINAL BUY                  
         GOTO1 =A(CRDIT16S),DMCB,(RC),RR=Y                                      
*                                  REMERGE ANY CREDIT SPOTS                     
*                                     TO RESET TO ORIGINAL BUY                  
         GOTO1 =A(LOGCREDS),DMCB,(RC),(R3),RR=Y                                 
*                                  SET UP CREDIT LOG                            
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   E2RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E2RTYP+1(200),E2RTYP                                             
         MVC   E2RTYP+201(119),E2RTYP+200                                       
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'20'        GET PATTERN ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   BLD30050            NOT FOUND                                    
         USING RBUYPTEL,R6                                                      
         MVC   E2SPNM,SPACES       CLEAR PATTERN NAME                           
         MVC   E2SPNM,RBUYPTPT     INSERT PATTERN NAME                          
         MVC   E2NOTF,RBUYPTNT     INSERT NOTATION                              
         MVI   E2TUSE,C' '         SET 'USE E2STTM/E2ENTM' TIMES                
         TM    RBUYPTFL,X'80'      USE SELL PATTERN TIMES?                      
         BNO   BLD30050            NO                                           
         MVI   E2TUSE,C'S'         YSE - SET 'USE PATTERN TIMES'                
*                                                                               
         DROP  R6                                                               
*                                                                               
BLD30050 EQU   *                                                                
         GOTO1 BLD3MKGD,DMCB,(RC)  SET MG + COMMENT FLAGS                       
*                                                                               
*                                                                               
**>>     OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
**<<     BNZ   BLD30060            NO                                           
*                                                                               
*   ZEROSPTS WAS NEVER BEING SET TO ANY VALUE BUT ZERO.  AS A RESULT,           
*        COMFLAGS M/G SWITCH WAS ALWAYS TURNED OFF.  THIS WAS NEVER             
*        REPORTED AS A PROBLEM.  TO ENSURE THAT NO NEW DIFFICULTIES             
*        ARE INTRODUCED, THE M/G SWITCH TURN OFF CODE IS ALWAYS DONE.           
*                                                                               
         NI    COMFLAGS,X'7F'      YES - TURN OFF M/G SWITCH                    
BLD30060 EQU   *                                                                
         MVI   E2RTYP,C'B'         INSERT RECORD TYPE                           
         GOTO1 HEXOUT,DMCB,RCONKCON,E2RREF,4,=C'TOG'                            
*                                  INSERT CONTRACT NUMBER                       
         MVC   E2RREV,FOXZEROS     CLEAR REVISION #                             
         MVC   E2DRNR(06),FOXZEROS CLEAR DATED REV#/ADD'L CL#                   
         MVC   E2PRDR,FOXZEROS     CLEAR PRE-EMPT DATED REV#                    
         MVC   E2PRLN,FOXZEROS     CLEAR PRE-EMPT LINE#                         
         MVC   E2PRDT,FOXZEROS     CLEAR PRE-EMPT DATE                          
         MVC   E2MGGP,FOXZEROS     CLEAR M/G GROUP #                            
         MVI   E2TRAN,C'A'         INSERT TRANSACTION TYPE                      
         EDIT  RBUYCOS,(9,E2COST),FILL=0                                        
*                                  INSERT SPOT COST                             
         MVC   E2RSEC,RBUYSEC      INSERT RATE SECTION                          
*                                     FIRST TWO CHARACTERS ONLY                 
*                                                                               
*   EACH D/T STRING WILL BE TREATED, TO CONSTRUCT AN ORBIT                      
*                                                                               
***      CLI   DTSTRNGS,2          HOW MANY D/T STRINGS?                        
***      BL    BLD30100            ONE                                          
***      MVC   E2FILL(15),=C'MULTI DAY/TIMES'                                   
*                                     SET 'PATTERN RECORDS FOLLOW'              
***      B     BLD30120                                                         
BLD30100 EQU   *                                                                
         GOTO1 DYTIME,DMCB,(RC)    NTH CODE:  GET/TRANSLATE IT                  
BLD30120 EQU   *                                                                
         XC    FIRSTSW,FIRSTSW     INITIALIZE 'ALTERNATE WEEK' IND              
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        GET EFFECTIVE DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   BLD30380            NOT FOUND                                    
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         USING RBUYDTCD,R6                                                      
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK INDIC SET?                    
         BNO   BLD30180            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
*                                                                               
         DROP  R6                                                               
*                                                                               
*   LOOP THROUGH MULTIPLE EFFECTIVE DATES                                       
*                                                                               
BLD30180 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      ANY MORE '03' ELEMENTS?                      
         BE    BLD30380            NO                                           
         L     R6,AEFFDATE         RESET A(EFFECTIVE DATE ELEMENT)              
         ZIC   RF,1(R6)            L(EFFECTIVE DATE ELEMENT)                    
         BCTR  RF,0                DECREMENT 1                                  
         EX    RF,BLD30200         MOVE EFF DATE TO TABLE                       
         B     BLD30220                                                         
*                                                                               
BLD30200 MVC   BLDTABLE(0),0(R6)                                                
*                                                                               
BLD30220 EQU   *                                                                
         GOTO1 DTEINFO,DMCB,(RC),(R6)                                           
         CLI   SPOTSWK,0           ANY SPOTS IN THIS WEEK?                      
         BZ    BLD30360            NO  - DON'T PROCESS BUY                      
*****    EDIT  RBUYNW,(3,E2NRWK),FILL=0                                         
         EDIT  ORIGSPTS,(3,E2NRWK),FILL=0                                       
*                                  INSERT NUMBER PER WEEK                       
*                                  USE SPOTS FROM EFF DATE ELT,                 
*                                        NOT BUYLINE: MAY DIFFER                
         MVI   E2ALTW,C'1'         SET WEEK INDICATOR TO EVERY                  
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   E2ALTW,C'2'         SET WEEK INDICATOR TO ALTERNATE              
BLD30240 EQU   *                                                                
         TM    ORIGALTW,X'40'      ORIGINAL DATE RUN INDICATOR                  
*                                     SET TO 'ALTERNATE WEEKS'?                 
         BNO   BLD30260            NO                                           
         MVI   E2ALTW,C'2'         YES - SET WEEK IND TO ALTERNATE              
BLD30260 EQU   *                                                                
         ZIC   RF,MULTEFDT                                                      
         LA    RF,1(RF)            ADD TO # EFF DATES                           
         STC   RF,MULTEFDT         SAVE IT BACK                                 
         EDIT  RBUYKLIN,(5,E2BYLN),FILL=0,ZERO=NOBLANK                          
         EDIT  SVBYLN#,(3,E2DRNR),FILL=0,ZERO=NOBLANK                           
*                                  INSERT NEW DATED REV  #                      
         MVC   E2PRDR,FOXZEROS     CLEAR PRE-EMPT DATED REV#                    
         MVC   E2PRLN,FOXZEROS     CLEAR PRE-EMPT LINE#                         
         MVC   E2PRDT,FOXZEROS     CLEAR PRE-EMPT DATE                          
         MVC   E2MGGP,FOXZEROS     CLEAR M/G GROUP #                            
         MVC   MGBUYLIN,RBUYKLIN   SAVE BUYLINE #                               
         MVC   MGDRNR,SVBYLN#      SAVE DATED REVISION NUMBER                   
*                                                                               
         ZIC   RF,SVBYLN#          BUMP BUYLINE COUNT                           
         LA    RF,1(RF)                                                         
         STC   RF,SVBYLN#          PUT IT BACK                                  
*                                                                               
*   DETERMINE IF BUYLINE IS 'OLD-STYLE' (CR=) CREDIT BUY.  IF SO,               
*        TRANSFORM OUTPUT INTO E2TRAN=CP, MODIFY LINE NUMBER,                   
*        AND SET ADDITIONAL INFORMATION                                         
*                                                                               
         GOTO1 OLDCRED,DMCB,(RC)                                                
*                                                                               
*   DETERMINE IF BUYLINE IS M/G. IF SO, ADD APPROPRIATE                         
*        M/G DATA, SET E2TRAN=AM.                                               
*                                                                               
         GOTO1 =A(MAKEGOOD),DMCB,(RC),RR=Y                                      
DIE2     EQU   *                                                                
*                                                                               
         CLI   SETMKG,1            SET M/G TABLE?                               
         BNE   BLD30340            NO                                           
         GOTO1 =A(SETMGTAB),DMCB,(RC),RR=Y                                      
BLD30340 EQU   *                                                                
         TM    RBUYRTS,X'08'       LATE RUN BUY?                                
         BO    BLD30360            YES - DON'T OUTPUT IT                        
         TM    RBUYRTS,X'04'       LATE RUN W/BONUS BUY?                        
         BNO   BLD30350            NO  - PROCESS IT                             
         TM    RBUYRTS,X'20'       YES - BONUS OF LR W/BONUS?                   
         BNO   BLD30360            NO  - LATE RUN PORTION: SKIP IT              
BLD30350 EQU   *                                                                
         CLI   DTSTRNGS,2          HOW MANY D/T STRINGS?                        
         BL    BLD30355            ONE - CL # STAYS ZERO                        
         MVI   E2CLLN+2,C'1'       MORE: FIRST CL # SET TO 1                    
BLD30355 EQU   *                                                                
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
         CLC   DTSTRNG#,DTSTRNGS   ALL D/T STRINGS PROCESSED?                   
         BE    BLD30360            YES - CONTINUE                               
         BAS   RE,MORSTRNG         NO  - OUTPUT REMAINING STRINGS               
BLD30360 EQU   *                                                                
         XC    BLDTABLE,BLDTABLE                                                
         MVC   E2STDT(12),SPACES   CLEAR DATES                                  
         MVC   E2PRLN(11),SPACES   CLEAR PRE-EMPT INFO                          
         MVI   FIRSTSW,0           CLEAR FIRSTSW                                
         B     BLD30180            GET NEXT EFF DATE ELEMENT                    
BLD30380 EQU   *                                                                
         OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
         BNZ   BLD30400            NO  - CONTINUE                               
         CLI   MULTEFDT,2          IF ZERO SPOTS AND ONLY 1                     
         BL    BLD30420               EFF DATE, DON'T SEND COMMENTS             
BLD30400 EQU   *                                                                
         OC    COMFLAGS,COMFLAGS   ANY COMMENTS EXPECTED?                       
         BZ    BLD30420            NO  -                                        
         TM    RBUYRTS,X'08'       LATE RUN BUY?                                
         BO    BLD30420            YES - NO COMMENTS                            
         TM    RBUYRTS,X'04'       LATE RUN W/BONUS BUY?                        
         BNO   BLD30410            NO  - PROCESS IT                             
         TM    RBUYRTS,X'20'       YES - BONUS OF LR W/BONUS?                   
         BNO   BLD30420            NO  - LATE RUN PORTION: SKIP IT              
BLD30410 EQU   *                                                                
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD CMMTS/MG AS APPROP.                
         MVI   E3RTYP,C'B'         RESET TYPE TO 'BUY'                          
BLD30420 EQU   *                                                                
         MVI   ZEROSPTS,0          SET TO ZERO SPOTS                            
         GOTO1 =A(GENCREDS),DMCB,(RC),RR=Y                                      
*                                  PUT OUT CREDITS, IF NEEDED                   
DIE      EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLI   ORIGBUYF,1                                                       
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*   END TEST                                                                    
*                                                                               
         CLI   GENUNMAT,0          GENERATE UNMATCHED PREEMPT(S)?               
         BE    BLD30440            NO                                           
*                                                                               
*   NEED TO PUT 'GENERATE UNMATCHED PREEMPTS' AT THIS POINT                     
*                                                                               
         GOTO1 =A(GENUNPRE),DMCB,(RC),RR=Y                                      
         MVI    GENUNMAT,0          RESET UNMATCHED PREEMPT FLAG                
*                                                                               
BLD30440 EQU   *                                                                
         B     BLD30020            ACCESS NEXT BUY RECORD                       
BLD30460 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETRIEVE AUXILIARY DATA, SET MG AND/OR COMMENT FLAG.                        
*        SET MULTI D/T STRING FLAGS IF PATTERN RECORDS NEEDED.                  
*                                                                               
BLD3MKGD NTR1                                                                   
         XC    DTSTRNGS,DTSTRNGS   CLEAR D/T STRING COUNTER                     
         XC    DTSTRNG#,DTSTRNG#   CLEAR OUTPUT COUNTER                         
         XC    COMFLAGS,COMFLAGS                                                
         BAS   RE,CHKLTRUN         LATE RUN COMMENTS NEEDED?                    
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   BMKG0005            NO PROGRAM NAME ELEMENT                      
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0005 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'84'        LOOK FOR ORD COMMENTS                        
         BAS   RE,GETEL                                                         
         BNE   BMKG0010            NO ORDER COMMENTS FOUND                      
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0010 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'04'        LOOK FOR CONTRACT COMMENT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0020            NO CONTRACT COMMENT FOUND                    
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0020 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'05'        LOOK FOR M/G ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   BMKG0030            NO M/G ELEMENT FOUND                         
         OI    COMFLAGS,X'80'      FOUND - SET MG FLAG                          
         MVC   CNVDATE,3(R6)       SAVE M/G START DATE                          
*                                                                               
*   IS THIS SUPPOSED TO BE M/G START DATE?????                                  
*                                                                               
BMKG0030 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'02'        COUNT EFFECTIVE D/T STRINGS                  
         BAS   RE,GETEL                                                         
         BNE   BMKG0060            NOT FOUND - DONE                             
         B     BMKG0050                                                         
BMKG0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT D/T STRING                          
         BNE   BMKG0060            NOT FOUND - DONE                             
BMKG0050 EQU   *                                                                
         ZIC   RF,DTSTRNGS                                                      
         LA    RF,1(RF)            INCREMENT                                    
         STC   RF,DTSTRNGS                                                      
*                                                                               
*   COUNT ALL DAY/TIME STRINGS                                                  
*                                                                               
****     CLI   DTSTRNGS,1          MORE THAN ONE DAY/TIME STRING?               
****     BNH   BMKG0040            NO  - LOOK FOR ANOTHER                       
*                                                                               
         B     BMKG0040            LOOK FOR ANOTHER                             
BMKG0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE 'E3RTYP' COMMENT RECORD                             
*                                                                               
*   NOTE:  THERE MUST BE COMMENTS TO PRODUCE A COMMENT RECORD                   
*                                                                               
BLD04REC NTR1                                                                   
         BAS   RE,PRTLTRUN         GENERATE LATE RUN COMMENTS                   
*                                                                               
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELEMENT                
BLD40020 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLD40060                                                         
BLD40040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLD40060 EQU   *                                                                
         BNE   BLD40120            NOT FOUND:  COMMENT TYPE DONE                
         MVI   E3RTYP,C'M'         INDICATE COMMENT TYPE                        
         MVI   E3RTYP+17,C' '      SPACE FILL THE RECORD                        
*                                     AFTER THE CONTROL INFO                    
         MVC   E3RTYP+18(183),E3RTYP+17                                         
         MVC   E3RTYP+201(119),E3RTYP+200                                       
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         CLI   ELCODE,X'84'        ORDER COMMENT?                               
         BNE   BLD40080            NO                                           
         BCTR  RF,0                YES - SUBTRACT 1 FOR                         
*                                     STATION/REP COMMENT INDICATOR             
         EX    RF,BLD40900         MOVE 1ST COMMENT (BUY)                       
         B     BLD40100                                                         
BLD40080 EQU   *                                                                
         EX    RF,BLD40920         MOVE 1ST COMMENT (ORDER)                     
BLD40100 EQU   *                                                                
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
*                                                                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
         B     BLD40040            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLD40120 EQU   *                                                                
         CLI   ELCODE,X'21'        PROGRAM NAME COMMENT DONE?                   
         BNE   BLD40130            NO  - MUST HAVE BEEN ORDER COMMENT           
*                                     (X'84' ELEMENT)                           
         LA    R6,RBUYREC          NO  - RESET A(BUY RECORD)                    
*                                     SET FOR ORDER COMMENTS                    
         MVI   ELCODE,X'84'        YES - GO BACK AND DO THEM                    
         B     BLD40020                                                         
BLD40130 EQU   *                                                                
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40140            YES - COMMENTS FINISHED                      
         LA    R6,RBUYREC          NO  - RESET A(BUY RECORD)                    
         MVI   ELCODE,X'04'        GO BACK AND DO THEM                          
         B     BLD40020                                                         
BLD40140 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
BLD40900 MVC   E3COMM(0),3(R6)                                                  
BLD40920 MVC   E3COMM(0),2(R6)                                                  
*                                                                               
         EJECT                                                                  
*********************************************************************           
*   PRTLTRUN --- SCAN LATE RUN TABLE.  IF BUYLINE IS PRESENT IN                 
*        TABLE, GENERATE A COMMENT FOR EACH LATE RUN ENTRY                      
*                                                                               
*********************************************************************           
PRTLTRUN NTR1                                                                   
         LA    R2,LTRUNTAB         SET A(LATE RUN TABLE)                        
PRTL0020 EQU   *                                                                
         CLI   0(R2),X'FF'         DELIMITER REACHED?                           
         BE    PRTL0100            YES - FINISHED                               
         CLI   0(R2),0             END OF DATA REACHED?                         
         BE    PRTL0100            YES - FINISHED                               
         CLC   RBUYKLIN,0(R2)      BUY NUMBER IN LATE RUN TABLE?                
         BE    PRTL0040            YES - GENERATE A PRINT COMMENT               
         LA    R2,LLTERUN1(R2)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     PRTL0020            GO BACK FOR NEXT                             
PRTL0040 EQU   *                                                                
         MVI   E3RTYP,C'M'         INDICATE COMMENT TYPE                        
         MVI   E3RTYP+17,C' '      SPACE FILL THE RECORD                        
*                                     AFTER THE CONTROL INFO                    
         MVC   E3RTYP+18(183),E3RTYP+17                                         
         MVC   E3RTYP+201(119),E3RTYP+200                                       
         MVC   E3COMM(11),=C'LATE RUN ON'                                       
         GOTO1 DATCON,DMCB,(3,1(R2)),(5,E3COMM+12)                              
*                                                                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
PRTL0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*   CHKLTRUN --- SCAN LATE RUN TABLE.  IF BUYLINE IS PRESENT IN                 
*        TABLE, SET COMFLGS SO ADDITIONAL COMMENTS ARE GENERATED                
*                                                                               
*********************************************************************           
CHKLTRUN NTR1                                                                   
         LA    R2,LTRUNTAB         SET A(LATE RUN TABLE)                        
CHKL0020 EQU   *                                                                
         CLI   0(R2),X'FF'         DELIMITER REACHED?                           
         BE    CHKL0100            YES - FINISHED                               
         CLI   0(R2),0             END OF DATA REACHED?                         
         BE    CHKL0100            YES - FINISHED                               
         CLC   RBUYKLIN,0(R2)      BUY NUMBER IN LATE RUN TABLE?                
         BE    CHKL0040            YES - TURN ON FLAG                           
         LA    R2,LLTERUN1(R2)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     CHKL0020            GO BACK FOR NEXT                             
CHKL0040 EQU   *                                                                
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
CHKL0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*********************************************************************           
*   INIT --- SET INITAL ADDRS AND VALUES                                        
*        NOTE:  ROUTINE RESETS R3, WHICH IS MODULE REGISTER FOR                 
*        LOCALWRK.  THIS CAN CAUSE MISUSE OF STORAGE.                           
*                                                                               
*********************************************************************           
INIT     NTR1                                                                   
*                                                                               
         MVI   TESTCTR,0           CLEAR TEST COUNTER                           
         MVI   TESTFLG,0           CLEAR TEST FLG                               
         LA    RF,BUYWORK          SET A(NEXT BUY TO READ)                      
         ST    RF,AREADBUY                                                      
         LA    RF,CRDWORK          SET A(NEXT CREDIT SLOT)                      
         ST    RF,ACRDWORK                                                      
         LA    RF,MKGWORK          SET A(NEXT MG     SLOT)                      
         ST    RF,AMKGWORK                                                      
         MVC   CRDWORKD,=X'FFFF'   SET TABLE DELIMITERS                         
         MVC   MKGWORKD(4),=X'FFFFFFFF'                                         
         MVC   LTRUNDEL(4),=X'FFFFFFFF'                                         
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
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
*        LA    RF,RBUYREC          SET A(IO AREA 2)                             
*        A     RF,=F'1000'         FOR USE WITH EDICT                           
*        ST    RF,AIO2                                                          
*        LA    RF,RBUYREC          SET A(IO AREA 3)                             
*        A     RF,=F'2000'                                                      
*        ST    RF,AIO3                                                          
         MVI   INTTYPE,C'E'        SET INPUT TYPE TO 'EC'                       
         LA    RF,SPOOLEND-SPOOLD  CLEAR THE SPOOL AREA                         
         LA    RE,SPOOLD                                                        
         XCEF                                                                   
         MVI   SPACES,C' '         INITIALIZE SPACES FIELD                      
         MVC   SPACES+1(131),SPACES                                             
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
*   RETRIEVE SUPPORT INFORMATION FROM VARIOUS RECORDS                           
*                                                                               
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,X'01'       REP RECORD                                   
         MVC   RREPKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RREPKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0000                                                         
         DC    H'0',C'MISSING REP    RECORD'                                    
         DS    0H                                                               
GCON0000 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RREPREC                                             
         MVC   REPNAME,RREPABBR    SAVE REP ABBREV NAME                         
*                                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKEY,X'0A'       AGENCY RECORD                                
         MVC   RAGYKAGY(6),RCONKAGY                                             
*                                  INSERT AGENCY/AGY OFF CODES                  
         MVC   RAGYKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RAGYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0010                                                         
         DC    H'0',C'MISSING AGENCY RECORD'                                    
         DS    0H                                                               
GCON0010 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RAGYREC                                             
         MVC   AGYNAME,RAGYNAM1    SAVE AGENCY NAME                             
         MVC   ZIPCODE,RAGYZIP     SAVE ZIP CODE                                
*                                                                               
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKEY,X'08'       ADVERT RECORD                                
         MVC   RADVKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RADVKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RADVKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0020                                                         
         DC    H'0',C'MISSING ADVERT RECORD'                                    
         DS    0H                                                               
GCON0020 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RADVREC                                             
         MVC   ADVNAME,RADVNAME    SAVE ADVERT NAME                             
*                                                                               
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKEY,X'06'       S/P    RECORD                                
         MVC   RSALKSAL,RCONSAL    INSERT SALESPERSON                           
         MVC   RSALKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RSALKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0030                                                         
         DC    H'0',C'MISSING S/P    RECORD'                                    
         DS    0H                                                               
GCON0030 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RSALREC                                             
         MVC   SALNAME,RSALNAME    SAVE S/P    NAME                             
*                                                                               
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKEY,X'04'       OFFICE RECORD                                
         MVC   ROFFKOFF,RCONKOFF   INSERT OFFICE CODE                           
         MVC   ROFFKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,ROFFKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0040                                                         
         DC    H'0',C'MISSING OFFICE RECORD'                                    
         DS    0H                                                               
GCON0040 EQU   *                                                                
         GOTO1 VGETREC,DMCB,ROFFREC                                             
         MVC   OFFNAME,ROFFNAME    SAVE OFFICE NAME                             
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
READBUY  NTR1                                                                   
         MVC   KEY,EXTRAKEY        INITIALIZE OR RESET KEY                      
         OC    EXTRAKEY,EXTRAKEY   ANY PRIOR KEY?                               
***>>>   BNZ   REBU0020            YES - DO SEQ READ                            
         LA    RF,KEY                                                           
         USING RBUYREC,RF                                                       
         MVI   RBUYKEY,X'0B'       INSERT ID                                    
         MVC   RBUYKREP,REPALPHA                                                
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RE                                                       
         MVC   RBUYKCON,TWACNUM                                                 
REBU0020 EQU   *                                                                
         L     R1,AREADBUY         SET A(NEXT BUY RECORD)                       
         OC    0(6,R1),0(R1)       ANY ENTRY?                                   
         BNZ   REBU0060            YES                                          
         SR    R0,R0               NO  - EXIT W/CC = ZERO                       
         LTR   R0,R0                                                            
         B     REBU0100            GO BACK                                      
REBU0060 EQU   *                                                                
         MVC   RBUYKPLN(5),1(R1)   INSERT PLAN+MASTER+DETAIL #S                 
         LA    R1,LBUYWORK(R1)     BUMP TO NEXT BUY IN TABLE                    
         ST    R1,AREADBUY                                                      
         DROP  RE,RF                                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     COMPARE ENTIRE KEY                           
         BE    REBU0080            BUY FOUND                                    
         DC    H'0'                KEY MUST BE THERE!!                          
REBU0080 EQU   *                                                                
         MVC   EXTRAKEY(27),KEY    SAVE KEY FOUND                               
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         LTR   RB,RB               BUY FOUND:  CC NOT = ZERO                    
REBU0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   OUTPUT 2ND THRU NTH D/T STRING DATA AS ADDITIONAL CL LINE #S.               
*        THE RECORD CURRENTLY IN THE PRINT AREA IS TO SERVE AS                  
*        THE BASIS FOR ADDL RECORDS                                             
*                                                                               
MORSTRNG NTR1                                                                   
MSTR0020 EQU   *                                                                
         CLC   DTSTRNG#,DTSTRNGS   ALL STRINGS PROCESSED?                       
         BE    MSTR0900            YES                                          
         MVC   E2NRMO(14),SPACES   CLEAR ROTATOR FIELD                          
         MVC   E2PRLN,FOXZEROS     CLEAR ANY PREEMPT LINE #                     
         MVC   E2PRDT,FOXZEROS     CLEAR ANY PREEMPT DATE                       
         ZIC   (RF),DTSTRNG#                                                    
         LA    RF,1(RF)                                                         
         EDIT  (RF),(3,E2CLLN),FILL=0                                           
         BAS   RE,DYTIME                                                        
*                                                                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                                                               
*   NEED TO INCREMENT CL LINE # HERE SOMEWHERE                                  
*                                                                               
         B     MSTR0020            GO BACK FOR POSSIBLE NEXT                    
MSTR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
                                                                                
*                                                                               
*   TRANSLATE DAY AND TIME TO JDS FORMAT.                                       
*                                                                               
DYTIME   NTR1                                                                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
         LA    R6,RBUYREC          A(BUY RECORD)                                
         ZIC   RF,DTSTRNG#                                                      
         LA    RF,1(RF)            BUMP STRING COUNTER                          
         STC   RF,DTSTRNG#         REPLACE STRING #                             
         LR    R0,RF                                                            
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         B     DYTI0020                                                         
DYTI0010 EQU   *                                                                
         BAS   RE,NEXTEL           GET FIRST DAY/TIME ELEMENT                   
DYTI0020 EQU   *                                                                
         BNE   DYTI0280            NOT FOUND - EXIT                             
         BCT   R0,DYTI0010         GET NTH ELEMENT                              
*                                                                               
**       CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
**       BL    DYTI0040            NO  - ONLY 1                                 
*                                                                               
*   FOR EACH LOOP, THE 'NTH' D/T STRING SET WILL BE PROCESSED                   
*                                                                               
***      MVC   E2FILL(15),=C'MULTI DAY/TIMES'                                   
*                                  NO CALENDAR FIELDS WILL BE SET               
***      MVC   E2NRMO(14),SPACES   CLEAR ROTATOR FIELD                          
***      B     DYTI0080                                                         
DYTI0040 EQU   *                                                                
*                                                                               
         GOTO1 CALNDRFL,DMCB,(RC),(R6)                                          
*                                  FILL CALENDAR FIELDS                         
DYTI0080 EQU   *                                                                
         MVC   E2LLEN,FOXZEROS     INITIALIZE LENGTH OF SPOT                    
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    DYTI0120            YES                                          
         ZICM  R1,RBUYDUR,2        LOAD REGISTER WITH SECONDS                   
         B     DYTI0140                                                         
DYTI0120 EQU   *                   SPOT LENGTH IN MINUTES                       
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         M     R0,=F'60'           MINUTES * 60 = TOTAL SECONDS                 
DYTI0140 EQU   *                   SPOT LENGTH IN MINUTES                       
         C     R1,=F'999'          VALUE > MAX ESG VALUE?                       
         BH    DYTI0160            YES - SET PHONY 'PROGRAM BUY'                
         MVI   E2SPTP,C'4'         NO  - SET 'NORMAL SPOT'                      
         EDIT  (R1),(3,E2LLEN),FILL=0,ZERO=NOBLANK                              
*                                  INSERT SECONDS                               
         B     DYTI0240                                                         
DYTI0160 EQU   *                                                                
         MVI   E2SPTP,C'3'         SET 'PROGRAM BUY'                            
         MVC   E2SHNM(11),=C'PROGRAM BUY'                                       
DYTI0240 EQU   *                                                                
         MVC   E2STTM(12),FOXZEROS LOAD TIMES WITH X'F0'                        
         CLC   =C'VAR',4(R6)       TIME = VARIOUS?                              
         BE    DYTI0280            YES - LEAVE AS ZEROS                         
*                                                                               
         EDIT  (2,4(R6)),(4,E2STTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT START TIME                            
         CLC   =C'CC',6(R6)        END TIME = CC?                               
         BNE   DYTI0250            NO                                           
         MVC   E2ENTM,=C'9999'     YES - FILL END TIME WITH 'F9'                
         B     DYTI0260                                                         
DYTI0250 EQU   *                                                                
         EDIT  (2,6(R6)),(4,E2ENTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT END   TIME                            
DYTI0260 EQU   *                                                                
         CLC   =C'240000',E2STTM   MIDNIGHT?                                    
         BE    DYTI0270            YES - LEAVE AS IS                            
         CLC   =C'24',E2STTM       BEGIN TIME = 24?                             
         BNE   DYTI0270            NO                                           
         MVC   E2STTM(2),=C'00'    YES - SET TO 0000                            
DYTI0270 EQU   *                                                                
         CLC   =C'240000',E2ENTM   MIDNIGHT?                                    
         BE    DYTI0280            YES - LEAVE AS IS                            
         CLC   =C'24',E2ENTM       START TIME = 24?                             
         BNE   DYTI0280            NO                                           
         MVC   E2ENTM(2),=C'00'    YES  - SET TO 0000                           
DYTI0280 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    GET CALENDAR DAYS FROM BITS 8-15 OF 02 ELEMENT                             
*                                                                               
CALNDRFL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            RESET A(02 ELEMENT)                          
**       LA    R6,RBUYREC                                                       
**       ZIC   R0,DTSTRNG#                                                      
**       MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
**       BAS   RE,GETEL            GET D/T ELEMENT                              
**       B     CALN0040                                                         
CALN0020 EQU   *                                                                
**       BAS   RE,NEXTEL           GET D/T ELEMENT                              
CALN0040 EQU   *                                                                
**       BNE   CALN0100            NOT FOUND - EXIT                             
**       BCT   R0,CALN0020         GET THE NTH SET                              
*                                                                               
         LA    RF,E2NRMO           A(CALENDAR)                                  
         ZIC   RE,3(R6)            GET DAYS BYTE IN REG                         
         SLL   RE,25               SHIFT DAYS BYTE TO HI-ORDER                  
*                                     DROP 'SPARE' BIT                          
         LA    R0,7                LOOP CONTROL                                 
CALN0060 EQU   *                                                                
         LTR   RE,RE               SET CONDITION CODES FOR REG                  
         BNM   CALN0080            NOT MINUS = BIT NOT SET                      
         MVC   0(2,RF),=C' .'      MINUS = BIT SET: SET CALENDAR                
         L     R1,DAYCNT           BUMP DAY COUNTER                             
         LA    R1,1(R1)                                                         
         ST    R1,DAYCNT           STORE IT BACK                                
CALN0080 EQU   *                                                                
         LA    RF,2(RF)            BUMP TO NEXT CALENDAR POSITION               
         SLL   RE,1                SHIFT BITS UP 1                              
         BCT   R0,CALN0060         TEST NEXT BIT                                
CALN0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    GET BUY DATE INFORMATION                                                   
*                                                                               
DTEINFO  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            RESET A(EFF DATE ELEMENT)                    
         USING RBUYDTEL,R6                                                      
         MVC   STARTDTE,RBUYDTST   SAVE START DATE                              
         MVC   NOFWKS,RBUYDTWK     SET NUMBER OF WEEKS                          
         MVC   SPOTSWK,RBUYDTNW    SET NUMBER SPOTS/WEEK                        
         CLI   RBUYDTNW,0          ANY SPOTS/WK?                                
         BZ    DINF0010            NO                                           
         MVC   ZEROSPTS,RBUYDTNW   YES - SET INDICATOR 'NOT ZERO'               
DINF0010 EQU   *                                                                
         SR    RE,RE                                                            
         ZIC   RF,NOFWKS           CALCULATE TOTAL SPOTS                        
         ZIC   R1,SPOTSWK                                                       
         MR    RE,R1               # WEEKS X SPOTS/WK = TOTAL SPOTS             
         L     RE,TLSPOTS                                                       
         AR    RE,RF                                                            
         ST    RE,TLSPOTS          SAVE TOTAL SPOTS                             
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         XC    COLLAPSE,COLLAPSE   CLEAR COLLAPSE FLAG                          
         GOTO1 CHKDATE,DMCB,(RC),(R6)                                           
*                                                                               
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
         MVC   E2STDT(12),FOXZEROS                                              
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
         GOTO1 DATCON,DMCB,(3,STARTDTE),(X'20',HOLDDATE)                        
         MVC   E2STDT(4),HOLDDATE+2         INSERT MM/DD                        
         MVC   E2STDT+4(2),HOLDDATE         INSERT YY                           
*                                                                               
DINF0180 EQU   *                                                                
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',HOLDDATE)                          
         MVC   E2ENDT(4),HOLDDATE+2         INSERT MM/DD                        
         MVC   E2ENDT+4(2),HOLDDATE         INSERT YY                           
*                                                                               
DINF0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
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
CHKDATE  NTR1                                                                   
         L     RC,0(R1)            RESET A (WORKSPACE)                          
         L     R6,4(R1)            A(1ST EFFECTIVE DATE ELT)                    
         USING RBUYDTEL,R6                                                      
         MVC   CKSTRDTE,STARTDTE   SAVE 1ST EFFECTIVE START DATE                
         MVC   ORIGSPTS,RBUYDTNW   SAVE SPOTS/WK FOR 1ST ELEMENT                
         MVC   ORIGALTW,RBUYDTIN   SAVE DATE RUN INDICATOR BYTE                 
CHKD0020 EQU *                                                                  
         L     R6,AEFFDATE         A(EFF DATE ELEMENT IN PROGRESS)              
         MVI   ELCODE,3            RESET A(ELEMENT CODE)                        
         BAS   RE,NEXTEL           GET NEXT 03 ELEMENT                          
         BNE   CHKD0120            NO MORE - EXIT                               
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         MVC   SVSTRDTE,RBUYDTST   SAVE EFFECT START DATE                       
         MVC   SVSPTSWK,RBUYDTNW   SAVE SPOTS PER WEEK                          
         CLC   SVSPTSWK,SPOTSWK    HAS THIS WEEK BEEN MADE GOOD?                
         BNE   CHKD0140            YES - CAN'T COLLAPSE WEEKS                   
*                                                                               
*   ABOVE TEST IS STRANGE:  ARE SPOTS/WEEK CHANGED IF LINE IS MADE              
*        GOOD?  DOES IT GET TO THE EFFECTIVE DATE ELEMENT, OR TOTAL             
*        SPOTS FOR LINE?  THIS MAY NOT BE A VALID TEST.....                     
*                                                                               
         CLC   ORIGSPTS,RBUYDTNW   SPOTS/WK:  THIS ELT SPOTS/WK SAME            
*                                     AS STARTING ELEMENT'S SPOTS/WK?           
         BNE   CHKD0140            NO  - CAN'T COLLAPSE THE ELEMENTS            
         CLC   NOFWKS,RBUYDTWK     NUMBER OF WEEKS SAME:                        
*                                     1ST EFF DATE VS THIS ONE                  
         BNE   CHKD0140            NO  - CAN'T COLLAPSE                         
         CLI   NOFWKS,1            NUMBER OF WEEKS = 1?                         
         BNE   CHKD0140            NO  - DON'T COLLAPSE                         
         CLI   FIRSTSW,0           ALTERNATE WEEK PATTERN?                      
         BNZ   CHKD0040            YES                                          
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
         CLI   COLLAPSE,0          HAS THIS BUY BEEN SEQ COLLAPSED?             
         BNZ   CHKD0140            YES - NO LONGER SEQ, SO EXIT                 
CHKD0040 EQU   *                                                                
         LA    RF,14               LOOK FOR ALTERNATING                         
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE(3),SVSTRDTE                                             
*                                  ALTERNATING BUY WEEK?                        
         BNE   CHKD0140            NO  - NOT SEQ OR ALTERNATING                 
         MVI   FIRSTSW,1           SET ALTERNATING WEEK FLAG                    
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
CHKD0140 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*                                                                               
*   THIS ROUTINE MODIFIES 'OLD-STYLE' CREDIT OUTPUT                             
*                                                                               
*---------------------------------------------------------------------          
OLDCRED  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         MVI   ELCODE,4            LOOK FOR BUY COMMENT ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   OCRD0120            NOT FOUND - EXIT                             
         CLC   2(3,R6),=C'CR='     CREDIT BUY?                                  
         BNE   OCRD0120            NO  - EXIT                                   
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         MVI   ELCODE,5            LOOK FOR BUY COMMENT ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                CREDIT W/O MISSED SPOT ELEMENT?              
         USING RBUYMGEL,R6                                                      
         EDIT  RBUYMGLI,(5,E2BYLN),FILL=0                                       
         EDIT  RBUYMGLI,(5,E2PRLN),FILL=0                                       
         DROP  R6                                                               
         MVC   E2PRDT,E2STDT                                                    
         MVC   E2TRAN,=C'CP'       SET TRANSACTION TYPE                         
OCRD0120 EQU   *                                                                
*                                                                               
*   TEST TEMP END                                                               
***      CLI   RBUYKLIN,4                                                       
***      BL    TEST0010            END JOB                                      
***      CLI   TESTCTR,1                                                        
***      BNE   TEST0010                                                         
***      DC    H'0'                                                             
***      MVC   DIE2,=X'0000'        KILL JOB                                    
TEST0010 EQU   *                                                                
*   TEST TEMP END END                                                           
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FILL IN CONTRACT '15' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
DATETIME NTR1                                                                   
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
         LA    R2,CONCACTH         SET CURSOR INCASE OF REC FULL ERR            
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,NEW15ELT                                   
*                                  ADD UPDATED X'15' ELEMENT                    
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                  REWRITE UPDATED CONTRACT RECORD              
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        ADDRESS                                                                
*                                                                               
RELX     DS    F                                                                
**YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                       
**BYEAR    DS    X                   BINARY YEAR                                
**STATFLAG DS    X                                                              
**HASCANLN EQU   X'80'               ORDER HAS CANCELLED BUYLINES               
         SPACE 1                                                                
ADCONS   DS    0A                  A/V TYPES                                    
         DC    V(OUTDAY)                                                        
*        DC    V(REGENPBY)                                                      
*        DC    V(REGENBUC)                                                      
*        DC    V(REGENTL2)                                                      
         DC    V(UNTIME)                                                        
NADCONS  EQU   (*-ADCONS)/4                                                     
         SPACE 2                                                                
DASH     DC    51C'-'                                                           
         SPACE 2                                                                
**RMSCMODE DC    X'00'               REGENSC MODES                              
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
CREDITD  DSECT                                                                  
CRSTDT   DS    CL8                 BUY GRID START DATE                          
         DS    CL1                 SEPARATOR                                    
CRSTDT2  DS    CL8                 USER INPUT START DATE                        
         DS    CL1                 SEPARATOR                                    
CRSTDT3  DS    CL8                 USER INPUT END DATE                          
CRNUMWK  DS    X                   NUMBER OF WEEKS FOR THIS CREDIT              
CRNUMSPT DS    X                   NUMBER OF SPOTS CREDITED                     
CRFLAG   DS    X                                                                
CRFALTQ  EQU   X'80'               ALTERNATING WEEKS                            
USRSDATE DS    XL3                 USER INPUT START DATE OF CREDIT              
USREDATE DS    XL3                 USER INPUT END DATE OF CREDIT                
TMPDATE  DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
TMPDATE2 DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
CRBUYEL  DS    XL11                BUY ELEMENT BUILD AREA                       
CRENDDAY DS    X                                                                
CREDITDX EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
LOCALWRK DSECT                                                                  
*                                                                               
*   NEW RECORD LAYOUT FOR EC CHANGES                                            
*                                                                               
       ++INCLUDE RECNTENTCH                                                     
*                                                                               
*      LOCAL VARIABLES                                                          
*                                                                               
FOXZEROS DS    CL24                STRING OF X'F0'                              
EXTRAKEY DS    CL34                BUY KEY SAVE AREA                            
TESTCTR  DS    XL1                                                              
TESTFLG  DS    XL1                                                              
COLLAPSE DS    XL1                                                              
MULTEFDT DS    XL1                                                              
ZEROSPTS DS    XL1                                                              
TRANSCNT DS    XL1                                                              
COMFLAGS DS    XL1                 COMMENT FLAGS                                
*                                  BIT 0  =  MG SWITCH                          
*                                  BIT 1  =  BUY COMMENTS EXIST                 
MGCOUNTR DS    F                   M/G MISSED SPOT COUNTER                      
AMGTABLE DS    A                   A(MG TABLE)                                  
ANXTMGTB DS    A                   A(NEXT SPOT IN MG TABLE)                     
MGSPOTS  DS    F                   MISSED SPOT COUNT                            
LASTMGMS DS    X                   LAST MASTER LINE #                           
FIRSTMG  DS    X                   FIRST M/G AGAINST MASTER                     
FIRSTMGD DS    XL8                 DATE OF FIRST MISSED SPOT YYYYMMDD           
FIRSTSW  DS    XL1                                                              
TRANSCT  DS    XL1                 TRANSACTION COUNTER                          
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
SVBYLN#  DS    XL1                 SAVED BUYLINE COUNT                          
MGDRNR   DS    XL1                 M/G DATED REVISION NUMBER                    
MGBUYLIN DS    XL1                 M/G BUYLINE NUMBER                           
BLDTABLE DS    XL24                (???? CHECK LENGTH)                          
SVSL#    DS    XL1                 SAVE SUBLINE #                               
SETMKG   DS    XL1                 SET M/G FLAG                                 
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
ORIGSPTS DS    XL1                 STARTING SPOTS/WK FOR COLLAPSE               
ORIGALTW DS    XL1                 ALTERNATE WEEK FLAG                          
TLSPOTS  DS    F                   TOTAL SPOTS                                  
DAYBUMP  DS    F                                                                
DAYCNT   DS    F                                                                
SPOTLEFT DS    F                                                                
SPOTMAX  DS    F                                                                
SPOTMAX2 DS    F                                                                
AEFFDATE DS    A                                                                
ANXTMG   DS    A                                                                
TOTLSPTS DS    F                   TOTAL SPOTS IN ORDER                         
TOTLDOLS DS    F                   TOTAL $$    IN ORDER                         
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
DTSTRNG# DS    XL1                 OUTPUT COUNT: DAY/TIME STRINGS               
*********************************************************************           
*    FIELDS USED TO HOLD DAYS AND TIMES BEFORE TRANSLATING TO                   
*        VALID BIAS FORMAT                                                      
XDATE    DS    CL12                TRANSLATE DATE                               
XTIME    DS    CL4                 TRANSLATE TIME                               
*********************************************************************           
         DS    0H                                                               
EOPRECS  DS    0CL48               EOP SAVE AREAS                               
EOPADV   DS    CL12                                                             
EOPAGY   DS    CL12                                                             
EOPOFF   DS    CL12                                                             
EOPSAL   DS    CL12                                                             
*                                                                               
SUPPRECS DS    0CL100              SUPPORT RECORD INFORMATION                   
AGYNAME  DS    CL20                                                             
ADVNAME  DS    CL20                                                             
SALNAME  DS    CL20                                                             
OFFNAME  DS    CL20                                                             
PRODNAME DS    CL20                                                             
REPNAME  DS    CL10                                                             
ZIPCODE  DS    CL10                                                             
         DS    0F                                                               
BLOCK    DS    480C                DEMO WORK AREA                               
NEW15ELT DS    CL20                EC CONTROL ELEMENT STORAGE                   
LOCXOPT  DS    CL1                 LOCAL STORAGE FOR RSTAXOPT                   
STAOPTS  DS    CL1                 LOCAL STORAGE FOR RSTAOPTB                   
STAOPTC  DS    CL1                 LOCAL STORAGE FOR RSTAOPTC                   
SAVETRAF DS    CL1                 STATION TRAFFIC CODE                         
SAVESIGN DS    CL8                 LOCAL STORAGE FOR SIGNON ID                  
LASTLINE DS    320C                SAVE AREA FOR LAST LINE                      
LLASTLIN EQU   320                                                              
OLD03ELT DS    CL11                OLD X'03' ELEMENT                            
NEW03ELT DS    CL11                EXPLODED 03 ELT                              
DAYDIFF  DS    F                   DAY DIFFERENCE FOR NEW 03 ELT                
ACRDWORK DS    A                   A(NEXT CREDIT SLOT)                          
AMKGWORK DS    A                   A(NEXT MG     SLOT)                          
CRDWORK  DS    200C                100 CREDIT ENTRIES                           
*                                  BYTE 1 - BUYLINE #                           
*                                  (BUYLINE MAY HAVE > 1 CRED BUYLINE)          
*                                  BYTE 2 - CREDIT BUYLINE                      
CRDWORKD DS    CL2                 DELIMITER FOR TABLE                          
LCRDWORK EQU   2                                                                
MKGWORK  DS    2000C               250 M/G ENTRIES                              
*                                  BYTE 1     - BUYLINE #                       
*                                  BYTE 2     - DATED REVISION NUMBER           
*                                  BYTE 3 - 6 - FLIGHT DATES                    
*                                  BYTE 7     - MISC FLAGS                      
*                                               X'80' = M/G EXISTS              
*                                               X'40' = CREDIT   EXISTS         
*                                  BYTE 8     - M/G GROUP #                     
MKGWORKD DS    CL8                 DELIMITER FOR TABLE                          
*                                                                               
DBUYLINE EQU   0                                                                
DDRNR    EQU   1                                                                
DSTDATE  EQU   2                                                                
DENDATE  EQU   4                                                                
DMISFLGS EQU   6                                                                
DMGGRP#  EQU   7                                                                
LMKGWORK EQU   8                                                                
*                                                                               
BUYWORK  DS    1800C               300 BUYLINE ENTRIES                          
*                                  BYTE 1   =  DETAIL BUYLINE #                 
*                                  BYTE 2-6 =  PLAN+MASTER+DETAIL LN #          
LTRUNTAB DS    400C                50 LATE RUN ENTRIES                          
*                                  BYTE 1   =  TARGET BUYLINE #                 
*                                  BYTE 2-4 =  DATE MISSED                      
*                                  BYTE 5   =  LATERUN LINE #                   
*                                  BYTE 6   =  LATERUN # SPOTS                  
*                                  BYTE 7-8 =  SPARE                            
LTRUNDEL DS    XL4                 TABLE DELIMITER                              
LBUYWORK EQU   6                                                                
ABUYWORK DS    A                   A(NEXT BUY SLOT)                             
LLTERUN1 EQU   8                                                                
ALTERUN1 DS    A                   A(NEXT LATE RUN SLOT)                        
BUYWORK# DS    F                   BUYWORK COUNTER                              
LATERUN# DS    F                   LATERUN COUNTER                              
AREADBUY DS    A                                                                
AMGGROUP DS    A                                                                
MGGROUP# DS    F                   MGGROUP COUNTER                              
*                                                                               
*   DON'T INSERT ANY FIELDS BETWEEN NEXT THREE                                  
*                                                                               
LASTGRUP DS    X                   LAST GROUP                                   
POSIGRUP DS    X                   POSITION WITHIN GROUP                        
ORIGBUYF DS    X                   ORIGINAL BUY FLAG                            
*                                  0  =  CC = ZERO                              
*                                  1  =  CC NOT ZERO                            
GENUNMAT DS    CL1                 GENERATE UNMATCHED PRE-EMPT FLAG             
*                                      0  =  NO                                 
*                                  NOT 0  =  YES:  CONTAINS COUNTER             
*                                                                               
MISDDATE DS    CL3                                                              
*                                                                               
KEYRSTRT DS    CL27                KEY FOR RESTARTING BUY SEQUENCE              
*                                                                               
BUYSGRP  DS    CL24                BUYS W/IN MAKEGOOD GROUP                     
AMGUNMAT DS    A                   A(MG GROUP STARTING UNMATCHED)               
STARTGRP DS    A                   A(START OF MG GROUP)                         
LOOPGRUP DS    A                   A(LOOP COUNT FOR M/G GROUP)                  
*                                                                               
UNMATELT DS    200C                UNMATCHED X'05' ELTS                         
*                                                                               
MGGROUP  DS    9600C               150 M/G GROUP ENTRIES                        
*                                  BYTE 1      =   1ST TARGET BUYLINE           
*                                  BYTE 2 - 3  =   1ST MISSED DATE              
*                                  BYTE 4      =   2ND TARGET BUYLINE           
*                                  BYTE 5 - 6  =   2ND MISSED DATE              
*                                  BYTE 7      =   3RD TARGET BUYLINE           
*                                  BYTE 8 - 9  =   3RD MISSED DATE              
*                                  BYTE 10     =   4TH TARGET BUYLINE           
*                                  BYTE 11- 12 =   4TH MISSED DATE              
*                                  BYTE 13     =   5TH TARGET BUYLINE           
*                                  BYTE 14- 15 =   5TH MISSED DATE              
*                                  BYTE 16     =   6TH TARGET BUYLINE           
*                                  BYTE 17- 18 =   6TH MISSED DATE              
*                                  BYTE 19     =   7TH TARGET BUYLINE           
*                                  BYTE 20- 21 =   7TH MISSED DATE              
*                                  BYTE 22     =   8TH TARGET BUYLINE           
*                                  BYTE 23- 24 =   8TH MISSED DATE              
*                                  BYTE 25     =   9TH TARGET BUYLINE           
*                                  BYTE 26- 27 =   9TH MISSED DATE              
*                                  BYTE 28     =  10TH TARGET BUYLINE           
*                                  BYTE 29- 30 =  10TH MISSED DATE              
*                                  BYTE 31     =  11TH TARGET BUYLINE           
*                                  BYTE 32- 33 =  11TH MISSED DATE              
*                                  BYTE 34     =  12TH TARGET BUYLINE           
*                                  BYTE 35- 36 =  12TH MISSED DATE              
*                                  BYTE 13/37  =   ORIGINAL BUYLINE #           
*                                  BYTE 14/38  =   M/G GROUP #                  
*                                  BYTE 15/39  =   1ST TARGET # SPOTS           
*                                  BYTE 16/40  =   2ND TARGET # SPOTS           
*                                  BYTE 17/41  =   3RD TARGET # SPOTS           
*                                  BYTE 18/42  =   4TH TARGET # SPOTS           
*                                  BYTE 18/43  =   5TH TARGET # SPOTS           
*                                  BYTE 18/44  =   6TH TARGET # SPOTS           
*                                  BYTE 18/45  =   7TH TARGET # SPOTS           
*                                  BYTE 18/46  =   8TH TARGET # SPOTS           
*                                  BYTE 18/47  =   9TH TARGET # SPOTS           
*                                  BYTE 18/48  =  10TH TARGET # SPOTS           
*                                  BYTE 18/49  =  11TH TARGET # SPOTS           
*                                  BYTE 18/50  =  12TH TARGET # SPOTS           
*                                  BYTE 19/51  =   TOTAL MG REF ELTS            
*                                  BYTE 20/52  =   TOTAL W/IN MG GROUP          
*                                  BYTE 21/53  =   TOTAL SPOTS MISSED           
*                                  BYTE 22/54  =   TOTAL SPOTS GIVEN            
LMGGROUP EQU   64                                                               
DTAR#1   EQU   0                   D(FIRST TARGET BUYLINE)                      
DTARDAT1 EQU   1                   D(FIRST TARGET BUYLINE DATE)                 
LTAR#    EQU   3                   L(TARGET BL# + DATE)                         
DORIGBLN EQU   36     WAS 12                                                    
DMGRP#   EQU   37     WAS 13                                                    
LMGGRPKY EQU   37                  L(MAKEGOOD GROUP KEY)                        
DTAR#SP1 EQU   38     WAS 14                                                    
LTAR#SP  EQU   1                   L(TARGET # SPOTS ENTRY)                      
TOTMGREF EQU   50     WAS 18       D(TOTAL MG REF ELTS)                         
CTWINGP  EQU   51     WAS 19       D(TOTAL W/IN MG GROUP)                       
SPTSMISD EQU   52     WAS 20       TOTAL SPOTS MISSED                           
SPTSGIVN EQU   53     WAS 21       TOTAL SPOTS GIVEN                            
MGGRPCT  EQU   12                  NUMBER OF MG REF ELTS                        
LOCALEND EQU   *                                                                
         EJECT                                                                  
*                                                                               
*********************************************************************           
*- GETSTA -- READ STATION RECORD INTO IO3.                                      
*********************************************************************           
         CSECT                                                                  
*                                                                               
*                                                                               
*                                                                               
PRINTREC NMOD1 0,*PREC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLI   7(R1),0             SAVE CURRENT LINE?                           
         BNE   PRC20010            NO  - LEAVE AS IS                            
         CLI   EORTYP,C'B'         BUYLINE?                                     
         BNE   PRC20010            NO  - SAVE ONLY BUYLINE                      
***      MVC   LASTLINE,EORTYP     SAVE LINE TO BE SENT                         
         MOVE  (LASTLINE,320),EORTYP                                            
PRC20010 EQU   *                                                                
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         BCTR  RF,0                YES - DON'T COUNT IT IN TOTALS               
         STC   RF,TRANSCNT         SAVE REVISED COUNT                           
*                                  INSERT RECORD COUNT                          
PRC20020 EQU   *                                                                
         LA    R2,3                LOOP CONTROL FOR PRINTING                    
         LA    R4,EORTYP           A(OUTPUT RECORD)                             
PRC20040 EQU   *                                                                
         CLC   =C'DOWNLOADER',RCONBUYR                                          
*                                  FORMAT OUTPUT AS DOWNLOADABLE?               
         BE    PRC20060            YES                                          
*                                  NO  - STANDARD OUTPUT                        
         MVC   P(093),0(R4)                                                     
         MVC   P+093(3),=C'DDS'    SENTINEL                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,093(R4)          BUMP TO NEXT SEGMENT OF RECORD               
         BCT   R2,PRC20040         DO NEXT SEQMENT OF RECORD                    
         MVC   P(041),0(R4)        SEND 4TH SEGMENT AS 41 CHARS                 
         MVC   P+041(3),=C'DDS'    SENTINEL                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRC20050 EQU   *                                                                
         XIT1                                                                   
PRC20060 EQU   *                                                                
         MVC   P+1(093),0(R4)                                                   
         MVI   P,C'"'              INSERT TEXT DOWNLOAD INDS                    
         MVI   P+094,C'"'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,093(R4)          BUMP TO NEXT SEGMENT OF RECORD               
         BCT   R2,PRC20060         DO NEXT SEQMENT OF RECORD                    
         MVC   P+1(041),0(R4)      SEND 4TH SEGMENT AS 41 CHARS                 
         MVI   P,C'"'              INSERT TEXT DOWNLOAD INDS                    
         MVI   P+042,C'"'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     PRC20050            DONE                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*    TOTLBUYS  ---  SCAN BUYS TO DETERMINE TOTAL SPOTS, $$$ - INSERT            
*        BUYS INTO A TABLE TO SEQUENCE FOR RETRIEVAL                            
*********************************************************************           
TOTLBUYS NMOD1 0,*TBUY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    RF,BUYWORK          SET A(NEXT BUYWORK ENTRY)                    
         ST    RF,ABUYWORK         SET A(NEXT BUY SLOT)                         
         LA    RF,LTRUNTAB         SET A(NEXT LATERUN ENTRY)                    
         ST    RF,ALTERUN1         SET A(NEXT LATERUN SLOT)                     
         LA    RF,MGGROUP          SET A(NEXT MG GROUP ENTRY)                   
         ST    RF,AMGGROUP                                                      
         XC    MGGROUP(64),MGGROUP CLEAR FIRST TABLE ENTRY                      
         XC    TOTLDOLS,TOTLDOLS   INITIALIZE SPOTS AND $ CHECKSUM              
         XC    TOTLSPTS,TOTLSPTS                                                
*                                                                               
         XC    KEY,KEY             INITIALIZE KEY                               
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
         B     TOBU0040                                                         
TOBU0020 EQU   *                                                                
         GOTO1 VSEQ                                                             
TOBU0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH CONTRACT #                   
         BNE   TOBU0900            NO MORE BUYS - FINISHED                      
*                                                                               
         L     RF,ABUYWORK         SET A(NEXT BUY SLOT IN TABLE)                
         MVC   0(1,RF),KEY+26      INSERT DETAIL LINE NUMBER                    
         MVC   1(5,RF),KEY+22      INSERT PLAN+MASTER+DETAIL LN #               
         LA    RF,LBUYWORK(RF)     BUMP TO NEXT SLOT                            
         ST    RF,ABUYWORK         SAVE A(NEXT SLOT)                            
         XC    0(6,RF),0(RF)       CLEAR NEXT SLOT                              
         L     RF,BUYWORK#         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,BUYWORK#         SAVE BUYWORK COUNTER                         
TOBU0060 EQU   *                                                                
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BE    TOBU0020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED BUY?                               
         BE    TOBU0020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    TOBU0020            YES - SKIP IT                                
         TM    RBUYRTS,X'08'       LATE RUN FLAG SET?                           
         BO    TOBU0065            YES                                          
         TM    RBUYRTS,X'04'       NO  - LATE RUN W/BONUS FLAG SET?             
         BNO   TOBU0070            NO                                           
TOBU0065 EQU   *                                                                
         TM    RBUYRTS,X'20'       YES - BONUS LINE?                            
         BO    TOBU0070            YES - SKIP IT                                
         GOTO1 LTRUNBUY                                                         
*                                  YES - PROCESS LATE RUN INTO TABLE            
*                                                                               
TOBU0070 EQU   *                                                                
*                                                                               
*   TEST DUMP                                                                   
***      CLC   RBUYKMLN(2),=X'0409'                                             
***      BNE   TDUM0020                                                         
***      LA    RF,RBUYREC                                                       
***      LA    RE,MGGROUP                                                       
***      DC    H'0'                                                             
TDUM0020 EQU   *                                                                
*   TEST DUMP END                                                               
*                                                                               
         GOTO1 PROCMGRP            CHECK M/G GROUP                              
*                                                                               
         LA    R4,RBUYELEM         CYCLE THRU DATE ELEMENTS                     
TOBU0080 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    TOBU0090            YES - PERMIT DUMP TEST TWEEN RECS            
         CLI   0(R4),3             DATE ELEMENT?                                
         BE    TOBU0100            YES                                          
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     TOBU0080            GO BACK FOR NEXT                             
TOBU0090 EQU   *                                                                
*                                                                               
*   TEST CTR                                                                    
***      L     RF,DUMMYCTR                                                      
***      LA    RF,1(RF)                                                         
***      ST    RF,DUMMYCTR                                                      
***      CLI   DUMMYCTR+3,1                                                     
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   TEST END                                                                    
         B     TOBU0020            YES - GO BACK FOR NEXT BUY REC               
DUMMYCTR DS    F                                                                
TOBU0100 EQU   *                                                                
         USING RBUYDTEL,R4                                                      
         ZIC   RF,RBUYDTNW         # PER WEEK                                   
         ZIC   R2,RBUYDTWK         # OF WEEKS                                   
         SR    RE,RE                                                            
         MR    RE,R2               # PER WK * # OF WKS = TTL SPOTS              
         LA    R1,RBUYELEM         APPLY MISSED SPOTS                           
TOBU0120 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    TOBU0180            YES                                          
         CLI   0(R1),6             MISSED ELEMENT?                              
         BE    TOBU0160            YES                                          
         CLI   0(R1),7             MISSED CREDIT ISSUED?                        
         BE    TOBU0160            YES                                          
TOBU0140 EQU   *                                                                
         ZIC   R5,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R5                                                            
         B     TOBU0120            GO BACK FOR NEXT                             
TOBU0160 EQU   *                   CHECK MISSED DATE VS DATE ELT                
         USING RBUYMSEL,R1                                                      
*                                                                               
*   BOTH BUY MISSED AND CREDIT MISSED USING RBUYMSEL FOR DSECT HERE             
*                                                                               
         CLC   RBUYMSDT,RBUYDTST   CHECK START DATE                             
         BL    TOBU0140            EARLIER - CHECK NEXT DATE                    
         CLC   RBUYMSDT,RBUYDTED   CHECK END   DATE                             
         BH    TOBU0140            LATER   - CHECK NEXT DATE                    
*                                                                               
*   MISSED ELEMENT APPLIES, SUBTRACT MISSED SPOTS                               
*                                                                               
         ZIC   R5,RBUYMSSP         MISSED SPOTS                                 
         SR    RF,R5                                                            
         B     TOBU0140            GO BACK FOR NEXT                             
*                                                                               
         DROP  R1                                                               
TOBU0180 EQU   *                                                                
         LR    R5,RF               SAVE TOTAL SPOTS                             
         A     RF,TOTLSPTS         ACCUMULATE TOTAL SPOTS                       
         ST    RF,TOTLSPTS         STORE IT BACK                                
         SR    RE,RE               CALCULATE TOTAL COST                         
         LR    RF,R5               RESET TOTAL SPOTS FOR BUY                    
         MVC   FULL,RBUYCOS        MOVE OUT FOR ALIGNMENT                       
         M     RE,FULL             SPOTS * SPOT COST = LINE COST                
         A     RF,TOTLDOLS         ACCUMULATE TOTAL COST                        
         ST    RF,TOTLDOLS         STORE IT BACK                                
*                                                                               
*   FOLLOWING CODE IS TEST CODE TO DISPLAY RESULTS WITHIN PRINTOUT              
*                                                                               
****>    MVC   P+1(23),=C'SPOTS:            COST:'                              
****>    EDIT  (R5),(11,P+7),COMMAS=YES                                         
****>    EDIT  RBUYCOS,(15,P+31),2,COMMAS=YES                                   
****>    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*   END OF TEST CODE                                                            
*                                                                               
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     TOBU0080            GO BACK FOR NEXT EFF DATE ELT                
TOBU0900 EQU   *                                                                
*                                                                               
*   FOLLOWING CODE IS TEST CODE TO DISPLAY RESULTS WITHIN PRINTOUT              
*                                                                               
****>    MVC   P+1(31),=C'TOT SPOTS:            TOT COST:'                      
****>    EDIT  TOTLSPTS,(11,P+11),COMMAS=YES                                    
****>    EDIT  TOTLDOLS,(11,P+39),2,COMMAS=YES                                  
****>    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*   END OF TEST CODE                                                            
*                                                                               
         DROP  R4                                                               
         CLC   BUYWORK#,=F'1'      MORE THAN ONE ENTRY?                         
         BNH   TOBU0920            NO  - DON'T SORT                             
*                                                                               
*  SORT THE BUYLINES INTO RETRIEVAL SEQUENCE                                    
*                                                                               
         L     RF,BUYWORK#         SET NUMBER OF RECORDS TO SORT                
         GOTO1 XSORT,DMCB,(0,BUYWORK),(RF),6,1,0                                
*                                                                               
TOBU0920 EQU   *                                                                
         CLC   LATERUN#,=F'1'      MORE THAN ONE ENTRY?                         
         BNH   TOBU0930            NO  - DON'T SORT                             
*                                                                               
*  SORT THE LATE RUN ENTRIES                                                    
*                                                                               
         L     RF,LATERUN#         SET NUMBER OF RECORDS TO SORT                
         GOTO1 XSORT,DMCB,(0,LTRUNTAB),(RF),8,6,0                               
*                                                                               
TOBU0930 EQU   *                                                                
         CLC   MGGROUP#,=F'1'      MORE THAN ONE ENTRY?                         
         BNH   TOBU0940            NO  - DON'T SORT                             
*                                                                               
*  SORT M/G INFO INTO M/G GROUP SEQUENCE                                        
*                                                                               
         L     RF,MGGROUP#         SET NUMBER OF RECORDS TO SORT                
         GOTO1 XSORT,DMCB,(0,MGGROUP),(RF),LMGGROUP,LMGGRPKY,0                  
*                                                                               
TOBU0940 EQU   *                                                                
         OC    MGGROUP#,MGGROUP#   ANY ENTRIES IN TABLE?                        
         BZ    TOBU0960            NO  - EXIT                                   
         GOTO1 NUMMGGRP            YES - NUMBER THE M/G GROUPS                  
         GOTO1 NUMWINGP            NUMBER W/IN M/G GROUPS                       
TOBU0960 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROCMGRP:  CYCLE THROUGH BUY LOOKING FOR X'05' BUY MG REF ELTS              
*        TABLE AND NUMBER FOR LATER PROCESSING                                  
*                                                                               
PROCMGRP NTR1                                                                   
         L     R2,AMGGROUP         SET A(NEXT MG GROUP SLOT)                    
         MVI   ELCODE,4            CHECK FOR OLD CREDIT ELT                     
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         BAS   RE,GETEL                                                         
         BNE   PMGR0010            NOT FOUND                                    
         CLC   2(3,R6),=C'CR='     OLD CREDIT ELEMENT?                          
         BE    PMGR0200            YES - DON'T PROCESS HERE                     
PMGR0010 EQU   *                                                                
         MVI   ELCODE,5            SET MG REF ELT CODE                          
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         LA    R4,DTAR#1(R2)       SET A(1ST TARGET BUYLINE)                    
         LA    R5,DTAR#SP1(R2)     SET A(1ST TARGET # SPOTS)                    
         LA    R0,MGGRPCT          SET MAX COUNTER                              
         BAS   RE,GETEL            GET FIRST MG REF ELT                         
         BNE   PMGR0120            NOT FOUND:  FINISH ROUTINE                   
         MVC   DORIGBLN(1,R2),RBUYKLIN                                          
*                                  INSERT MG BUYLINE #                          
         B     PMGR0060                                                         
PMGR0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT MG REF ELT                          
         BNE   PMGR0120            NOT FOUND:  FINISH ROUTINE                   
PMGR0060 EQU   *                                                                
         USING RBUYMGEL,R6                                                      
         MVC   0(1,R4),RBUYMGLI    INSERT MISSED LINE NUMBER                    
         GOTO1 DATCON,DMCB,(3,RBUYMGD1),(2,WORK)                                
         MVC   1(2,R4),WORK        INSERT MISSED DATE                           
         MVC   0(1,R5),RBUYMGSP    INSERT MISSED # SPOTS                        
         LA    R4,LTAR#(R4)        BUMP TO NEXT TARGET BL SLOT                  
         LA    R5,LTAR#SP(R5)      BUMP TO NEXT TARGET # SPOTS SLOT             
         BCT   R0,PMGR0020         GO BACK FOR NEXT ELT                         
PMGR0120 EQU   *                                                                
*                                                                               
*   STOP TEST                                                                   
***      CLC   =X'6DC3',RBUYKMLN                                                
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   STOP TEST                                                                   
*                                                                               
         LA    RF,MGGRPCT          SET LOOP COUNTER                             
****     C     R0,=F'12'           ANY ELTS FOUND?                              
         CR    R0,RF               ANY ELTS FOUND? (NO LOOP)                    
         BE    PMGR0200            NO  - DON'T ADVANCE SLOT ADDR                
*                                  YES - COUNT # X'05' ELTS                     
         MVI   ELCODE,5            SET MG REF ELT CODE                          
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         SR    R4,R4               CLEAR FOR COUNTER                            
         BAS   RE,GETEL            GET FIRST MG REF ELT                         
         BNE   PMGR0200            NOT FOUND:  FINISH ROUTINE                   
PMGR0140 EQU   *                                                                
         LA    R4,1(R4)            INCREMENT COUNTER                            
         BAS   RE,NEXTEL           GET NEXT MG REF ELT                          
         BE    PMGR0140            FOUND:  COUNT AND GO AGAIN                   
         STC   R4,TOTMGREF(R2)     INSERT MG REF COUNTER                        
*                                                                               
*   STOP TEST                                                                   
***      CLC   =X'6DC3',RBUYKMLN                                                
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   STOP TEST                                                                   
*                                                                               
         LA    R2,LMGGROUP(R2)     BUMP TO NEXT SLOT                            
         ST    R2,AMGGROUP         SAVE ADDR(NEXT SLOT)                         
         XC    0(64,R2),0(R2)      CLEAR NEXT SPACE                             
         L     RF,MGGROUP#         INCREASE COUNTER                             
         LA    RF,1(RF)                                                         
         ST    RF,MGGROUP#                                                      
PMGR0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
*    NUMMGGRP  -  INSERT M/G GROUP NUMBERS INTO MGGROUP TABLE                   
*        PROCESSING SUMMARY:  TABLE CONSISTS OF 20-CHARACTER ENTRIES.           
*        IF TWO CONSECUTIVE ENTRIES HAVE THE SAME FOUR TARGET                   
*        BUYLINE#S AND MISSED DATES FOR EACH, IT IS HIGHLY LIKELY               
*        THAT THEY WERE BOOKED AS THE SAME M/G GROUP.       THIS IS             
*        CONFIRMED BY A TEST OF THE 2ND MGGROUP ENTRY WHICH CHECKS              
*        IT FOR SPOTS.  IF THERE ARE NO SPOTS, IT IS THE SAME                   
*        M/G GROUP.       IF THERE ARE SPOTS, IT IS NOT - REPEAT,               
*        NOT - THE SAME GROUP, AND MUST BE GIVEN A DIFFERENT #.                 
*                                                                               
*********************************************************************           
NUMMGGRP NTR1                                                                   
         LA    R2,MGGROUP          SET A(M/G GROUP TABLE)                       
         LA    R5,1                SET COUNTER                                  
         STC   R5,DMGRP#(R2)       STORE 1ST M/G GROUP NUMBER                   
         LA    R4,LMGGROUP(R2)     SET A(2ND TABLE ENTRY)                       
NUMM0020 EQU   *                                                                
         OC    0(20,R4),0(R4)      2ND ENTRY EMPTY?                             
         BZ    NUMM0120            YES - FINISHED                               
         CLC   0(12,R2),0(R4)      1ST ENTRY VS 2ND ENTRY                       
         BNE   NUMM0060            NO  - 2ND GETS NEXT NUMBER                   
         OC    DTAR#SP1(4,R4),DTAR#SP1(R4)                                      
*                                  YES - 2ND ENTRY HAVE SPOTS?                  
         BNZ   NUMM0060            YES - 2ND GETS NEXT NUMBER                   
         B     NUMM0080                                                         
NUMM0060 EQU   *                                                                
         LA    R5,1(R5)            INCREMENT M/G GROUP NUMBER                   
NUMM0080 EQU   *                                                                
         STC   R5,DMGRP#(R4)       NO  - USE SAME NUMBER                        
         LA    R2,LMGGROUP(R2)     BUMP TO NEXT PAIR OF ENTRIES                 
         LA    R4,LMGGROUP(R4)                                                  
         B     NUMM0020            GO BACK AND CHECK NEXT PAIR                  
NUMM0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*    NUMWINGP  -  INSERT NUMBER OF ENTRIES WITHIN EACH M/G GROUP.               
*        THIS IS TO BE USED WHEN UNMATCHED PRE-EMPT RECORDS ARE TO              
*        BE GENERATED.  FOR EXAMPLE, WHEN A TWO-FOR-THREE M/G                   
*        IS ISSUED, A RECORD MUST BE GENERATED FOR THE THIRD MISSED             
*        SPOT SO THAT ENTERPRISE CAN ACCURATELY REMOVE IT.                      
*        TOTAL NUMBER OF SPOTS MISSED FOR EACH MG GROUP WILL BE                 
*        ACCUMULATED AND INSERTED INTO THE 1ST ENTRY WITHIN EACH                
*        M/G GROUP                                                              
*                                                                               
*********************************************************************           
NUMWINGP NTR1                                                                   
         LA    R5,0                SET COUNTER                                  
         LA    R6,0                SET SPOTS MISSED COUNTER                     
         LA    R2,MGGROUP          SET A(M/G GROUP TABLE)                       
         LR    R4,R2               SET A(2ND TABLE ENTRY = 1ST TABLE)           
*                                  START WITH R2 = R4 TO GET 1ST ENTRY          
*                                     ADDED INTO THE COUNT                      
NUWI0020 EQU   *                                                                
         OC    0(LMGGROUP,R4),0(R4)    2ND ENTRY EMPTY?                         
         BZ    NUWI0120            YES - DONE: INSERT LAST CT W/IN GRP          
         CLC   DMGRP#(1,R2),DMGRP#(R4)                                          
*                                  1ST ENTRY VS 2ND ENTRY                       
         BNE   NUWI0060            NO  - INSERT COUNT W/IN GROUP                
         LA    R5,1(R5)            YES - INCREMENT COUNT W/IN GROUP             
         LA    RF,DTAR#SP1(R4)     COUNT SPOTS IN ENTRY                         
*                                     SET A(1ST SPOT MISSED)                    
         LA    R0,MGGRPCT          SET LOOP CONTROL                             
NUWI0040 EQU   *                                                                
         ZIC   RE,0(RF)            GET NUMBER OF SPOTS FOR TARGET               
         AR    R6,RE               INCREMENT TOTAL SPOTS MISSED                 
         LA    RF,1(RF)            BUMP TO NEXT TARGET SPOT COUNT               
         BCT   R0,NUWI0040         GO BACK FOR NEXT                             
*                                                                               
         LA    R4,LMGGROUP(R4)     BUMP A(2ND ENTRY)                            
         B     NUWI0020            GO BACK FOR NEXT                             
NUWI0060 EQU   *                                                                
         LR    R4,R2               RESET TO BEGINNING OF GROUP                  
*                                  START WITH R2 = R4 TO GET 1ST ENTRY          
*                                     UPDATED WITH THE COUNT                    
         STC   RF,SPTSMISD(R2)     INSERT SPOTS MISSED INTO FIRST               
*                                     ENTRY OF THE MGGROUP                      
NUWI0080 EQU   *                                                                
         CLC   DMGRP#(1,R2),DMGRP#(R4)                                          
*                                  1ST ENTRY VS 2ND ENTRY                       
*                                                                               
         BNE   NUWI0100            NO  - GROUP COUNT W/IN GROUP DONE            
         STC   R5,CTWINGP(R4)      YES - INSERT COUNT W/IN GROUP                
         LA    R4,LMGGROUP(R4)     BUMP A(2ND ENTRY)                            
         B     NUWI0080            GO BACK FOR NEXT                             
*                                                                               
NUWI0100 EQU   *                                                                
         LR    R2,R4               SET A(1ST ENTRY) TO A(2ND ENTRY)             
         LA    R5,0                RESET COUNTER TO 0                           
         LA    R6,0                RESET COUNTER TO 0                           
         B     NUWI0020            GO BACK FOR NEXT GROUP                       
NUWI0120 EQU   *                                                                
         LR    R4,R2               RESET TO BEGINNING OF GROUP                  
         STC   R6,SPTSMISD(R2)     INSERT SPOTS MISSED INTO FIRST               
*                                     ENTRY OF THE MGGROUP                      
NUWI0140 EQU   *                                                                
         CLC   DMGRP#(1,R2),DMGRP#(R4)                                          
*                                  1ST ENTRY VS 2ND ENTRY                       
         BNE   NUWI0160            NO  - GROUP COUNT W/IN GROUP DONE            
         STC   R5,CTWINGP(R4)      YES - INSERT COUNT W/IN GROUP                
         LA    R4,LMGGROUP(R4)     BUMP A(2ND ENTRY)                            
         B     NUWI0140            GO BACK FOR NEXT                             
*                                                                               
NUWI0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   LTRUNBUY:  TABLE THE INFORMATION FROM LATE RUNS FOR REBUILDING              
*        THESE RECORDS DURING REGULAR BUYLINE PROCESSING                        
*                                                                               
LTRUNBUY NTR1                                                                   
         LA    R4,RBUYELEM         SET A(DESC ELT OF BUY RECORD)                
LTRU0020 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    LTRU0200            YES - FINISHED                               
         CLI   0(R4),5             MISSED SPOT ELEMENT?                         
         BE    LTRU0040            YES - TABLE IT UP                            
LTRU0030 EQU   *                                                                
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     LTRU0020            GO BACK FOR NEXT                             
LTRU0040 EQU   *                                                                
         USING RBUYMGEL,R4                                                      
         L     RF,ALTERUN1         SET A(NEXT LATE RUN SLOT)                    
         CLC   0(2,RF),=X'FFFF'    TABLE FULL?                                  
         BNE   *+6                 NO                                           
*                                                                               
         DC    H'0'                MUST EXPAND TABLE                            
*                                                                               
         MVC   0(1,RF),RBUYMGLI    INSERT TARGET BUYLINE #                      
         MVC   1(3,RF),RBUYMGD1    INSERT MISSED DATE IN TARGET                 
         MVC   4(1,RF),RBUYKLIN    INSERT LATE RUN LINE #                       
         MVC   5(1,RF),RBUYMGSP    INSERT # SPOTS MISSED                        
         LA    RF,LLTERUN1(RF)     BUMP TO NEXT LATE RUN SLOT                   
         ST    RF,ALTERUN1         SAVE                                         
         L     RE,LATERUN#         INCREMENT LATE RUN COUNTER                   
         LA    RE,1(RE)                                                         
         ST    RE,LATERUN#         SAVE                                         
         CLC   LATERUN#,=F'50'     TABLE FULL?                                  
         BE    LTRU0060            YES - DON'T CLEAR NEXT SLOT                  
         XC    0(8,RF),0(RF)       NO  - CLEAR NEXT SLOT                        
LTRU0060 EQU   *                                                                
         B     LTRU0030            GO BACK FOR NEXT ELEMENT                     
LTRU0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   GENUNPRE:  GENERATE UNMATCHED PREEMPTS AT THIS POINT.                       
*                                                                               
GENUNPRE NMOD1 0,*UPRE*                                                         
         L     RC,0(R1)                                                         
*                                                                               
*   TEST                                                                        
         MVC   SAVEBYLN,E2BYLN     SAVE VALUE FOR TESTING                       
*   TEST END                                                                    
*                                                                               
         XC    UNMATELT(64),UNMATELT                                            
*                                                                               
         MVC   KEYRSTRT,RBUYREC    SAVE KEY FOR RESTARTING                      
*                                                                               
*   AT THIS POINT, THE MAKEGOOD GROUP MAY OR MAY NOT HAVE ANY                   
*        SPOTS LEFT.  IT MUST BE SCANNED.  FOR ANY ORIGINAL LINES               
*        HAVING SPOTS LEFT, EACH BUY RECORD MUST BE RETRIEVED.                  
*        THE CORRESPONDING X'05' ELEMENT, WITH THE NUMBER OF                    
*        REMAINING SPOTS (THIS WILL HAVE TO BE INSERTED INTO THE                
*        EXTRACTED ELEMENT, AND MAY OVERRIDE THE ORIGINAL VALUE)                
*        MUST BE PLACED IN THE TABLE.                                           
*                                                                               
         LA    R2,UNMATELT         SET A(WORK AREA)                             
         L     R6,STARTGRP         SET A(M/G GROUP START)                       
         ST    R6,LOOPGRUP         SAVE A(M/G GROUP START) FOR LOOP             
         ZIC   R0,CTWINGP(R6)      SET LOOP W/IN M/G GROUP                      
GPRE0020 EQU   *                                                                
         LA    R4,DTARDAT1(R6)     SET A(1ST DATE W/IN ENTRY)                   
         LA    RE,DTAR#SP1(R6)     SET A(1ST SPOT COUNT W/IN ENTRY)             
         LA    R5,MGGRPCT          SET MAX DATES W/IN M/G                       
GPRE0040 EQU   *                                                                
         CLI   0(RE),0             ANY SPOT COUNT?                              
         BNE   GPRE0080            YES - GENERATE UNMATELT                      
*                                  NO  -                                        
GPRE0060 EQU   *                                                                
         LA    R4,LTAR#(R4)        BUMP A(DATE PTR) W/IN M/G                    
         LA    RE,LTAR#SP(RE)      BUMP A(SPOT COUNT) W/IN M/G                  
         BCT   R5,GPRE0040         GO BACK FOR NEXT                             
GPRE0070 EQU   *                                                                
         L     RF,LOOPGRUP         BUMP TO NEXT M/G W/IN GROUP                  
         LA    RF,LMGGROUP(RF)                                                  
         ST    RF,LOOPGRUP                                                      
         LR    R6,RF                                                            
         BCT   R0,GPRE0020         PROCESS NEXT M/G W/IN GROUP                  
*                                  FALL THROUGH WHEN ALL PROCESSED              
*                                                                               
*   TEST DEATH 2                                                                
***      CLI   TESTFLG,1                                                        
***      BNE   TEST0200                                                         
***      DC    H'0'                                                             
TEST0200 EQU   *                                                                
*   TEST DEATH 2                                                                
*                                                                               
         B     GPRE0100                                                         
SAVEREGE DS    F                                                                
*                                                                               
GPRE0080 EQU   *                                                                
         MVC   0(2,R2),=X'050A'    CONSTRUCT DUMMY X'05' ELTS                   
         LR    RF,R4               SET A(DATE ENTRY IN PROGRESS)                
         BCTR  RF,0                BACK UP ONE POSITION TO LINE #               
         MVC   2(1,R2),0(RF)       INSERT LINE NUMBER INTO ELT                  
         ST    RE,SAVEREGE         SAVE REGISTER VALUE                          
         GOTO1 DATCON,DMCB,(2,0(R4)),(3,3(R2))                                  
         L     RE,SAVEREGE         RESET REGISTER VALUE                         
         MVC   9(1,R2),0(RE)       INSERT SPOTS REMAINING INTO ELT              
         LA    R2,10(R2)           BUMP TO NEXT SLOT                            
         XC    0(10,R2),0(R2)      CLEAR NEXT SLOT                              
         BCT   R5,GPRE0060         GO BACK FOR NEXT ENTRY                       
         B     GPRE0070                                                         
GPRE0100 EQU   *                                                                
         LA    R2,UNMATELT         SET A(WORK AREA)                             
GPRE0120 EQU   *                                                                
         OC    0(10,R2),0(R2)      ANY ENTRY?                                   
         BZ    GPRE0400            NO  - FINISHED                               
*                                     RELOAD ORIG AND EXIT                      
         MVC   KEY(27),KEYRSTRT    INSERT SKELETON KEY                          
         USING RBUYMGEL,R2                                                      
         LA    R6,BUYWORK          SET A(BUYWORK)                               
GPRE0140 EQU   *                                                                
         CLI   0(R6),0             END OF BUYWORK?                              
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN: MUST BE THERE              
         CLC   RBUYMGLI,0(R6)      M/G LINE IN TABLE?                           
         BE    GPRE0160            YES                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R6,LBUYWORK(R6)     NO  - BUMP TO NEXT SLOT                      
         B     GPRE0140            GO BACK FOR NEXT                             
GPRE0160 EQU   *                                                                
         MVC   KEY+22(5),1(R6)     INSERT PLAN+MASTER+DETAIL LINE #             
         GOTO1 VHIGH               READ BUY RECORD KEY                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES - MUST BE ON FILE                        
         DC    H'0'                                                             
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         MVI   E2RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E2RTYP+1(200),E2RTYP                                             
         MVC   E2RTYP+201(119),E2RTYP+200                                       
         MVI   E2RTYP,C'B'         INSERT RECORD TYPE                           
         GOTO1 HEXOUT,DMCB,RCONKCON,E2RREF,4,=C'TOG'                            
*                                  INSERT CONTRACT NUMBER                       
         EDIT  RBUYKLIN,(5,E2PRLN),FILL=0                                       
*                                  INSERT PREEMPT BUYLINE #                     
         EDIT  LASTGRUP,(3,E2MGGP),FILL=0                                       
         MVC   E2BYLN,FOXZEROS     SET BUYLINE # TO ZERO                        
         MVC   E2RREV,FOXZEROS     CLEAR REVISION #                             
         MVC   E2DRNR(06),FOXZEROS CLEAR DATED REV#/ADD'L CL#                   
         MVC   E2PRDR,FOXZEROS     CLEAR PRE-EMPT DATED REV#                    
         MVC   E2PRDT,FOXZEROS     CLEAR PRE-EMPT DATE                          
         MVC   E2TRAN,=C'P '       INSERT TRANSACTION TYPE                      
         EDIT  RBUYCOS,(9,E2COST),FILL=0                                        
*                                  INSERT SPOT COST                             
         MVC   E2RSEC,RBUYSEC      INSERT RATE SECTION                          
*                                     FIRST TWO CHARACTERS ONLY                 
         MVC   E2LLEN,FOXZEROS     INITIALIZE LENGTH OF SPOT                    
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    GPRE0180            YES                                          
         EDIT  RBUYDUR,(3,E2LLEN),FILL=0,ZERO=NOBLANK                           
*                                  NO  - IN SECONDS                             
         B     GPRE0200                                                         
GPRE0180 EQU   *                   SPOT LENGTH IN MINUTES                       
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         M     R0,=F'60'           MINUTES * 60 = TOTAL SECONDS                 
         EDIT  (R1),(3,E2LLEN),FILL=0,ZERO=NOBLANK                              
*                                  INSERT CALCULATED SECONDS                    
GPRE0200 EQU   *                                                                
**<<<                                                                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            FIND D/T ELT                                 
         GOTO1 GETEL               GET FIRST DAY TIME ELT                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   E2STTM(12),FOXZEROS LOAD TIMES WITH X'F0'                        
         CLC   =C'VAR',4(R6)       TIME IS VARIOUS?                             
         BE    GPRE0280            YES                                          
         EDIT  (2,4(R6)),(4,E2STTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT START TIME                            
         CLC   =C'CC',6(R6)        END TIME = CC?                               
         BNE   GPRE0220            NO                                           
         MVC   E2ENTM,=C'9999'     YES - FILL END TIME WITH 'F9'                
         B     GPRE0240                                                         
GPRE0220 EQU   *                                                                
         EDIT  (2,6(R6)),(4,E2ENTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT END   TIME                            
GPRE0240 EQU   *                                                                
         CLC   =C'240000',E2STTM   MIDNIGHT?                                    
         BE    GPRE0260            YES - LEAVE AS IS                            
         CLC   =C'24',E2STTM       BEGIN TIME = 24?                             
         BNE   GPRE0260            NO                                           
         MVC   E2STTM(2),=C'00'    YES - SET TO 0000                            
GPRE0260 EQU   *                                                                
         CLC   =C'240000',E2ENTM   MIDNIGHT?                                    
         BE    GPRE0280            YES - LEAVE AS IS                            
         CLC   =C'24',E2ENTM       START TIME = 24?                             
         BNE   GPRE0280            NO                                           
         MVC   E2ENTM(2),=C'00'    YES  - SET TO 0000                           
GPRE0280 EQU   *                                                                
**<<<                                                                           
         GOTO1 SETMGGRP            SET UP M/G GROUP INFO                        
         LA    R6,RBUYREC                                                       
*                                                                               
         MVI   ELCODE,X'56'        FIND M/G REF ELT                             
         GOTO1 GETEL                                                            
         B     GPRE0320                                                         
GPRE0300 EQU   *                                                                
         GOTO1 NEXTEL                                                           
*                                                                               
*   TEST DEATH 3                                                                
***      MVC   TEST0300(2),=X'0000'                                             
*   TEST DEATH 3 END                                                            
*                                                                               
GPRE0320 EQU   *                                                                
         BNE   GPRE0380            PICK UP NEXT X'56' ELT IN STORAGE            
         USING RBYMGSEL,R6                                                      
         LA    R4,BUYSGRP          LOOK AT EVERY ENTRY IN BUYSGRP               
GPRE0340 EQU   *                                                                
         CLI   0(R4),0             END OF LIST?                                 
         BE    GPRE0350            YES - ELT NOT IN GROUP: SKIP ELT             
         CLC   RBYMGSLI,0(R4)      MG MISSED IN LIST?                           
         BE    GPRE0360            YES                                          
TEST0300 EQU   *                                                                
         LA    R4,1(R4)            NO  - BUMP TO NEXT IN LIST                   
         B     GPRE0340            GO BACK FOR NEXT                             
GPRE0350 EQU   *                                                                
*                                                                               
*   TEST                                                                        
**       CLC   =C'00008',SAVEBYLN                                               
**       BNE   *+6                                                              
**       DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         LA    R4,BUYSGRP          RESET START OF BUY GROUP                     
         B     GPRE0300            YES - ELT NOT IN GROUP: SKIP ELT             
GPRE0360 EQU   *                                                                
         CLI   RBYMGSSP,0          ANY SPOTS MISSED?                            
         BE    GPRE0300            NO  - DON'T USE THIS ONE                     
         CLC   RBYMGSDT,RBUYMGD1-RBUYMGEL(R2)                                   
*                                  MISSED DATE IN TABLE?                        
         BNE   GPRE0300            NO  - SKIP THIS ENTRY                        
         EDIT  (1,RBUYMGSP-RBUYMGEL(R2)),(3,E2NRWK),FILL=0                      
         MVI   RBUYMGSP-RBUYMGEL(R2),0                                          
*                                  RESET SPOT COUNT TO ZERO                     
         GOTO1 DATCON,DMCB,(3,RBYMGSDT),(X'20',HOLDDATE)                        
         MVC   E2PRDT(4),HOLDDATE+2         INSERT MM/DD                        
         MVC   E2PRDT+4(2),HOLDDATE         INSERT YY                           
         MVC   E2STDT,FOXZEROS                                                  
         MVC   E2ENDT,FOXZEROS                                                  
*                                                                               
         GOTO1 =A(PRINTREC),DMCB,(RC),0,RR=Y                                    
*                                  OUTPUT THE PRINT RECORD                      
****     MVC   DIE(2),=X'0000'     DUMP ON NEXT LOOP                            
*                                                                               
GPRE0380 EQU   *                                                                
         LA    R2,10(R2)           BUMP TO NEXT X'05' ELEMENT                   
         B     GPRE0120            GO BACK FOR NEXT                             
GPRE0400 EQU   *                                                                
         MVC   KEY(27),KEYRSTRT    RESTORE ORIGINAL BUY                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         XIT1                                                                   
SAVEBYLN DS    CL5                                                              
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*   SETMGGRP                                                                    
*                                                                               
SETMGGRP NTR1                                                                   
         XC    BUYSGRP,BUYSGRP     CLEAR BUYS IN GROUP LIST                     
         LA    R4,BUYSGRP                                                       
         L     R2,STARTGRP         SET A(START OF GROUP)                        
         MVC   BYTE,DMGRP#(R2)     GET GROUP NUMBER                             
SMGG0020 EQU   *                                                                
         CLC   BYTE,DMGRP#(R2)     SAME GROUP?                                  
         BNE   SMGG0100            NO  - FINISHED                               
         MVC   0(1,R4),DORIGBLN(R2)                                             
*                                  YES - SAVE ORIGINAL BUYLINE #                
         LA    R4,1(R4)            BUMP TO NEXT SAVE AREA                       
         LA    R2,LMGGROUP(R2)     BUMP TO NEXT MGGROUP AREA                    
         B     SMGG0020            GO BACK FOR NEXT                             
SMGG0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
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
         MVC   SAVETRAF,RSTATRAF   SAVE TRAFFIC FORMAT                          
*                                                                               
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'06'        SIGNON ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   GSTA30              NOT THERE                                    
*                                                                               
         USING RSTASOEL,R6                                                      
         MVC   SAVESIGN,RSTASO     SAVE SIGNON ID                               
         DROP  R6                                                               
GSTA30   DS    0H                                                               
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   GSTA55              NOT THERE                                    
*                                                                               
         USING RSTAXXEL,R6                                                      
*                                                                               
         MVC   LOCXOPT,RSTAXOPT    SAVE RSTAXOPT                                
         MVC   STAOPTS,RSTAOPTB    SAVE RSTAOPTB                                
         MVC   STAOPTC,RSTAOPTC    SAVE RSTAOPTC                                
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
*   CLASS WILL BE CHANGED ONCE A NEW TYPE IS ASSIGNED TO THIS                   
*        FUNCTION                                                               
*                                                                               
         MVI   PLCLASS,C'G'        CLASS G                                      
*                                                                               
*   TEMPORARILY OVERRIDE CLASS 'G' WITH SPACE                                   
*                                                                               
****>>>  MVI   PLCLASS,C' '        CLASS ' ' - TEMPORARY!!                      
*                                                                               
*   TEMPORARILY OVERRIDE CLASS 'G' WITH SPACE                                   
*                                                                               
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
         EJECT                                                                  
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
EDICT    NMOD1 0,*EDIC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
                                                                                
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(6),=C'EDICT='                                                
         TM    STAOPTS,X'10'       ENCODA ROUTING?                              
         BNO   EDICT01             NO                                           
         MVC   P+15(7),=C'*ENCODA' YES                                          
         B     EDICT06                                                          
EDICT01  EQU   *                                                                
         CLI   TWAACCS,C'$'        SIGNON FROM STATION SIDE?                    
         BE    EDICT02             YES                                          
         TM    STAOPTC,X'80'       SIGNON FROM REP SIDE ALLOWED?                
         BO    *+6                 YES                                          
         DC    H'0'                NO  - SHOULDN'T HAVE GOTTEN THIS FAR         
         MVC   P+15(8),SAVESIGN    YES - INSERT SIGNON ID                       
         B     EDICT06                                                          
EDICT02  EQU   *                                                                
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
                                                                                
         CLI   0(R6),CTDSCELQ                                                   
         BE    EDICT05                                                          
         ZIC   RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
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
EDICT06  EQU   *                                                                
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
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,DUB                       
         GOTO1 DATCON,DMCB,(0,WORK2),(5,EDIRCNFS)                               
         GOTO1 DATCON,DMCB,(0,WORK2+6),(5,EDIRCNFE)                             
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
*                                  SEND SPECIAL PRINT LINE                      
         CLI   SAVETRAF,C'Z'       VSS TRAFFIC USAGE?                           
         BNE   EDICT40             NO  -                                        
*                                                                               
*                                  YES - SET EDICT HEADER FOR BDE               
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
EDICT40  EQU   *                                                                
         MVI   FORCEHED,C'Y'                                                    
EDICTX   DS    0H                                                               
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* INCLUDE DMPRTQL                                                               
* INCLUDE CTGENFILE                                                             
       ++INCLUDE DMPRTQL                                                        
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*          DATA SET RECNT64    AT LEVEL 072 AS OF 08/24/93                      
*   RESOLVE:  FILL IN THE ADDRESSES OF CALLED ROUTINES                          
*   DERIVED FROM THE RECNT80 MODULE                                             
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
         EJECT                                                                  
         LTORG                                                                  
*********INCLUDE REMERG03                                                       
       ++INCLUDE REMERG16                                                       
       ++INCLUDE RECYCLMG                                                       
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   THIS ROUTINE SETS TABLE OF M/G INFORMATION                                  
*                                                                               
*--------------------------------------------------------------------           
SETMGTAB NMOD1 0,*MGTB*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AMKGWORK         SET A(NEXT MG SLOT IN TABLE)                 
         CLC   0(4,R2),=X'FFFFFFFF'                                             
*                                  TABLE FULL?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - CANNOT CONTINUE                        
         MVC   DBUYLINE(1,R2),MGBUYLIN    INSERT BUYLINE NUMBER                 
         MVC   DDRNR(1,R2),MGDRNR         INSERT M/G DATED REV #                
         MVC   WORK(2),E2STDT+4    REARRANGE START DATE                         
         MVC   WORK+2(4),E2STDT                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(2,WORK+6)                                  
         MVC   DSTDATE(2,R2),WORK+6       INSERT START DATE                     
         MVC   WORK(2),E2ENDT+4    REARRANGE END   DATE                         
         MVC   WORK+2(4),E2ENDT                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(2,WORK+6)                                  
         MVC   DENDATE(2,R2),WORK+6       INSERT END   DATE                     
         LA    R2,LMKGWORK(R2)                                                  
         ST    R2,AMKGWORK         SET A(NEXT MG SLOT IN TABLE)                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   THIS ROUTINE CHECKS IF BUY IS M/G, SETS UP INTERNAL DATA                    
*                                                                               
*--------------------------------------------------------------------           
MAKEGOOD NMOD1 0,*MAKE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST                                                                        
***      CLI   RBUYKLIN,4                                                       
***      BNE   TEST0020                                                         
***      ZIC   RF,TESTBYTE         SET FLAG                                     
***      LA    RF,1(RF)                                                         
***      STC   RF,TESTBYTE                                                      
TEST0020 EQU   *                                                                
*   TEST                                                                        
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'        CHECK FOR CREDIT                             
         BAS   RE,GETEL                                                         
         BNE   MAKE0010            NOT FOUND                                    
         CLC   2(3,R6),=C'CR='     OLD-STYLE CREDIT LINE?                       
         BE    MAKE0060            YES - EXIT 'NOT FOUND'                       
MAKE0010 EQU   *                                                                
         OC    ANXTMG,ANXTMG       NEXT MG WANTED?                              
         BNZ   MAKE0020            YES                                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'05'        GET M/G ELEMENT                              
         BAS   RE,GETEL                                                         
         B     MAKE0040                                                         
MAKE0020 EQU   *                                                                
         MVI   ELCODE,X'05'        GET M/G ELEMENT                              
         L     R6,ANXTMG           SET A(LAST M/G X'05' ELT)                    
         BAS   RE,NEXTEL                                                        
***      LA    RF,RBUYREC                                                       
***      CLI   TESTBYTE,2                                                       
***      BNE   *+6                                                              
***      DC    H'0'                                                             
MAKE0040 EQU   *                                                                
         BNE   MAKE0060            NOT FOUND:  EXIT CC NOT ZERO                 
         ST    R6,ANXTMG           FOUND:  SAVE A(05 ELT)                       
MAKE0050 EQU   *                                                                
         BAS   RE,SETUPMG          SET UP M/G RECORD AS O/P                     
         SR    R0,R0               SET CC = ZERO                                
         B     MAKE0080                                                         
MAKE0060 EQU   *                                                                
         XC    ANXTMG,ANXTMG       NO MORE FOUND                                
         LTR   RB,RB               SET CC NOT ZERO                              
MAKE0080 EQU   *                                                                
         XIT1                                                                   
TESTBYTE DC    XL1'00'                                                          
         DS    0F                                                               
         EJECT                                                                  
SETUPMG  NTR1                                                                   
         MVC   E2TRAN,=C'AM'       SET RECORD TRAN CODE                         
         MVC   E2PRDR,FOXZEROS     SET ALL PRE-EMPT FLDS TO ZERO                
         MVC   E2PRLN,FOXZEROS                                                  
         MVC   E2PRDT,FOXZEROS                                                  
         GOTO1 ORIGBUY             FIND ORIG BUY IN MGGROUP TABLE               
*                                                                               
         EDIT  LASTGRUP,(3,E2MGGP),FILL=0                                       
         EDIT  BYTE,(5,E2PRLN),FILL=0                                           
                                                                                
         OC    MISDDATE,MISDDATE                                                
         BZ    SUPM0020                                                         
         GOTO1 DATCON,DMCB,(3,MISDDATE),(X'20',WORK)                            
*                                  INSERT MISSED (PRE-EMPT) DATE                
         MVC   E2PRDT(4),WORK+2                                                 
         MVC   E2PRDT+4(2),WORK                                                 
SUPM0020 EQU   *                                                                
         CLI   ORIGBUYF,1          SPOT FOUND?                                  
         BE    SUPM0080            NO  - EXIT                                   
*                                                                               
         L     R4,FULL             SET A(MKGWORK TABLE ENTRY)                   
         EDIT (1,DDRNR(R4)),(3,E2PRDR),FILL=0                                   
*                                  INSERT DATED REV #                           
         BAS   RE,DROPSPOT         DECREMENT SPOT COUNT FOR THIS DATE           
SUPM0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    DROPSPOT:  WITHIN THIS MAKEGOOD GROUP, THE SPOT COUNT FOR THIS             
*        DATE MUST BE DROPPED BY 1 SPOT (NO MORE, NO LESS).  WHEN               
*        ALL MAKEGOODS ARE PROPERLY OUTPUT, THE REMAINING SPOTS WILL            
*        BE CONSIDERED AS UNMATCHED PRE-EMPTS, AND PUT OUT AS SUCH.             
*                                                                               
DROPSPOT NTR1                                                                   
         L     R2,STARTGRP         SET A(START OF THIS MAKEGOOD GROUP)          
         ST    R2,LOOPGRUP         SAVE A(START)                                
         GOTO1 DATCON,DMCB,(3,MISDDATE),(2,WORK)                                
*                                  COMPRESSED DATE OF PRE-EMPT SPOT             
         ZIC   R0,CTWINGP(R2)      SET COUNT W/IN THIS MAKEGOOD GROUP           
DSPO0010 EQU   *                                                                
         LA    R4,DTARDAT1(R2)     SET A(1ST DATE WITHIN ENTRY)                 
         LA    R6,DTAR#SP1(R2)     SET A(1ST SPOT COUNT W/IN ENTRY)             
         LA    R2,MGGRPCT          SET MAX DATES W/IN MG                        
DSPO0020 EQU   *                                                                
         CLC   WORK(2),0(R4)       DATE FOUND IN MAKEGOOD ?                     
         BE    DSPO0040            YES                                          
DSPO0030 EQU   *                                                                
         LA    R4,LTAR#(R4)        BUMP A(DATE POINTER) W/IN M/G                
         LA    R6,LTAR#SP(R6)      BUMP A(SPOT COUNT) W/IN M/G                  
         BCT   R2,DSPO0020         GO BACK FOR NEXT                             
         L     RF,LOOPGRUP         BUMP TO NEXT M/G IN GROUP                    
         LA    RF,LMGGROUP(RF)                                                  
         ST    RF,LOOPGRUP                                                      
         LR    R2,RF                                                            
         BCT   R0,DSPO0010         PROCESS NEXT M/G IN GROUP                    
         DC    H'0'                NO SPOT FOUND:  ERROR                        
DSPO0040 EQU   *                                                                
         ZIC   RF,0(R6)            GET SPOT COUNT                               
         LTR   RF,RF               ANY SPOTS LEFT?                              
         BZ    DSPO0030            NO  - KEEP SCANNING                          
         BCTR  RF,0                YES - DECREMENT BY 1                         
         STC   RF,0(R6)            PUT DECREMENTED COUNT BACK                   
         B     DSPO0080            FINISHED                                     
DSPO0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ORIGBUY:  FIND THIS BUY IN THE ORIGINAL MG GROUP TABLE.  DETERMINE          
*        ITS POSITION WITHIN THE MG GROUP ITSELF.                               
*                                                                               
ORIGBUY  NTR1                                                                   
         MVI   LASTGRUP,0          CLEAR COMPARISON FIELD                       
         MVI   POSIGRUP,0          CLEAR POSITION   FIELD                       
         MVI   GENUNMAT,0          CLEAR GENERATE UNMATCHED PREEMPTS            
*                                                                               
         SR    R4,R4               SET COUNTER TO ZERO                          
         LA    R2,MGGROUP          SET A(MG GROUP TABLE)                        
OBUY0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BNE   *+6                 NO  -                                        
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLC   LASTGRUP,DMGRP#(R2) SAME GROUP?                                  
         BE    OBUY0040            YES -                                        
         MVI   POSIGRUP,0          NO  - SET POSITION TO ZERO                   
         MVC   LASTGRUP,DMGRP#(R2) SAVE GROUP #                                 
         ST    R2,STARTGRP         SAVE A(START OF GROUP)                       
OBUY0040 EQU   *                                                                
         ZIC   RF,POSIGRUP         INCREMENT POSITION IN GROUP                  
         LA    RF,1(RF)                                                         
         STC   RF,POSIGRUP         SAVE POSITION IN GROUP                       
         CLC   RBUYKLIN,DORIGBLN(R2)                                            
*                                  BUY# = ORIG BUYLINE IN TABLE?                
         BE    OBUY0060            YES                                          
         LA    R2,LMGGROUP(R2)     NO  - BUMP TO NEXT TABLE SLOT                
         B     OBUY0020            GO BACK FOR NEXT                             
OBUY0060 EQU   *                                                                
*                                  ACCUMULATE THIS M/G BUY'S SPOTS              
         L     RF,STARTGRP         SET A(START OF GROUP) - SPOTS GIVEN          
*                                     ARE ONLY ADDED IN FIRST ENTRY             
*                                        OF MAKEGOOD GROUP                      
         ZIC   R1,SPTSGIVN(RF)     EXTRACT SPOTS GIVEN FROM TABLE               
         ZICM  RE,RBUYTSPT,2       BUY TOTAL SPOTS                              
         AR    R1,RE               ACCUMULATE TOTALS                            
         STC   R1,SPTSGIVN(RF)     PUT IT BACK IN TABLE                         
*                                                                               
         CLC   POSIGRUP,CTWINGP(R2)                                             
*                                  POSITIONED AT LAST M/G IN GROUP?             
         BNE   OBUY0070            NO                                           
         MVC   GENUNMAT,POSIGRUP   YES - GENERATE UNMATCHED PREEMPTS            
*                                     AFTER THIS BUYLINE PUT OUT                
*                                  SAVE COUNT OF LAST M/G IN GROUP              
*                                     AND USE AS FLAG                           
         ST    R2,AMGUNMAT         SAVE A(MGGROUP IN PROGRESS)                  
OBUY0070 EQU   *                                                                
         MVI   BYTE,0              CLEAR M/G MISSED LINE NUMBER                 
         XC    MISDDATE,MISDDATE   CLEAR MISSED DATE                            
*                                                                               
         BAS   RE,FINDSPOT         FIND NEXT PRE-EMPT SPOT/DATE                 
         BNZ   OBUY0180            NO SPOT:  MULTI-FOR-LESS                     
*                                                                               
         L     R6,FULL             SET A(DATE FOUND)                            
         LR    RF,R6               BACK UP 1 BYTE FOR LINE NUMBER               
         BCTR  RF,0                                                             
         MVC   BYTE,0(RF)          SAVE M/G MISSED LINE NUMBER                  
         MVC   WORK(2),0(R6)       SAVE DATE COMPRESSED                         
         GOTO1 DATCON,DMCB,(2,WORK),(3,MISDDATE)                                
         LA    R6,MKGWORK          FIND M/G LINE IN MKGWORK TABLE               
OBUY0140 EQU   *                                                                
         CLI   0(R6),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         CLC   BYTE,DBUYLINE(R6)   BUYLINE FOUND?                               
         BE    OBUY0160            YES                                          
         LA    R6,LMKGWORK(R6)     NO  - BUMP TO NEXT SLOT                      
         B     OBUY0140            GO BACK FOR NEXT                             
OBUY0160 EQU   *                                                                
         CLC   WORK(2),DSTDATE(R6) MISSED DATE WITHIN TABLE ENTRY?              
         BNL   *+6                                                              
         DC    H'0'                SHOULD NEVER BE LOW                          
         CLC   WORK(2),DENDATE(R6)                                              
         BNH   OBUY0170            BETWEEN LOW AND HIGH: USE IT                 
         LA    R6,LMKGWORK(R6)     HIGHER: LOOK AT NEXT TABLE SLOT              
         B     OBUY0140            GO BACK FOR NEXT                             
OBUY0170 EQU   *                                                                
         ST    R6,FULL                                                          
         MVI   ORIGBUYF,0          SET CC = ZERO                                
         B     OBUY0200                                                         
OBUY0180 EQU   *                                                                
         MVI   ORIGBUYF,1          SET CC NOT ZERO                              
OBUY0200 EQU   *                                                                
*                                  INSERT MISSED (PRE-EMPT) DATE                
*   TEST                                                                        
***      CLC   =C'00010',E2BYLN                                                 
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    FINDSPOT:  FIND THE NEXT SPOT THAT REPRESENTS A PREEMPT WITHIN             
*        THIS MAKEGOOD GROUP.  PASS ADDR OUT IN FULL                            
*                                                                               
FINDSPOT NTR1                                                                   
         L     R2,STARTGRP         SET A(START OF THIS MAKEGOOD GROUP)          
         ST    R2,LOOPGRUP         SAVE A(START)                                
         ZIC   R0,CTWINGP(R2)      SET COUNT W/IN THIS MAKEGOOD GROUP           
FSPO0010 EQU   *                                                                
         LA    R4,DTARDAT1(R2)     SET A(1ST DATE WITHIN ENTRY)                 
         LA    R6,DTAR#SP1(R2)     SET A(1ST SPOT COUNT W/IN ENTRY)             
         LA    R2,MGGRPCT          SET MAX DATES W/IN MG                        
FSPO0020 EQU   *                                                                
         CLI   0(R6),0             ANY PRE-EMPT SPOTS LEFT?                     
         BNE   FSPO0040            YES                                          
FSPO0030 EQU   *                                                                
         LA    R4,LTAR#(R4)        BUMP A(DATE POINTER) W/IN M/G                
         LA    R6,LTAR#SP(R6)      BUMP A(SPOT COUNT) W/IN M/G                  
         BCT   R2,FSPO0020         GO BACK FOR NEXT                             
         L     RF,LOOPGRUP         BUMP TO NEXT M/G IN GROUP                    
         LA    RF,LMGGROUP(RF)                                                  
         ST    RF,LOOPGRUP                                                      
         LR    R2,RF                                                            
         BCT   R0,FSPO0010         PROCESS NEXT M/G IN GROUP                    
         LTR   RB,RB               SET CC NOT ZERO                              
         B     FSPO0060                                                         
FSPO0040 EQU   *                                                                
         ST    R4,FULL             SET A(DATE FOUND)                            
         SR    R0,R0               SET CC = ZERO                                
FSPO0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   THIS ROUTINE SCANS BUY FOR CREDITS AND SENDS RECORDS                        
*                                                                               
*--------------------------------------------------------------------           
GENCREDS NMOD1 0,*GCRD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
GCRD0020 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'16'        GET CREDIT ELEMENT                           
         BAS   RE,GETEL                                                         
         B     GCRD0060                                                         
GCRD0040 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
GCRD0060 EQU   *                                                                
         BNE   GCRD0800            NOT FOUND                                    
         USING RBUYCAEL,R6                                                      
***      MVC   EORTYP,LASTLINE     RESTORE LAST LINE GENERATED                  
         MOVE  (EORTYP,320),LASTLINE                                            
         MVC   E2PRLN,E2BYLN       CREDITS ARE TAKEN DIRECTLY                   
*                                     FROM SAME LINE                            
         MVC   E2TRAN,=C'CP'       SET 'CREDIT PRE-EMPT'                        
*                                                                               
*    INSERT CREDIT START DATE                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYCASD),(X'20',HOLDDATE)                        
         MVC   E2STDT(4),HOLDDATE+2         INSERT MM/DD                        
         MVC   E2STDT+4(2),HOLDDATE         INSERT YY                           
*                                                                               
         OC    RBUYCAED,RBUYCAED   ANY END DATE?                                
         BZ    GCRD0080            NO                                           
*                                                                               
*    INSERT CREDIT END   DATE                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYCAED),(X'20',HOLDDATE)                        
         MVC   E2ENDT(4),HOLDDATE+2         INSERT MM/DD                        
         MVC   E2ENDT+4(2),HOLDDATE         INSERT YY                           
         B     GCRD0100                                                         
GCRD0080 EQU   *                                                                
         MVC   E2ENDT,E2STDT       SET END DATE = START DATE                    
GCRD0100 EQU   *                                                                
         MVC   E2PRDT,E2STDT                                                    
         EDIT  RBUYCASP,(3,E2NRWK),FILL=0                                       
         DROP  R6                                                               
         GOTO1 PRNTREC2,DMCB,(RC),1                                             
         B     GCRD0040            GO BACK FOR NEXT CREDIT LINE                 
GCRD0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   L0CALIZED PRINTREC ROUTINE                                                  
*                                                                               
PRNTREC2 NTR1                                                                   
         CLI   7(R1),0             SAVE CURRENT LINE?                           
         BNE   PNT20010            NO  - LEAVE AS IS                            
         CLI   EORTYP,C'B'         BUYLINE?                                     
         BNE   PNT20010            NO  - SAVE ONLY BUYLINE                      
***      MVC   LASTLINE,EORTYP     SAVE LINE TO BE SENT                         
         MOVE  (LASTLINE,320),EORTYP                                            
PNT20010 EQU   *                                                                
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         BCTR  RF,0                YES - DON'T COUNT IT IN TOTALS               
         STC   RF,TRANSCNT         SAVE REVISED COUNT                           
*                                  INSERT RECORD COUNT                          
PNT20020 EQU   *                                                                
         LA    R2,3                LOOP CONTROL FOR PRINTING                    
         LA    R4,EORTYP           A(OUTPUT RECORD)                             
PNT20040 EQU   *                                                                
         CLC   =C'DOWNLOADER',RCONBUYR                                          
*                                  FORMAT OUTPUT AS DOWNLOADABLE?               
         BE    PNT20060            YES                                          
*                                  NO  - STANDARD OUTPUT                        
         MVC   P(093),0(R4)                                                     
         MVC   P+093(3),=C'DDS'    SENTINEL                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,093(R4)          BUMP TO NEXT SEGMENT OF RECORD               
         BCT   R2,PNT20040         DO NEXT SEQMENT OF RECORD                    
         MVC   P(041),0(R4)        SEND 4TH SEGMENT AS 41 CHARS                 
         MVC   P+041(3),=C'DDS'    SENTINEL                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PNT20050 EQU   *                                                                
         XIT1                                                                   
PNT20060 EQU   *                                                                
         MVC   P+1(093),0(R4)                                                   
         MVI   P,C'"'              INSERT TEXT DOWNLOAD INDS                    
         MVI   P+094,C'"'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,093(R4)          BUMP TO NEXT SEGMENT OF RECORD               
         BCT   R2,PNT20060         DO NEXT SEQMENT OF RECORD                    
         MVC   P+1(041),0(R4)      SEND 4TH SEGMENT AS 41 CHARS                 
         MVI   P,C'"'              INSERT TEXT DOWNLOAD INDS                    
         MVI   P+042,C'"'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     PNT20050            DONE                                         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*                                                                               
*   THIS ROUTINE SETS UP A TABLE OF CREDIT SPOTS FOR OLD-STYPE CREDITS          
*                                                                               
*---------------------------------------------------------------------          
LOGCREDS NMOD1 0,*LCRD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(LOCALWORK)                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'07'        LOOK FOR CREDIT ELEMENT(S)                   
         BAS   RE,GETEL            GET FIRST CREDIT ELEMENT                     
         B     LCRD0040                                                         
LCRD0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT CREDIT ELEMENT                      
LCRD0040 EQU   *                                                                
         BNE   LCRD0120            NOT FOUND - EXIT                             
         L     R2,ACRDWORK         SET A(NEXT CREDIT SLOT)                      
         CLC   0(2,R2),=X'FFFF'    TABLE FULL?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - 100 CREDIT ITEMS IN ORDER              
         MVC   0(1,R2),RBUYKLIN    INSERT BUYLINE # INTO TABLE                  
         USING RBUYCREL,R6                                                      
         MVC   1(1,R2),RBUYCRLI    INSERT CREDIT BUYLINE # INTO TABLE           
         DROP  R6                                                               
         LA    R2,LCRDWORK(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ACRDWORK         SAVE NEXT SLOT                               
         B     LCRD0020            GO BACK FOR NEXT                             
LCRD0120 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   THIS ROUTINE EXPLODES 03 ELEMENTS IF BUY IS FOR ALTERNATE WEEKS.            
*                                                                               
*--------------------------------------------------------------------           
CYCLEALT NMOD1 0,*CALT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
CALT0020 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        GET EFFECTIVE DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   CALT0800            NOT FOUND                                    
         USING RBUYDTCD,R6                                                      
CALT0040 EQU   *                                                                
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK INDIC SET?                    
         BO    CALT0060            YES - EXPLODE THIS ELEMENT                   
         BAS   RE,NEXTEL           NO  - GET NEXT ELEMENT                       
         BNE   CALT0800            NO MORE - FINISHED                           
         B     CALT0040            GO BACK AND CHECK ELEMENT                    
CALT0060 EQU   *                                                                
         MVC   OLD03ELT,0(R6)      YES - SAVE OLD 03 ELEMENT                    
         MVC   NEW03ELT,0(R6)      ALSO SET 1ST NEW 03 ELEMENT                  
         MVI   NEW03ELT+10,1       SET NUMBER WEEKS TO 1                        
         NI    NEW03ELT+8,X'FF'-X'40'                                           
*                                  TURN OFF ALTERNATING BIT                     
         OI    NEW03ELT+8,X'80'                                                 
*                                  TURN ON EVERY WEEK BIT                       
         MVI   0(R6),X'FF'         SET OLD 03 TO BE DROPPED                     
         GOTO1 VDELELEM,DMCB,(X'FF',RBUYREC)                                    
*                                  DROP 03 ELEMENT                              
         BAS   RE,EXPLOD03         EXPLODE OLD 03 INTO NEW                      
         B     CALT0020            GO BACK AND CHECK OTHERS                     
CALT0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   EXPLODE 03 ELEMENT IN OLD03ELT INTO ALTERNATE WEEK 03 ELEMS.                
*                                                                               
EXPLOD03 NTR1                                                                   
         ZIC   R5,OLD03ELT+10      SET # OF WEEKS LOOP                          
*                                                                               
*   GET FLIGHT START DAY OF WEEK                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,OLD03ELT+2),(0,WORK)                              
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                                                               
         ZIC   R4,DMCB             FLIGHT START DAY OF WEEK                     
*                                                                               
*   GET FLIGHT END   DAY OF WEEK                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,OLD03ELT+5),(0,WORK)                              
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                                                               
         ZIC   RF,DMCB             FLIGHT END DAY OF WEEK                       
         CR    RF,R4               FLIGHT END => START?                         
         BNL   XPLD0020            YES                                          
         A     RF,=F'7'            NO  - BUMP INTO NEXT WEEK                    
XPLD0020 EQU   *                                                                
         SR    RF,R4               CALCULATE DAY DIFFERENCE                     
         ST    RF,DAYDIFF          SAVE DAY DIFFERENCE                          
XPLD0040 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,NEW03ELT+2),(0,WORK)                              
*                                  CONVERT NEW START DATE TO EBCDIC             
         L     R4,DAYDIFF                                                       
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4)                                     
*                                  CALCULATE NEW END DAY                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,NEW03ELT+5)                           
*                                  CONVERT NEW END DAY TO BINARY                
****     GOTO1 VADDELEM,DMCB,RBUYREC,NEW03ELT                                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,NEW03ELT,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
         GOTO1 DATCON,DMCB,(3,NEW03ELT+2),(0,WORK)                              
*                                  CONVERT ELT START DATE TO EBCDIC             
         LA    R4,14               BUMP START DATE TWO WEEKS                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4)                                     
*                                  CALCULATE NEW START DAY                      
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,NEW03ELT+2)                           
*                                  REINSERT INTO ELEMENT                        
         BCT   R5,XPLD0040         GO BACK AND EXPLODE NEXT 03 ELT              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015RECNT7ES  05/10/04'                                      
         END                                                                    
