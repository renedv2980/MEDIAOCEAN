*          DATA SET RECNT76    AT LEVEL 038 AS OF 10/02/03                      
*          DATA SET RECNT76    AT LEVEL 246 AS OF 06/15/00                      
*PHASE T80276A,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENPBY                                                               
*INCLUDE REGENBUC                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T80276 - RECNT76 - COLUMBINE EC CHANGES'                        
*********************************************************************           
*                                                                   *           
*        RECNT76 --- COLUMBINE  ELECTRONIC CONTRACT INTERFACE       *           
*                    BASED ON JDS/2000 EC W/CHANGES SPECIFICATION   *           
*                                                                   *           
*        THIS IS AN ENTIRELY NEW MODULE.  AS SUCH, IT IS NOT BEING  *           
*        OFFERED TO ALL CLIENTS.  TO CONTROL ACCESS, IT IS BEING    *           
*        LOADED AS A SEPARATE TRAFFIC FORMAT, SET IN THE STATION    *           
*        RECORD OF THOSE STATIONS WHICH ARE TO UTILIZE IT RATHER    *           
*        THAN THE OLD FORMAT.                                       *           
*        CONTROL AT THE STATION END IS THE RESPONSIBILITY OF        *           
*        COLUMBINE.                                                 *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN02/99 (BU ) --- ORIGINAL ENTRY - REDESIGN TO NEW JDS SPEC,     *           
*                    W/CONTRACT CHANGES                             *           
*                                                                   *           
* MAR13/00 (BU ) --- INCLUDE EDICT HEADER, CLASS G                  *           
*                                                                   *           
* AUG11/00 (BU ) --- SET LOCAL FLAG PER NEW MODELS                  *           
*                                                                   *           
* DEC06/00 (BU ) --- SET EOP R100 LENGTHS                           *           
*                                                                   *           
* JAN17/01 (BU ) --- ADJUSTED PLAN PROCESSING                       *           
*                                                                   *           
* FEB12/01 (BU ) --- TRADE AGENCY/ADV PROCESSING, TD/TC ALSO        *           
*                                                                   *           
* APR19/01 (BU ) --- END TIME = CC PROCESSING                       *           
*                                                                   *           
* JUL05/01 (BU ) --- SKIP XCLD BUYLINES                             *           
*                                                                   *           
* SEP16/02 (BU ) --- FOX (FB) = FXFX: PER DEAN HARDER               *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
T80276   CSECT                                                                  
         NMOD1 LOCALEND-LOCALWRK,*T80276*,R9,RR=R8                              
         ST    R8,RELX                                                          
         LR    R3,RC               SET A(MODULE WORK SPACE)                     
         USING LOCALWRK,R3         SET DSECT FOR AREA                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
****>    LA    R8,SPOOLAR          SET A(SPOOLAR) RATHER INDIRECTLY             
*        LA    R8,RBUYREC             3000 AFTER RBUYREC                        
*        A     R8,=F'3000'                                                      
         L     R8,ASPULAR                                                       
         ST    R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*********************************************************************           
*        MAIN LINE PROCESSING                                                   
*********************************************************************           
MAIN     EQU   *                                                                
         GOTO1 =A(INIT),RR=Y       SET INITAL ADDRS AND VALUES                  
         BAS   RE,GETCON           READ CONTRACT RECORD                         
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(GETSTA),DMCB,(RC),RCONKSTA,RR=YES                             
*                                  READ STATION RECORD INTO AIO3                
         BNZ   EXXMOD                                                           
*                                                                               
* DON'T USE 'HALF' IN NEXT SUBROUTINE (USING TO STORE STATION OPTION)           
         GOTO1 READEOP,DMCB,(RC)   RETRIEVE EOP CODES                           
*                                                                               
         CLI   HALF,C'Y'           IS OPTION TURNED ON?                         
         BNE   MN090               NO, SKIP TEST                                
*                                                                               
         LA    RF,EOPRECS          ARE ALL CODES PRESENT?                       
         LA    RE,4                                                             
MN050    EQU   *                                                                
         OC    0(12,RF),0(RF)      CODE PRESENT?                                
         BZ    MN075               NO  - RETURN ERROR                           
         LA    RF,12(RF)           YES - BUMP TO NEXT CODE                      
         BCT   RE,MN050                                                         
         B     MN090               ALL PRESENT                                  
MN075    EQU   *                                                                
         LA    R3,583              SET 'EOP CODE MISSING'                       
         LA    R2,CONCNUMH                                                      
         B     ERROR                                                            
MN090    EQU   *                                                                
*                                                                               
         GOTO1 =A(FINDID),DMCB,(RC),RR=YES      FIND SEND ID                    
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(PQOPENA),DMCB,(RC),(R8),RR=YES                                
*                                  OPEN PRINT QUEUE                             
         GOTO1 =A(EDICT),DMCB,(RC),(R8),RR=YES                                  
*                                  CREATE EDICT HEADER CARDS                    
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
         GOTO1 =A(TOTLBUYS),DMCB,RR=Y   READ BUYS TO GET EARLY/LATE             
*                                     SPOT DATES                                
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         '0301' HEADER                                
*                                                                               
         CLC   SAVEST#,SPACES      ANY ESTIMATE NUMBER?                         
         BNH   MN250               NO                                           
         GOTO1 =A(BLD08REC),RR=Y   '0308' EDI RECORD                            
MN250    EQU   *                                                                
*                                                                               
MN300    EQU   *                                                                
         BAS   RE,BLD02REC         '0302' ORDER HEADER COMMENTS                 
*                                                                               
MN400    EQU   *                                                                
         BAS   RE,BLD03REC         '0303' LINE ADD                              
*                                                                               
MN500    EQU   *                                                                
         BAS   RE,BLD10REC         '0310' ORDER ADD FINAL BUFFER                
*                                                                               
MN600    EQU   *                                                                
         GOTO1 =A(DATETIME),RR=Y   DATE AND TIME STAMP                          
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
                                                                                
*********************************************************************           
*        BLD01REC  ---  BUILDS THE '0201' TRANSACTION RECORD                    
*********************************************************************           
BLD01REC NTR1                                                                   
         GOTO1 READEOP,DMCB,(RC)                                                
         MVI   RC301REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC301REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC301LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC301REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*    SHOULD NOT BE A PROBLEM WITH NATIONAL VS LOCAL REP.....                    
*                                                                               
         MVC   RC301ID,=C'0301'    LOAD RECORD TYPE                             
         MVC   RC301STA(5),RCONKSTA INSERT STATION+MEDIA                        
         CLI   RCONKSTA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC301STA+4(2),=C'TV'                                             
         MVC   RC301STA+6(1),RCONKGRP                                           
*                                  INSERT 1ST CHAR OF GROUP                     
*                                                                               
         MVC   RC301NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC301SOF(2),RCONKOFF   LOAD OFFICE CODE                          
*                                                                               
         MVC   RC301REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC301REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
*   TEST FOR EOP RECORDS FOUND.  IF NOT FOUND, CODE IS LEFT EMPTY.              
*        IF FOUND, CODE IS INSERTED.  IN ALL CASES, THE DDS NAME                
*        IS INSERTED WHERE CALLED FOR.                                          
*                                                                               
         MVC   RC301AGY,FOXZEROS   SET TO DEFAULT                               
         OC    EOPAGY,EOPAGY       ANY VALUE IN AGENCY?                         
         BZ    BLD10020            NO  - SKIP LOADING CODE                      
         MVC   RC301AGY,EOPAGY     YES - LOAD AGENCY CODE                       
BLD10020 EQU   *                                                                
         MVC   RC301AGN(20),AGYNAME                                             
*                                  LOAD AGENCY NAME FROM SUPPORT                
         MVC   RC301ADV,FOXZEROS   SET TO DEFAULT                               
         OC    EOPADV,EOPADV       ANY VALUE IN ADVERTISER?                     
         BZ    BLD10040            NO  - SKIP LOADING CODE                      
         MVC   RC301ADV,EOPADV     YES - LOAD ADVERT CODE                       
BLD10040 EQU   *                                                                
         MVC   RC301ADN(20),ADVNAME                                             
*                                  LOAD ADVERT NAME FROM SUPPORT                
*                                                                               
*   NO EOP EQUIVALENCING OF OFFICE??? THAT'S CORRECT!!                          
*                                                                               
*        MVC   RC301SOF,FOXZEROS   SET TO DEFAULT                               
***      OC    EOPOFF,EOPOFF       ANY VALUE IN OFFICE?                         
***      BZ    BLD10060            NO  - SKIP LOADING CODE                      
***      MVC   RC301SOF,EOPOFF     YES - LOAD OFFICE CODE                       
BLD10060 EQU   *                                                                
*                                                                               
         MVC   RC301SAL,=C'000'    INITIALIZE TO ZERO                           
         OC    EOPSAL,EOPSAL       ANY VALUE IN S/P?                            
         BZ    BLD10080            NO  - SKIP LOADING CODE                      
         MVC   RC301SAL,EOPSAL     YES - LOAD S/P    CODE                       
BLD10080 EQU   *                                                                
         MVC   RC301SAN(20),SALNAME                                             
*                                  LOAD S/P NAME FROM SUPPORT                   
         GOTO1 DATCON,DMCB,(3,RCONCREA),(X'20',CNVDATE)                         
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC301ORD+3(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC301ORD+1(2),CNVDATE INSERT YY                                  
*                                                                               
*   IF YEAR IS > 70, DATE IS (MOST LIKELY) IN THE 20TH CENTURY.                 
*        LESS THAN THAT INDICATES 21ST CENTURY.  BEYOND THAT,                   
*        I DON'T THINK THE PROGRAM WILL BE AROUND THAT LONG.                    
*                                                                               
         MVI   RC301ORD,C'0'       SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    BLD10100            YES                                          
         MVI   RC301ORD,C'1'       NO  - 21ST CENTURY                           
BLD10100 EQU   *                                                                
*                                                                               
*   SET START AND END DATE FROM SAVED EARLIEST BUY START AND                    
*        LATEST BUY END DATES  (NOT CONTRACT FLIGHTS).                          
*                                                                               
         MVC   RC301STD(14),SPACES                                              
         GOTO1 DATCON,DMCB,(3,ERLYSTRT),(X'20',CNVDATE)                         
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC301STD+3(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC301STD+1(2),CNVDATE INSERT YY                                  
*                                                                               
*   IF YEAR IS > 70, DATE IS (MOST LIKELY) IN THE 20TH CENTURY.                 
*        LESS THAN THAT INDICATES 21ST CENTURY.  BEYOND THAT,                   
*        I DON'T THINK THE PROGRAM WILL BE AROUND THAT LONG.                    
*                                                                               
         MVI   RC301STD,C'0'       SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    BLD10120            YES                                          
         MVI   RC301STD,C'1'       NO  - 21ST CENTURY                           
BLD10120 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,LATREND),(X'20',CNVDATE)                          
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC301END+3(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC301END+1(2),CNVDATE INSERT YY                                  
         MVI   RC301END,C'0'       SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    BLD10140            YES                                          
         MVI   RC301END,C'1'       NO  - 21ST CENTURY                           
BLD10140 EQU   *                                                                
*                                                                               
         MVC   RC301PRD,PRODNAME   LOAD PRODUCT NAME FROM SUPPORT               
**DEMO>>                                                                        
*                                                                               
* DISPLAY FIRST PRIMARY DEMO ONLY.  IF NONE EXISTS, DISPLAY FIRST DEMO          
*                                                                               
         LA    R6,RCONREC                                                       
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'        RETRIEVE SAR ELEMENT                         
         BAS   RE,GETEL            NONE FOUND - SKIP DEMO                       
         BNE   BLD10146                                                         
         USING RSARXEL,R6                                                       
         LA    RE,RSARXDEM                                                      
         LA    RF,6                                                             
BLD10142 EQU     *                                                              
         TM    0(RE),X'40'         CHECK FOR PRIMARY DEMO                       
         BO    BLD10144                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,BLD10142                                                      
         LA    RE,RSARXDEM         NO PRIMARY, JUST TAKE FIRST                  
         DROP  R6                                                               
BLD10144 EQU   *                                                                
         LA    R5,WORK                                                          
         XC    WORK(30),WORK                                                    
         MVC   0(3,R5),0(RE)                                                    
                                                                                
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
                                                                                
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
*                                                                               
         LA    R5,WORK                                                          
*                                                                               
         GOTO1 DEMCON,DMCB,(1,(R5)),(6,RC301DEM),(0,DBLOCKD)                    
BLD10146 EQU   *                                                                
**DEMO>>                                                                        
*   NO BUYER FIELD IN COLUMBINE    LOAD BUYER NAME FROM CON REC                 
*                                                                               
*                                  SET UP ESTIMATE FIELD                        
*                                  EST FIELD ALREADY SET TO SPACES              
***********************************************************************         
*   ESTIMATE INFORMATION IS IN THE 0308 RECORD NOW                              
*                                                                               
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'A2'        FIND EASI ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BLD10160            NOT FOUND - SKIP IT                          
         USING RCONIEL,R6                                                       
         MVC   SAVEST#,RCONXEST    SAVE ESTIMATE NUMBER                         
         OC    RCONXEST,RCONXEST                                                
         BNZ   *+10                                                             
         MVC   SAVEST#(4),RCONIEST SAVE 4-CHAR ESTIMATE NUMBER                  
         OC    SAVEST#,SPACES      SET BINARY ZERO TO SPACES                    
*                                                                               
         DROP  R6                                                               
***********************************************************************         
*                                                                               
BLD10160 EQU   *                                                                
*                                                                               
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'13'        FIND EC CONFLICT CODE ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   BLD10190            NOT FOUND                                    
*                                                                               
*    SHOULD THIS BE THIS WAY?  DO WE WANT TO ABORT?  SHOULD THE                 
*        EC ACTION CHECK FOR THE EXISTENCE OF DX/CX, AND REQUIRE                
*        IT BE ENTERED BEFORE ACTION IS ALLOWED?                                
*                                                                               
BLD10180 EQU   *                                                                
*                                                                               
*   LOAD FIELDS FROM EC CONFLICT ELEMENT.                                       
*                                                                               
         USING RCONCJEL,R6                                                      
         MVC   RC301RAT,RCONCJRC   INSERT RATE CARD                             
         OC    RC301RAT,SPACES     CLEAR TO SPACES                              
         MVC   RC301PER,RCONCJCY   INSERT BILLING CYCLE                         
         OC    RC301PER,SPACES     CLEAR TO SPACES                              
****     MVC   RC301TRD,RCONCJTF   INSERT TRADE FLAG                            
***   THIS FIELD WILL BE TAKEN FROM THE DDS STATUS OF THE ORDER.                
*        FLAG ENTERED INTO DX/CX SCREEN WILL BE IGNORED.                        
*                                                                               
         OC    RC301TRD,SPACES     CLEAR TO SPACES                              
         MVC   RC301PC1(8),FOXZEROS                                             
         CLI   RCONCJP1+1,0        ONE OR TWO CHAR CODE?                        
         BE    BLD10182            ONE-CHARACTER                                
         MVC   RC301PC1+2(2),RCONCJP1                                           
*                                  TWO-CHARACTER CODE                           
         B     BLD10184                                                         
BLD10182 EQU   *                                                                
         MVC   RC301PC1+3(1),RCONCJP1                                           
BLD10184 EQU   *                                                                
         CLI   RCONCJP2+1,0        ONE OR TWO CHAR CODE?                        
         BE    BLD10186            ONE-CHARACTER                                
         MVC   RC301PC2+2(2),RCONCJP2                                           
*                                  TWO-CHARACTER CODE                           
         B     BLD10188                                                         
BLD10186 EQU   *                                                                
         MVC   RC301PC2+3(1),RCONCJP2                                           
BLD10188 EQU   *                                                                
         MVC   RC301COP,RCONCJCT   INSERT COOP TYPE                             
         OC    RC301COP,SPACES     CLEAR TO SPACES                              
BLD10190 EQU   *                                                                
         LA    R6,RCONREC                                                       
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'1E'        RETRIEVE SAR ELEMENT                         
         BAS   RE,GETEL            NONE FOUND - SKIP DEMO                       
         BNE   BLD10195                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   BLD10195            NO                                           
         MVI   RC301TRD,C'X'       INSERT TRADE FLAG                            
         DROP  R6                                                               
BLD10195 EQU   *                                                                
*                                                                               
*   NO BREAK SEPARATION FIELD IN COLUMBINE                                      
*                                                                               
***      CLC   =C'BRK',RCONCJCS    'BREAK' SEPARATION?                          
***      BNE   BLD10200            NO                                           
***      MVC   RC301SEP,RCONCJCS   YES - INSERT 'BRK' SEPARATION                
***      B     BLD10220                                                         
*BLD10200 EQU   *                                                               
***      OC    RCONCJCS,RCONCJCS   ANY SEPARATION VALUE?                        
***      BZ    BLD10220            NO  - ALREADY SET TO SPACES                  
***      NI    RCONCJCS,X'FF'-X'80'                                             
*                                  TURN OFF DISPLAY FLAG                        
***      EDIT  RCONCJCS,(3,RC301SEP),FILL=0                                     
BLD10220 EQU   *                                                                
***      MVC   RC301SPC,RCONCJSH   INSERT SPECIAL HANDLING                      
***      MVC   RC301INC,RCONCJIC   INSERT INVOICE COMMENTS                      
***      MVC   RC301INM,RCONCJIM   INSERT INVOICE MESSAGE CODE                  
***      MVC   RC301TAX,RCONCJST   INSERT SALES TAX CODE                        
*                                                                               
*   NO NATIONAL/LOCAL   FIELD IN COLUMBINE                                      
*                                                                               
***      MVC   RC301NRL,RCONCJNR   INSERT NAT/LOC CODE                          
***      CLI   RC301NRL,X'00'      ANY VALUE THERE?                             
***      BNE   BLD10240            MAYBE                                        
***      MVI   RC301NRL,C'N'       NO  - DEFAULT TO 'N'                         
***      B     BLD10260                                                         
*BLD10240 EQU   *                                                               
*        CLI   RC301NRL,C' '       ANY VALUE THERE?                             
*        BNE   BLD10260            YES                                          
*        MVI   RC301NRL,C'N'       NO  - DEFAULT TO 'N'                         
*                                                                               
***      DROP  R6                                                               
*                                                                               
BLD10260 EQU   *                                                                
         MVC   RC301COM,=C'1500000'                                             
*                                  AGENCY COMMISSION DEFAULT                    
         MVC   RC301DIS,FOXZEROS   SET DISCOUNT TO ZERO                         
         MVC   RC301COS,FOXZEROS   SET COOP SORT DIGITS                         
         MVI   RC301PRT,C'X'                                                    
BLD10280 EQU   *                                                                
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        BLD02REC  ---  BUILDS THE '0302' COMMENT RECORD                        
*********************************************************************           
BLD02REC NTR1                                                                   
         MVI   RC302REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC302REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC302LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC302REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   RC302ID,=C'0302'    LOAD RECORD TYPE                             
         MVC   RC302NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC302REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC302REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
         MVC   RC302PRT,=C'XXX'    INSERT PRINT FLAGS                           
         MVI   RC302SPC,C'1'       INSERT FIXED VALUE                           
*                                                                               
*    GENERATE UP TO 2 COMMENTS RECORDS                                          
*    NO RECORD SENT IF THERE ARE NO COMMENTS AT ALL.                            
*                                                                               
         LA    R2,1                SET TEST COUNTER TO 1                        
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'02'        CONTRACT COMMENT ELEMENT                     
         BAS   RE,GETEL            NONE FOUND - FINISHED                        
         BNE   BLD20140            EXIT WITH NO WRITES                          
         B     BLD20060                                                         
BLD20040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   BLD20140            NOT FOUND - CHECK FOR WRITE                  
BLD20060 EQU   *                                                                
         EDIT  (R2),(1,RC302SEQ)   EDIT IN COMMENT NUMBER                       
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         EX    RF,BLD20090         MOVE COMMENT BY LENGTH                       
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         MVC   RC302TXT,SPACES     CLEAR OUT COMMENT                            
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
         CH    R2,=H'2'            TWO COMMENTS DONE?                           
         BH    BLD20140            YES - FINISHED WITH COMMENTS                 
         B     BLD20040            GO BACK FOR NEXT COMMENT                     
BLD20090 MVC   RC302TXT(0),2(R6)   LOAD COMMENT BY LENGTH                       
*                                                                               
BLD20140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE '0203' TRANSACTION RECORD.                         
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE '0203' RECORD PER EFFECTIVE DATE IS GENERATED.                   
*      2.  COMMENTS ARE GEN'D AS '0204' RECORDS                                 
*                                                                               
BLD03REC NTR1                                                                   
BLD30020 EQU   *                                                                
         XC    MGCOUNTR,MGCOUNTR   CLEAR MAKEGOOD COUNTER                       
*                                                                               
         LR    RF,RC               SET A(MKG TABLE BUILD AREA)                  
         A     RF,=AL4(IO4-GENOLD)                                              
*                                                                               
         MVC   0(8,RF),=C'MGTABLE '                                             
         LA    RF,8(RF)            SKIP TABLE TAG                               
         XC    0(11,RF),0(RF)      CLEAR OUT FIRST ENTRY                        
         ST    RF,AMGTABLE         STORE A(MAKEGOOD TABLE)                      
         ST    RF,ANXTMGTB                                                      
*                                                                               
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    COLLAPS2,COLLAPS2   USED IN COLLAPSE BUY ROUTINE                 
         XC    TESTCTR,TESTCTR     **TEST                                       
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   RC303REC,C' '       SPACE FILL RECORD                            
         LA    RF,RC303REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC303LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC303REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30500            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       XCLED BUYLINE?                               
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     XCLED BUYLINE?                               
         BE    BLD30020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    BLD30020            YES - SKIP IT                                
BLD30040 EQU   *                                                                
         GOTO1 =A(MERGE03S),DMCB,(RC),RR=Y                                      
*                                  REMERGE ANY MAKEGOOD SPOTS                   
*                                     TO RESET TO ORIGINAL BUY                  
*                                  ALSO DONE FOR MAKEGOOD RECORDS               
*                                                                               
         GOTO1 =A(CRDIT16S),DMCB,(RC),RR=Y                                      
*                                  REMERGE ANY CREDIT SPOTS                     
*                                     TO RESET TO ORIGINAL BUY                  
*                                                                               
         CLC   RBUYKMLN,RBUYKLIN   MASTER/DETAIL LINE SAME?                     
         BE    BLD30050            YES - REGULAR BUY RECORD                     
*                                                                               
         CLC   LASTMGMS,RBUYKMLN   IF ITS THE SAME MASTER                       
         BE    BLD30050            DON'T TOTAL MISSED SPOTS                     
*                                                                               
* READ ALL THE MAKE GOODS FOR THIS MASTER LINE AND TABLE UP MISSED              
*  SPOTS                                                                        
*                                                                               
         GOTO1 =A(CYCLEMKG),DMCB,(RC),RR=Y                                      
*                                  NO  - MAKEGOOD RECORD                        
BLD30050 EQU   *                                                                
         GOTO1 =A(CYCLEALT),DMCB,(RC),RR=Y                                      
*                                  CYCLE ALTERNATE WEEK BUYS                    
         MVC   SVBYLN#,RBUYKLIN    SAVE BUY LINE #                              
         GOTO1 BLD3CTCM,DMCB,(RC)  SET MG + COMMENT FLAGS                       
         MVC   RC303ID,=C'0303'    INSERT RECORD ID                             
         MVC   RC303NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC303REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC303REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,RC303LIN),FILL=0,ZERO=NOBLANK                         
*                                                                               
         MVI   RC303TYP,C'1'       INSERT FIXED VALUE                           
         MVC   RC303SEP,=C'999'    INSERT FIXED VALUE                           
         MVC   RC303ASP,=C'999'    DITTO                                        
         MVI   RC303FLT,C'1'       SET TO 'ACTIVE EVERY WEEK'                   
         MVC   RC303MKG,=C'00'     DEFAULT TO REGULAR BUY                       
         MVC   RC303PRA,FOXZEROS   SET DEFAULT                                  
         MVC   RC303PUN,=C'00'     SET DEFAULT                                  
         MVI   RC303SEC,C'1'       INSERT FIXED VALUE                           
*                                                                               
         GOTO1 =A(SETTDTCS),RR=Y                                                
*                                  SET FIELDS FROM TD/TC SCREEN                 
*                                                                               
*    INSERT ISSUE DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',HOLDDATE)                            
         MVC   RC303ISS+3(4),HOLDDATE+2     INSERT MM/DD                        
         MVC   RC303ISS+1(2),HOLDDATE       INSERT YY                           
*                                                                               
         MVI   RC303ISS,C'0'       SET TO 2OTH CENTURY                          
         CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
         BH    BLD30060            YES                                          
         MVI   RC303ISS,C'1'       NO  - 21ST CENTURY                           
BLD30060 EQU   *                                                                
         MVI   RC303PRT,C'X'       INSERT FIXED VALUE                           
         CLC   RBUYKMLN,RBUYKLIN   MASTER/DETAIL LINE SAME?                     
         BE    BLD30080            YES - REGULAR BUY                            
         MVC   RC303MKG,=C'01'     SET MAKEGOOD FLAG                            
         CLC   FIRSTMG,RBUYKLIN    FIRST LINE IN SET?                           
         BE    BLD30080            YES                                          
         MVC   RC303MKG,=C'99'     SET ADDITION LINE IN SET                     
         EDIT  (B1,FIRSTMG),RC303OMG,FILL=0                                     
****>>>> MVC   RC303MKD,FIRSTMGD                                                
*                                                                               
*   WHEN LATERUN/REPLACEMENT INFORMATION IS AVAILABLE, TEST IT                  
*        HERE TO SET THE FLAG APPROPRIATELY.                                    
*                                                                               
BLD30080 EQU   *                                                                
*                                                                               
*                                                                               
*   NEW TRAFFIC ELEMENTS:  NOT PRESENT WITHIN DDS.....                          
*                                                                               
**       GOTO1 PRTY,DMCB,(RC)                                                   
*                                                                               
**       GOTO1 PCDISP,DMCB,(RC)                                                 
*                                                                               
**       GOTO1 PLNDISP,DMCB,(RC)                                                
*                                                                               
         GOTO1 DYTIME,DMCB,(RC)                                                 
*                                                                               
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
         L     R6,AEFFDATE         RESET A(EFF DATE ELEMENT)                    
         ZIC   RF,1(R6)            L(EFFECTIVE DATE ELEMENT)                    
         BCTR  RF,0                DECREMENT 1                                  
         EX    RF,BLD30200         MOVE EFF DATE TO TABLE                       
         B     BLD30220                                                         
BLD30200 MVC   BLDTABLE(0),0(R6)                                                
BLD30220 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      ANY MORE '03' ELEMENTS?                      
         BE    BLD30440            NO                                           
         GOTO1 DTEINFO,DMCB,(RC),(R6)                                           
         CLI   SPOTSWK,0           ANY SPOTS IN THIS WEEK?                      
***>>>   BZ    BLD30460            NO  - DON'T PROCESS BUY                      
         BZ    BLD30020            NO  - SKIP THIS BUY: READ NEXT               
         GOTO1 SPTINFO,DMCB,(RC),(R6)                                           
*                                                                               
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   RC303FLT,C'2'       SET INACTIVE WEEKS TO 2                      
BLD30240 EQU   *                                                                
         ZIC   RF,MULTEFDT                                                      
         LA    RF,1(RF)            ADD TO # EFF DATES                           
         STC   RF,MULTEFDT         SAVE IT BACK                                 
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         B     BLD30260                                                         
SUBLINES DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
BLD30260 EQU   *                                                                
         LA    RE,SUBLINES                                                      
         AR    RE,RF                                                            
         ZIC   RF,0(RE)            GET 'SUBLINE' FROM SUBLINES                  
         CLI   COLLAPSE,X'FF'      LAST EFF DT ELEMENT?                         
         BNE   BLD30280            NO  - CONTINUE                               
         CLI   MULTEFDT,1          ONLY ONE MULT EFF DT?                        
         BNE   BLD30280            NO                                           
         MVI   RC303SLN,C'A'       YES - SET TO A                               
         B     BLD30380                                                         
BLD30280 EQU   *                                                                
         STC   RF,RC303SLN         INSERT 'SUBLINE' FROM SUBLINES               
BLD30380 EQU   *                                                                
*                                                                               
*   WHEN SINGLE DAY/TIME ELT IS A FIXED DAY (IE, MON/10P),                      
*        CLEAR THE X'S IN THE CALENDAR, AND LOAD THE SPOTS/WK.                  
*                                                                               
         CLI   DTSTRNGS,2          > 1 D/T STRING?                              
         BNL   BLD30400            YES - LEAVE AS IS                            
         LA    R6,RBUYREC          NO  - LOOK FOR D/T STRING ELEMENT            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         ZIC   R0,2(R6)            GET START/END DAY                            
         SRDL  R0,4                SHIFT END DAY OUT OF R0                      
         SRL   R1,28               MOVE TO LOW ORDER                            
         CR    R0,R1               SINGLE DAY?                                  
         BNE   BLD30400            NO                                           
*                                                                               
*    FOR FIXED DAY BUY, CLEAR X'S, MOVE IN # SPOTS - EMPTY DAYS                 
*        RECEIVE 'SPACES' (X'40')                                               
*                                                                               
         MVC   RC303PAT,SPACES                                                  
         LA    R4,RC303PAT         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         SLL   R0,1                DOUBLE FOR TWO-BYTE FIELD LENGTHS            
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTSWK,(2,0(R4)),FILL=0,ZERO=NOBLANK                            
BLD30400 EQU   *                                                                
         EDIT  SPOTSWK,(2,RC303SPT),FILL=0,ZERO=NOBLANK                         
         LA    R2,SUBLINES                                                      
         ZIC   RF,MULTEFDT         CURRENT NUMBER OF EFF DATES                  
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         AR    R2,RF               DISPLACE TO SUBLINE #                        
         MVC   RC303SLN,0(R2)      INSERT SUBLINE #                             
         MVC   SVSL#,0(R2)         SAVE SUB LINE #                              
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
         BE    BLD30410            NO                                           
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
*                                     WILL BE ADDED AFTER 1ST BUYLINE           
*                                        ONLY, BASED ON COMFLAGS SWITCH         
BLD30410 EQU   *                                                                
         XC    BLDTABLE,BLDTABLE   COLLAPSE TABLE                               
         XC    FIRSTSW,FIRSTSW                                                  
         B     BLD30180            GO BACK FOR NEXT EFF DATE ELT                
BLD30440 EQU   *                                                                
         CLI   ZEROSPTS,0          ZERO SPOTS/WEEK?                             
         BNE   BLD30460            NO  -                                        
         CLI   MULTEFDT,2          YES - > 1 EFF DATE?                          
         BL    BLD30480            NO  - DON'T SEND COMMENTS                    
BLD30460 EQU   *                                                                
         CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
         BE    BLD30480            NO                                           
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
BLD30480 EQU   *                                                                
         OC    MGCOUNTR,MGCOUNTR   MAKEGOODS FOUND?                             
         BZ    BLD30490            NO                                           
         GOTO1 =A(BLD07REC),DMCB,(RC),RR=Y                                      
         XC    MGCOUNTR,MGCOUNTR   CLEAR COUNTER                                
BLD30490 EQU   *                                                                
         B     BLD30020            ACCESS NEXT BUY RECORD                       
*                                                                               
BLD30500 EQU   *                                                                
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
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELT                    
         BAS   RE,GETEL                                                         
         BNE   BCTC0005            NO PROGRAM NAME ELT FOUND                    
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
*   THIS ROUTINE BUILDS THE '0304' COMMENT RECORD                               
*                                                                               
*   NOTE:  THERE MUST BE COMMENTS, MULTIPLE EFFECTIVE DATES, OR                 
*        MG INDICATOR TO PRODUCE A 0304 RECORD                                  
*                                                                               
BLD04REC NTR1                                                                   
         MVC   SAVELCOD,ELCODE     SAVE CURRENT ELEMENT CODE                    
         LA    RF,SAVRC303         SET A(RECEIVING FIELD)                       
         LA    R1,RC303LEN         SET L(RECEIVING FIELD                        
         LA    RE,RC303REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
*                                  SAVE THE BUY RECORD                          
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
         MVI   RC304REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC304REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC304LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC304REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   RC304ID,=C'0304'                                                 
         MVC   RC304NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC304REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC304REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
         MVI   RC304SBP,C'1'       INSERT FIXED VALUE                           
         EDIT  SVBYLN#,(3,RC304LIN),FILL=0,ZERO=NOBLANK                         
         MVC   RC304SLN,SVSL#      INSERT LINE/SUB LINE #S                      
*                                                                               
         TM    COMFLAGS,X'40'      COMMENTS EXIST?                              
         BNO   BLD40340            NO                                           
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELT                    
BLD40140 EQU   *                                                                
         MVC   RC304TXT,SPACES     SPACE OUT COMMENT AREA                       
BLD40160 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLD40200                                                         
BLD40180 EQU   *                                                                
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
         EX    RF,BLD4023A         MOVE 1ST COMMENT (ORDER)                     
         B     BLD40235            PUT OUT COMMENT RECORD                       
BLD40210 EQU   *                                                                
         EX    RF,BLD4023C         MOVE 1ST COMMENT (BUY)                       
         B     BLD40235            PUT OUT COMMENT RECORD                       
BLD40235 EQU   *                                                                
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
         MVC   RC304TXT,SPACES     SPACE OUT COMMENT AREA                       
*                                                                               
         B     BLD40180            RETURN FOR NEXT COMMENT                      
*                                  PUT OUT BOTH ORDER AND CONTRACT              
*                                     COMMENTS:  DECIDE LATER WHAT'S            
*                                        NEEDED BY COLUMBINE                    
*                                                                               
BLD4023A MVC   RC304TXT(0),3(R6)                                                
BLD4023C MVC   RC304TXT(0),2(R6)                                                
*                                                                               
BLD40260 EQU   *                                                                
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40340            YES - COMMENTS FINISHED                      
         CLI   ELCODE,X'21'        PROGRAM NAME DONE?                           
         BNE   BLD40280            NO  - MUST BE ORDER COMMENTS                 
         LA    R6,RBUYREC          YES - RESET A(BUY RECORD)                    
         MVI   ELCODE,X'84'        GO BACK AND DO ORDER COMMENTS                
         B     BLD40160                                                         
BLD40280 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'04'        NO  - GO BACK AND DO THEM                    
         B     BLD40160                                                         
*                                                                               
BLD40340 EQU   *                                                                
         LA    RF,RC303REC         SET A(RECEIVING FIELD)                       
         LA    R1,RC303LEN         SET L(RECEIVING FIELD                        
         LA    RE,SAVRC303         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
*                                  RESTORE THE BUY RECORD                       
         XC    COMFLAGS,COMFLAGS   TURN OFF COMMENTS FLAG                       
         MVC   ELCODE,SAVELCOD     RESTORE CURRENT ELEMENT CODE                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE '0310' TRANSACTION RECORD                           
*                                                                               
*                                                                               
BLD10REC NTR1                                                                   
         MVI   RC310REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC310REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC310LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC310REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   RC310ID,=C'0310'    INSERT ID                                    
         MVC   RC310NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC310REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC310REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
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
         MVC   RUNREP,=C'BLAR'     SET TO 'BLAIR'                               
         CLC   RCONKREP,=C'BL'     BLAIR?                                       
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'FXFX'     SET TO 'FOX'                                 
         CLC   RCONKREP,=C'FB'     FOX?                                         
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'KATZ'     SET TO 'KATZ '                               
         CLC   RCONKREP,=C'AM'     AMERICAN?                                    
         BE    GCON0005            YES                                          
         CLC   RCONKREP,=C'CQ'     CONTINENTAL?                                 
         BE    GCON0005            YES                                          
         CLC   RCONKREP,=C'NK'     NATIONAL?                                    
         BE    GCON0005            YES                                          
         CLC   RCONKREP,=C'CR'     CHRISTAL?                                    
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'PTRY'     SET TO 'PTRY '                               
         CLC   RCONKREP,=C'PV'     PETRY?                                       
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'TELE'     SET TO 'TELE '                               
         CLC   RCONKREP,=C'B1'     TELEMUNDO?                                   
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'SELT'     SET TO 'SELT '                               
         CLC   RCONKREP,=C'SZ'     SELTEL?                                      
         BE    GCON0005            YES                                          
         MVC   RUNREP,=C'TEST'     SET TO 'TEST '                               
*                                                                               
*   WILL SET TO 'TEST' IF UNRECOGNIZED!!!                                       
*                                                                               
GCON0005 EQU   *                                                                
*                                                                               
*   RETRIEVE SUPPORT INFORMATION FROM VARIOUS RECORDS                           
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
*                                                                               
*                                                                               
READEOP  NTR1                                                                   
** DON'T USE 'HALF' IN THIS ROUTINE                                             
         XC    EOPRECS,EOPRECS     CLEAR EOPRECS                                
         LA    R2,EOPRECS          A(EOPRECS)                                   
         MVI   ION,3                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'1B'           GET ADVERTISER                               
*                                                                               
*   CHECK ADVERTISER FOR TRADE ORDER                                            
*                                                                               
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'1E'        LOAD FOR RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   REOP0010            NO RANDOM FLAG ELEMENT                       
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   REOP0010            NO                                           
         DROP  R6                                                               
         MVI   KEY+1,1             SET ALTERNATE 1B REC KEY                     
REOP0010 EQU   *                                                                
         MVC   KEY+15(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+17,4            SET TYPE = COLUMBINE                         
         MVC   KEY+18(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+23(4),RCONKADV  INSERT ADVERTISER                            
         GOTO1 EOPREAD,DMCB,(R2),5 READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
*                                                                               
*    TRADE AGENCY CODING                                                        
*                                                                               
         XC    HALF2,HALF2         USE HALF2 FOR TRADE ALTERNATE OFFICE         
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'1E'        LOAD FOR RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   REOP0020            NO RANDOM FLAG ELEMENT                       
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   REOP0020            NO                                           
         DROP  R6                                                               
*                                                                               
*   YES - FIND TRADE ALTERNATE KEY                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'1A'           YES - BUILD AGENCY2 KEY                      
         MVC   KEY+19(6),RCONKAGY  INSERT AGENCY/OFFICE                         
         MVC   KEY+25(2),REPALPHA  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 AGENCY KEY MUST BE ON FILE                   
         DC    H'0'                                                             
         MVC   DMCB(4),AIO3        A(IO AREA)                                   
         GOTO1 VGETREC,DMCB                                                     
         L     R4,AIO3                                                          
         USING RAGY2REC,R4                                                      
         LA    R6,RAGY2REC         A(AGENCY2  RECORD)                           
         MVI   ELCODE,X'1F'        AGENCY ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   REOP0020            NO AGENCY ELEMENT                            
         USING RAG2ELEM,R6                                                      
         CLI   RAG2TRAD,X'40'      ANY VALUE IN FIELD?                          
         BNH   REOP0020            NO                                           
         MVI   HALF2,C'#'          YES - LOAD UP ALTERNATE OFFICE               
         MVC   HALF2+1(1),RAG2TRAD                                              
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
REOP0020 EQU   *                                                                
*                                                                               
*    TRADE AGENCY CODING                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'1C'           GET AGENCY                                   
         MVC   KEY+13(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+15,4            SET TYPE = COLUMBINE                         
         MVC   KEY+16(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+21(6),RCONKAGY  INSERT AGENCY/OFFICE                         
*                                                                               
*   TRADE AGENCY CODING (2)                                                     
         OC    HALF2,HALF2         ANY TRADE ALTERNATE OFFICE?                  
         BZ    REOP0040            NO                                           
         MVC   KEY+25(2),HALF2                                                  
REOP0040 EQU   *                                                                
*                                                                               
*   TRADE AGENCY CODING (2)                                                     
*                                                                               
         GOTO1 EOPREAD,DMCB,(R2),5 READ/ADD CODE TO TABLE                       
*                                                                               
*   COLUMBINE DOESN'T USE OFFICE EQUIV:  FORCE TBL TO NON-ZERO.                 
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         MVI   0(R2),X'FF'                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1D'           GET OFFICE                                   
         MVC   KEY+17(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+19,4            SET TYPE = COLUMBINE                         
         MVC   KEY+20(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+25(4),RCONKOFF  INSERT OFFICE                                
         GOTO1 EOPREAD,DMCB,(R2),3 READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1E'           GET SALESPERSON                              
         MVC   KEY+16(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+18,4            SET TYPE = COLUMBINE                         
         MVC   KEY+19(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+24(4),RCONSAL   INSERT SALESPERSON                           
         GOTO1 EOPREAD,DMCB,(R2),6 READ/ADD CODE TO TABLE                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    ROUTINE ACCESSES KEY/RECORD.  IF NOT FOUND, ENTRY IN TABLE                 
*        IS LEFT BLANK.  IF FOUND, EQUIV CODE IS MOVED TO TABLE.                
*        MOVE IS DONE BY LENGTH OF CODE.                                        
*                                                                               
EOPREAD  NTR1                                                                   
** DON'T USE 'HALF' IN THIS ROUTINE                                             
         L     R2,0(R1)            RELOAD A(TABLE ENTRY)                        
         L     R5,4(R1)            LOAD L(CODE) TO MOVE                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   EOPR0200            NO  - EXIT                                   
         MVC   DMCB(4),AIO3        A(IO AREA)                                   
         MVI   UPDATE,C'Y'         READ FOR UPDATE                              
         GOTO1 VGETREC,DMCB                                                     
         L     R4,AIO3                                                          
         USING REOPREC,R4                                                       
         BCTR  R5,0                SUBTRACT 1 FOR EX                            
         EX    R5,EOPR0400         MOVE CODE BY LENGTH                          
         TM    REOPFLAG,X'80'      ALREADY ACTIVE?                              
         BO    EOPR0200            YES - DON'T REWRITE                          
         OI    REOPFLAG,X'80'      NO  - SET ACTIVE FLAG                        
         GOTO1 VPUTREC,DMCB,REOPREC  REWRITE FROM SAME AREA                     
EOPR0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
EOPR0400 MVC   0(0,R2),REOPEQUV    MOVE CODE TO TABLE                           
         DROP  R4                                                               
*        EJECT                                                                  
*                                                                               
*                                                                               
CROSSDAY NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
READBUY  NTR1                                                                   
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
*                                                                               
*                                                                               
BLD3PTRN NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE DAY AND TIME TO COLUMBINE FORMAT.                                 
*                                                                               
DYTIME   NTR1                                                                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
*                                                                               
*   TEST DUMP                                                                   
**       CLI   RBUYKLIN,4                                                       
**       BNE   TEST0020                                                         
**       MVC   DIE0020,=X'0000'        FORCE DUMP                               
TEST0020 EQU   *                                                                
*   TEST DUMP END                                                               
*                                                                               
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   DYTI0240            NOT FOUND - EXIT                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         CLC   RC303GRP,SPACES     ANY TD/TC GROUP ENTERED?                     
         BH    DYTI0020            YES                                          
         CLC   RC303STO(10),SPACES ANY TD/TC BDCST START TIME ORIDE?            
         BH    DYTI0020            YES                                          
DIE0020  EQU   *                                                                
         OC    RBUYDYT1,RBUYDYT1   ANY START TIME?                              
         BZ    DYTI0010            NO                                           
         GOTO1 TIMEADJ,DMCB,RBUYDYT1,RC303STO                                   
DYTI0010 EQU   *                                                                
         OC    RBUYDYT2,RBUYDYT2   ANY END   TIME?                              
         BZ    DYTI0020            NO                                           
DYTI0015 EQU   *                                                                
         GOTO1 TIMEADJ,DMCB,RBUYDYT2,RC303ETO                                   
*                                  NO  - LOAD ORDERED AS BDCST STRT/END         
DYTI0020 EQU   *                                                                
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    DYTI0040            NO  - ONLY 1                                 
****>>>> MVC   RC303DAY(2),=C'*P'  2 OR MORE: INDICATE PATTERN                  
         GOTO1 CALNDRFL,DMCB,(RC)                                               
         MVC   RC303PAT,SPACES     CLEAR X'S IN ROTATOR FIELD                   
         B     DYTI0080                                                         
DYTI0040 EQU   *                                                                
*                                        OTHERWISE NOT USED                     
*                                                                               
         MVC   HEXSTTIM(12),FOXZEROS CLEAR START/END TIMES                      
         EDIT  RBUYDYT1,(4,HEXSTTIM),FILL=0                                     
         CLC   RBUYDYT2,=C'CC'     'TO CONCLUSION?'                             
         BNE   DYTI0050            NO                                           
         MVC   HEXENTIM,=C'9999'   YES                                          
         B     DYTI0060                                                         
DYTI0050 EQU   *                                                                
         EDIT  RBUYDYT2,(4,HEXENTIM),FILL=0                                     
DYTI0060 EQU   *                                                                
         MVC   RC303OST,HEXSTTIM   INSERT ORDERED START TIME                    
         MVC   RC303OET,HEXENTIM   INSERT ORDERED END   TIME                    
*                                                                               
         GOTO1 CALNDRFL,DMCB,(RC)                                               
DYTI0080 EQU   *                                                                
         MVC   RC303LNS,FOXZEROS   INITIALIZE LENGTH OF SPOT                    
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    DYTI0120            YES                                          
*                                                                               
*   IF 300 SECONDS OR LESS, USE SECONDS AS IS                                   
*                                                                               
         ZICM  RF,RBUYDUR,2        NO  - SECONDS                                
         CH    RF,=H'300'          > 300 SECONDS?                               
         BH    DYTI0100            YES  - SET '000', LOAD PROG LEN              
         EDIT  RBUYDUR,(3,RC303LNS),FILL=0,ZERO=NOBLANK                         
*                                  USE AS IS                                    
         B     DYTI0240                                                         
DYTI0100 EQU   *                                                                
*                                                                               
*   BUY IN SECONDS, > 300.  CONVERT SECONDS TO MINUTES, LOAD AS PRGRM           
*       LENGTH.  SET SPOT LENGTH TO 000.                                        
*                                                                               
         MVC   RC303LNS,FOXZEROS   INSERT ZEROS IN SPOT LENGTH                  
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT TO MINS                              
         ST    R0,FULL             SAVE REMAINDER IN FULL                       
*                                     REMAINDER IS USELESS IN THIS CASE         
         EDIT  (R1),(3,RC303PLN),FILL=0,ZERO=NOBLANK                            
*                                  INSERT MINS AS PROGRAM LENGTH                
         B     DYTI0240            EXIT ROUTINE                                 
DYTI0120 EQU   *                   BUY IS IN MINUTES                            
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
*                                     REMAIN AS 'MINUTES'                       
         CH    R1,=H'05'           5 MINUTES OR LESS?                           
         BH    DYTI0140            NO  - HIGHER: USE AS MINUTES                 
*                                     IN PROGRAM LENGTH FIELD                   
DYTI0135 EQU   *                                                                
*                                  YES - SHOW AS SECS                           
         M     R0,=F'60'           MULTIPLY BY 60 TO GET SECS                   
         EDIT  (R1),(3,RC303LNS),FILL=0,ZERO=NOBLANK                            
*                                  INSERT SECS AS SPOT LENGTH                   
         B     DYTI0240            EXIT ROUTINE                                 
DYTI0140 EQU   *                                                                
*                                                                               
         MVC   RC303LNS,FOXZEROS                                                
         EDIT  (R1),(3,RC303PLN),FILL=0,ZERO=NOBLANK                            
*                                  INSERT MINUTES AS PRGRM LEN                  
DYTI0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*    GET CALENDAR DAYS FROM BITS 8-15 OF 02 ELEMENT                             
*                                                                               
CALNDRFL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   RC303PAT,SPACES     CLEAR ROTATOR FIELDS                         
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     CALN0040                                                         
CALN0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
CALN0040 EQU   *                                                                
         BNE   CALN0100            NOT FOUND - EXIT                             
         LA    RF,RC303PAT         A(CALENDAR)                                  
         ZIC   RE,3(R6)            GET DAYS BYTE IN REG                         
         SLL   RE,25               SHIFT DAYS BYTE TO HI-ORDER                  
*                                     DROP 'SPARE' BIT                          
         LA    R0,7                LOOP CONTROL                                 
CALN0060 EQU   *                                                                
         LTR   RE,RE               SET CONDITION CODES FOR REG                  
         BNM   CALN0080            NOT MINUS = BIT NOT SET                      
         MVC   0(2,RF),=C'XX'      MINUS = BIT SET: SET CALENDAR                
         L     R1,DAYCNT           BUMP DAY COUNTER                             
         LA    R1,1(R1)                                                         
         ST    R1,DAYCNT           STORE IT BACK                                
CALN0080 EQU   *                                                                
         LA    RF,2(RF)            BUMP TO NEXT CALENDAR POSITION               
         SLL   RE,1                SHIFT BITS UP 1                              
         BCT   R0,CALN0060         TEST NEXT BIT                                
         B     CALN0020            LOOK FOR NEXT ELEMENT                        
CALN0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TIMEADJ  NTR1                                                                   
         L     R2,0(R1)            SET A(INPUT)                                 
         L     R3,4(R1)            SET A(OUTPUT)                                
         ZICM  R6,0(R2),2          GET TIME VALUE                               
         C     R6,=F'1200'         TIME AFTER NOON?                             
         BH    TADJ0100            YES                                          
         EDIT  (R6),(4,0(R3)),FILL=0                                            
         CLC   =C'00',0(R3)        1201-1259 CHECK                              
         BNE   TADJ0020            NO                                           
         MVC   0(2,R3),=C'12'      YES                                          
TADJ0020 EQU   *                                                                
         MVI   4(R3),C'A'          SET TIME TO AM                               
         CLC   =C'1200',0(R3)      NOON?                                        
         BNE   TADJ0800            NO  - EXIT                                   
         MVI   4(R3),C'P'                                                       
         B     TADJ0800            EXIT                                         
TADJ0100 EQU   *                                                                
         S     R6,=F'1200'         AFTER NOON:                                  
         EDIT  (R6),(4,0(R3)),FILL=0                                            
         MVI   4(R3),C'P'          SET TIME TO PM                               
         CLC   =C'00',0(R3)        1201-1259 CHECK                              
         BNE   TADJ0120            NO                                           
         MVC   0(2,R3),=C'12'      YES                                          
TADJ0120 EQU   *                                                                
         CLC   =C'1200',0(R3)      MIDNIGHT?                                    
         BNE   TADJ0800            NO  - EXIT                                   
         MVI   4(R3),C'A'                                                       
         B     TADJ0800            EXIT                                         
TADJ0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    FILL PRIORITY                                                              
*                                                                               
**PRTY     NTR1                                                                 
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
**         XIT1                                                                 
         EJECT                                                                  
*                                                                               
*    FILL PROGRAM CODE                                                          
*                                                                               
**PCDISP   NTR1                                                                 
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
**         XIT1                                                                 
         EJECT                                                                  
*                                                                               
*    FILL SECT/PLAN                                                             
*                                                                               
**PLNDISP  NTR1                                                                 
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
**         XIT1                                                                 
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
         SR    RE,RE                                                            
         ZIC   RF,NOFWKS           CALCULATE TOTAL SPOTS                        
         ZIC   R1,SPOTSWK                                                       
         MR    RE,R1               # WEEKS X SPOTS/WK = TOTAL SPOTS             
         L     RE,TLSPOTS                                                       
         AR    RE,RF                                                            
         ST    RE,TLSPOTS          SAVE TOTAL SPOTS                             
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         XC    FULL,FULL           SET COST TO ZERO                             
         CLC   RBUYKPLN,=X'FFFFFF' PLAN BUY?                                    
         BNE   DINF0010            YES - DON'T ACCUM VALUE FOR LINE             
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
DINF0010 EQU   *                                                                
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         XC    COLLAPSE,COLLAPSE   CLEAR COLLAPSE FLAG                          
         GOTO1 CHKDATE,DMCB,(RC),(R6)                                           
         MVI   NOFWKS,0            CLEAR NUMBER OF WEEKS                        
*                                                                               
*   SHIFT 'USING' FROM EFFECTIVE DATE ELEMENT TO BLDTABLE AREA,                 
*      WHERE A 'REVISED/COLLAPSED' EFFECTIVE DATE ELEMENT MAY HAVE              
*      BEEN BUILT.  IF IT HASN'T, IT STILL LOOKS LIKE THE ONE PASSED            
*      IN AT THE ORIGINAL CALL.                                                 
*                                                                               
         LA    R6,BLDTABLE                                                      
*                                                                               
*   TEST ALTERNATING WEEK PROBLEM                                               
**       CLI   RBUYKLIN,X'01'      BUYLINE # 1?                                 
**       BNE   TEST0010            NO                                           
**       DC    H'0'                                                             
TEST0010 EQU   *                                                                
*   TEST ALT WK PROB END                                                        
*                                                                               
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
         MVC   RC303STD(14),FOXZEROS                                            
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
         MVC   RC303STD+3(4),HOLDDATE+2     INSERT MM/DD                        
         MVC   RC303STD+1(2),HOLDDATE       INSERT YY                           
*                                                                               
         MVI   RC303STD,C'0'       SET TO 2OTH CENTURY                          
         CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
         BH    DINF0180            YES                                          
         MVI   RC303STD,C'1'       NO  - 21ST CENTURY                           
DINF0180 EQU   *                                                                
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',HOLDDATE)                          
         MVC   RC303END+3(4),HOLDDATE+2     INSERT MM/DD                        
         MVC   RC303END+1(2),HOLDDATE       INSERT YY                           
*                                                                               
         MVI   RC303END,C'0'       SET TO 2OTH CENTURY                          
         CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
         BH    DINF0200            YES                                          
         MVI   RC303END,C'1'       NO  - 21ST CENTURY                           
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
         XC    FULL,FULL           SET COST TO ZERO                             
         CLC   RBUYKPLN,=X'FFFFFF' PLAN BUY?                                    
         BNE   CHKD0110            YES - DON'T ACCUM VALUE FOR LINE             
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
CHKD0110 EQU   *                                                                
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
*                                                                               
*   GET SPOT INFO:  # OF SPOTS PER WEEK, PRICE PER SPOT, ETC.                   
*                                                                               
SPTINFO  NTR1                                                                   
*                                                                               
*   WHEN SINGLE DAY/TIME ELT IS A FIXED DAY (IE, MON/10P),                      
*        CLEAR THE X'S IN THE CALENDAR, AND LOAD THE SPOTS/WK.                  
*                                                                               
         CLI   DTSTRNGS,2          > 1 D/T STRING?                              
         BNL   SPTI0400            YES - LEAVE AS IS                            
         LA    R6,RBUYREC          NO  - LOOK FOR D/T STRING ELEMENT            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         ZIC   R0,2(R6)            GET START/END DAY                            
         SRDL  R0,4                SHIFT END DAY OUT OF R0                      
         SRL   R1,28               MOVE TO LOW ORDER                            
         CR    R0,R1               SINGLE DAY?                                  
         BNE   SPTI0400            NO                                           
*                                                                               
*    FOR FIXED DAY BUY, CLEAR X'S, MOVE IN # SPOTS - EMPTY DAYS                 
*        RECEIVE 'SPACES' (X'40')                                               
*                                                                               
         MVC   RC303PAT,SPACES                                                  
         LA    R4,RC303PAT         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTSWK,(1,0(R4))                                                
*                                  INSERT # SPTS/WK INTO CALENDAR               
SPTI0400 EQU   *                                                                
         EDIT  SPOTSWK,(2,RC303SPT),FILL=0,ZERO=NOBLANK                         
*                                  INSERT NUMBER SPOTS PER WEEK                 
         EDIT  RBUYCOS,(11,RC303RTE),FILL=0,ZERO=NOBLANK                        
*                                  INSERT PRICE PER SPOT                        
         CLC   RBUYKPLN,=X'FFFFFF' PLAN BUY?                                    
         BE    SPTI0600            NO                                           
         LA    RF,PLANTABL         YES - SET A(PLAN/COST TABLE)                 
SPTI0420 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE REACHED?                        
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLC   0(3,RF),RBUYKPLN    TABLE = BUY PLAN?                            
         BE    SPTI0440            YES                                          
         LA    RF,7(RF)            NO  - BUMP TO NEXT SLOT                      
         B     SPTI0420            GO BACK FOR NEXT                             
SPTI0440 EQU   *                                                                
         LA    R4,3(RF)            SET A(PLAN COST)                             
*                                                                               
         EDIT  (4,(R4)),(11,RC303PRA),FILL=0,ZERO=NOBLANK                       
*                                  INSERT PLAN RATE                             
         MVI   RC303PFR,C'W'       INSERT PLAN FREQUENCY                        
         MVC   RC303RTE,SPACES     CLEAR PRICE PER SPOT FIELD                   
         MVC   RC303PID,SPACES     SET PLAN ID TO SPACES                        
*                                                                               
*   RIGHT JUSTIFY ALL SINGLE CHARACTER PLAN CODES                               
*                                                                               
         CLC   RBUYKPLN+1(2),=X'4040' CHARS 2+3 SPACE/BIN ZERO?                 
         BH    SPTI0500            NO  - LOAD FIRST TWO CHARACTERS              
SPTI0460 EQU   *                                                                
         MVC   RC303PID+1(1),RBUYKPLN                                           
*                                  RIGHT JUSTIFY ALPHA VALUE                    
SPTI0480 EQU   *                                                                
         B     SPTI0600                                                         
SPTI0500 EQU   *                                                                
         MVC   RC303PID(2),RBUYKPLN      SET TWO-CHARACTER PLAN                 
SPTI0600 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROGRAM BUY AND PROGRAM CODE DISPLAY                                        
*        DDS HAS NO BIAS PROGRAM CODES TRAFFIC ELEMENT                          
*                                                                               
PBDISP   NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STATION CODE TRAFFIC ELEMENT DISPLAY                                        
*        DDS HAS NO BIAS STATION CODES TRAFFIC ELEMENT                          
*                                                                               
STACDISP NTR1                                                                   
         XIT1                                                                   
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
         DC    V(REGENPBY)                                                      
         DC    V(REGENBUC)                                                      
         DC    V(REGENTL2)                                                      
         DC    V(UNTIME)                                                        
NADCONS  EQU   (*-ADCONS)/4                                                     
         SPACE 2                                                                
DASH     DC    51C'-'                                                           
         SPACE 2                                                                
**RMSCMODE DC    X'00'               REGENSC MODES                              
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
LOCALWRK DSECT                                                                  
       ++INCLUDE RECNTCOLA                                                      
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
*                                  BIT 0  =  MG SWITCH                          
*                                  BIT 1  =  BUY COMMENTS EXIST                 
FIRSTSW  DS    XL1                                                              
TRANSCT  DS    XL1                 TRANSACTION COUNTER                          
ALTWK    DS    XL1                 ALTWEEK COLLAPSE INDICATOR                   
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
TESTCTR  DS    F                                                                
*                                     ID/REP/CON#/BUYLINE #                     
MGCOUNTR DS    F                   MAKEGOOD MISSED SPOT COUNTER                 
AMGTABLE DS    A                   A(MG TABLE)                                  
ANXTMGTB DS    A                   A(NEXT SPOT IN MG TABLE)                     
MGSPOTS  DS    F                   MISSED SPOT COUNT                            
LASTMGMS DS    X                   LAST MASTER LINE #                           
FIRSTMG  DS    X                   FIRST MAKEGOOD AGAINST MASTER                
FIRSTMGD DS    XL8                 DATE OF FIRST MISSED SPOT YYYYMMDD           
SAV303ID DS    CL21                FIRST 21 BYTES OF 303 REC                    
*                                     ID/REP/CON#/BUYLINE #                     
BLDTABLE DS    XL24                (???? CHECK LENGTH)                          
SVSL#    DS    XL1                 SAVE SUBLINE #                               
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
         DS    0F                                                               
BLOCK    DS    480C                DEMO WORK AREA                               
*                                                                               
NEW15ELT DS    CL20                AREA FOR DATE/TIME ELT                       
SAVRC303 DS    330C                SAVE AREA FOR BUY RECORD                     
SAVELCOD DS    CL1                 SAVE AREA FOR CURRENT ELEMENT                
HEXSTTIM DS    CL6                 START TIME FOR BUY LINE                      
HEXENTIM DS    CL6                 END   TIME FOR BUY LINE                      
OLD03ELT DS    CL11                OLD X'03' ELEMENT                            
NEW03ELT DS    CL11                EXPLODED 03 ELT                              
DAYDIFF  DS    F                   DAY DIFFERENCE FOR NEW 03 ELT                
SAVEST#  DS    CL10                SAVE AREA FOR ESTIMATE NUMBER                
PLANTABL DS    CL70                PLAN/COST TABLE:                             
*                                  10 SETS OF PLAN/COST:                        
*                                  POS 1-3=  PLAN ID (COL = 3 BYTE)             
*                                  POS 4-7=  COST OF PLAN                       
PLANTABD DS    CL1                                                              
*                                  PLANTABL+1 = END OF TABLE: X'FF'             
STAOPTS  DS    XL1                                                              
RUNREP   DS    CL4                 REP OF RUN                                   
LOCALEND EQU   *                                                                
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
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
         CSECT                                                                  
*********************************************************************           
*        TOTLBUYS  ---  SCAN BUYS TO DETERMINE TOTAL SPOTS, $$$*                
*********************************************************************           
TOTLBUYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PLANTABD,X'FF'      SET END OF TABLE DELIMITER                   
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
TOBU0060 EQU   *                                                                
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         GOTO1 =A(CHEKPLAN),RR=Y   CHECK FOR PLAN BUY                           
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    TOBU0020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       XCLED BUYLINE?                               
         BE    TOBU0020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     XCLED BUYLINE?                               
         BE    TOBU0020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    TOBU0020            YES - SKIP IT                                
         LA    R4,RBUYELEM         CYCLE THRU DATE ELEMENTS                     
TOBU0080 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    TOBU0020            YES - GO BACK FOR NEXT BUY REC               
         CLI   0(R4),3             DATE ELEMENT?                                
         BNE   TOBU0120            YES                                          
TOBU0100 EQU   *                                                                
         USING RBUYDTEL,R4                                                      
         CLC   RBUYDTST,ERLYSTRT   START DATE < EARLIEST?                       
         BNL   TOBU0110            NO                                           
         MVC   ERLYSTRT,RBUYDTST   YES - REPLACE                                
TOBU0110 EQU   *                                                                
         CLC   RBUYDTED,LATREND    END   DATE > LAST DATE?                      
         BNH   TOBU0120            NO                                           
         MVC   LATREND,RBUYDTED    YES - REPLACE                                
TOBU0120 EQU   *                                                                
         ZIC   R5,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R5                                                            
         B     TOBU0080            GO BACK FOR NEXT                             
TOBU0900 EQU   *                                                                
*                                                                               
*   FOLLOWING CODE IS TEST CODE TO DISPLAY RESULTS WITHIN PRINTOUT              
*                                                                               
****>    MVC   P+1(31),=C'EARLIEST :            LATEST  :'                      
****>    GOTO1 DATCON,DMCB,(3,ERLYSTRT),(0,P+12)                                
****>    GOTO1 DATCON,DMCB,(3,LATREND),(0,P+33)                                 
****>    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*   END OF TEST CODE                                                            
*                                                                               
         DROP  R4                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   CHEKPLAN:  IF PLAN BUY, INSERT ONE CHARACTER PLAN CODE AND COST             
*        INTO TABLE.  IF PLAN NOT IN TABLE, INSERT.  IF PLAN IN                 
*        TABLE, REPLACE COST.  THIS WILL RESULT IN LAST PLAN PRICE              
*        BEING LEFT IN TABLE.  THERE IS ROOM FOR **10** PLANS IN                
*        TABLE.  IF END REACHED, JOB WILL ABORT, TABLE MUST BE                  
*        EXPANDED.                                                              
*        NOTE:  COLUMBINE PLAN CODES ARE LIMITED TO ONE CHARACTER.              
*                                                                               
CHEKPLAN NTR1  BASE=*,LABEL=*                                                   
         CLC   RBUYKPLN,=X'FFFFFF' PLAN BUY?                                    
         BE    CPLA0800            NO  - EXIT                                   
         LA    RF,PLANTABL         SET A(PLAN TABLE)                            
CPLA0020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE REACHED?                        
         BNE   CPLA0040            NO                                           
         DC    H'0'                TABLE MUST BE EXPANDED.                      
CPLA0040 EQU   *                                                                
         CLI   0(RF),X'00'         SLOT IN TABLE EMPTY?                         
         BE    CPLA0060            YES - FILL IT UP                             
         CLC   0(3,RF),RBUYKPLN    NO  - SLOT IN TABLE SAME AS PLAN?            
         BE    CPLA0060            YES - FILL IT UP                             
         LA    RF,7(RF)            NO  - BUMP TO NEXT TABLE SLOT                
         B     CPLA0020            GO BACK FOR NEXT SLOT                        
CPLA0060 EQU   *                                                                
         MVC   0(3,RF),RBUYKPLN    (RE)INSERT PLAN ID INTO TABLE                
         MVC   3(4,RF),RBUYCOS     INSERT PLAN PRICE INTO TABLE                 
CPLA0800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRINTREC NTR1  BASE=*,LABEL=*                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         CLC   =C'0303',RC303ID    BUY RECORD?                                  
         BNE   PREC0010            NO                                           
         OC    MGCOUNTR,MGCOUNTR   0303 = MAKEGOOD RECORD?                      
         BZ    PREC0010            NO                                           
         MVC   SAV303ID,RC303ID    YES - SAVE FIRST 21 BYTES                    
*                                                                               
*   TEST DUMP                                                                   
***      MVC   PREC0060(2),=X'0000'                                             
*   TEST DUMP END                                                               
*                                                                               
PREC0010 EQU   *                                                                
         CLC   =C'0310',RC310ID    FINAL BUFFER RECORD?                         
         BNE   PRC20020            NO                                           
         EDIT  TRANSCNT,(4,RC310TRC),FILL=0,ZERO=NOBLANK                        
*                                  INSERT RECORD COUNT                          
         EDIT  TOTALAMT,(13,RC310TDL),FILL=0,ZERO=NOBLANK                       
         EDIT  TLSPOTS,(6,RC310TOT),FILL=0,ZERO=NOBLANK                         
PRC20020 EQU   *                                                                
         LA    R2,3                LOOP CONTROL FOR PRINTING                    
         LA    R4,RC301REC         A(OUTPUT RECORD)                             
PRC20040 EQU   *                                                                
         MVC   P(110),0(R4)                                                     
         MVC   P+110(03),=C'DDS'   INSERT 'DDS' AT END OF LINE                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,110(R4)          BUMP TO SECOND HALF OF RECORD                
         BCT   R2,PRC20040         DO SECOND HALF OF RECORD                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*   SETTDTCS - SET FIELDS WITH CONTENTS OF TD/TC SCREEN             *           
*        THESE FIELDS ARE SET BEFORE THE RC303OST/OET FIELDS ARE    *           
*        SET.  THE DEFAULTS ARE SET IN THE DYTIME ROUTINE WHEN      *           
*        RC303OST/OET ARE SET                                       *           
*                                                                   *           
*********************************************************************           
SETTDTCS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'40'        GET TD/TC BUY DATA ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SETT0200            NOT FOUND                                    
         USING RBUYTDEL,R6                                                      
         CLC   RBUYTDGP,=X'FFFF'   ANY VALUE ENTERED? GROUP                     
         BE    SETT0010            NO                                           
         EDIT  RBUYTDGP,(3,RC303GRP),FILL=0                                     
SETT0010 EQU   *                                                                
         CLC   RBUYTDTP,=X'FF'     ANY VALUE ENTERED? TYPE                      
         BE    SETT0020            NO                                           
         EDIT  RBUYTDTP,(1,RC303TYP),FILL=0                                     
SETT0020 EQU   *                                                                
         CLC   RBUYTDPS,=X'FFFF'   ANY VALUE ENTERED? PROD SEP                  
         BE    SETT0030            NO                                           
         EDIT  RBUYTDPS,(3,RC303SEP),FILL=0                                     
SETT0030 EQU   *                                                                
         CLC   RBUYTDAS,=X'FFFF'   ANY VALUE ENTERED? ADV  SEP                  
         BE    SETT0040            NO                                           
         EDIT  RBUYTDAS,(3,RC303ASP),FILL=0                                     
SETT0040 EQU   *                                                                
         CLC   RBUYTDS#,=X'FF'     ANY VALUE ENTERED? SECTION #                 
         BE    SETT0050            NO                                           
         EDIT  RBUYTDS#,(1,RC303SEC),FILL=0                                     
SETT0050 EQU   *                                                                
         CLC   RBUYTDCL,=X'FFFF'   ANY VALUE ENTERED? CLASS                     
         BE    SETT0060            NO                                           
         MVC   RC303CLS,RBUYTDCL                                                
SETT0060 EQU   *                                                                
         CLC   RBUYTDST,=X'FFFF'   ANY VALUE ENTERED? START TIME OVER           
         BE    SETT0070            NO                                           
         GOTO1 TIMEADJ,DMCB,RBUYTDST,RC303STO                                   
SETT0070 EQU   *                                                                
         CLC   RBUYTDET,=X'FFFF'   ANY VALUE ENTERED? END TIME OVER             
         BE    SETT0080            NO                                           
         GOTO1 TIMEADJ,DMCB,RBUYTDET,RC303ETO                                   
SETT0080 EQU   *                                                                
         CLC   RBUYTDPU,=X'FF'     ANY VALUE ENTERED? PLOT UNIT                 
         BE    SETT0090            NO                                           
         EDIT  RBUYTDPU,(2,RC303PUN),FILL=0                                     
SETT0090 EQU   *                                                                
         DROP  R6                                                               
SETT0200 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'41'        GET TD/TC M/G DATA ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SETT0500            NOT FOUND                                    
         USING RBUYTCEL,R6                                                      
         CLC   RBUYTCDT,=X'FFFFFF' ANY DATE ENTERED?                            
         BE    SETT0210            NO                                           
         GOTO1 DATCON,DMCB,(3,RBUYTCDT),(X'20',CNVDATE)                         
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC303MGF+3(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC303MGF+1(2),CNVDATE INSERT YY                                  
*                                                                               
*   IF YEAR IS > 70, DATE IS (MOST LIKELY) IN THE 20TH CENTURY.                 
*        LESS THAN THAT INDICATES 21ST CENTURY.  BEYOND THAT,                   
*        I DON'T THINK THE PROGRAM WILL BE AROUND THAT LONG.                    
*                                                                               
         MVI   RC303MGF,C'0'       SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    SETT0210            YES                                          
         MVI   RC303MGF,C'1'       NO  - 21ST CENTURY                           
SETT0210 EQU   *                                                                
         CLC   RBUYTCC#,=X'FFFFFFFF'                                            
*                                  ANY VALUE ENTERED? CSI CON#                  
         BE    SETT0220            NO                                           
         EDIT  RBUYTCC#,(5,RC303CSI),FILL=0                                     
SETT0220 EQU   *                                                                
         CLC   RBUYTCCL,=X'FFFF'   ANY VALUE ENTERED? CSI LINE #                
         BE    SETT0230            NO                                           
         MVC   RC303MGC,RBUYTCCL                                                
SETT0230 EQU   *                                                                
         CLC   RBUYTCML,=X'FFFF'   ANY VALUE ENTERED? MASTER CSI LINE #         
         BE    SETT0240            NO                                           
         MVC   RC303MCS,RBUYTCML                                                
SETT0240 EQU   *                                                                
         CLC   RBUYTCMS,=X'FF'     ANY VALUE ENTERED? MASTER CSI SUBLN#         
         BE    SETT0250            NO                                           
         EDIT  RBUYTCMS,(2,RC303MCL),FILL=0                                     
SETT0250 EQU   *                                                                
         CLC   RBUYTCOP,=X'FF'     ANY VALUE ENTERED? OPEN FLAG                 
         BE    SETT0260            NO                                           
         MVC   RC303OFL,RBUYTCOP                                                
SETT0260 EQU   *                                                                
*                                                                               
SETT0500 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
*                                                                               
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*********************************************************************           
INIT     NTR1  BASE=*,LABEL=*                                                   
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
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
*   THIS ROUTINE BUILDS THE 307 RECORD FROM THE MAKEGOODS ENTERED               
*      BY THE CYCLEMKG ROUTINE                                                  
*                                                                               
*--------------------------------------------------------------------           
BLD07REC NMOD1 0,*BLD7*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   RC307REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC307REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC307LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC307REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   RC307REC(21),SAV303ID                                            
*                                  INSERT FIRST 21 BYTES FROM BUY REC           
         MVC   RC307REC(4),=C'0307'                                             
*                                  REPLACE RECORD ID                            
         L     R2,AMGTABLE         SET A(MAKEGOOD TABLE)                        
         SR    R5,R5               CLEAR COUNTER                                
BLD70020 EQU   *                                                                
         OC    0(5,R2),0(R2)       ANY ENTRY IN SLOT?                           
         BZ    BLD70040            NO  - FINISHED                               
         LA    R5,1(R5)            YES - INCREMENT COUNTER                      
         LA    R2,5(R2)            BUMP TO NEXT SLOT                            
         B     BLD70020            GO BACK FOR NEXT SLOT                        
BLD70040 EQU   *                                                                
         LTR   R5,R5               ANY MAKEGOOD ENTRIES?                        
         BZ    BLD70800            NO  - FINISHED                               
         MVI   RC307MCN,C'0'       SET 'ALL MISSED IN THIS RECORD'              
         C     R5,=F'17'           MORE THAN ONE RECORD REMAINING?              
         BNH   BLD70060            NO                                           
         MVI   RC307MCN,C'1'       YES - SET 'ANOTHER 107 FOLLOWS'              
BLD70060 EQU   *                                                                
         L     R2,AMGTABLE         SET A(MAKEGOOD TABLE) AGAIN                  
         LA    R6,17               SET MAX LOOP CONTROL                         
         LA    R4,RC307MMS                                                      
BLD70080 EQU   *                                                                
         OC    0(5,R2),0(R2)       ANY ENTRY IN SLOT?                           
         BNZ   BLD70100            NO  - END OF OUTPUT PHASE                    
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                  OUTPUT REC WITH < 17 ENTRIES                 
         B     BLD70800            FINISHED                                     
BLD70100 EQU   *                                                                
         EDIT  (1,3(R2)),(3,0(R4)),FILL=0                                       
         GOTO1 DATCON,DMCB,(3,0(R2)),(0,WORK+16)                                
         MVC   3(4,R4),WORK+18     INSERT MMDD INTO OUTPUT                      
         MVC   7(2,R4),WORK+16     INSERT YY INTO OUTPUT                        
*                                     OUTPUT FORMAT = MMDDYY                    
         EDIT  (1,4(R2)),(2,9(R4)),FILL=0                                       
         LA    R4,11(R4)           BUMP TO NEXT OUTPUT                          
         LA    R2,5(R2)            BUMP TO NEXT INPUT                           
*                                                                               
         BCT   R6,BLD70080         GO BACK FOR NEXT INPUT                       
*                                     RECORD MAX'D OUT!                         
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
         S     R5,=F'17'           SUBTRACT FROM TOTAL                          
         B     BLD70040            GO BACK FOR NEXT RECORD                      
*                                                                               
BLD70800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*- GETSTA -- READ STATION RECORD INTO IO3.                                      
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
         MVI   HALF,C'N'           SET TO OFF ORIGINALLY                        
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   GSTA55              NOT THERE                                    
*                                                                               
         USING RSTAXXEL,R6                                                      
         MVC   STAOPTS,RSTAOPTB    SAVE OPTIONS BYTE                            
         MVC   HALF(1),RSTAOPT9    USING HALF FOR TEMP STORAGE OF OPT           
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
****>>>  MVI   PLCLASS,C' '        TEMPORARY                                    
         MVI   PLCLASS,C'G'        CLASS G                                      
*                                  LIVE PER DAVID EISENBERG: 9/29/95            
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
*                                                                               
         LA    RF,RSTAREC                                                       
*                                                                               
         MVC   P+9(6),=C'EDICT='                                                
         TM    STAOPTS,X'10'       ENCODA OPTION FOR STATION?                   
         BNO   EDICT01             NO                                           
         MVC   P+15(7),=C'*ENCODA' YES                                          
         B     EDICT07                                                          
EDICT01  EQU   *                                                                
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
EDICT07  EQU   *                                                                
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
         DROP  R5,R6                                                            
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE CTGENFILE                                                      
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
         EJECT                                                                  
       ++INCLUDE REMERG03                                                       
       ++INCLUDE REMERG16                                                       
       ++INCLUDE RECYCLMG                                                       
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
         DROP  R6                                                               
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
*                                                                               
*   THIS ROUTINE BUILDS THE '0308' EDI RECORD                                   
*                                                                               
*                                                                               
BLD08REC NTR1  BASE=*,LABEL=*                                                   
         MVI   RC308REC,C' '       SPACE FILL THE RECORD                        
         LA    RF,RC308REC+1       SET A(RECEIVING FIELD)                       
         LA    R1,RC308LEN-1       SET L(RECEIVING FIELD                        
         LA    RE,RC308REC         SET A(SENDING FIELD)                         
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   RC308ID,=C'0308'    INSERT ID                                    
         MVC   RC308NRP(4),RUNREP  FILL SOURCE ID                               
         MVC   RC308REF(2),=C'00'                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,RC308REF+2,4,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER                       
         MVC   RC308EST,SAVEST#                                                 
*                                                                               
         LA    R1,RCONELEM                                                      
BLD80020 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    BLD80060            YES - FINISHED                               
         CLI   0(R1),X'A2'         EASI ELEMENT?                                
         BE    BLD80040            YES                                          
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     BLD80020            GO BACK FOR NEXT ELEMENT                     
BLD80040 EQU   *                                                                
         USING RCONIEL,R1                                                       
         MVC   RC308AAD,SPACES                                                  
         MVC   RC308AAD(4),RCONIADV                                             
         MVC   RC308AP1,SPACES                                                  
         MVC   RC308AP1(4),RCONIPRD                                             
         MVC   RC308AP2(4),RCONIPR2                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
BLD80060 EQU   *                                                                
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   FILL IN CONTRACT '15' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
DATETIME NTR1  BASE=*,LABEL=*                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
*                                                                               
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
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038RECNT76   10/02/03'                                      
         END                                                                    
