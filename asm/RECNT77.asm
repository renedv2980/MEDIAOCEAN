*          DATA SET RECNT77    AT LEVEL 018 AS OF 10/02/03                      
*PHASE T80277A,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
         TITLE 'T80277 - RECNT77 - BIAS EC CHANGES'                             
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT77 --- BIAS ELECTRONIC CONTRACT INTERFACE             *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG09/93 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* DEC01/93 (BU ) --- UPDATE EOP RECORD FLAG WHEN CODE ACTIVE        *           
*                                                                   *           
* FEB28/94 (BU ) --- ADJUST SINGLE-DAY DESCRIPTOR IN 0103 RECORD    *           
*                                                                   *           
* MAR03/94 (BU ) --- ALTERNATE WEEK:  DON'T END ON INACTIVE WEEK    *           
*                                                                   *           
* APR01/94 (BU ) --- TIME-STAMP:  ADJUST HOURS...                   *           
*                                                                   *           
* APR06/94 (BU ) --- FIX 'COLLAPSE' BUGS FOR TWO-WEEK OFF - ALSO    *           
*                    ADD 'CATEGORY' CODE TO OUTPUT                  *           
*                                                                   *           
* JUN06/94 (BU ) --- FIX ESTIMATE NUMBER FIELD FORMAT....           *           
*                                                                   *           
* AUG03/94 (BU ) --- FIX ALTERNATING WEEK ABORT PROBLEM             *           
*                                                                   *           
* APR12/95 (BU ) --- STATION KGW -- KGWT (4 CHARS)                  *           
*                                                                   *           
* MAY25/95 (BU ) --- INCREASE TRANSACTION COUNT ACCUMULATOR TO      *           
*                    HANDLE MORE THAN 255.                          *           
*                                                                   *           
* AUG29/95 (SKU) --- CHECK IF BUY SECTION NULLS AS WELL AS SPACES   *           
*                                                                   *           
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                             *           
*                                                                   *           
* DEC18/95 (SKU) --- DROP R4 REOPREC DSECT REGISTER AFTER USE       *           
*                    RE-READ CONTRACT RECORD VIA TWAKADDR INSTEAD   *           
*                    OF HIGH IN DATETIME TO GET CORRECT CONTRACT    *           
*                                                                   *           
* FEB22/96 (BU ) --- PASS 'SZ' (SELTEL) THROUGH AS 'HR'             *           
*                                                                   *           
* MAR28/96 (BU ) --- PASS 'PV' (PETRY) THROUGH AS 'PT'              *           
*                    DON'T EC IF EOP CODE(S) MISSING                *           
*                                                                   *           
* APR02/96 (BU ) --- GENERATE TYPE 6 (COMPETITIVE) RECORD IN O/P    *           
*                                                                   *           
* APR10/96 (WSB) --- DON'T CHECK EOP CODES IF STA OPT BYTE IS NO    *           
*                                                                   *           
* APR16/96 (BU ) --- FIX 'NO BUY, BUT COMMENTS SENT' SITUATION      *           
*                                                                   *           
* MAY21/96 (BU ) --- PASS 'AM/CQ/NK' (KATZ) THROUGH AS 'KZ'         *           
*                                                                   *           
* JUL26/96 (SKU) --- CHANGE TO USE THMS FOR TIME STAMPING           *           
*                                                                   *           
* APR04/97 (BU ) --- PASS 'FN' (FOXNW) THROUGH AS 'NW'              *           
*                    PASS 'JB' (FOXNW) THROUGH AS '04'              *           
*                    DON'T EC IF EOP CODE(S) MISSING                *           
*                                                                   *           
* APR10/97 (SKU) --- PASS 'JB' (FOXNW) THROUGH AS 'NW' INSTEAD      *           
*                                                                   *           
* APR07/98 (BU ) --- INSERT EDICT HEADER, CHANGE CLASS TO 'G'       *           
*                                                                   *           
* MAY20/98 (BU ) --- INSERT FINAL NON-SPACE CHAR ON 2ND 110CHAR LINE*           
*                                                                   *           
* MAY29/98 (BU ) --- PASS '46' (SELOC) THROUGH AS 'HR' INSTEAD      *           
*                                                                   *           
* JUN16/98 (BU ) --- PASS '41' (SELOC) THROUGH AS 'HR' INSTEAD      *           
*                                                                   *           
* MAR26/99 (BU ) --- EC CHANGE VERSION                              *           
*                                                                   *           
* JAN06/00 (BU ) --- MODIFY CREDIT APPLICATION                      *           
*                                                                   *           
* FEB07/00 (BU ) --- MODIFY MG 107 RECORD GENERATION                *           
*                                                                   *           
* AUG10/00 (BU ) --- SET LOCAL FLAG FROM X'1E' CONTRACT ELEMENT     *           
*                                                                   *           
* AUG14/00 (BU ) --- ADJUST COMPETITIVE STATION ARRAY               *           
*                                                                   *           
* SEP27/00 (BU ) --- UPDATE MAKEGOOD SWITCH FLAGS                   *           
*                                                                   *           
* NOV20/00 (BU ) --- TRADE/AGENCY PROCESSING                        *           
*                                                                   *           
* APR05/01 (BU ) --- TRADE FLAG SETTING IN EC                       *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80277   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T80277*,R9,RR=R8                              
         ST    R8,RELX                                                          
         LR    R3,RC               SET A(MODULE WORK SPACE)                     
         USING LOCALWRK,R3         SET DSECT FOR AREA                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
*****>   LA    R8,SPOOLAR                                                       
*        LA    R8,RBUYREC          SET A(SPOOLAR) RATHER INDIRECTLY             
*        A     R8,=F'3000'            3000 AFTER RBUYREC                        
         L     R8,ASPULAR                                                       
         ST    R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    RB,SAVERB                                                        
         ST    RD,SAVERD                                                        
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
* DON'T USE 'HALF' IN NEXT SUBROUTINE (USING TO STORE STATION OPTION)           
         GOTO1 =A(READEOP),DMCB,(RC),RR=Y                                       
*                                  READ EOP CODES                               
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
*                                                                               
         GOTO1 =A(EDICT),DMCB,(RC),(R8),RR=YES                                  
                                                                                
*                                  CREATE EDICT HEADER CARDS                    
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         '0101' HEADER                                
*                                                                               
MN300    EQU   *                                                                
         BAS   RE,BLD02REC         '0102' ORDER HEADER COMMENTS                 
*                                                                               
MN400    EQU   *                                                                
         BAS   RE,BLD03REC         '0103' LINE ADD                              
*                                                                               
MN500    EQU   *                                                                
         GOTO1 =A(BLD06REC),DMCB,(RC),RR=Y                                      
*                                  '0106' COMPETITIVE RECORD                    
MN520    EQU   *                                                                
         GOTO1 =A(BLD08REC),DMCB,(RC),RR=Y                                      
*                                  '0108' EASI RECORD                           
MN600    EQU   *                                                                
         GOTO1 =A(BLD10REC),DMCB,(RC),RR=Y                                      
*                                  '0110' ORDER ADD FINAL BUFFER                
*                                                                               
MN700    EQU   *                                                                
         GOTO1 =A(DATETIME),DMCB,(RC),RR=Y                                      
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
                                                                                
*********************************************************************           
*        BLD01REC  ---  BUILDS THE '0101' TRANSACTION RECORD                    
*********************************************************************           
BLD01REC NTR1                                                                   
*                                                                               
*  A SPECIAL TEST FOR STATION KAS:  RESET IT SO THAT ALL OUTPUT                 
*    REFLECTS 'KTVB'.  AT THIS POINT, ALL SPECIAL CALLS HAVE BEEN               
*    MADE ON THE ORIGINAL (KAS) STATION.  FROM NOW ON, THE STATION              
*    CALL LETTERS ARE ONLY INSERTED INTO THE OUTPUT RECORDS.                    
*                                                                               
         CLC   =C'KAS ',RCONKSTA   SPECIAL BLAIR TEST                           
         BNE   BLD10020            NOT SPECIAL STATION                          
         MVC   RCONKSTA(4),=C'KTVB' SPECIAL:  RESET STATION CALLS               
         B     BLD10040                                                         
BLD10020 EQU   *                                                                
*                                                                               
*  A SPECIAL TEST FOR STATION KGW:  RESET IT SO THAT ALL OUTPUT                 
*    REFLECTS 'KGWT'.  AT THIS POINT, ALL SPECIAL CALLS HAVE BEEN               
*    MADE ON THE ORIGINAL (KGW) STATION.  FROM NOW ON, THE STATION              
*    CALL LETTERS ARE ONLY INSERTED INTO THE OUTPUT RECORDS.                    
*                                                                               
         CLC   =C'KGW ',RCONKSTA   SPECIAL BLAIR TEST                           
         BNE   BLD10040            NOT SPECIAL STATION                          
         MVC   RCONKSTA(4),=C'KGWT' SPECIAL:  RESET STATION CALLS               
BLD10040 EQU   *                                                                
         MVI   REC01REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC01REC+1(REC01LEN-1),REC01REC                                  
***      MVC   REC01RP,REPALPHA    LOAD REP CODE                                
*                                  LOCAL REP CODE NO LONGER LOADED              
         MVC   REC01ID,=C'0101'    LOAD RECORD TYPE                             
         MVC   REC01REP(2),=C'RP'                                               
         MVC   REC01REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC01REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         MVC   REC01STA,RCONKSTA   INSERT STATION                               
         MVC   REC01IND,=C'03'     EC INDICATOR                                 
*                                                                               
*   TEST FOR EOP RECORDS FOUND.  IF NOT FOUND, TYPE = REP FOR REP.              
*        IF FOUND, TYPE = S FOR STATION.                                        
*                                                                               
         MVI   REC01TAG,C'S'       SET TO STATION INITIALLY                     
         OC    EOPAGY,EOPAGY       ANY VALUE IN AGENCY?                         
         BNZ   BLD10240            YES - TYPE = S                               
         MVI   REC01TAG,C'R'       NO  - TYPE = R                               
         MVC   EOPAGY(4),RCONKAGY  SET DDS CODE AS AGENCY                       
BLD10240 EQU   *                                                                
         MVC   REC01AGY,EOPAGY     LOAD AGENCY CODE                             
         MVC   REC01ANM,AGYNAME    LOAD AGENCY NAME FROM SUPPORT                
*                                                                               
         MVI   REC01TAD,C'S'       SET TO STATION INITIALLY                     
         OC    EOPADV,EOPADV       ANY VALUE IN ADVERTISER?                     
         BNZ   BLD10260            YES - TYPE = S                               
         MVI   REC01TAD,C'R'       NO  - TYPE = R                               
         MVC   EOPADV(4),RCONKADV  SET DDS CODE AS ADVERTISER                   
BLD10260 EQU   *                                                                
         MVC   REC01ADV,EOPADV     LOAD ADVERT CODE                             
         MVC   REC01NM,ADVNAME     LOAD ADVERT NAME FROM SUPPORT                
*                                                                               
         MVI   REC01TOF,C'S'       SET TO STATION INITIALLY                     
         OC    EOPOFF,EOPOFF       ANY VALUE IN OFFICE?                         
         BNZ   BLD10280            YES - TYPE = S                               
         MVI   REC01TOF,C'R'       NO  - TYPE = R                               
         MVC   EOPOFF(2),RCONKOFF  SET DDS CODE AS OFFICE                       
BLD10280 EQU   *                                                                
         MVC   REC01OFF,EOPOFF     LOAD OFFICE CODE                             
         MVC   REC01ONM,OFFNAME    LOAD OFFICE NAME FROM SUPPORT                
*                                                                               
         MVI   REC01TSP,C'S'       SET TO STATION INITIALLY                     
         OC    EOPSAL,EOPSAL       ANY VALUE IN S/P?                            
         BNZ   BLD10300            YES - TYPE = S                               
         MVI   REC01TSP,C'R'       NO  - TYPE = R                               
         MVC   EOPSAL(3),RCONSAL   SET DDS CODE AS S/P                          
BLD10300 EQU   *                                                                
         MVC   REC01SLS,EOPSAL     LOAD S/P    CODE                             
         MVC   REC01SNM,SALNAME    LOAD S/P    NAME FROM SUPPORT                
*                                                                               
         MVC   REC01PRD,PRODNAME   LOAD PRODUCT NAME FROM SUPPORT               
         MVC   REC01BYR,RCONBUYR   LOAD BUYER NAME FROM CON REC                 
         MVC   REC01BDY,=C'M '     SET BILL DAY                                 
         MVC   REC01BCD,=C'01'     SET ACCOUNTING PERIOD                        
         MVI   REC01RTG,C' '       SET RATING SERVICE INITIALLY                 
         CLI   RCONRTGS,C'A'       ARB?                                         
         BE    BLD10320            YES - USE IT                                 
         CLI   RCONRTGS,C'N'       NO  - NSI?                                   
         BNE   BLD10340            NO  - LEAVE AS SPACES                        
BLD10320 EQU   *                                                                
         MVC   REC01RTG,RCONRTGS   LOAD RATING SERVICE                          
BLD10340 EQU   *                                                                
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'A2'        LOAD FOR EASI ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   BLD10360            NO EASI ELEMENT                              
         USING RCONIEL,R6                                                       
*                                                                               
*   CHECK FORMAT OF ESTIMATE #.  WHY A BINARY FOUR BYTES?                       
*                                                                               
         MVC   SAVEST#,RCONXEST    MOVE EXPANDED ESTIMATE                       
         OC    RCONXEST,RCONXEST                                                
         BNZ   *+10                                                             
         MVC   SAVEST#,RCONIEST    MOVE ORIGINAL ESTIMATE                       
         OC    SAVEST#,SPACES      SET ANY BINARY ZERO TO SPACE                 
         CLC   SAVEST#,SPACES      ALL SPACES?                                  
         BE    BLD10360            YES                                          
         CLC   SAVEST#+4(6),SPACES LAST SIX CHARS SPACES?                       
         BNE   BLD10350            NO  - LONGER THAN 4 CHARS                    
         MVC   REC01EST(4),SAVEST# YES - INSERT 4 CHAR EST #                    
         MVC   SAVEST#,SPACES      CLEAR ESTIMATE NUMBER SAVE                   
         B     BLD10360                                                         
BLD10350 EQU   *                                                                
         MVC   REC01EST,=C'****'   INSERT INDICATOR FOR LONGER EST              
         DROP  R6                                                               
BLD10360 EQU   *                                                                
         MVC   REC01OTP,=C'05'     DEFAULT ORDER TYPE                           
         MVC   REC01ARE,=C'01'     SET AREA REP TO 'NATIONAL'                   
*                                                                               
         CLC   REPALPHA,=C'JB'     WJBK LOCAL?                                  
         BNE   BLD10370            NO                                           
         MVC   REC01ARE,=C'02'     SET AREA REP TO 'LOCAL'                      
BLD10370 EQU   *                                                                
*                                                                               
*   NEED BIT IN REP RECORD TO INDICATE 'NATIONAL' VS 'LOCAL',                   
*        WHICH IS A TYPE '02'                                                   
*                                                                               
         MVC   REC01CF1(6),FOXZEROS                                             
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'13'        LOAD FOR CONFLICT ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   BLD10460            NO CONFLICT ELEMENT                          
         USING RCONCCEL,R6                                                      
         MVC   REC01CF1,RCONCCPR   LOAD PRODUCT CONFLICT                        
         MVC   REC01CF2,RCONCCAD   LOAD ADVERTISER PRODUCT                      
         GOTO1 CROSSDAY,DMCB,(RC),(R6)                                          
         CLI   RCONCCAR,C'N'       NATIONAL AREA?                               
         BE    BLD10420            YES                                          
         CLI   RCONCCAR,C'L'       LOCAL AREA?                                  
         BNE   BLD10380            NO                                           
         MVC   REC01ARE,=C'02'     YES                                          
         B     BLD10420                                                         
BLD10380 EQU   *                                                                
         CLI   RCONCCAR,C'R'       REGIONAL AREA?                               
         BNE   BLD10400            NO                                           
         MVC   REC01ARE,=C'03'     YES                                          
         B     BLD10420                                                         
BLD10400 EQU   *                                                                
         CLI   RCONCCAR,C'O'       OTHER    AREA?                               
         BNE   BLD10420            NO                                           
         MVC   REC01ARE,=C'04'     YES                                          
BLD10420 EQU   *                                                                
         MVC   REC01OTP,=C'05'     SET ORDER TYPE DEFAULT                       
         CLC   RCONCCOT,=C'  '     ANY ORDER TYPE?                              
         BE    BLD10440            NO                                           
         CLC   RCONCCOT,=X'0000'   ANY ORDER TYPE?                              
         BE    BLD10440            NO                                           
         MVC   REC01OTP+1(1),RCONCCOT                                           
*                                  YES - INSERT ORDER TYPE                      
BLD10440 EQU   *                                                                
         CLI   RCONCCLN,RCONCCL2   LENGTH INDICATES CATEGORY CODE?              
         BL    BLD10460            NO  - DON'T LOAD ANYTHING                    
         MVC   REC01BPC,RCONCCCT   YES - MOVE TO PRODUCT CODE                   
         DROP  R6                                                               
BLD10460 EQU   *                                                                
*                                                                               
* CHECK FOR LOCAL FLAG IN X'1E' ELEMENT.  OVERRIDES ALL OTHER SETTINGS          
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONRFEL,R6                                                      
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL            NONE FOUND -                                 
         BNE   BLD10475                                                         
         TM    RCONRF1,X'20'       LOCAL ORDER?                                 
         BNO   BLD10470            NO                                           
         MVC   REC01ARE,=C'02'     YES                                          
BLD10470 EQU   *                                                                
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   BLD10475            NO                                           
         CLC   REC01OTP,=C'05'     YES - DEFAULT SET AS CASH?                   
         BNE   BLD10475            NO                                           
         MVC   REC01OTP,=C'01'     YES - RESET TO TRADE                         
         DROP  R6                                                               
BLD10475 EQU   *                                                                
*                                                                               
* DISPLAY FIRST PRIMARY DEMO ONLY.  IF NONE EXISTS, DISPLAY FIRST DEMO          
*                                                                               
         LA    R6,RCONREC                                                       
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'        RETRIEVE SAR ELEMENT                         
         BAS   RE,GETEL            NONE FOUND - SKIP DEMO                       
         BNE   BLD10520                                                         
         USING RSARXEL,R6                                                       
         LA    RE,RSARXDEM                                                      
         LA    RF,6                                                             
BLD10480 EQU     *                                                              
         TM    0(RE),X'40'         CHECK FOR PRIMARY DEMO                       
         BO    BLD10500                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,BLD10480                                                      
         LA    RE,RSARXDEM         NO PRIMARY, JUST TAKE FIRST                  
         DROP  R6                                                               
BLD10500 EQU   *                                                                
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
         GOTO1 DEMCON,DMCB,(1,(R5)),(6,REC01DMO),(0,DBLOCKD)                    
BLD10520 EQU   *                                                                
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        BLD02REC  ---  BUILDS THE '0102' TRANSACTION RECORD                    
*********************************************************************           
BLD02REC NTR1                                                                   
         MVI   REC02REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC02REC+1(REC02LEN-1),REC02REC                                  
*                                                                               
*    SHOULD NOT BE A PROBLEM WITH NATIONAL VS LOCAL REP.....                    
*                                                                               
         MVC   REC02ID,=C'0102'    LOAD RECORD TYPE                             
         MVC   REC02REP(2),=C'RP'                                               
         MVC   REC02REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC02REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         MVC   REC02STA,RCONKSTA   INSERT STATION                               
         MVC   REC02IND,=C'03'     EC INDICATOR                                 
*                                                                               
*    FILL IN THE COMMENTS AND SEND AS MANY RECORDS AS NEEDED.                   
*    TWO COMMENTS PER RECORD.                                                   
*    NO RECORD SENT IF THERE ARE NO COMMENTS AT ALL.                            
*                                                                               
         SR    R2,R2               SET TEST COUNTER                             
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'02'        CONTRACT COMMENT ELEMENT                     
         BAS   RE,GETEL            NONE FOUND - FINISHED                        
         BNE   BLD20140            EXIT WITH NO WRITES                          
         B     BLD20060                                                         
BLD20020 EQU   *                                                                
         MVI   REC02CM1,C' '       SPACE-FILL COMMENT FIELD                     
         MVC   REC02CM1+1(139),REC02CM1                                         
BLD20040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   BLD20120            NOT FOUND - CHECK FOR WRITE                  
BLD20060 EQU   *                                                                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         LTR   R2,R2               1ST OR SECOND COMMENT?                       
         BNZ   BLD20080            SECOND                                       
         EX    RF,BLD20090         MOVE FIRST COMMENT                           
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
         B     BLD20040            GO BACK FOR NEXT COMMENT                     
BLD20080 EQU   *                                                                
         EX    RF,BLD20091         MOVE SECOND COMMENT                          
         SR    R2,R2               RESET COMMENT COUNTER                        
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         B     BLD20020            CLEAR COMMENT FIELD,                         
*                                     GO BACK FOR NEXT COMMENT                  
BLD20090 MVC   REC02CM1(0),2(R6)   LOAD FIRST  COMMENT                          
BLD20091 MVC   REC02CM2(0),2(R6)   LOAD SECOND COMMENT                          
*                                                                               
BLD20120 EQU   *                                                                
         LTR   R2,R2               ANY COMMENT TO PRINT?                        
         BZ    BLD20140            NO  - FINISHED                               
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD20140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE '0103' TRANSACTION RECORD.                         
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE '0103' RECORD PER EFFECTIVE DATE IS GENERATED.                   
*      2.  IF THE BUY IS A M/G, SPECIAL COMMENTS IS/ARE GEN'D                   
*          DETAILING THE # OF SPOTS AND DATE FOR WHICH THE M/G                  
*          IS APPLICABLE.                                                       
*      3.  COMMENTS ARE GEN'D AS '0104' RECORDS                                 
*      4.  IF ANY COMMENTS EXIST, AND MULTIPLE EFFECTIVE DATES                  
*          ALSO EXIST, THE FIRST COMMENT GENERATED WILL INDICATE                
*          THE SUB LINES TO WHICH ALL SUBSEQUENT COMMENTS APPLY                 
*                                                                               
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
         ST    RF,ANXTMGTB         STORE A(NEXT AVAILABLE ENTRY)                
*                                                                               
         MVI   BUYPRTD,C'N'        SET 'BUY NOT PRINTED'                        
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    COLLAPS2,COLLAPS2   USED TO SKIP COLLAPSING                      
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   REC03REC,C' '       SPACE FILL RECORD                            
         MVC   REC03REC+1(REC03LEN-1),REC03REC                                  
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30460            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    BLD30020            YES - SKIP IT                                
BLD30040 EQU   *                                                                
**       GOTO1 =A(MERGE03S),DMCB,(RC),RR=Y                                      
*                                  REMERGE ANY MAKEGOOD SPOTS                   
*                                     TO RESET TO ORIGINAL BUY                  
*                                  ALSO DONE FOR MAKEGOOD RECORDS               
         GOTO1 =A(REC110$$),DMCB,(RC),RR=Y                                      
*                                  CYCLE ORIGINAL 03 ELTS,                      
*                                     ACCUMULATE BUY VALUE                      
         CLC   RBUYKMLN,RBUYKLIN   MASTER/DETAIL LINE SAME?                     
         BNE   BLD30045            NO  - PROCESS MAKEGOOD                       
*                                                                               
         GOTO1 =A(CRDIT16S),DMCB,(RC),=C'NOCR',RR=Y                             
*                                  REMERGE ANY CREDIT SPOTS                     
*                                     TO RESET TO ORIGINAL BUY                  
*                                  ALSO DONE FOR MAKEGOOD RECORDS               
*                                                                               
         MVI   FIRSTMG,0           YES - REGULAR BUY RECORD:                    
*                                     CLEAR FIRST MG LINE#                      
         B     BLD30050                                                         
BLD30045 EQU   *                                                                
*                                                                               
* READ THE MAKE GOODS FOR THIS MAKEGOOD LINE AND TABLE UP MISSED                
*  SPOTS                                                                        
*                                                                               
         GOTO1 =A(CYCLEMKG),DMCB,(RC),RR=Y                                      
*                                  NO  - MAKEGOOD RECORD                        
*                                  * 'SVBYLN#' = LAST BUYLINE                   
*                                     ACCUMULATE BUY VALUE                      
*                                                                               
*   THE 'CYCLEMKG' ROUTINE REREADS THE MAKEGOOD RECORD.  AS A RESULT,           
*        THIS REMOVES THE WORK DONE BY THE FIRST CALL TO CRDIT16S. SO           
*        THERE IS A SEPARATE CALL FOR REGULAR VS MAKEGOOD LINES.                
*                                                                               
         GOTO1 =A(CRDIT16S),DMCB,(RC),=C'NOCR',RR=Y                             
*                                  REMERGE ANY CREDIT SPOTS                     
*                                     TO RESET TO ORIGINAL BUY                  
*                                  ALSO DONE FOR MAKEGOOD RECORDS               
*                                                                               
BLD30050 EQU   *                                                                
         MVC   SVBYLN#,RBUYKLIN    SAVE BUY LINE #                              
         GOTO1 BLD3MKGD,DMCB,(RC)  SET MG + COMMENT FLAGS                       
*                                                                               
*   THIS NEXT TEST MAKES NO SENSE -  CHECK BLD3MKGD (B03RAUX)                   
*                                                                               
         OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
         BZ    BLD30060            NO                                           
         NI    COMFLAGS,X'7F'      TURN OFF M/G SWITCH                          
BLD30060 EQU   *                                                                
         MVC   REC03ID,=C'0103'    INSERT RECORD ID                             
         MVC   REC03REP(2),=C'RP'                                               
         MVC   REC03REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC02REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,REC03L#),FILL=0,ZERO=NOBLANK                          
         MVC   REC03STA,RCONKSTA                                                
         MVI   REC03MLN,C' '       SPACE ALL MG DATES/TIMES                     
         MVC   REC03MLN+1(23),REC03MLN                                          
         MVC   REC03IND,=C'03'     EC INDICATOR                                 
         MVI   REC03CMI,C'0'       SET 'NO COMMENTS'                            
         MVC   REC03MGS,=C'00'     SET MAKEGOOD SWITCH                          
         CLC   RBUYKMLN,RBUYKLIN   MASTER LINE = DETAIL LINE?                   
         BE    BLD30080            YES - NOT A MAKEGOOD                         
*                                                                               
*   TEST DUMPER                                                                 
****>>>  MVC   DIE2,=X'0000'       SET ABORT                                    
*   TEST DUMPER END                                                             
*                                                                               
         MVC   REC03MGS,=C'01'     SET MAKEGOOD SWITCH                          
         OC    MGSPOTS,MGSPOTS     ANY SPOTS MISSED?                            
         BNZ   BLD30080            YES  -                                       
         MVC   REC03MGS,=C'99'     NO  - SET ADDITION LINE IN SET               
         EDIT  (B1,FIRSTMG),REC03MLN,FILL=0                                     
         B     BLD30090                                                         
BLD30080 EQU   *                                                                
         MVC   FIRSTMG,RBUYKLIN    SAVE FIRST MG LINE # AS 1ST IN               
*                                     POSSIBLE SET OF MG LINES                  
BLD30090 EQU   *                                                                
*                                                                               
*   WHEN LATERUN/REPLACEMENT INFORMATION IS AVAILABLE, TEST IT                  
*        HERE TO SET THE FLAG APPROPRIATELY.                                    
*                                                                               
         CLC   RBUYKMLN,RBUYKLIN   MASTER LINE = DETAIL LINE?                   
         BE    BLD30094            YES - NOT A MAKEGOOD                         
*                                                                               
         TM    RBUYRTS,X'08'       LATE RUN?                                    
         BNO   BLD30092            NO                                           
         MVC   REC03MGS,=C'02'     YES                                          
         B     BLD30094                                                         
BLD30092 EQU   *                                                                
         TM    RBUYRTS,X'04'       LATE RUN/W BONUS - LATE RUN PART             
         BNO   BLD30094            NO                                           
         MVC   REC03MGS,=C'02'     YES                                          
BLD30094 EQU   *                                                                
         XC    MGSPOTS,MGSPOTS                                                  
         MVC   REC03FLG(2),FOXZEROS                                             
         MVC   REC03BBT(8),FOXZEROS                                             
         GOTO1 BLD3PTRN,DMCB,(RC)  PATTERN CODES?                               
         BNZ   BLD30120            YES                                          
         CLI   DTSTRNGS,2          NO  - HOW MANY D/T STRINGS?                  
         BL    BLD30100            ONE                                          
         MVC   REC03DAY(2),=C'*P'  TWO OR MORE:                                 
*                                     SET 'PATTERN RECORDS FOLLOW'              
         B     BLD30120                                                         
BLD30100 EQU   *                                                                
         GOTO1 DYTIME,DMCB,(RC)    ONE CODE:  GET/TRANSLATE IT                  
BLD30120 EQU   *                                                                
         MVC   FULL(2),RBUYDUR     SET UP SPOT LENGTH                           
         NI    FULL,X'7F'          TURN OFF 'MINUTES' BIT                       
         EDIT  (2,FULL),(3,REC03LN1),FILL=0,ZERO=NOBLANK                        
*                                  INSERT SPOT LENGTH 1                         
         MVI   REC03MS,C'S'        SET SEC/MIN TO 'SECS'                        
         TM    RBUYDUR,X'80'       SECONDS OR MINUTES?                          
         BNO   BLD30140            NOT ON = SECONDS                             
         MVI   REC03MS,C'M'        ON  =  SET SEC/MIN TO 'MINS'                 
         OI    COMFLAGS,X'20'      SET COMMENT BIT 2                            
BLD30140 EQU   *                                                                
         MVC   REC03LN2,FOXZEROS                                                
         MVC   REC03SEC(14),FOXZEROS                                            
         MVC   REC03PLC,SPACES                                                  
         CLC   RBUYKPLN,=X'FFFFFF' PLAN DEFAULT?                                
         BE    BLD30150            YES                                          
         MVC   REC03PLC,RBUYKPLN   NO  - LOAD PLAN CODE                         
BLD30150 EQU   *                                                                
         MVC   REC03PLP,FOXZEROS                                                
*                                                                               
*  DDS HAS NO TRAFFIC PLAN PRICE.  JDS KEEPS A SEPARATE ELEMENT WITH            
*    TRAFFIC PLAN CODE AND TRAFFIC PLAN PRICE.                                  
*                                                                               
         MVC   REC03CLS,SPACES                                                  
         MVC   REC03CLS(3),RBUYCLS INSERT CLASS, IF ANY                         
*                                                                               
BLD30160 EQU   *                                                                
         GOTO1 BLD3STCD,DMCB,(RC)                                               
*                                  GET STATION AND SECTION CODES                
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
         L     R6,AEFFDATE         RESET A(EFFECTIVE DATE ELEMENT)              
         ZIC   RF,1(R6)            L(EFFECTIVE DATE ELEMENT)                    
         BCTR  RF,0                DECREMENT 1                                  
         EX    RF,BLD30200         MOVE EFF DATE TO TABLE                       
         B     BLD30220                                                         
*                                                                               
BLD30200 MVC   BLDTABLE(0),0(R6)                                                
*                                                                               
BLD30220 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      ANY MORE '03' ELEMENTS?                      
         BE    BLD30380            NO                                           
         GOTO1 DTEINFO,DMCB,(RC),(R6)                                           
         CLI   SPOTSWK,0           ANY SPOTS IN THIS WEEK?                      
*                                                                               
*   TEST LOCATION FOR DUMP                                                      
DIE2     EQU   *                                                                
*   TEST LOCATION FOR DUMP                                                      
*                                                                               
         BZ    BLD30360            NO  - DON'T PROCESS BUY                      
         GOTO1 SPTINFO,DMCB,(RC),(R6)                                           
*                                                                               
         MVC   REC03ACT(3),FOXZEROS                                             
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   REC03ACT,C'1'       YES                                          
         MVC   REC03INA,=C'01'     SET INACTIVE WEEKS TO 01                     
         GOTO1 =A(ALTWKCHK),DMCB,(RC),RR=YES                                    
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
         CLI   MULTEFDT,1          YES - ONLY ONE MULT EFF DATE ELT?            
         BNE   BLD30280            NO  - INSERT SUBLINE                         
         MVI   REC03SL#,C' '       YES - SPACE OUT SUBLINE                      
         B     BLD30300                                                         
BLD30280 EQU   *                                                                
         STC   RF,REC03SL#         INSERT 'SUBLINE' FROM SUBLINES               
BLD30300 EQU   *                                                                
         MVC   SVSL#,REC03SL#      SAVE SUBLINE                                 
         CLI   DTSTRNGS,2          *P (PATTERN FOLLOWS)?                        
         BNL   BLD30320            YES                                          
         TM    COMFLAGS,X'40'      NO  - COMMENTS TO FOLLOW?                    
         BO    BLD30320            YES                                          
         TM    COMFLAGS,X'20'      NO  - TIME IN MINUTES?                       
         BNO   BLD30340            NO                                           
BLD30320 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      LAST EFF DATE ELEMENT?                       
         BNE   BLD30340            NO                                           
         MVI   REC03CMI,C'1'       YES - SET COMMENT FLAG = 1                   
BLD30340 EQU   *                                                                
***>     GOTO1 PBDISP,DMCB,(RC)    PRINT PROGRAM BUY AND CODE                   
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
         MVI   BUYPRTD,C'Y'        SET 'BUY PRINTED'                            
*                                                                               
BLD30360 EQU   *                                                                
         XC    BLDTABLE,BLDTABLE                                                
         MVI   REC03MLN,C' '       CLEAR DATES/TIMES                            
         MVC   REC03MLN+1(23),REC03MLN                                          
         MVI   FIRSTSW,0           CLEAR FIRSTSW                                
         B     BLD30180            GET NEXT EFF DATE ELEMENT                    
BLD30380 EQU   *                                                                
         OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
         BZ    BLD30400            NO  - CONTINUE                               
         CLI   MULTEFDT,2          IF ZERO SPOTS AND ONLY 1                     
         BL    BLD30440               EFF DATE, DON'T SEND COMMENTS             
BLD30400 EQU   *                                                                
         CLI   BUYPRTD,C'N'        BUY PRINTED?                                 
         BE    BLD30440            NO  - DON'T SEND SECONDARY INFO              
         OC    COMFLAGS,COMFLAGS   ANY COMMENTS EXPECTED?                       
         BZ    BLD30420            NO  -                                        
         GOTO1 =A(BLD04REC),DMCB,(RC),RR=Y                                      
*                                  YES - ADD CMMTS/MG AS APPROP                 
BLD30420 EQU   *                                                                
         CLI   DTSTRNGS,2          > 1 D/T STRNG, OR NO PATTERN?                
         B     BLD30440            NO  - NO 0105 RECORD                         
*                                                                               
*   ABOVE BRANCH IS IN JDS CODE.... TEST IS BEING IGNORED, AND                  
*        NO 0105 RECORDS ARE EVER PUT OUT....                                   
*                                                                               
         GOTO1 BLD05REC,DMCB,(RC)                                               
BLD30440 EQU   *                                                                
         OC    MGCOUNTR,MGCOUNTR   MAKEGOODS FOUND?                             
         BZ    BLD30450            NO                                           
         GOTO1 =A(BLD07REC),DMCB,(RC),RR=Y                                      
         XC    MGCOUNTR,MGCOUNTR   CLEAR COUNTER                                
BLD30450 EQU   *                                                                
         B     BLD30020            ACCESS NEXT BUY RECORD                       
*                                                                               
BLD30460 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETRIEVE AUXILIARY DATA, SET MG AND/OR COMMENT FLAG.                        
*        SET MULTI D/T STRING FLAGS IF PATTERN RECORDS NEEDED.                  
*                                                                               
BLD3MKGD NTR1                                                                   
         XC    DTSTRNGS,DTSTRNGS                                                
         XC    COMFLAGS,COMFLAGS                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0005            NO PROGRAM NAME ELT FOUND                    
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
         MVI   ELCODE,X'05'        LOOK FOR MAKEGOOD ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0030            NO MAKEGOOD ELEMENT FOUND                    
         OI    COMFLAGS,X'80'      FOUND - SET MG FLAG                          
         MVC   CNVDATE,3(R6)       SAVE MAKEGOOD START DATE                     
*                                                                               
*   IS THIS SUPPOSED TO BE MAKEGOOD START DATE?????                             
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
         CLI   DTSTRNGS,1          MORE THAN ONE DAY/TIME STRING?               
         BNH   BMKG0040            NO  - LOOK FOR ANOTHER                       
BMKG0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   BLD3STCD -  CHECK FOR STATION CODE ELEMENTS                                 
*        THIS WILL REQUIRE SOME ALIGNMENT.  JDS AND DDS HAVE                    
*        DIFFERING FORMATS FOR THIS PIECE OF INFORMATION.                       
*                                                                               
BLD3STCD NTR1                                                                   
*        GOTO1 STACDISP,DMCB,(RC)                                               
*                                                                               
*   ABOVE ROUTINE ADDS SOME SORT OF STATION CODE TO THE 03 RECORD               
*        DDS HAS NO BIAS STATION CODES TRAFFIC ELEMENT                          
*                                                                               
         MVC   REC03SEC,FOXZEROS   SET SECTION CODE TO C'0'                     
         OC    RBUYSEC(2),RBUYSEC  INCASE IF NULLS, 1ST 2 CHARS ONLY            
         BZ    BSTC0020                                                         
         CLC   =C'  ',RBUYSEC      ANY VALUE IN BUY SECTION CODE?               
*                                     1ST 2 CHARS ONLY                          
         BE    BSTC0020            NO  - LEAVE AS IS                            
         CLI   RBUYSEC+1,C' '      SECOND CHARACTER SPACE?                      
         BNE   BSTC0010            NO  - LOAD BOTH CHARACTERS                   
         MVC   REC03SEC+1(1),RBUYSEC                                            
*                                  YES - LOAD SINGLE CHAR TO 2ND POS            
         B     BSTC0020                                                         
BSTC0010 EQU   *                                                                
         MVC   REC03SEC,RBUYSEC    LOAD 2-CHAR SECTION CODE                     
BSTC0020 EQU   *                                                                
*        GOTO1 PLNDISP,DMCB,(RC)                                                
*                                                                               
*   ABOVE ROUTINE ADDS PLAN INFORMATION.  DDS HAS NO PLAN INFO TO               
*        ADD, AS THESE ITEMS ARE NOT ON DDS FILE.                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE '0105' TRANSACTION RECORD                           
*                                                                               
*   NOTE:  0105 PATTERN RECORD(S) IS/ARE PRODUCED ONLY IF                       
*        1.  BUYLINE HAS MORE THAN ONE DAY/TIME STRING                          
*        2.  THE SINGLE DAY/TIME STRING GENERATES A TRANSLATION                 
*            IN XS OF 12 CHARACTERS                                             
*                                                                               
BLD05REC NTR1                                                                   
         MVI   REC05REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC05REC+1(REC05LEN-1),REC05REC                                  
BLD50020 EQU   *                                                                
         MVC   REC05ID,=C'0105'    INSERT RECORD TYPE                           
         MVC   REC05REP(2),=C'RP'                                               
         MVC   REC05REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC05REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,REC05L#),FILL=0,ZERO=NOBLANK                          
         MVC   REC05SL#,SVSL#      SUB LINE #                                   
         MVC   REC05STA,RCONKSTA                                                
         MVC   REC05IND,=C'03'     EC INDICATOR                                 
         MVC   REC05TYP,=C'03'     PATTERN COMMENT                              
         XC    CMTCTR,CMTCTR                                                    
         LA    R4,DTCOMNT          A(D/T STRING CMMT BUILD AREA)                
         LA    R7,REC05CM1         A(1ST 0105 COMMENT)                          
BLD50040 EQU   *                                                                
         XC    DTBUILD,DTBUILD                                                  
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'02'        BUY DAY/TIME ELEMENT                         
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLD50080                                                         
BLD50060 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLD50080 EQU   *                                                                
         BNE   BLD50300            ELEMENT NOT FOUND - DONE                     
         LA    R2,DTBUILD                                                       
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),(R2),RR=YES                          
*                                  BUILD A DAY-STRING FROM ELEMENT              
         LA    R1,DTBUILD          FIND NEXT AVAILABLE POSITION                 
         LA    R1,L'DTBUILD-1(R1)  FIND LAST POSITION                           
BLD50100 EQU   *                                                                
         CLI   0(R1),0             ANY VALUE IN FIELD?                          
         BNE   BLD50120            YES -                                        
         BCTR  R1,0                NO  - GO BACK 1 POSITION                     
         LTR   R1,R1               ANYTHING LEFT?                               
         BNZ   BLD50100            GO BACK FOR NEXT POSITION                    
         DC    H'0'                SOMETHING SHOULD HAVE BEEN THERE             
BLD50120 EQU   *                                                                
         AR    R2,R1               ADD L(DAY STRING)                            
         MVI   0(R2),C'/'          INSERT SEPARATOR                             
         LA    R1,1(R1)            INCREASE LEN FOR SEPARATOR                   
         LA    R2,1(R2)            SET NEXT AVAILABLE POSIT                     
         GOTO1 =V(UNTIME),DMCB,4(R6),(R2),RR=YES                                
*                                  BUILD A TIME-STRING FROM ELEMENT             
         LA    R1,DTBUILD          FIND LENGTH OF D/T STRING                    
         LA    R1,L'DTBUILD-1(R1)  FIND LAST POSITION                           
BLD50140 EQU   *                                                                
         CLI   0(R1),0             ANY VALUE IN FIELD?                          
         BNE   BLD50160            YES -                                        
         BCTR  R1,0                NO  - GO BACK 1 POSITION                     
         LTR   R1,R1               ANYTHING LEFT?                               
         BNZ   BLD50140            GO BACK FOR NEXT POSITION                    
         DC    H'0'                SOMETHING SHOULD HAVE BEEN THERE             
BLD50160 EQU   *                                                                
         LR    R5,R4               SEE IF NEW D/T STRING FITS                   
         AR    R5,R1               ADD LENGTH TO NEXT ADDR                      
         LA    RF,DTCOMEND         END OF SPACE                                 
         CR    R5,RF               DATA VS SPACE END                            
         BNL   BLD50240            NO ROOM -                                    
BLD50180 EQU   *                                                                
         LR    RF,R1               RESET LENGTH                                 
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,BLD50200                                                      
         B     BLD50220                                                         
BLD50200 MVC   0(0,R4),DTBUILD                                                  
*                                                                               
BLD50220 EQU   *                                                                
         LA    R4,1(R4)            ADD 1 SPACE TO OUTPUT                        
         B     BLD50040            GO FOR NEXT D/T STRING                       
BLD50240 EQU   *                                                                
         MVC   0(DTCOMLEN,R7),DTCOMNT                                           
*                                  LOAD COMMENT TO #1 OR # 2                    
         ZIC   RF,CMTCTR                                                        
         LA    RF,1(RF)            ADD TO # COMMENTS IN 0105                    
         STC   RF,CMTCTR                                                        
         CLI   CMTCTR,3            0105 COMPLETELY FULL?                        
         BE    BLD50280            YES - PRINT IT                               
         LA    R7,REC05CLN(R7)     NO  - BUMP TO NEXT COMMENT                   
BLD50260 EQU   *                                                                
         XC    DTCOMNT,DTCOMNT                                                  
         LA    R4,DTCOMNT          RESET START OF D/T STRING BUILD              
         B     BLD50180            START NEW STRING                             
BLD50280 EQU   *                                                                
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         MVI   REC05CM1,C' '                                                    
         MVC   REC05CM1+1(REC05TCM-1),REC05CM1                                  
*                                  SPACE OUT COMMENT FIELDS                     
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
         LA    R7,REC05CM1         RESET A(COMMENT FIELDS)                      
         B     BLD50260                                                         
BLD50300 EQU   *                                                                
*                                                                               
*   THERE MAY BE A PARTIAL SET OF D/T STRINGS BEING CONSTRUCTED                 
*        WHEN THE FINAL ELEMENT IF FOUND, WHICH MUST BE PRINTED.                
*                                                                               
         MVC   0(DTCOMLEN,R7),DTCOMNT                                           
*                                  MOVE STRING TO OUTPUT AREA                   
         LA    R1,REC05CM1         SCAN FIELD FOR NON-SPACES                    
         LA    R0,REC05TCM         SET LENGTH OF TOTAL COMMENTS                 
BLD50320 EQU   *                                                                
         CLI   0(R1),X'40'         FIELD = SPACE?                               
         BNE   BLD50340            NO  - PRINT OUT RECORD                       
         LA    R1,1(R1)            YES - CHECK NEXT POSITION                    
         BCT   R0,BLD50320         GO BACK FOR NEXT                             
         B     BLD50360            ALL SPACES - GET OUT W/NO PRINT              
*                                                                               
*    I DON'T KNOW ABOUT THIS FINAL RECORD TEST... IT DOESN'T FEEL OK            
*                                                                               
BLD50340 EQU   *                                                                
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD50360 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*********************************************************************           
INIT     NTR1                                                                   
*                                                                               
         GOTO1 =A(RESOLVE),DMCB,(RC),RR=YES   RESOLVE ADDRESSES                 
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
         MVI   NOPQCLOS,0          ASSUME WE HAVE OUTPUT                        
         MVI   FOXZEROS,X'F0'                                                   
         MVC   FOXZEROS+1(L'FOXZEROS-1),FOXZEROS                                
         XC    TRANSCNT,TRANSCNT                                                
         XC    TOTALAMT,TOTALAMT                                                
         XC    EXTRAKEY,EXTRAKEY                                                
         XC    MYP,MYP             CLEAR AREA FOR PLAN TABLE                    
*        LA    RF,RBUYREC          SET A(IO AREA # 3)                           
*        A     RF,=F'2000'         RBUYREC+2000                                 
*        ST    RF,AIO3                                                          
         MVI   INTTYPE,C'E'        SET INPUT TYPE TO 'EC'                       
         LA    RF,SPOOLEND-SPOOLD                                               
         LA    RE,SPOOLD           CLEAR THE SPOOL AREA                         
         XCEF                                                                   
         MVI   SPACES,C' '         INITIALIZE SPACES FIELD                      
         MVC   SPACES+1(131),SPACES   IN SPOOL AREA                             
*                                                                               
         MVC   SPOOLDM,DATAMGR     INITIALIZE SPOOLER DATAMGR                   
         L     RF,AFACILS          A(FACILITIES LIST)                           
         LM    R2,R4,8(RF)                                                      
         ST    R3,ATIA             A(TERMINAL INPUT AREA??)                     
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
CROSSDAY NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            RESET A(CONFLICT CODE ELEMENT)               
         USING RCONCCEL,R6                                                      
         ZIC   RF,RCONCCCD         LOAD CROSS-DAY TO REGISTER                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         SLL   RF,25               MOVE CROSS-DAY TO HIGH-ORDER                 
*                                    1ST POSITION IS SPARE, SO                  
*                                        PASS IT ALSO!!                         
         LA    RE,7                SET LOOP CONTROL                             
         LA    R1,DAYCODES         SET A(CODE TABLE)                            
CDAY0020 EQU   *                                                                
         LTR   RF,RF               IS HIGH-ORDER (NEGATIVE) SET?                
         BNM   CDAY0040            NO  - DAY OPEN - USE IT                      
         SLL   RF,1                YES - DAY NOT OPEN                           
         LA    R1,2(R1)            BUMP TO NEXT TABLE ENTRY                     
         BCT   RE,CDAY0020         GO BACK FOR NEXT                             
         DC    H'0'                NO DAYS OPEN!!!!  ABORT                      
CDAY0040 EQU   *                                                                
         MVC   REC01BDY,0(R1)      FOUND - INSERT DAY CODE                      
         XIT1                                                                   
*                                                                               
DAYCODES DC    CL14'M T W THF SASU'                                             
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
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         LTR   RB,RB               BUY FOUND:  CC NOT = ZERO                    
REBU0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PATTERN ELEMENT DOES NOT EXIST IN DDS.  RETURN 'NOT FOUND'                  
*                                                                               
BLD3PTRN NTR1                                                                   
         SR    R0,R0               RETURN CC = ZERO                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE DAY AND TIME TO BIAS FORMAT.                                      
*        NOTE:  DAYS AND TIMES MUST BE BROUGHT IN LINE WITH BIAS                
*        FORMATS.  FOR DAYS, JDS STRIPS OUT EXTRA CHARACTERS (IE,               
*        MON -> M, SAT -> SA.  THIS MAY OR MAY NOT BE NECESSARY AS              
*        DDS ROUTINES DO NOT PERFORM IDENTICAL WITH JDS.  CHECK IT              
*        OUT AND MAKE NECESSARY ADJUSTMENTS.                                    
*                                                                               
DYTIME   NTR1                                                                   
         MVC   XDATE,SPACES        SPACE FILL WORK AREA                         
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET DAY/TIME ELEMENT                         
         BNE   DYTEXT              NOT FOUND - EXIT                             
         LA    RF,3(R6)            A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),2(R6)       INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =A(DAYUNPK),DMCB,,(0,XDATE),RR=YES                               
         BAS   RE,CHKSHORT         CHECK STRING FOR TRUNC'D CHARS               
         BAS   RE,CHKXDATE         CHANGE DDS STRING TO BIAS                    
         BAS   RE,CHKXTRDA         CHECK FOR EXTRA CHAR IN STRING               
         MVC   REC03DAY,XDATE      LOAD RESULT TO OUTPUT RECORD                 
         CLC   =C'TB',4(R6)        START TIME = TBA?                            
         BNE   DYTI0020            NO                                           
         MVI   REC03BBT,C'8'       YES - FILL TIMES WITH X'F8'                  
         MVC   REC03BBT+1(7),REC03BBT                                           
         B     DYTEXT                                                           
DYTI0020 EQU   *                                                                
         EDIT  (2,4(R6)),(4,REC03BBT),FILL=0,ZERO=NOBLANK                       
*                                  SET BEGINNING TIME                           
         CLC   =C'CC',6(R6)        END TIME = CC?                               
         BNE   DYTI225             NO                                           
         MVC   REC03BET,=C'9999'   YES - FILL END TIME WITH X'F9'               
         B     DYTI232                                                          
DYTI225  EQU   *                                                                
         EDIT  (2,6(R6)),(4,REC03BET),FILL=0,ZERO=NOBLANK                       
DYTI232  EQU   *                                                                
         CLC   =C'00',REC03BBT     BEGIN TIME = 00?                             
         BNE   DYTI235             NO                                           
         MVC   REC03BBT(2),=C'24'  YES - SET TO 2400                            
DYTI235  EQU   *                                                                
         CLC   =C'00',REC03BET     END TIME HOURS = ZERO?                       
         BNE   DYTEXT              NO                                           
         CLC   =C'00',REC03BET+2   YES - END TIME MINS = ZERO?                  
         BE    DYTEXT              YES - END TIME ALL ZERO                      
         MVC   REC03BET(2),=C'24'  NO  - HRS = ZERO, MINS HAVE VALUE            
*                                     SET HOURS TO 24                           
DYTEXT   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHECK FOR A TRUNCATED LAST DAY IN CHARACTER STRING RETURNED.                  
*                                                                               
CHKSHORT NTR1                                                                   
         CLC   XDATE+10(2),SPACES  ANYTHING IN LAST TWO POS?                    
         BE    CHKS0900            NO                                           
         CLC   =C'S ',XDATE+10     SAT/SUN TRUNCATED?                           
         BE    CHKS0040            YES - FIX IT                                 
         CLC   =C'T ',XDATE+10     TUE/THU TRUNCATED?                           
         BNE   CHKS0900            NO  - EXIT                                   
CHKS0040 EQU   *                                                                
*                                                                               
*   USE ROTATOR END DAY TO DETERMINE WHAT LAST DAY WAS.                         
*                                                                               
         ZICM  RE,2(R6)            GET ROTATOR DAYS                             
         SLL   RE,28               DROP FIRST DAY                               
         SRL   RE,28               REALIGN                                      
         BCTR  RE,0                MAKE DAY ZERO RELATIVE                       
         SLL   RE,1                DOUBLE FOR SIZE DISPLACEMENT                 
         LA    RF,ROTEDAYS                                                      
         AR    RF,RE               ADD DISPLACEMENT TO A(TABLE)                 
         MVC   XDATE+10(2),0(RF)   INSERT FULL VALUE INTO TABLE                 
CHKS0900 EQU   *                                                                
         XIT1                                                                   
*                .0.1.2.3.4.5.6                                                 
ROTEDAYS DC    C'M T W THF SASU'                                                
         EJECT                                                                  
*                                                                               
* CHANGE DDS DAY-STRING DESCRIPTOR TO THE BIAS EQUIVALENT                       
*                                                                               
CHKXDATE NTR1                                                                   
         LA    R1,XDATETAB         A(EQUIV TABLE)                               
CKXD0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF TABLE?                                
         BE    CKXD0090            YES - FINISHED                               
         CLC   XDATE(3),0(R1)      NO  - DDS VS TABLE                           
         BE    CKXD0020            FOUND IN TABLE                               
         LA    R1,5(R1)            NOT FOUND - BUMP TO NEXT ENTRY               
         B     CKXD0010            GO BACK FOR NEXT                             
CKXD0020 EQU   *                                                                
         MVC   XDATE,SPACES        CLEAR XDATE FIELD                            
         MVC   XDATE(2),3(R1)      MOVE IN EQUIVALENT DAY STRING                
CKXD0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
XDATETAB DC    CL5'MONM '                                                       
         DC    CL5'TUET '                                                       
         DC    CL5'WEDW '                                                       
         DC    CL5'THUTH'                                                       
         DC    CL5'FRIF '                                                       
         DC    CL5'SATSA'                                                       
         DC    CL5'SUNSU'                                                       
         DC    X'00'               DELIMITER                                    
         EJECT                                                                  
*                                                                               
* CHKXTRDA:  IF 'TU' IN STRING, CHANGE IT TO 'T', DROP REST OF                  
*    STRING BACK                                                                
*                                                                               
CHKXTRDA NTR1                                                                   
         LA    R1,XDATE            A(CONVERTED DAY STRING)                      
         LA    RF,13               NUMBER OF CHARS - 1                          
CKTU0020 EQU   *                                                                
         CLC   =C'MO',0(R1)        STRING = 'MO'?                               
         BE    CKTU0100            YES - STRIP + COMPRESS                       
         CLC   =C'TU',0(R1)        STRING = 'TU'?                               
         BE    CKTU0100            YES - STRIP + COMPRESS                       
         CLC   =C'WE',0(R1)        STRING = 'WE'?                               
         BE    CKTU0100            YES - STRIP + COMPRESS                       
         CLC   =C'FR',0(R1)        STRING = 'FR'?                               
         BE    CKTU0100            YES - STRIP + COMPRESS                       
         LA    R1,1(R1)            NO  - BUMP TO NEXT POSITION                  
         BCT   RF,CKTU0020         GO BACK FOR NEXT                             
         B     CKTU0150            DONE - NOT FOUND                             
CKTU0100 EQU   *                                                                
         LA    R2,2(R1)            START OF STRING TO MOVE                      
         LA    R1,1(R1)            WHERE TO MOVE IT                             
         BCTR  RF,0                DECREMENT FOR MOVE                           
         BCTR  RF,0                DECREMENT FOR EX STATEMENT                   
         EX    RF,CKTU0200         EXEC STATEMENT                               
         MVI   XDATE+12,C' '       SPACE FILL LAST POSITION                     
CKTU0150 EQU   *                                                                
         XIT1                                                                   
CKTU0200 MVC   0(0,R1),0(R2) MOVE BY LENGTH (RF)                                
         EJECT                                                                  
*                                                                               
* GET BUY DATE INFORMATION                                                      
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
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         CLC   RBUYKPLN,=X'FFFFFF'                                              
*                                  PLAN DEFAULT?                                
         BE    DINF0010            YES                                          
         BAS   RE,ADDPLAN$         NO  - ADD PLAN $$, IF NEEDED                 
         B     DINF0015                                                         
DINF0010 EQU   *                                                                
*                                                                               
*   TEST DUMP                                                                   
***      CLI   RBUYKLIN,13         BUYLINE 4?                                   
***      BNE   *+6                                                              
***      DC    H'0'                YES = TEST DUMP                              
*   TEST END                                                                    
*                                                                               
         CLC   RBUYKMLN,RBUYKLIN   MASTER LINE = DETAIL LINE?                   
         BNE   DINF0015            NO  - MG:  DON'T ADD THESE SPOTS IN          
*                                  YES - ACCUMULATE THESE SPOTS                 
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
*                                                                               
*   NO LONGER DONE HERE:  NOW DONE IN REC110$$ ROUTINE                          
*                                                                               
***      L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
***      AR    RF,R1                                                            
***      ST    RF,TOTALAMT                                                      
DINF0015 EQU   *                                                                
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
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK?                              
         BNO   DINF0020            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
DINF0020 EQU   *                                                                
         MVC   ENDDTE,RBUYDTED     FORCE END DATE IN                            
         OC    RBUYDTED,RBUYDTED   ANY END DATE?                                
         BNZ   DINF0070            YES - END DATE EXISTS                        
*                                     USE FORCED DATE                           
         MVC   HOLDDATE(3),CKSTRDTE                                             
*                                  NO  - TEMPORARY HOLD AREA                    
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
         L     RE,TLSPOTS                                                       
         AR    RE,RF                                                            
         ST    RE,TLSPOTS                                                       
         ZIC   RF,NOFWKS           BUMP NUMBER OF WEEKS                         
         LA    RF,1(RF)                                                         
         STC   RF,NOFWKS                                                        
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
         MVC   REC03BBD(12),FOXZEROS                                            
         XC    HOLDDATE,HOLDDATE                                                
*                                                                               
*    INSERT START DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,STARTDTE),(X'20',HOLDDATE)                        
         MVC   REC03BBD(4),HOLDDATE+2 INSERT MM/DD                              
         MVC   REC03BBD+4(2),HOLDDATE INSERT YY                                 
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',HOLDDATE)                          
         MVC   REC03BED(4),HOLDDATE+2 INSERT MM/DD                              
         MVC   REC03BED+4(2),HOLDDATE INSERT YY                                 
DINF0140 EQU   *                                                                
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
         L     R6,AEFFDATE         A(EFF DATE ELT IN PROGRESS)                  
         BAS   RE,NEXTEL           GET NEXT 03 ELEMENT                          
         BNE   CHKD0120            NO MORE - EXIT                               
         ST    R6,AEFFDATE         SAVE A(NEXT EFF DATE ELT)                    
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
********************************************************************            
*    FOLLOWING TEST INVALID:  REMOVED                              *            
*                                                                  *            
*****>>  CLI   FIRSTSW,0           ALTERNATE WEEK PATTERN?         *            
*****>>  BNE   CHKD0040            YES                             *            
*                                                                  *            
********************************************************************            
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
         CLC   THIRDATE,SVSTRDTE                                                
*                                  SEQUENTIAL BUY WEEK?                         
         BE    CHKD0060            YES - SET SWITCH AND EXIT                    
         B     CHKD0160            NO  - NOT ALT WEEK PATTERN,                  
*                                     DON'T COLLAPSE ANY FURTHER                
CHKD0040 EQU   *                                                                
         LA    RF,14               LOOK FOR ALTERNATING                         
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE,SVSTRDTE                                                
*                                  ALTERNATING BUY WEEK?                        
         BNE   CHKD0160            NO  - NOT SEQ OR ALTERNATING                 
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
         SR    R0,R0               CALCULATE TOTAL AMOUNT OF ELT                
         MVC   FULL,RBUYCOS        RATE FOR LINE *                              
         CLC   RBUYKPLN,=X'FFFFFF'                                              
*                                  PLAN DEFAULT?                                
         BE    CHKD0110            YES                                          
         BAS   RE,ADDPLAN$         NO  - ADD PLAN $$, IF NEEDED                 
         B     CHKD0020                                                         
CHKD0110 EQU   *                                                                
         L     R1,FULL                                                          
         ZIC   RF,SPOTSWK             SPOTS PER WEEK *                          
         MR    R0,RF                                                            
         ZIC   RF,NOFWKS                 NUMBER OF WEEKS =                      
         MR    R0,RF                        TOTAL AMOUNT                        
         CLC   RBUYKMLN,RBUYKLIN   MASTER LINE = DETAIL LINE?                   
         BNE   CHKD0020            NO  - MG:  DON'T ADD THESE SPOTS IN          
*                                  YES - ACCUMULATE THESE SPOTS                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT         SAVE IT                                      
         B     CHKD0020        GO BACK FOR NEXT ELEMENT                         
CHKD0120 EQU   *                                                                
         MVI   COLLAPSE,X'FF'      SET COLLAPSE TO 'NO MORE'                    
CHKD0160 EQU   *                                                                
         CLI   COLLAPSE,1          PASS PRODUCED COLLAPSE?                      
         BE    CHKD0180            YES -                                        
         MVI   COLLAPS2,1          NO  - DON'T COLLAPSE BUY FURTHER             
CHKD0180 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   GET SPOT INFO:  # OF SPOTS PER WEEK, PRICE PER SPOT, ETC.                   
*                                                                               
SPTINFO  NTR1                                                                   
         EDIT  SPOTSWK,(3,REC03SPT),FILL=0,ZERO=NOBLANK                         
*                                  INSERT NUMBER SPOTS PER WEEK                 
         CLC   RBUYKPLN,=X'FFFFFF' PLAN DEFAULT?                                
         BNE   SPFO0010            NO  - PLAN RECORD - NO SPOT $$               
*                                  YES - ADD SPOT PRICE                         
         EDIT  RBUYCOS,(9,REC03SPR),FILL=0,ZERO=NOBLANK                         
*                                  INSERT PRICE PER SPOT                        
         MVC   REC03PLP,FOXZEROS   CLEAR PLAN PRICE                             
         B     SPFO0020                                                         
SPFO0010 EQU   *                                                                
         EDIT  RBUYCOS,(9,REC03PLP),FILL=0,ZERO=NOBLANK                         
*                                  INSERT PLAN PRICE                            
         MVC   REC03SPR,FOXZEROS   CLEAR SPOT PRICE                             
SPFO0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADDPLAN$:  BUY IS A PLAN.  CHECK IF PLAN ALREADY ENCOUNTERED.               
*      IF NOT, ADD PLAN$ (RBUYCOS) TO TOTAL AMOUNT.  IF YES, DO                 
*      NOTHING:  PLAN$ ARE TO BE ADDED ONLY ONCE.                               
*                                                                               
ADDPLAN$ NTR1                                                                   
         LA    R1,MYP              A(MY PLAN AREA)                              
ADP$0010 EQU   *                                                                
         OC    0(3,R1),0(R1)       SLOT EMPTY?                                  
         BZ    ADP$0040            YES - ADD PLAN TO TABLE                      
         CLC   RBUYKPLN,0(R1)      SLOT PLAN = BUY PLAN?                        
         BE    ADP$0090            YES - FINISHED: DON'T ADD                    
         LA    R1,3(R1)            NO  - BUMP TO NEXT SLOT                      
         B     ADP$0010            GO BACK FOR NEXT                             
ADP$0040 EQU   *                                                                
         MVC   0(3,R1),RBUYKPLN    ADD PLAN TO TABLE                            
         L     RF,FULL             BUYLINE'S PLAN $$                            
         A     RF,TOTALAMT         ADD TOTALAMT                                 
         ST    RF,TOTALAMT         STORE IT BACK                                
ADP$0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROGRAM BUY AND PROGRAM CODE DISPLAY                                        
*        DDS HAS NO BIAS PROGRAM CODES TRAFFIC ELEMENT                          
*                                                                               
*PBDISP   NTR1                                                                  
*         XIT1                                                                  
         EJECT                                                                  
*                                                                               
*   STATION CODE TRAFFIC ELEMENT DISPLAY                                        
*        DDS HAS NO BIAS STATION CODES TRAFFIC ELEMENT                          
*                                                                               
*STACDISP NTR1                                                                  
*         XIT1                                                                  
         EJECT                                                                  
*                                                                               
*   PROGRAM BIAS PLAN CODE DISPLAY                                              
*        DDS HAS NO BIAS PLAN CODES TRAFFIC ELEMENT                             
*                                                                               
*PLNDISP  NTR1                                                                  
*         XIT1                                                                  
         EJECT                                                                  
*                                                                               
*        ADDRESS                                                                
*                                                                               
RELX     DS    F                                                                
**RELO     DS    F                                                              
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
RMSCMODX DC    X'00'               REGENSC MODES                                
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
       ++INCLUDE RECNTBIA2                                                      
         EJECT                                                                  
*                                                                               
*      LOCAL VARIABLES                                                          
*                                                                               
FOXZEROS DS    CL24                STRING OF X'F0'                              
EXTRAKEY DS    CL34                BUY KEY SAVE AREA                            
COLLAPSE DS    XL1                 FIRST  COLLAPSE FLAG                         
COLLAPS2 DS    XL1                 SECOND COLLAPSE FLAG                         
MULTEFDT DS    XL1                                                              
SVBYLN#  DS    XL1                                                              
ZEROSPTS DS    XL1                                                              
BUYPRTD  DS    CL1                                                              
TRANSCNT DS    F                                                                
COMFLAGS DS    XL1                 COMMENT FLAGS                                
*                                  BIT 0  =  MG SWITCH                          
*                                  BIT 1  =  BUY COMMENTS EXIST                 
FIRSTSW  DS    XL1                                                              
TRANSCT  DS    XL1                 TRANSACTION COUNTER                          
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
*                                                                               
MGCOUNTR DS    F                   MAKEGOOD MISSED SPOT COUNTER                 
AMGTABLE DS    A                   A(MG TABLE)                                  
ANXTMGTB DS    A                   A(NEXT SPOT IN MG TABLE)                     
MGSPOTS  DS    F                   MISSED SPOT COUNT                            
LASTMGMS DS    X                   LAST MASTER LINE #                           
FIRSTMG  DS    X                   FIRST MAKEGOOD AGAINST MASTER                
FIRSTMGD DS    XL8                 DATE OF FIRST MISSED SPOT YYYYMMDD           
SAV103ID DS    CL21                FIRST 21 BYTES OF 103 REC                    
*                                     ID/REP/CON#/BUYLINE #                     
BLDTABLE DS    XL24                (???? CHECK LENGTH)                          
SVSL#    DS    XL1                 SAVE SUBLINE #                               
CNVDATE  DS    CL6                 MG DATE                                      
HOLDDATE DS    CL6                                                              
DATEWORK DS    CL12                DATE WORK AREA                               
         ORG   DATEWORK                                                         
FRSTDATE DS    CL6                                                              
SECDATE  DS    CL6                                                              
THIRDATE DS    XL3                                                              
CMTCTR   DS    XL1                 COMMENT COUNTER                              
NOFWKS   DS    XL1                 NUMBER OF WEEKS CTR                          
ALTWKS   DS    XL1                 ALTERNATING WEEKS                            
STARTDTE DS    XL3                 START DATE                                   
ENDDTE   DS    XL3                 END   DATE                                   
SVCNTWK  DS    XL1                 SAVE NUMBER OF WEEKS                         
SPOTSWK  DS    XL1                 SPOTS PER WEEK                               
CKSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSPTSWK DS    XL1                 FOR CHECKDATE ROUTINE                        
TLSPOTS  DS    F                   TOTAL SPOTS                                  
DAYBUMP  DS    F                                                                
AEFFDATE DS    A                   A(EFFECTIVE DATE ELEMENT)                    
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
XDATE    DS    CL14                TRANSLATE DATE                               
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
NEW15ELT DS    CL20                EC CONTROL ELT AREA                          
*                                                                               
COMPSTAS DS    CL48                SPACE FOR 12 STATIONS                        
MYDUB    DS    D                                                                
SAVEST#  DS    CL10                SAVE AREA FOR ESTIMATE NUMBER                
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
         CSECT                                                                  
*---------------------------------------------------------------------          
PRINTREC NTR1  BASE=*,LABEL=*                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         L     RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         ST    RF,TRANSCNT         SAVE COUNT                                   
         CLC   =C'0103',REC03ID    BUY RECORD?                                  
         BNE   PREC0010            NO                                           
         OC    MGCOUNTR,MGCOUNTR   0103 = MAKEGOOD RECORD?                      
         BZ    PREC0010            NO                                           
         MVC   SAV103ID,REC03ID    YES - SAVE FIRST 21 BYTES                    
*                                                                               
*   TEST DUMP                                                                   
***      MVC   PREC0060(2),=X'0000'                                             
*   TEST DUMP END                                                               
*                                                                               
PREC0010 EQU   *                                                                
         CLC   =C'0110',REC10ID    FINAL BUFFER RECORD?                         
         BNE   PREC0020            NO                                           
         BCTR  RF,0                YES - DON'T COUNT IT IN TOTALS               
         ST    RF,TRANSCNT         SAVE REVISED COUNT                           
         EDIT  TRANSCNT,(3,REC10#TR),FILL=0,ZERO=NOBLANK                        
*                                  INSERT RECORD COUNT                          
         EDIT  TOTALAMT,(11,REC10ORD),FILL=0,ZERO=NOBLANK                       
PREC0020 EQU   *                                                                
         CLC   REC01REP+2(2),=C'J1'                                             
         BNE   PREC0030                                                         
         MVC   REC01REP+2(2),=C'UT'                                             
PREC0030 EQU   *                                                                
         CLC   REC01RP(2),=C'J1'                                                
         BNE   PREC0035                                                         
         MVC   REC01RP(2),=C'UT'                                                
PREC0035 EQU   *                                                                
         LA    R2,2                LOOP CONTROL FOR PRINTING                    
         LA    R4,REC01REC         A(OUTPUT RECORD)                             
         MVI   219(R4),C'*'        PUT OUT FINAL NON-SPACE                      
*                                     IN LAST POS OF 2ND RECORD                 
PREC0040 EQU   *                                                                
         MVC   P(110),0(R4)                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,110(R4)          BUMP TO SECOND HALF OF RECORD                
         BCT   R2,PREC0040         DO SECOND HALF OF RECORD                     
PREC0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FILL IN CONTRACT '??' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
DATETIME NMOD1 0,*DTTM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  REREAD CONTRACT                              
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
*                                                                               
*                                                                               
*                                                                               
READEOP  NMOD1 0,*REOP*                                                         
** DON'T USE 'HALF' IN THIS ROUTINE                                             
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    EOPRECS,EOPRECS     CLEAR EOPRECS                                
         LA    R2,EOPRECS          A(EOPRECS)                                   
         MVI   ION,3                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'1B'           GET ADVERTISER                               
         MVC   KEY+15(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+17,1            SET TYPE = BIAS                              
         MVC   KEY+18(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+23(4),RCONKADV  INSERT ADVERTISER                            
         GOTO1 EOPREAD,DMCB,(R2)   READ/ADD CODE TO TABLE                       
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
         MVI   KEY+15,1            SET TYPE = BIAS                              
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
         GOTO1 EOPREAD,DMCB,(R2)   READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1D'           GET OFFICE                                   
         MVC   KEY+17(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+19,1            SET TYPE = BIAS                              
         MVC   KEY+20(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+25(4),RCONKOFF  INSERT OFFICE                                
         GOTO1 EOPREAD,DMCB,(R2)   READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1E'           GET SALESPERSON                              
         MVC   KEY+16(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+18,1            SET TYPE = BIAS                              
         MVC   KEY+19(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+24(4),RCONSAL   INSERT SALESPERSON                           
         GOTO1 EOPREAD,DMCB,(R2)   READ/ADD CODE TO TABLE                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    ROUTINE ACCESSES KEY/RECORD.  IF NOT FOUND, ENTRY IN TABLE                 
*        IS LEFT BLANK.  IF FOUND, EQUIV CODE IS MOVED TO TABLE.                
*                                                                               
EOPREAD  NTR1                                                                   
** DON'T USE 'HALF' IN THIS ROUTINE                                             
         L     R2,0(R1)            RELOAD A(TABLE ENTRY)                        
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   EOPR0200            NO  - EXIT                                   
         MVC   DMCB(4),AIO3        A(IO AREA)                                   
         MVI   UPDATE,C'Y'         READ FOR UPDATE                              
         GOTO1 VGETREC,DMCB                                                     
         L     R4,AIO3                                                          
         USING REOPREC,R4                                                       
         MVC   0(6,R2),REOPEQUV    MOVE CODE TO TABLE                           
         TM    REOPFLAG,X'80'      ALREADY ACTIVE?                              
         BO    EOPR0200            YES - DON'T REWRITE                          
         OI    REOPFLAG,X'80'      NO  - SET ACTIVE FLAG                        
         GOTO1 VPUTREC,DMCB,REOPREC  REWRITE FROM SAME AREA                     
EOPR0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**REPCD>                                                                        
*                                                                               
*   SETREPCD:  SETS CORRESPONDING FIELD IN EACH OF THE SUBRECORDS:              
*        REC0XREP+2(2), IF OVERRIDDEN.                                          
*                                                                               
SETREPCD NMOD1 0,*REPC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   REPALPHA,=C'SZ'     SELTEL?                                      
         BNE   SETR0020            NO                                           
         MVC   REC01REP+2(2),=C'HR'   YES - OVERRIDE VALUE                      
SETR0020 EQU   *                                                                
         CLC   REPALPHA,=C'46'     SELTEL LOCAL?                                
         BNE   SETR0040            NO                                           
         MVC   REC01REP+2(2),=C'HR'   YES - OVERRIDE VALUE                      
SETR0040 EQU   *                                                                
         CLC   REPALPHA,=C'41'     SELTEL LOCAL?                                
         BNE   SETR0060            NO                                           
         MVC   REC01REP+2(2),=C'HR'   YES - OVERRIDE VALUE                      
SETR0060 EQU   *                                                                
         CLC   REPALPHA,=C'PV'     PETRY?                                       
         BNE   SETR0080            NO                                           
         MVC   REC01REP+2(2),=C'PT'   YES - OVERRIDE VALUE                      
SETR0080 EQU   *                                                                
         CLC   REPALPHA,=C'AM'     KATZ?                                        
         BNE   SETR0100            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
SETR0100 EQU   *                                                                
         CLC   REPALPHA,=C'CQ'     KATZ?                                        
         BNE   SETR0120            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
SETR0120 EQU   *                                                                
         CLC   REPALPHA,=C'NK'     KATZ?                                        
         BNE   SETR0140            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
SETR0140 EQU   *                                                                
         CLC   REPALPHA,=C'B3'     EJOR TEST?                                   
         BNE   SETR0160            NO                                           
         MVC   REC01REP+2(2),=C'**'   YES - OVERRIDE VALUE                      
SETR0160 EQU   *                                                                
         CLC   REPALPHA,=C'FN'     FOX NETWORK?                                 
         BNE   SETR0180            NO                                           
         MVC   REC01REP+2(2),=C'NW'   YES - OVERRIDE VALUE                      
SETR0180 EQU   *                                                                
         CLC   REPALPHA,=C'JB'     FOX LOCAL?                                   
         BNE   SETR0200            NO                                           
         MVC   REC01REP+2(2),=C'NW'   YES - OVERRIDE VALUE                      
SETR0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**REPCD>                                                                        
**BLD7>>                                                                        
*                                                                               
*   THIS ROUTINE BUILDS THE 107 RECORD FROM THE MAKEGOODS ENTERED               
*      BY THE CYCLEMKG ROUTINE                                                  
*                                                                               
BLD07REC NMOD1 0,*BLD7*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   REC07REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC07REC+1(REC07LEN-1),REC07REC                                  
         MVC   REC07REC(4),=C'0107'                                             
*                                  REPLACE RECORD ID                            
         MVC   REC03REP(2),=C'RP'                                               
         MVC   REC03REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC02REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  RBUYKLIN,(3,REC03L#),FILL=0,ZERO=NOBLANK                         
         L     R2,AMGTABLE         SET A(MAKEGOOD TABLE)                        
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
*   TEST END                                                                    
**       CLI   RBUYKLIN,34         BUYLINE XX FOUND?                            
**       BNE   *+6                 NO  -                                        
**       DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
BLD70020 EQU   *                                                                
         OC    0(5,R2),0(R2)       ANY ENTRY IN SLOT?                           
         BZ    BLD70040            NO  - FINISHED                               
         LA    R5,1(R5)            YES - INCREMENT COUNTER                      
         LA    R2,5(R2)            BUMP TO NEXT SLOT                            
         B     BLD70020            GO BACK FOR NEXT SLOT                        
BLD70040 EQU   *                                                                
         LTR   R5,R5               ANY MAKEGOOD ENTRIES?                        
         BZ    BLD70800            NO  - FINISHED                               
         MVI   REC07MCN,C'0'       SET 'ALL MISSED IN THIS RECORD'              
         C     R5,=F'17'           MORE THAN ONE RECORD REMAINING?              
         BNH   BLD70060            NO                                           
         MVI   REC07MCN,C'1'       YES - SET 'ANOTHER 107 FOLLOWS'              
BLD70060 EQU   *                                                                
         L     R2,AMGTABLE         SET A(MAKEGOOD TABLE) AGAIN                  
         LA    R6,17               SET MAX LOOP CONTROL                         
         LA    R4,REC07MMS                                                      
BLD70080 EQU   *                                                                
         OC    0(5,R2),0(R2)       ANY ENTRY IN SLOT?                           
         BNZ   BLD70100            NO  - END OF OUTPUT PHASE                    
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                  OUTPUT REC WITH < 17 ENTRIES                 
         B     BLD70800            FINISHED                                     
BLD70100 EQU   *                                                                
         EDIT  (1,3(R2)),(3,0(R4)),FILL=0                                       
         GOTO1 DATCON,DMCB,(3,0(R2)),(X'20',WORK+16)                            
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
**BLD7>>                                                                        
**BLD4>>                                                                        
*                                                                               
*   THIS ROUTINE BUILDS THE '0104' TRANSACTION RECORD                           
*                                                                               
*   NOTE:  THERE MUST BE COMMENTS, MULTIPLE EFFECTIVE DATES, OR                 
*        MG INDICATOR TO PRODUCE A 0104 RECORD                                  
*                                                                               
BLD04REC NMOD1 0,*BLD4*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
         MVI   REC04REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC04REC+1(REC04LEN-1),REC04REC                                  
         MVC   REC04ID,=C'0104'                                                 
         MVC   REC04REP(2),=C'RP'                                               
         MVC   REC04REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC04REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,REC04L#),FILL=0,ZERO=NOBLANK                          
         MVC   REC04SL#,SVSL#      INSERT LINE/SUB LINE #S                      
         MVC   REC04STA,RCONKSTA   INSERT STATION                               
         MVC   REC04IND,=C'03'     EC INDICATOR                                 
         MVC   REC04TYP,=C'02'     LINE COMMENT                                 
*                                  TEST FOR SPECIAL COMMENT                     
         CLI   MULTEFDT,1          HOW MANY EFFECTIVE DATES?                    
         BNH   BLD40040            1 OR LESS                                    
         B     BLD40020                                                         
SPECMSG  DC    CL36'COMMENTS APPLY TO SUB LINES A THRU  '                       
*                   0........1.........2.........3......                        
*                   1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.                        
SUBLINE2 DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         DS    0H                                                               
BLD40020 EQU   *                                                                
         MVC   REC04CM1(36),SPECMSG INSERT SPECIAL MESSAGE                      
         LA    RE,SUBLINE2         ALPHABET LIST (LOCAL)                        
         ZIC   RF,MULTEFDT         NUMBER OF DAYPARTS                           
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         AR    RE,RF                                                            
         MVC   REC04CM1+35(1),0(RE) INSERT 'TO' CHARACTER                       
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD40040 EQU   *                                                                
         TM    COMFLAGS,X'80'      IS BUY A MAKEGOOD?                           
         BNO   BLD40120            NO  - SKIP MG MESSAGES                       
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'06'        LOOK FOR BUY MISSED ELEMENT                  
         BAS   RE,GETEL                                                         
         B     BLD40080                                                         
BLD40060 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT BUY MISSED                          
BLD40080 EQU   *                                                                
         BNE   BLD40120            NOT FOUND - FINISHED                         
         B     BLD40100                                                         
REPLAMSG DC    CL48'THIS IS A REPLACEMENT FOR BUY LINE #   FOR DATE '           
*                   0........1.........2.........3.........4.........           
*                   1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9           
BLD40100 EQU   *                                                                
         MVC   REC04CM1(48),REPLAMSG                                            
         LA    R2,REC04CM1+36      OUTPUT ADDRESS                               
         EDIT  (1,6(R6)),(3,(R2))                                               
*                                  INSERT MISSED SPOT LINE #                    
         GOTO1 DATCON,DMCB,(3,2(R6)),(5,REC04CM1+49)                            
*                                  CONVERT MISSED DATE                          
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         MVI   REC04CM1,C' '       SPACE OUT COMMENT AREA                       
         MVC   REC04CM1+1(139),REC04CM1                                         
         B     BLD40060                                                         
BLD40120 EQU   *                                                                
         SR    R2,R2               SET SWITCH                                   
         TM    COMFLAGS,X'40'      COMMENTS EXIST?                              
         BNO   BLD40280            NO                                           
         MVI   REC04CM1,C' '       SPACE OUT COMMENT AREA                       
         MVC   REC04CM1+1(139),REC04CM1                                         
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROG NAME ELT                       
         BAS   RE,GETEL            GET PROG NAME ELEMENT                        
         BNE   BLD40140            NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         EX    RF,BLD4023C         MOVE PROG NAME TO 1ST COMMENT                
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
BLD40140 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'84'        LOOK FOR ORDER COMMENT                       
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
         LTR   R2,R2               1ST OR SECOND COMMENT?                       
         BNZ   BLD40220            SECOND                                       
         CLI   ELCODE,X'84'        ORDER COMMENT?                               
         BNE   BLD40210            NO                                           
         BCTR  RF,0                YES - SUBTRACT 1 FOR                         
*                                     STATION/REP COMMENT INDICATOR             
         EX    RF,BLD4023A         MOVE 1ST COMMENT (ORDER)                     
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
         B     BLD40180            GO BACK FOR NEXT ELEMENT                     
BLD40210 EQU   *                                                                
         EX    RF,BLD4023C         MOVE 1ST COMMENT (BUY)                       
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
         B     BLD40180            GO BACK FOR NEXT ELEMENT                     
BLD40220 EQU   *                                                                
         CLI   ELCODE,X'84'        ORDER COMMENT?                               
         BNE   BLD40230            NO                                           
         BCTR  RF,0                YES - SUBTRACT 1 FOR                         
*                                     STATION/REP COMMENT INDICATOR             
         EX    RF,BLD4023B         MOVE SECOND COMMENT (ORDER)                  
         B     BLD40235                                                         
BLD40230 EQU   *                                                                
         EX    RF,BLD4023D         MOVE SECOND COMMENT (BUY)                    
BLD40235 EQU   *                                                                
         SR    R2,R2               RESET COMMENT COUNTER                        
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         MVI   REC04CM1,C' '       SPACE OUT COMMENT AREA                       
         MVC   REC04CM1+1(139),REC04CM1                                         
         B     BLD40180            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLD4023A MVC   REC04CM1(0),3(R6)                                                
BLD4023B MVC   REC04CM2(0),3(R6)                                                
BLD4023C MVC   REC04CM1(0),2(R6)                                                
BLD4023D MVC   REC04CM2(0),2(R6)                                                
*                                                                               
BLD40260 EQU   *                                                                
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40280            YES - COMMENTS FINISHED                      
         LA    R6,RBUYREC          NO  - RESET A(BUY RECORD)                    
         MVI   ELCODE,X'04'        GO BACK AND DO THEM                          
         B     BLD40160                                                         
BLD40280 EQU   *                                                                
         LTR   R2,R2               ANYTHING TO PRINT?                           
         BZ    BLD40300            NO                                           
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD40300 EQU   *                                                                
         MVI   REC04CM1,C' '       SPACE OUT COMMENT AREA                       
         MVC   REC04CM1+1(139),REC04CM1                                         
         TM    COMFLAGS,X'20'      TIME IN MINUTES?                             
         BNO   BLD40340            NO                                           
         B     BLD40320                                                         
MINUTMSG DC    CL38'THE ABOVE TIME IS REPORTED IN MINUTES '                     
*                   0........1.........2.........3...........4.......           
*                   1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9           
BLD40320 EQU   *                                                                
         MVC   REC04CM1(38),MINUTMSG                                            
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD40340 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**BLD4>>                                                                        
*                                                                               
*   THIS ROUTINE BUILDS THE '0106' COMPETITIVE RECORD                           
*                                                                               
*                                                                               
         DS    0F                                                               
BLD06REC NMOD1 0,*BL06*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   REC06REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC06REC+1(REC10LEN-1),REC06REC                                  
         MVC   REC06ID,=C'0106'    INSERT ID                                    
         MVC   REC06REP(2),=C'RP'                                               
         MVC   REC06REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC06REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
         XC    FULL,FULL           CLEAR ACCUMULATOR                            
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'06'        ANY SPL ELEMENT?                             
         BAS   RE,GETEL                                                         
         BNE   BLD60800            NO SPL ELEMENT = NO RECORD                   
         TM    RCONSPES-RCONSPEL(R6),X'20'                                      
*                                  REP'D $$ OVERRIDDEN?                         
         BNO   BLD60058                                                         
*                                  FIND 08, PROP $$ TO MARKET $$                
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'08'        FIND 08 ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         MVC   FULL,RCONAC$$-RCONACEL(R6)                                       
*                                  RETRIEVE OVERRIDE $$                         
         L     RF,FULL                                                          
         SR    RE,RE                                                            
         M     RE,=F'100'          INSERT PENNIES FOR ALIGNMENT                 
         ST    RF,FULL                                                          
         B     BLD60120            PROP OVERRIDE TO MARKET $$                   
BLD60058 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R6),X'40'                                      
*                                  REP'D $$ 0, 08 = TOTAL BUDGET?               
         BNO   BLD60060                                                         
*                                  FIND 08, USE $$ AS MKT                       
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'08'        FIND 08 ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         MVC   FULL,RCONAC$$-RCONACEL(R6)                                       
*                                  RETRIEVE TOTAL MARKET $$                     
         L     RF,FULL                                                          
         B     BLD60160            USE DOLLARS AS MARKET $$                     
*                                                                               
BLD60060 EQU   *                                                                
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'03'        GET DOLLARS FOR ORDER                        
         BAS   RE,GETEL                                                         
         BNE   BLD60800            NO DOLLAR ELEMENTS = NO RECORD               
         B     BLD60100                                                         
BLD60080 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLD60100 EQU   *                                                                
         BNE   BLD60120            DOLLAR ELEMENTS FINISHED                     
         USING RCONBKEL,R6                                                      
         L     RF,FULL             ACCUMULATE CONTRACT DOLLARS                  
         ZICM  RE,RCONBKAM,4                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         AR    RF,RE                                                            
         ST    RF,FULL             PUT IT BACK                                  
         B     BLD60080            GO BACK FOR NEXT                             
BLD60120 EQU   *                                                                
         OC    FULL,FULL                                                        
         BZ    BLD60800            NO DOLLARS = NO RECORD                       
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'06'        GET SPL ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   BLD60800            NO SPL ELEMENT = NO RECORD                   
*                                     SHOULDN'T HAVE HAPPENED                   
         USING RCONSPEL,R6                                                      
*                                                                               
         MVC   DUB(4),RCONSPAM     REP'D STATION'S PERCENT                      
*                                                                               
         DROP  R6                                                               
*                                                                               
         SR    RE,RE               CALCULATE MARKET DOLLARS                     
         L     RF,FULL                                                          
         D     RE,=F'100'          DROP PENNIES                                 
         M     RE,=F'10000'        DECIMAL ALIGN FOR DIVISION                   
         SLL   RF,1                DOUBLE FOR ROUNDING                          
         A     RF,DUB              ADD DIVISOR FOR ROUNDING                     
         OC    DUB(4),DUB          VALUE IN DIVISOR?                            
         BZ    BLD60800            NO  - DON'T DIVIDE/NO RECORD                 
         D     RE,DUB              DIVIDE STA $ BY STA %                        
         SRL   RF,1                HALVE AFTER ROUNDING+CALCULATION             
BLD60160 EQU   *                                                                
         ST    RF,DUB              SAVE MARKET DOLLARS FOR STA CALCS            
         EDIT  (RF),(11,REC06MDL),FILL=0,DUB=MYDUB                              
*                                  INSERT INTO RECORD                           
         MVC   REC06STA(4),RCONKSTA INSERT STATION LETTERS (REP)                
         CLC   =C'KGWT',REC06STA   SPECIAL BLAIR SITUATION?                     
         BNE   BLD60180            NO                                           
         MVI   REC06STA+3,C' '     YES - SET TO 'KGW '                          
BLD60180 EQU   *                                                                
         BAS   RE,LOADSTAS         LOAD OTHER STATIONS                          
         MVI   REC06PCT,C' '       INITIALIZE PCTS TO SPACES                    
         MVC   REC06PCT+1(L'REC06PCT-1),REC06PCT                                
         MVI   REC06SDL,C' '       INITIALIZE SDLS TO SPACES                    
         MVC   REC06SDL+1(L'REC06SDL-1),REC06SDL                                
*                                                                               
*                                  GET SPL ELEMENT AGAIN                        
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'06'        GET SPL ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   BLD60800            NO SPL ELEMENT = NO RECORD                   
*                                     SHOULDN'T HAVE HAPPENED                   
         USING RCONSPEL,R6                                                      
*                                                                               
         LA    R5,RCONSPST         A(1ST MINI-ELEMENT)                          
         ZIC   R4,RCONSPNU         NUMBER OF MINI-ELEMENTS                      
*                                                                               
         DROP  R6                                                               
*                                                                               
BLD60200 EQU   *                                                                
         BAS   RE,SETSTA$$         SET STATION ENTRY                            
         LA    R5,9(R5)            BUMP TO NEXT MINI-ELEMENT                    
         BCT   R4,BLD60200         GO BACK FOR NEXT                             
*                                                                               
*                                  RECORD BUILD:  OUTPUT IT                     
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD60800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
LOADSTAS NTR1                                                                   
         LA    R5,REC06STA+4       SET A(1ST COMPETITIVE)                       
         LA    R6,COMPSTAS         SET A(COMPETITIVE STATION LIST)              
LSTA0020 EQU   *                                                                
         OC    0(4,R6),0(R6)       ANY ENTRY?                                   
         BZ    LSTA0400            NO  - FINISHED                               
         MVC   0(4,R5),0(R6)       INSERT STATION CALLS                         
         LA    R5,4(R5)            BUMP TO NEXT ENTRY                           
         LA    R6,4(R6)            BUMP TO NEXT ENTRY                           
         B     LSTA0020            GO BACK FOR NEXT                             
LSTA0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SETSTA$$:  ROUTINE UPDATES REC06PCT/REC06SDL AS FOLLOWS:                    
*        1.  FINDS STATION IN REC06STA, SET UP FROM COMP ELTS                   
*            OF STATION RECORD                                                  
*        2.  STATION COUNT FROM STEP 1 DETERMINES DISPLACEMENT                  
*            INTO OTHER ARRAYS, ZERO RELATIVE                                   
*        3.  STATION $$ AMOUNTS ARE CALC'D FROM PERCENT, TOTAL                  
*            MARKET $$                                                          
*                                                                               
*        R5 -->  MINI-ELEMENT IN X'06' ELEMENT                                  
*                                                                               
SETSTA$$ NTR1                                                                   
         LA    R2,REC06STA         A(REC06 STN LIST)                            
         SR    RF,RF                                                            
         LA    R0,12               SET STATION LOOP                             
SS$$0020 EQU   *                                                                
         CLC   0(4,R2),SPACES      END OF DATA IN TABLE?                        
         BE    SS$$0025            YES                                          
         OC    0(4,R2),0(R2)       NO  - END OF DATA IN TABLE?                  
         BNZ   SS$$0030            NO  - KEEP TESTING                           
*                                  YES - STATION NOT FOUND IN LIST:             
SS$$0025 EQU   *                                                                
         MVC   0(4,R2),0(R5)          INSERT STATION INTO SLOT                  
         B     SS$$0040                                                         
SS$$0030 EQU   *                                                                
         LA    RF,1(RF)            INCREMENT COUNTER                            
         CLC   0(4,R5),0(R2)       MINI STN VS REC06 LIST                       
         BE    SS$$0040            FOUND                                        
         LA    R2,4(R2)            BUMP TO NEXT STN IN LIST                     
         BCT   R0,SS$$0020         GO BACK FOR NEXT                             
*                                  TABLE FULL:  NO ROOM FOR                     
*                                     UNKNOWN STATION                           
         DC    H'0'                NOT FOUND:  PROBLEM IN STN RECORD            
SS$$0040 EQU   *                                                                
         BCTR  RF,0                MAKE COUNT ZERO RELATIVE                     
         LR    R2,RF               SAVE VALUE OF RF                             
         MH    RF,=H'5'            SET DISPLACEMENT FOR PCT                     
         LA    R1,REC06PCT(RF)     SET A(PCT ARRAY)+DISPLACEMENT                
         MVC   DUB+4(4),5(R5)      SET STATION PERCENT                          
         EDIT  (4,DUB+4),(5,(R1)),FILL=0,DUB=MYDUB                              
*                                  INSERT STATION PERCENT INTO ARRAY            
*                                                                               
*   NOTE:  NEW REC06 HAS NO STATION DOLLAR AMOUNTS.  FOLLOWING                  
*        CODE, WHICH SETS SAME, HAS BEEN COMMENTED OUT.                         
*                                                                               
**       SR    RE,RE                                                            
**       L     RF,DUB              INSERT MARKET DOLLAR VALUE                   
**       M     RE,DUB+4            MULTIPLY BY STATION PERCENT                  
**       A     RF,=F'5000'         ADD FOR WHOLE-DOLLAR ROUND                   
**       D     RE,=F'10000'        DECIMAL ALIGN VALUE                          
**       ST    RF,FULL                                                          
**       LR    RF,R2               RESET DISPLACEMENT COUNT                     
**       MH    RF,=H'11'           FIGURE DISPLACEMENT                          
**       LA    R1,REC06SDL(RF)     SET A(SDL ARRAY)+DISPLACEMENT                
**       EDIT  FULL,(11,(R1)),FILL=0,DUB=MYDUB                                  
SS$$0080 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE '0108' EASI RECORD                                  
*                                                                               
*                                                                               
         DS    0F                                                               
BLD08REC NMOD1 0,*BL08*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   REC08REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC08REC+1(REC08LEN-1),REC08REC                                  
         MVC   REC08ID,=C'0108'    INSERT ID                                    
         MVC   REC08REP(2),=C'RP'                                               
         MVC   REC08REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,REC08REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
         XC    FULL,FULL           CLEAR ACCUMULATOR                            
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'A2'        FIND A2 ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   BLD80800            NO A2 ELEMENT = NO RECORD                    
         MVC   REC08EIX(4),RCONIADV-RCONIEL(R6)                                 
*                                  INSERT ADVERTISER CODE                       
         MVC   REC08PR1(4),RCONIPRD-RCONIEL(R6)                                 
         MVC   REC08PR2(4),RCONIPR2-RCONIEL(R6)                                 
*                                  INSERT PRODUCT 1  CODE                       
         MVC   REC08EST(10),RCONXEST-RCONIEL(R6)                                
*                                  INSERT ESTIMATE CODE                         
         OC    REC08EIX(18),SPACES SPACE FILL                                   
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
BLD80800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE '0110' TRANSACTION RECORD                           
*                                                                               
*                                                                               
BLD10REC NMOD1 0,*B10R*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   REC10REC,C' '       SPACE FILL THE RECORD                        
         MVC   REC10REC+1(REC10LEN-1),REC10REC                                  
         MVC   REC10ID,=C'0110'    INSERT ID                                    
         MVC   REC10REP(2),=C'RP'                                               
         MVC   REC10REP+2(2),REPALPHA                                           
         GOTO1 =A(SETREPCD),DMCB,(RC),RR=Y                                      
***>>>   BAS   RE,STREPCD2                                                      
         MVC   REC10TYP(2),=C'02'  SET PROGRAM TYPE                             
         MVC   REC10STA,RCONKSTA   INSERT STATION                               
         MVC   REC10STA+4(6),FOXZEROS                                           
         MVC   REC10IND,=C'03'     EC INDICATOR                                 
         MVC   REC10IND+6(4),FOXZEROS                                           
         MVC   REC10CHG(5),=C'51REL'                                            
*                                  SET CHG + REL                                
         GOTO1 HEXOUT,DMCB,RCONKCON,REC10REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
*                                                                               
         GOTO1 =A(PRINTREC),RR=Y                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STREPCD2:  SETS CORRESPONDING FIELD IN EACH OF THE SUBRECORDS:              
*        REC0XREP+2(2), IF OVERRIDDEN.                                          
*                                                                               
STREPCD2 NTR1                                                                   
         CLC   REPALPHA,=C'PV'     PETRY?                                       
         BNE   STR2001A            NO                                           
         MVC   REC01REP+2(2),=C'PT'   YES - OVERRIDE VALUE                      
STR2001A EQU   *                                                                
         CLC   REPALPHA,=C'AM'     KATZ?                                        
         BNE   STR2A01A            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
STR2A01A EQU   *                                                                
         CLC   REPALPHA,=C'CQ'     KATZ?                                        
         BNE   STR2B01A            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
STR2B01A EQU   *                                                                
         CLC   REPALPHA,=C'NK'     KATZ?                                        
         BNE   STR2C01A            NO                                           
         MVC   REC01REP+2(2),=C'KZ'   YES - OVERRIDE VALUE                      
STR2C01A EQU   *                                                                
         CLC   REPALPHA,=C'B3'     EJOR TEST?                                   
         BNE   STR2D01A            NO                                           
         MVC   REC01REP+2(2),=C'**'   YES - OVERRIDE VALUE                      
STR2D01A EQU   *                                                                
         CLC   REPALPHA,=C'FN'     FOX NETWORK?                                 
         BNE   STR2E01A            NO                                           
         MVC   REC01REP+2(2),=C'NW'   YES - OVERRIDE VALUE                      
STR2E01A EQU   *                                                                
         CLC   REPALPHA,=C'JB'     FOX LOCAL?                                   
         BNE   STR2F01A            NO                                           
         MVC   REC01REP+2(2),=C'NW'   YES - OVERRIDE VALUE                      
STR2F01A EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*- GETSTA -- READ STATION RECORD INTO IO3.                                      
*********************************************************************           
GETSTA   NMOD1 0,*GSTA*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(STATION FROM CONTRACT REC)             
GSTA05   DS    0H                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R2)      INSERT STATION                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSTA10                                                           
         DC    H'0',C'MISSING STA REC'                                          
GSTA10   GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                                                               
*- PICK UP RECORD DATA                                                          
*                                                                               
*    NEW DATA TO DEFINE THE OUTPUT FOR ELECTRONIC CONTRACTING WILL              
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
         MVC   HALF(1),RSTAOPT9    USING HALF FOR TEMP STORAGE OF OPT           
         DROP  R6                                                               
*                                                                               
GSTA55   EQU   *                                                                
         LA    R4,COMPSTAS         SET A(COMPETITIVE STN LIST)                  
         XC    COMPSTAS,COMPSTAS   CLEAR AREA                                   
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'02'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL            RETRIEVE FIRST ELEMENT                       
         B     GSTA0070                                                         
GSTA0060 EQU   *                                                                
         BAS   RE,NEXTEL           RETRIEVE NEXT ELEMENT(S)                     
GSTA0070 EQU   *                                                                
         BNE   GSTA0080            NOT THERE                                    
*                                                                               
         USING RSTAMKEL,R6                                                      
*                                                                               
         MVC   0(4,R4),RSTAMKST    MOVE STATION CALL LETTERS                    
         LA    R4,4(R4)            BUMP TO NEXT SLOT                            
         B     GSTA0060            GO BACK FOR NEXT                             
GSTA0080 EQU   *                                                                
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
         LA    R6,RSTAREC          STATION RECORD                               
*                                                                               
         MVC   SENDID,TWAUSRID                                                  
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
*                                  ALWAYS RETURN A ZERO CC                      
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
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
*     CLASS                                                                     
*                                                                               
         MVI   PLCLASS,C'G'        CLASS 'G'                                    
***>>>   MVI   PLCLASS,C' '        CLASS 'SPACE/BLANK'                          
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
VPQ50    GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ALTWKCHK:  FOR BUYLINES WHICH ARE ALTERNATING WEEKS, BIAS CANNOT            
*      ACCEPT A FLIGHT DATE WHERE THE END WEEK IS AN INACTIVE WEEK.             
*      ADJUST THE FINAL DATE IF INACTIVE.  THIS IS DONE BY USING                
*      'PERVERT' TO DETERMINE THE NUMBER OF WEEKS IN THE BUY'S                  
*      FLIGHT.  IF NUMBER IS EVEN, BUY END DATE MUST BE BACKED UP               
*      1 WEEK.                                                                  
*                                                                               
ALTWKCHK NMOD1 0,*ALTW*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   REVERSE THE ORDER OF DATA IN REC03 TO YYMMDD FROM MMDDYY                    
*                                                                               
         MVC   FRSTDATE(2),REC03BBD+4                                           
         MVC   FRSTDATE+2(4),REC03BBD                                           
         MVC   SECDATE(2),REC03BED+4                                            
         MVC   SECDATE+2(4),REC03BED                                            
*                                                                               
*   CALCULATE SPREAD FROM FIRST TO SECOND DATES                                 
*                                                                               
         GOTO1 PERVERT,DMCB,FRSTDATE,SECDATE                                    
*                                                                               
         ZICM  RF,12(R1),2         QUOTIENT OF DAYS/7 (#WEEKS)                  
         ZICM  RE,10(R1),2         REMAINDER OF DAYS/7                          
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    AWKC0020            NO  - EVEN WEEK BREAK                        
         LA    RF,1(RF)            YES - ADD 1 TO #WEEKS                        
AWKC0020 EQU   *                                                                
         SLL   RF,31               SHIFT OUT ALL BUT LOW ORDER BIT              
         SRL   RF,31               SHIFT IT BACK                                
         LTR   RF,RF               ANY VALUE (LOW ORDER BIT SET?)               
         BNZ   AWKC0040            YES - ODD # WEEKS IS OKAY                    
         LA    RF,7                NO  - EVEN NUMBER OF WEEKS                   
         LNR   RF,RF               NEGATE THE 7 DAYS                            
*                                                                               
*   SUBTRACT 1 WEEK FROM BUY END DATE TO LAST ACTIVE WEEK                       
*                                                                               
         GOTO1 ADDAY,DMCB,SECDATE,FRSTDATE,(RF)                                 
         GOTO1 DATCON,DMCB,FRSTDATE,(X'20',FRSTDATE)                            
         MVC   REC03BED+4(2),FRSTDATE                                           
         MVC   REC03BED(4),FRSTDATE+2                                           
AWKC0040 EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
* INCLUDE CTGENFILE                                                             
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
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
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
       ++INCLUDE REMERG03                                                       
       ++INCLUDE REMERG16                                                       
       ++INCLUDE RECYCLMG                                                       
         EJECT                                                                  
REC110$$ NMOD1 0,*REC110*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RBUYREC          SET A(BUY RECORD)                            
         MVI   ELCODE,X'03'        GET EFFECTIVE DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         B     REC$0040                                                         
REC$0020 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
REC$0040 EQU   *                                                                
         BNE   REC$0100            NOT FOUND - EXIT ROUTINE                     
         USING RBUYDTEL,R6                                                      
         MVC   STARTDTE,RBUYDTST   SAVE START DATE                              
         SR    RE,RE                                                            
         ZIC   RF,RBUYDTWK         # WEEKS                                      
         ZIC   R1,RBUYDTNW         # SPOTS / WEEK                               
         MR    RE,R1               # WEEKS X SPOTS/WK = TOTAL SPOTS             
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         CLC   RBUYKPLN,=X'FFFFFF'                                              
*                                  PLAN DEFAULT?                                
         BE    REC$0060            YES                                          
*                                                                               
*   PLAN DOLLARS NOT DONE HERE:  DONE IN ORIGINAL CYCLE                         
*                                                                               
***      BAS   RE,ADDPLAN$         NO  - ADD PLAN $$, IF NEEDED                 
         B     REC$0100                                                         
REC$0060 EQU   *                                                                
*                                                                               
*   TEST DUMP                                                                   
***      CLI   RBUYKLIN,13         BUYLINE 4?                                   
***      BNE   *+6                                                              
***      DC    H'0'                YES = TEST DUMP                              
*   TEST END                                                                    
*                                                                               
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         B     REC$0020            GO BACK FOR NEXT ELEMENT                     
REC$0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
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
                                                                                
         MVC   P+9(11),=C'EDICT=*BIAS'                                          
***>>>   MVC   P+9(6),=C'EDICT='                                                
*                                                                               
*        XC    KEY,KEY                                                          
*        LA    R6,KEY                                                           
*        USING CTIREC,R6                                                        
*        MVI   CTIKTYP,CTIKTYPQ                                                 
*        MVC   CTIKNUM,TWAUSRID                                                 
*        DROP  R6                                                               
*                                                                               
*        GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
*        L     R6,AIO2                                                          
*        CLC   KEY(L'CTIKEY),0(R6)   IT ISN'T A GENCON READ HIGH                
*        BE    *+6                                                              
*        DC    H'0'                MUST BE THERE!                               
*                                                                               
*        LA    R6,28(R6)           OFFSET TO FIRST ELEMENT                      
*                                                                               
EDICT03  DS    0H                                                               
*        CLI   0(R6),0             NOT FOUND, EXIT                              
*        BNE   *+6                                                              
*        DC    H'0'                MUST BE THERE!                               
*                                                                               
*        CLI   0(R6),CTDSCELQ                                                   
*        BE    EDICT05                                                          
*        ZIC   RF,1(R6)                                                         
*        LTR   RF,RF                                                            
*        BNZ   *+6                                                              
*        DC    H'0'                MUST BE THERE!                               
*                                                                               
*        AR    R6,RF                                                            
*        B     EDICT03                                                          
*                                                                               
EDICT05  DS    0H                                                               
*        USING CTDSCD,R6                                                        
*        CLI   CTDSCLEN,3          MUST BE AT LEAST 3 CHAR LONG                 
*        BL    EDICTX                                                           
*        ZIC   R1,CTDSCLEN                                                      
*        SH    R1,=H'3'                                                         
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   P+15(0),CTDSC                                                    
*        DROP  R6                                                               
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
         DROP  R5,R6                                                            
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REDAYUNPK                                                      
         LTORG                                                                  
         EJECT                                                                  
****>>>>                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018RECNT77   10/02/03'                                      
         END                                                                    
