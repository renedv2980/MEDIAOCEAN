*          DATA SET RECNT65    AT LEVEL 181 AS OF 10/02/03                      
*PHASE T80265A,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T80265 - RECNT65 - JDS/2000 EC'                                 
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT65 --- JDS/2000 ELECTRONIC CONTRACT INTERFACE         *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG17/93 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* DEC01/93 (BU ) --- SET EOP ACTIVE FLAGS WHEN ORDER SENT           *           
*                                                                   *           
* APR01/94 (BU ) --- TIME-STAMP:  ADJUST HOURS...                   *           
*                                                                   *           
* SEP22/94 (BU ) --- MAKE ADJUSTMENTS PER MARILYN PAIGE.            *           
*                                                                   *           
* OCT26/94 (BU ) --- MAKE ADJUSTS PER M. PAIGE: FAX DATED 10/25.    *           
*                                                                   *           
* DEC12/94 (BU ) --- FIX LENGTH EDIT PROBLEMS PER M. PAIGE: FAX     *           
*                    DATED 12/8, RECEIVED 8AM 12/12/.               *           
*                                                                   *           
* DEC21/94 (BU ) --- FIX LENGTH EDIT PROBLEM: 90 MIN OR GREATER->   *           
*                    HOURS/MINS, NOT GREATER 90 MINS.  ERROR BY     *           
*                    M. PAIGE.                                      *           
*                                                                   *           
* JUN07/95 (BU ) --- FIX LENGTH EDIT AGAIN:  60 MIN OR GREATER ->   *           
*                                                                   *           
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                             *           
*                                                                   *           
* 19APR96 (WSB) --- DON'T CHECK EOP CODES IF STA OPT BYTE IS NO     *           
*                                                                   *           
* 29MAY96 (SKU) --- EXPAND EST NUM                                  *           
*                                                                   *           
* 26JUL96 (SKU) --- CHANGE TO USE THMS FOR TIME STAMPING            *           
*                                                                   *           
* 08OCT96 (SKU) --- LOW POWER STATION                               *           
*                                                                   *           
* 09JUL97 (BU ) --- UPGRADE FOR YR-2000                             *           
*                                                                   *           
* 26JUL99 (BU ) --- SKIP CANCELLED BUYLINES                         *           
*                                                                   *           
* 11AUG00 (BU ) --- SET 'LOCAL' ACCORDING TO NEW MODELS             *           
*                                                                   *           
* APR05/01 (BU ) --- TRADE FLAG SETTING                             *           
*                                                                   *           
* APR18/01 (BU ) --- TRADE FLAG SETTING ADJUSTMENT                  *           
*                                                                   *           
* MAY04/01 (BU ) --- PROGRAM NAME ADJUSTMENTS TO COMMENT            *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80265   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T80265*,R9,RR=R8                              
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
         BAS   RE,INIT             SET INITAL ADDRS AND VALUES                  
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
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         '0201' HEADER                                
*                                                                               
MN300    EQU   *                                                                
         BAS   RE,BLD02REC         '0202' ORDER HEADER COMMENTS                 
*                                                                               
MN400    EQU   *                                                                
         BAS   RE,BLD03REC         '0203' LINE ADD                              
*                                                                               
MN500    EQU   *                                                                
         BAS   RE,BLD10REC         '0210' ORDER ADD FINAL BUFFER                
*                                                                               
MN600    EQU   *                                                                
         BAS   RE,DATETIME         DATE AND TIME STAMP                          
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
         MVI   RC201REC,C' '       SPACE FILL THE RECORD                        
         MVC   RC201REC+1(RC201LEN-1),RC201REC                                  
*                                                                               
*    SHOULD NOT BE A PROBLEM WITH NATIONAL VS LOCAL REP.....                    
*                                                                               
         MVC   RC201ID,=C'0201'    LOAD RECORD TYPE                             
         MVC   RC201STA(5),RCONKSTA INSERT STATION+MEDIA                        
         CLI   RCONKSTA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC201STA+4(2),=C'TV'                                             
         MVC   RC201STA+6(1),RCONKGRP                                           
*                                  INSERT 1ST CHAR OF GROUP                     
*                                                                               
*   NOTE:  DDS NATIONAL/LOCAL NOT SAME AS JDS.  SAME CODE PLACED                
*        IN BOTH FIELDS.                                                        
*                                                                               
         MVC   RC201LRP,REPALPHA   FILL LOCAL WITH NATL REP                     
         MVC   RC201NRP,REPALPHA   FILL NAT'L WITH NATL REP                     
         GOTO1 HEXOUT,DMCB,RCONKCON,RC201REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         MVI   RC201TCD,C'A'       'ADD' INDICATOR                              
*                                                                               
*   TEST FOR EOP RECORDS FOUND.  IF NOT FOUND, CODE IS LEFT EMPTY.              
*        IF FOUND, CODE IS INSERTED.  IN ALL CASES, THE DDS NAME                
*        IS INSERTED WHERE CALLED FOR.                                          
*                                                                               
         OC    EOPAGY,EOPAGY       ANY VALUE IN AGENCY?                         
         BZ    BLD10020            NO  - SKIP LOADING CODE                      
         MVC   RC201AGY,EOPAGY     YES - LOAD AGENCY NAME                       
BLD10020 EQU   *                                                                
         MVC   RC201AGN,AGYNAME    LOAD AGENCY NAME FROM SUPPORT                
*                                                                               
         OC    EOPAGY,EOPAGY       ANY VALUE IN ADVERTISER?                     
         BZ    BLD10040            NO  - SKIP LOADING CODE                      
         MVC   RC201ADV,EOPADV     YES - LOAD ADVERT CODE                       
BLD10040 EQU   *                                                                
         MVC   RC201ANM,ADVNAME    LOAD ADVERT NAME FROM SUPPORT                
*                                                                               
         OC    EOPOFF,EOPOFF       ANY VALUE IN OFFICE?                         
         BZ    BLD10060            NO  - SKIP LOADING CODE                      
         MVC   RC201OFF,EOPOFF     YES - LOAD OFFICE CODE                       
BLD10060 EQU   *                                                                
*                                                                               
         OC    EOPSAL,EOPSAL       ANY VALUE IN S/P?                            
         BZ    BLD10080            NO  - SKIP LOADING CODE                      
         MVC   RC201SLS,EOPSAL     YES - LOAD S/P    CODE                       
BLD10080 EQU   *                                                                
*                                                                               
         MVC   RC201PRD,PRODNAME   LOAD PRODUCT NAME FROM SUPPORT               
         MVC   RC201BYR(20),RCONBUYR                                            
*                                  LOAD BUYER NAME FROM CON REC                 
*                                                                               
*                                  SET UP ESTIMATE FIELD                        
*                                  EST FIELD ALREADY SET TO SPACES              
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'A2'        FIND EASI ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BLD10090            NOT FOUND - SKIP IT                          
         USING RCONIEL,R6                                                       
         MVC   RC201EST,RCONXEST   INSERT ESTIMATE NUMBER                       
         OC    RCONXEST,RCONXEST                                                
         BNZ   *+10                                                             
         MVC   RC201EST(4),RCONIEST INSERT ESTIMATE NUMBER                      
         MVC   RC201EIX(4),RCONIADV                                             
*                                  INSERT ADVERTISER CODE                       
         MVC   RC201EIX+8(4),RCONIPRD                                           
*                                  INSERT FIRST PRODUCT CODE                    
         MVC   RC201EIX+16(4),RCONIPR2                                          
*                                  INSERT SECOND PRODUCT CODE                   
         OC    RC201EIX(24),SPACES                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
BLD10090 EQU   *                                                                
*                                                                               
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'13'        FIND EC CONFLICT CODE ELEMENT                
         BAS   RE,GETEL                                                         
         BE    BLD10100            FOUND                                        
         B     BLD10119            NOT FOUND: SKIP                              
*****>>  DC    H'0'                NOT FOUND - MUST BE THERE                    
*                                                                               
*    SHOULD THIS BE THIS WAY?  DO WE WANT TO ABORT?  SHOULD THE                 
*        EC ACTION CHECK FOR THE EXISTENCE OF DX/CX, AND REQUIRE                
*        IT BE ENTERED BEFORE ACTION IS ALLOWED?                                
*                                                                               
BLD10100 EQU   *                                                                
*                                                                               
*   LOAD FIELDS FROM EC CONFLICT ELEMENT.  FIELDS IN ELEMENT                    
*        HAVE BEEN INITIALIZED TO SPACE, SO THAT IT IS NOT                      
*        NECESSARY TO CHECK WHETHER STATION HAS A PROFILE WHICH                 
*        INCLUDES THE FIELD.                                                    
*                                                                               
         USING RCONCJEL,R6                                                      
         MVC   RC201RTE,RCONCJRC   INSERT RATE CARD                             
         EDIT  RCONCJBC,(6,RC201BLC),FILL=0,ZERO=BLANK                          
         MVC   RC201CYC,RCONCJCY   INSERT BILLING CYCLE                         
         MVC   RC201REV,RCONCJRV   INSERT REVENUE CATEGORY                      
*                                                                               
         OC    RC201TRD,SPACES     CLEAR TO SPACES                              
         MVC   RC201PC1,RCONCJP1   INSERT PROD CAT 1                            
         MVC   RC201PC2,RCONCJP2   INSERT PROD CAT 2                            
         MVC   RC201COP,RCONCJCT   INSERT COOP TYPE                             
         BAS   RE,SETTRADE         CHECK/SET TRADE FLAG IF NEEDED               
         CLI   RCONCJTF,C'X'       DX/CX TRADE FLAG SET?                        
         BNE   BLD10105            NO  - DON'T RESET TRADE VALUE                
         MVC   RC201TRD,RCONCJTF   YES - INSERT TRADE FLAG                      
BLD10105 EQU   *                                                                
         CLC   =C'BRK',RCONCJCS    'BREAK' SEPARATION?                          
         BNE   BLD10110            NO                                           
         MVC   RC201SEP,RCONCJCS   YES - INSERT 'BRK' SEPARATION                
         B     BLD10115                                                         
BLD10110 EQU   *                                                                
         OC    RCONCJCS,RCONCJCS   ANY SEPARATION VALUE?                        
         BZ    BLD10115            NO  - ALREADY SET TO SPACES                  
         NI    RCONCJCS,X'FF'-X'80'                                             
*                                  TURN OFF DISPLAY FLAG                        
         EDIT  RCONCJCS,(3,RC201SEP),FILL=0                                     
BLD10115 EQU   *                                                                
         MVC   RC201SPC,RCONCJSH   INSERT SPECIAL HANDLING                      
         MVC   RC201INC,RCONCJIC   INSERT INVOICE COMMENTS                      
         MVC   RC201INM,RCONCJIM   INSERT INVOICE MESSAGE CODE                  
         MVC   RC201TAX,RCONCJST   INSERT SALES TAX CODE                        
         MVC   RC201NRL,RCONCJNR   INSERT NAT/LOC CODE                          
         CLI   RC201NRL,X'00'      ANY VALUE THERE?                             
         BNE   BLD10117            MAYBE                                        
         MVI   RC201NRL,C'N'       NO  - DEFAULT TO 'N'                         
         B     BLD10119                                                         
BLD10117 EQU   *                                                                
         CLI   RC201NRL,C' '       ANY VALUE THERE?                             
         BNE   BLD10119            YES                                          
         MVI   RC201NRL,C'N'       NO  - DEFAULT TO 'N'                         
*                                                                               
         DROP  R6                                                               
*                                                                               
BLD10119 EQU   *                                                                
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         USING RCONRFEL,R6                                                      
         MVI   ELCODE,X'1E'        FIND RANDOM FLAG ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   BLD10120            FOUND                                        
         TM    RCONRF1,X'20'       LOCAL ORDER?                                 
         BNO   BLD10120            NO                                           
         MVC   RC201NRL,=C'L'      YES                                          
         DROP  R6                                                               
BLD10120 EQU   *                                                                
         MVI   RC201CMM,C'X'       AGENCY COMMISSION: X= 15%                    
         MVI   RC201STD,C'S'       STANDARD CALENDAR                            
         MVC   RC201LOG,=C'CM'     LOG TYPE                                     
         MVC   RC201ORD,=C'DDS'    'DDS' = REPPAK (DDS)                         
         LA    R6,RCONREC          A(CONTRACT RECORD)                           
         MVI   ELCODE,X'15'        LOOK FOR EC CONTROL ELEMENT                  
         BAS   RE,GETEL            LOOK FOR IT                                  
         BE    BLD10130            FOUND                                        
         MVC   RC201E2#,FOXZEROS   NOT FOUND:  SET TO ZERO                      
         B     BLD10140                                                         
BLD10130 EQU   *                                                                
         USING RCONECEL,R6                                                      
         EDIT  RCONECCT,(2,RC201E2#),FILL=0,ZERO=NOBLANK                        
*                                                                               
         DROP  R6                                                               
*                                                                               
BLD10140 EQU   *                                                                
*                                                                               
         PRINT GEN                                                              
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
SETTRADE NTR1                                                                   
         LA    R6,RCONREC                                                       
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'1E'        RETRIEVE SAR ELEMENT                         
         BAS   RE,GETEL            NONE FOUND - SKIP DEMO                       
         BNE   SETT0020                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BNO   SETT0020            NO                                           
         MVI   RC201TRD,C'X'       INSERT TRADE FLAG                            
         DROP  R6                                                               
SETT0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         PRINT NOGEN                                                            
         EJECT                                                                  
*********************************************************************           
*        BLD02REC  ---  BUILDS THE '0202' TRANSACTION RECORD                    
*********************************************************************           
BLD02REC NTR1                                                                   
         MVI   RC202REC,C' '       SPACE FILL THE RECORD                        
         MVC   RC202REC+1(RC202LEN-1),RC202REC                                  
         MVC   RC202ID,=C'0202'    LOAD RECORD TYPE                             
         GOTO1 HEXOUT,DMCB,RCONKCON,RC202REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         MVC   RC202STA(5),RCONKSTA   INSERT STATION                            
         CLI   RCONKSTA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC202STA+4(2),=C'TV'                                             
         MVC   RC202STA+6(1),RCONKGRP                                           
*                                  INSERT FIRST CHAR OF GROUP                   
         MVC   RC202SEQ(4),=C'0010'                                             
*                                  INSERT 001 IN SEQ, 0 IN CMT                  
         MVI   RC202TCD,C'A'       SET CODE TO 'ADD'                            
*                                                                               
*    FILL IN UP TO TWO COMMENTS.                                                
*    NO RECORD SENT IF THERE ARE NO COMMENTS AT ALL.                            
*                                                                               
         SR    R2,R2               SET TEST COUNTER                             
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'02'        CONTRACT COMMENT ELEMENT                     
         BAS   RE,GETEL            NONE FOUND - FINISHED                        
         BNE   BLD20140            EXIT WITH NO WRITES                          
         B     BLD20060                                                         
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
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         B     BLD20140            FINISHED - EXIT ROUTINE                      
BLD20090 MVC   RC202CM1(0),2(R6)   LOAD FIRST  COMMENT                          
BLD20091 MVC   RC202CM2(0),2(R6)   LOAD SECOND COMMENT                          
*                                                                               
BLD20120 EQU   *                                                                
         LTR   R2,R2               ANY COMMENT TO PRINT?                        
         BZ    BLD20140            NO  - FINISHED                               
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
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
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    COLLAPS2,COLLAPS2   USED IN COLLAPSE BUY ROUTINE                 
         XC    TESTCTR,TESTCTR     **TEST                                       
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   RC203REC,C' '       SPACE FILL RECORD                            
         MVC   RC203REC+1(RC203LEN-1),RC203REC                                  
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30500            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       BUYLINE CANCELLED?                           
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     BUYLINE CANCELLED?                           
         BE    BLD30020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    BLD30020            YES - SKIP IT                                
BLD30040 EQU   *                                                                
         MVC   SVBYLN#,RBUYKLIN    SAVE BUY LINE #                              
         GOTO1 BLD3CTCM,DMCB,(RC)  SET MG + COMMENT FLAGS                       
         MVC   RC203ID,=C'0203'    INSERT RECORD ID                             
         MVC   RC203STA(5),RCONKSTA                                             
         CLI   RC203STA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC203STA+4(2),=C'TV'                                             
         MVC   RC203STA+6(1),RCONKGRP                                           
*                                  INSERT FIRST CHAR OF GROUP                   
         GOTO1 HEXOUT,DMCB,RCONKCON,RC202REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,RC203BY1),FILL=0,ZERO=NOBLANK                         
         MVI   RC203TCD,C'A'       'ADD' CODE                                   
*                                                                               
*   NEW TRAFFIC ELEMENTS:  NOT PRESENT WITHIN DDS.....                          
*                                                                               
****>    GOTO1 STRTEND,DMCB,(RC)                                                
*                                                                               
         GOTO1 PRTY,DMCB,(RC)                                                   
*                                                                               
         GOTO1 PCDISP,DMCB,(RC)                                                 
*                                                                               
         GOTO1 PLNDISP,DMCB,(RC)                                                
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
*        IF THERE IS NEED TO CREATE MULTIPLE 203 LINES BECAUSE OF               
*        9 SPOTS PER DAY LIMIT, A LOOP WITHIN THE MULT EFF DATE                 
*        LOOP WILL SEND THE EXTRA 203 RECORDS                                   
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
         BZ    BLD30420            NO  - DON'T PROCESS BUY                      
         GOTO1 SPTINFO,DMCB,(RC),(R6)                                           
*                                                                               
         MVC   RC203ACT(2),FOXZEROS                                             
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   RC203ACT,C'1'       YES                                          
         MVI   RC203INA,C'1'       SET INACTIVE WEEKS TO 01                     
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
         MVI   RC203BY2,C' '       YES - SPACE OUT SUBLINE                      
         B     BLD30300                                                         
BLD30280 EQU   *                                                                
         STC   RF,RC203BY2         INSERT 'SUBLINE' FROM SUBLINES               
BLD30300 EQU   *                                                                
         MVC   SVSL#,RC203BY2      SAVE SUBLINE                                 
         SR    R0,R0                                                            
         ZIC   R1,SPOTSWK          LOAD # SPOTS/WEEK                            
         D     R0,DAYCNT           SPTS/WK DIV BY # DAYS                        
         CH    R1,=H'9'            RESULT < 9?                                  
         BL    BLD30320            YES                                          
         CH    R1,=H'10'           RESULT 10 OR GREATER?                        
         BNL   BLD30340            YES                                          
         LTR   R0,R0               RESULT = 9: CHECK REMAINDER                  
         BNZ   BLD30340                                                         
BLD30320 EQU   *                                                                
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
         CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
         BE    BLD30420            NO                                           
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
*                                     WILL BE ADDED AFTER 1ST BUYLINE           
*                                        ONLY, BASED ON COMFLAGS SWITCH         
         B     BLD30420                                                         
BLD30340 EQU   *                                                                
         LA    R1,9                MAX # SPOTS/DAY                              
         SR    R0,R0                                                            
         M     R0,DAYCNT           MAX SPOTS X # DAYS =                         
*                                     MAX PER 203 LINE                          
         ST    R1,SPOTMAX          SAVE MAX PER 203 LINE                        
         ST    R1,SPOTMAX2                                                      
         XC    SPOTLEFT,SPOTLEFT                                                
         MVC   SPOTLEFT+3(1),SPOTSWK                                            
*                                  NUMBER SPOTS PER WEEK                        
BLD30360 EQU   *                                                                
         L     RE,SPOTMAX          LOAD MAX NUM SPOTS (9)                       
         L     RF,SPOTLEFT         SUBTRACT 9 FROM REMAINING                    
         SR    RF,RE               SUBTRACT MAX FROM TOTAL                      
         ST    RF,SPOTLEFT         PUT IT BACK                                  
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
         MVC   RC203CAL,SPACES                                                  
         LA    R4,RC203CAL         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTMAX2,(1,0(R4))                                               
BLD30400 EQU   *                                                                
         EDIT  SPOTMAX2,(3,RC203SPT),FILL=0,ZERO=NOBLANK                        
         LA    R2,SUBLINES                                                      
         ZIC   RF,MULTEFDT         CURRENT NUMBER OF EFF DATES                  
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         AR    R2,RF               DISPLACE TO SUBLINE #                        
         MVC   RC203BY2,0(R2)      INSERT SUBLINE #                             
         MVC   SVSL#,0(R2)         SAVE SUB LINE #                              
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
         BE    BLD30410            NO                                           
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
*                                     WILL BE ADDED AFTER 1ST BUYLINE           
*                                        ONLY, BASED ON COMFLAGS SWITCH         
BLD30410 EQU   *                                                                
         CLC   SPOTLEFT+2(2),=H'1' ANY SPOTS LEFT?                              
         BL    BLD30420            NO  - FINISHED                               
         ZIC   RF,MULTEFDT         ADD TO NUMBER OF EFF DATES                   
         LA    RF,1(RF)                                                         
         STC   RF,MULTEFDT         SAVE IT                                      
         CLC   SPOTLEFT,SPOTMAX    MORE THAN 1 203 REC TO GO?                   
         BH    BLD30360            YES                                          
         MVC   SPOTMAX2,SPOTLEFT   LESS THAN MAX: CAN FIT ON 1                  
         XC    SPOTLEFT,SPOTLEFT                                                
         B     BLD30380            DO LAST ONE                                  
BLD30420 EQU   *                                                                
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
*   THIS ROUTINE BUILDS THE '0204' TRANSACTION RECORD                           
*                                                                               
*   NOTE:  THERE MUST BE COMMENTS, MULTIPLE EFFECTIVE DATES, OR                 
*        MG INDICATOR TO PRODUCE A 0204 RECORD                                  
*                                                                               
BLD04REC NTR1                                                                   
         MVC   SAVELCOD,ELCODE     SAVE CURRENT ELEMENT CODE                    
         MVC   SAVRC203(RC203LEN),RC203REC                                      
*                                  SAVE THE BUY RECORD                          
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
         MVI   RC204REC,C' '       SPACE FILL THE RECORD                        
         MVC   RC204REC+1(RC204LEN-1),RC204REC                                  
         MVC   RC204ID,=C'0204'                                                 
         MVC   RC204STA(5),RCONKSTA   INSERT STATION                            
         CLI   RCONKSTA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC204STA+4(2),=C'TV'                                             
         MVC   RC204STA+6(1),RCONKGRP                                           
*                                  INSERT 1ST CHAR OF GROUP                     
         GOTO1 HEXOUT,DMCB,RCONKCON,RC204REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,RC204BY1),FILL=0,ZERO=NOBLANK                         
         MVC   RC204BY2,SVSL#      INSERT LINE/SUB LINE #S                      
         MVI   RC204TCD,C'A'       'ADD' CODE                                   
*                                                                               
         SR    R2,R2               SET SWITCH                                   
         TM    COMFLAGS,X'40'      COMMENTS EXIST?                              
         BNO   BLD40280            NO                                           
         MVI   RC204CM1,C' '       SPACE OUT COMMENT AREA                       
         MVC   RC204CM1+1(139),RC204CM1                                         
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
         EX    RF,BLD4023D         MOVE SECOND COMMENT (ORDER)                  
BLD40235 EQU   *                                                                
         SR    R2,R2               RESET COMMENT COUNTER                        
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         B     BLD40340            ONLY PUT OUT ONE COMMENT RECORD              
*                                     EITHER ORDER OR CONTRACT                  
*                                                                               
BLD4023A MVC   RC204CM1(0),3(R6)                                                
BLD4023B MVC   RC204CM2(0),3(R6)                                                
BLD4023C MVC   RC204CM1(0),2(R6)                                                
BLD4023D MVC   RC204CM2(0),2(R6)                                                
*                                                                               
BLD40260 EQU   *                                                                
         LTR   R2,R2               ANY ORDER COMMENTS?                          
         BNZ   BLD40280            YES - PRINT AND GET OUT                      
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40340            YES - COMMENTS FINISHED                      
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'04'        NO  - GO BACK AND DO THEM                    
         B     BLD40160                                                         
BLD40280 EQU   *                                                                
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
BLD40340 EQU   *                                                                
         MVC   RC203REC(RC203LEN),SAVRC203                                      
*                                  RESTORE THE BUY RECORD                       
         XC    COMFLAGS,COMFLAGS   TURN OFF COMMENTS FLAG                       
         MVC   ELCODE,SAVELCOD     RESTORE CURRENT ELEMENT CODE                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE '0210' TRANSACTION RECORD                           
*                                                                               
*                                                                               
BLD10REC NTR1                                                                   
         MVI   RC210REC,C' '       SPACE FILL THE RECORD                        
         MVC   RC210REC+1(RC210LEN-1),RC210REC                                  
         MVC   RC210ID,=C'0210'    INSERT ID                                    
         MVC   RC210STA(5),RCONKSTA   INSERT STATION                            
         CLI   RC210STA+4,C'L'                                                  
         BE    *+10                                                             
         MVC   RC210STA+4(2),=C'TV'                                             
         MVC   RC210STA+6(1),RCONKGRP                                           
*                                  INSERT 1ST CHAR OF GROUP                     
         GOTO1 HEXOUT,DMCB,RCONKCON,RC210REF,4,=C'TOG'                          
*                                  INSERT CONTRACT NUMBER                       
         MVC   RC210LRP,REPALPHA                                                
         MVC   RC210NRP,REPALPHA                                                
*                                                                               
*   SET START AND END DATE FROM SAVED EARLIEST BUY START AND                    
*        LATEST BUY END DATES  (NOT CONTRACT FLIGHTS).                          
*                                                                               
         MVC   RC210STD(16),FOXZEROS                                            
         CLC   ERLYSTRT,=X'FFFFFF' NO DATA FOUND?                               
         BE    BLDA0040            NONE FOUND: DON'T SET ANYTHING               
         GOTO1 DATCON,DMCB,(3,ERLYSTRT),(X'20',CNVDATE)                         
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC210STD+4(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC210STD+2(2),CNVDATE INSERT YY                                  
*                                                                               
*   IF YEAR IS > 70, DATE IS (MOST LIKELY) IN THE 20TH CENTURY.                 
*        LESS THAN THAT INDICATES 21ST CENTURY.  BEYOND THAT,                   
*        I DON'T THINK THE PROGRAM WILL BE AROUND THAT LONG.                    
*                                                                               
         MVC   RC210STD(2),=C'19'  SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    BLDA0020            YES                                          
         MVC   RC210STD(2),=C'20'  NO  - 21ST CENTURY                           
BLDA0020 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,LATREND),(X'20',CNVDATE)                          
*                                  YMD BINARY -> YYMMDD EBCDIC                  
         MVC   RC210END+4(4),CNVDATE+2                                          
*                                  INSERT MMDD                                  
         MVC   RC210END+2(2),CNVDATE INSERT YY                                  
         MVC   RC210END(2),=C'19'  SET TO 2OTH CENTURY                          
         CLC   CNVDATE(2),=C'70'   20TH CENTURY?                                
         BH    BLDA0040            YES                                          
         MVC   RC210END(2),=C'20'  NO  - 21ST CENTURY                           
BLDA0040 EQU   *                                                                
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*********************************************************************           
INIT     NTR1                                                                   
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
PRINTREC NTR1                                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         CLC   =C'0210',RC210ID    FINAL BUFFER RECORD?                         
         BNE   PRC20020            NO                                           
         BCTR  RF,0                YES - DON'T COUNT IT IN TOTALS               
         STC   RF,TRANSCNT         SAVE REVISED COUNT                           
         EDIT  TRANSCNT,(4,RC210TRC),FILL=0,ZERO=NOBLANK                        
*                                  INSERT RECORD COUNT                          
         EDIT  TOTALAMT,(11,RC210TDL),FILL=0,ZERO=NOBLANK                       
         EDIT  TLSPOTS,(6,RC210TOT),FILL=0,ZERO=NOBLANK                         
PRC20020 EQU   *                                                                
         LA    R2,2                LOOP CONTROL FOR PRINTING                    
         LA    R4,RC201REC         A(OUTPUT RECORD)                             
         MVC   217(3,R4),=C'DDS'   INSERT 'DDS' AT END OF LINE                  
PRC20040 EQU   *                                                                
         MVC   P(110),0(R4)                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,110(R4)          BUMP TO SECOND HALF OF RECORD                
         BCT   R2,PRC20040         DO SECOND HALF OF RECORD                     
         XIT1                                                                   
         EJECT                                                                  
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
         MVC   KEY+15(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+17,2            SET TYPE = JDS                               
         MVC   KEY+18(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+23(4),RCONKADV  INSERT ADVERTISER                            
         GOTO1 EOPREAD,DMCB,(R2),4 READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1C'           GET AGENCY                                   
         MVC   KEY+13(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+15,2            SET TYPE = JDS                               
         MVC   KEY+16(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+21(6),RCONKAGY  INSERT AGENCY/OFFICE                         
         GOTO1 EOPREAD,DMCB,(R2),6 READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1D'           GET OFFICE                                   
         MVC   KEY+17(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+19,2            SET TYPE = JDS                               
         MVC   KEY+20(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+25(4),RCONKOFF  INSERT OFFICE                                
         GOTO1 EOPREAD,DMCB,(R2),3 READ/ADD CODE TO TABLE                       
         LA    R2,12(R2)           BUMP TO NEXT CODE ENTRY                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'1E'           GET SALESPERSON                              
         MVC   KEY+16(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+18,2            SET TYPE = JDS                               
         MVC   KEY+19(5),RCONKSTA  INSERT STATION/MEDIA                         
         MVC   KEY+24(4),RCONSAL   INSERT SALESPERSON                           
         GOTO1 EOPREAD,DMCB,(R2),4 READ/ADD CODE TO TABLE                       
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
*   TRANSLATE DAY AND TIME TO JDS FORMAT.                                       
*                                                                               
DYTIME   NTR1                                                                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   DYTI0240            NOT FOUND - EXIT                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    DYTI0040            NO  - ONLY 1                                 
         MVC   RC203DAY(2),=C'*P'  2 OR MORE: INDICATE PATTERN                  
         GOTO1 CALNDRFL,DMCB,(RC)                                               
         MVC   RC203CAL,SPACES     CLEAR X'S IN ROTATOR FIELD                   
         B     DYTI0080                                                         
DYTI0040 EQU   *                                                                
         LA    RF,RBUYDAYS         A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),RBUYDYIN    INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =V(DAYUNPK),DMCB,,(0,XDATE),RR=YES                               
         MVC   RC203DAY,XDATE      LOAD RESULT TO OUTPUT RECORD                 
*                                                                               
         MVC   WORK(12),SPACES                                                  
         GOTO1 =V(UNTIME),DMCB,(0,RBUYDYT1),WORK,RR=YES                         
         MVC   RC203ORT(11),WORK   SET TIME STRING                              
*                                     FIELD EXTENDS INTO FOLLOWING              
*                                        RC203ORB FIELD, WHICH IS               
*                                        OTHERWISE NOT USED                     
*                                                                               
         MVC   HEXSTTIM(12),FOXZEROS CLEAR START/END TIMES                      
         EDIT  RBUYDYT1,(4,HEXSTTIM),FILL=0                                     
         EDIT  RBUYDYT2,(4,HEXENTIM),FILL=0                                     
         CLC   =C'0500',HEXSTTIM   BEFORE START OF DAY?                         
         BL    DYTI0050            NO                                           
         MVI   HEXSTTIM+4,C'M'     YES - INSERT 'M' INDICATOR                   
DYTI0050 EQU   *                                                                
         CLC   =C'0500',HEXENTIM   BEFORE START OF DAY?                         
         BL    DYTI0060            NO                                           
         MVI   HEXENTIM+4,C'M'     YES - INSERT 'M' INDICATOR                   
DYTI0060 EQU   *                                                                
*                                                                               
*   NOTE:  PER MARILYN PAIGE, FOLLOWING FIELDS (LOCK TIMES) ARE                 
*    BEING LEFT AS SPACES.  ONLY THE ORDERED TIME FIELD WILL BE                 
*    SENT.  BILL UHR.  SEP23/94.                                                
*                                                                               
****>>>  MVC   RC203STM(12),HEXSTTIM                                            
*                                  LOAD LOCK TIMES                              
         GOTO1 CALNDRFL,DMCB,(RC)                                               
DYTI0080 EQU   *                                                                
         MVC   RC203LNS,FOXZEROS   INITIALIZE LENGTH OF SPOT                    
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    DYTI0120            YES                                          
*                                                                               
*   IF 90 SECONDS OR LESS, USE SECONDS AS IS                                    
*                                                                               
         ZICM  RF,RBUYDUR,2        NO  - SECONDS                                
         CH    RF,=H'90'           > 90 SECONDS?                                
         BH    DYTI0120            YES                                          
         EDIT  RBUYDUR,(5,RC203LNS),FILL=0,ZERO=NOBLANK                         
*                                  USE AS IS                                    
         B     DYTI0240                                                         
DYTI0120 EQU   *                                                                
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         TM    RBUYDUR,X'80'       LENGTH IN MINUTES?                           
         BO    DYTI0130            YES - MINUTES:                               
*                                  NO  - SECONDS:  90 SECS OR LESS              
*                                     REMAIN AS 'SECONDS'                       
         CH    R1,=H'90'           90 SECS OR LESS?                             
         BH    DYTI0140            NO  - HIGHER: CONVERT TO MINS:SECS           
         B     DYTI0135                                                         
DYTI0130 EQU   *                                                                
*                                  MINUTES: 89 MINUTES OR LESS                  
*                                     REMAIN AS 'MINUTES'                       
         CH    R1,=H'59'           59 MINS OR LESS?                             
         BH    DYTI0140            NO  - HIGHER: CONVERT TO HRS:MIN             
DYTI0135 EQU   *                                                                
*                                  YES - SHOW AS SECS OR MINS                   
         ST    R1,FULL             SAVE 'REMAINDER': TOTAL LENGTH               
         SR    R1,R1               CLEAR ORIGINAL VALUE                         
         B     DYTI0150            NOW SKIP THE DIVIDE                          
DYTI0140 EQU   *                                                                
*                                                                               
*   SPOT LENGTH IS EITHER MINUTES OR SECONDS AT THIS POINT.                     
*        DIVIDING BY 60 PRODUCES EITHER HOURS (IF MINUTES)                      
*        OR MINUTES (IF SECONDS).  REMAINDER IS IN REG 0.                       
*                                                                               
         D     R0,=F'60'           YES - CONVERT TO HRS:MINS                    
         ST    R0,FULL             SAVE REMAINDER IN FULL                       
DYTI0150 EQU   *                                                                
*                                                                               
*   FULL CONTAINS THE REMAINDER (SECS/MINS) IF DIVIDE WAS DONE, OR              
*        TOTAL SECS/MINS IF TIME WAS 90 OR LESS.  R1 CONTAINS EITHER            
*        MINS OR HOURS.                                                         
*                                                                               
         TM    RBUYDUR,X'80'       LENGTH IN MINUTES?                           
         BO    DYTI0200            YES                                          
         LA    R4,RC203LNS+3       INSERT REMAINDER (SECS) FIRST                
         EDIT  (R0),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT MINUTES                               
DYTI0160 EQU   *                                                                
         LA    R4,RC203LNS+1       INSERT MINUTES NEXT                          
         EDIT  (R1),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT SECONDS                               
         B     DYTI0240                                                         
DYTI0200 EQU   *                                                                
         EDIT  (R1),(1,RC203LNS),ZERO=NOBLANK                                   
*                                  INSERT HOURS                                 
         LA    R4,RC203LNS+1                                                    
         L     R0,FULL             RESTORE VALUE OF REMAINDER                   
         EDIT  (R0),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT MINUTES                               
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
         MVC   RC203CAL,SPACES     CLEAR ROTATOR FIELDS                         
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     CALN0040                                                         
CALN0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
CALN0040 EQU   *                                                                
         BNE   CALN0100            NOT FOUND - EXIT                             
         LA    RF,RC203CAL         A(CALENDAR)                                  
         ZIC   RE,3(R6)            GET DAYS BYTE IN REG                         
         SLL   RE,25               SHIFT DAYS BYTE TO HI-ORDER                  
*                                     DROP 'SPARE' BIT                          
         LA    R0,7                LOOP CONTROL                                 
CALN0060 EQU   *                                                                
         LTR   RE,RE               SET CONDITION CODES FOR REG                  
         BNM   CALN0080            NOT MINUS = BIT NOT SET                      
         MVI   0(RF),C'X'          MINUS = BIT SET: SET CALENDAR                
         L     R1,DAYCNT           BUMP DAY COUNTER                             
         LA    R1,1(R1)                                                         
         ST    R1,DAYCNT           STORE IT BACK                                
CALN0080 EQU   *                                                                
         LA    RF,1(RF)            BUMP TO NEXT CALENDAR POSITION               
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
*    FILL START AND END TIME                                                    
*                                                                               
STRTEND  NTR1                                                                   
         MVC   RC203STM(12),SPACES                                              
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    FILL PRIORITY                                                              
*                                                                               
PRTY     NTR1                                                                   
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    FILL PROGRAM CODE                                                          
*                                                                               
PCDISP   NTR1                                                                   
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    FILL SECT/PLAN                                                             
*                                                                               
PLNDISP  NTR1                                                                   
                                                                                
*                                                                               
*   DDS HAS NO SUCH TRAFFIC CODE ELEMENT.  THIS NEEDS INVESTIGATION             
*        AND LATER DEVELOPMENT                                                  
*                                                                               
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
         MVC   RC203STD(16),FOXZEROS                                            
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
         MVC   RC203STD+4(4),HOLDDATE+2     INSERT MM/DD                        
         MVC   RC203STD+2(2),HOLDDATE       INSERT YY                           
*                                                                               
         MVC   RC203STD(2),=C'19'  SET TO 2OTH CENTURY                          
         CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
         BH    DINF0180            YES                                          
         MVC   RC203STD(2),=C'20'  NO  - 21ST CENTURY                           
DINF0180 EQU   *                                                                
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',HOLDDATE)                          
         MVC   RC203END+4(4),HOLDDATE+2     INSERT MM/DD                        
         MVC   RC203END+2(2),HOLDDATE       INSERT YY                           
*                                                                               
         MVC   RC203END(2),=C'19'  SET TO 2OTH CENTURY                          
         CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
         BH    DINF0200            YES                                          
         MVC   RC203END(2),=C'20'  NO  - 21ST CENTURY                           
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
         MVC   RC203CAL,SPACES                                                  
         LA    R4,RC203CAL         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTSWK,(1,0(R4))                                                
*                                  INSERT # SPTS/WK INTO CALENDAR               
SPTI0400 EQU   *                                                                
         EDIT  SPOTSWK,(3,RC203SPT),FILL=0,ZERO=NOBLANK                         
*                                  INSERT NUMBER SPOTS PER WEEK                 
         EDIT  RBUYCOS,(9,RC203RTE),FILL=0,ZERO=NOBLANK                         
*                                  INSERT PRICE PER SPOT                        
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
*   FILL IN CONTRACT '15' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
DATETIME NTR1                                                                   
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
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
LOCALWRK DSECT                                                                  
       ++INCLUDE RECNTJDS                                                       
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
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
TESTCTR  DS    F                                                                
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
SAVRC203 DS    250C                SAVE AREA FOR BUY RECORD                     
SAVELCOD DS    CL1                 SAVE AREA FOR CURRENT ELEMENT                
HEXSTTIM DS    CL6                 START TIME FOR BUY LINE                      
HEXENTIM DS    CL6                 END   TIME FOR BUY LINE                      
LOCALEND EQU   *                                                                
         EJECT                                                                  
*                                                                               
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
         MVI   PLCLASS,C' '        CLASS 'SPACE/BLANK'                          
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'181RECNT65   10/02/03'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
