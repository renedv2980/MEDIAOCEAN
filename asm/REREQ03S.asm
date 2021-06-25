*          DATA SET REREQ03S   AT LEVEL 109 AS OF 05/01/02                      
*PHASE T80703A,*                                                                
*INCLUDE HEXOUT                                                                 
REQ3     TITLE 'T80703 - REREQ03 - NEW REP REQUESTS PROCESS'                    
***********************************************************************         
*                                                                     *         
*  REREQ03 -- PHASE T80703 -- REP REQUEST PROCESS OVERLAY             *         
*                             (ALL ACTIONS EXCEPT ADD AND MENU)       *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  12/29/89  PJS  ORIGINAL DEVELOPMENT.                               *         
*                                                                     *         
*  MAR16/90 (MRR) --- CHAIN READING DOES NOT WORK.  TURN IT OFF UNTIL *         
*                      TIME TO INVETIGATE AND FIX.                    *         
*                                                                     *         
*  JUN05/90 (MRR) --- ADD A DATA FIELD FOR 1 MONTH ONLY               *         
*                                                                     *         
*  AUG10/90 (MRR) --- ADD DATA FIELDS: TVB REQION, OWNER, MARKET,     *         
*                      RANK, POINTPERSON AND NETWORK CONTRACT         *         
*                                                                     *         
*  SEP17/90 (MRR) --- ADD ALLOCATION LIST AND BUDGET START,END FIELD  *         
*                                                                     *         
*  FEB14/91 (BU ) --- CHAIN READING, CORRECTED A SHORT TIME BACK, IS  *         
*                     NO LONGER WORKING, AND HAS BEEN DISABLED.       *         
*                                                                     *         
*  MAR06/92 (BU ) --- CHANGE AS-AT DATE DISPLAY TO SHOW A DATE RANGE  *         
*                                                                     *         
*  MAR19/92 (MRR) --- ADD CREDIT RATING AND LIABILITY                 *         
*                                                                     *         
*  MAY13/92 (BU ) --- ADD DISPLAY OF SECOND REQUEST CARD              *         
*                                                                     *         
*  OCT29/93 (SKU) --- ADD RRG-ONLY SET IDENTIFIER REDISPLAY           *         
*                                                                     *         
*  JAN28/94 (BU ) --- ADD DEVELOPMENTAL S/P AND CONTRACT TYPE         *         
*                                                                     *         
*  APR11/94 (BU ) --- ADD STATION SET AND 3RD CARD LOGIC              *         
*                                                                     *         
*  OCT05/94 (BU ) --- RESET SPACING AND COMBO SUPPRESSION VALUES      *         
*                                                                     *         
*  DEC16/94 (BU ) --- MODULE DATE CHANGE:  NO OTHER CHANGES           *         
*                                                                     *         
*  APR07/95 (BU ) --- SUPPRESS DETAILS IF NO CUR$$ OPTION DISPLAY     *         
*                                                                     *         
*  OCT27/95 (SKU) --- ADD RRG-ONLY SET IDENTIFIER REDISPLAY FOR NEW   *         
*                     SETS                                            *         
*                                                                     *         
*  JAN19/96 (BU ) --- ADD REDISPLAY OF CONFIRM/UNCONFIRM              *         
*                                                                     *         
*  JAN26/96 (BU ) --- ADD REDISPLAY OF DIRECT RESPONSE                *         
*                                                                     *         
*  JAN06/98 (BU ) --- ADD TERRITORY FILTER                            *         
*                                                                     *         
*  FEB25/98 (BU ) --- ADD NO BACK BILLING OPTION                      *         
*                                                                     *         
*  JUL12/99 (BU ) --- ADD RRG-ONLY SET IDENTIFIER REDISPLAY FOR NEW   *         
*                     MARKET SET                                      *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                     *         
*  NOTE:  RC IS USED AS A PROGRAM BASE REGISTER AND DOES NOT POINT TO *         
*          THE WORK AREA                                              *         
*                                                                     *         
*         FURTHER, RC CANNOT BE SET-UP IN THE NMOD MACRO BECAUSE NMOD *         
*          TRIES TO (AND DOES) USE RC FOR THE WORKAREA                *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
T80703   CSECT                                                                  
         NMOD1 0,T80703,R7,RR=R2                                                
*                                                                               
         LA    RC,2048(R7)                                                      
         LA    RC,2048(RC)                                                      
         USING T80703+8192,RC                                                   
*                                                                               
         L     R9,0(R1)            A(WORK AREA FROM CALLER)                     
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,SUBRELO          SUB-OVERLAY RELOCATION FACTOR                
*                                                                               
*                                                                               
         LR    RF,R9               SET A(REQWRK AREA)                           
         AH    RF,=Y(DIOWORK)      SET A(IOWORK W/IN DSECT)                     
*                                                                               
         MVC   0(4,RF),=C'*IO*'    INSERT FLAG                                  
         LA    RF,4(RF)            BUMP PAST FLAG                               
         ST    RF,AIOADDR2         SAVE ADDRESS OF IOWORK                       
*                                                                               
         L     RA,ASAVE            A(TWA)                                       
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)         SECOND TWA REQISTER                          
         USING TWAD,RA,R8                                                       
*                                                                               
         LA    RE,ROUTINES                                                      
MAIN100  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ACTION NOT PROCESSED BY THE PHASE            
         CLC   ACTION,0(RE)                                                     
         BE    MAIN120                                                          
         LA    RE,4(RE)                                                         
         B     MAIN100                                                          
*                                                                               
MAIN120  EQU   *                   BRANCH TO PROCESS ROUTINE                    
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)          A(RTN)                                       
         A     RF,SUBRELO          RELOCATE                                     
         BR    RF                  AND GO                                       
         SPACE 2                                                                
EXXIT    XIT1                      GENERIC EXIT                                 
         SPACE 2                                                                
ROUTINES DS    0F                                                               
         DC    AL1(EQLIST),AL3(LIST000)                                         
         DC    AL1(EQNEXT),AL3(NEXT000)                                         
         DC    AL1(EQTOT),AL3(TOT000)                                           
         DC    AL1(EQDISP),AL3(DISP0000)                                        
         DC    H'0'                EOT                                          
         TITLE 'LIST REQUESTS MAINLINE'                                         
*                                                                               
*- TOTALS -- COUNT REQUESTS MATCHING FILTERS; NO DETAILS                        
*                                                                               
TOT000   EQU   *                                                                
         MVI   NODETAIL,1          SUPPRESS SCREEN DETAILS                      
         B     LIST000             PROCESS AS NORMAL LIST                       
         SPACE 2                                                                
*                                                                               
*- LIST REQUESTS MATCHING GIVEN FILTERS                                         
*                                                                               
LIST000  EQU   *                                                                
*                                                                               
*- LOAD IN MENU SCREEN                                                          
LIST100  EQU    *                                                               
         CLI   SCRNBILT,SCLIST                                                  
         BE    LIST120             SCREEN ALREADY LOADED.                       
*                                                                               
         LA    R2,RQSLAST                                                       
         LA    R4,SCLIST                                                        
         GOTO1 CALLOV,P1,((R4),(R2)),0                                          
         CLI   P2,X'FF'                                                         
         BNE   *+6                                                              
         DC    H'0'                ERROR LOADING SCREEN                         
         XC    SCRNBILT,SCRNBILT                                                
         MVI   SCRNBILT,SCLIST                                                  
*                                                                               
*- BUILD KEY FOR 1ST READ                                                       
LIST120  EQU    *                                                               
         XC    DISKADDR,DISKADDR   ASSUME LISTING ALL REQUESTS                  
*                                                                               
         BAS   RE,TITLES           PUT UP LIST TITLES                           
*                                                                               
         OC    RPTNUM,RPTNUM                                                    
         BZ    LIST200             ALL                                          
         B     LIST200             CHAIN READING IS OUT OF ORDER,               
*                                  AND IS DISABLED (AGAIN)                      
*                                                                               
*- SINGLE RQST -- POINT TO HEAD OF CHAIN.                                       
*  (USES LINK-FORWARD AS DISK ADDRESS)                                          
         MVI   RQHLINK+3,X'FF'     LAST BYTE OF ADDRESS                         
         L     RF,AREQNTRY                                                      
         MVC   RQHLINK(2),RQCARDID(RF)   2 CHAR CARD ID                         
         B     LIST200                                                          
         SPACE 2                                                                
*                                                                               
*- LIST NEXT.  RESTORE KEY FROM LAST PASS.                                      
*  (SAVED KEY IS LAST OF PRIOR SCREEN)                                          
NEXT000  EQU   *                                                                
         BAS   RE,TITLES           FIND FORMAT ROUTINE                          
         MVC   DISKADDR,SAVEDA                                                  
         OC    DISKADDR,DISKADDR                                                
         BZ    LIST120             START FROM THE BEGINNING                     
         SPACE 2                                                                
*                                                                               
*- CHECK FOR DELETE OR SELECT                                                   
LIST200  EQU    *                                                               
         BAS   RE,CKDEL                                                         
         BNZ   LIST940             REQUEST CHANGED                              
*                                                                               
         BAS   RE,CKSEL                                                         
         BNZ   LIST940             REQUEST SELECTED                             
*                                                                               
*- CLEAR OUT SCREEN AND POINT TO 1ST DISPLAY LINE                               
         GOTO1 ACLRSCRN,P1,LST1STH,LSTLAST-1                                    
*                                                                               
         LA    R2,LST1STH                                                       
*                                                                               
*- SET UP FOR REQUEST FILE READING                                              
         GOTO1 ASETREQ             POINT TO REQUEST FILE                        
*                                                                               
*- ONLY ASKING FOR 26 BYTE HEADER.                                              
*  READ RECORD AT OLD HEADER ADDRESS.                                           
         LA    RE,RQHAGY           READ INTO OLD HEADER                         
         ST    RE,AIOWORK                                                       
*                                                                               
*- CLEAR OUT DISK ADDRESSES OF LISTED REQUESTS.                                 
         LA    RE,LISTDA                                                        
         ST    RE,ALISTDA          PUT DISK ADDRESSES HERE                      
         XC    LISTDA(LLISTDA),LISTDA                                           
         MVI   #LISTDA,0                                                        
         SPACE                                                                  
*                                                                               
*- TOP OF LIST LOOP.                                                            
*  READ REQUEST FILE FOR 1ST/NEXT MATCHING RECORD                               
LIST220  EQU   *                                                                
         BAS   RE,NEXTREQ                                                       
         BNZ   LIST800             END OF REQUESTS                              
*                                                                               
*- APPLY FILTERS (IF ANY)                                                       
*                                                                               
*        SPOOF REQUESTS EXCLUDED                                                
*        REP CODE   (UNLESS AGY=ALL OPTION **DDS ONLY**)                        
*        REPORT ID                                                              
*        REQUESTOR                 PATTERN MATCH.                               
*        OFFICE                                                                 
*        DESTINATION                                                            
*        OUTPUT                                                                 
*                                                                               
         CLI   RREPO,C' '          BLANK ON SPOOF REQUESTS                      
         BE    LIST500                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TUBE CAN ASK FOR ALL AGENCIES            
         BNE   LIST225                                                          
         LA    RE,RQSOPT+4                                                      
         CLC   =C'AGY=ALL',RQSOPT  SKIP AGY FILTER                              
         BE    LIST230                                                          
*                                                                               
LIST225  CLC   RREP,TWAAGY         REQ REQ CODE MATCH TWA?                      
         BNE   LIST500                                                          
*                                                                               
LIST230  EQU   *                                                                
         OC    RPTNUM,RPTNUM       REPORT NUMBER?                               
         BZ    LIST240                                                          
         L     RF,AREQNTRY                                                      
         CLC   RNUM(2),RQCARDID(RF) SAME CARD ID?                               
         BE    LIST235                                                          
*                                                                               
         CLC   =C'99',RNUM         NO  - REQUEST CANCELLED?                     
         BE    LIST500             YES - SKIP IT                                
*                                                                               
         CLC   REQCARD1+52(4),SPACES                                            
*                                  CROSS-FILE REQUEST?                          
         BE    LIST235             NO  - CHECK REPORT GROUP                     
         OC    REQCARD1+52(4),REQCARD1                                          
*                                  CROSS-FILE REQUEST?                          
         BZ    LIST500             NO  - SKIP IT                                
LIST235  EQU   *                                                                
         CLC   =XL2'00',RQOPT4ID(RF) NEED TO CHECK RRG ID?                      
         BE    LIST240                                                          
         CLC   ROPTN4(2),RQOPT4ID(RF)                                           
         BNE   LIST500                                                          
*                                                                               
LIST240  EQU   *                                                                
         CLI   RQSNAMEH+5,0        REQUESTOR?                                   
         BE    LIST250                                                          
         ZIC   RF,RQSNAMEH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,LIST245                                                       
         BNE   LIST500                                                          
         B     LIST250                                                          
*                                                                               
LIST245  CLC   RQSNAME(0),RNAME                                                 
*                                                                               
LIST250  EQU   *                                                                
         OC    IREQOFF,IREQOFF     OFFICE?                                      
         BZ    LIST260                                                          
         CLC   IREQOFF,RREPO                                                    
         BNE   LIST500                                                          
*                                                                               
LIST260  EQU   *                                                                
         OC    IDEST,IDEST         DESTINATION ID?                              
         BZ    LIST270                                                          
         CLC   IDEST,REQDEST                                                    
         BNE   LIST500                                                          
*                                                                               
LIST270  EQU   *                                                                
         OC    IOUT,IOUT           OUTPUT?                                      
         BZ    LIST280                                                          
         CLC   IOUT,REQOUT                                                      
         BNE   LIST500                                                          
*                                                                               
LIST280  EQU   *                   ALL REPORT FILTERS                           
         OC    RPTNUM,RPTNUM                                                    
         BNZ   LIST290             NOT ALL                                      
*                                                                               
         TM    REQFLAG,X'01'       ONLY REPORT LINKED                           
         BZ    LIST500                                                          
*                                                                               
         CLC   RNUM,=X'FFFF'        EXCLUDE CANCELED                            
         BE    LIST500                                                          
*                                                                               
         CLC   RNUM,=C'99'          THIS MAY ALSO MEAN CANCELED                 
         BE    LIST500                                                          
*                                                                               
LIST290  EQU   *                   FUTURE FILTERS GO HERE                       
         SPACE                                                                  
*                                                                               
*- REQUEST PASSES FILTERS.                                                      
*  IF DISPLAYING DETAILS (LIST/NEXT)                                            
*     CHECK FOR ROOM ON SCREEN                                                  
*     CALL DISPLAY FORMAT ROUTINE (SELECTED BY TITLES ROUTINE)                  
*     INCREMENT LINE POINTER                                                    
*     SAVE DISK ADDRESS OF DISPLAYED REQUEST AND BUMP DA COUNTER                
*  ADD TO COUNTERS                                                              
*                                                                               
         CLI   NODETAIL,1                                                       
         BE    LIST480             NO DETAILS NEEDED                            
*                                                                               
         BAS   RE,GOTROOM                                                       
         BNZ   LIST900             OUT OF ROOM. SET UP FOR NEXT                 
*                                                                               
         FOUT  (R2)                XMIT THIS LINE                               
         L     RF,AFORMAT          FORMAT ROUTINE FOR DISPLAY                   
         BASR  RE,RF                                                            
*                                                                               
*  IF REQUEST IS TO DISPLAY MORE THAN ONE CARD, FORMAT ROUTINE                  
*    'DUMPCARD' WILL PASS BACK NEW R2 (ADDRESS OF SCREEN LINE)                  
*                                                                               
         CLC   =C'CARDS',RQSOPT    MULTIPLE CARD DISPLAY?                       
         BNE   LIST400             NO                                           
         L     R2,FULL             YES - SET A NEW R2                           
*                                                                               
LIST400  EQU   *                                                                
         BAS   RE,NEXTLINE         POINT R2 TO NEXT LINE                        
*                                                                               
         BAS   RE,BUMPDADR         SAVE DA OF LISTED REQUEST                    
*                                                                               
LIST480  BAS   RE,COUNTIT                                                       
         SPACE                                                                  
*                                                                               
*- LOOP BACK FOR NEXT REQUEST                                                   
LIST500  B     LIST220                                                          
         SPACE                                                                  
*                                                                               
*- END OF DATA.  NO MORE TO LIST.                                               
LIST800  EQU   *                                                                
         MVI   LISTMORE,1          NEED FOR POSSIBLE DEL/SELECT                 
         MVC   RQSACT,=CL8'LIST'   START AT THE BEGINNING AGAIN                 
         XC    SAVEDA,SAVEDA       CLEAR OUT TWA DISK ADDRESS                   
         MVC   RQSMSG(L'NOMORE),NOMORE                                          
         OI    RQSACTH+6,X'01'     ALLOW D/S AFTER LIST                         
         B     LIST950                                                          
         SPACE                                                                  
*                                                                               
*- MORE DATA TO LIST.  SET UP FOR NEXT.                                         
LIST900  EQU   *                                                                
         MVC   RQSMSG(L'MORE),MORE                                              
LIST920  MVI   LISTMORE,1                                                       
         ZIC   R1,#LISTDA          # DISK ADDRESSES SAVED                       
         SLL   R1,2                * 4 BYTES PER D/A                            
         LA    RE,LISTDA-4                                                      
         AR    RE,R1                                                            
         MVC   SAVEDA,0(RE)        SAVE LAST LIST D/A                           
         MVC   RQSACT,=CL8'NEXT'                                                
         OI    RQSACTH+6,X'01'     AVOID 'NO INPUT' MSG                         
         B     LIST950                                                          
*                                                                               
*- EXIT FOR DELETE/SELECT                                                       
LIST940  OI    RQSACTH+6,X'01'     AVOID 'NO INPUT' MSG                         
         MVI   LISTMORE,1                                                       
*                                                                               
LIST950  EQU   *                                                                
         LA    R2,RQSACTH                                                       
         FOUT  (R2)                                                             
         ST    R2,CURSPOS          PUT CURSOR HERE                              
         XC    AERROR,AERROR       MSG ALREADY DONE.                            
         SR    R0,R0                                                            
LISTEXT  B     EXXIT                                                            
         TITLE 'READ FIRST/NEXT REQUEST'                                        
*                                                                               
*- NEXTREQ -- READ 1ST/NEXT REQUEST RECORD                                      
*                                                                               
*  ACTUALLY DOES 2 KINDS OF READING:                                            
*                                                                               
*  LISTING ALL REQUESTS:                                                        
*        SEQ THRU FILE USING DISK ADDRESS RETURNED BY DMGR                      
*                                                                               
*  LISTING PARTICULAR REPORT ID                                                 
*        READ THRU FILE USING LINK ADDRESS.                                     
*                                                                               
NEXTREQ  NTR1                                                                   
*                                                                               
*- LISTING ALL?                                                                 
         OC    RPTID,RPTID                                                      
*        BNZ   NXREQ200            NO.                                          
*                                                                               
*   CHAIN READING IS OUT OF ORDER, AND IS DISABLED (AGAIN)                      
*                                                                               
         MVC   KEY(4),DISKADDR                                                  
*                                                                               
         GOTO1 DIRSEQ                                                           
*                                                                               
         B     NXREQ300                                                         
         SPACE                                                                  
*                                                                               
*- READING SINGLE REPORT ID                                                     
NXREQ200 EQU   *                                                                
         MVC   KEY(4),RQHLINK      D/A OF NEXT                                  
*                                                                               
*- NO FORWARD LINK IS EOF ON SINGLE-ID READS.                                   
         OC    RQHLINK,RQHLINK                                                  
         BZ    NXREQEOF                                                         
*                                                                               
         GOTO1 DIRCHAIN                                                         
*                                                                               
*- BOTH READS COME HERE                                                         
NXREQ300 EQU   *                                                                
         MVC   DISKADDR,KEYSAVE    PASS BACK DA FOR CURRENT REQ.                
*                                                                               
         CLI   P3,0                DMGR ERROR?                                  
         BE    NXREQOK                                                          
*                                                                               
         TM    P3,X'80'            END OF FILE?                                 
         BO    NXREQEOF            YES.                                         
*                                                                               
         DC    H'0'                DMGR ERROR IN P3.                            
*                                                                               
NXREQOK  SR    R0,R0               REQUEST FOUND                                
         B     EXXIT                                                            
NXREQEOF LTR   RD,RD               END OF FILE CC                               
         B     EXXIT                                                            
         TITLE 'SELECT TITLES FOR SCREEN'                                       
*                                                                               
*- TITLES -- SELECT SCREEN TITLES AND FORMAT ROUTINE                            
*                                                                               
*  FOR NOW, USE TITLES BUILT INTO SCREEN.                                       
*                                                                               
TITLES   NTR1                                                                   
*****    GOTO1 ACLRSCRN,P1,LSTTTL1H,LSTTTL2H                                    
         SPACE                                                                  
*                                                                               
*- PICK DISPLAY ROUTINE AND TITLES BASED ON OPTION FIELD INPUT.                 
*                                                                               
*  SET UP DEFAULT TITLES/FORMAT IN CASE NO MATCH FOUND.                         
****     MVC   LSTTTL1,COL1                                                     
****     MVC   LSTTTL2,COL2                                                     
****     MVC   LSTTTL3,COL3                                                     
         LA    RE,DUMPCARD         BASIC CARD DUMP                              
         ST    RE,AFORMAT                                                       
*                                                                               
         CLI   RQSOPTH+5,0                                                      
         BE    TTLEXT              NO OPTION. USE DEFAULK                       
*                                                                               
         LA    R1,DFORMAT                                                       
TTL100   EQU   *                                                                
         CLI   0(R1),0                                                          
         BE    TTLEXT              NO MATCH. USE DEFAULT.                       
*                                                                               
         CLC   DFMIN(1,R1),RQSOPTH+5   MEET MINIMUM IPT LEN                     
         BH    TTL150                                                           
*                                                                               
         ZIC   RE,RQSOPTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,TTLCHECK                                                      
         BE    TTL200              A HIT!                                       
*                                                                               
TTL150   LA    R1,DFLNTRY(R1)      NEXT ENTRY                                   
         B     TTL100                                                           
         SPACE                                                                  
TTLCHECK CLC   DFOPT(0,R1),RQSOPT                                               
         SPACE                                                                  
*                                                                               
*- PICK UP 3 78 BYTE COLUMNS                                                    
TTL200   EQU   *                                                                
*        LA    R0,3                                                             
*        LA    RE,DFTTL1(R1)       GET FROM HERE                                
*        LA    RF,LSTTTL1H         PUT THEM HERE                                
*TL220   ICM   R2,15,0(RE)                                                      
*        A     R2,SUBRELO                                                       
*        MVC   8(78,RF),0(R2)      MOVE A TITLE                                 
*        LA    RE,4(RE)                                                         
*        ZIC   R2,0(RF)                                                         
*        AR    RF,R2               NEXT SCREEN TITLE                            
*        BCT   R0,TTL220                                                        
         SPACE                                                                  
*                                                                               
*- PICK UP A(FORMAT DISPLAY RTN)                                                
         ICM   RF,15,DFRTN(R1)                                                  
         A     RF,SUBRELO                                                       
         ST    RF,AFORMAT                                                       
*                                                                               
TTLEXT   EQU   *                                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
DFMIN    EQU   0                   MINIMUM INPUT FOR THIS OPTION                
DFOPT    EQU   1                   OPTION KEYWORD                               
DFRTN    EQU   16                  A(PROCESSING RTN)                            
DFTTL1   EQU   20                  A(TITLE 1)                                   
DFTTL2   EQU   24                  A(TITLE 2)                                   
DFTTL3   EQU   28                  A(TITLE 3)                                   
DFLNTRY  EQU   32    <---   ENTRY LENGTH                                        
         SPACE                                                                  
DFORMAT  DS    0H                                                               
         DC    AL1(3),CL15'HEX',AL4(DHEX,COL1,COL2,COL3)                        
         DC    X'0'                                                             
         DS    0H                                                               
         SPACE                                                                  
*                                                                               
*- 01-72 POSITIONAL TITLES                                                      
*                                                                               
COL1     DC    CL44'ACT 0........1.........2.........3.........4'               
         DC    CL34'.........5.........6.........7..'                           
*                                                                               
COL2     DC    CL44'--- 1234567890123456789012345678901234567890'               
         DC    CL34'12345678901234567890123456789012'                           
*                                                                               
COL3     DC    CL78' '                                                          
         TITLE 'LIST DATA FORMAT ROUTINES'                                      
*                                                                               
*- DUMPCARD -- STANDARD OUTPUT                                                  
DUMPCARD NTR1                                                                   
         USING DCARDD,R2                                                        
         MVC   DCID,RNUM           REPORT ID NUMBER                             
         MVC   DCCARD,RREPO        REMAINDER OF CARD                            
*                                                                               
*- IF ALL AGENCY OPTION ON LIST, PUT REP CODE IN LAST 2 RQSTR NAME              
         CLC   =C'AGY=ALL',RQSOPT                                               
         BNE   DMPCD004                                                         
         MVI   DCBLANK,C' '        1 BLANK BEFORE REP J.I.C. LONG RQSTR         
         MVC   DCREP,RREP                                                       
         B     DMPCDEXT                                                         
*                                                                               
DMPCD004 EQU   *                                                                
         CLC   =C'CARDS',RQSOPT    DISPLAY 2ND CARD?                            
         BNE   DMPCDEXT            NO  - FINISHED WITH CARD                     
*                                                                               
*   MULTIPLE LINE DISPLAY:  SCREEN HAS EVEN NUMBER OF LINES.  A                 
*    CHECK FOR ROOM IS NEEDED BECAUSE A THIRD LINE MAY BE PRESENT.              
*    ADDR(NEXT LINE) IS SET. THE DISK ADDRESS OF THE PREVIOUS LINE IS           
*    TABLED.  THE DISK ADDRESS OF 2ND A/O 3RD LINE WILL BE THE SAME             
*    AS THE FIRST, SO THAT THE SELECTION OR DELETION MAY BE DONE                
*    FROM ANY LINE.  ALSO, ADDRESS OF NEW LINE MUST BE PASSED                   
*    BACK TO CALLING ROUTINE.                                                   
*                                                                               
         BAS   RE,NEXTLINE         POINT TO NEXT SCREEN LINE                    
         BAS   RE,BUMPDADR         SAVE DISK ADDRESS OF PREVIOUS LINE           
         FOUT  (R2)                TRANSMIT THIS LINE                           
         CLI   RQHFLAG,0           MORE THAN ONE CARD?                          
         BE    DMPCD008            NO                                           
         MVC   DCID(78),REQCARD0   YES - DISPLAY 78 CHARS                       
         CLI   R2CONTIN,C'+'       THIRD CARD?                                  
         BNE   DMPCD008            NO  -                                        
         BAS   RE,NEXTLINE         YES - POINT TO NEXT SCREEN LINE              
         BAS   RE,BUMPDADR         SAVE DISK ADDRESS OF PREVIOUS LINE           
         FOUT  (R2)                TRANSMIT THIS LINE                           
         MVC   DCID(78),REQCARD1   YES - DISPLAY 78 CHARS                       
DMPCD008 EQU   *                                                                
         ST    R2,FULL             PASS NEW R2 BACK                             
DMPCDEXT B     COMMXIT                                                          
         DROP  R2                                                               
         SPACE 2                                                                
*                                                                               
*- DHEX -- DISPLAY DISK ADDRESS, THE 1ST 10 BYTES, AND OPT4                     
DHEX     NTR1                                                                   
         USING DHEXD,R2                                                         
         GOTO1 =V(HEXOUT),P1,DISKADDR,DHADDR,4,=C'STD',0,RR=SUBRELO             
         MVC   DH10,REQCARD                                                     
         MVC   DHOPT4,ROPTN4                                                    
         DROP  R2                                                               
         B     COMMXIT                                                          
         TITLE 'DELETE MAINLINE'                                                
*                                                                               
*- CKDEL - DELETE REQUESTS.                                                     
*          CC 0 = NONE DELETED;  ^0 = REQUESTS DELETED                          
*                                                                               
CKDEL    NTR1                                                                   
*                                                                               
*- SET UP FOR REQUEST FILE READING                                              
         GOTO1 ASETREQ             POINT TO REQUEST FILE                        
*                                                                               
*- FOR SOME REASON, DMGR RETURNS 26 BYTE HEADER ONLY.                           
*  COMPENSATE BY READING RECORD AT OLD HEADER ADDRESS.                          
         LA    RE,RQHAGY           READ INTO OLD HEADER                         
         ST    RE,AIOWORK                                                       
*                                                                               
         LA    R2,LST1STH          1ST IPT FIELD                                
         LA    R3,LISTDA           D/A'S OF LISTED RQSTS                        
*                                                                               
*- PROCESS FIELD LOOP                                                           
DEL100   CLI   5(R2),0             ANY INPUT?                                   
         BE    DEL500                                                           
*                                                                               
         CLI   8(R2),C'D'          DELETE?                                      
         BNE   DEL500                                                           
*                                                                               
         OC    0(4,R3),0(R3)       ANY DISK ADDRESS?                            
         BZ    DEL500                                                           
*                                                                               
*- READ IN REQUEST FOR CHANGE                                                   
         MVC   KEY(4),0(R3)                                                     
         GOTO1 DIRCHAIN                                                         
         BNZ   DELERRD             DMGR ERROR                                   
*                                                                               
*- DELETE REQUEST                                                               
DEL200   EQU   *                                                                
         CLC   RNUM,=C'99'         ALREADY DELETED?                             
         BE    DEL500                                                           
         CLC   RNUM,=X'FFFF'       ALTERNATE CANCEL INDICATOR                   
         BE    DEL500                                                           
*                                                                               
         MVC   RNUM,=C'99'         MARK AS CANCELED                             
*                                                                               
         MVI   ANYCHA,1            CHANGES MADE                                 
         MVC   KEY(4),0(R3)                                                     
         GOTO1 DIRWRIT                                                          
*                                                                               
*- NOTIFY USER RQST HAS BEEN DELETED                                            
         MVC   8(L'DELMSG,R2),DELMSG                                            
         FOUT  (R2)                                                             
*                                                                               
         XC    0(4,R3),0(R3)       ZERO DISK ADDRESS                            
*                                                                               
*- DONE WITH THIS REQUEST.                                                      
*  LOOP BACK IF MORE LINES TO PROCESS.                                          
*  AT END OF SCREEN, TURN OFF VALIDATION SWITCH AND LOOP BACK AGAIN             
*     TO DO ACTUAL UPDATES.  (STOP IF THIS IS THE UPDATE PASS)                  
*                                                                               
DEL500   EQU   *                                                                
         BAS   RE,NEXTLINE         NEXT SCREEN LINE                             
         LA    R3,4(R3)            NEXT DISK ADDRESS                            
*                                                                               
         LA    RE,LSTLAST          AT END OF SCREEN?                            
         CR    R2,RE                                                            
         BL    DEL100              NO.                                          
*                                                                               
DEL520   EQU   *                                                                
         OC    ANYCHA,ANYCHA                                                    
         BZ    DELNODEL            NO.                                          
*                                                                               
         LA    RE,CHAMSG                                                        
         LA    RF,L'CHAMSG                                                      
         STM   RE,RF,AERROR                                                     
         GOTO1 ADOMSG                                                           
*                                                                               
*- TRANSMIT ALL FIELDS TO AVOID HAVING SOME FIELDS FLIPPED TO PROTECT           
         LA    R2,LST1STH                                                       
DEL540   FOUT  (R2)                                                             
         BAS   RE,NEXTLINE                                                      
         LA    RE,LSTLAST          AT END OF SCREEN?                            
         CR    R2,RE                                                            
         BL    DEL540              NO.                                          
*                                                                               
         LTR   RD,RD               DELETES FOUND                                
         B     EXXIT                                                            
*                                                                               
DELNODEL SR    R0,R0               NO DELETES PROCESSED                         
         B     EXXIT                                                            
         SPACE                                                                  
*                                                                               
DELERRD  XC    AERROR,AERROR       DMGR ERROR. MSG ALREADY UP.                  
         ST    R2,CURSPOS                                                       
         LTR   RD,RD                                                            
         B     EXXIT                                                            
*                                                                               
         SPACE                                                                  
DELMSG   DC    CL79'  ** REQUEST HAS BEEN DELETED **'                           
CHAMSG   DC    C'REQUESTS CHANGED AS INDICATED'                                 
*                                                                               
         DS    0H                                                               
INVIPT   EQU   2                   INVALID INPUT MSG.                           
         TITLE 'SELECT MAINLINE'                                                
*                                                                               
*- CKSEL - SELECT 1 REQUEST FOR DISPLAY                                         
*                                                                               
*  LOOK FOR 'S' (SELECT) OR 'C' (SELECT FOR CHANGE) IN SELECT COLUMN            
*          CC 0 = NONE SELECTED  ^0 = REQUEST SELECTED                          
*                                                                               
CKSEL    NTR1                                                                   
         LA    R2,LST1STH          1ST IPT FIELD                                
         LA    R3,LISTDA           D/A'S OF LISTED RQSTS                        
*                                                                               
*- PROCESS FIELD LOOP                                                           
SEL100   CLI   5(R2),0             ANY INPUT?                                   
         BE    SEL500                                                           
*                                                                               
         CLI   8(R2),C'S'          SELECT? (FOR DISPLAY)                        
         BE    SEL120                                                           
*                                                                               
         CLI   8(R2),C'C'          SELECT? (FOR CHANGE)                         
         BNE   SEL500                                                           
*                                                                               
SEL120   OC    0(4,R3),0(R3)       ANY DISK ADDRESS?                            
         BZ    SEL500                                                           
*                                                                               
         MVC   SELECTSW,8(R2)      SET SELECT SWITCH                            
*                                                                               
*- READ IN REQUEST FOR DISPLAY                                                  
         GOTO1 ASETREQ             POINT TO REQUEST FILE                        
         LA    RE,RQHAGY           READ INTO OLD HEADER                         
         ST    RE,AIOWORK                                                       
*                                                                               
         MVC   KEY(4),0(R3)                                                     
         MVC   SAVEDA,0(R3)                                                     
         GOTO1 DIRCHAIN                                                         
         BNZ   SELERRD             DMGR ERROR                                   
*                                                                               
*- POINT BACK TO REP FILE                                                       
         GOTO1 ASETREP                                                          
         L     RE,AIOADDR2                                                      
         ST    RE,AIOWORK                                                       
*                                                                               
         MVI   RPTPREFX,C'R'                                                    
         MVC   RPTNUM,RNUM                                                      
         CLI   RNUM,C'R'           RRG?                                         
         BNE   SEL150                                                           
         MVC   RPTNUM,ROPTN4       RPT ID FOR RRG CLASS REPORTS                 
*                                                                               
*- EXIT WITH ^0 CC (REQUEST READY FOR DISPLAY)                                  
SEL150   LTR   RD,RD                                                            
         B     EXXIT                                                            
*                                                                               
*- DONE WITH THIS REQUEST.                                                      
*  LOOP BACK IF MORE LINES TO PROCESS.                                          
*  AT END OF SCREEN, TURN OFF VALIDATION SWITCH AND LOOP BACK AGAIN             
*     TO DO ACTUAL UPDATES.  (STOP IF THIS IS THE UPDATE PASS)                  
*                                                                               
SEL500   EQU   *                                                                
         BAS   RE,NEXTLINE         NEXT SCREEN LINE                             
         LA    R3,4(R3)            NEXT DISK ADDRESS                            
*                                                                               
         LA    RE,LSTLAST          AT END OF SCREEN?                            
         CR    R2,RE                                                            
         BL    SEL100              NO.                                          
*                                                                               
SEL520   EQU   *                                                                
         SR    R0,R0               NO SELECTS FOUND                             
         B     EXXIT                                                            
         SPACE                                                                  
*                                                                               
SELERRD  XC    AERROR,AERROR       DMGR ERROR. MSG ALREADY UP.                  
         ST    R2,CURSPOS                                                       
         LTR   RD,RD                                                            
         B     EXXIT                                                            
         TITLE 'MISC ROUTINES'                                                  
*                                                                               
*- GOTROOM -- MAKE SURE WE HAVE ROOM TO DISPLAY THIS LINE                       
GOTROOM  EQU   *                                                                
         SR    R0,R0                                                            
         LA    R1,LST2ND-LST1ST    L(ENTRY)                                     
         ZIC   RF,RQHFLAG          GET # OF CARDS BYTE                          
         SRL   RF,4                KEEP HI NYBBLE ONLY                          
         LA    RF,1(RF)            ADJUST TO ABSOLUTE COUNT                     
         MR    R0,RF               LENGTH * # OF CARDS                          
         LA    RF,LSTLAST          A(END OF SCREEN)                             
         SR    RF,R1               BACK UP # OF LINES                           
         CR    R2,RF               CURRENT LINE POINTER -VS- E.O.S.             
         BNH   GROOMOK                                                          
         LTR   RD,RD               NO ROOM LEFT (^0 CC)                         
         BR    RE                                                               
GROOMOK  CR    R0,R0               SET 0 CC (GOOD)                              
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*- NEXTLINE -- POINT R2 TO NEXT SCREEN LINE                                     
NEXTLINE EQU   *                                                                
         LA    R2,(LST2ND-LST1ST)(R2)                                           
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*- BUMPDADR -- SAVES THE DISK ADDRESS OF THE LISTED REQUEST                     
BUMPDADR EQU   *                                                                
         L     R1,ALISTDA          SAVE DA'S OF LISTED RQSTS                    
         MVC   0(4,R1),DISKADDR                                                 
         LA    R1,4(R1)                                                         
         ST    R1,ALISTDA          POINT TO NEXT DA SLOT                        
*                                                                               
         ZIC   R1,#LISTDA          BUMP COUNT OF SAVED DA'S                     
         LA    R1,1(R1)                                                         
         STC   R1,#LISTDA                                                       
*                                                                               
         BR    RE                  RETURN                                       
         SPACE 2                                                                
*                                                                               
*                                                                               
*- COUNTIT -- ADD TO ANY COUNTERS WE MAY BE KEEPING                             
COUNTIT  BR    RE                                                               
         TITLE 'DISPLAY CURRENT REQUEST'                                        
*                                                                               
*- DISP0000-- DISPLAY CURRENT REQUEST.                                          
*                                                                               
DISP0000 EQU   *                                                                
*                                                                               
*- DISPLAY HEADLINE FIELDS                                                      
         LA    R2,RQSNUMH                                                       
         MVC   8(L'RPTNUM,R2),RPTNUM    REPORT NUMBER                           
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RQSNAMEH                                                      
         MVC   8(L'RNAME,R2),RNAME  REQUESTOR NAME                              
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RQSOFFH                                                       
         MVC   8(L'RREPO,R2),RREPO  REQUESTING OFFICE                           
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RQSSPCEH                                                      
         MVC   8(L'R2SPACE,R2),R2SPACE    OPTIONAL SPACING VALUE                
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RQSCMBOH                                                      
         MVC   8(L'RCOMPNO,R2),RCOMPNO    OPTIONAL COMBO SUPPRESS               
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RQSOPT                                                        
         LA    R3,RQSOPT                                                        
         LA    R4,RQSOPT+15        CALC END OF FIELD                            
         MVC   0(L'RQSOPT,R2),SPACES CLEAR THE FIELD                            
         CLI   RPAYPRO,C'Y'        PAID PROGRAMMING SET?                        
         BNE   DISP0020            NO                                           
         MVC   0(2,R2),=C'PP'      SET DIRECT RESPONSE                          
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0020 EQU   *                                                                
         CLI   RDIRRES,C'Y'        DIRECT RESPONSE SET?                         
         BNE   DISP0040            NO                                           
         MVC   0(2,R2),=C'DR'      SET DIRECT RESPONSE                          
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0040 EQU   *                                                                
         CLI   RNOBAKB,C'+'        ONLY BACK BILLING SET?                       
         BNE   DISP0060            NO                                           
         MVC   0(2,R2),=C'B+'      SET ONLY BACK BILLING                        
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0060 EQU   *                                                                
         CLI   RNOBAKB,C'-'        *NO* BACK BILLING SET?                       
         BNE   DISP0080            NO                                           
         MVC   0(2,R2),=C'B-'      SET *NO* BACK BILLING                        
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0080 EQU   *                                                                
         CLI   RNOBAKB,C'A'        ALL       BILLING SET?                       
         BNE   DISP0100            NO                                           
         MVC   0(2,R2),=C'BA'      SET ALL       BILLING                        
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0100 EQU   *                                                                
         CLI   RCONUNC,C' '        CONF/UNCF SET?                               
         BE    DISP0140            NO                                           
         MVC   0(2,R2),=C'CF'      SET CONFIRM                                  
         CLI   RCONUNC,C'C'        CONFIRM?                                     
         BE    DISP0120            YES                                          
         MVC   0(2,R2),=C'UC'      NO  - SET UNCONFIRM                          
DISP0120 EQU   *                                                                
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0140 EQU   *                                                                
         CLI   RJNLEFT,C' '        JOIN/LEFT SET?                               
         BE    DISP0260            NO                                           
         CLI   RJNLEFT,X'00'       JOIN/LEFT SET?                               
         BE    DISP0260            NO                                           
         CLI   RJNLEFT,C'1'        GROSS BILLING?                               
         BNE   DISP0160            NO                                           
         MVC   0(2,R2),=C'R1'      YES - SET 'GROSS BILLING' REPORT             
         B     DISP0240                                                         
DISP0160 EQU   *                                                                
         CLI   RJNLEFT,C'2'        EQUAL BAR?                                   
         BNE   DISP0180            NO                                           
         MVC   0(2,R2),=C'R2'      YES - SET 'EQUAL BAR' REPORT                 
         B     DISP0240                                                         
DISP0180 EQU   *                                                                
         CLI   RJNLEFT,C'3'        KATZ COMP?                                   
         BNE   DISP0200            NO                                           
         MVC   0(2,R2),=C'R3'      YES - SET 'GROSS BILLING' REPORT             
         B     DISP0240                                                         
DISP0200 EQU   *                                                                
         CLI   RJNLEFT,C'5'        JOIN/LEAV?                                   
         BNE   DISP0220            NO                                           
         MVC   0(2,R2),=C'R5'      YES - SET 'JOIN/LEAVE' REPORT                
         B     DISP0240                                                         
DISP0220 EQU   *                                                                
         CLI   RJNLEFT,C'6'        TOM OLSON?                                   
         BE    *+6                 YES                                          
         DC    H'0'                UNRECOGNIZED OPTION                          
         MVC   0(2,R2),=C'R6'      YES - SET 'TOM OLSON' REPORT                 
DISP0240 EQU   *                                                                
         MVI   2(R2),C','          INSERT COMMA                                 
         LA    R2,3(R2)            BUMP TO NEXT FIELD                           
DISP0260 EQU   *                                                                
         CLI   RDNHEAD,C'Y'        OPTION SET?                                  
         BNE   DISP0280            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'2'            AT LEAST 2 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'HD'      DOWNLOAD HEADINGS (D/L ONLY)                 
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0280 EQU   *                                                                
         CLI   RSUPDET,C'Y'        OPTION SET?                                  
         BNE   DISP0300            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'4'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(4,R2),=C'CUR$'           SUPPRESS DETAILS IF NO CUR$           
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0300 EQU   *                                                                
         CLI   RTRADE,C'Y'         OPTION SET?                                  
         BNE   DISP0310            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'4'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'TR'      TRADE OPTION DATA                            
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0310 EQU   *                                                                
         CLI   RALTCAL,C'Y'        OPTION SET?                                  
         BNE   DISP0320            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'4'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'AC'      ALTERNATE CALENDAR DATA                      
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0320 EQU   *                                                                
         CLI   RDISALL,C'Y'        OPTION SET?                                  
         BNE   DISP0340            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'4'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(3,R2),=C'ALL'     'DISPLAY ALL' IF NO ROW VALUE                
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0340 EQU   *                                                                
         CLI   RNATLOC,C' '        OPTION SET?                                  
         BE    DISP0380            NO                                           
         CLI   RNATLOC,X'00'       OPTION SET?                                  
         BE    DISP0380            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'3'            AT LEAST 3 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'$N'      'NATL BILLING' USED                          
         CLI   RNATLOC,C'N'                                                     
         BE    DISP0360                                                         
         MVC   0(2,R2),=C'$L'      'LOC  BILLING' USED                          
DISP0360 EQU   *                                                                
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0380 EQU   *                                                                
         CLI   RRERFLG,C' '        OPTION SET?                                  
         BE    DISP0420            NO                                           
         CLI   RRERFLG,X'00'       OPTION SET?                                  
         BE    DISP0420            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'3'            AT LEAST 3 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'R+'      'RER ONLY' USED                              
         CLI   RRERFLG,C'+'                                                     
         BE    DISP0400                                                         
         MVC   0(2,R2),=C'R-'      'EXCLUDE RER' USED                           
DISP0400 EQU   *                                                                
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0420 EQU   *                                                                
         CLI   RCOMBUD,C'Y'        OPTION SET?                                  
         BNE   DISP0440            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'3'            AT LEAST 3 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'CB'      'COMPANY BUDS' USED                          
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0440 EQU   *                                                                
         CLI   RSALBUD,C'Y'        OPTION SET?                                  
         BNE   DISP0460            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'3'            AT LEAST 3 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'SP'      'S/P     BUDS' USED                          
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0460 EQU   *                                                                
         CLI   RFORCST,C'Y'        OPTION SET?                                  
         BNE   DISP0480            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'3'            AT LEAST 3 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(2,R2),=C'FC'      'FORECAST' USED                              
         FOUT  RQSOPTH                                                          
         B     DISP0520                                                         
*                                                                               
DISP0480 EQU   *                                                                
         CLC   RREPSET,SPACES      REP= OPTION SET?                             
         BE    DISP0500            NO                                           
         OC    RREPSET,RREPSET     REP= OPTION SET?                             
         BZ    DISP0500            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'4'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0500            NO  - CAN'T PUT IN                           
         MVC   0(L'RQSOPT,R2),SPACES CLEAR THE FIELD                            
         MVC   0(4,R2),=C'REP='                                                 
         MVC   4(4,R2),RREPSET     INSERT SET NAME                              
         FOUT  RQSOPTH                                                          
*                                                                               
DISP0500 EQU   *                                                                
         CLC   RXFILNM,SPACES      FIL= OPTION SET?                             
         BE    DISP0520            NO                                           
         OC    RXFILNM,RXFILNM     FIL= OPTION SET?                             
         BZ    DISP0520            NO                                           
         LR    RF,R4               SET A(END OF FIELD)                          
         SR    RF,R2               CALC SPACE REMAINING                         
         C     RF,=F'6'            AT LEAST 4 CHARS LEFT?                       
         BL    DISP0520            NO  - CAN'T PUT IN                           
         MVC   0(L'RQSOPT,R2),SPACES CLEAR THE FIELD                            
         MVC   0(2,R2),=C'F='                                                   
         MVC   2(4,R2),RXFILNM     INSERT SET NAME                              
         FOUT  RQSOPTH                                                          
*                                                                               
DISP0520 EQU   *                                                                
         OI    RQSDESTH+4,II1C                                                  
         OC    REQDEST,REQDEST     ANY DESTINATION?                             
         BZ    DISP0620                                                         
*                                                                               
         GOTO1 ASETCNTL            READ FROM CONTROL FILE                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            ID RECORD                                    
         MVC   KEY+23(2),REQDEST                                                
         GOTO1 DIRREAD                                                          
         TM    P3,X'10'            ID NOT FOUND?                                
         BO    DISP0600            LEAVE BLANK                                  
*                                                                               
         CLI   P3,0                                                             
         BNE   DISP0600            ERROR. LEAVE BLANK.                          
*                                                                               
         L     R3,AIOWORK                                                       
         USING CTIREC,R3                                                        
         LA    R1,CTIDATA                                                       
         DROP  R3                                                               
DISP0540 CLI   0(R1),X'02'         PRINCIPAL ID ELEMENT                         
         BNE   DISP0580                                                         
         ZIC   RF,1(R1)                                                         
         SH    RF,=H'3'            EL CODE/EL LEN/1 FOR THE EX                  
         EX    RF,DISP0560                                                      
         FOUT  RQSDESTH                                                         
         B     DISP0600                                                         
DISP0560 MVC   RQSDEST(0),2(R1)    DEST TO SCREEN (EXECUTED)                    
*                                                                               
DISP0580 ZIC   RF,1(R1)            NEXT ELEMENT                                 
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BNE   DISP0540                                                         
*                                                                               
DISP0600 GOTO1 ASETREP             POINT BACK TO REP FILE.                      
*                                                                               
DISP0620 LA    R2,RQSOUTH                                                       
         OI    4(R2),II1C                                                       
         MVC   8(L'REQOUT,R2),REQOUT  OUTPUT FORMAT                             
         FOUT  (R2)                                                             
*                                                                               
         LA    R3,REQMAP           UNPROTECTED FIELD MAP                        
*                                                                               
*   TEST                                                                        
         XC    HALF1,HALF1         CLEAR A COUNTER                              
DISP0640 EQU   *                                                                
         CLC   =XL2'00',0(R3)       END OF FIELDS?                              
         BE    DISP0700                                                         
*                                                                               
         ST    R3,AMAPNTRY                                                      
*                                                                               
         L     R2,MAPFLD(R3)       A(FIELD HEADER TO DISPLAY)                   
         AR    R2,RA                 RELATIVE TO A(TWA)                         
*                                                                               
*- CALL DISPLAY ROUTINE DEFINED BY REQMAP ENTRY.                                
DISP0660 EQU   *                                                                
         XC    KEY,KEY             CLEAR FOR EACH DISPLAY RTN                   
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,MAPVAL(R3)     DISPLAY ROUTINE NUMBER                       
         GOTO1 GOINDEX,P1,(R4),FLDVAL,FLDVALX                                   
*                                                                               
DISP0680 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        ZICM  RF,HALF1,2                                                       
*        LA    RF,1(RF)                                                         
*        STCM  RF,3,HALF1                                                       
*        CH    RF,=H'10'           TEN FIELDS PROCESSED?                        
*        BNE   *+6                 NO                                           
*        DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         LA    R3,MAPLNTRY(R3)     NEXT REQMAP ENTRY                            
         B     DISP0640                                                         
*                                                                               
*- ALL FIELDS DISPLAYED.                                                        
DISP0700 EQU   *                                                                
         LA    RE,RQSACTH          POINT TO ACTION                              
         ST    RE,CURSPOS                                                       
*                                                                               
         MVI   ACTION,EQADD        LOOK LIKE AN ADD                             
         MVI   LASTACT,EQADD                                                    
         MVC   DAONSCRN,SAVEDA     TO ALLOW CHANGE AFTER DISPLAY                
*                                                                               
         MVC   RQSMSG(L'REQDISP),REQDISP                                        
*                                                                               
         MVC   RQSACT,SPACES       BLANK ACTION FOR SELECT                      
         CLI   SELECTSW,C'S'                                                    
         BE    DISP0720                                                         
*                                                                               
         MVC   RQSACT,=CL8'CHANGE' SELECTED FOR CHANGE                          
         L     R2,REQMAP+MAPFLD                                                 
         AR    R2,RA               A(1ST UNPROT FLD)                            
         ST    R2,CURSPOS                                                       
*                                                                               
DISP0720 EQU   *                                                                
         FOUT  RQSACTH                                                          
         MVI   SELECTSW,0          PREVENT LOOP IN BASE.                        
         SPACE 2                                                                
DISP0740 B     COMMGOOD            GOOD CC                                      
         TITLE 'GOINDEX - CALL RTN VIA INDEX'                                   
*                                                                               
*- GOINDEX - CALL ROUTINE VIA INDEX NUMBER AND ADDRESS LIST                     
*            WITH BRANCH-PAST-LIST CHECKING                                     
*                                                                               
*  REGISTERS RE, RF, R0 AND R1 USED INTERNALLY. ALL OTHERS UNCHANGED            
*                                                                               
*  INPUT:  P1 = INDEX NUMBER (4 BYTE BINARY, 1ST RTN = 1)                       
*          P2 = A(ADDRESS LIST TO INDEX)                                        
*          P3 = A(END OF ADDRESS LIST)                                          
GOINDEX  NTR1                                                                   
         LM    RE,R0,0(R1)         PICK UP IPT PARMS                            
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                INDEX NUMBER CAN'T BE 0                      
*                                                                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                A(ADDRESS LIST) CAN'T BE 0                   
*                                                                               
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                A(END OF ADDRESS LIST) CAN'T BE 0            
*                                                                               
         BCTR  RE,0                INDEX # -1                                   
         SLL   RE,2                * 4 = DISPLACEMENT                           
*                                                                               
         AR    RF,RE               START OF LIST + DISP = ENTRY                 
         CR    RF,R0                                                            
         BL    *+6                                                              
         DC    H'0'                INDEX EXTENDS PAST END OF LIST               
*                                                                               
         L     RF,0(RF)            PICK UP A(ROUTINE FROM LIST)                 
         A     RF,SUBRELO          RELOCATE                                     
         BASR  RE,RF                                                            
         B     COMMXIT                                                          
         TITLE 'GENEX -- KEY VALIDATOR/EXPANSION DISPLAY'                       
*                                                                               
*- GENEX - GENERIC KEY AND RECORD READER WITH OPTIONAL EXPANSION MOVE.          
*          ALSO HAS OPTIONAL REQUEST CARD TO SCREEN FIELD MOVE.                 
*                                                                               
*  <<CLONED FROM GENEX IN VALIDATION MODULE>>                                   
*                                                                               
*  ** EXPANSION NOTE **                                                         
*     DATA LENGTH IN RECORD MUST BE AT LEAST AS LONG AS EXPANSION               
*     FIELD LENGTH.  DATA ONLY MOVED FOR LENGTH OF FIELD.                       
*                                                                               
*  INPUT: AMAPNTRY - A(REQMAP ENTRY) FOR THIS FIELD                             
*         KEY      - KEY TO FIND                                                
*         P1       - BYTE 1 - I/O CONTROL                                       
*                      0 = EXACT HIT ONLY (REP IS HI IN KEY)                    
*                      1 = LOOK FOR 'ZZ' REP (REP IS LOW IN KEY)                
*                    BYTES 2-4 - EXPANSION DISPLACEMENT IN RECORD               
*                                                                               
*         P2       - NOT IN USE (RESIDUAL FROM VAL GENX)                        
*         P3       - A(REQUEST CARD FIELD) - FILLED IN IF VALID DATA.           
*         P4       - LENGTH OF DATA TO MOVE                                     
*         P5       - LENGTH OF EXPANSION DATA. 0 = USE FIELD LENGTH.            
*         P6       - NOT IN USE. MUST BE 0                                      
*                                                                               
GENEX    NTR1  WORK=(R4,3)                                                      
*                                                                               
*- MOVE INPUT PARMS TO TEMP. WORK AREA                                          
         MVC   0(24,4),0(R1)       P1-P6                                        
*                                                                               
*- PARAMETERS NOT CURRENTLY IN USE MUST BE 0                                    
         CLC   =F'0',P6-P1(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                P6 NOT 0                                     
*                                                                               
*- IF FIELD IS BLANK ON RQST CARD, JUST EXIT.                                   
         L     RF,P3-P1(R4)        RF=A(REQ CARD FLD)                           
         ICM   R1,15,P4-P1(R4)     LENGTH                                       
         BZ    GENEX010            NO TEST ON 0 LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,GENEX005                                                      
         BE    COMMGOOD            ALL BLANK                                    
         B     GENEX010                                                         
         SPACE                                                                  
GENEX005 CLC   0(0,RF),SPACES      DATA ON CARD?                                
*                                                                               
GENEX010 ZIC   R2,0(R4)            I/O CONTROL BYTE                             
         L     R3,0(R4)            RECORD EXPANSION DISPLACEMENT                
GENEX020 GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    GENEX100            FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
*                                                                               
*- KEY NOT ON FILE.  SHOULD WE LOOK FOR 'ZZ' REP?                               
         LTR   R2,R2                                                            
         BZ    COMMGOOD            NO. JUST GET OUT                             
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         MVC   KEY(27),KEYSAVE                                                  
         SR    R2,R2               GET OUT IF NO HIT THIS TIME                  
         B     GENEX020                                                         
*                                                                               
*- KEY IS VALID.  MOVE DATA FROM REQUEST CARD TO SCREEN.                        
GENEX100 EQU   *                                                                
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         L     RF,P3-P1(R4)        RF=A(REQ CARD FLD)                           
         ICM   R1,15,P4-P1(R4)     LENGTH                                       
         BZ    GENEX200            NO MOVE ON 0 LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,GENEX120                                                      
         B     GENEX200                                                         
         SPACE                                                                  
GENEX120 MVC   8(0,R2),0(RF)       MOVE DATA TO SCREEN                          
         SPACE                                                                  
*                                                                               
*- IF FIELD HAS EXPANSION, READ IN RECORD                                       
*  AND MOVE EXPANSION TO SCREEN, ELSE EXIT WITH GOOD CC.                        
GENEX200 EQU   *                                                                
         L     R2,AMAPNTRY                                                      
         TM    MAPCNTL(R2),FCXPAN                                               
         BZ    COMMGOOD            NO EXPANSION. JUST EXIT.                     
*                                                                               
         GOTO1 FILREAD,P1                                                       
*                                                                               
         L     R2,MAPFLD(R2)       FIND EXPANSION FIELD HEADER                  
         AR    R2,RA                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         L     RF,AIOWORK          RECORD IS HERE                               
         AR    R3,RF               R3 = A(EXPANSION DATA IN RECORD)             
*                                                                               
         ICM   RE,15,P5-P1(R4)     EXPANSION LENGTH                             
         BNZ   GENEX220                                                         
*                                                                               
         ZIC   RE,0(R2)            USE FIELD LENGTH AS EXPANSION LENGTH         
         LA    RF,8                                                             
         TM    1(R2),X'02'         EXTENDED FIELD?                              
         BZ    *+8                                                              
         LA    RF,8(RF)                                                         
         SR    RE,RF                                                            
*                                                                               
GENEX220 BCTR  RE,0                LESS 1 FOR EX                                
*                                                                               
         EX    RE,GENEX900         FROM RECORD TO EXPAN. FLD                    
*                                                                               
         B     COMMGOOD            0 = GOOD                                     
         SPACE                                                                  
GENEX900 MVC   8(0,R2),0(R3)       MOVE EXPANSION TO SCREEN                     
         SPACE 2                                                                
*                                                                               
*- MOVENB -- MOVE NON-BLANKS                                                    
*                                                                               
*  INPUT R0 = NUMBER OF CHARACTERS TO MOVE  (BINARY 1, 2, 3, ETC)               
*        R1 = A(SOURCE)  'FROM'                                                 
*        RE = LINK REG                                                          
*        RF = A(TARGET)  'TO'.  POINTS TO NEXT AVAILABLE BYTE AT END            
MOVENB   EQU   *                                                                
         CLI   0(R1),C' '          STOP ON BLANK                                
         BER   RE                                                               
         MVC   0(1,RF),0(R1)       MOVE SOURCE TO TARGET                        
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,MOVENB           LOOP BACK FOR NEXT BYTE                      
         BR    RE                                                               
         TITLE 'INDIVIDUAL FIELD DISPLAY ROUTINES'                              
*                                                                               
*- REGION DISPLAY                                                               
OFFRVAL  NTR1                                OFFICE REGION - FIND BITS          
         MVI   KEY,X'03'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),ROFFR                                                  
         GOTO1 GENEX,P1,(0,RREGNAME-RREGREC),KEY+25,ROFFR,2,0,0                 
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- OFFICE DISPLAY                                                               
OFFVAL   NTR1                                OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   OFFVAL5                                                          
         CLC   =C'* ',ROFF         AND FIELD STARTS WITH *                      
         BNE   OFFVAL5                                                          
         MVI   8(R2),C'*'          INSERT OFFICE NAME                           
         MVC   9(4,R2),ROFFSET     INSERT SET NAME                              
         B     COMMXIT                                                          
OFFVAL5  EQU   *                                                                
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),ROFF                                                   
         GOTO1 GENEX,P1,(0,ROFFNAME-ROFFREC),KEY+25,ROFF,2,0,0                  
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- SALESPERSON DISPLAY                                                          
SMANVAL  NTR1                                OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   SMANVAL5                                                         
         CLC   =C'* ',RSMAN        AND FIELD STARTS WITH *                      
         BE    SMANVAL8                                                         
                                                                                
SMANVAL5 DS    0H                                                               
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),RSMAN                                                  
         GOTO1 GENEX,P1,(0,RSALNAME-RSALREC),KEY+24,RSMAN,3,0,0                 
         B     COMMXIT                                                          
                                                                                
SMANVAL8 DS    0H                                                               
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
         MVC   KEY+21(2),=C'SP'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    SMANVALX            FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
SMANVALX DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVI   8(R2),C'*'                                                       
         MVC   9(4,R2),RSETSAL                                                  
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
* - DEVELOPMENTAL SALESPERSON DISPLAY                                           
DEVSP    NTR1                                                                   
         MVI   KEY,X'3A'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),R2DEVSP                                                
         GOTO1 GENEX,P1,(0,RDSPNAME-RDSPREC),KEY+24,R2DEVSP,3,10,0              
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
* - DEVELOPMENTAL CONTRACT TYPE DISPLAY                                         
BUSTYP   NTR1                                                                   
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   BTYVAL5                                                          
         CLC   =C'* ',R2DEVCTY     AND FIELD STARTS WITH *                      
         BNE   BTYVAL5                                                          
         MVI   8(R2),C'*'          INSERT OFFICE NAME                           
         MVC   9(4,R2),RDTYSET     INSERT SET NAME                              
         B     COMMXIT                                                          
BTYVAL5  EQU   *                                                                
         MVI   KEY,X'3B'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),R2DEVCTY                                               
         GOTO1 GENEX,P1,(0,RDCTDESC-RDCTREC),KEY+25,R2DEVCTY,2,10,0             
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
* - RADAR SPOT LENGTH BREAK OUT OPTION                                          
SPLNDIS  NTR1                                                                   
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVC   8(1,R2),RSPLNOPT                                                 
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- ADVERTISER DISPLAY                                                           
ADVVAL   NTR1                                OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       IF RRG                                       
         BNE   ADVVAL10                                                         
         CLC   =C'*   ',RADV                                                    
         BE    ADVVAL20                                                         
                                                                                
ADVVAL10 DS    0H                                                               
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),RADV                                                   
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(1,RADVNAME-RADVREC),KEY+21,RADV,4,0,0                  
         B     COMMXIT                                                          
                                                                                
ADVVAL20 DS    0H                                                               
         BAS   RE,GETREP           CHECK IF REP A SUBSIDIARY                    
         BZ    ADVVAL30                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'             VALIDATE USING MASTER REP                  
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     ADVVAL40                                                         
                                                                                
ADVVAL30 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
ADVVAL40 DS    0H                                                               
         MVC   KEY+21(2),=C'AD'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    ADVVALX             FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
ADVVALX  DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVI   8(R2),C'*'                                                       
         MVC   9(4,R2),RSETADV                                                  
         B     COMMXIT                                                          
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*- PRODUCT DISPLAY                                                              
PRDVAL   NTR1                                OFFICE - FIND BITS                 
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RADV                                                   
         MVC   KEY+22(3),RPRO                                                   
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(1,RPRDNAME-RPRDREC),KEY+24,RPRO,3,0,0                  
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- PRODUCT CLASS DISPLAY                                                        
PROCLVAL NTR1                                                                   
         MVI   KEY,X'0D'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),RPROCL                                                 
         GOTO1 GENEX,P1,(0,RCLSNAME-RCLSREC),KEY+25,RPROCL,2,0,0                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- PRODUCT CATAGORY DISPLAY                                                     
PROCAVAL NTR1                                                                   
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),RPROCA                                                 
         GOTO1 GENEX,P1,(0,RCTGNAME-RCTGREC),KEY+25,RPROCA,2,0,0                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- DIVISION/TEAM DISPLAY                                                        
SDIVVAL  NTR1                                                                   
         MVI   KEY,X'05'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),RSDIV                                                  
         GOTO1 GENEX,P1,(0,RTEMDVNM-RTEMREC),KEY+25,RSDIV,2,0,0                 
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- AGENCY/AGENCY OFFICE.                                                        
*                                                                               
*  FORMAT:  AAAA,OO                                                             
*           AAAA-OO                                                             
*           AAAAOO       (POSITIONAL)                                           
*                                                                               
*  WHERE AAAA = 1-4 CHARACTER AGENCY CODE                                       
*        OO   = 1-2 OPTIONAL AGENCY OFFICE CODE. (DEFAULT TO BLANK)             
*                                                                               
AGYVAL   NTR1                                                                   
         CLI   RRRGFLAG,C'Y'       IF RRG                                       
         BNE   AGYV10                                                           
         CLC   =C'*     ',RAGY                                                  
         BE    AGYV20                                                           
         CLC   =C'+     ',RAGY                                                  
         BE    AGYV35                                                           
                                                                                
AGYV10   DS    0H                                                               
         CLC   RAGY,SPACES         FIELD ON CARD?                               
         BE    COMMXIT                                                          
*                                                                               
         LA    R0,L'RAGY                                                        
         LA    R1,RAGY                                                          
         LA    RF,8(R2)                                                         
         BAS   RE,MOVENB           MOVE NON-BLANKS                              
*                                                                               
         CLC   RAGYO,SPACES        AGENCY OFFICE?                               
         BE    AGYV15                                                           
         MVI   0(RF),C'-'                                                       
         MVC   1(L'RAGYO,RF),RAGYO                                              
*                                                                               
AGYV15   MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),RAGY                                                   
         MVC   KEY+23(2),RAGYO                                                  
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(1,RAGYNAM1-RAGYREC),0,0,0,0,0                          
         B     COMMXIT                                                          
                                                                                
AGYV20   DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVI   8(R2),C'*'                                                       
         MVC   9(4,R2),RSETAGY                                                  
         B     COMMXIT                                                          
                                                                                
AGYV35   DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVC   8(2,R2),=C'T='                                                   
         MVC   10(2,R2),RSETTER    INSERT TERRITORY FILTER                      
         B     COMMXIT                                                          
                                                                                
         EJECT                                                                  
*                                                                               
*- VALIDATE STATION                                                             
STAVAL   NTR1                                                                   
         CLI   RRRGFLAG,C'Y'       IF RRG                                       
         BNE   STAV0100                                                         
         CLC   =C'*    ',RSTA      SET REQUESTED?                               
         BNE   STAV0100            NO                                           
         BAS   RE,GETREP           YES - CHECK IF REP A SUBSIDIARY              
         BZ    STAV0030                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'             VALIDATE USING MASTER REP                  
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     STAV0040                                                         
                                                                                
STAV0030 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
STAV0040 DS    0H                                                               
         MVC   KEY+21(2),=C'ST'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    STAVALX             FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
STAVALX  DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVI   8(R2),C'*'                                                       
         MVC   9(4,R2),RSETSTA                                                  
         B     COMMXIT                                                          
*                                                                               
STAV0100 EQU   *                                                                
         LA    R1,RSTA             STANDARD STATION IN RQST                     
         BAS   RE,STAKEY           PARSE STA IPT/BLD KEY                        
         GOTO1 GENEX,P1,(0,RSTAMKT-RSTAREC),KEY+22,RSTA,0,0,0                   
*                                  DON'T MOVE ANY DATA                          
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- STAKEY -- DISPLAY STATION FIELD OUTPUT (UP TO 6 CHARS)                       
*            AND BUILD KEY FOR FILE READING.                                    
*                                                                               
*  INPUT: R1 = A(5 CHAR INTERNAL STATION CODE FOR DISPLAY)                      
*                                                                               
STAKEY   NTR1                                                                   
         LR    R3,R1               SAVE A(INPUT STATION)                        
*                                                                               
         LA    R0,4                                                             
         LA    RF,8(R2)                                                         
         BAS   RE,MOVENB           DISPLAY CALL LETTERS                         
*                                                                               
         CLI   4(R3),C' '          ANY BAND?                                    
         BE    STAKEY10                                                         
*                                                                               
         MVI   0(RF),C'-'          MOVE BAND TO SCREEN                          
         MVC   1(1,RF),4(R3)                                                    
*                                                                               
STAKEY10 MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),TWAAGY                                                 
         MVC   KEY+22(5),0(R3)                                                  
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- GROUP/SUBGROUP DISPLAY                                                       
GSGVAL   NTR1                                                                   
         CLI   RRRGFLAG,C'Y'       IF RRG                                       
         BNE   GSGVAL10                                                         
         CLC   =C'* ',RGROUP                                                    
         BE    GSGVAL20                                                         
                                                                                
GSGVAL10 DS    0H                                                               
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),RGROUP                                                 
         GOTO1 GENEX,P1,(0,RGRPNAME-RGRPREC),KEY+25,RGROUP,2,0,0                
         B     COMMXIT                                                          
                                                                                
GSGVAL20 DS    0H                                                               
         BAS   RE,GETREP                                                        
         BZ    GSGVAL30                                                         
         XC    KEY,KEY             IF SUBSIDIARY, VALIDATE USING MASTER         
         MVI   KEY,X'38'                                                        
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     GSGVAL40                                                         
                                                                                
GSGVAL30 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
GSGVAL40 DS    0H                                                               
         MVC   KEY+21(2),=C'GS'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    GSGVALX             FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
GSGVALX  DS    0H                                                               
         L     R2,AMAPNTRY                                                      
         L     R2,MAPFLD(R2)                                                    
         AR    R2,RA               A(OUTPUT FIELD HEADER)                       
*                                                                               
         MVI   8(R2),C'*'                                                       
         MVC   9(4,R2),RSETGSG                                                  
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*- CONTRACT NUMBER                                                              
CONVAL   NTR1                                                                   
         MVC   8(L'RCON,R2),RCON                                                
         B     COMMGOOD            GOOD CC                                      
         SPACE 3                                                                
*                                                                               
*- AFFILIATE                                                                    
AFFVAL   NTR1                                                                   
         MVC   8(3,R2),R2AFFL                                                   
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- CONTRACT TYPE.                                                               
*  1 OR 2 CHAR INPUT.   'T' OR '*T'  (*=EXCLUDE)                                
*                                                                               
*  EXCLUDES PASSED AS LOWER CASE.                                               
*                                                                               
CTYPVAL  NTR1                                                                   
         CLI   RCONTYP,C' '                                                     
         BE    COMMGOOD                                                         
*                                                                               
         CLC   RCTYSET,SPACES      ANY VALUE IN SET FIELD?                      
         BE    CTYP0020            NO                                           
         OC    RCTYSET,RCTYSET     ANY VALUE IN SET FIELD?                      
         BZ    CTYP0020            NO                                           
         MVI   8(R2),C'*'          INSERT SET INDICATOR                         
         MVC   9(4,R2),RCTYSET     INSERT SET NAME                              
         B     COMMGOOD                                                         
CTYP0020 EQU   *                                                                
         MVC   8(1,R2),RCONTYP     ASSUME INCLUDE                               
         TM    RCONTYP,X'40'                                                    
         BO    COMMGOOD                                                         
*                                                                               
         MVI   8(R2),C'*'          EXCLUDE                                      
         MVC   9(1,R2),RCONTYP                                                  
         OI    9(R2),X'40'                                                      
         B     COMMGOOD            OK.                                          
         EJECT                                                                  
*                                                                               
*- TVB REGION FIELD                                                             
*                                                                               
*- TVB REGION CODE.  'XX' MUST BE IN TVBLST.                                    
*                                                                               
TVBVAL   NTR1                                                                   
*                                                                               
         MVC   8(2,R2),RTVB                                                     
*                                                                               
         B     COMMGOOD                                                         
*                                                                               
**    NOTE                                                                      
*     THIS ROUTINE IS NO LONGER USED.  IT HAS BEEN LOADING THE                  
*     EXPANSION VALUE WHETHER A CODE IS PRESENT OR NOT, AND HAS                 
*     BEEN INSERTING MISCELLANEOUS JUNK.                                        
*                                                                               
         LA    RE,TVBLST                                                        
TVBV10   EQU   *                                                                
         CLC   8(2,R2),0(RE)       SCREEN MATCHES TABLE?                        
         BE    TVBV20                                                           
         LA    RE,20(RE)           NEXT TVB LIST ENTRY                          
         B     TVBV10                                                           
TVBV20   EQU   *                                                                
*- PUT UP TVB EXPANSION                                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         FOUT  (R2)                                                             
         LR    R3,RE               SAVE A(TVB TBL ENTRY) FROM GOTO1             
         GOTO1 ACLRSCRN,P1,(R2),(R2)                                            
         MVC   8(18,R2),2(R3)                                                   
         B     COMMGOOD                                                         
         SPACE 3                                                                
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
*                                                                               
*- MARKET CODE.  MUST BE ON FILE.                                               
MKTVAL   NTR1                                                                   
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   MKTVAL5                                                          
         CLC   =C'* ',RMKT         AND FIELD STARTS WITH *                      
         BNE   MKTVAL5                                                          
         MVI   8(R2),C'*'          INSERT SET FLAG                              
         MVC   9(4,R2),RSETMKT     INSERT SET NAME                              
         B     COMMXIT                                                          
*                                                                               
MKTVAL5  EQU   *                                                                
         MVI   KEY,X'2B'                                                        
         MVC   KEY+21(2),TWAAGY                                                 
         MVC   KEY+23(4),RMKT                                                   
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),RMKT,4,0,0                   
         B     COMMXIT                                                          
         SPACE 3                                                                
*                                                                               
*- OWNER CODE.  MUST BE ON FILE.                                                
OWNERVAL NTR1                                                                   
*                                                                               
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),ROWNER                                                 
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),ROWNER,3,0,0                 
         B     COMMXIT                                                          
         SPACE 3                                                                
*                                                                               
*- POINT PERSON CODE.  MUST BE ON FILE.                                         
PPSONVAL NTR1                                                                   
*                                                                               
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   PPRVAL5                                                          
         CLC   =C'* ',RPPSON       AND FIELD STARTS WITH *                      
         BNE   PPRVAL5                                                          
         MVI   8(R2),C'*'          INSERT OFFICE NAME                           
         MVC   9(4,R2),RPPRSET     INSERT SET NAME                              
         B     COMMXIT                                                          
PPRVAL5  EQU   *                                                                
         MVI   KEY,X'31'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),RPPSON                                                 
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),RPPSON,3,0,0                 
         B     COMMXIT                                                          
         SPACE 3                                                                
*                                                                               
*- BUDGET YEAR.  MUST BE 00-99 INCLUSIVE.                                       
BYVAL    NTR1                                                                   
*                                                                               
         MVC   8(2,R2),ROPTN4                                                   
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*                                                                               
*- OLD REP                                                                      
OLDREP   NTR1                                                                   
*                                                                               
         MVC   8(7,R2),RNCNUM      INSERT OLD REP CODE INTO SCREEN              
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*- SOURCE STATION                                                               
TKOSTAC  NTR1                                                                   
*                                                                               
         MVC   8(5,R2),RSTA        INSERT STATION CODE INTO SCREEN              
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*- SOURCE AGENCY FILTER                                                         
TKOAGYC  NTR1                                                                   
*                                                                               
         MVC   8(6,R2),RAGY        INSERT AGENCY  CODE INTO SCREEN              
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*- SOURCE OFFICE FILTER                                                         
TKOOFFC  NTR1                                                                   
*                                                                               
         MVC   8(2,R2),ROFF        INSERT OFFICE  CODE INTO SCREEN              
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*- CLOSED THRU MONTH                                                            
CLOSMON  NTR1                                                                   
*                                                                               
         MVC   8(4,R2),R2CLOSMN    INSERT CLOSED THRU MONTH                     
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*                                                                               
*- MARKET RANK.  MUST BE 1-7 INCLUSIVE.                                         
RANKVAL  NTR1                                                                   
*                                                                               
         MVC   8(1,R2),RRANK                                                    
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*                                                                               
*- NETWORK CONTRACT NUMBER VALIDATION                                           
NCONVAL  NTR1                                                                   
*                                                                               
         MVC   8(8,R2),RNCNUM                                                   
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- RANK MAX VALUE.  NUMERIC 1-99.  PASSED AS OPTION 1 AND OPTION 2              
*                                                                               
RKMAXVAL NTR1                      RANK MAX                                     
         CLC   =C'1K',RNUM         STRATEGY SEEDER REPORT?                      
         BNE   RKMX0020            NO                                           
         MVC   8(2,R2),RRANKMAX    YES - SET SCREEN FROM ALT AREA               
         B     COMMGOOD                                                         
RKMX0020 EQU   *                                                                
         MVC   8(2,R2),ROPTN                                                    
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- DEPTH.  NUMERIC, 1-99.  ** CARD 2 FIELD **                                   
*                                                                               
DEPTHV   NTR1                                                                   
         MVC   8(2,R2),RDEPTH                                                   
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- CREDIT RATING  ** CARD 2 FIELD **                                            
*                                                                               
CREDRAT  NTR1                                                                   
         MVC   8(1,R2),R2CREDR                                                  
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- LIABILITY POSITION COMMENT  ** CARD 2 FIELD **                               
*                                                                               
LIABPOS  NTR1                                                                   
         MVC   8(2,R2),R2LIABP                                                  
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- ALCLSTV --- ALLOCATION LIST DISPLAY                                          
*                                                                               
ALCLSTV  NTR1                                                                   
*                                                                               
         MVC   8(5,R2),SPACES                                                   
*                                                                               
         L     RE,AMAPNTRY                                                      
         TM    MAPCNTL(RE),FCLIST                                               
         BNZ   ALCLST10                                                         
         MVI   COUNTER,0           INIT BINARY COUNTER                          
*                                                                               
ALCLST10 EQU   *                                                                
         ZIC   RE,COUNTER          BUMP # ITEMS IN LIST                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNTER                                                       
*                                                                               
         BCTR  RE,0                FIND A(ALLOCATION PAIR) IN LIST              
         MH    RE,=H'4'            4 BYTES/PAIR                                 
         LA    R1,R2ALCLST                                                      
         AR    R1,RE               A(ALLOCATION PAIR FOR DISPLAY)               
*                                                                               
         CLI   0(R1),0                                                          
         BE    COMMGOOD                                                         
         CLI   0(R1),X'40'                                                      
         BE    COMMGOOD                                                         
*                                                                               
         MVC   8(1,R2),0(R1)                                                    
         MVI   9(R2),C','                                                       
         EDIT  (C3,1(R1)),(3,10(R2)),ALIGN=LEFT                                 
*                                                                               
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- ALCLST2 --- ALLOCATION RESET LIST DISPLAY                                    
*                                                                               
ALCLST2  NTR1                                                                   
*                                                                               
         MVC   8(1,R2),SPACES                                                   
*                                                                               
         L     RE,AMAPNTRY                                                      
         TM    MAPCNTL(RE),FCLIST                                               
         BNZ   ALCLS210                                                         
         MVI   COUNTER,0           INIT BINARY COUNTER                          
*                                                                               
ALCLS210 EQU   *                                                                
         ZIC   RE,COUNTER          BUMP # ITEMS IN LIST                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNTER                                                       
*                                                                               
         BCTR  RE,0                FIND A(ALLOCATION PAIR) IN LIST              
         MH    RE,=H'4'            4 BYTES/PAIR                                 
         LA    R1,R2ALCLST                                                      
         AR    R1,RE               A(ALLOCATION PAIR FOR DISPLAY)               
*                                                                               
         CLI   0(R1),0                                                          
         BE    COMMGOOD                                                         
         CLI   0(R1),X'40'                                                      
         BE    COMMGOOD                                                         
*                                                                               
         MVC   8(1,R2),0(R1)                                                    
*                                                                               
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- STATION LIST DISPLAY.      ** CARD 2 FIELDS **                               
*                                                                               
*  BUILDS RSTALST, LIST OF 5 CHAR INTERNAL STATION CODES.                       
*  SETS   RSTALST# TO NUMBER OF ITEMS IN LIST.                                  
*                                                                               
*  ON 1ST FIELD OF LIST, 'FCLIST' BIT SHOULD BE OFF                             
*     (INITIALIZE LIST COUNT)                                                   
*  ALL FOLLOWING FIELDS SHOULD HAVE 'FCLIST' BIT ON.                            
*                                                                               
STALSTV  NTR1                                                                   
         L     RE,AMAPNTRY                                                      
         TM    MAPCNTL(RE),FCLIST                                               
         BNZ   STALST10                                                         
         MVI   COUNTER,0           INIT BINARY COUNTER                          
*                                                                               
STALST10 EQU   *                                                                
         ZIC   RE,COUNTER          BUMP # ITEMS IN LIST                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNTER                                                       
*                                                                               
         BCTR  RE,0                FIND A(STA) IN LIST                          
         MH    RE,=H'5'            5 BYTES/STATION                              
         LA    R1,RSTALST                                                       
         AR    R1,RE               A(STATION CODE FOR DISPLAY)                  
*                                                                               
         BAS   RE,STAKEY           DISPLAY CALL LETTERS                         
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*  REPORT 30 (RADAR REPORT) DISPLAY ROUTINES                                    
*                                                                               
YADREC   NTR1                                                                   
         MVC   8(8,R2),RYADREC                                                  
         B     COMMGOOD                                                         
         SPACE 5                                                                
LABREC   NTR1                                                                   
         MVC   8(8,R2),RLBLREC                                                  
         B     COMMGOOD                                                         
         SPACE 5                                                                
TARGAGY  NTR1                                                                   
         MVC   8(6,R2),RTAGY       REDISPLAY AGENCY/OFFICE                      
         B     COMMGOOD                                                         
         SPACE 5                                                                
RSERV    NTR1                                                                   
         MVC   8(1,R2),R2RSVC      REDISPLAY RATING SERVICE                     
         B     COMMGOOD                                                         
         SPACE 5                                                                
RBOOK    NTR1                                                                   
         MVC   8(6,R2),R2RBOOK     REDISPLAY RATING BOOK                        
         B     COMMGOOD                                                         
         SPACE 5                                                                
DEMOCD   NTR1                                                                   
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 (RF),(R1),0         GO TO CALLOV                                 
         MVC   DEMOCON,DMCB        SAVE MODULE ADDRESS                          
*                                                                               
         LA    R6,DBDEMOB          A(DEMO INTERFACE AREA)                       
         USING DEMOD,R6                                                         
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR USE                    
*                                                                               
         L     RF,APARM            LOAD PARAM LIST FROM MONITOR                 
         L     RF,16(RF)           SET A(COMFACS)                               
         ST    RF,DBCOMFCS         INSERT A(COMFACS) INTO DBLOCK                
*                                                                               
         MVC   DBSELAGY,TWAAGY     INSERT USER CODE                             
         MVC   DBFILE,=C'PAV'      INSERT DATA TYPE                             
         MVI   DBSELMED,C'T'       INSERT MEDIA                                 
*                                                                               
*   IF THIS IS TO BE USED FOR RADIO, THIS WILL REQUIRE REVISION                 
*      TO HANDLE THE GEOGRAPHICAL QUALIFIER                                     
*                                                                               
         XC    DEMWORK,DEMWORK     SET UP WORKSPACE                             
         MVI   DEMWORK+3,X'FF'     SET STRING DELIMITER                         
         MVI   DEMWORK,X'00'       INSERT GEOGRAPHICAL QUALIFIER                
         MVC   DEMWORK+1(1),R2DEMO+1                                            
         CLI   R2DEMO+1,C'T'       CHANGE 'T' TO 'X'                            
         BNE   DECO0010            NO  - LEAVE AS IS                            
         MVI   DEMWORK+1,C'I'      YES - CHANGE IT                              
DECO0010 EQU   *                                                                
         XC    WORK,WORK           SET UP WORKSPACE                             
         PACK  WORK(8),R2DEMO+2(3)                                              
         CVB   R3,WORK             TRANSLATE DEMO NUMBER TO BINARY              
         STC   R3,DEMWORK+2        INSERT INTO WORK AREA                        
*                                                                               
         GOTO1 DEMOCON,P1,(1,DEMWORK),(6,8(R2)),DBLOCK                          
         B     COMMGOOD                                                         
         SPACE 5                                                                
*                                                                               
*- DATE DISPLAY ROUTINES.                                                       
*                                                                               
*  AREQMAP = A(CURRENT ENTRY)                                                   
*  R2      = A(FIELD HEADER)                                                    
*                                                                               
*  FIELDS WHERE 2 DATES MAY BE ENTERED, SCANNER IS USED FOR PARSE.              
*                                                                               
*  DELIMITER MAY BE ',' OR '-' BETWEEN DATES.                                   
*                                                                               
*  FORMAT BITS:  X'04' = MMMDD/YY FORMAT                                        
*                X'08' = MMM/YY   FORMAT                                        
*                X'80' = DEFAULT END TO START DATE                              
*                X'40' = DEFAULT START TO END DATE                              
*                X'20' = DEFAULT 1ST DATE TO TODAY                              
*                X'10' = DEFAULT 2ND DATE TO TODAY                              
*                                                                               
         SPACE                                                                  
*                                                                               
*- START DATE ONLY                                                              
STRDVAL  NTR1                                                                   
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RSTRD,1                                           
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- END DATE ONLY                                                                
*  IF START DATE GIVEN, IT MAY NOT PRECEED END DATE                             
ENDDVAL  NTR1                                                                   
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RENDD,2                                           
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- START,END DATES                                                              
STRTENDV NTR1                                                                   
*                                                                               
         CLC   RSTRD,SPACES                                                     
         BE    STEV10                                                           
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RSTRD,1                                           
*                                                                               
*- IF START = END OR END = BLANK, STOP                                          
STEV10   CLC   RSTRD,RENDD                                                      
         BE    COMMGOOD                                                         
         CLC   RENDD,SPACES                                                     
         BE    COMMGOOD                                                         
*                                                                               
*- PUT OUT DELIMITER                                                            
         LA    R4,8(R2)                                                         
         LA    R0,8                MAX DATE SIZE                                
STEV20   CLI   0(R4),C' '                                                       
         BE    STEV40                                                           
         CLI   0(R4),0                                                          
         BE    STEV40                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,STEV20                                                        
STEV40   CLC   RSTRD,SPACES        NO DELIM FOR END DATE ONLY                   
         BE    STEV50                                                           
         MVI   0(R4),C'-'          DATE DELIMITER                               
         LA    R4,1(R4)            A(OUTPUT)                                    
*                                                                               
STEV50   GOTO1 DODATE,P1,(R4),RENDD,2                                           
         B     COMMGOOD            OK                                           
         SPACE 2                                                                
*                                                                               
*- START,END DATES EXTRA SET OF DATES                                           
EXTRADAT NTR1                                                                   
*                                                                               
         CLC   RXTRAFRM,SPACES                                                  
         BE    EXTV10                                                           
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RXTRAFRM,1                                        
*                                                                               
*- IF START = END OR END = BLANK, STOP                                          
EXTV10   CLC   RXTRAFRM,RXTRATO                                                 
         BE    COMMGOOD                                                         
         CLC   RXTRATO,SPACES                                                   
         BE    COMMGOOD                                                         
*                                                                               
*- PUT OUT DELIMITER                                                            
         LA    R4,8(R2)                                                         
         LA    R0,8                MAX DATE SIZE                                
EXTV20   CLI   0(R4),C' '                                                       
         BE    EXTV40                                                           
         CLI   0(R4),0                                                          
         BE    EXTV40                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,EXTV20                                                        
EXTV40   CLC   RXTRAFRM,SPACES     NO DELIM FOR END DATE ONLY                   
         BE    EXTV50                                                           
         MVI   0(R4),C'-'          DATE DELIMITER                               
         LA    R4,1(R4)            A(OUTPUT)                                    
*                                                                               
EXTV50   GOTO1 DODATE,P1,(R4),RXTRATO,2                                         
         B     COMMGOOD            OK                                           
         SPACE 2                                                                
*                                                                               
*- CUTOFF DATE                                                                  
CUTOFF   NTR1                                                                   
*                                                                               
         CLC   RSTRD,SPACES                                                     
         BE    CUT010                                                           
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RSTRD,1                                           
CUT010   EQU   *                                                                
         B     COMMGOOD                                                         
*                                                                               
*- IF START = END OR END = BLANK, STOP                                          
*                                                                               
*- BUDGET START,END DATES                                                       
BUDSTED  NTR1                                                                   
*                                                                               
         CLC   R2BUDST,SPACES                                                   
         BE    BUDD10                                                           
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),R2BUDST,1                                         
*                                                                               
*- IF START = END OR END = BLANK, STOP                                          
BUDD10   CLC   R2BUDST,R2BUDED                                                  
         BE    COMMGOOD                                                         
         CLC   R2BUDED,SPACES                                                   
         BE    COMMGOOD                                                         
*                                                                               
*- PUT OUT DELIMITER                                                            
         LA    R4,8(R2)                                                         
         LA    R0,8                MAX DATE SIZE                                
BUDD20   CLI   0(R4),C' '                                                       
         BE    BUDD40                                                           
         CLI   0(R4),0                                                          
         BE    BUDD40                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,BUDD20                                                        
BUDD40   CLC   R2BUDST,SPACES        NO DELIM FOR END DATE ONLY                 
         BE    BUDD50                                                           
         MVI   0(R4),C'-'          DATE DELIMITER                               
         LA    R4,1(R4)            A(OUTPUT)                                    
*                                                                               
BUDD50   GOTO1 DODATE,P1,(R4),R2BUDED,2                                         
         B     COMMGOOD            OK                                           
         SPACE 2                                                                
*                                                                               
*- AS AT DATE                                                                   
ASADVAL  NTR1                                                                   
         CLC   RASAD,SPACES        ANY AS-AT START DATE?                        
         BE    COMMGOOD            NO  -                                        
         LA    R4,8(R2)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),RASAD,1                                           
*                                                                               
         CLC   R2ASAT2,SPACES      ANY AS-AT END DATE?                          
         BE    COMMGOOD            NO  -                                        
*                                                                               
*- PUT OUT DELIMITER                                                            
         LA    R4,8(R2)                                                         
         LA    R0,8                MAX DATE SIZE                                
ASADV04  EQU   *                                                                
         CLI   0(R4),C' '                                                       
         BE    ASADV08                                                          
         CLI   0(R4),0                                                          
         BE    ASADV08                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,ASADV04                                                       
ASADV08  EQU   *                                                                
         MVI   0(R4),C'-'          DATE DELIMITER                               
         LA    R4,1(R4)            A(OUTPUT)                                    
         GOTO1 DODATE,P1,(R4),R2ASAT2,2                                         
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- DODATE -- VALIDATE 1 DATE.                                                   
*                                                                               
*  INPUT:  P1 = A(OUTPUT AREA)                                                  
*          P2 = A(INPUT DATE FROM CARD)  YYMMDD OR YYMM FORMAT                  
*                                                                               
*  FORMAT BITS:  X'04' = MMMDD/YY FORMAT                                        
*                X'08' = MMM/YY   FORMAT                                        
*                                                                               
DODATE   NTR1                                                                   
         L     R1,P2                                                            
         CLC   SPACES(6),0(R1)     DATE ON CARD?                                
         BE    COMMGOOD                                                         
*                                                                               
         L     R3,AMAPNTRY                                                      
         LA    R2,5                ASSUME MMMDD/YY FORMAT                       
         TM    MAPFMT(R3),X'04'                                                 
         BO    DD50                                                             
         LA    R2,6                TRY FOR MMM/YY FORMAT                        
         TM    MAPFMT(R3),X'08'                                                 
         BO    DD50                                                             
         DC    H'0'                INVALID FORMAT                               
DD50     EQU   *                                                                
         LM    R3,R4,P1                                                         
         GOTO1 DATCON,P1,(0,(R4)),((R2),(R3))                                   
         B     COMMGOOD            DATE IS OK.                                  
         SPACE 2                                                                
*                                                                               
*- 12 MONTH PERIOD RTN                                                          
*                                                                               
*  JUST DISPLAY END DATE.                                                       
*                                                                               
PERDVAL  EQU   *                                                                
         B     ENDDVAL             DISPLAY END DATE                             
         EJECT                                                                  
*                                                                               
*- BASIS DISPLAY                                                                
BASISVAL EQU   *                                                                
         MVC   8(1,R2),RBASIS                                                   
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- STATION TYPE DISPLAY                                                         
STATVAL  EQU   *                                                                
         MVC   8(1,R2),RSTAT                                                    
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- ACCOUNTING OPTION DISPLAY                                                    
AOPTVAL  EQU   *                                                                
         CLC   =C'PX',ROPTN4      'PX' REPORT?                                  
*****>   CLC   =C'PR',ROPTN4      'PR' REPORT?                                  
*                                                                               
*   PROGRAM IDENTIFIER IS IN ROPTN4+5 FOR RRG REPORTS                           
*                                                                               
         BNE   AOVA0010            NO                                           
         ST    RE,DUB              SAVE EXIT ADDRESS                            
         BAS   RE,RESETDIS         YES - RESET DISPLAY VALUES                   
         L     RE,DUB              RESTORE EXIT ADDRESS                         
AOVA0010 EQU   *                                                                
         MVC   8(1,R2),RACCTOPT                                                 
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- READ SEQUENCE                                                                
SEQVAL   EQU   *                   SEQUENCE - FIND BITS                         
         MVC   8(1,R2),RSEQ                                                     
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- RECAP OPTIONS                                                                
RCPVAL   EQU   *                                                                
         CLC   =C'PX',ROPTN4      'PX' REPORT?                                  
*****>   CLC   =C'PR',ROPTN4      'PR' REPORT?                                  
*                                                                               
*   PROGRAM IDENTIFIER IS IN ROPTN4+5 FOR RRG REPORTS                           
*                                                                               
         BNE   RCVA0010            NO                                           
         ST    RE,DUB              SAVE EXIT ADDRESS                            
         BAS   RE,RESETDIS         YES - RESET DISPLAY VALUES                   
         L     RE,DUB              RESTORE EXIT ADDRESS                         
RCVA0010 EQU   *                                                                
         MVC   8(1,R2),RACCTOPT                                                 
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- TOTALS CONTROL.  PASSED AS OPTION 2                                          
TCNTVAL  EQU   *                                                                
         MVC   8(1,R2),ROPTN2                                                   
         B     OPTOK                                                            
         SPACE                                                                  
*                                                                               
*- OPTION DISPLAY                                                               
OPT1VAL  EQU   *                                                                
         CLC   =C'PX',ROPTN4      'PX' REPORT?                                  
*****>   CLC   =C'PR',ROPTN4      'PR' REPORT?                                  
*                                                                               
*   PROGRAM IDENTIFIER IS IN ROPTN4+5 FOR RRG REPORTS                           
*                                                                               
         BNE   O1VA0010            NO                                           
         ST    RE,DUB              SAVE EXIT ADDRESS                            
         BAS   RE,RESETDIS         YES - RESET DISPLAY VALUES                   
         L     RE,DUB              RESTORE EXIT ADDRESS                         
O1VA0010 EQU   *                                                                
         MVC   8(1,R2),ROPTN                                                    
         B     OPTOK                                                            
         SPACE                                                                  
OPT2VAL  EQU   *                                                                
         MVC   8(1,R2),ROPTN2                                                   
         B     OPTOK                                                            
         SPACE                                                                  
OPT3VAL  EQU   *                                                                
         MVC   8(1,R2),ROPTN3                                                   
         B     OPTOK                                                            
         SPACE                                                                  
OPT4VAL  EQU   *                                                                
         MVC   8(1,R2),ROPTN4                                                   
         B     OPTOK                                                            
         SPACE                                                                  
OPT5VAL  EQU   *                                                                
         MVC   8(1,R2),ROPTN5                                                   
         B     OPTOK                                                            
         SPACE                                                                  
OPT6VAL  EQU   *                                                                
         MVC   8(1,R2),R2OPT6      WAS ROPTN6                                   
         B     OPTOK                                                            
         SPACE                                                                  
OPTOK    CR    R0,R0               OK CC                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* FOR 'PR/PX' REPORT, SEE IF 'ROPTN' IS SET.  IF IT ISN'T, TRANSLATE            
*   'RACCOPT' TO DISPLAY VALUES OF 'RACCTOPT' + 'ROPTN'                         
*   ROUTINE WILL PERMIT ENTRY FROM EITHER ACCOUNTING OPTION OR                  
*   OPTION1 REDISPLAY FIRST.                                                    
*                                                                               
RESETDIS NTR1                                                                   
         CLI   ROPTN,C' '          ANY VALUE IN OPTION1?                        
         BNE   REDI0030            YES - ALREADY DONE.                          
         LA    RF,FORTABLE         NO  - SET A(TABLE)                           
REDI0010 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DUMP IT OUT                            
         CLC   RACCTOPT,0(RF)      ACCOUNT OPTION = TABLE?                      
         BE    REDI0020            YES                                          
         LA    RF,3(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     REDI0010            GO BACK FOR NEXT                             
REDI0020 EQU   *                                                                
         MVC   RACCTOPT,1(RF)      LOAD DISPLAY ACCTING OPTION                  
         MVC   ROPTN,2(RF)         LOAD DISPLAY OPTION 1                        
REDI0030 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*   TABLE SETUP:  EACH ENTRY CONTAINS THREE CHARACTERS:                         
*        CHAR  1  =  VALUE OF RACCTOPT                                          
*        CHAR  2  =  DISPLAY VALUE OF RACCTOPT                                  
*        CHAR  3  =  DISPLAY VALUE OF OPTION1                                   
*                                                                               
FORTABLE EQU   *                                                                
         DC    CL12'SSBOOBMMBBBB'  REGULAR REPORT                               
         DC    CL12'VSFPOFLMFCBF'  FORECAST REPORT                              
         DC    CL12'USPQOPKMPDBP'  PENDING REPORT                               
         DC    H'0'                DELIMITER                                    
         EJECT                                                                  
* LOOK AT REP RECORD TO SEE IF REP IS A SUBSIDIARY                              
* IF REP IS SUBSIDIARY, ROUTINE RETURNS COMMBAD WITH REP RECORD                 
*   IN IOWORK (SO CALLER HAS ACCESS TO RREPMAST)                                
GETREP   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE REP RECORD                          
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 DIRREAD                                                          
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         CLC   =X'0000',RREPMAST                                                
         BE    COMMGOOD                                                         
         CLC   =X'4040',RREPMAST                                                
         BE    COMMGOOD                                                         
         CLC   =X'FFFF',RREPMAST                                                
         BE    COMMGOOD                                                         
         B     COMMBAD                                                          
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
*- COMMON XIT AND COND CODE SETTING BRANCH LABELS                               
*                                                                               
COMMGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     COMMTEST                                                         
*                                                                               
COMMBAD  EQU   *                                                                
         LA    R0,1                                                             
*                                                                               
COMMTEST EQU   *                                                                
         LTR   R0,R0                                                            
COMMXIT  EQU   *                                                                
         XIT1                                                                   
         TITLE 'DISPLAY ADDRESS LISTS'                                          
*                                                                               
*- FIELD DISPLAY ADDRESS LIST                                                   
*                                                                               
FLDVAL   DS    0F                                                               
         DC    A(OFFRVAL)          01 - REGION                                  
         DC    A(OFFVAL)           02 - OFFICE                                  
         DC    A(GSGVAL)           03 - GROUP/SUBGROUP                          
         DC    A(DUMP)      @@     04 - SUBGROUP ONLY                           
         DC    A(STALSTV)          05 - STATION LIST                            
         DC    A(SDIVVAL)          06 - SALES DIVISION/TEAM                     
         DC    A(SMANVAL)          07 - SALESMAN                                
         DC    A(STAVAL)           08 - STATION                                 
         DC    A(ADVVAL)           09 - ADVERTISER                              
         DC    A(AGYVAL)           10 - AGENCY                                  
         DC    A(PROCLVAL)         11 - PRODUCT CLASS                           
         DC    A(PROCAVAL)         12 - PRODUCT CATEGORY                        
         DC    A(CTYPVAL)          13 - CONTRACT TYPE                           
         DC    A(STATVAL)          14 - STATION TYPE                            
         DC    A(PRDVAL)           15 - PRODUCT                                 
         DC    A(STAVAL)           16 - STATION ONLY                            
         DC    A(STRDVAL)          17 - START DATE                              
         DC    A(ENDDVAL)          18 - END DATE                                
         DC    A(ASADVAL)          19 - ASAT DATE                               
         DC    A(STRTENDV)         20 - START,END DATES                         
         DC    A(BASISVAL)         21 - BASIS                                   
         DC    A(STRTENDV)         22 - PERIOD START, END DATES                 
         DC    A(STRDVAL)          23 - PERIOD START                            
         DC    A(ENDDVAL)          24 - PERIOD END                              
         DC    A(SEQVAL)           25 - SEQUENCE                                
         DC    A(DEPTHV)    **     26 - DEPTH                                   
         DC    A(OPT1VAL)          27 - OPTION 1                                
         DC    A(EXTRADAT)         28 - EXTRA SET OF DATES                      
         DC    A(OPT2VAL)          29 - OPTION 2                                
         DC    A(DUMP)      **     30 - N/D                                     
         DC    A(AOPTVAL)          31 - ACCOUNTING OPTION                       
         DC    A(CUTOFF)           32 - CUTOFF                                  
         DC    A(CONVAL)           33 - CONTRACT                                
         DC    A(CONVAL)    **     34 - CON VALIDATION # 2 REDISPLAY            
         DC    A(TCNTVAL)          35 - TOTALS CONTROL                          
         DC    A(DUMP)      **     36 - N/D                                     
         DC    A(OPT3VAL)          37 - OPTION 3                                
         DC    A(DUMP)      **     38 - N/D                                     
         DC    A(AFFVAL)           39 - AFFILATE                                
         DC    A(DUMP)      **     40 - N/D                                     
         DC    A(OPT4VAL)          41 - OPTION 4                                
         DC    A(DUMP)      **     42 - N/D                                     
         DC    A(OPT5VAL)          43 - OPTION 5                                
         DC    A(DUMP)      **     44 - N/D                                     
         DC    A(OPT6VAL)          45 - OPTION 6                                
         DC    A(DUMP)      **     46 - N/D                                     
         DC    A(OPT6VAL)          47 - FORMAT (OPTION 6)                       
         DC    A(DUMP)      **     48 - N/D                                     
         DC    A(RKMAXVAL)         49 - RANK MAX                                
         DC    A(DUMP)      **     50 - N/D                                     
         DC    A(OPT3VAL)          51 - RANK PERIOD (OPTION 3)                  
         DC    A(BYVAL)            52 - BUDGET YEAR                             
         DC    A(RCPVAL)           53 - RECAP                                   
         DC    A(STRDVAL)          54 - ONE MONTH ONLY PERIOD                   
         DC    A(PERDVAL)          55 - ONE MONTH ENDING PERIOD                 
         DC    A(CREDRAT)          56 - CREDIT RATING                           
         DC    A(LIABPOS)          57 - LIABILITY POSITION                      
         DC    A(DUMP)      **     58 - N/D                                     
         DC    A(DUMP)      **     59 - N/D                                     
         DC    A(TVBVAL)           60 - TVB REGION                              
         DC    A(OWNERVAL)         61 - STATION OWNER                           
         DC    A(MKTVAL)           62 - MARKET CODE                             
         DC    A(RANKVAL)          63 - STATION RANK                            
         DC    A(PPSONVAL)         64 - POINTPERSON                             
         DC    A(NCONVAL)          65 - NETWORK CONTRACT NUMBER                 
         DC    A(BUDSTED)          66 - BUDGET START,END                        
         DC    A(ALCLSTV)          67 - BUDGET ALLOCATION PAIRS                 
         DC    A(ALCLST2)          68 - BUDGET ALLOCATION PAIRS                 
         DC    A(YADREC)           69 - YADR RECORD                             
         DC    A(TARGAGY)          70 - TARGET AGENCY                           
         DC    A(RSERV)            71 - RATING SERVICE                          
         DC    A(DEMOCD)           72 - DEMOGRAPHIC CODE                        
         DC    A(RBOOK)            73 - RATING BOOK                             
         DC    A(LABREC)           74 - RATING BOOK                             
*                                                                               
*   FOLLOWING TWO ITEMS DO NOT BELONG IN THIS TABLE:                            
*                                                                               
         DC    A(DUMP)      **     75 - 'ACTIVITY'                              
         DC    A(DUMP)      **     76 - 'START/END'                             
         DC    A(DEVSP)            77 - DEVELOPMENTAL SALESPERSON               
         DC    A(BUSTYP)           78 - NEW BUSINESS DEVELOPMENT TYPE           
         DC    A(SPLNDIS)          79 - NEW BUSINESS DEVELOPMENT TYPE           
         DC    A(DUMP)      **     80 -                                         
         DC    A(DUMP)      **     81 -                                         
         DC    A(DUMP)      **     82 -                                         
         DC    A(DUMP)      **     83 -                                         
         DC    A(DUMP)      **     84 -                                         
         DC    A(DUMP)      **     85 -                                         
         DC    A(OLDREP)           87 - TAKEOVER SOURCE REP                     
         DC    A(TKOSTAC)          87 - TAKEOVER STATION                        
         DC    A(TKOAGYC)          88 - TAKEOVER AGENCY FILTER                  
         DC    A(TKOOFFC)          89 - TAKEOVER OFFICE FILTER                  
         DC    A(CLOSMON)          90 - CLOSED THRU MONTH                       
FLDVALX  EQU   *                                                                
         SPACE 2                                                                
DUMP     DS    H'0'                INVALID INDEX VALUE                          
         TITLE 'LITERALS AND CONSTANTS'                                         
*                                                                               
SPACES   DC    CL80' '                                                          
*                                                                               
NOMORE   DC    C'END OF REQUESTS FOR THESE FILTERS.'                            
MORE     DC    C'MORE REQUESTS TO SEE.  PRESS ENTER FOR NEXT PAGE'              
REQDISP  DC    C'REQUEST HAS BEEN DISPLAYED'                                    
         LTORG                                                                  
         TITLE 'WORK AREA'                                                      
*                                                                               
*- REQTWA AND REQWRK                                                            
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
         SPACE                                                                  
*                                                                               
*- LOCAL WORK AREA                                                              
         ORG   USERWORK                                                         
LOCALWRK EQU   *        <----------INSERT VARIABLES BELOW THIS LABEL            
*                                                                               
AFORMAT  DS    A                   A(FORMATTING ROUTINE)                        
*                                                                               
ALISTDA  DS    A                   A(NEXT LISTDA AREA)                          
*                                                                               
DISKADDR DS    XL4                                                              
*                                                                               
NODETAIL DS    X                   1=SUPPRESS LIST DETAILS                      
*                                                                               
FIRSTSW  DS    X                   1=FIRST PASS                                 
*                                                                               
VALIDATE DS    X                   1=NO UPDATES ON CHANGE.                      
*                                  0=WRITE TO THE FILE.                         
*                                                                               
ANYCHA   DS    X                   1=CHANGES PROCESSED                          
*                                                                               
SUBRELO  DS    A                   SUBOVERLAY RELOCATION FACTOR                 
*                                                                               
AMAPNTRY DS    A                   A(CURRENT REQMAP ENTRY)                      
*                                                                               
COUNTER  DS    X                   LIST FIELD ITEM COUNTER                      
*                                                                               
DEMOVAL  DS    A                   DEMO VALIDATION ROUTINE                      
DEMOCON  DS    A                   DEMO CONVERSION ROUTINE                      
DBDEMOB  DS    288C                DEMO BLOCK AREA                              
DEMWORK  DS    CL30                DEMO OUTPUT AREA                             
*                                                                               
LOCALWKX EQU   *        <----------INSERT VARIABLES ABOVE THIS LABEL            
AMTLOCAL EQU   USERWRKX-LOCALWKX  AMOUNT LOCAL WRK AREA                         
         DS    (AMTLOCAL)X                                                      
         SPACE 2                                                                
*                                                                               
*- RECORD DSECTS FOLLOW                                                         
*                                                                               
*  RECORD      ORG                                                              
*  --------    ------                                                           
*  REGENREG    IOWORK                                                           
*  REGENOFF    IOWORK                                                           
*  REGENSTA    IOWORK                                                           
*  REGENADV    IOWORK                                                           
*  REGENPRD    IOWORK                                                           
*  REGENAGY    IOWORK                                                           
*  REGENTEM    IOWORK                                                           
*  REGENSAL    IOWORK                                                           
*  REGENCTG    IOWORK                                                           
*  REGENCLS    IOWORK                                                           
*  REGENCON    IOWORK                                                           
*  REGENGRP    IOWORK                                                           
*  REGENOWN    IOWORK                                                           
*  REGENSET    IOWORK                                                           
*  REGENREPA   IOWORK                                                           
*                                                                               
*        PRINT OFF                                                              
         ORG   IOWORK                                                           
       ++INCLUDE REGENREG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOFF                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSTA                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENADV                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENPRD                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENAGY                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENTEM                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSAL                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDSP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDCT                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCTG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCLS                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCON                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENGRP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOWN                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSET                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENREPA                                                      
*        PRINT ON                                                               
         SPACE 4                                                                
DCARDD   DSECT                                                                  
         DS    CL8                 FIELD HEADER                                 
DCCANC   DS    CL1                 CANCELLED CODE ON CHANGE                     
DCID     DS    CL2                                                              
DCCARD   DS    0CL76               REP OFFICE - END OF CARD                     
DCREPO   DS    CL2                                                              
DCNAME   DS    CL12                                                             
         ORG   DCNAME-3+L'DCNAME                                                
DCBLANK  DS    C                   BLANK ON ALL ID LIST                         
DCREP    DS    CL2                 REP CODE ON ALL AGY LIST                     
*                                                                               
DCOFFR   DS    CL2                                                              
DCOFF    DS    CL2                                                              
DCSTA    DS    CL5                                                              
DCSDIV   DS    CL1                                                              
DCSTEAM  DS    CL1                                                              
DCSMAN   DS    CL3                                                              
DCAGY    DS    CL4                                                              
DCAGYO   DS    CL2                                                              
DCADV    DS    CL4                                                              
DCPRO    DS    CL3                                                              
DCPROCL  DS    CL2                                                              
DCPROCA  DS    CL2                                                              
DCSTRD   DS    CL6                                                              
DCENDD   DS    CL6                                                              
DCASAD   DS    CL6                                                              
DCBASIS  DS    CL1                                                              
DCSTAT   DS    CL1                                                              
DCSEQ    DS    CL1                                                              
DCOPTN   DS    CL1                                                              
DCOPT2   DS    CL1                                                              
DCAOPT   DS    CL1                                                              
DCGRP    DS    CL1                                                              
DCSGRP   DS    CL1                                                              
DCCTYP   DS    CL1                                                              
DCOPT3   DS    CL1                                                              
DCOPT4   DS    CL1                                                              
DCOPT5   DS    CL1                                                              
DCOPT6   DS    CL1                                                              
         SPACE                                                                  
DHEXD    DSECT                     'HEX' DISPLAY                                
         DS    CL8                                                              
DHADDR   DS    CL8                 DISK ADDRESS                                 
         DS    CL2                                                              
DH10     DS    CL10                1ST 10 OF RQST                               
         DS    CL2                                                              
DHOPT4   DS    CL2                 OPTION 4 (RRG ID)                            
         SPACE                                                                  
*                                                                               
*- CTGENFILE (CONTROL FILE RECORD DESECTS) FOLLOW                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*  CONTROL BLOCK FOR DEMOGRAPHIC REQUIREMENTS                                   
*  DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                 
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109REREQ03S  05/01/02'                                      
         END                                                                    
