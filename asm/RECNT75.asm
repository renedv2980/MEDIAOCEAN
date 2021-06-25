*          DATA SET RECNT75    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T8026EB,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T80275 - RECNT75 - DDS/ENTERPRISE COMMON SPEC'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT75 --- DDS/ENTERPRISE COMMON SPEC                     *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN21/99 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
*    NOTE:  CHANGE PHASE NAME!!!                                    *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80275   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T80275*,R9,RR=R8                              
         ST    R8,RELO                                                          
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
         GOTO1 =A(GETCON),DMCB,(RC),RR=YES                                      
*                                  READ CONTRACT RECORD                         
***>>>   BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(GETSTA),DMCB,(RC),RCONKSTA,RR=YES                             
*                                  READ STATION RECORD INTO AIO3                
*                                                                               
*********************************************************************           
*   ENTERPRISE DOESN'T USE EOP CODES.  THIS IS LEFT IN AS INFO.                 
*********************************************************************           
*                                                                               
* DON'T USE 'HALF' IN NEXT SUBROUTINE (USING TO STORE STATION OPTION)           
*        GOTO1 READEOP,DMCB,(RC)   RETRIEVE EOP CODES                           
*                                                                               
*        CLI   HALF,C'Y'           IS OPTION TURNED ON?                         
*        BNE   MN090               NO, SKIP TEST                                
*                                                                               
*        LA    RF,EOPRECS          ARE ALL CODES PRESENT?                       
*        LA    RE,4                                                             
MN050    EQU   *                                                                
*        OC    0(12,RF),0(RF)      CODE PRESENT?                                
*        BZ    MN075               NO  - RETURN ERROR                           
*        LA    RF,12(RF)           YES - BUMP TO NEXT CODE                      
*        BCT   RE,MN050                                                         
*        B     MN090               ALL PRESENT                                  
MN075    EQU   *                                                                
*        LA    R3,583              SET 'EOP CODE MISSING'                       
*        LA    R2,CONCNUMH                                                      
*        B     ERROR                                                            
*********************************************************************           
*                                                                               
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
         LA    R2,*                SET ADDRESS OF HEADER SECTION                
         A     R2,=A(HDHDR-(*-4))                                               
         GOTO1 BLDHDREC,DMCB,(R2)  HEADER RECORD GENERATION                     
*                                                                               
         LA    R2,*                SET ADDRESS OF PROPRIETARY HEADER            
         A     R2,=A(PDHDR-(*-4))                                               
         GOTO1 BLDHDREC,DMCB,(R2)  HEADER RECORD GENERATION                     
*                                                                               
MN300    EQU   *                                                                
         LA    R2,*                SET ADDRESS OF HEADER SECTION                
         A     R2,=A(HCHDR-(*-4))                                               
         GOTO1 BLD02REC,DMCB,(R2)  ORDER HEADER COMMENTS                        
*                                                                               
         LA    R2,*                SET ADDRESS OF PROPRIETARY HEADER            
         A     R2,=A(PCHDR-(*-4))                                               
         GOTO1 BLD02REC,DMCB,(R2)  ORDER HEADER COMMENTS                        
*                                                                               
MN400    EQU   *                                                                
         LA    R2,*                SET ADDRESS OF BUYLINE SECTION               
         A     R2,=A(HBHDR-(*-4))                                               
         GOTO1 BLD03REC,DMCB,(R2)  BUYLINE RECORDS                              
*                                                                               
         LA    R2,*                SET ADDRESS OF PROPRIETARY BUYLINE           
         A     R2,=A(PBHDR-(*-4))                                               
         GOTO1 BLD03REC,DMCB,(R2)  BUYLINE RECORDS                              
*                                                                               
*   PREMATURE END                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
*   PREMATURE END                                                               
*                                                                               
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
*        BLDHDREC  ---  BUILDS THE HEADER RECORD FROM SPECS         *           
*********************************************************************           
BLDHDREC NTR1                                                                   
         L     R2,0(R1)            SET A(ELEMENT ARRAY TO BUILD)                
         CLI   0(R2),X'FF'         SKIP PROPRIETARY RECORD BUILD?               
         BE    BHDR0800            YES                                          
         MVC   OPLABEL,=C'OPRECORD'                                             
         MVI   OPRECORD,C' '       SPACE FILL THE RECORD                        
         LA    RF,OPRECORD+1       SET A(SECOND SPACE IN FIELD)                 
         LA    R1,OPRECLEN-1       SET L(FIELD) -1                              
         LA    RE,OPRECORD         SET A(FIRST SPACE IN FIELD)                  
         MOVE  ((RF),(R1)),(RE)    SPACE FILL THE RECORD IN SECTIONS            
         GOTO1 =A(BUILDREC),DMCB,(RC),(R2),(0,0),RR=Y                           
*                                  CYCLE TABLE ENTRIES TO BUILD RECORD          
         GOTO1 PRINTREC,DMCB,(RC)  OUTPUT RECORD BUILT                          
*                                                                               
*                                                                               
BHDR0800 EQU   *                                                                
         XIT1                                                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        BLD02REC  ---  BUILDS THE ORDER COMMENT RECORDS            *           
*              GENERATE ONE RECORD PER   COMMENT RECORD             *           
*********************************************************************           
BLD02REC NTR1                                                                   
         L     R2,0(R1)            SET A(ELEMENT ARRAY TO BUILD)                
         CLI   0(R2),X'FF'         SKIP PROPRIETARY RECORD BUILD?               
         BE    BLD20140            YES                                          
*                                                                               
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'02'        CONTRACT COMMENT ELEMENT                     
         BAS   RE,GETEL            NONE FOUND - FINISHED                        
         BNE   BLD20140            EXIT WITH NO WRITES                          
         B     BLD20060                                                         
BLD20040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   BLD20140            NOT FOUND - CHECK FOR WRITE                  
BLD20060 EQU   *                                                                
         MVI   OPRECORD,C' '       SPACE FILL THE RECORD                        
         LA    RF,OPRECORD+1       SET A(SECOND SPACE IN FIELD)                 
         LA    R1,OPRECLEN-1       SET L(FIELD) -1                              
         LA    RE,OPRECORD         SET A(FIRST SPACE IN FIELD)                  
         MOVE  ((RF),(R1)),(RE)    SPACE FILL THE RECORD IN SECTIONS            
*                                                                               
         GOTO1 =A(BUILDREC),DMCB,(RC),(R2),(0,(R6)),RR=Y                        
*                                  CYCLE TABLE ENTRIES TO BUILD RECORD          
         GOTO1 PRINTREC,DMCB,(RC)  OUTPUT RECORD BUILT                          
         B     BLD20040            GO BACK FOR ANOTHER                          
*                                                                               
BLD20140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE 'BUY' TRANSACTION RECORD.                          
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE 'BUY' RECORD PER EFFECTIVE DATE RANGE IS GENERATED.              
*      2.  COMMENTS ARE GEN'D AS 'COMM' RECORDS                                 
*                                                                               
BLD03REC NTR1                                                                   
         L     R2,0(R1)            SET A(ELEMENT ARRAY TO BUILD)                
         CLI   0(R2),X'FF'         SKIP PROPRIETARY RECORD BUILD?               
         BE    BLD30500            YES                                          
         LA    RF,RBUYREC                                                       
         ST    RF,ABUYREC          SAVE A(BUY RECORD)                           
*                                                                               
BLD30020 EQU   *                                                                
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    COLLAPS2,COLLAPS2   USED IN COLLAPSE BUY ROUTINE                 
         XC    TESTCTR,TESTCTR     **TEST                                       
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   OPRECORD,C' '       SPACE FILL THE RECORD                        
         LA    RF,OPRECORD+1       SET A(SECOND SPACE IN FIELD)                 
         LA    R1,OPRECLEN-1       SET L(FIELD) -1                              
         LA    RE,OPRECORD         SET A(FIRST SPACE IN FIELD)                  
         MOVE  ((RF),(R1)),(RE)    SPACE FILL THE RECORD IN SECTIONS            
*                                                                               
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30500            NO MORE BUYS - EXIT                          
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
BLD30040 EQU   *                                                                
         MVC   SVBYLN#,RBUYKLIN    SAVE BUY LINE #                              
         GOTO1 BLD3CTCM,DMCB,(RC)  SET MG + COMMENT FLAGS                       
*                                                                               
         GOTO1 JDSDUR,DMCB,(RC)    SET UP JDS LENGTH FORMAT                     
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
*        IF THERE IS NEED TO CREATE MULTIPLE BUY LINES BECAUSE OF               
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
         BZ    BLD30360            NO  - DON'T PROCESS BUY                      
         GOTO1 SPTINFO,DMCB,(RC),(R6)                                           
*                                                                               
         MVC   EC203ACT(2),FOXZEROS                                             
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   EC203ACT,C'1'       YES                                          
         MVI   EC203INA,C'1'       SET INACTIVE WEEKS TO 01                     
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
         MVI   EC203BY2,C' '       YES - SPACE OUT SUBLINE                      
         B     BLD30300                                                         
BLD30280 EQU   *                                                                
         STC   RF,EC203BY2         INSERT 'SUBLINE' FROM SUBLINES               
BLD30300 EQU   *                                                                
         MVC   SVSL#,EC203BY2      SAVE SUBLINE                                 
         B     BLD30320                                                         
*                                                                               
*   GENERATING MULTIPLE LINES IF COUNT EXCEEDS 9 SPOTS PER DAY IS               
*        DEACTIVATED.  IF SOME SEMBLANCE OF THIS IS DESIRED, THE                
*        CODE NEED ONLY BE UPGRADED TO ACCOMPLISH THE FUNCTION.                 
*                                                                               
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
         GOTO1 =A(BUILDREC),DMCB,(RC),(R2),(0,0),RR=Y                           
*                                  CYCLE TABLE ENTRIES TO BUILD RECORD          
         GOTO1 PRINTREC,DMCB,(RC)                                               
         CLI   COMFLAGS,0          ANY COMMENTS EXPECTED?                       
         BE    BLD30420            NO                                           
*                                                                               
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
         MVC   EC203CAL,SPACES                                                  
         LA    R4,EC203CAL         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTMAX2,(1,0(R4))                                               
BLD30400 EQU   *                                                                
         EDIT  SPOTMAX2,(3,EC203SPT),FILL=0,ZERO=NOBLANK                        
         LA    R2,SUBLINES                                                      
         ZIC   RF,MULTEFDT         CURRENT NUMBER OF EFF DATES                  
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         AR    R2,RF               DISPLACE TO SUBLINE #                        
         MVC   EC203BY2,0(R2)      INSERT SUBLINE #                             
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
***>>>   GOTO1 BLD04REC,DMCB,(RC)  YES - ADD COMMENTS                           
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
         XC    CMTCTR,CMTCTR       CLEAR COMMENT COUNTER                        
**>  COMMENT PROCESS                                                            
*                                                                               
******                                                                          
******   NEED PROPRIETARY LOOP PROCESSING                                       
******   LA    R2,*                SET ADDRESS OF PROPRIETARY HEADER            
******   A     R2,=A(PCHDR-(*-4))                                               
******                                                                          
*                                                                               
**>  COMMENT PROCESS                                                            
         MVI   OPRECORD,C' '       SPACE FILL THE RECORD                        
         LA    RF,OPRECORD+1       SET A(SECOND SPACE IN FIELD)                 
         LA    R1,OPRECLEN-1       SET L(FIELD) -1                              
         LA    RE,OPRECORD         SET A(FIRST SPACE IN FIELD)                  
         MOVE  ((RF),(R1)),(RE)    SPACE FILL THE RECORD IN SECTIONS            
*                                                                               
         SR    R2,R2               SET SWITCH                                   
         TM    COMFLAGS,X'40'      COMMENTS EXIST?                              
         BNO   BLD40340            NO - DON'T PUT OUT ANY RECORDS               
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'84'        LOOK FOR ORDER COMMENT                       
         MVC   EC203CMT,SPACES     SPACE OUT COMMENT AREA                       
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
         B     BLD40220            GO BUILD AN OUTPUT RECORD                    
BLD40210 EQU   *                                                                
         EX    RF,BLD4023C         MOVE 1ST COMMENT (BUY)                       
         B     BLD40220            GO BUILD AN OUTPUT RECORD                    
BLD40220 EQU   *                                                                
*                                                                               
         LA    R2,*                SET ADDRESS OF BUY COMMENT                   
         A     R2,=A(BCHDR-(*-4))                                               
         GOTO1 =A(BUILDREC),DMCB,(RC),(R2),(0,0),RR=Y                           
*                                  CYCLE TABLE ENTRIES TO BUILD RECORD          
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         B     BLD40180            GO BACK FOR NEXT COMMENT RECORD              
*                                     EITHER ORDER OR CONTRACT                  
*                                                                               
BLD4023A MVC   EC203CMT(0),3(R6)                                                
BLD4023C MVC   EC203CMT(0),2(R6)                                                
*                                                                               
BLD40260 EQU   *                                                                
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40340            YES - COMMENTS FINISHED                      
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'04'        NO  - GO BACK AND DO THEM                    
         B     BLD40160                                                         
*                                                                               
BLD40340 EQU   *                                                                
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
         A     R1,RELO                                                          
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
*                                                                               
PRINTREC NTR1                                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
         LA    R2,OPCOUNT          SET LOOP CONTROL                             
         LA    R5,OPLENGTH         SET LINE LENGTH                              
         LA    R6,OPLENGTH-1       SET LINE LENGTH FOR EX STATEMENT             
PRC20020 EQU   *                                                                
         LA    R4,OPRECORD         A(OUTPUT RECORD)                             
PRC20040 EQU   *                                                                
         EX    R6,PRC20800         MOVE N CHARS TO PRINT                        
         LA    RF,P                SET A(PRINTLINE)                             
         AR    RF,R5               ADD LENGTH OF OUTPUT LINE                    
         LA    RF,3(RF)            BUMP FOR SPACING                             
         MVC   0(3,RF),=C'DDS'     SET LINE SENTINEL                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,OPLENGTH(R4)     BUMP TO NEXT PORTION OF RECORD               
         CLC   0(OPLENGTH,R4),SPACES                                            
*                                  ANY VALUE LEFT ON LINE?                      
         BNE   PRC20040            YES - OUTPUT THE SECTION                     
         XIT1                      NO  - FINISHED                               
PRC20800 MVC   P(0),0(R4)          MOVE N CHARS TO PRINT                        
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
*   TRANSLATE LENGTH TO JDS OUTPUT FORMAT.                                      
*                                                                               
JDSDUR   NTR1                                                                   
         MVC   EC203LNS,FOXZEROS   INITIALIZE LENGTH OF SPOT                    
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    JDUR0020            YES                                          
*                                                                               
*   IF 90 SECONDS OR LESS, USE SECONDS AS IS                                    
*                                                                               
         ZICM  RF,RBUYDUR,2        NO  - SECONDS                                
         CH    RF,=H'90'           > 90 SECONDS?                                
         BH    JDUR0020            YES                                          
         EDIT  RBUYDUR,(5,EC203LNS),FILL=0,ZERO=NOBLANK                         
*                                  USE AS IS                                    
         B     JDUR0160                                                         
JDUR0020 EQU   *                                                                
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         TM    RBUYDUR,X'80'       LENGTH IN MINUTES?                           
         BO    JDUR0040            YES - MINUTES:                               
*                                  NO  - SECONDS:  90 SECS OR LESS              
*                                     REMAIN AS 'SECONDS'                       
         CH    R1,=H'90'           90 SECS OR LESS?                             
         BH    JDUR0080            NO  - HIGHER: CONVERT TO MINS:SECS           
         B     JDUR0060                                                         
JDUR0040 EQU   *                                                                
*                                  MINUTES: 89 MINUTES OR LESS                  
*                                     REMAIN AS 'MINUTES'                       
         CH    R1,=H'59'           59 MINS OR LESS?                             
         BH    JDUR0080            NO  - HIGHER: CONVERT TO HRS:MIN             
JDUR0060 EQU   *                                                                
*                                  YES - SHOW AS SECS OR MINS                   
         ST    R1,FULL             SAVE 'REMAINDER': TOTAL LENGTH               
         SR    R1,R1               CLEAR ORIGINAL VALUE                         
         B     JDUR0100            NOW SKIP THE DIVIDE                          
JDUR0080 EQU   *                                                                
*                                                                               
*   SPOT LENGTH IS EITHER MINUTES OR SECONDS AT THIS POINT.                     
*        DIVIDING BY 60 PRODUCES EITHER HOURS (IF MINUTES)                      
*        OR MINUTES (IF SECONDS).  REMAINDER IS IN REG 0.                       
*                                                                               
         D     R0,=F'60'           YES - CONVERT TO HRS:MINS                    
         ST    R0,FULL             SAVE REMAINDER IN FULL                       
JDUR0100 EQU   *                                                                
*                                                                               
*   FULL CONTAINS THE REMAINDER (SECS/MINS) IF DIVIDE WAS DONE, OR              
*        TOTAL SECS/MINS IF TIME WAS 90 OR LESS.  R1 CONTAINS EITHER            
*        MINS OR HOURS.                                                         
*                                                                               
         TM    RBUYDUR,X'80'       LENGTH IN MINUTES?                           
         BO    JDUR0140            YES                                          
         LA    R4,EC203LNS+3       INSERT REMAINDER (SECS) FIRST                
         EDIT  (R0),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT MINUTES                               
JDUR0120 EQU   *                                                                
         LA    R4,EC203LNS+1       INSERT MINUTES NEXT                          
         EDIT  (R1),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT SECONDS                               
         B     JDUR0160                                                         
JDUR0140 EQU   *                                                                
         EDIT  (R1),(1,EC203LNS),ZERO=NOBLANK                                   
*                                  INSERT HOURS                                 
         LA    R4,EC203LNS+1                                                    
         L     R0,FULL             RESTORE VALUE OF REMAINDER                   
         EDIT  (R0),(2,0(R4)),FILL=0,ZERO=NOBLANK                               
*                                  INSERT MINUTES                               
JDUR0160 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
**>>DYTM BIAS FORMAT                                                            
*                                                                               
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
         MVC   EC203STD(16),FOXZEROS                                            
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
         MVC   EC203STD(3),STARTDTE                                             
*                                  SAVE FLIGHT START DATE                       
***      GOTO1 DATCON,DMCB,(3,STARTDTE),(X'20',HOLDDATE)                        
***      MVC   EC203STD+4(4),HOLDDATE+2     INSERT MM/DD                        
***      MVC   EC203STD+2(2),HOLDDATE       INSERT YY                           
*                                                                               
***      MVC   EC203STD(2),=C'19'  SET TO 2OTH CENTURY                          
***      CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
***      BH    DINF0180            YES                                          
***      MVC   EC203STD(2),=C'20'  NO  - 21ST CENTURY                           
DINF0180 EQU   *                                                                
*                                                                               
*    INSERT END   DATE                                                          
         MVC   EC203END(3),ENDDTE                                               
*                                  SAVE FLIGHT END DATE                         
*                                                                               
***      GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',HOLDDATE)                          
***      MVC   EC203END+4(4),HOLDDATE+2     INSERT MM/DD                        
***      MVC   EC203END+2(2),HOLDDATE       INSERT YY                           
*                                                                               
***      MVC   EC203END(2),=C'19'  SET TO 2OTH CENTURY                          
***      CLC   HOLDDATE(2),=C'70'  20TH CENTURY?                                
***      BH    DINF0200            YES                                          
***      MVC   EC203END(2),=C'20'  NO  - 21ST CENTURY                           
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
         MVC   EC203CAL,SPACES                                                  
         LA    R4,EC203CAL         A(CALENDAR FIELD)                            
         BCTR  R0,0                MAKE START DAY ZERO RELATIVE                 
         AR    R4,R0               POINT TO DAY IN CALENDAR                     
         EDIT  SPOTSWK,(1,0(R4))                                                
*                                  INSERT # SPTS/WK INTO CALENDAR               
SPTI0400 EQU   *                                                                
         EDIT  SPOTSWK,(3,EC203SPT),FILL=0,ZERO=NOBLANK                         
*                                  INSERT NUMBER SPOTS PER WEEK                 
         EDIT  RBUYCOS,(9,EC203RTE),FILL=0,ZERO=NOBLANK                         
*                                  INSERT PRICE PER SPOT                        
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
RELO     DS    F                                                                
YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                         
BYEAR    DS    X                   BINARY YEAR                                  
STATFLAG DS    X                                                                
HASCANLN EQU   X'80'               ORDER HAS CANCELLED BUYLINES                 
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
RMSCMODE DC    X'00'               REGENSC MODES                                
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
OPLABEL  DS    CL8                                                              
OPRECORD DS    1100C                                                            
OPRECLEN EQU   *-OPRECORD                                                       
*                                                                               
OPLENGTH EQU   110                 LINE LENGTH                                  
OPCOUNT  EQU   10                  MAX NUMBER OF LINES                          
*                                                                               
ELTBUILD DS    64C                                                              
         ORG   ELTBUILD                                                         
EBLDCODE DS    CL4                                                              
EBLDMAX  DS    CL2                                                              
EBLDTHIS DS    CL2                                                              
EBLDDATA DS    CL56                                                             
*                                                                               
TRANSWRK DS    CL64                FORMAT TRANSLATE AREA                        
TRANSWK2 DS    CL64                ALTERNATE FORMAT TRANSLATE AREA              
BINARWRK DS    CL4                 BINARY TRANSLATE AREA                        
*                                                                               
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
OUTFORM  DS    XL1                 OUTPUT FORMAT INDICATOR                      
*                    0  =  CONTROL ID/FIELD MAX-MIN/COMPRESSED      *           
*                    1  =  CONTROL ID/FIELD MAX-MIN/UNCOMPRESSED    *           
*                    2  =  NO CONTROL/FIELD MAX SIZE FIXED-LENGTH   *           
ELTADDR  DS    A                                                                
EC203CAL DS    CL7                                                              
EC203SPT DS    CL3                                                              
EC203RTE DS    CL9                                                              
EC203STD DS    CL8                                                              
EC203END DS    CL8                                                              
EC203ACT DS    CL1                                                              
EC203INA DS    CL1                                                              
EC203BY2 DS    CL1                                                              
EC203DAY DS    CL12                                                             
EC203ORT DS    CL15                                                             
EC203STM DS    CL5                                                              
EC203LNS DS    CL5                                                              
EC203BBT DS    CL4                                                              
EC203BET DS    CL4                                                              
EC203CMT DS    CL60                                                             
*                                                                               
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
ANXTSUPT DS    A                   A(NEXT SUPPORT RECORD) IN IO4                
AAGYREC  DS    A                   A(AGENCY RECORD)                             
AADVREC  DS    A                   A(ADVERT RECORD)                             
ASALREC  DS    A                   A(SALESPERSON RECORD)                        
AOFFREC  DS    A                   A(OFFICE RECORD)                             
APRDREC  DS    A                   A(PRODUCT RECORD)                            
ASTAREC  DS    A                   A(STATION RECORD)                            
AREPREC  DS    A                   A(REP RECORD)                                
ABUYREC  DS    A                   A(CONTRACT RECORD)                           
ACONREC  DS    A                   A(BUY RECORD)                                
*                                                                               
SAVEREG5 DS    F                                                                
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
*                                                                               
*********************************************************************           
*        BUILDREC  ---  BUILDS RECORD FROM TABLE ENTRIES            *           
*                                                                   *           
*        P1       :  A(TABLE ENTRY IN PROCESS)                      *           
*        P2/BYTE 1:  OUTPUT FORMAT INDICATOR                        *           
*                    0  =  CONTROL ID/FIELD MAX-MIN/COMPRESSED      *           
*                    1  =  CONTROL ID/FIELD MAX-MIN/UNCOMPRESSED    *           
*                    2  =  NO CONTROL/FIELD MAX SIZE FIXED-LENGTH   *           
*              2-4:  IF NOT ZERO, USE AS A(ELEMENT WITH DATA)       *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
BUILDREC NMOD1 0,*BREC*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(TABLE ENTRY IN PROCESS)                
         MVC   OUTFORM,8(R1)       SAVE OUTPUT FORMAT INDICATOR                 
         XC    ELTADDR,ELTADDR     CLEAR A(ELEMENT TO USE)                      
         MVC   ELTADDR+1(3),9(R1)  LOAD OPTIONAL ADDRESS                        
         LA    R4,OPRECORD         SET A(RECORD BUILD AREA)                     
BREC0020 EQU   *                                                                
         CLI   0(R2),X'FF'         END OF TABLE REACHED?                        
         BE    BREC9000            YES - FINISHED                               
         SR    R5,R5               CLEAR A(RECORD IN PROCESS)                   
         CLI   DERECTYP(R2),CONTROL    CONTROL ENTRY?                           
         BE    BREC0080            YES - LEAVE A(RECORD) EMPTY                  
         L     R5,AREPREC          LOAD A(REP RECORD)                           
         CLI   DERECTYP(R2),REP        REP ENTRY?                               
         BE    BREC0080            YES                                          
         L     R5,ASTAREC          LOAD A(STATION RECORD)                       
         CLI   DERECTYP(R2),STATION    STATION ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,AOFFREC          LOAD A(OFFICE  RECORD)                       
         CLI   DERECTYP(R2),OFFICE     OFFICE  ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,ASALREC          LOAD A(SALESP  RECORD)                       
         CLI   DERECTYP(R2),SALESP     SALESP  ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,AADVREC          LOAD A(ADVERT  RECORD)                       
         CLI   DERECTYP(R2),ADVERT     ADVERT  ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,APRDREC          LOAD A(PRODUCT RECORD)                       
         LTR   R5,R5               PRODUCT RECORD EXISTS?                       
         BNZ   BREC0040            YES                                          
         LA    R5,1                NO  - SET REG TO -1                          
         LNR   R5,R5                                                            
BREC0040 EQU   *                                                                
         CLI   DERECTYP(R2),PROD       PRODUCT ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,AAGYREC          LOAD A(AGENCY RECORD)                        
         CLI   DERECTYP(R2),AGENCY     AGENCY  ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,ABUYREC          LOAD A(BUY    RECORD)                        
         CLI   DERECTYP(R2),BUY        BUY     ENTRY?                           
         BE    BREC0080            YES                                          
         L     R5,ACONREC          LOAD A(CONTRACT RECORD)                      
         CLI   DERECTYP(R2),CONTRACT   CONTRACT ENTRY?                          
         BE    BREC0080            YES                                          
         LA    R5,LOCALWRK         LOAD A(LOCAL WORK AREA)                      
         CLI   DERECTYP(R2),ABSFIELD   USE ABSOLUTE FIELD FOR DATA?             
         BE    BREC0080            YES                                          
         CLI   DERECTYP(R2),DEFFIELD   SPECIAL ROUTINE NEEDED?                  
         BE    BREC1000            YES - SKIP THIS FIELD TEMPORARILY            
*                                                                               
*    NOTE 'X'FE' FIELDS ARE BEING SKIPPED UNTIL CODING IS INSERTED              
*        TO DETERMINE WHICH SPECIAL ROUTINE TO LINK TO                          
*                                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE                     
BREC0080 EQU   *                                                                
         LTR   R5,R5               CONTROL FIELD IN PROGRESS?                   
         BNZ   BREC0120            NO                                           
         ZICM  RF,DEOPLEN(R2),2    YES - CONTROL ID ONLY                        
*                                                                               
*   RECORD IDENTIFICATION FIELD CONTAINS NO LENGTH.  IT IS ALWAYS               
*        FOUR CHARACTERS, BEGINNING IN FIRST POSITION OF RECORD.                
*        THE FIRST 'VARIABLE' FIELD BEGINS IN POSITION 4.                       
*                                                                               
****>>>  EDIT  (RF),(2,0(R4)),FLOAT=0                                           
*                                  SET FIELD LENGTH MAX                         
****>>>  MVC   2(2,R4),0(R4)       SET FIELD LENGTH USED SAME AS MAX            
         LR    RE,RF                                                            
         BCTR  RE,0                BACK OFF 1 FOR EX                            
         EX    RE,BREC0100         MOVE BY LENGTH                               
         AR    R4,RF               ADD LENGTH TO FIGURE A(NEXT FIELD)           
****>>>  LA    R4,4(R4)            ADD LENGTH OF CONTROL FIELDS                 
         B     BREC1000            BUMP TO NEXT FIELD                           
BREC0100 MVC   0(0,R4),DEOPFID#(R2)                                             
*                                  MOVE FIELD BY LENGTH                         
BREC0120 EQU   *                                                                
         CLI   DEELT(R2),0         DATA IN ELEMENT?                             
         BZ    BREC0200            NO  - DATA DIRECTLY ACCESSIBLE               
         OC    ELTADDR,ELTADDR     A(ELEMENT) PASSED IN?                        
         BZ    BREC0140            NO                                           
         L     R5,ELTADDR          YES - USE IT                                 
         B     BREC0200                                                         
BREC0140 EQU   *                                                                
         LA    R5,34(R5)           SET TO DESCRIPTOR ELEMENT                    
BREC0160 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD REACHED?                       
         BNE   BREC0180            NO                                           
*                                  YES - DATA NOT PRESENT                       
         TM    DEOPTBIT(R2),NOELTOK                                             
*                                  MUST IT BE PRESENT?                          
         BNZ   BREC1000            NO  - BRANCH TO TEST SPACE                   
*                                                                               
         DC    H'0'                YES - FORCE DUMP: MISSING DATA               
BREC0180 EQU   *                                                                
         CLC   0(1,R5),DEELT(R2)   ELEMENT REQUIRED FOUND?                      
         BE    BREC0200            YES                                          
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     BREC0160            GO BACK FOR NEXT ELEMENT                     
BREC0200 EQU   *                                                                
         ZICM  RF,DERECDIS(R2),4   TAKE DATA DISPLACEMENT FROM TABLE            
         AR    R5,RF               ADD DISP + A(START)                          
         MVC   ELTBUILD,SPACES     CLEAR ELEMENT BUILD AREA                     
*                                                                               
         CLI   DERECTYP(R2),BUY    BUY     ENTRY?                               
         BNE   BREC0201            NO                                           
         GOTO1 CALNDRFL,DMCB,(RC)  YES - SET UP CALENDAR                        
BREC0201 EQU   *                                                                
*                                                                               
*   CHECK FOR SPECIAL OPTIONS WHICH MAY CALL SPECIAL ROUTINES TO SET            
*        UP INDICATED FIELDS                                                    
*   NOTE:  DATE AND TIME OPTIONS SHOULD BE SET UP MUTUALLY EXCLUSIVE.           
*                                                                               
         TM    DEOPTBIT(R2),JDSTIMO                                             
*                                  JDS FORMAT TIME?                             
         BNO   BREC0202            NO                                           
         GOTO1 JDSTIME             YES - CALCULATE JDS FORMAT TIME              
BREC0202 EQU   *                                                                
         TM    DEOPTBIT(R2),JDSDAYO                                             
*                                  JDS DATE FORMAT?                             
         BNO   BREC0204            NO                                           
         GOTO1 JDSDAYS             YES - CALCULATE JDS FORMAT DATE              
BREC0204 EQU   *                                                                
         TM    DEOPTBIT(R2),BIASTIMO                                            
*                                  BIAS TIME FORMAT?                            
         BNO   BREC0206            NO                                           
         GOTO1 BIASTIME            YES - CALCULATE BIAS FORMAT TIME             
BREC0206 EQU   *                                                                
         TM    DEOPTBIT(R2),BIASDAYO                                            
*                                  BIAS DATE FORMAT?                            
         BNO   BREC0208            NO                                           
         GOTO1 BIASDAYS            YES - CALCULATE BIAS FORMAT DATE             
BREC0208 EQU   *                                                                
*                                                                               
*   PROCESS INPUT FORMATS:  ALL DATA BEING TRANSFERRED VIA EC WILL              
*        BE EBCDIC.  ALL INPUT FORMATS WILL BE CONVERTED TO AN                  
*        INTERMEDIATE EBCDIC FORMAT.                                            
*                                                                               
         CLI   DEIPFORM(R2),C'H'   INPUT = HEX?                                 
         BNE   BREC0220            NO                                           
         BAS   RE,TRANSHEX         YES                                          
         B     BREC0320                                                         
BREC0220 EQU   *                                                                
         CLI   DEIPFORM(R2),C'B'   INPUT = BINARY?                              
         BNE   BREC0240            NO                                           
         BAS   RE,TRANSBIN         YES                                          
         B     BREC0320                                                         
BREC0240 EQU   *                                                                
         CLI   DEIPFORM(R2),C'E'   INPUT = EBCDIC?                              
         BNE   BREC0260            NO                                           
         BAS   RE,TRANSEBC         YES                                          
         B     BREC0320                                                         
BREC0260 EQU   *                                                                
         CLI   DEIPFORM(R2),C'D'   INPUT = DATE?                                
         BNE   BREC0280            NO                                           
         BAS   RE,TRANSDAT         YES                                          
         B     BREC0320                                                         
BREC0280 EQU   *                                                                
         CLI   DEIPFORM(R2),C'F'   INPUT = FLAG?                                
         BNE   BREC0300            NO                                           
         BAS   RE,TRANSFLG         YES                                          
         B     BREC0320                                                         
BREC0300 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED INPUT TYPE                      
BREC0320 EQU   *                                                                
*                                                                               
*   TEST FOR ANY SPECIAL ROUTINES WHICH MAY MODIFY DATA IN TRANSWRK             
*                                                                               
         TM    DESPCRTN(R2),NOSPECHR                                            
*                                  NO SPECIAL CHARACTERS IN FIELD?              
         BNO   BREC0340            NO                                           
         BAS   RE,DROPSPEC         YES - DROP SPECIAL CHARACTERS                
BREC0340 EQU   *                                                                
         TM    DESPCRTN(R2),SUBDARE                                             
*                                  GET DARE AGENCY CODE?                        
         BNO   BREC0360            NO                                           
         BAS   RE,DAREAGCY         YES -                                        
BREC0360 EQU   *                                                                
         TM    DESPCRTN(R2),PRODNMCD                                            
*                                  PRODUCT NAME TEST?                           
         BNO   BREC0380            NO                                           
         BAS   RE,CHKPROD          YES -                                        
BREC0380 EQU   *                                                                
*                                                                               
*   PROCESS OUTPUT FORMATS:  DATA WILL HAVE BEEN TRANSLATED TO                  
*        EBCDIC.  EBCDIC FORMAT WILL BE CONVERTED TO APPROPRIATE                
*        FINAL OUTPUT FORMAT.                                                   
*        DATA IS IN FIELD 'TRANSWRK'.  O/P LENGTH OF FIELD APPLIES.             
*                                                                               
                                                                                
         OC    TRANSWRK,SPACES     SET BINARY ZEROS TO SPACES                   
         MVC   ELTBUILD+0(4),DEOPFID#(R2)                                       
*                                  INSERT FIELD ID # FROM TABLE                 
         ZICM  RF,DEOPLEN(R2),2    GET OUTPUT LENGTH OF DATA                    
         LR    R0,RF               SET MIN = MAX COUNT                          
         EDIT  (RF),(2,ELTBUILD+4),FILL=0                                       
*                                  SET FIELD LENGTH MAX                         
         CLI   OUTFORM,0           0 = OUTPUT W/CONTROL + COMPRESSED            
         BNE   BREC0460            NO  - EITHER                                 
*                                  OUTPUT W/CONTROL + UNCOMPRESSED              
*                                  OR                                           
*                                  OUTPUT W/O CONTROL + UNCOMPRESSED            
BREC0400 EQU   *                                                                
*                                                                               
*   CALCULATE NUMBER OF SIGNIFICANT CHARACTERS IN FIELD                         
*                                                                               
         LA    RF,TRANSWRK+63      SET A(LAST CHARACTER OF FIELD)               
         LA    R0,64               SET COUNT                                    
BREC0420 EQU   *                                                                
         CLI   0(RF),C' '          FIELD CONTAINS SPACE?                        
         BE    BREC0440            YES - NO SIGNIFICANT DATA FOUND              
         CLI   0(RF),0             FIELD CONTAINS BINARY ZERO?                  
         BNE   BREC0460            NO  - SIGNIFICANT DATA FOUND                 
BREC0440 EQU   *                                                                
         BCTR  RF,0                YES - BACK UP 1 POSITION                     
         BCTR  R0,0                DECREMENT SIGNIFICANT DATA COUNTER           
         LTR   R0,R0               ALL SPACES CHECKED?                          
         BNZ   BREC0420            NO  - GO BACK TO CHECK PREVIOUS CHAR         
*                                                                               
BREC0460 EQU   *                                                                
         EDIT  (R0),(2,ELTBUILD+6),FILL=0                                       
*                                  SET FIELD LENGTH OF DATA                     
         LR    RF,R0                                                            
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,BREC0480         MOVE FIELD BY LENGTH                         
         B     BREC0500                                                         
BREC0480 EQU   *                                                                
         MVC   ELTBUILD+8(0),TRANSWRK                                           
BREC0500 EQU   *                                                                
         LTR   RB,RB                                                            
         CLI   OUTFORM,2           CONTROL IN OUTPUT?                           
         BE    BREC0520            NO                                           
         LA    RF,8(RF)            YES - ADD L(CONTROL INFO) TO                 
*                                     CALC L(ELTBUILD)                          
         EX    RF,BREC0540         MOVE ELTBUILD                                
         LA    RF,1(RF)                                                         
         AR    R4,RF               BUMP PAST LATEST ELEMENT                     
         B     BREC0580                                                         
BREC0520 EQU   *                                                                
         EX    RF,BREC0560         MOVE ELTBUILD AFTER CONTROL                  
         LA    RF,1(RF)                                                         
         AR    R4,RF               BUMP PAST LATEST ELEMENT                     
         B     BREC0580                                                         
BREC0540 EQU   *                                                                
         MVC   0(0,R4),ELTBUILD    MOVE ELTBUILD TO RECORD                      
BREC0560 EQU   *                                                                
         MVC   0(0,R4),ELTBUILD+8  MOVE ELTBUILD W/O CONTROL TO RECORD          
*                                                                               
BREC0580 EQU   *                                                                
BREC1000 EQU   *                                                                
         LA    R2,LDDSENT(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     BREC0020            GO BACK FOR NEXT FIELD                       
BREC9000 EQU   *                                                                
         XIT1                                                                   
BRECTEST EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   DROPSPEC:  DROP ANY CHARACTERS NOT A-Z, 0-9, AND COMPRESS OUT               
*        OF STRING CURRENTLY FOUND IN 'TRANSWRK'                                
*                                                                               
DROPSPEC NTR1                                                                   
         MVC   TRANSWK2,TRANSWRK                                                
         XC    TRANSWRK,TRANSWRK   CLEAR FINAL FIELD                            
         LA    R1,TRANSWK2         SET A(SENDING FIELD)                         
         LA    R2,TRANSWRK         SET A(RECEIVING FIELD)                       
         LA    R0,64               SET LOOP                                     
DSPE0020 EQU   *                                                                
         CLI   0(R1),C'A'          FIELD LESS THAN ALPHA 'A'?                   
         BL    DSPE0060            YES - SKIP IT                                
         CLI   0(R1),C'9'          FIELD MORE THAN ALPHA '9'?                   
         BH    DSPE0060            YES - SKIP IT                                
         MVC   0(1,R2),0(R1)       NO  - KEEP CHAR IN FIELD                     
         LA    R2,1(R2)            INCREASE RECEIVING ADDR                      
DSPE0060 EQU   *                                                                
         LA    R1,1(R1)            INCREASE SENDING ADDR                        
         BCT   R0,DSPE0020         GO BACK FOR NEXT                             
         XIT1  NO MORE - FINISHED                                               
         EJECT                                                                  
*                                                                               
*   DAREAGCY:  IF DARE ORDER, DETERMINE DARE AGENCY CODE FROM REP               
*        AGENCY CODE AND CONTENTS OF X'1D' ELEMENT IN ORDER                     
*                                                                               
DAREAGCY NTR1                                                                   
*                                                                               
*   DOES ORDER HAVE AN X'1D' ELEMENT?  IF NO, GET OUT.  THERE IS                
*        NO ELEMENT TO BE ADDED.                                                
*                                                                               
         LA    R6,RCONREC          ACCESS DARE ELEMENT                          
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DAGY1000            NOT FOUND:  EXIT                             
*                                                                               
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
*                                                                               
         L     R2,AAGYREC          SET A(AGENCY2 RECORD)                        
         USING RAGY2REC,R2            RECORD ALREADY READ                       
*                                                                               
         MVC   RDARKAGY(5),RAGY2DAR                                             
*                                  EQUIVALENCY CODE                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         MVC   WORK2(32),KEY       SAVE START KEY FOR X'51' LOOKS               
         DROP  R6                                                               
                                                                                
DAGY0010 DS    0H                                                               
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R0,4                COMBINATIONS WE NEED TO CHECK                
                                                                                
DAGY0020 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    DAGY0060            AGENCY CODE FOUND                            
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE         RESET KEY                                    
         LA    R4,5(R4)                                                         
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         BCT   R0,DAGY0020                                                      
*                                  SPECIAL FOR SELTEL                           
         CLC   =C'SZ',REPALPHA                                                  
         BNE   DAGY0040                                                         
         CLC   =C'1342  ',RAGK2AGY AND AGENCY 1342 ONLY                         
         BNE   DAGY0040                                                         
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    DAGY0060                                                         
*                                                                               
* INSTEAD OF ABEND, JUST EXIT PROGRAM. THE CONTRACT MIGHT HAVE BEEN             
* IN DARE BUT NOT THIS MODIFICATION VERSION                                     
*                                                                               
DAGY0040 DS    0H                                                               
         CLI   WORK2,X'41'         DARE RECS LOOKED AT?                         
         BNE   DAGY0050            NO  - X'51'S ALSO DONE                       
         MVI   WORK2,X'51'         SET TO PROCESS DARE HISTORY RECS             
         MVC   KEY(32),WORK2       RELOAD KEY AREA                              
         B     DAGY0010            GO BACK AND DO IT                            
DAGY0050 DS    0H                                                               
         DC    H'0'                DARE, BUT NO AGENCY                          
DAGY0060 DS    0H                                                               
         XC    TRANSWRK,TRANSWRK                                                
         MVC   TRANSWRK(5),RDARKAGY                                             
*                                  USE DARE AGENCY IN OUTPUT                    
DAGY1000 EQU   *                                                                
         DROP  R2,R5                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKPROD :  IF RCONPRD = SPACES, MUST REPLACE THE FIELD FROM                 
*        PROD REC WITH THE LITERAL ENTERED BY THE USER, AND                     
*        STORED IN THE X'05' ELEMENT                                            
*                                                                               
CHKPROD  NTR1                                                                   
         CLC   RCONPRD,SPACES      ANY CODE IN ORDER?                           
         BNE   CPRO1000            YES - LEAVE AS IS                            
         LA    R6,RCONREC          NO  - FIND PRODUCT NAME ELEMENT              
         MVI   ELCODE,X'05'        PRODUCT EXPANSION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NO CODE, NO PRODUCT NAME?                    
         XC    TRANSWRK,TRANSWRK   CLEAR OUT WORK AREA                          
         LR    R5,R6               CALCULATE END OF ELEMENT                     
         LA    R5,21(R5)           POINT TO LAST CHARACTER                      
         LA    R0,20               SET LOOP CONTROL                             
CPRO0020 EQU   *                                                                
         CLI   0(R5),X'40'         CHARACTER = SPACE OR LESS?                   
         BH    CPRO0040            NO  - SIGNIFICANT CHAR FOUND                 
         BCTR  R5,0                YES - BACK UP ONE CHARACTER                  
         BCT   R0,CPRO0020         GO BACK FOR NEXT                             
         DC    H'0'                EMPTY ELEMENT?  N.G.                         
CPRO0040 EQU   *                                                                
         LA    R6,2(R6)            SKIP OVER ELEMENT CONTROL                    
         LR    RF,R0               SET REMAINING LENGTH                         
         EX    RF,CPRO0100         MOVE PRODUCT NAME BY LENGTH                  
         B     CPRO1000            EXIT                                         
CPRO0100 MVC   TRANSWRK(0),0(R6)                                                
CPRO1000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE HEXOUT TO EBCDIC, PLACE IN COMMON STORAGE AREA                    
*                                                                               
TRANSHEX NTR1                                                                   
         XC    TRANSWRK,TRANSWRK   CLEAR TEMPORARY BUILD AREA                   
         ZICM  RF,DEIPLEN(R2),2    GET FIELD INPUT LENGTH                       
         ST    RF,DMCB+8           SET LENGTH OF FIELD                          
         GOTO1 HEXOUT,DMCB,(R5),TRANSWRK,,=C'TOG'                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FIELD IS EBCDIC.  MOVE TO COMMON WORK FIELD.                                
*                                                                               
TRANSEBC NTR1                                                                   
         XC    TRANSWRK,TRANSWRK   CLEAR TEMPORARY BUILD AREA                   
         CLI   DEIPFORM+1(R2),C'V' INPUT = EBCDIC/VARIABLE LEN?                 
         BE    TREB0020            YES - VAR LEN IS IN AN ELEMENT               
         ZICM  RF,DEIPLEN(R2),2    GET LENGTH OF INPUT FIELD                    
         BCTR  RF,0                DECREMENT FOR EX                             
         B     TREB0040                                                         
TREB0020 EQU   *                                                                
         LR    RE,R5               GET LENGTH OF ELEMENT                        
         ZICM  RF,DERECDIS(R2),4   TAKE DATA DISPLACEMENT FROM TABLE            
         SR    RE,RF               A(FIELD) - DISPLACE -> ELT START             
         ZIC   RF,1(RE)            TAKE ELEMENT LENGTH                          
         ZICM  RE,DERECDIS(R2),4   TAKE DATA DISPLACEMENT AGAIN                 
         SR    RF,RE               SUBTRACT DISPLACEMENT FROM LENGTH            
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
TREB0040 EQU   *                                                                
         EX    RF,TREB0100         MOVE BY LENGTH                               
TREB0060 EQU   *                                                                
         XIT1                                                                   
TREB0100 EQU   *                                                                
         MVC   TRANSWRK(0),0(R5)   MOVE FIELD BY LENGTH                         
         EJECT                                                                  
*                                                                               
*   MOVE BINARY VALUE TO A STANDARD FOUR-CHARACTER FIELD FOR                    
*        EDITING.  FIELD BEING MOVED MUST BE RIGHT-ALIGNED                      
*        IN A BINARY-ZERO FIELD.                                                
*                                                                               
TRANSBIN NTR1                                                                   
*                                                                               
         XC    TRANSWRK,TRANSWRK   CLEAR TEMPORARY BUILD AREA                   
         XC    BINARWRK,BINARWRK   CLEAR BINARY RECEIVING FIELD                 
         LA    RE,BINARWRK         SET A(RECEIVING FIELD)                       
*                                                                               
         ZICM  RF,DEIPLEN(R2),2    GET INPUT FIELD LENGTH                       
         LNR   RF,RF               MAKE VALUE NEGATIVE                          
         LA    RF,4(RF)            ADD SIZE OF FIELD TO CALCULATE               
*                                     DISPLACEMENT FOR NEW DATA                 
         AR    RE,RF               SET D(NEW FIELD)                             
         ZICM  RF,DEIPLEN(R2),2    GET INPUT FIELD LENGTH AGAIN                 
         BCTR  RF,0                DECREMENT LENGTH FOR EX                      
         EX    RF,TRBI0100         MOVE FIELD BY LENGTH                         
         CLI   DEOPFORM+1(R2),C'L' INCLUDE LEADING ZEROS?                       
         BNE   TRBI0020            NO                                           
         PRINT GEN                                                              
         EDIT  (4,BINARWRK),(12,TRANSWRK+16),FILL=0                             
         PRINT NOGEN                                                            
         B     TRBI0040                                                         
TRBI0020 EQU   *                                                                
         PRINT GEN                                                              
         EDIT  (4,BINARWRK),(12,TRANSWRK+16)                                    
         PRINT NOGEN                                                            
TRBI0040 EQU   *                                                                
         ZICM  RE,DEOPLEN(R2),2    GET FIELD OUTPUT LENGTH                      
         LNR   RE,RE               MAKE VALUE NEGATIVE                          
         LA    RE,12(RE)           CALCULATE DISPLACEMENT TO FIELD              
         LA    RF,TRANSWRK+16(RE)     WITHIN TRANSWRK                           
         ZICM  RE,DEOPLEN(R2),2    GET FIELD OUTPUT LENGTH AGAIN                
         BCTR  RE,0                DECREMENT LENGTH FOR EX                      
         EX    RE,TRBI0200         MOVE FIELD BY LENGTH                         
         XC    TRANSWRK+16(48),TRANSWRK+16                                      
*                                  CLEAR REMAINDER OF FIELD                     
         XIT1                                                                   
TRBI0100 EQU   *                                                                
         MVC   0(0,RE),0(R5)       MOVE FIELD BY LENGTH                         
TRBI0200 EQU   *                                                                
         MVC   TRANSWRK(0),0(RF)   MOVE FIELD BY LENGTH                         
         EJECT                                                                  
*                                                                               
*   CONVERT DATE INPUT TO OUTPUT FORMATS, PLACE RESULT IN COMMON AREA           
*                                                                               
TRANSDAT NTR1                                                                   
         XC    TRANSWRK,TRANSWRK                                                
         PRINT GEN                                                              
         GOTO1 ,DMCB,0(R5),TRANSWRK                                             
         ZIC   RF,DEIPFORM+1(R2)   GET INPUT FORM OF DATE                       
         STC   RF,DMCB             INSERT INTO P1, 1ST BYTE                     
         ZIC   RF,DEOPFORM+1(R2)   GET OUTPUT FORM OF DATE                      
         STC   RF,DMCB+4           INSERT INTO P2, 1ST BYTE                     
         GOTO1 DATCON              CALL DATCON                                  
         PRINT NOGEN                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHECK FLAG CONTENT.  REPLACE WITH REQUIRED OUTPUT BYTES,                    
*        DEPENDING IF FLAG IS ON OR OFF                                         
*                                                                               
TRANSFLG NTR1                                                                   
         XC    TRANSWRK,TRANSWRK                                                
         ZIC   RF,DEIPFORM+1(R2)   GET INPUT VALUE OF FLAG                      
         ZIC   RE,0(R5)            GET VALUE OF INPUT FROM RECORD               
         NR    RF,RE               'AND' TWO VALUES                             
         STC   RF,BYTE                                                          
         MVC   TRANSWRK(1),DEFLGVAL(R2)                                         
*                                  SET 'ON' VALUE OF FLAG                       
         CLC   BYTE,DEIPFORM+1(R2) FINAL VALUE = INPUT VALUE?                   
         BE    TFLG0020            YES - ALL BITS ON                            
         MVC   TRANSWRK(1),DEFLGVAL+1(R2)                                       
*                                  SET 'OFF' VALUE OF FLAG                      
TFLG0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE DATE TO JDS OUTPUT FORMAT.                                        
*                                                                               
JDSDAYS  NTR1                                                                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   JDSD0060            NOT FOUND - EXIT                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    JDSD0040            NO  - ONLY 1                                 
         MVC   EC203DAY(2),=C'*P'  2 OR MORE: INDICATE PATTERN                  
***      GOTO1 CALNDRFL,DMCB,(RC)                                               
***      MVC   EC203CAL,SPACES     CLEAR X'S IN ROTATOR FIELD                   
         B     JDSD0060                                                         
JDSD0040 EQU   *                                                                
         LA    RF,RBUYDAYS         A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),RBUYDYIN    INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =V(DAYUNPK),DMCB,,(0,XDATE),RR=YES                               
         MVC   EC203DAY,XDATE      LOAD RESULT TO OUTPUT RECORD                 
JDSD0060 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*   TRANSLATE TIME TO JDS OUTPUT FORMAT.                                        
*                                                                               
JDSTIME  NTR1                                                                   
         MVC   WORK(12),SPACES                                                  
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    JDST0020            NO  - ONLY 1                                 
         MVC   WORK(2),=C'*P'      YES - INSERT PATTERN INDICATOR               
         B     JDST0040            EXIT                                         
JDST0020 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   JDST0100            NOT FOUND - EXIT                             
         USING RBUYDYEL,R6                                                      
                                                                                
         GOTO1 =V(UNTIME),DMCB,(0,RBUYDYT1),WORK,RR=YES                         
JDST0040 EQU   *                                                                
         MVC   EC203ORT(11),WORK   SET TIME STRING                              
*                                     FIELD EXTENDS INTO FOLLOWING              
*                                        EC203ORB FIELD, WHICH IS               
*                                        OTHERWISE NOT USED                     
*                                                                               
         MVC   HEXSTTIM(12),FOXZEROS CLEAR START/END TIMES                      
         EDIT  RBUYDYT1,(4,HEXSTTIM),FILL=0                                     
         EDIT  RBUYDYT2,(4,HEXENTIM),FILL=0                                     
         CLC   =C'0500',HEXSTTIM   BEFORE START OF DAY?                         
         BL    JDST0060            NO                                           
         MVI   HEXSTTIM+4,C'M'     YES - INSERT 'M' INDICATOR                   
JDST0060 EQU   *                                                                
         CLC   =C'0500',HEXENTIM   BEFORE START OF DAY?                         
         BL    JDST0080            NO                                           
         MVI   HEXENTIM+4,C'M'     YES - INSERT 'M' INDICATOR                   
JDST0080 EQU   *                                                                
*                                                                               
         MVC   EC203STM(12),HEXSTTIM                                            
*                                  LOAD LOCK TIMES                              
***      GOTO1 CALNDRFL,DMCB,(RC)                                               
JDST0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*    GET CALENDAR DAYS FROM BITS 8-15 OF 02 ELEMENT                             
*                                                                               
CALNDRFL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   EC203CAL,SPACES     CLEAR ROTATOR FIELDS                         
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     CALN0040                                                         
CALN0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
CALN0040 EQU   *                                                                
         BNE   CALN0100            NOT FOUND - EXIT                             
         LA    RF,EC203CAL         A(CALENDAR)                                  
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
**>>DYTM BIAS FORMAT                                                            
*                                                                               
*   TRANSLATE DAY TO BIAS FORMAT.                                               
*                                                                               
BIASDAYS NTR1                                                                   
         MVC   XDATE,SPACES        SPACE FILL WORK AREA                         
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET DAY/TIME ELEMENT                         
         BNE   BIAD0100            NOT FOUND - EXIT                             
         LA    RF,3(R6)            A(DAYS OF WEEK)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),2(R6)       INSERT ROTATOR DAYS                          
         OI    DMCB,X'80'          TURN ON 'REP' INDICATOR                      
         GOTO1 =A(DAYUNPK),DMCB,,(0,XDATE),RR=YES                               
         BAS   RE,CHKSHORT         CHECK STRING FOR TRUNC'D CHARS               
         BAS   RE,CHKXDATE         CHANGE DDS STRING TO BIAS                    
         BAS   RE,CHKXTRDA         CHECK FOR EXTRA CHAR IN STRING               
         MVC   EC203DAY,XDATE      LOAD RESULT TO OUTPUT RECORD                 
BIAD0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE TIME TO BIAS FORMAT                                               
*                                                                               
BIASTIME NTR1                                                                   
         MVC   WORK(12),SPACES                                                  
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    BIAT0010            NO  - ONLY 1                                 
         MVC   WORK(2),=C'*P'      YES - INSERT PATTERN INDICATOR               
         MVC   EC203ORT,WORK       LOAD TO OUTPUT AREA                          
         B     BIAT0120            EXIT                                         
BIAT0010 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET DAY/TIME ELEMENT                         
         BNE   BIAT0100            NOT FOUND - EXIT                             
         CLC   =C'TB',4(R6)        START TIME = TBA?                            
         BNE   BIAT0020            NO                                           
         MVI   EC203BBT,C'8'       YES - FILL TIMES WITH X'F8'                  
         MVC   EC203BBT+1(7),EC203BBT                                           
         B     BIAT0100                                                         
BIAT0020 EQU   *                                                                
         EDIT  (2,4(R6)),(4,EC203BBT),FILL=0,ZERO=NOBLANK                       
*                                  SET BEGINNING TIME                           
         CLC   =C'CC',6(R6)        END TIME = CC?                               
         BNE   BIAT0040            NO                                           
         MVC   EC203BET,=C'9999'   YES - FILL END TIME WITH X'F9'               
         B     BIAT0060                                                         
BIAT0040 EQU   *                                                                
         EDIT  (2,6(R6)),(4,EC203BET),FILL=0,ZERO=NOBLANK                       
BIAT0060 EQU   *                                                                
         CLC   =C'00',EC203BBT     BEGIN TIME = 00?                             
         BNE   BIAT0080            NO                                           
         MVC   EC203BBT(2),=C'24'  YES - SET TO 2400                            
BIAT0080 EQU   *                                                                
         CLC   =C'00',EC203BET     END TIME HOURS = ZERO?                       
         BNE   BIAT0100            NO                                           
         CLC   =C'00',EC203BET+2   YES - END TIME MINS = ZERO?                  
         BE    BIAT0100            YES - END TIME ALL ZERO                      
         MVC   EC203BET(2),=C'24'  NO  - HRS = ZERO, MINS HAVE VALUE            
*                                     SET HOURS TO 24                           
BIAT0100 EQU   *                                                                
*                                  STRING START+END TIMES                       
         MVC   EC203ORT(4),EC203BBT                                             
         MVI   EC203ORT+4,C'-'                                                  
         MVC   EC203ORT+5(4),EC203BET                                           
BIAT0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**>>BIAS SUPPORT ROUTINES                                                       
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
**>>BIAS SUPPORT ROUTINES                                                       
*                                                                               
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
         ZICM  R1,RSTALEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,ASTAREC          SET A(STATION RECORD)                        
         LA    RE,RSTAREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE STATION RECORD TO SAVE                  
         ZICM  R1,RSTALEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
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
         EJECT                                                                  
*********************************************************************           
*        GETCON --- READ CONTRACT RECORD                                        
*********************************************************************           
GETCON   NMOD1 0,*GCON*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         LA    RF,RCONREC                                                       
         ST    RF,ACONREC          SET A(CONREC)                                
*                                                                               
*   RETRIEVE SUPPORT INFORMATION FROM VARIOUS RECORDS                           
*                                                                               
         MVC   ANXTSUPT,AIO4       SET A(FIRST/NEXT SUPPORT REC)                
*                                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKEY,X'1A'       AGENCY RECORD                                
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
         GOTO1 VGETREC,DMCB,RAGY2REC                                            
         ZICM  R1,RAGY2LEN,2       RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,AAGYREC          SET A(AGENCY RECORD)                         
         LA    RE,RAGY2REC                                                      
         MOVE  ((RF),(R1)),(RE)    MOVE AGENCY RECORD TO SAVE                   
         ZICM  R1,RAGY2LEN,2       GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
*                                                                               
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
         ZICM  R1,RADVLEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,AADVREC          SET A(ADVERT RECORD)                         
         LA    RE,RADVREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE ADVERT RECORD TO SAVE                   
         ZICM  R1,RADVLEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
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
         ZICM  R1,RSALLEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,ASALREC          SET A(S/P    RECORD)                         
         LA    RE,RSALREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE S/P    RECORD TO SAVE                   
         ZICM  R1,RSALLEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
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
         ZICM  R1,ROFFLEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,AOFFREC          SET A(OFFICE RECORD)                         
         LA    RE,ROFFREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE OFFICE RECORD TO SAVE                   
         ZICM  R1,ROFFLEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
*                                                                               
         XC    APRDREC,APRDREC     CLEAR                                        
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BNE   GCON0060            YES - GET RECORD                             
         LA    R6,RCONREC          NO  - RETRIEVE X'05' ELEMENT                 
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    GCON0050                                                         
         DC    H'0'                NO PRODUCT CODE NAME ELEMENT                 
GCON0050 EQU   *                                                                
**       MVC   PRODNAME,2(R6)      LOAD PRODUCT NAME                            
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
         ZICM  R1,RPRDLEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,APRDREC          SET A(PRODUCT RECORD)                        
         LA    RE,RPRDREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE PRODUCT RECORD TO SAVE                  
         ZICM  R1,RPRDLEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
*                                                                               
GCON0080 EQU   *                                                                
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,X'01'       REP    RECORD                                
         MVC   RREPKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RREPKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0090                                                         
         DC    H'0',C'MISSING REP RECORD'                                       
         DS    0H                                                               
GCON0090 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RREPREC                                             
         ZICM  R1,RREPLEN,2        RETRIEVE RECORD LENGTH                       
         L     RF,ANXTSUPT         SET A(NEXT SUPPORT RECORD SPACE)             
         ST    RF,AREPREC          SET A(REP RECORD)                            
         LA    RE,RREPREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE REP RECORD TO SAVE                      
         ZICM  R1,RREPLEN,2        GET RECORD LENGTH AGAIN                      
         L     RF,ANXTSUPT         GET A(NEXT SUPP REC SPACE) AGAIN             
         AR    RF,R1               ADD TO GET END OF RECORD                     
         XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT END                       
         LA    RF,2(RF)            BUMP PAST EOR BYTES                          
         ST    RF,ANXTSUPT         SAVE NEW A(NEXT SUPP REC SPACE)              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REDDSENT                                                       
RDARED   DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         CSECT                                                                  
       ++INCLUDE REDAYUNPK                                                      
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075RECNT75   05/01/02'                                      
         END                                                                    
