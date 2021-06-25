*          DATA SET REREPP202  AT LEVEL 124 AS OF 05/01/02                      
*PHASE REP202B,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPP202  (REP202A) --- PETRY COMMENT BUILDER'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPP202  -- PETRY CONVERSION: STRIP COMMENTS FROM       *            
*                      PETRY TAPE, BUILD RECORDS FOR LOAD TO       *            
*                      PETRY DDS FILE.  USED BY CONVERSION.        *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* MAY24/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
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
*     QUESTOR+0   = Y  DISPLAY COMPETITIVE COMMENTS                *            
*     QUESTOR+1   = Y  DISPLAY ORDER COMMENTS                      *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REP102   CSECT                                                                  
         NMOD1 0,**REP1**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0640                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
MAIN0040 EQU   *                                                                
         GOTO1 GETTPREC,DMCB,(RC)                                               
*                                  UNBLOCK AND DELIVER TAPE REC                 
         BZ    MAIN0400            CC = ZERO = END OF FILE                      
         CLC   RECORD2+30(2),=X'0066'                                           
*                                  LAST RECORD TO BE CONVERTED                  
*                                     COMPLETED?                                
         BH    MAIN0400            YES - FINISHED                               
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR           INCREMENT TOTAL RECORDS READ                 
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
         C     RF,=F'10000'        DISPLAY EVERY N RECORDS                      
         BNE   MAIN0060                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  TOTCTR,(10,P+15)     DISPLAY TOTAL COUNTER                       
         EDIT  CONCTR,(10,P+28)     DISPLAY CONTRACT COUNTER                    
         EDIT  BUYCTR,(10,P+41)     DISPLAY BUY      COUNTER                    
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
MAIN0060 EQU   *                                                                
         LA    RF,RECTABLE         IS RECORD TO BE PROCESSED?                   
MAIN0080 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    MAIN0040            YES - SKIP THIS RECORD TYPE                  
         CLC   0(2,RF),RECORD2+30  CHECK RECORD TYPE AGAINST TABLE              
         BE    MAIN0120            PROCESS                                      
         LA    RF,2(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0080            GO BACK FOR NEXT                             
*                                                                               
RECTABLE EQU   *                                                                
         DC    X'0032'             REP ORDER COMMENT                            
         DC    X'0039'             STATION ORDER COMMENT                        
         DC    X'0066'             COMPETITIVE COMMENT                          
         DC    X'FFFF'             DELIMITER                                    
         DS    0F                                                               
*                                                                               
*   REPTABLE:                                                                   
*        8-BYTE ENTRIES CONSISTING OF:                                          
*        BYTES  1  -  2   =  REP CODE FROM JDS FILE                             
*        BYTES  3  -  4   =  REP CODE IN OUTPUT FILE (DDS)                      
*        BYTES  5  -  8   =  DISPLACEMENT INTO RECORD TYPE COUNTS               
*                                                                               
*EPTABLE EQU   *                   TEST TABLE                                   
*        DC    C'NWV4',F'0'        NEW WORLD                                    
*        DC    C'03F2',F'4'        LOCAL:  WJW                                  
*        DC    C'04F1',F'8'        LOCAL:  WJBK                                 
*        DC    C'05F3',F'12'       LOCAL:  WITI                                 
*        DC    X'FFFF'             DELIMITER                                    
REPTABLE EQU   *                   PRODUCTION TABLE                             
         DC    C'NWFN',F'0'        NEW WORLD                                    
         DC    C'03JA',F'4'        LOCAL:  WJW                                  
         DC    C'04JB',F'8'        LOCAL:  WJBK                                 
         DC    C'05IW',F'12'       LOCAL:  WITI                                 
         DC    X'FFFF'             DELIMITER                                    
MAIN0120 EQU   *                                                                
*                                                                               
         CLC   =X'0066',RECORD2+30 COMP/ACTIV COMT FOUND?                       
         BNE   MAIN0140            NO                                           
         OC    JCMPKCNT,JCMPKCNT   ANY CONTINUATION INDICATOR?                  
         BNZ   MAIN0040            YES - DON'T PROCESS CONTINUATIONS            
         LA    RF,REPTABLE         YES - VALIDATE REP CODE                      
MAIN0130 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    MAIN0040            YES - SKIP THIS REP CODE                     
         CLC   0(2,RF),JCMPKREP    CHECK RECORD REP AGAINST TABLE               
         BE    MAIN0135            PROCESS                                      
         LA    RF,8(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0130            GO BACK FOR NEXT                             
MAIN0135 EQU   *                                                                
         MVC   REPOUTCD,2(RF)      SAVE CONVERSION REP CODE                     
         MVC   REPDISP,4(RF)       SAVE COUNT DISPLACEMENT                      
         GOTO1 =A(DISPIACT),DMCB,(RC),RR=Y                                      
         GOTO1 CMPRPROC,DMCB,(RC)                                               
         LA    RF,CMPRCTR          A(1ST COMP REC COUNT)                        
         L     RE,REPDISP          D(THIS REP'S COUNT)                          
         AR    RE,RF               DISPLACE TO COUNTER                          
         L     RF,0(RE)            GET CURRENT COUNT                            
         LA    RF,1(RF)            INCREMENT                                    
         ST    RF,0(RE)            PUT IT BACK                                  
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0140 EQU   *                                                                
         LA    RF,REPTABLE         YES - VALIDATE REP CODE                      
MAIN0150 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    MAIN0040            YES - SKIP THIS REP CODE                     
         CLC   0(2,RF),JORDKREP    CHECK RECORD REP AGAINST TABLE               
         BE    MAIN0155            PROCESS                                      
         LA    RF,8(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0150            GO BACK FOR NEXT                             
MAIN0155 EQU   *                                                                
         MVC   REPOUTCD,2(RF)      SAVE CONVERSION REP CODE                     
         MVC   REPDISP,4(RF)       SAVE COUNT DISPLACEMENT                      
         CLC   =X'0032',RECORD2+30 REP ORDER COMT   FOUND?                      
         BNE   MAIN0160            NO                                           
         GOTO1 =A(DISPIROR),DMCB,(RC),RR=Y                                      
         GOTO1 ORDRPROC,DMCB,(RC),1                                             
         LA    RF,RORDCTR          A(1ST REP ORDER REC COUNT)                   
         L     RE,REPDISP          D(THIS REP'S COUNT)                          
         AR    RE,RF               DISPLACE TO COUNTER                          
         L     RF,0(RE)            GET CURRENT COUNT                            
         LA    RF,1(RF)            INCREMENT                                    
         ST    RF,0(RE)            PUT IT BACK                                  
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0160 EQU   *                                                                
         CLC   =X'0039',RECORD2+30 STATION ORDER CMT FOUND?                     
         BNE   MAIN0180            NO                                           
         GOTO1 =A(DISPISOR),DMCB,(RC),RR=Y                                      
         GOTO1 ORDRPROC,DMCB,(RC),2                                             
         LA    RF,SORDCTR          A(1ST REP STATION REC COUNT)                 
         L     RE,REPDISP          D(THIS REP'S COUNT)                          
         AR    RE,RF               DISPLACE TO COUNTER                          
         L     RF,0(RE)            GET CURRENT COUNT                            
         LA    RF,1(RF)            INCREMENT                                    
         ST    RF,0(RE)            PUT IT BACK                                  
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0180 EQU   *                                                                
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0400 EQU   *                                                                
MAIN0420 EQU   *                                                                
         CLOSE INTAPE                                                           
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=RELO                                   
*                                  DISPLAY TOTALS FOR RUN                       
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
*                                                                               
MAIN0640 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
         DS    0F                                                               
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   BLKFLAG,C'Y'        SET 'BLOCK EMPTY' FLAG                       
         MVI   BLK#1,C'Y'          SET 'FIRST PASS' FLAG                        
         MVI   NAMEDUMP,C'N'       SET 'NEED NAME DUMP' FLAG                    
         MVI   LOADALFA,C'N'       SET 'LOAD ALPHA TBL' FLAG                    
         OPEN  (INTAPE,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1 MEG STORAGE SPACE                      
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'30000'        TAPE BUFFER AREA:                            
         ST    RF,AAGYAREA         SET A(AGENCY AREA)                           
         ST    RF,AAGYEND          SET A(NEXT AGENCY)                           
         A     RF,=F'120000'       SAVE ROOM FOR 10,000 ENTRIES                 
         ST    RF,AAG2AREA         SET A(MISSING AGENCY TABLE)                  
         ST    RF,AAG2END          SET A(NEXT MISSING AGENCY)                   
         A     RF,=F'120000'       SAVE ROOM FOR 20,000 ENTRIES                 
         ST    RF,ACMPAREA         SET A(STATION AREA)                          
         ST    RF,ACMPNXT          SET A(NEXT AVAILABLE SLOT)                   
         MVI   CMPFLAG,C'N'                                                     
*                                                                               
*   LEAVE SUFFICIENT SPACE FOR STATION COMPETITIVE TABLE                        
*        EACH ENTRY:                                                            
*        4 CHARS:  BASE STATION                                                 
*        7 CHARS:  EACH OF SIX (?) COMPS (MAX) + REP'D                          
*        2000 STATIONS TOTAL (?)                                                
*        7 X 7 X 2000 =  98000                                                  
*                                                                               
         A     RF,=F'98000'        SIZE OF COMP TABLE                           
         ST    RF,ASTAAREA         SET A(STATION GRP/SUBGRP AREA)               
*                                                                               
*   LEAVE SUFFICIENT SPACE FOR STATION GROUP/SUBGROUP TABLE                     
*        EACH ENTRY:                                                            
*        4 CHARS:  BASE STATION                                                 
*        2 CHARS:  GROUP/SUBGROUP                                               
*        1 CHAR :  ACE/GRAPHNET INDICATOR                                       
*        SPARE     5 CHARACTERS                                                 
*        12 X 400  =  4800                                                      
*                                                                               
         A     RF,=F'6000'         SIZE OF STATION GROUP/SUBGRP TABLE           
         ST    RF,ASALAREA         SET A(SALESPERSON AREA)                      
*                                                                               
*   LEAVE SUFFICIENT SPACE IN SALESPERSON TABLE FOR 2,000 ENTRIES.              
*        EACH ENTRY:  (16 CHARS)                                                
*        4 CHARS:  JDS CODE                                                     
*        2 CHARS:  JDS DIVISION/TEAM                                            
*        2 CHARS:  JDS OFFICE                                                   
*        3 CHARS:  DDS CODE                                                     
*        2 CHARS:  DDS DIVISION/TEAM                                            
*        3 CHARS:  SPARE                                                        
*                                                                               
         A     RF,=F'32000'        2ND S/P TABLE                                
         ST    RF,A2SPAREA         SET A(2ND S/P AREA)                          
         ST    RF,A2SPEND          SET A(NEXT SLOT)                             
*                                                                               
*   LEAVE SUFFICIENT SPACE IN 2ND S/P TABLE FOR 6,000 ENTRIES.                  
*        EACH ENTRY:  (3 CHARS)                                                 
*        3 CHARS:  DDS CODE                                                     
*                                                                               
         A     RF,=F'18000'        MISSING STATION TABLE                        
         ST    RF,AMISAREA         SET A(MISSING STATION AREA)                  
         ST    RF,ANEXTMIS         SET A(NEXT MISSING SLOT)                     
*                                                                               
*                                                                               
*   LEAVE SUFFICIENT SPACE IN MISSING STATION                                   
*        TABLE FOR 2000 BYTES.                                                  
         A     RF,=F'2000'         NEWLY ADDED SALESPERSON TABLE                
         ST    RF,ANEWSALS         SET A(NEWLY ADDED S/P AREA)                  
         ST    RF,ANXTSAL          SET A(NEXT ADDED SLOT)                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN NEW SALESPERSON                                   
*        TABLE FOR 10000 ENTRIES: 30,000 BYTES                                  
         A     RF,=F'30000'                                                     
         ST    RF,ASPNAREA         SET A(SALESPERSON NAME AREA)                 
         ST    RF,ASPNEND                                                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN SALESPERSON NAME TABLE FOR ??? ENTRIES            
*        OF 50 CHARS EACH:                                                      
*        CHARS 1 - 8 :  JDS S/P CODE - DIV/TEAM - OFFICE                        
*        CHAR  9 - 38:  JDS S/P NAME (1ST 30 CHARS)                             
*        CHAR 29 - 50:  JDS S/P TELEPHONE NUMBER                                
*                                                                               
*****>>>>>>>>   NEW TABLE ENTRIES GET ADDED HERE!!                              
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
         MVC   BUCKWORK(4),GETBROAD                                             
*                                  SET UP BUCKET UPDATE ARGUMENTS               
         MVC   BUCKWORK+4(4),GETDAY                                             
         MVC   BUCKWORK+8(4),ADDAY                                              
         MVC   BUCKWORK+12(4),DATCON                                            
         CLC   QRECORD+20(12),SPACES                                            
*                                  ANY DISPLAY VALUES?                          
         BE    INIT0060            NO                                           
         PACK  DUB,QRECORD+20(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,LOWCTR           SAVE LOW VALUE                               
         PACK  DUB,QRECORD+26(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,HIGHCTR          SAVE HIGH VALUE                              
*                                                                               
INIT0060 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  GETTPREC:  UNBLOCK PETRY RECORD INPUT.  EXPAND KEY.  MOVE     *              
*             RECORD TO RECORD2, WHERE IT WILL BE PROCESSED.     *              
*             UPON EOF, SET CC = ZERO, WHICH WILL END JOB        *              
*             ELSE SET CC NOT ZERO.                              *              
******************************************************************              
*                                                                               
*                                                                               
*                                                                               
GETTPREC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*  TAPE READ IS TO BE DONE WHEN A BLOCK IS REQUIRED.  WHEN A RECORD             
*        REMAINS IN THE BLOCK, IT IS TO BE UNBLOCKED, AND DELIVERED             
*        TO THE RECORD2 AREA                                                    
*                                                                               
GETT0005 EQU   *                                                                
         CLI   BLKFLAG,C'Y'        EMPTY BLOCK?                                 
         BNE   GETT0020            NO  - BUMP TO NEXT RECORD                    
GETT0010 EQU   *                                                                
         GET   INTAPE,RECORD3      READ TAPE RECORD INTO RDA                    
*                                     END OF FILE -> GETT0100 (DCB)             
         CLI   BLK#1,C'Y'          SKIP FIRST RECORD?                           
         BNE   GETT0015            NO                                           
         MVI   BLK#1,C'N'          YES - SET 'NOT FIRST BLOCK'                  
         B     GETT0010            GO BACK AND READ NEXT                        
GETT0015 EQU   *                                                                
         LA    R6,RECORD3          CALCULATE EOR, ETC                           
         ZICM  R5,RECORD3,2        GET BLOCK LENGTH FROM RECORD                 
         AR    R5,R6               CALCULATE END OF BLOCK                       
         ST    R5,BLKEND           SAVE A(END OF RECORD)                        
         LA    R6,12(R6)           BUMP TO L(FIRST RECORD)                      
         ST    R6,BLKADDR          SAVE A(REC IN PROCESS)                       
         B     GETT0040            DON'T BUMP TO NEXT RECORD                    
GETT0020 EQU   *                                                                
         MVI   BLKFLAG,C'N'        SET 'BLOCK NOT EMPTY'                        
         L     R6,BLKADDR          LOAD A(RECORD IN PROCESS)                    
         PRINT GEN                                                              
         ZICM  R5,0(R6),2          GET RECORD LENGTH                            
         PRINT NOGEN                                                            
         AR    R6,R5               BUMP TO NEXT RECORD                          
         L     R5,BLKEND           GET EOR                                      
         CR    R6,R5               END OF RECORD REACHED?                       
         BNL   GETT0010            YES - READ ANOTHER BLOCK                     
         ST    R6,BLKADDR          NO  - STORE A(NEW REC IN PROCESS)            
GETT0040 EQU   *                                                                
         MVI   BLKFLAG,C'N'        SET 'BLOCK NOT EMPTY'                        
         LR    R1,R6               SAVE A(CONTROL BYTES)                        
         MVC   RECORD2+24(6),0(R1) INSERT CONTROL INFORMATION                   
         LA    R6,6(R6)            SET A(FIRST CHARACTER TO MOVE)               
         LA    R5,RECORD2+30       SET A(RECEIVING RECORD)                      
*                                     SKIP CONTROL 15 JDS WORDS                 
*                                  SET DISPLACEMENT INTO RECVING RECORD         
         LA    R1,4(R1)            SET A(COMPRESSED BYTE COUNT)                 
         PRINT GEN                                                              
         ZIC   R2,0(R1)            GET COMPRESSED BYTE COUNT                    
         PRINT NOGEN                                                            
         AR    R5,R2               DISPLACE INTO RECEIVING REC                  
         L     R1,BLKADDR          SET A(RECORD IN PROCESS)                     
         ZICM  R2,0(R1),2          RETRIEVE RECORD LENGTH                       
         LR    RF,R5               SET A(RECEIVING FIELD)                       
         LR    RE,R6               SET A(SENDING FIELD)                         
         LR    R1,R2               SET L(RECEIVING FIELD)                       
         PRINT GEN                                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         PRINT NOGEN                                                            
         ZICM  R6,RECORD2+24,2     GET RECORD LENGTH                            
         ZIC   R5,RECORD2+28       GET COMPRESSED LENGTH                        
         AR    R6,R5               CALCULATE NEW RECORD LEN =                   
*                                     ORIG LEN + L(DUPE KEY PORTION)            
         STCM  R6,3,RECORD2+24     RESET RECORD LENGTH                          
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GETT0200            EXIT ROUTINE                                 
GETT0060 EQU   *                                                                
         MVC   0(0,R5),0(R6)       MOVE DATA BY LENGTH                          
GETT0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO = EOF                          
GETT0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CMPRPROC:  PROCESS THE COMPETITIVE COMMENT RECORD             *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CMPRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   CMFNDFLG,C'N'       SET 'NO CMMT FOUND' FLAG                     
         XCEFL RXXXREC,1000        CLEAR TEMPCOM RECORD                         
         MVI   RXXXREC,X'47'       INSERT RECORD TYPE                           
         MVC   RXXXKREP,REPOUTCD   INSERT REP CODE FOR THIS RECORD              
CMPR0020 EQU   *                                                                
         MVC   WORK+20(8),FOXZEROS                                              
         LA    R1,8                SET LOOP CONTROL                             
         LA    RF,WORK+27          SET LAST POS OF RECEIVING FIELD              
         LA    RE,JCMPKCON+7       SET LAST POS OF SENDING   FIELD              
CMPR0040 EQU   *                                                                
         CLI   0(RE),C' '          SENDING FIELD A SPACE?                       
         BE    CMPR0060            YES - BACK UP SENDING ONLY                   
         MVC   0(1,RF),0(RE)       NO  - MOVE SENDING TO RECEIVING              
         BCTR  RF,0                BACK UP RECEIVING FIELD                      
CMPR0060 EQU   *                                                                
         BCTR  RE,0                BACK UP SENDING   FIELD                      
         BCT   R1,CMPR0040         GO BACK FOR NEXT FIELD                       
*                                                                               
         GOTO1 =V(HEXIN),DMCB,WORK+20,RXXXKCON,8,=C'TOG'                        
*                                  INSERT CONTRACT NUMBER INTO RECORD           
*                                     TYPE OF COMMENT = 0                       
         MVC   RXXXLEN(2),=X'0026' INSERT RECORD LEN: 34 + 4                    
         MVC   RXXXELEM(2),=X'0104' SET DESCRIPTOR ELEMENT                      
         GOTO1 JDSDATE,DMCB,JCMPDATE,WORK,(2,0)                                 
*                                  INSERT DATE ENTERED                          
         GOTO1 DATCON,DMCB,(3,WORK),(2,RXXXDATE)                                
*                                  CONVERT DATE TO 2-CHAR FORMAT                
         MVI   JDSELT,X'03'        GET COMPETITIVE COMMENT ELEMENT              
CMPR0080 EQU   *                                                                
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     CMPR0120                                                         
CMPR0100 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
CMPR0120 EQU   *                                                                
         BNZ   CMPR0180            NOT FOUND/FINISHED                           
         MVI   CMFNDFLG,C'Y'       SET 'CMMT FOUND' FLAG                        
         L     RF,JDSADDR          SET A(ELEMENT FOUND)                         
         L     RE,JDSLEN           SET L(JDS DATA)                              
         CLC   JDSLEN,=F'60'       COMMENT EXCEEDS 60 CHARS?                    
         BNH   CMPR0140            NO                                           
         LA    RE,60               YES - SET MAX TO 60                          
CMPR0140 EQU   *                                                                
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CMPR0160         MOVE COMPET COMMENT BY LENGTH                
         MVC   ELTBILD1(1),JDSELT  INSERT COMPET COMMENT ELT CODE               
         LA    RE,3(RE)            CALCULATE L(ELEMENT)                         
         STC   RE,ELTBILD1+1       INSERT INTO ELEMENT                          
*                                                                               
*   TEST                                                                        
*        BAS   RE,TESTDISP                                                      
*   TEST END                                                                    
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RXXXREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
*                                  ADD BY ELEMENT CODE ONLY                     
         XC    ELTBILD1,ELTBILD1                                                
*                                                                               
*   TEST                                                                        
*        BAS   RE,TESTDIS2                                                      
*   TEST END                                                                    
*                                                                               
         B     CMPR0100            GO BACK AND CHECK IF FOUND                   
*                                  PERMIT OPEN-ENDED # OF COMMENTS              
CMPR0160 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
CMPR0180 EQU   *                                                                
         CLI   JDSELT,X'04'        SECONDARY ELEMENTS DONE?                     
         BE    CMPR0200            YES                                          
         MVI   JDSELT,X'04'        NO  - GO BACK AND DO THEM                    
         B     CMPR0080                                                         
CMPR0200 EQU   *                                                                
         CLI   CMFNDFLG,C'Y'       COMMENT FOUND?                               
         BNE   CMPR0220            NO  - FINISHED                               
         GOTO1 PUTRECS,DMCB,(RC)   GENERATE O/P REC FOR COMMENTS                
         CLI   QUESTOR+0,C'Y'      DISPLAY COMPETITIVE COMMENTS                 
         BNE   CMPR0220            NO                                           
         LA    RF,CMPRCTR          YES - SELECT PROPER COUNTER FOR              
         L     RE,REPDISP             COMPETITIVE  RECORDS                      
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY 1ST N RECORDS                        
         BH    CMPR0220                                                         
         MVC   P+10(14),=C'COMP/ACT O/P :'                                      
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
CMPR0220 EQU   *                                                                
         XIT1                                                                   
FOXZEROS DC    CL8'00000000'                                                    
         EJECT                                                                  
*                                                                               
*  TESTDISP                                                                     
*                                                                               
TESTDISP NTR1                                                                   
         MVC   P+01(21),=C'COMP RECORD BUILDING:'                               
         MVC   P+25(25),ELTBILD1                                                
         EDIT  RXXXLEN,(4,P+54)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RXXXREC          A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RXXXLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  TESTDIS2                                                                     
*                                                                               
TESTDIS2 NTR1                                                                   
         MVC   P+01(18),=C'COMP RECORD BUILT:'                                  
         EDIT  RXXXLEN,(4,P+54)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RXXXREC          A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RXXXLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  ORDRPROC:  PROCESS THE REP/STATION ORDER COMMENT RECORD       *              
*      ORDER FLAG:  0  = REP ORDER COMMENT                       *              
*                   1  = STA ORDER COMMENT                       *              
******************************************************************              
*                                                                               
ORDRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   ORDRFLAG,7(R1)      SAVE ORDER FLAG                              
         MVI   CMFNDFLG,C'N'       SET 'NO COMMENT FLAG'                        
         BAS   RE,ALLSPACE         CHECK FOR ALL SPACES                         
         BZ    ORDR0200            ALL COMMENTS ARE SPACES                      
*                                                                               
*   TEST                                                                        
*        MVC   P+1(21),=C'COMMT BEING PROCESSED'                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         XCEFL RXXXREC,1000        CLEAR TEMPCOM RECORD                         
         MVI   RXXXREC,X'47'       INSERT RECORD TYPE                           
         MVC   RXXXKREP,REPOUTCD   INSERT REP CODE FOR THIS RECORD              
         MVC   RXXXKCON,JORDKCON                                                
         MVC   RXXXKTP2,ORDRFLAG   SET 'ORDER COMMENT TYPE'                     
*                                                                               
         MVC   RXXXLEN(2),=X'0026' INSERT RECORD LEN: 34 + 4                    
         MVC   RXXXELEM(2),=X'0104' SET DESCRIPTOR ELEMENT                      
         MVI   JDSELT,X'01'        GET ORDER COMMENT ELEMENT                    
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     ORDR0040                                                         
ORDR0020 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
ORDR0040 EQU   *                                                                
         BNZ   ORDR0100            NOT FOUND/FINISHED                           
         MVI   CMFNDFLG,C'Y'       SET 'COMMENT FOUND'                          
         L     RF,JDSADDR          SET A(ELEMENT FOUND)                         
         L     RE,JDSLEN           SET L(JDS DATA)                              
         CLC   JDSLEN,=F'60'       COMMENT EXCEEDS 60 CHARS?                    
         BNH   ORDR0060            NO                                           
         LA    RE,60               YES - SET MAX TO 60                          
ORDR0060 EQU   *                                                                
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,ORDR0080         MOVE ORDER COMMENT BY LENGTH                 
         MVI   ELTBILD1,X'03'      INSERT ORDER COMMENT ELT CODE                
         LA    RE,3(RE)            CALCULATE L(ELEMENT)                         
         STC   RE,ELTBILD1+1       INSERT INTO ELEMENT                          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RXXXREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
*                                  ADD BY ELEMENT CODE ONLY                     
         XC    ELTBILD1,ELTBILD1                                                
         B     ORDR0020            GO BACK AND CHECK IF FOUND                   
*                                  PERMIT OPEN-ENDED # OF COMMENTS              
ORDR0080 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
ORDR0100 EQU   *                                                                
         CLI   CMFNDFLG,C'Y'       ANY COMMENT FOUND?                           
         BNE   ORDR0200            NO  - COMMENT EMPTY                          
         GOTO1 PUTRECS,DMCB,(RC)   GENERATE O/P REC FOR COMMENTS                
         CLI   QUESTOR+1,C'Y'      DISPLAY ORDER COMMENTS                       
         BNE   ORDR0200            NO                                           
         CLI   ORDRFLAG,1          REP ORDER COMMENT?                           
         BNE   ORDR0160            NO  - STATION                                
         LA    RF,RORDCTR          YES - SET APPROPRIATE COUNTER                
         L     RE,REPDISP                                                       
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY 1ST N RECORDS                        
         BH    ORDR0200                                                         
         MVC   P+1(18),=C'REP ORDER CMT O/P:'                                   
         B     ORDR0180                                                         
ORDR0160 EQU   *                                                                
         LA    RF,SORDCTR          YES - SET APPROPRIATE COUNTER                
         L     RE,REPDISP                                                       
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY 1ST N RECORDS                        
         BH    ORDR0200                                                         
         MVC   P+10(18),=C'STA ORDER CMT O/P:'                                  
ORDR0180 EQU   *                                                                
         MVC   P+30(2),REPOUTCD    DISPLAY OUTPUT CODE                          
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
ORDR0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ALLSPACE:  CHECK EACH ELEMENT.  IF ALL ARE SPACES, SKIP THE COMMT.          
*                                                                               
ALLSPACE NTR1                                                                   
         MVI   ALSPCFLG,C'N'       TURN 'VALID COMMENT' OFF                     
         MVI   JDSELT,X'01'        GET ORDER COMMENT ELEMENT                    
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     ALLS0040                                                         
ALLS0020 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
ALLS0040 EQU   *                                                                
         BNZ   ALLS0100            NOT FOUND/FINISHED                           
         L     RF,JDSADDR          SET A(ELEMENT FOUND)                         
         L     RE,JDSLEN           SET L(JDS DATA)                              
         CLC   JDSLEN,=F'60'       COMMENT EXCEEDS 60 CHARS?                    
         BNH   ALLS0060            NO                                           
         LA    RE,60               YES - SET MAX TO 60                          
ALLS0060 EQU   *                                                                
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,ALLS0099         COMPARE FOR SPACE                            
         BE    ALLS0020            ALL SPACE - LEAVE FLAG OFF                   
         MVI   ALSPCFLG,C'Y'       NOT ALL SPACE:  TURN FLAG ON                 
         B     ALLS0100                                                         
ALLS0099 CLC   2(0,RF),SPACES      COMPARE COMMENT TO SPACES                    
ALLS0100 EQU   *                                                                
         CLI   ALSPCFLG,C'Y'       VALID COMMENT FOUND?                         
         BNE   ALLS0200            NO  - EXIT CC ZERO                           
         LTR   RB,RB               YES - EXIT CC NOT ZERO                       
         B     ALLS0240                                                         
ALLS0200 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
ALLS0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   GETJDSEL:  RETRIEVE A(ELEMENT SOUGHT) BASED ON ELEMENT TYPE                 
*   GETJDSNX:  RETRIEVE NEXT ELEMENT OF TYPE.  JDSELT MUST BE SET,              
*        AND JDSADDR MUST CONTAIN ADDRESS OF LAST ELEMENT FOUND.                
*                                                                               
GETJDSNX NTR1                                                                   
*                                                                               
*   TEST                                                                        
         CLC   JCMPKCON,=C'1696608 '                                            
         BNE   GTST0001                                                         
         MVC   P+1(08),=C'NX SEEK:'                                             
         GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,JDSADDR,P+17,1,=C'TOG'                               
         EDIT  JCONLEN,(3,P+25)                                                 
         GOTO1 REPORT                                                           
GTST0001 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LA    R1,RECORD2+24       SET A(RECORD IN PROCESS: LENGTH)             
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         L     R1,JDSADDR          SET A(LAST ELEMENT)                          
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0400            COMPRESSED: SKIP THIS ELEMENT                
         B     GJDS0100            UNCOMPRESSED: SKIP THIS ELEMENT              
GETJDSEL NTR1                                                                   
*                                                                               
*   TEST                                                                        
         CLC   JCMPKCON,=C'1696608 '                                            
         BNE   GTST0010                                                         
         MVC   P+1(08),=C'EL SEEK:'                                             
         GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,JDSADDR,P+17,1,=C'TOG'                               
         EDIT  JCONLEN,(3,P+25)                                                 
*        GOTO1 REPORT                                                           
GTST0010 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LA    R1,RECORD2+24       SET A(RECORD LENGTH)                         
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         LA    R1,RECORD2+66       SET A(DESCRIPTOR ELEMENT)                    
         CLI   0(R1),0             EMPTY ELEMENT FOUND?                         
         BE    GJDS0800            YES - END OF RECORD REACHED                  
*                                                                               
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0200            COMPRESSED:                                  
GJDS0020 EQU   *                                                                
         CR    R1,R2               END OF RECORD REACHED?                       
         BNL   GJDS0800            YES - NO ELT: RETURN CC NOT ZERO             
*                                                                               
*   TEST                                                                        
*        MVC   P+1(04),=C'ELT='                                                 
*        MVC   P+5(20),0(R1)                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   0(R1),0             EMPTY ELEMENT FOUND?                         
         BE    GJDS0800            YES - END OF RECORD REACHED                  
*                                                                               
         TM    0(R1),X'F0'         ELT COMPRESSED/UNCOMPRESSED?                 
         BNZ   GJDS0200            COMPRESSED                                   
*                                  UNCOMPRESSED                                 
         CLC   JDSELT,0(R1)        COMPARE ELEMENT TYPE                         
         BNE   GJDS0100            NOT EQUAL - GO TO NEXT                       
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         ZIC   RF,1(R1)            GET ELEMENT LENGTH                           
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         SH    RF,=H'2'            SUBTRACT TWO BYTES FOR CONTROL               
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0100 EQU   *                   BUMP PAST UNCOMPRESSED ELT                   
         ZIC   RF,1(R1)            RETRIEVE ELEMENT LENGTH                      
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               ADD TO ELEMENT ADDR                          
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0200 EQU   *                                                                
         ZIC   RE,0(R1)            UNLOAD ELT/LENGTH                            
         SRL   RE,4                DROP LENGTH NYBBLE                           
         SLL   RE,4                RESTORE TO ORIG PLACE                        
         STC   RE,JDSELT2                                                       
         CLC   JDSELT,JDSELT2      COMPARE ELT SOUGHT VS THIS ELT               
         BNE   GJDS0400            NOT FOUND -                                  
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         BCTR  RF,0                SUBTRACT 1 BYTE FOR CONTROL                  
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0400 EQU   *                                                                
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0800 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   BTST0800                                                         
*        MVC   P+1(08),=C'EXIT N/F'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0800 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GJDS1200                                                         
GJDS1000 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   BTST1000                                                         
*        MVC   P+1(08),=C'EXIT FND'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST1000 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         SR    R0,R0                                                            
GJDS1200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   JDSDATE:  CONVERT JDS FORMAT DATES TO A DDS FORMAT DATE                     
*        P1  =   ADDR(INPUT DATE)                                               
*        P2  =   ADDR(OUTPUT DATE)                                              
*        P3  =   DATE TYPE:  2  = YMD 2 BYTE FORMAT                             
*                            1  = YM  1 BYTE FORMAT                             
*        P4  =                                                                  
*                                                                               
JDSDATE  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         MVC   DATEFLAG,8(R1)                                                   
         ZIC   RF,0(R2)            GET YR/MON BYTE                              
         SRL   RF,4                DROP MONTH NYBBLE                            
         A     RF,BASEYEAR         ADD BASE YEAR TO YEAR BYTE                   
         STC   RF,0(R3)            INSERT YEAR INTO OUTPUT                      
         ZIC   RF,0(R2)            GET YR/MON BYTE AGAIN                        
         SLL   RF,28               DROP YEAR NYBBLE                             
         SRL   RF,28               REALIGN MONTH                                
         STC   RF,1(R3)            INSERT MONTH INTO OUTPUT                     
         CLI   DATEFLAG,1          YEAR/MONTH ONLY?                             
         BE    JDSD0100            YES - FINISHED                               
         MVC   2(1,R3),1(R2)       NO  - INSERT DAY INTO OUTPUT                 
JDSD0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   HELLO CALLS:  LABEL NAME INDICATES RECORD AND ELEMENT ADDRESS:              
*        HELO  =  HELLO CALL                                                    
*        CON   =  CONTRACT RECORD, BUY   =  BUY RECORD                          
*        1     =  ELTBILD1, ETC                                                 
*                                                                               
HELOCON1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCONA NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
*                                  INSERT ON CODE SEQUENCE ONLY, AND            
*                                     DON'T CLEAR THE ELEMENT                   
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
HELOBUY1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUYA NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
*                                  DON'T CLEAR THE ELEMENT                      
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOREC4 NTR1                                                                   
         L     RF,ARECORD4                                                      
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),(RF),ELTBILD1,0                
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RXXXREC          SET A(RECORD BEING OUTPUT)                   
*                                                                               
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
         LA    RF,REC                                                           
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(R2)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RXXXLEN-RXXXKEY(R2)                                     
*                                  INSERT LENGTH, USING COMMON RCONLEN          
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
ADDRFILL DS    CL500                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
ASTAAREA DS    A                   STATION/GROUP-SUBGROUP AREA                  
ASTAEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ACMPAREA DS    A                   STATION COMPETITIVE AREA                     
ACMPNXT  DS    A                   A(NEXT AVAILABLE COMP SLOT)                  
ASALAREA DS    A                   A(SALESPERSON AREA)                          
ASALEND  DS    A                   A(LAST ENTRY IN TABLE)                       
A2SPAREA DS    A                   A(2ND S/P AREA)                              
A2SPEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AMISAREA DS    A                   A(STATION MISSING AREA)                      
ANEXTMIS DS    A                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AAG2AREA DS    A                   MISSING AGENCY AREA                          
AAG2END  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWSALS DS    A                                                                
ANXTSAL  DS    A                                                                
ASPNAREA DS    A                                                                
ASPNEND  DS    A                                                                
ANEWPPRS DS    A                                                                
ANXTPPR  DS    A                                                                
ARECORD4 DS    A                                                                
ARECORD5 DS    A                                                                
NEWPPRCT DS    F                                                                
LBLDAREA DS    F                                                                
TOTCTR   DS    F                                                                
DISPCTR  DS    F                                                                
CONDCTR  DS    F                                                                
BUYDCTR  DS    F                                                                
CONCTR   DS    F                                                                
CMPRCTR  DS    F                                                                
CMPRCTR1 DS    F                                                                
CMPRCTR2 DS    F                                                                
CMPRCTR3 DS    F                                                                
RORDCTR  DS    F                                                                
RORDCTR1 DS    F                                                                
RORDCTR2 DS    F                                                                
RORDCTR3 DS    F                                                                
SORDCTR  DS    F                                                                
SORDCTR1 DS    F                                                                
SORDCTR2 DS    F                                                                
SORDCTR3 DS    F                                                                
CONSREAD DS    F                                                                
NAMCTR   DS    F                                                                
BUYCTR   DS    F                                                                
BUYSREAD DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTRIN DS    F                                                                
AGYCTR   DS    F                                                                
AGYCTR2  DS    F                                                                
AGYCTR3  DS    F                                                                
AG2CTR   DS    F                                                                
TSPTCTR  DS    F                                                                
TCOSCTR  DS    F                                                                
TWEEKCTR DS    F                                                                
NEWSPCTR DS    F                                                                
NEWSTATS DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
STACTR   DS    F                                                                
CMPCTR   DS    F                                                                
COMPCTR  DS    F                                                                
SALCTR   DS    F                                                                
SP2CTR   DS    F                                                                
SALMISS  DS    F                                                                
MISSTAT  DS    F                                                                
AIOAREA  DS    F                                                                
BUCKWORK DS    4F                  BUCKET UPDATE ARGUMENTS                      
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
*                                                                               
KEYSAV2  DS    CL27                                                             
DATEFLAG DS    CL1                                                              
CMPFLAG  DS    CL1                                                              
ACEGRAPH DS    XL1                 ACE/GRAPHNET INDICATOR                       
JDSELT   DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
JDSELT2  DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
BLKFLAG  DS    CL1                 BLOCK EMPTY FLAG                             
BLK#1    DS    CL1                 BLOCK 'FIRST PASS' FLAG                      
NAMEDUMP DS    CL1                                                              
LOADALFA DS    CL1                                                              
CONFLAG  DS    CL1                                                              
ORDRFLAG DS    CL1                                                              
CMFNDFLG DS    CL1                                                              
ALSPCFLG DS    CL1                                                              
SAVER2   DS    A                                                                
JDSADDR  DS    F                                                                
JDSLEN   DS    F                                                                
BLKADDR  DS    F                   ADDRESS OF CURRENT RECORD IN BLOCK           
BLKEND   DS    F                   ADDRESS OF CURRENT RECORD EOR                
BASEYEAR DC    F'84'               BASE YEAR IS UNDETERMINED!!                  
BUYREC4  DS    CL1                 RECORD IN BUYREC4 AREA FLAG                  
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=1024,             X        
               BLKSIZE=30720,MACRF=GM,EODAD=GETT0100                            
*                                                                               
         SPACE 3                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ISOLATE REC AFTER LTORG TO PRESERVE ADDRESSABILITY                          
*                                                                               
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2048              AREA FOR RECORD                              
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENXXX          TEMPORARY COMMENT RECORD                     
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT      RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSADV          ADVERTISER RECORD                            
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCMP          COMPETITIVE/ACTIVITY COMMENT                 
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSORD          REP/STA ORDER COMMENT                        
         EJECT                                                                  
*  OTHER JDS RECORD DSECTS GET ORG'D HERE                                       
         EJECT                                                                  
         ORG                                                                    
REPOUTCD DS    CL2                 OUTPUT REP FOR THIS RECORD                   
REPDISP  DS    F                   REP DISPLACEMENT                             
CTREDIT  DS    F                                                                
RECORD3  DS    CL1024                                                           
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         EJECT                                                                  
*********************************************************************           
         CSECT                                                                  
*                                                                               
******************************************************************              
*  DISPIROR:  DISPLAY JDS   RECORD INPUT.                        *              
******************************************************************              
*                                                                               
DISPIROR NMOD1 0,*DIRO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    RF,RORDCTR                                                       
         L     RE,REPDISP                                                       
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY FIRST N RECORD                       
         BH    DIRO0100                                                         
         MVC   P+1(13),=C'REP ORDER CM:'                                        
         MVC   CTREDIT,0(RF)                                                    
         EDIT  CTREDIT,(4,P+15)                                                 
         MVC   P+30(2),REPOUTCD    DISPLAY OUTPUT CODE                          
         MVI   P+32,C'/'                                                        
         MVC   P+33(2),JORDKREP                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
DIRO0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPISOR:  DISPLAY PETRY RECORD INPUT.                        *              
******************************************************************              
*                                                                               
DISPISOR NMOD1 0,*DISO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    RF,SORDCTR                                                       
         L     RE,REPDISP                                                       
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY FIRST N RECORD                       
         BH    DISO0100                                                         
         MVC   P+1(13),=C'STN ORDER CM:'                                        
         MVC   CTREDIT,0(RF)                                                    
         EDIT  CTREDIT,(4,P+15)                                                 
         MVC   P+30(2),REPOUTCD    DISPLAY OUTPUT CODE                          
         MVI   P+32,C'/'                                                        
         MVC   P+33(2),JORDKREP                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
DISO0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPIACT:  DISPLAY PETRY RECORD INPUT.                        *              
******************************************************************              
*                                                                               
DISPIACT NMOD1 0,*DIAC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    RF,CMPRCTR                                                       
         L     RE,REPDISP                                                       
         AR    RF,RE                                                            
         CLC   0(4,RF),=F'50'      DISPLAY FIRST N RECORD                       
         BH    DIAC0100                                                         
         MVC   P+1(13),=C'COMP/ACT  CM:'                                        
         MVC   CTREDIT,0(RF)                                                    
         EDIT  CTREDIT,(4,P+15)                                                 
         MVC   P+25(2),JCMPKREP                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
DIAC0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NMOD1 0,**DTOT**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
DITO0200 EQU   *                                                                
*                                                                               
         MVC   P+40(20),=C'**** RUN TOTALS ****'                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'TOTAL RECORDS READ     :'                             
         EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMPETITIVE COMMENTS NW:'                             
         EDIT  CMPRCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMPETITIVE COMMENTS 03:'                             
         EDIT  CMPRCTR1,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMPETITIVE COMMENTS 04:'                             
         EDIT  CMPRCTR2,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMPETITIVE COMMENTS 05:'                             
         EDIT  CMPRCTR3,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REP ORDER COMMENTS   NW:'                             
         EDIT  RORDCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REP ORDER COMMENTS   03:'                             
         EDIT  RORDCTR1,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REP ORDER COMMENTS   04:'                             
         EDIT  RORDCTR2,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REP ORDER COMMENTS   05:'                             
         EDIT  RORDCTR3,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATN ORDER COMMENTS NW:'                             
         EDIT  SORDCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATN ORDER COMMENTS 03:'                             
         EDIT  SORDCTR1,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATN ORDER COMMENTS 04:'                             
         EDIT  SORDCTR2,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATN ORDER COMMENTS 05:'                             
         EDIT  SORDCTR3,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**** INSERT                                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'124REREPP202 05/01/02'                                      
         END                                                                    
