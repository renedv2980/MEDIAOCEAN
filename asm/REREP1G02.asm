*          DATA SET REREP1G02  AT LEVEL 215 AS OF 05/01/02                      
*PHASE RE1G02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP1G02 - OFFICE BUDGET REPORT '                              
*********************************************************************           
*                                                                   *           
*        REREP1G02 --- REPPAK OFFICE BUDGET REPORT                  *           
*                                                                   *           
*  NOTE:  BECAUSE THIS REPORT DOES NOT USE A REGULAR SEQUENCE, BUT  *           
*     RATHER NEEDS DATA IN A UNIQUE ORDER, WITH IRREGULAR BREAKS,   *           
*     SOME 'LIBERTIES' HAVE BEEN TAKEN WITH THE NORMAL PROCESSING.  *           
*     MODE BREAKS REFERENCE ONE ANOTHER TO PRODUCE CORRECT DATA     *           
*     OUTPUT TO SORT, ETC.  SORRY ABOUT THAT.                       *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* SEP05/90 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* DEC21/92 (BU ) --- INCLUDE CHICAGO AND LA IN 'TOTAL REGIONAL'     *           
*                    (ADJUST TABLE)                                 *           
*                                                                   *           
* FEB03/93 (BU ) --- FIX GRAND TOTAL DISPLAY PAGE BREAK.            *           
*                                                                   *           
* FEB05/93 (BU ) --- CLOSE SORTFILE AFTER DATA EOF                  *           
*                                                                   *           
* MAY03/94 (BU ) --- FIX PERCENT CALCULATION WHEN $$ TOO GREAT      *           
*                                                                   *           
* MAR28/95 (BU ) --- BREAK OUT ATLANTA, DALLAS, DETROIT FOR TOTALS. *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* MAY23/97 (BU ) --- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* JAN26/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                    **  END TOMBSTONE  **                          *           
*********************************************************************           
*  DISPLAY OPTIONS USING REQUEST NAME AS FLAG:                      *           
*                                                                   *           
*  REQUEST -> **TEST**  **BUDG**  **SOUT**  **SBACK* **SCOMP*       *           
*                                                                   *           
*  BUDGET KEY    Y         Y                                        *           
*  SORT O/P      Y         Y*        Y                              *           
*  SORT I/P      Y                              Y                   *           
*  ACCUM SORT    Y                                       Y          *           
*                                                                   *           
*                          Y* = SORT O/P BUDGET RECORDS ONLY        *           
*                                                                   *           
*                                                                   *           
*  REQUEST -> DUMPTABL  --->  DISPLAY OF OFFICE TABLE BUILT         *           
*  REQUEST -> DATAFLOW  --->  DISPLAY OF MODE BREAK PROCESSING      *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R8  =  THIRD  BASE REGISTER                                  *           
*********************************************************************           
*                                                                               
RE1G02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1G02,R7,R8,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF),DMCB,(RC)      GO TO THE ROUTINE                            
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         B     MAINBAD             NON-ZERO IS ERROR                            
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*   NOTE:  ONE OF THE BREAKS REQUIRED FOR THIS REPORT IS NOT                    
*        AVAILABLE AS A 'MODE' IN THE SEQUENCE THIS REPORT IS                   
*        RUN IN.  AS A RESULT, ITS BREAKS ARE INSERTED IN THE                   
*        'POST' ROUTINE.  THE MISSING BREAK IS 'SUBGROUP'.                      
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(OFFFRST),AL3(OFFINIT)    OFFICE FIRST                        
         DC    AL1(TEAMFRST),AL3(TEMINIT)   TEAM   FIRST                        
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(TEAMLAST),AL3(TEMDONE)   TEAM    BREAK                       
         DC    AL1(OFFLAST),AL3(OFFDONE)    OFFICE  BREAK                       
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         L     RF,=A(SAVEREGS)                                                  
         STM   R2,RC,0(RF)         SAVE REGS 2 -> C                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVI   CONFLAG,C'N'        SET CONTRACT FLAG TO NO                      
         XC    EOSORT,EOSORT       RESET 'END OF DATA' FLAG                     
         XC    QTRCOUNT,QTRCOUNT   RESET COUNTER                                
         L     R3,=A(ALTHDR)       STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC2,SORTREC2                                                
*                                                                               
*   PICK UP 2ND REQUEST CARD                                                    
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED TWO CARDS                               
         BE    INIT0010                                                         
         MVC   P(L'CARDMISS),CARDMISS                                           
         B     BADEXIT                                                          
INIT0010 EQU   *                                                                
         L     R4,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R4)                                                     
         DROP  R5                                                               
         MVI   HIGHVALU,X'FF'      SET HIGHVALU TO ALL X'FF'                    
         MVC   HIGHVALU+1(L'HIGHVALU-1),HIGHVALU                                
         XC    PRISGRUP,PRISGRUP   SET PRIOR SUBGROUP TO X'00'                  
         XC    YEARONE(4),YEARONE                                               
*                                  INITIALIZE OFFICE BUDGET YEARS               
         MVC   YEARONE,QSTART      LOAD FIRST OFFICE BUDGET YEAR                
         CLC   QSTART(2),QEND      START/END IN SAME YEAR?                      
         BE    INIT0020            YES                                          
         MVC   YEARTWO,QEND        NO  - LOAD LAST OFFICE BUDGET YEAR           
INIT0020 EQU   *                                                                
         XC    DATETABL,DATETABL   INITIALIZE DATE TABLE                        
         LA    R4,DATETABL         A(DATETABL)                                  
         MVC   CONVRTYR(4),QEND    CONVERT END DATE OF REQUEST                  
*                                     INTO 1ST TABLE ENTRY                      
         GOTO1 DATCON,DMCB,(0,CONVRTYR),(3,0(R4))                               
         MVI   DATETABL+2,0        WIPE OUT 'DAY' OF DATE                       
INIT0030 EQU   *                                                                
         MVC   CONVRTYR(4),QSTART  CONVERT START-END DATES                      
*                                     TO YYMM TABLE                             
         GOTO1 DATCON,DMCB,(0,CONVRTYR),(3,WORK)                                
INIT0040 EQU   *                                                                
         CLC   WORK(2),0(R4)       END OF TABLE REACHED?                        
         BE    INIT0070            YES                                          
         MVC   2(2,R4),0(R4)       NO  - MOVE END DOWN ONE MONTH                
         MVC   0(2,R4),WORK        MOVE IN CONVERTED DATE                       
         CLI   WORK+1,12           MONTH OF DATE = DECEMBER?                    
         BNE   INIT0050            NO                                           
         MVI   WORK+1,1            YES - SET TO JANUARY                         
         ZIC   RF,WORK             BUMP UP YEAR                                 
         LA    RF,1(RF)                                                         
         STC   RF,WORK             STORE IT BACK                                
         B     INIT0060                                                         
INIT0050 EQU   *                                                                
         ZIC   RF,WORK+1           BUMP UP MONTH BY 1                           
         LA    RF,1(RF)                                                         
         STC   RF,WORK+1           STORE IT BACK                                
INIT0060 EQU   *                                                                
         LA    R4,2(R4)            BUMP UP MONTH TABLE                          
         B     INIT0040            GO BACK AND CHECK                            
INIT0070 EQU   *                                                                
         BAS   RE,LOADTABL         SET UP OPTIONS TABLE                         
*                                                                               
         CLC   =C'DUMPTABL',QUESTOR **TEST**                                    
         BNE   INIT0080                                                         
         BAS   RE,DUMPTABL         **TEST**                                     
*                                                                               
INIT0080 EQU   *                                                                
         BAS   RE,BUDGOUTS         GENERATE BUDGET INFORMATION                  
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*  LOADTABL:  RETRIEVE OFFICE RECORDS/PART II FOR THIS REP.  CHECK              
*        OPTION BITS.  IF FIRST BIT IS OFF, BYPASS.  IF ON, INSERT              
*        INTO TABLE, AND THEN CHECK SECOND BIT.  IF ON, SET SUBGROUP            
*        NEEDED FLAG TO YES.                                                    
*                                                                               
LOADTABL NTR1                                                                   
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         LA    RF,L'TEAMTABL       L(TEAM TABLE)                                
         LA    RE,TEAMTABL         A(TEAM TABLE)                                
         XCEFL                                                                  
         LA    R6,TEAMTABL         A(TABLE)                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           SKELETONIZE OFFICES IN TABLE                 
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                GET FIRST KEY                                
         B     LTAB0020                                                         
LTAB0010 EQU   *                                                                
         GOTO1 SEQ                 GET NEXT KEY                                 
LTAB0020 EQU   *                                                                
         CLI   KEY,X'04'           SAME RECORD TYPE?                            
         BNE   LTAB0060            NO  - FINISHED SKELETONIZING                 
         CLC   KEY+23(2),RCREPFL   SAME REP?                                    
         BNE   LTAB0060            NO  - FINISHED SKELETONIZING                 
         MVC   0(2,R6),RCREPFL     INSERT REP CODE INTO TABLE                   
         MVC   2(2,R6),KEY+25      INSERT OFFICE CODE INTO TABLE                
         MVC   5(2,R6),=C'NN'      INITIALIZE TEAM/SUBGROUP FLAGS               
         MVI   4(R6),X'20'         SET SEQUENCE NUMBER DEFAULT                  
*                                                                               
         LA    R1,OFFSEQTB         FIND SEQUENCE NUMBER                         
LTAB0030 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE REACHED?                        
         BE    LTAB0050            YES - GET NEXT KEY                           
         CLC   0(4,R6),0(R1)       REP/OFFICE FOUND IN TABLE?                   
         BNE   LTAB0040            NO  - BUMP TO NEXT                           
         MVC   4(1,R6),4(R1)       YES - LOAD SEQUENCE NUMBER TO TABLE          
         B     LTAB0050            GET NEXT KEY                                 
LTAB0040 EQU   *                                                                
         LA    R1,OFFSEQLN(R1)     BUMP TO NEXT OFF SEQ ENTRY                   
         B     LTAB0030            GO BACK FOR NEXT                             
LTAB0050 EQU   *                                                                
         LA    R6,TABENTRY(R6)     BUMP TO NEXT TEAM TABLE                      
         CLI   0(R6),X'FF'         TABLE DELIMITER REACHED?                     
         BNE   LTAB0010            NO                                           
         MVC   P(32),=C'**OFFICE TABLE MAXIMUM REACHED**'                       
         GOTO1 REPORT                                                           
LTAB0060 EQU   *                                                                
         LA    R6,TEAMTABL         A(TABLE)                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           TYPE:  OFFICE/PART II.                       
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
LTAB0070 EQU   *                                                                
         OC    0(4,R6),0(R6)       ANY REP/OFFICE IN TABLE SLOT?                
         BZ    LTAB0100            NO  - FINISHED                               
         MVC   KEY+25(2),2(R6)     INSERT OFFICE FROM TABLE                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEYSAVE(27),KEY     RECORD FOUND?                                
         BE    LTAB0080            YES - PROCESS IT                             
         LA    R6,TABENTRY(R6)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LTAB0070            GO BACK FOR NEXT                             
LTAB0080 EQU   *                                                                
         LA    R4,ROFF2REC         A(IO AREA FOR OFFICE REC 2)                  
         GOTO1 =A(LINKFILE),DMCB,(RC),(R4),GETREC                               
         LA    R3,ROFF2PRF         A(PROFILE BITS)                              
         TM    0(R3),X'80'         IS 'TEAM' REQUIRED FOR REPORT?               
         BNO   LTAB0090            NO  - READ NEXT RECORD                       
         MVI   5(R6),C'Y'          SET 'TEAM NEEDED' FLAG TO 'YES'              
         TM    0(R3),X'40'         IS SUBGROUP NEEDED?                          
         BNO   LTAB0090            NO  -                                        
         MVI   6(R6),C'Y'          SET 'SUBGROUP NEEDED' FLAG TO 'YES'          
LTAB0090 EQU   *                                                                
         LA    R6,TABENTRY(R6)     BUMP TO NEXT TABLE ENTRY                     
         B     LTAB0070            READ NEXT RECORD                             
LTAB0100 EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESTORE CONTRACT KEY                         
         CLI   KEY,X'0C'           IS IT A CONTRACT?                            
         BNE   LTAB0110            NO  - DON'T RESTART                          
         GOTO1 READ                REESTABLISH CONTRACT RECORD                  
LTAB0110 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  DUMPTABL:  ON OPTIONAL REQUEST (QUESTOR=DUMPTABL), THE CONTENTS              
*     OF THE TEAM TABLE WILL BE DISPLAYED WITHIN THE BODY OF                    
*     THE REPORT.                                                               
*                                                                               
DUMPTABL NTR1                                                                   
         LA    R2,TEAMTABL                                                      
DUMT0010 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    DUMT0020            YES                                          
         CLI   0(R2),X'FF'         DELIMITER REACHED?                           
         BE    DUMT0020            YES                                          
         MVC   P+10(TABENTRY),0(R2)                                             
*                                  MOVE TABLE ENTRY TO PRINT LINE               
         GOTO1 REPORT                                                           
         LA    R2,TABENTRY(R2)     BUMP ADDRESS WITHIN TABLE                    
         B     DUMT0010            GO BACK FOR NEXT                             
DUMT0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* TEAMTABL:  SEVEN POSITIONS PER ENTRY:                                         
*        POSITIONS 1-2     =   REP CODE                                         
*                  3-4     =   OFFICE CODE                                      
*                    5     =   SEQUENTIAL POSITION ON REPORT                    
*                                LOADED TO SOFFSEQ IN SORT RECORD               
*                    6     =   REQUIRES TEAM:      Y = YES                      
*                    7     =   REQUIRES SUBGROUP:  Y = YES                      
*                                                                               
*     IF REP/OFFICE CODE FOUND IN TABLE, REQUIRES TEAM CODE.  IF                
*       NOT, TEAM CODE PROHIBITED.                                              
*                                                                               
*     TABLE HAS SPACE FOR 150 SEVEN-BYTE ENTRIES                                
*                                                                               
TEAMTABL DS    CL1050                                                           
         DC    X'FFFF'             DELIMITER                                    
TABENTRY EQU   7                                                                
         EJECT                                                                  
*                                                                               
*  OFFICE SEQUENCE TABLE:  IF REPORT IS TO BE PRODUCED IN AN OFFICE             
*    ORDER OTHER THAN ALPHABETICAL, THE FOLLOWING TABLE DETAILS THAT            
*    ORDER.  FOR OFFICES THAT DON'T APPEAR IN THE TABLE, A SEQUENCE             
*    NUMBER OF X'21' IS INSERTED INTO THE SORT RECORD, FORCING THE              
*    OFFICES TO APPEAR ALPHABETICALLY WITHIN THIS GROUP.                        
*                                                                               
* LINE1: POSITIONS 1-2     =   REP CODE                                         
*                  3-4     =   OFFICE CODE                                      
*                    5     =   SEQUENTIAL POSITION ON REPORT                    
*                                LOADED TO SOFFSEQ IN SORT RECORD               
*                    6     =   OFFICE TOTALS WANTED?     Y/N                    
*                    7     =   SEQUENCE TOTALS WANTED?   Y/N                    
*                    8     =   SEQUENCE TOTALS AFTER SEQUENCE #                 
* LINE2: POSITIONS 1-20    =   SEQUENCE BREAK TITLE                             
*                                                                               
*        LAST ENTRY IN TABLE IS DEFAULT FOR TOTALS                              
*                                                                               
OFFSEQTB DS    0H                                                               
OFFSEQ#1 EQU   *                                                                
         DC    CL4'BLNY',XL1'01',CL2'NY',XL1'03'  BLAIR NEW YORK                
         DC    CL20'TOTAL NEW YORK      '                                       
OFFSEQLN EQU   *-OFFSEQ#1                      LENGTH OF ENTRY                  
         DC    CL4'BLAT',XL1'20',CL2'YY',XL1'20'  BLAIR ATLANTA                 
         DC    CL20'TOTAL REGIONAL      '                                       
         DC    CL4'BLCH',XL1'20',CL2'YY',XL1'20'  BLAIR CHICAGO                 
         DC    CL20'TOTAL REGIONAL      '                                       
         DC    CL4'BLDA',XL1'20',CL2'YY',XL1'20'  BLAIR DALLAS                  
         DC    CL20'TOTAL REGIONAL      '                                       
         DC    CL4'BLDE',XL1'20',CL2'YY',XL1'20'  BLAIR DETROIT                 
         DC    CL20'TOTAL REGIONAL      '                                       
         DC    CL4'BLLA',XL1'20',CL2'YY',XL1'20'  BLAIR LOS ANGELES             
         DC    CL20'TOTAL REGIONAL      '                                       
         DC    CL4'BLUW',XL1'21',CL2'NN',XL1'00'  BLAIR UNWIRED                 
         DC    CL20'TOTAL NY/REGIONAL/UW'                                       
         DC    CL4'SJNY',XL1'01',CL2'YY',XL1'03'  SJR   NEW YORK                
         DC    CL20'TOTAL NY/BOSTON/LA  '                                       
         DC    CL4'SJBO',XL1'02',CL2'YY',XL1'03'  SJR   BOSTON                  
         DC    CL20'TOTAL NY/BOSTON/LA  '                                       
         DC    CL4'SJLA',XL1'03',CL2'YY',XL1'03'  SJR   LOS ANGELES             
         DC    CL20'TOTAL NY/BOSTON/LA  '                                       
         DC    CL4'SJNL',XL1'21',CL2'NN',XL1'00'  SJR   NEW YORK LOCAL          
         DC    CL20'TOTAL NY/REGIONAL/NL'                                       
         DC    XL4'0000',XL1'00',CL2'NY',XL1'20'  DEFAULT                       
         DC    CL20'TOTAL REGIONAL      '                                       
         EJECT                                                                  
*                                                                               
*   BUDGOUT: SCAN THE BUDGET RECORDS FOR THE YEAR(S) OF THE REQUEST,            
*      AND GENERATE DETAILED (MONTHLY) SORT RECORDS FOR THE MONTHLY             
*      BUDGETS, AS WELL AS BUDGET SORT RECORDS FOR THE VARIOUS BREAKS           
*      NEEDED.  THESE SORT RECORDS ARE MERGED WITH THE DATA RECORDS             
*      TO PRODUCE THE DISPLAYED INFORMATION.                                    
*                                                                               
BUDGOUTS NTR1                                                                   
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         LA    R6,YEARONE                                                       
*                                                                               
BUDU0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'19'           LOAD RECORD TYPE                             
         MVC   KEY+17(2),RCREPFL   LOAD REP CODE                                
         MVC   KEY+19(2),0(R6)     LOAD YEAR OF REQUEST                         
         GOTO1 HIGH                                                             
         B     BUDU0030                                                         
BUDU0020 EQU   *                                                                
         GOTO1 SEQ                                                              
BUDU0030 EQU   *                                                                
         CLI   KEY,X'19'           SAME RECORD TYPE?                            
         BNE   BUDU0080            NO  - SCAN SECOND YEAR                       
         CLC   KEY+17(2),RCREPFL   SAME REP?                                    
         BNE   BUDU0080            NO  - SCAN SECOND YEAR                       
         CLC   KEY+19(2),0(R6)     SAME YEAR?                                   
         BNE   BUDU0080            NO  - SCAN SECOND YEAR                       
         CLC   QOFFICE,SPACES      ANY OFFICE FILTER?                           
         BE    BUDU0040            NO                                           
         CLC   QOFFICE,KEY+21      YES - OFFICE OF RECORD?                      
         BNE   BUDU0020            NO  - SKIP RECORD                            
BUDU0040 EQU   *                                                                
         CLC   QTEAM,SPACES        ANY TEAM FILTER?                             
         BE    BUDU0050            NO                                           
         CLC   QTEAM,KEY+24        YES - TEAM OF RECORD?                        
         BNE   BUDU0020            NO  - SKIP RECORD                            
BUDU0050 EQU   *                                                                
         CLC   QSBGROUP,SPACES     ANY SUBGROUP FILTER?                         
         BE    BUDU0060            NO                                           
         CLC   QSBGROUP,KEY+26     YES - SUBGROUP OF RECORD?                    
         BNE   BUDU0020            NO  - SKIP RECORD                            
BUDU0060 EQU   *                                                                
         LA    R2,ROBDRECS         A(OFFICE BUDGET RECORD)                      
         USING ROBDREC,R2                                                       
         GOTO1 =A(LINKFILE),DMCB,(RC),(R2),GETREC                               
*                                                                               
         CLC   =C'**BUDG**',QUESTOR                                             
         BE    BUDU0061                                                         
         CLC   =C'**TEST**',QUESTOR                                             
         BNE   BUDU0070                                                         
BUDU0061 EQU   *                                                                
         MVC   P+1(19),=C'BUDGET RECORD KEY: '                                  
         MVC   P+22(50),ROBDREC                                                 
         GOTO1 REPORT                                                           
BUDU0070 EQU   *                                                                
         GOTO1 GENBUDS,DMCB,(R6)   OUTPUT BUDGET RECORDS                        
         B     BUDU0020            GO BACK FOR NEXT BUDGET                      
BUDU0080 EQU   *                                                                
         LA    R6,2(R6)            BUMP TO NEXT YEAR                            
         CLI   0(R6),X'FF'         DELIMITER REACHED?                           
         BE    BUDU0090            YES - FINISHED                               
         OC    0(2,R6),0(R6)       IS ANOTHER YEAR NEEDED?                      
         BNZ   BUDU0010            YES - GO BACK AND DO IT                      
BUDU0090 EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESTORE CONTRACT KEY                         
         CLI   KEY,X'0C'           IS IT A CONTRACT RECORD?                     
         BNE   BUDU0100            NO  - DON'T TRY TO RESTART                   
         GOTO1 READ                REESTABLISH CONTRACT RECORD                  
BUDU0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GENBUDS: GENERATE THE DETAIL, SUB, AND GRAND TOTAL SORT RECORDS             
*        FOR EACH MONTH COVERED BY THE BUDGET.  THESE RECORDS                   
*        WILL BE MERGED IN AT THE APPROPRIATE LEVELS WITH THE                   
*        CONTRACT DETAILS                                                       
*                                                                               
GENBUDS  NTR1                                                                   
         L     R6,0(R1)            RELOAD A(YEAR IN PROGRESS)                   
         CLC   =C'**STOPAT',QUESTOR                                             
         BNE   GBUD0002                                                         
         CLC   QUESTOR+8(2),0(R6)  DISPLAY THIS YEAR?                           
         BNE   GBUD0002            NO  - KEEP GOING                             
         XC    SORTREC,SORTREC                                                  
*                                                                               
*   THE INSTRUCTION ABOVE GIVES A CONVENIENT PLACE TO INSERT A                  
*       SIMON TRAP ONCE THE FIRST BUDGET FOR THE YEAR IS FOUND.                 
*                                                                               
GBUD0002 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         MVI   SOFFSEQ,X'20'       SET OFFICE SEQUENCE DEFAULT                  
         LA    R1,OFFSEQTB                                                      
GBUD0010 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE REACHED?                        
         BE    GBUD0040            YES - FINISHED - USE DEFAULT                 
         CLC   0(2,R1),RCREPFL     REP FOUND?                                   
         BNE   GBUD0020            NO  -                                        
         CLC   2(2,R1),ROBDKOFF    OFFICE FOUND?                                
         BE    GBUD0030            YES -                                        
GBUD0020 EQU   *                                                                
         LA    R1,OFFSEQLN(R1)     BUMP TO NEXT TABLE ENTRY                     
         B     GBUD0010            GO BACK FOR NEXT                             
GBUD0030 EQU   *                                                                
         MVC   SOFFSEQ,4(R1)       INSERT SEQUENCE NUMBER                       
GBUD0040 EQU   *                                                                
         MVC   SOFFICE,ROBDKOFF    INSERT OFFICE INTO RECORD                    
         MVC   SOFFNAME(20),=C'OFFICE BUDGET ONLY  '                            
         MVC   SSUBGRP,ROBDKSUB+1  INSERT SUBGROUP INTO RECORD                  
         CLI   SSUBGRP,C' '        ANY SUBGROUP?                                
         BNE   GBUD0050            YES - USE IT                                 
         MVI   SSUBGRP,C'*'        INSERT DEFAULT SUBGROUP                      
GBUD0050 EQU   *                                                                
         MVC   STEAM,ROBDKTM       INSERT TEAM CODE, IF ANY                     
         MVC   CONVRTYR+2(4),=C'0101'                                           
*                                  GET BINARY FOR RECORD YEAR                   
         MVC   CONVRTYR(2),0(R6)   INSERT YEAR                                  
         GOTO1 DATCON,DMCB,CONVRTYR,(3,WORK+8)                                  
         LA    R3,DATETABL         A(DATE TABLE FOR REQUEST)                    
GBUD0060 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    GBUD0080            YES                                          
         CLC   0(1,R3),WORK+8      RECORD YR = YR OF TABLE ENTRY?               
         BNE   GBUD0070            NO                                           
         GOTO1 BUDGADDS,DMCB,(R3)                                               
*                                  GENERATE BUDGET RECORDS                      
GBUD0070 EQU   *                                                                
         LA    R3,2(R3)            BUMP TO NEXT DATE IN TABLE                   
         B     GBUD0060            GO BACK FOR NEXT                             
GBUD0080 EQU   *                                                                
         XC    SORTREC,SORTREC     REINITIALIZE MAIN SORT RECORD                
         XC    RESETREC,RESETREC   REINITIALIZE RESETREC                        
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*   BUDGADDS: LOAD THE BUDGET FIGURES HERE.  USE THE MONTHLY $$ DATE            
*     TO DISPLACE TO THE PROPER FIGURES WITHIN THE BUDGET RECORD.               
*                                                                               
BUDGADDS NTR1                                                                   
         L     R3,0(R1)            RELOAD A(DATE IN PROGRESS)                   
         XC    SCURBUDG,SCURBUDG   SET CURRENT BUDGET   TO ZERO                 
         XC    SCURFORE,SCURFORE   SET CURRENT FORECAST TO ZERO                 
         MVC   S$$DATE,0(R3)       INSERT DATE FROM TABLE                       
         MVI   SLOWKEY,1           FORCE BUDGET RECORDS TO                      
*                                     SORT AFTER DETAILS                        
         ZIC   RF,1(R3)            GET MONTH FROM TABLE DATE                    
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLL   RF,2                MULTIPLY BY 4 FOR OFFSET                     
         LA    RE,ROBDSTA-ROBDREC(R2)                                           
*                                  A(STATION BUDGETS)                           
         AR    RE,RF               ADD DISPLACEMENT                             
         MVC   SCURBUDG,0(RE)      INSERT REGULAR OFFICE BUDGET                 
         LA    RE,ROBDELEM-ROBDREC(R2)                                          
*                                  FIND FORECAST BUDGET ELEMENT                 
BADD0020 EQU   *                                                                
         ZIC   R6,1(RE)                                                         
         AR    RE,R6               BUMP TO NEXT ELEMENT                         
         CLI   0(RE),0             END OF RECORD?                               
         BE    BADD0030            YES - FINISHED                               
         CLI   0(RE),4             FORECAST BUDGET ELEMENT?                     
         BNE   BADD0020            NO  - GO BACK FOR NEXT                       
         LA    RE,2(RE)            A(FORECAST BUDGETS)                          
         AR    RE,RF               ADD DISPLACEMENT                             
         MVC   SCURFORE,0(RE)      INSERT FORECAST OFFICE BUDGET                
BADD0030 EQU   *                                                                
*                                                                               
*   PUT OUT DETAIL SORT RECORD                                                  
*                                                                               
         GOTO1 SORTOUT                                                          
         MVC   RESETREC,SORTREC    SAVE FOR RESETTING                           
*                                                                               
*   PUT OUT SUBGROUP LEVEL TOTAL RECORD, IF SUBGROUP REPORTED                   
*                                                                               
         CLI   SSUBGRP,0           ANY SUBGROUP IN RECORD?                      
         BE    BADD0040            NO  - SKIP SUBGROUP TOTAL REC                
         MVC   STEMNAME,SPACES     CLEAR THE TEAM NAME (IF ANY)                 
         MVC   STEMNAME(14),=C'SUBGROUP TOTAL'                                  
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         GOTO1 SORTOUT                                                          
BADD0040 EQU   *                                                                
*                                                                               
*   CHECK THE OFFICE SEQUENCE TABLE TO DETERMINE WHICH TOTAL                    
*      RECORD(S) ARE NEEDED.  THEN PUT THE PROPER ONES OUT,                     
*      ALONG WITH APPROPRIATE TOTAL HEADINGS.                                   
*                                                                               
         LA    R4,OFFSEQTB         A(OFFICE SEQUENCE TABLE)                     
BADD0041 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    BADD0044            YES                                          
         CLC   RCREPFL,0(R4)       SAME REP?                                    
         BNE   BADD0042            NO                                           
         CLC   SOFFICE,2(R4)       SAME OFFICE?                                 
         BE    BADD0044            YES                                          
BADD0042 EQU   *                                                                
         LA    R4,OFFSEQLN(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     BADD0041            GO BACK FOR NEXT                             
BADD0044 EQU   *                                                                
         CLI   5(R4),C'Y'          OFFICE TOTALS NEEDED?                        
         BNE   BADD0050            NO  - SKIP OFFICE TOTALS                     
*                                                                               
*   PUT OUT OFFICE LEVEL TOTAL RECORD                                           
*                                                                               
         MVI   SSUBGRP,X'FF'       SET SUBGROUP TO X'FF'                        
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         MVC   SSUBNAME,SPACES     SPACE-FILL FIELD                             
         MVC   SSUBNAME(12),=C'OFFICE TOTAL'                                    
         MVC   STEMNAME,SPACES     SPACE OUT TEAM NAME                          
         GOTO1 SORTOUT                                                          
BADD0050 EQU   *                                                                
*                                                                               
*   PUT OUT SEQUENCE LEVEL TOTAL RECORD                                         
*                                                                               
         CLC   QOFFICE,SPACES      ANY OFFICE FILTER?                           
         BNE   BADD0070            YES - NO SEQUENCE LEVEL TOTALS               
*                                        OR GRAND TOTALS NEEDED                 
         CLI   6(R4),C'Y'          SEQUENCE TOTALS NEEDED?                      
         BNE   BADD0060            NO  - SKIP SEQUENCE TOTALS                   
         MVC   SOFFSEQ,7(R4)       SET SEQUENCE LEVEL TOTAL POSITION            
         MVC   SOFFICE,HIGHVALU    SET OFFICE TO HIGHVALU                       
         MVI   SSUBGRP,X'FF'       SET SUBGROUP TO X'FF'                        
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         MVC   SOFFNAME(20),8(R4)  MOVE SEQUENCE BREAK TITLE IN                 
         MVC   SSUBNAME,SPACES     SPACE OUT SUBGROUP NAME                      
         MVC   STEMNAME,SPACES     SPACE OUT TEAM     NAME                      
         GOTO1 SORTOUT                                                          
BADD0060 EQU   *                                                                
*                                                                               
*   PUT OUT GRAND TOTAL RECORD                                                  
*                                                                               
         MVI   SOFFSEQ,X'FE'       SET SEQUENCE TO 'END'                        
         MVC   SOFFICE(5),HIGHVALU SET OFF/SUBGRP/TEAM TO X'FF'                 
         MVC   SSUBNAME,SPACES     SPACE OUT SUBGROUP NAME                      
         MVC   STEMNAME,SPACES     SPACE OUT TEAM     NAME                      
         MVC   SOFFNAME,SPACES     SPACE OUT OFFICE   NAME                      
         MVC   SOFFNAME(12),=C'REPORT TOTAL'                                    
         GOTO1 SORTOUT                                                          
*                                                                               
BADD0070 EQU   *                                                                
         MVC   SORTREC,RESETREC    RESTORE SORT RECORD AFTER                    
         XIT1                                                                   
         EJECT                                                                  
*   OFFINIT: FIRST OFFICE S                                                     
*                                                                               
OFFINIT  NTR1                                                                   
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   OFFI0000                                                         
         MVC   P+1(10),=C'OFFINIT   '                                           
         BAS   RE,DISPCODE                                                      
OFFI0000 EQU   *                                                                
         XC    STEAM,STEAM         INITIALIZE TEAM VALUE                        
         XC    SSUBGRP,SSUBGRP     INITIALIZE SUBGROUP VALUE                    
*                                                                               
         LA    RF,TEAMTABL                                                      
         MVI   SOFFSEQ,X'00'       SET 'NO SEQ: USE OFFICE                      
*                                     ALPHA CODE ORDER'                         
OFFI0010 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    OFFI0040            YES - LOAD ZEROS                             
         CLC   RCONKREP,0(RF)      SAME REP?                                    
         BNE   OFFI0030            NO  - BUMP TO NEXT                           
         CLC   RCONKOFF,2(RF)      YES - SAME OFFICE?                           
         BNE   OFFI0030            YES - LOAD IT UP                             
         MVC   SOFFSEQ,4(RF)       MOVE PRINT ORDER SEQ#                        
         CLI   6(RF),C'Y'          IS TEAM NEEDED FOR OFFICE?                   
         BNE   OFFI0040            NO  -                                        
         BAS   RE,TEMINIT          YES - FORCE A LOOK AT TEAM BREAK             
         B     OFFI0040                                                         
OFFI0030 EQU   *                                                                
         LA    RF,TABENTRY(RF)     BUMP TO NEXT ENTRY                           
         B     OFFI0010            GO BACK FOR NEXT                             
OFFI0040 EQU   *                                                                
         MVC   SOFFICE,RCONKOFF    INSERT OFFICE                                
         MVC   SOFFNAME(20),ROFFNAME   INSERT OFFICE NAME                       
OFFI0070 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*   TEMINIT: FIRST TEAM MODE SETTING                                            
*                                                                               
TEMINIT  NTR1                                                                   
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   TEMI0000                                                         
         MVC   P+1(10),=C'TEMINIT   '                                           
         BAS   RE,DISPCODE                                                      
TEMI0000 EQU   *                                                                
         MVC   STEMNAME,SPACES     CLEAR TEAM NAME                              
         XC    STEAM,STEAM         CLEAR TEAM CODE                              
         MVC   SSUBNAME,SPACES     CLEAR SUBGROUP NAME                          
         XC    SSUBGRP,SSUBGRP     CLEAR SUBGROUP CODE                          
         LA    RF,TEAMTABL                                                      
         MVC   SOFFSEQ,X'00'       CLEAR OFFICE SEQUENCE NUMBER                 
TEMI0010 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    TEMI0040            YES - LOAD ZEROS TO TEAM                     
         CLC   RCONKREP,0(RF)      SAME REP?                                    
         BNE   TEMI0020            NO  - BUMP TO NEXT                           
         CLC   RCONKOFF,2(RF)      YES - SAME OFFICE?                           
         BNE   TEMI0020            NO  - BUMP TO NEXT                           
         MVC   SOFFSEQ,4(RF)       YES - MOVE PRINT ORDER SEQ#                  
         CLI   5(RF),C'Y'          IS TEAM REQUIRED?                            
         BNE   TEMI0040            NO  - LOAD ZEROS TO TEAM                     
         B     TEMI0030            YES - LOAD TEAM, TEAM NAME                   
TEMI0020 EQU   *                                                                
         LA    RF,TABENTRY(RF)     BUMP TO NEXT ENTRY                           
         B     TEMI0010            GO BACK FOR NEXT                             
TEMI0030 EQU   *                                                                
         MVC   STEAM,RTEMKTEM      YES - INSERT TEAM CODE                       
         MVC   STEMNAME(17),=C'NO TEAM SPECIFIED'                               
*                                  INSERT DEFAULT TEAM NAME                     
         CLC   RTEMNAME,SPACES     ANY TEAM NAME?                               
         BE    TEMI0040            NO  - USE DEFAULT                            
         OC    RTEMNAME,RTEMNAME   ANY TEAM NAME?                               
         BZ    TEMI0040            NO  - USE DEFAULT                            
         XC    STEMNAME,STEMNAME   BLANK OUT DEFAULT                            
         MVC   STEMNAME(10),RTEMNAME   INSERT TEAM NAME                         
TEMI0040 EQU   *                                                                
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   TEMI0050                                                         
         MVC   P+1(10),=C'TEAM SORT '                                           
         MVC   P+15(80),SORTREC                                                 
         GOTO1 REPORT                                                           
TEMI0050 EQU   *                                                                
*                                                                               
*   INSERT OFFICE/OFFICE NAME FOR NEW TEAM                                      
*                                                                               
         MVC   SOFFICE,RCONKOFF        INSERT OFFICE                            
         MVC   SOFFNAME(20),ROFFNAME   INSERT OFFICE NAME                       
         B     MODEEXIT                                                         
         EJECT                                                                  
*   SUBINIT: FIRST SUBGROUP MODE SETTING                                        
*                                                                               
SUBINIT  NTR1                                                                   
*        CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
*        BNE   SUBI0000                                                         
*        MVC   P+1(10),=C'SUBINIT   '                                           
*        BAS   RE,DISPCODE                                                      
SUBI0000 EQU   *                                                                
         LA    RF,TEAMTABL                                                      
         MVI   SAVSGRP,X'0'        INITIALIZE SUBGROUP                          
         MVC   SAVSNAME,SPACES     INITIALIZE SUBGROUP NAME                     
SUBI0010 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    SUBI0060            YES - LOAD ZEROS TO TEAM                     
         CLC   RCONKREP,0(RF)      SAME REP?                                    
         BNE   SUBI0020            NO  - BUMP TO NEXT                           
         CLC   RCONKOFF,2(RF)      YES - SAME OFFICE?                           
         BNE   SUBI0020            YES - LOAD IT UP                             
         CLI   6(RF),C'Y'          IS SUBGROUP REQUIRED?                        
         BNE   SUBI0060            NO  - LOAD ZEROS TO SUBGROUP                 
         B     SUBI0030            YES - LOAD SUBGROUP, SUBGRP NAME             
SUBI0020 EQU   *                                                                
         LA    RF,TABENTRY(RF)     BUMP TO NEXT ENTRY                           
         B     SUBI0010            GO BACK FOR NEXT                             
SUBI0030 EQU   *                                                                
*                                                                               
*   NECESSARY TO RETRIEVE THE GROUP RECORD.  THIS SEQUENCE                      
*     DOES NOT DELIVER IT AS A FREE-BEE.                                        
         MVC   KEYSAV2(27),KEY     SAVE FOR CONTRACT RESTART                    
         XC    KEY,KEY                                                          
         MVI   KEY,7               LOAD RECORD TYPE                             
         MVC   KEY+23(2),RCREPFL   LOAD REP CODE                                
         MVC   KEY+25(2),RCONKGRP  LOAD GROUP/SUBGROUP                          
         GOTO1 HIGH                RETRIEVE RECORD                              
         CLC   KEYSAVE(27),KEY     RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         LA    R4,RGRPREC          A(GROUP RECORD)                              
         GOTO1 =A(LINKFILE),DMCB,(RC),(R4),GETREC                               
*                                                                               
         MVI   SAVSGRP,C'*'        INSERT DEFAULT CODE                          
         CLI   RGRPKGRP+1,C' '     ANY SUBGROUP CODE?                           
         BE    SUBI0040            NO  - USE DEFAULT CODE                       
         MVC   SAVSGRP,RGRPKGRP+1  INSERT SUBGROUP CODE                         
SUBI0040 EQU   *                                                                
         MVC   SAVSNAME(16),=C'NO SUBGROUP NAME'                                
*                                  INSERT DEFAULT SUBGROUP NAME                 
         CLC   RGRPSBNM,SPACES     ANY SUBGROUP NAME?                           
         BE    SUBI0050            NO  - USE DEFAULT                            
         OC    RGRPSBNM,RGRPSBNM   ANY SUBGROUP NAME?                           
         BZ    SUBI0050            NO  - USE DEFAULT                            
         MVC   SAVSNAME,SPACES     BLANK OUT SUBGROUP NAME                      
         MVC   SAVSNAME(10),RGRPSBNM                                            
*                                  INSERT SUBGROUP NAME                         
SUBI0050 EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESTORE CONTRACT KEY                         
         GOTO1 READ                REESTABLISH CONTRACT RECORD                  
*                                                                               
SUBI0060 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
POST     NTR1                                                                   
*                                                                               
*                                                                               
         GOTO1 CHEKSGRP            INSERT SUBGROUP INFO, IF NEEDED              
*                                                                               
*              NOW LOOP THROUGH THE MONTH TABLE                                 
*                                                                               
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R3,TMONTBL          A(ACCUMULATORS)                              
         LA    R1,L'TMONTBL(R3)    CALC END OF TABLE FOR WRAP                   
*                                                                               
POST0010 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0020            FOUND - BEGIN TO PULL FIGURES                
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0010            GO BACK FOR NEXT                             
*                                                                               
POST0020 EQU   *                                                                
         CLI   0(R4),0                                                          
         BE    POST0100                                                         
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST0100            TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROL                           
*                                                                               
         TM    FLAG6(R4),X'01'          ANY INVOICED $ IN BUCKET?               
         BZ    POST0030                 NO  - TAKE ORDERED $                    
         MVC   CONTOTS(4),CUASATIN(R6)  YES = INVOICED THIS YEAR                
         B     POST0040                                                         
*                                                                               
POST0030 EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)    NO  = ORDERED THIS YEAR                 
*                                                                               
POST0040 EQU   *                                                                
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD CURRENT BEST $                           
         LTR   R0,R0               TEST FOR NON-ZERO DOLLARS                    
         BZ    POST0050            ZERO DOLLARS IN BUCKET                       
         MVI   CONFLAG,C'Y'        SET CONTRACT FLAG TO YES                     
POST0050 EQU   *                                                                
         A     R0,0(R3)            TO ACCUMULATOR CURR BEST $                   
         ST    R0,0(R3)            STORE IT BACK                                
*                                                                               
         TM    FLAG6(R4),X'02'          ANY PRIOR TOTAL INVOICED $?             
         BZ    POST0060                 NO  - TAKE ORDERED $                    
         MVC   CONTOTS(4),PRTOTINV(R6)  YES = PRIOR TOTAL INVOICED              
         B     POST0070                                                         
*                                                                               
POST0060 EQU   *                                                                
         MVC   CONTOTS(4),PRTOTORD(R6)  NO  = ORDERED LAST YEAR                 
*                                                                               
POST0070 EQU   *                                                                
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD PRIOR FINAL $                            
         LTR   R0,R0               TEST FOR NON-ZERO DOLLARS                    
         BZ    POST0080            ZERO DOLLARS IN BUCKET                       
         MVI   CONFLAG,C'Y'        SET CONTRACT FLAG TO YES                     
POST0080 EQU   *                                                                
         A     R0,4(R3)            TO ACCUMULATOR CURR BEST $                   
         ST    R0,4(R3)            STORE IT BACK                                
*****>>  BAS   RE,TESTDISP         ***TEST***                                   
         LA    R3,8(R3)            BUMP A(TMONTBL ACCUM)                        
*                                                                               
         CR    R3,R1               END OF TABLE FOR WRAP-AROUND?                
         BNE   POST0090            NO                                           
         LA    R3,TMONTBL          RELOAD START OF TABLE                        
POST0090 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST0020            SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
POST0100 EQU   *                                                                
         MVC   POSTOLD,RCONKEY     SAVE PREVIOUS KEY                            
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
MEXIT    EQU   *                                                                
         XIT1                                                                   
BADEXIT  EQU   *                                                                
         LA    R0,1                SET CC NOT = ZERO                            
         B     MEXIT               RETURN                                       
*                                                                               
POSTOLD  DS    CL27                                                             
         EJECT                                                                  
TESTDISP NTR1                                                                   
         CLC   =C'TX',RCONKGRP     GROUP/SUBGROUP TEST                          
         BNE   TSTD0090            NO  - EXIT                                   
         LA    R2,POSTOLD                                                       
         MVC   P+1(12),=C'OLD CONTRACT'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON-RCONKEY(R2),P+16,4,=C'TOG'                  
         MVC   P+25(2),RCONKGRP-RCONKEY(R2)                                     
         MVC   P+32(12),=C'NEW CONTRACT'                                        
         GOTO1 HEXOUT,DMCB,RCONKCON,P+46,4,=C'TOG'                              
         L     R2,0(R3)            CURRENT YEAR                                 
         EDIT  (R2),(10,P+60),ZERO=NOBLANK                                      
         L     R2,4(R3)            PRIOR YEAR                                   
         EDIT  (R2),(10,P+74),ZERO=NOBLANK                                      
         GOTO1 REPORT                                                           
TSTD0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHEKSGRP: FOR EACH CONTRACT POSTED, IF A NEW SUBGROUP IS FOUND,             
*        CHECK TO SEE IF SUBGROUP IS REQUIRED.  IF SO,                          
*        GENERATE A SORT RECORD FOR ANY BREAK OTHER THAN THE FIRST              
*        THEN INSERT THE NEW INFORMATION INTO THE SORT KEY.                     
*                                                                               
CHEKSGRP NTR1                                                                   
         CLC   RCONKOFF,PRIOFF     RECORD OFFICE = LAST OFFICE?                 
         BNE   CHKS0005            NO  -                                        
         CLC   RCONKGRP+1(1),PRISGRUP                                           
*                                  RECORD SUBGRP = LAST SUBGRP?                 
         BE    CHKS0020            YES - NO BREAK                               
CHKS0005 EQU   *                                                                
         CLI   PRISGRUP,X'00'      LAST SUBGRP = INITIAL VALUE                  
         BE    CHKS0010            YES - NOTHING TO OUTPUT TO SORT              
         GOTO1 SUBDONE             NO  - GENERATE A SORT RECORD                 
*                                              IF NEEDED                        
CHKS0010 EQU   *                                                                
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   CHKS0015                                                         
         MVC   P+1(10),=C'CHEKSGRP  '                                           
         BAS   RE,DISPCODE                                                      
CHKS0015 EQU   *                                                                
         GOTO1 SUBINIT             SEE IF SUBGROUP CODE NEEDED                  
*                                  SUBINIT SETS UP FIELDS TO BE                 
*                                     LOADED ON EACH PASS                       
         MVC   PRISGRUP,RCONKGRP+1 LOAD NEW SUBGRP                              
         MVC   PRIOFF,RCONKOFF     LOAD NEW OFFICE                              
CHKS0020 EQU   *                                                                
         MVC   SSUBGRP,SAVSGRP     INSERT CODE                                  
         MVC   SSUBNAME,SAVSNAME   INSERT NAME                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DISPCODE: USED TO DEBUG THE FLOW OF DATA.                                   
*                                                                               
DISPCODE NTR1                                                                   
         MVC   P+12(33),=C'OFF=     TEAM=     GRP=     STA= '                   
         MVC   P+17(2),RCONKOFF                                                 
         MVC   P+27(2),RCONTEM                                                  
         MVC   P+36(2),RCONKGRP                                                 
         MVC   P+45(5),RCONKSTA                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+52,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*   SUBDONE:  LAST SUBGROUP MODE SETTING                                        
*                                                                               
SUBDONE  NTR1                                                                   
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   SUBD0000                                                         
         MVC   P+1(10),=C'SUBDONE   '                                           
         BAS   RE,DISPCODE                                                      
SUBD0000 EQU   *                                                                
****     MVC   P+5(104),SORTREC                                                 
****     GOTO1 REPORT                                                           
****     MVC   P+5(104),RESETREC                                                
****     GOTO1 REPORT                                                           
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    SDEXIT              YES                                          
         OC    SSUBGRP,SSUBGRP     ANY SUBGROUP IN RECORD?                      
         BZ    SDEXIT              NO  - NO OUTPUT AT THIS BREAK                
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
SDEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   TEAMDONE:  LAST TEAM MODE SETTING                                           
*                                                                               
TEMDONE  NTR1                                                                   
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   TEMD0000                                                         
         MVC   P+1(10),=C'TEMDONE   '                                           
         BAS   RE,DISPCODE                                                      
TEMD0000 EQU   *                                                                
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    TDEXIT              YES                                          
         OC    STEAM,STEAM         ANY TEAM IN RECORD?                          
         BZ    TDEXIT              NO  - NO OUTPUT AT THIS BREAK                
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
TDEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFDONE:  LAST OFFICE MODE SETTING                                          
*                                                                               
OFFDONE  NTR1                                                                   
         CLC   =C'DATAFLOW',QUESTOR **TEST**                                    
         BNE   OFFD0000                                                         
         MVC   P+1(10),=C'OFFDONE   '                                           
         BAS   RE,DISPCODE                                                      
OFFD0000 EQU   *                                                                
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    ODEXIT              YES                                          
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
ODEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1                                                                   
         OC    TMONTBL,TMONTBL     ANY DATA IN TABLE?                           
         BNZ   SRGE0005            YES - OUTPUT THE DATA                        
         XC    RESETREC,RESETREC   NO  - CLEAR TO FORCE NEXT O/P                
         B     SRGE0100            DON'T OUTPUT ANYTHING NOW                    
SRGE0005 EQU   *                                                                
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         LA    R2,TMONTBL          A(MONTHLY $$ TABLE)                          
         LA    R3,DATETABL         A(DATE TABLE FOR $$)                         
         LA    R5,12               LOOP CONTROL                                 
SRGE0010 EQU   *                                                                
         OC    0(2,R3),0(R3)       ANY DATE IN TABLE?                           
         BZ    SRGE0090            NO  - FINISHED                               
         MVC   SCURBLG,0(R2)       LOAD CURRENT BILLING $$                      
         MVC   SPRIFINL,4(R2)      LOAD PRIOR FINAL $$                          
         MVC   S$$DATE,0(R3)       LOAD MONTHLY $$ DATE                         
*                                                                               
*   PUT OUT DETAIL SORT RECORD                                                  
*                                                                               
         GOTO1 SORTOUT                                                          
         MVC   RESETREC,SORTREC    SAVE FOR RESETTING                           
*                                                                               
*   PUT OUT SUBGROUP LEVEL TOTAL RECORD, IF SUBGROUP REPORTED                   
*                                                                               
         CLI   SSUBGRP,0           ANY SUBGROUP IN RECORD?                      
         BE    SRGE0020            NO  - SKIP SUBGROUP TOTAL REC                
         MVC   STEMNAME,SPACES     CLEAR THE TEAM NAME (IF ANY)                 
         MVC   STEMNAME(14),=C'SUBGROUP TOTAL'                                  
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         GOTO1 SORTOUT                                                          
SRGE0020 EQU   *                                                                
*                                                                               
*   CHECK THE OFFICE SEQUENCE TABLE TO DETERMINE WHICH TOTAL                    
*     RECORD(S) ARE NEEDED.  THEN PUT THE PROPER ONES OUT,                      
*     ALONG WITH APPROPRIATE TOTAL HEADINGS.                                    
*                                                                               
         LA    R4,OFFSEQTB         A(OFFICE SEQUENCE TABLE)                     
SRGE0030 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    SRGE0050            YES                                          
         CLC   RCREPFL,0(R4)       SAME REP?                                    
         BNE   SRGE0040            NO                                           
         CLC   SOFFICE,2(R4)       SAME OFFICE?                                 
         BE    SRGE0050            YES                                          
SRGE0040 EQU   *                                                                
         LA    R4,OFFSEQLN(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     SRGE0030            GO BACK FOR NEXT                             
SRGE0050 EQU   *                                                                
         CLI   5(R4),C'Y'          OFFICE TOTALS NEEDED?                        
         BNE   SRGE0060            NO  - SKIP OFFICE TOTALS                     
*                                                                               
*   PUT OUT OFFICE LEVEL TOTAL RECORD                                           
*                                                                               
         MVI   SSUBGRP,X'FF'       SET SUBGROUP TO X'FF'                        
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         MVC   SSUBNAME,SPACES     SPACE-FILL FIELD                             
         MVC   SSUBNAME(12),=C'OFFICE TOTAL'                                    
         MVC   STEMNAME,SPACES     SPACE OUT TEAM NAME                          
         GOTO1 SORTOUT                                                          
SRGE0060 EQU   *                                                                
*                                                                               
*   PUT OUT SEQUENCE LEVEL TOTAL RECORD                                         
*                                                                               
         CLC   QOFFICE,SPACES      ANY OFFICE FILTER?                           
         BNE   SRGE0080            YES - NO SEQUENCE LEVEL TOTALS               
*                                        OR GRAND TOTALS NEEDED                 
         CLI   6(R4),C'Y'          SEQUENCE TOTALS NEEDED?                      
         BNE   SRGE0070            NO  - SKIP SEQUENCE TOTALS                   
         MVC   SOFFSEQ,7(R4)       SET SEQUENCE LEVEL TOTAL POSITION            
         MVC   SOFFICE,HIGHVALU    SET OFFICE TO HIGHVALU                       
         MVI   SSUBGRP,X'FF'       SET SUBGROUP TO X'FF'                        
         MVC   STEAM,HIGHVALU      SET TEAM TO HIGHVALU                         
         MVC   SOFFNAME(20),8(R4)  MOVE SEQUENCE BREAK TITLE IN                 
         MVC   SSUBNAME,SPACES     SPACE OUT SUBGROUP NAME                      
         MVC   STEMNAME,SPACES     SPACE OUT TEAM NAME                          
         GOTO1 SORTOUT                                                          
SRGE0070 EQU   *                                                                
*                                                                               
*   PUT OUT GRAND TOTAL RECORD                                                  
*                                                                               
         MVI   SOFFSEQ,X'FE'       SET SEQUENCE TO 'END'                        
         MVC   SOFFICE(5),HIGHVALU SET OFF/SUBGRP/TEAM TO X'FF'                 
         MVC   SSUBNAME,SPACES     SPACE OUT SUBGROUP NAME                      
         MVC   STEMNAME,SPACES     SPACE OUT TEAM     NAME                      
         MVC   SOFFNAME,SPACES     SPACE OUT OFFICE   NAME                      
         MVC   SOFFNAME(12),=C'REPORT TOTAL'                                    
         GOTO1 SORTOUT                                                          
SRGE0080 EQU   *                                                                
         MVC   SORTREC,RESETREC    RESTORE SORT RECORD AFTER                    
*                                     GENERATING 'TOTAL' RECORDS                
         LA    R2,8(R2)            BUMP MONTHLY $$ TABLE                        
         LA    R3,2(R3)            BUMP MONTHLY DATE TABLE                      
         BCT   R5,SRGE0010         GO BACK FOR NEXT                             
SRGE0090 EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESTORE CONTRACT KEY                         
         GOTO1 READ                REESTABLISH CONTRACT RECORD                  
SRGE0100 EQU   *                                                                
         XC    TMONTBL,TMONTBL     RESET $$ TABLE                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SORTOUT:  SINGLE FOCAL POINT FOR GENERATING SORT RECORDS, AND               
*        TESTING FOR DISPLAY OF SORT OUTPUT                                     
*                                                                               
SORTOUT  NTR1                                                                   
         CLC   QUESTOR(8),=C'**BUDG**'                                          
*                                  DISPLAY BUDGET REQUEST?                      
         BNE   SOUT0010            NO                                           
         CLI   SLOWKEY,1           YES - BUDGET RECORD?                         
         BE    SOUT0020            YES - DISPLAY IT                             
         B     SOUT0030            NO  - DON'T DISPLAY IT                       
SOUT0010 EQU   *                                                                
         CLC   QUESTOR(8),=C'DATAFLOW'                                          
         BE    SOUT0020                                                         
         CLC   QUESTOR(8),=C'**SOUT**'                                          
         BE    SOUT0020                                                         
         CLC   QUESTOR(8),=C'**TEST**'                                          
         BNE   SOUT0030                                                         
SOUT0020 EQU   *                                                                
         MVC   P+1(12),=C'OUT SORTREC:'                                         
         MVC   P+15(85),SORTREC                                                 
         GOTO1 REPORT                                                           
SOUT0030 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    SORTCTR,=P'1'       ADD TO SORTED RECORDS                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*             SORT FILE                                                         
*             COMPARE SORTED DATA KEYS VS BUDGET RECORDS ON FILE                
*             PRODUCE REPORT                                                    
*                                                                               
*                                                                               
         DS    0H                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         XC    RESETREC,RESETREC                                                
         XC    SORTREC2,SORTREC2                                                
REDA0100 EQU   *                                                                
         GOTO1 =A(GETSORT),DMCB,(RC)                                            
         CLI   SOFFSEQ,X'FF'       EOF?                                         
         BE    REDA0999            YES - FINISHED                               
         GOTO1 PRTDATA,DMCB,(RC)                                                
         B     REDA0100            GO BACK FOR NEXT RECORD                      
REDA0999 EQU   *                                                                
         LA    R2,ANLACCUM         DISPLAY FINAL ANNUAL FIGURES                 
         MVC   P+19(5),=C'TOTAL'   INSERT LINE HEADING                          
         BAS   RE,PRTDOLRS         PRINT FIGURES                                
         XC    ANLACCUM(16),ANLACCUM                                            
*                                  ZERO OUT ANNUAL ACCUMULATORS                 
         GOTO1 REPORT              INSERT A BLANK LINE                          
*****>>  MVI   FORCEHED,C'Y'       FORCE HEADING AFTER TOTALS                   
         GOTO1 SORTER,DMCB,=C'END'                                              
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PRTDATA:  FORMATS A PRINT LINE.  CONTROLS TOTALS.                           
*                                                                               
PRTDATA  NTR1                                                                   
         MVI   SKIPHEAD,C'N'       SET FLAG TO 'NO HEAD SKIPS'                  
         CLC   SORTREC(6),SORTREC2                                              
*                                  SAME SEQ/OFFICE/SUBGROUP/TEAM?               
         BE    PRDT0080            YES - NO NEW HEADINGS                        
*                                  NO  - BREAK TESTING                          
         CLI   QTRCOUNT,1          MORE THAN ONE QUARTERLY TOTAL SHOWN?         
         BNH   PRDT0010            NO                                           
         LA    R2,ANLACCUM         YES - DISPLAY ANNUAL FIGURES                 
         MVC   P+19(5),=C'TOTAL'   INSERT LINE HEADING                          
         BAS   RE,PRTDOLRS         PRINT FIGURES                                
         XC    ANLACCUM(16),ANLACCUM                                            
*                                  ZERO OUT ANNUAL ACCUMULATORS                 
         GOTO1 REPORT              INSERT A BLANK LINE                          
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
PRDT0010 EQU   *                                                                
         MVI   QTRCOUNT,0          RESET QUARTER COUNTER TO ZERO                
         MVI   SKIPHEAD,C'Y'       SET SKIP HEAD FLAG TO YES                    
         MVC   P+1(4),=C'***>'     'TOTAL' PROMPT                               
         CLI   SOFFICE,X'FF'       'TOTAL' AT THIS LEVEL?                       
         BE    PRDT0020            YES -                                        
         MVI   SKIPHEAD,C'N'       SET SKIP HEAD FLAG TO NO                     
         MVC   P+1(6),=C'OFFICE'   INSERT LINE TITLE                            
         MVC   P+17(2),SOFFICE     INSERT OFFICE                                
PRDT0020 EQU   *                                                                
         MVC   P+25(20),SOFFNAME   INSERT OFFICE NAME                           
         GOTO1 REPORT              PRINT OFFICE LINE                            
         CLI   SKIPHEAD,C'Y'       SKIP REMAINING HEADINGS?                     
         BE    PRDT0070            YES                                          
PRDT0030 EQU   *                                                                
         OC    SSUBGRP,SSUBGRP     ANY SUBGROUP?                                
         BZ    PRDT0050            NO                                           
         MVI   SKIPHEAD,C'Y'       SET SKIP HEAD FLAG TO YES                    
         MVC   P+3(4),=C'***>'     'TOTAL' PROMPT                               
         CLI   SSUBGRP,X'FF'       'TOTAL' AT THIS LEVEL?                       
         BE    PRDT0040            YES - SKIP 'SUBGROUP' TOTALS                 
         MVI   SKIPHEAD,C'N'       SET SKIP HEAD FLAG TO NO                     
         MVC   P+3(8),=C'SUBGROUP' INSERT LINE TITLE                            
         MVC   P+17(1),SSUBGRP     INSERT SUBGROUP                              
PRDT0040 EQU   *                                                                
         MVC   P+25(20),SSUBNAME   INSERT SUBGROUP NAME                         
         GOTO1 REPORT              PRINT SUBGROUP LINE                          
         CLI   SKIPHEAD,C'Y'       SKIP REMAINING HEADINGS?                     
         BE    PRDT0070            YES                                          
PRDT0050 EQU   *                                                                
         OC    STEAM,STEAM         ANY TEAM?                                    
         BZ    PRDT0070            NO                                           
         MVC   P+5(4),=C'***>'     'TOTAL' PROMPT                               
         CLI   STEAM,X'FF'         'TOTAL' AT THIS LEVEL?                       
         BE    PRDT0060            YES - SKIP 'TEAM' INFORMATION                
         MVC   P+5(4),=C'TEAM'     INSERT LINE TITLE                            
         MVC   P+17(2),STEAM       INSERT TEAM                                  
PRDT0060 EQU   *                                                                
         MVC   P+25(20),STEMNAME   INSERT TEAM NAME                             
         GOTO1 REPORT              PRINT WHATEVER HEADING IS UP                 
PRDT0070 EQU   *                                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
PRDT0080 EQU   *                                                                
         MVC   DUB(2),S$$DATE      UNLOAD DATE FOR CONVERSION                   
         MVI   DUB+2,1             INSERT DAY OF 1                              
         GOTO1 DATCON,DMCB,(3,DUB),(6,P+20)                                     
         LA    R2,SCURBLG          PRINT FIGURES FROM SORT CARD                 
         BAS   RE,PRTDOLRS         PRINT FIGURES                                
*                                                                               
         LA    R1,SCURBLG          ACCUMULATE VARIOUS TOTALS                    
         LA    R2,QTRACCUM         QUARTERLY                                    
         BAS   RE,TOTALIT                                                       
         LA    R2,ANLACCUM         ANNUAL                                       
         BAS   RE,TOTALIT                                                       
         LA    R2,REGACCUM         REGIONAL                                     
         BAS   RE,TOTALIT                                                       
         LA    R2,TOTACCUM         GRAND TOTAL                                  
         BAS   RE,TOTALIT                                                       
*                                                                               
         MVC   QTRLINE,=C'1ST QTR' INSERT LINE HEADING                          
         CLI   S$$DATE+1,3         CHECK DATE FOR QUARTERLY TOTALS              
         BE    PRDT0090                                                         
         MVC   QTRLINE,=C'2ND QTR' INSERT LINE HEADING                          
         CLI   S$$DATE+1,6                                                      
         BE    PRDT0090                                                         
         MVC   QTRLINE,=C'3RD QTR' INSERT LINE HEADING                          
         CLI   S$$DATE+1,9                                                      
         BE    PRDT0090                                                         
         MVC   QTRLINE,=C'4TH QTR' INSERT LINE HEADING                          
         CLI   S$$DATE+1,12                                                     
         BNE   PRDT0100                                                         
PRDT0090 EQU   *                                                                
         ZIC   RF,QTRCOUNT         INCREMENT QUARTER COUNTER                    
         LA    RF,1(RF)                                                         
         STC   RF,QTRCOUNT         STORE IT BACK                                
         LA    R2,QTRACCUM         PRINT QUARTERLY TOTALS                       
         MVC   P+19(7),QTRLINE     INSERT LINE HEADING                          
         BAS   RE,PRTDOLRS         PRINT FIGURES                                
         XC    QTRACCUM(16),QTRACCUM                                            
*                                  ZERO OUT QUARTERLY ACCUMULATORS              
         GOTO1 REPORT              INSERT A BLANK LINE                          
*                                                                               
PRDT0100 EQU   *                                                                
         MVC   SORTREC2,SORTREC    SAVE SORTREC                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PRTDOLRS:  FORMATS THE PRINT LINE REPRESENTED BY THE FIGURES                
*        REFERENCED BY R2.                                                      
*                                                                               
PRTDOLRS NTR1                                                                   
*                                                                               
         MVC   FULL,0(R2)          CURRENT BILLING                              
         EDIT  FULL,(11,P+33),COMMAS=YES                                        
         MVC   FULL,4(R2)          CURRENT FORECAST                             
         EDIT  FULL,(11,P+46),COMMAS=YES                                        
         MVC   FULL,8(R2)          CURRENT BUDGET                               
         EDIT  FULL,(11,P+72),COMMAS=YES                                        
         MVC   FULL,12(R2)         PRIOR FINAL                                  
         EDIT  FULL,(11,P+98),COMMAS=YES                                        
         SR    R4,R4                                                            
         MVC   CURRBILL,0(R2)      CURRENT BILLING                              
         MVC   DIVISOR,4(R2)       CURRENT FORECAST                             
         BAS   RE,SIZECHEK         CHECK SIZE OF FIGURES                        
         L     R5,CURRBILL                                                      
         M     R4,=F'100'          MULT CURR BILLING BY 100                     
         SLDL  R4,1                DOUBLE THE RESULT                            
         L     R6,DIVISOR                                                       
         AR    R5,R6               ADD FOR ROUNDING                             
         LTR   R6,R6               ANY DIVISOR?                                 
         BZ    PRTD0020            YES                                          
PRTD0010 EQU   *                                                                
         DR    R4,R6               CALCULATE PERCENT (DOUBLED)                  
         SRL   R5,1                DIVIDE QUOTIENT BY 2                         
         EDIT  (R5),(11,P+59),COMMAS=YES                                        
PRTD0020 EQU   *                                                                
         SR    R4,R4                                                            
         MVC   CURRBILL,0(R2)      CURRENT BILLING                              
         MVC   DIVISOR,8(R2)       CURRENT BUDGET                               
         BAS   RE,SIZECHEK         CHECK SIZE OF FIGURES                        
         L     R5,CURRBILL                                                      
         M     R4,=F'100'          MULT CURR BILLING BY 100                     
         SLDL  R4,1                DOUBLE THE RESULT                            
         L     R6,DIVISOR                                                       
         AR    R5,R6               ADD FOR ROUNDING                             
         LTR   R6,R6               ANY DIVISOR?                                 
         BZ    PRTD0040            YES                                          
PRTD0030 EQU   *                                                                
         DR    R4,R6               CALCULATE PERCENT (DOUBLED)                  
         SRL   R5,1                DIVIDE QUOTIENT BY 2                         
         EDIT  (R5),(11,P+85),COMMAS=YES                                        
PRTD0040 EQU   *                                                                
         SR    R4,R4                                                            
         MVC   CURRBILL,0(R2)      CURRENT BILLING                              
         MVC   DIVISOR,12(R2)      PRIOR FINAL                                  
         BAS   RE,SIZECHEK         CHECK SIZE OF FIGURES                        
         L     R5,CURRBILL                                                      
         M     R4,=F'100'          MULT CURR BILLING BY 100                     
         SLDL  R4,1                DOUBLE THE RESULT                            
         L     R6,DIVISOR                                                       
         AR    R5,R6               ADD FOR ROUNDING                             
         LTR   R6,R6               ANY DIVISOR?                                 
         BZ    PRTD0060            YES                                          
PRTD0050 EQU   *                                                                
         DR    R4,R6               CALCULATE PERCENT (DOUBLED)                  
         SRL   R5,1                DIVIDE QUOTIENT BY 2                         
         EDIT  (R5),(11,P+111),COMMAS=YES                                       
PRTD0060 EQU   *                                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SIZECHEK:  IF CURRENT BILLING EXCEEDS $10,000,000 (TEN MILLION),            
*        BOTH CURRBILL AND DIVISOR ARE DIVIDED BY 2 UNTIL CURRBILL              
*        IS LESS THAN $10M.  THIS WILL ENSURE THAT CALCULATIONS WILL            
*        NOT EXCEED A TWO-WORD LIMIT, NOR WILL THEY GO NEGATIVE.  AT            
*        THIS SIZE, THERE WILL BE NO SIGNIFICANT LOSS IN PRECISION, AS          
*        REPORTING IS TO A WHOLE PERCENTAGE POINT.                              
*        CURRBILL      =  VALUE OF CURRENT BILLING                              
*        DIVISOR       =  VALUE OF FORECAST OR BUDGET OR PRIOR FINAL            
*                                                                               
SIZECHEK NTR1                                                                   
         L     R5,=F'10000000'     $10,000,000 LIMIT                            
         L     R1,CURRBILL         LOAD CURRENT BILLING                         
         L     R2,DIVISOR          LOAD DIVISOR                                 
SICH0020 EQU   *                                                                
         CR    R1,R5               CURRBILL > $10MIL?                           
         BNH   SICH0040            NO  - FINISHED                               
         SRL   R1,1                YES - DIVIDE CURR BILL BY 2                  
         SRL   R2,1                DIVIDE DIVISOR BY 2 ALSO                     
         B     SICH0020            GO BACK AND CHECK AGAIN                      
SICH0040 EQU   *                                                                
         ST    R1,CURRBILL         RELOAD CURRENT BILLING                       
         ST    R2,DIVISOR          RELOAD DIVISOR                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TOTALIT:  ACCUMULATES THE VALUES PASSED IN FROM THE SORT CARDS              
*       R1  =  A(1ST SORT CARD $$ FIELD)                                        
*       R2  =  A(1ST ACCUMULATOR FIELD )                                        
*                                                                               
TOTALIT  NTR1                                                                   
         LA    RF,4                LOOP CONTROL                                 
TOTL0010 EQU   *                                                                
         MVC   FULL,0(R1)          UNLOAD CARD FIELD                            
         L     RE,FULL             LOAD TO REGISTER                             
         A     RE,0(R2)            ADD VALUE IN ACCUMULATOR                     
         ST    RE,0(R2)            STORE TOTAL BACK                             
         LA    R1,4(R1)            BUMP SORT CARD $$ FIELD                      
         LA    R2,4(R2)            BUMP ACCUMULATOR FIELD                       
         BCT   RF,TOTL0010         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
READ     MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY                                                          
*                                                                               
ADD      MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY                                                          
*                                                                               
WRT      MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         DS    0H                                                               
*                                                                               
*  FILLS OUT THE 24-MONTH FISCAL YEAR SETUP IN THE BUDGET RECORD                
*                                                                               
*                                                                               
SHIFTMO  EQU   *                                                                
         ZIC   R4,STARTMO          SET UP BCT                                   
         SH    R4,=H'13'                                                        
         LCR   R4,R4                                                            
         SPACE 1                                                                
SHIFT5   MVC   0(4,R6),0(R5)                                                    
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,SHIFT5                                                        
         BR    RE                                                               
         EJECT                                                                  
LINKFILE NMOD1 0,**LINK**                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         L     RF,4(R1)            RELOAD A(RECORD TO BE LOADED)                
         ST    RF,AIOAREA                                                       
         L     RF,8(R1)            LOAD A(COMMAND TO EXECUTE)                   
         MVC   COMMAND(8),0(RF)    LOAD COMMAND                                 
         BAS   RE,FILELINK                                                      
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     LINKEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    LINKEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
LINKEXIT EQU   *                                                                
         XIT1                                                                   
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
*                                                                               
FILELINK NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING - RETURNED RECORDS HAVE                 
*      TO BE 'COLLAPSED' BY KEY INTO A SINGLE RECORD.  'GETSORT'                
*      WILL ACCUMULATE RECORDS FOR A SINGLE KEY UNTIL A BREAK IS                
*      REACHED.  THEN THE 'BREAK' ACCUMULATED SORT RECORD IS                    
*      RELEASED TO THE PROCESS.                                                 
*                                                                               
         DS    0H                                                               
GETSORT  NMOD1 0,*GETSORT*                                                      
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         MVC   SOFFSEQ,EOSORT                                                   
         CLI   EOSORT,X'FF'        EOF REACHED?                                 
         BE    GETS0060            YES                                          
GETS0010 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6               TEST RETURN ZERO=EOF                         
         BNZ   GETS0020            NOT EOF                                      
         MVI   EOSORT,X'FF'        SET 'END OF SORT' FLAG TO YES                
         B     GETS0050            PROCESS FINAL RECORD                         
GETS0020 EQU   *                                                                
         CLC   QUESTOR(8),=C'**SBACK*'                                          
         BE    GETS0021                                                         
         CLC   QUESTOR(8),=C'**TEST**'                                          
         BNE   GETS0030                                                         
GETS0021 EQU   *                                                                
         MVC   P+1(12),=C'RET SORTREC:'                                         
         MVC   P+15(85),0(R6)                                                   
         GOTO1 REPORT                                                           
GETS0030 EQU   *                                                                
         OC    RESETREC,RESETREC   IS THIS THE FIRST RECORD?                    
         BNZ   GETS0040            NO  - COMPARE KEYS                           
         MVC   RESETREC,0(R6)      YES - LOAD THE RETURNED RECORD               
         B     GETS0010            GO BACK FOR NEXT RECORD                      
GETS0040 EQU   *                                                                
*                                                                               
*   NOTE:  BILLING DOLLARS APPEAR IN ALL RECORDS WHICH HAVE A                   
*        VALUE OF ZERO IN SLOWKEY.  THEY ARE ACCUMULATED TO GIVE                
*        THE TOTAL DOLLARS FOR THE OFFICE.                                      
*                                                                               
         CLC   RESETREC+DOFFICE(7),DOFFICE(R6)                                  
*                                  SAME OFFICE/SUBGROUP/TEAM/DATE?              
         BNE   GETS0050            RETURN THE TOTAL RECORD                      
         MVC   FULL,RESETREC+DCURBLG                                            
*                                  ACCUMULATE CURRENT BILLING                   
         L     RE,FULL                                                          
         MVC   FULL,DCURBLG(R6)                                                 
         A     RE,FULL                                                          
         ST    RE,FULL             STORE IT BACK                                
         MVC   RESETREC+DCURBLG(4),FULL                                         
*                                                                               
         MVC   FULL,RESETREC+DPRIFINL                                           
*                                  ACCUMULATE PRIOR FINAL                       
         L     RE,FULL                                                          
         MVC   FULL,DPRIFINL(R6)                                                
         A     RE,FULL                                                          
         ST    RE,FULL             STORE IT BACK                                
         MVC   RESETREC+DPRIFINL(4),FULL                                        
*                                                                               
*                                                                               
*   NOTE:  BUDGET FIGURES ARE ONLY PRESENT IN RECORDS WHICH                     
*        HAVE SLOWKEY VALUE OF 1.  AS THERE IS ONLY ONE BUDGET                  
*        RECORD PER PERIOD PER KEY, ADDING IT HAS THE EFFECT OF                 
*        INSERTING THE SINGLE BUDGET INTO THE TOTAL OF CONTRACT                 
*        DOLLARS ROLLED INTO THE FINAL AMOUNT.                                  
*                                                                               
         MVC   FULL,RESETREC+DCURFORE                                           
*                                  ACCUMULATE CURRENT FORECAST                  
         L     RE,FULL                                                          
         MVC   FULL,DCURFORE(R6)                                                
         A     RE,FULL                                                          
         ST    RE,FULL             STORE IT BACK                                
         MVC   RESETREC+DCURFORE(4),FULL                                        
*                                                                               
         MVC   FULL,RESETREC+DCURBUDG                                           
*                                  ACCUMULATE CURRENT BUDGET                    
         L     RE,FULL                                                          
         MVC   FULL,DCURBUDG(R6)                                                
         A     RE,FULL                                                          
         ST    RE,FULL             STORE IT BACK                                
         MVC   RESETREC+DCURBUDG(4),FULL                                        
         B     GETS0010            GO BACK FOR NEXT RECORD                      
*                                                                               
GETS0050 EQU   *                                                                
         MVC   SORTREC,RESETREC    LOAD TOTAL RECORD                            
         MVC   RESETREC,0(R6)      LOAD NEW RECORD TO COMPARE                   
         CLC   QUESTOR(8),=C'**SCOMP*'                                          
         BE    GETS0051                                                         
         CLC   QUESTOR(8),=C'**TEST**'                                          
         BNE   GETS0060                                                         
GETS0051 EQU   *                                                                
         MVC   P+1(12),=C'IN  SORTREC:'                                         
         MVC   P+15(85),SORTREC                                                 
         GOTO1 REPORT                                                           
GETS0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DS    0H                                                               
ALTHDR   NMOD1 0,*ALTHDR*                                                       
         LA    R1,SAVEREGS-ALTHDR  RESTORE REGISTERS                            
         AR    R1,RB                                                            
         LM    R2,RA,0(R1)         LOAD R2 -> RA                                
         L     RC,40(R1)           LOAD RC:  RB IS SET BY NMOD                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CARDMISS DC    C'SECOND REQUEST CARD MISSING'                                   
*                                                                               
*              WORK SPACE ETC                                                   
HIGHVALU DS    XL20                                                             
         SPACE 3                                                                
YEARONE  DS    CL2                 FIRST  YEAR OF OFFICE BUDGETS                
YEARTWO  DS    CL2                 SECOND YEAR OF OFFICE BUDGETS                
YEARFF   DC    XL2'FFFF'           DELIMITER                                    
CONVRTYR DC    CL6'000015'         DATE CONVERSION SPACE                        
DATETABL DS    CL24                                                             
PRISGRUP DS    CL1                 PRIOR SUBGROUP                               
PRIOFF   DS    CL2                 PRIOR OFFICE                                 
SAVSGRP  DS    CL1                 STORED GROUP CODE                            
SAVSNAME DS    CL20                STORED GROUP NAME                            
EOSORT   DC    XL1'00'             END OF SORT FLAG                             
         SPACE 3                                                                
CURRBILL DS    F                   CURRENT BILLING $$                           
DIVISOR  DS    F                   DIVISOR FOR PERCENT CALCULATIONS             
ASTART   DS    F                                                                
STARTMO  DS    XL1                 FISCAL YEAR START MONTH                      
PRIORKEY DS    CL6                 OFFICE/TEAM/SUBGROUP OF LAST KEY             
*                                                                               
SORTREC  DS    0CL104                                                           
SOFFSEQ  DS    CL1     +0          OFFICE SEQUENCE NUMBER                       
*                                     SEE TABLE 'OFFICE SEQUENCE'               
SOFFICE  DS    CL2     +1          OFFICE CODE                                  
SSUBGRP  DS    CL1     +3          SUBGROUP: NY ONLY                            
*                                              ELSE BINARY ZERO                 
STEAM    DS    CL2     +4          TEAM: NY, CH, LA ONLY                        
*                                              ELSE BINARY ZERO                 
S$$DATE  DS    CL2     +6          BUCKET MONTH:  YYMM BINARY                   
SLOWKEY  DS    CL2     +8          LOW-ORDER KEY POSITION                       
SCURBLG  DS    CL4     +10         CURR BILLING $$                              
SCURFORE DS    CL4     +14         CURR FORECAST BUDGET $$                      
SCURBUDG DS    CL4     +18         CURR REGULAR BUDGET $$                       
SPRIFINL DS    CL4     +22         PRIOR FINAL $$                               
SOFFNAME DS    CL20    +26         OFFICE NAME                                  
SSUBNAME DS    CL20    +46         SUBGROUP NAME                                
STEMNAME DS    CL20    +66         TEAM   NAME                                  
         DS    CL18    +86         SPARE                                        
*                                                                               
*    RECORD DISPLACEMENTS:                                                      
DOFFICE  EQU   SOFFICE-SORTREC                                                  
DSUBGRP  EQU   SSUBGRP-SORTREC                                                  
DTEAM    EQU   STEAM-SORTREC                                                    
D$$DATE  EQU   S$$DATE-SORTREC                                                  
DCURBLG  EQU   SCURBLG-SORTREC                                                  
DCURFORE EQU   SCURFORE-SORTREC                                                 
DCURBUDG EQU   SCURBUDG-SORTREC                                                 
DPRIFINL EQU   SPRIFINL-SORTREC                                                 
DOFFNAME EQU   SOFFNAME-SORTREC                                                 
DSUBNAME EQU   SSUBNAME-SORTREC                                                 
DTEMNAME EQU   STEMNAME-SORTREC                                                 
*                                                                               
SORTREC2 DS    0CL104              SAVED SORTKEY                                
SOFFSEQ2 DS    CL1     +0          OFFICE SEQUENCE NUMBER                       
*                                     SEE TABLE 'OFFICE SEQUENCE'               
SOFFICE2 DS    CL2     +1          OFFICE CODE                                  
SSUBGRP2 DS    CL1     +3          SUBGROUP: NY ONLY                            
*                                              ELSE BINARY ZERO                 
STEAM2   DS    CL2     +4          TEAM: NY, CH, LA ONLY                        
*                                              ELSE BINARY ZERO                 
S$$DATE2 DS    CL2     +6          BUCKET MONTH:  YYMM BINARY                   
SLOWKEY2 DS    CL2     +8                                                       
SCURBLG2 DS    CL4     +10         CURR BILLING $$                              
SCURFOR2 DS    CL4     +14         CURR FORECAST BUDGET $$                      
SCURBUD2 DS    CL4     +18         CURR REGULAR BUDGET $$                       
SPRIFIN2 DS    CL4     +22         PRIOR FINAL $$                               
SOFFNAM2 DS    CL20    +26         OFFICE NAME                                  
SSUBNAM2 DS    CL20    +46         SUBGROUP NAME                                
STEMNAM2 DS    CL20    +66         TEAM   NAME                                  
         DS    CL18    +86         SPARE                                        
*                                                                               
*                                                                               
RESETREC DS    CL104                                                            
*                                                                               
QTRACCUM DS    4F                  QUARTERLY ACCUMULATORS                       
ANLACCUM DS    4F                  ANNUAL ACCUMULATORS                          
REGACCUM DS    4F                  REGIONAL ACCUMULATOR                         
TOTACCUM DS    4F                  TOTAL ACCUMULATOR                            
*                                                                               
SAVEMKT  DS    CL20                SAVE MARKET NAME                             
SAVEOFF  DS    CL20                SAVE OFFICE NAME                             
*                                                                               
PROCCTR  DC    PL4'0'              CONTRACTS PROCESSED CTR                      
SORTCTR  DC    PL4'0'              RECORDS RELEASED TO SORT                     
RETCTR   DC    PL4'0'              RECORDS RETURNED FROM SORT                   
BUDCTR   DC    PL4'0'              BUDGET SORT RECS OUTPUT                      
QTRCOUNT DS    XL1                 QUARTER COUNTER                              
QTRLINE  DS    CL7                 QUARTER LINE HEADING                         
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
SKIPHEAD DS    CL1                 FLAG TO SKIP HEADINGS                        
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
ACTIVE   DS    CL1                                                              
ELCODE   DS    CL1                                                              
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=104'                                   
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
*                                                                               
         DC    C'**CARD2**'                                                     
CARD2    DS    0CL80               2ND REQUEST CARD                             
CBUDFRM  DS    CL6                 'BUDGET-FROM' DATE                           
CBUDTO   DS    CL6                 'BUDGET-TO' DATE                             
CBUDCTYP DS    5CL4                CONTRACT TYPE/PERCENTAGE                     
         DS    CL48                                                             
CARD2L   EQU   *-CARD2                                                          
*                                                                               
         DS    0F                                                               
*                                                                               
TMONTBL  DS    CL96                12 SETS OF 2 BUCKETS: PER MONTH:             
*                                  BUCKET 1  =  CURRENT BILLING                 
*                                  BUCKET 2  =  PRIOR FINAL                     
CONTOTS  DS    F                                                                
CONFLAG  DS    CL1                                                              
*                                                                               
         SPACE 3                                                                
MONCON   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
MCEND    DS    0H                                                               
SAVEREGS DS    11F                                                              
ROBDRECS DS    0CL600              OFFICE BUDGET RECORDS IO AREA                
ROBDREC1 DS    300C                                                             
ROBDREC2 DS    300C                                                             
*                                                                               
       ++INCLUDE REGENOFF2                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
*                                                                               
*   RECORD LAYOUT FOR OFFICE BUDGET                                             
       ++INCLUDE REGENOBUD                                                      
*                                                                               
*                                                                               
*   WORK SPACE DSECT                                                            
*                                                                               
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'215REREP1G02 05/01/02'                                      
         END                                                                    
