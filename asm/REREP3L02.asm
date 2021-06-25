*          DATA SET REREP3L02  AT LEVEL 022 AS OF 05/01/02                      
*          DATA SET REREP3K02  AT LEVEL 011 AS OF 09/22/95                      
*          DATA SET REREP3G02  AT LEVEL 104 AS OF 02/04/95                      
*PHASE RE3L02C,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE QSORT                                                                  
* KLAGENCY ??                                                                   
* KLADVERT ??                                                                   
         TITLE 'RE3L02 - REREP3L02 - KATZ 1640 INTERFACE TAPE'                  
**********************************************************************          
* UNDER MVS, AN UNLABELLED TAPE IS CREATED BY MEANS OF LABEL=(,NL)   *          
* PARAMETER ON THE APPROPRIATE DD STATEMENT FOR THE OUTPUT TAPE.     *          
**********************************************************************          
         SPACE 2                                                                
*********************************************************************           
*                                                                   *           
*   REREP3L02 - RE3L02 - KATZ 1640 TAPE                             *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*   QOPTION1 = 'M'ONTHLY OR 'W'EEKLY                                *           
*   QOPTION2 = 'E'ESTIMATE/ORDERED RECORDS ONLY ON THE WEEKLY TAPE  *           
*                                                                   *           
*                --- ADD PRINTOUT OF TAPE OUTPUT IF REQUESTOR =     *           
*                    'SPEC PRINT'                                   *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB04/95 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* JAN05/96 (BG ) --- ADD TERRITORY FROM AGENCY RECORD               *           
*                    ADD D TO D CONTYP TABLE                        *           
* JAN12/96 (BG ) 005 CT  TERRITORIES ENTERED AND CT OF AGY 1A RECS  *           
*                                                                   *           
* JAN16/96 (BG ) 006 CT  ADD PRNTBL, FIX 1 MONTH BUG                *           
*                                                                   *           
* JAN23/96 (BG ) 008 CT  ADD S3 - M SENTRY FOR CONVERSION           *           
*                                                                   *           
* JAN24/96 (BG ) 009 CT  BYPASS S3 CONTRACTS                        *           
*                                                                   *           
* JAN29/96 (BG ) 010 CT  ADD DV DENVER OFF PROPERLY                 *           
*                                                                   *           
* FEB05/96 (BG ) 011 CT  ALLOW  S3 CONTRACTS                        *           
*                                                                   *           
* FEB20/96 (BG ) 012 ADD UP SALES TMI/TM1                           *           
*                                                                   *           
* MAR08/96 (BG ) 013 BACK OUT ABOUT TMI/TM1                         *           
*                                                                   *           
* APR28/96 (BG ) 01? FIX AGY OFF COMPARE                            *           
*                                                                   *           
* MAY15/96 (BG ) 016 FIX MOVE SPACES TO WORK                        *           
*                                                                   *           
* MAY21/97 (BG ) 017 ADD AMCAST (ABCNY-RS) AND MIAMI - 046          *           
*                                                                   *           
* JAN07/98 (BG ) 018 RESTORE BANNY                                  *           
*                                                                   *           
* JAN26/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE3L02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3L02,R9,RR=R2                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R2,RELO                                                          
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         ST    RB,SAVEDRB                                                       
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
         GOTO1 (RF)                GO TO THE ROUTINE                            
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         GOTO1 REPORT                                                           
         MVC   P(24),=C'>>> PROCESSING ERROR <<<'                               
         GOTO1 REPORT                                                           
         B     MAINBAD                                                          
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
         B     TESTEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
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
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(RUNFRST),AL3(TAPINIT)    RUN FIRST - OPEN TAPE               
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(REQDONE)    END OF REQUEST                      
         DC    AL1(RUNLAST),AL3(RUNDONE)    RUN LAST - WRITE AGY,               
*                                            ADV, SAL, CLOSE TAPE               
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*        TAPINIT --- PROCESS TO OPEN THE TAPE OUTPUT FILE                       
*                                                                               
TAPINIT  NTR1                                                                   
         OPEN  (INTFILE,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD TAPE OPEN, CHECK JCL                     
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
*        STAINIT --- PROCESS FIRST ENCOUNTER OF A STATION                       
*                                                                               
STAINIT  NTR1                                                                   
*                                                                               
         BAS   RE,BLDSTA                                                        
*                                                                               
         XC    KEY,KEY         READ STATION RECORD FOR INTERFACE CODE           
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),RCREPFL                                                
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 HIGH                                                             
         CLI   KEY,X'02'                                                        
         BNE   SINT20                                                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SINT20                                                           
         BAS   RE,GETSTAT                                                       
*                                                                               
         MVC   STARANK(1),RSTARANK                                              
         MVC   STAMKT(20),RSTAMKT                                               
*                                                                               
         LA    R1,RSTAELEM                                                      
         SR    R0,R0                                                            
SINT10   ICM   R0,1,1(R1)                                                       
         BZ    SINT20                                                           
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    SINT20                                                           
         CLI   0(R1),8                                                          
         BNE   SINT10                                                           
         MVC   STAIFACE,RSTAOSI-RSTAXXEL(R1)   INTERFACE CODE                   
SINT20   EQU   *                                                                
         SPACE                                                                  
SINTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        POST --- PROCESS THE CURRENT CONTRACT                                  
*                                                                               
POST     NTR1                                                                   
*                                                                               
*        CLC   RCONKREP,=C'BF'     BYPASS ALL BANNER RECS                       
*        BE    POSTGOOD                                                         
*                                                                               
         CLC   RCONKREP,=C'S3'     BYPASS ALL SENTRY RECS                       
         NOP   POSTGOOD                                                         
*                                                                               
         SPACE                                                                  
         L     R1,CONCNT                                                        
         LA    R1,1(,R1)                                                        
         ST    R1,CONCNT          CONTRACT COUNTER                              
         XC    SVSALEP1,SVSALEP1                                                
*                                                                               
         CLC   RCONKREP,SVDREP                                                  
         BE    POST010                                                          
         OC    REPCONCT,REPCONCT                                                
         BZ    POST010                                                          
         MVC   P+5(2),SVDREP                                                    
         EDIT  REPCONCT,(7,P+9),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         XC    REPCONCT,REPCONCT                                                
*                                                                               
POST010  MVC   SVDREP,RCONKREP                                                  
         L     R1,REPCONCT                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,REPCONCT        CONTRACT COUNTER                              
*                                                                               
         L     R1,TALLYCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TALLYCT         PROGRESS COUNT                                
         CH    R1,=H'5000'        PROGRESS COUNT                                
         BL    POST020                                                          
         MVC   P+1(2),RCONKREP                                                  
         MVC   P+5(22),=CL22'CONTRACTS PROCESSED ='                             
         EDIT  CONCNT,(7,P+29),ZERO=NOBLANK                                     
         MVC   P+45(14),=CL14'RECS TO TAPE ='                                   
         EDIT  PUTCNT,(7,P+60),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         XC    TALLYCT,TALLYCT                                                  
*                                                                               
POST020  XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+12)                           
         GOTO1 GETBROAD,(R1),WORK+12,WORK,GETDAY,ADDAY                          
         GOTO1 DATCON,(R1),(0,WORK+6),(3,CONEND)                                
         MVC   CONENDC(6),WORK+6                                                
         XC    WORK,WORK                                                        
*                                                                               
         LA    R4,WRK                                                           
         LA    R0,WRK                                                           
         LA    R1,600                                                           
         SR    RE,RE                                                            
         L     RF,=XL4'40000000'   PAD WITH BLANKS                              
         MVCL  R0,RE                                                            
*        MVC   0(125,R4),SPACES          MOVE IN SPACES                         
*        MVC   125(125,R4),SPACES                                               
*        MVC   250(250,R4),0(R4)                                                
*        MVC   500(100,R4),0(R4)                                                
         USING KLTPREC,R4                                                       
         LA    R0,60                                                            
         LA    R1,KLMONCTR                                                      
         MVC   0(4,R1),=X'0000000F'                                             
         LA    R1,4(,R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         MVC   DUB(4),RCONKCON                                                  
         MVI   DUB+4,X'0F'                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   KLCON#,WORK+1       CONTRACT NUMBER                              
*                                                                               
* TADVC & TAGYC NOT IN TAPE REC, BUT NEEDED TO READ OTHER FIELDS                
*                                                                               
         MVC   SVSALESP(L'RCONSAL),RCONSAL                                      
         MVC   TADVC,RCONKADV               ADVERTISER CODE                     
         MVC   TAGYC,RCONKAGY               AGENCY CODE                         
*                                                                               
         TM    RCONMODR+1,X'10'    THIS A CONVERTED CONTRACT                    
         BZ    POST024                                                          
         CLC   KLCON#,=C'1600000'  MUST BE GREATER THAN 1600000                 
         BNH   POST024                                                          
         MVI   KLCNVT,C'Y'                                                      
         PACK  DUB,KLCON#                                                       
         SP    DUB,=P'1600000'                                                  
         OI    DUB+7,X'0F'                                                      
         UNPK  KLCON#,DUB                                                       
*                                                                               
* READSUB FILLS IN TADVNM, KLAGYNAM, KLPRDNAM (1ST 20 = KLPRDNAM)               
*                                                                               
POST024  BAS   RE,READSUB         READ REST OF INFO FOR PUT REC                 
*                                                                               
         MVC   KLMKTNAM,STAMKT    AND MARKET NAME                               
*        MVC   KLMKTNUM,???       MARKET NUMBER ????                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
         MVC   KLSEDTS(4),WORK+2                                                
         MVC   KLSEDTS+4(2),WORK                                                
         MVI   KLSEDTS+6,C'-'                                                   
         GOTO1 (RF),(R1),(3,RCONDATE+3),(0,WORK)                                
         MVC   KLSEDTS+7(4),WORK+2                                              
         MVC   KLSEDTS+11(2),WORK                                               
*        GOTO1 (RF),(R1),(3,RCONHDRD),(10,KLENTDT)                              
         GOTO1 (RF),(R1),(3,RCONHDRD),(10,KLORGDT)                              
         SPACE                                                                  
         MVI   KLCAT,C' '                                                       
         LA    R0,CONTYPTL                                                      
         L     R1,=A(CONTYPTB)                                                  
POST030  CLC   RCONTYPE,1(R1)                                                   
         BE    POST034                                                          
         LA    R1,2(,R1)                                                        
         BCT   R0,POST030                                                       
         B     POST036                                                          
POST034  MVC   KLCAT,0(R1)                                                      
         SPACE                                                                  
POST036  DS    0H                                                               
         ZIC   R0,RCONWKS                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KLWKS,DUB                                                        
         MVC   KLSTATN,RCONKSTA                                                 
         CLI   RCONKSTA+4,C'A'     THIS AM                                      
         BNE   POST040                                                          
         MVI   KLAF,C'1'                                                        
         B     POST050                                                          
POST040  CLI   RCONKSTA+4,C'F'     THIS FM                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KLAF,C'2'                                                        
*                                                                               
POST050  DS   0H                                                                
         LA    R0,OFFNTABN                                                      
         LA    R1,OFFNTAB                                                       
POST054  CLC   RCONKOFF,0(R1)                                                   
         BE    POST055                                                          
         LA    R1,L'OFFNTAB(,R1)                                                
         BCT   R0,POST054                                                       
         DC    H'0'                                                             
         SPACE                                                                  
POST055  MVC   KLOFFICE,2(R1)                                                   
         MVC   KLOFFNAM,ROFFNAME   MOVE IN OFFICE NAME                          
*                                                                               
* CK IF COMBO                                                                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   BYTE,X'17'                                                       
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   KLCOMBOS,C'Y'                                                    
*                                                                               
* CK IF NEW BUSINESS                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   BYTE,X'18'                                                       
         BAS   RE,GETEL                                                         
         BNE   POST058                                                          
         CLC   RCONDVCT-RCONDVEL(2,R6),=C'NB'                                   
         BNE   POST056                                                          
         MVI   KLNEWBUS,C'Y'                                                    
*                                                                               
* SEE IF SALESPERSON CODE IS *FM, *RL, *SP, *SY                                 
* IF SO, CK RCONDVSP, AND USE IT TO RELOOK UP SALESPERSON NAME                  
*                                                                               
POST056  LA    R0,SPSALCT                                                       
         LA    R1,SPSALTAB                                                      
POST057  CLC   RCONSAL,0(R1)       THIS A SPECIAL SALES CODE                    
         BE    POST057C                                                         
         LA    R1,L'SPSALTAB(,R1)                                               
         BCT   R0,POST057                                                       
         B     POST058                                                          
POST057C OC    RCONDVSP-RCONDVEL(2,R6),RCONDVSP-RCONDVEL(R6)                    
         BZ    POST058                                                          
         MVC   SVSALEP1,SVSALESP                                                
         MVC   SVSALESP,RCONDVSP-RCONDVEL(R6)                                   
         BAS   RE,READSAL                                                       
*                                                                               
* IF MASTER CONTRACT # NOT FOUND HERE, WILL CK RCONPRD LATER-READSUB            
*                                                                               
POST058  LA    R6,RCONREC                                                       
         MVI   BYTE,X'74'                                                       
         BAS   RE,GETEL                                                         
         BNE   POST060                                                          
         USING RCONK4EL,R6                                                      
         MVC   KLMCON#(7),RCONK4MC                                              
         DROP  R6                                                               
*                                                                               
* FILL IN DEMO FROM 10 ELEMENT IF FOUND                                         
*                                                                               
POST060  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   BYTE,X'10'                                                       
         BAS   RE,GETEL                                                         
         BNE   POST068                                                          
         USING RCONBPEL,R6                                                      
         LA    R0,DMOCOUNT                                                      
         L     R1,=A(KATZDEMO)                                                  
POST062  CLC   RCONBPDM+3(1),2(R1)                                              
         BE    POST064                                                          
         LA    R1,LKTZDEMO(,R1)                                                 
         BCT   R0,POST062                                                       
         MVC   KLDEMO1,RCONKTDM                                                 
         B     POST068                                                          
POST064  MVC   KLDEMO1,0(R1)                                                    
         DROP  R6                                                               
POST068  DS    0H                                                               
         MVC   KLSTATN,RCONKSTA                                                 
         SPACE                                                                  
*  BF = B                                                                       
*  KU = K                                                                       
*  KF = H                                                                       
*  EA = E                                                                       
*  CR = C                                                                       
*  K4 = S                                                                       
         SPACE                                                                  
         LA    R0,KDIVTABN                                                      
         LA    R1,KDIVTAB                                                       
POST080  CLC   RCONKREP,0(R1)                                                   
         BE    POST084                                                          
         LA    R1,L'KDIVTAB(,R1)                                                
         BCT   R0,POST080                                                       
         B     POST086                                                          
POST084  MVC   KLDIVISN,2(R1)               DIV                                 
         SPACE                                                                  
POST086  DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
         SPACE 3                                                                
* FIND FIRST MONTH BUCKET IN KLMONCTR AREA                                      
* THIS STARTED WITH 1ST 12 BUCKETS = 95                                         
*                   2ND 12 BUCKETS = 96                                         
*                   3RD 12 BUCKETS = 92                                         
*                   4TH 12 BUCKETS = 93                                         
*                   5TH 12 BUCKETS = 94                                         
* AND THE YEARS ROTATE YEAR BY YEAR                                             
*                                                                               
         SR    R6,R6                                                            
         ZAP   CROUND,=P'0'                                                     
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    RF,MCIO-MASTD(,RE)                                               
*                                                                               
*        TABLE ENTRY VS REQ START DATE                                          
*                                                                               
         PACK  DUB,QSTART-QRECORD(2,RF)                                         
*        PACK  DUB,=C'92'          TEMP                                         
         CLC   QSTART-QRECORD(2,RF),=C'24'       THIS AFTER YEAR 2000           
         BH    *+10                                                             
         AP    DUB,=P'100'                                                      
*                                                                               
         SP    DUB,=P'92'                                                       
         CVB   RE,DUB                                                           
         LA    R0,3                                                             
         LA    R1,KLMONCTR+96                                                   
POST160  LTR   RE,RE                                                            
         BZ    POST170                                                          
         LA    R1,48(,R1)                                                       
         SH    RE,=H'1'                                                         
         BCT   R0,POST160                                                       
         LA    R0,5                                                             
         LA    R1,KLMONCTR                                                      
         B     POST160                                                          
         SPACE                                                                  
POST170  PACK  DUB,QSTART+2(2)     GET MONTH                                    
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         MH    RF,=H'4'                                                         
         AR    R1,RF                                                            
         LR    R3,R1               SAVE ADDRESS OF FIRST COUNTER                
         SPACE                                                                  
****** MONTHLY ********                                                         
         SPACE                                                                  
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
POSTNOP  NOP   POST210                                                          
         OI    POSTNOP+1,X'F0'                                                  
*        LR    R0,R2                                                            
*        AH    R0,=H'6800'                                                      
*        GOTO1 PDUMPER,DMCB,WORK,(R2),(R0)                                      
         SPACE                                                                  
         LH    R0,=H'6800'                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(R0),=C'1D'                
POST210  EQU   *                                                                
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    RF,MCIO-MASTD(,RE)                                               
*                                                                               
*        TABLE ENTRY VS REQ START DATE                                          
*                                                                               
POST214  CLC   0(4,R2),QSTART-QRECORD(RF)                                       
         BE    POST220             FOUND                                        
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST214             GO BACK FOR NEXT                             
POST220  EQU   *                                                                
         LR    R8,R2               SET A(BUCKETS WITHIN MONTH)                  
         LA    R8,BUCKDISP(R8)     PASS MONTH CONTROLS                          
         CLC   CONENDC(4),0(R2)    MONTH DATE IN TABLE: YYMM                    
         BNL   POST280                                                          
         B     POST296             DONE                                         
*                                                                               
POST250  LR    R8,R2               SET A(BUCKETS WITHIN MONTH)                  
         LA    R8,BUCKDISP(R8)     PASS MONTH CONTROLS                          
         LA    R3,4(,R3)                                                        
         SPACE                                                                  
         LA    R0,KLMONCTR+240     IF PAST END                                  
         CR    R0,R3                                                            
         BH    POST280                                                          
         LA    R3,KLMONCTR                                                      
*                                                                               
POST280  L     RF,TOTORD(R8)       TOTAL ORDERED                                
         AR    R6,RF                                                            
         SR    RE,RE               ROUND TO DOLLARS                             
         LTR   RF,RF                                                            
         BNM   POST290                                                          
         BCTR  RE,0                                                             
         SH    RF,=H'50'                                                        
         B     POST294                                                          
*                                                                               
POST290  AH    RF,=H'50'                                                        
*                                                                               
POST294  D     RE,=F'100'                                                       
*                                                                               
         CVD   RF,DUB                                                           
         AP    0(4,R3),DUB                                                      
         AP    TROUND,DUB          ADD TO ROUNDED DOLLARS TOTAL                 
         AP    CROUND,DUB          ADD TO CONTRACT ROUNDED DOLLARS              
*                                                                               
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    RF,MCIO-MASTD(,RE)                                               
*                                                                               
*        TABLE ENTRY VS REQ END DATE                                            
*                                                                               
         CLC   0(4,R2),QEND-QRECORD(RF)  TABLE ENTRY VS REQ END DATE            
         BNH   POST250                    VALID, IN PERIOD                      
*                                                                               
POST296  LTR   R6,R6                                                            
         BZ    POST300                                                          
         CVD   R6,DUB                                                           
         AP    TDOLLARS,DUB                                                     
         BNO   *+6                                                              
         DC    H'0'                                                             
*        LR    RF,R6                                                            
*        SR    RE,RE               ROUND TO DOLLARS                             
*        AH    RF,=H'50'                                                        
*        D     RE,=F'100'                                                       
*                                                                               
*        CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KLCOST,DUB                                                       
         SPACE                                                                  
         BAS   RE,DOPUT                                                         
         SPACE 3                                                                
POST300  XC    KEY,KEY            RE-SET KEY (CONTRACT) FOR CONTROLLER          
         MVC   KEY(27),RCONKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
*        POST EXIT                                                              
*                                                                               
POSTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*        DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        REQDONE --- FINISH THE REPORT                                          
*                                                                               
REQDONE  NTR1                                                                   
*                                                                               
         MVC   P+5(2),SVDREP                                                    
         EDIT  REPCONCT,(7,P+9),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(22),=CL22'CONTRACTS PROCESSED ='                             
         EDIT  CONCNT,(9,P+23),ZERO=NOBLANK,COMMAS=YES                          
         MVC   P+35(17),=CL17'RECORDS TO TAPE ='                                
         EDIT  PUTCNT,(9,P+52),ZERO=NOBLANK,COMMAS=YES                          
         GOTO1 REPORT                                                           
         MVC   P+35(13),=CL13'TERRITORIES ='                                    
         EDIT  TERRCT,(9,P+52),ZERO=NOBLANK,COMMAS=YES                          
         GOTO1 REPORT                                                           
         MVC   P+35(15),=CL13'AGENCY 1A RES ='                                  
         EDIT  AGYACT,(9,P+52),ZERO=NOBLANK,COMMAS=YES                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+5(15),=CL15'TOTAL DOLLARS ='                                   
*        EDIT  TDOLLARS,(17,P+20),2,FLOAT=-,ZERO=NOBLANK,COMMAS=YES             
         MVC   P+20(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    P+20(18),TDOLLARS+1                                              
         MVC   P+45(17),=CL17'ROUNDED DOLLARS ='                                
*                                                                               
         EDIT  TROUND,(17,P+62),0,FLOAT=-,ZERO=NOBLANK,COMMAS=YES               
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(18),=CL18'*** END OF REQ ***'                                
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        RUNLAST --- WRITE OUT AGY, ADV, SAL, AND CLOSE TAPE                    
*                                                                               
RUNDONE  NTR1                                                                   
*                                                                               
         LA    R4,WRK                                                           
         MVC   0(125,R4),SPACES          MOVE IN SPACES                         
         MVC   125(125,R4),SPACES                                               
         MVC   250(250,R4),0(R4)                                                
         SPACE                                                                  
* WRITE ALL AGENCY RECS NOW                                                     
         SPACE                                                                  
         USING KLTPRECD,R4                                                      
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'0A'                                                        
         GOTO1 HIGH                                                             
         B     RUND20                                                           
         SPACE                                                                  
RUND10   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND20   CLI   KEY,X'0A'                                                        
         BNE   RUND30                                                           
         CLC   KEY+25(2),RCREPFL                                                
         CLC   KEY+25(2),=C'K3'                                                 
         BNE   RUND10                                                           
         BAS   RE,GETAGY                                                        
         MVI   KLRECTP2,C'A'                                                    
         MVC   KLDDSAGY,KEY+19                                                  
         MVC   KLAGYNM,RAGYNAM1                                                 
         MVI   KEY,X'1A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RUND24                                                           
         BAS   RE,GETAGY                                                        
         MVC   KLKZAGEQ,RAGY2EQU    KATZ EQUIV AGENCY                           
         SPACE                                                                  
RUND24   DS    0H                                                               
         PUT   INTFILE,WRK                                                      
         L     R1,TAGYCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TAGYCTR                                                       
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY,X'0A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RUND10                                                           
         DC    H'0'                                                             
*                                                                               
RUND30   MVC   P+5(11),=CL11'TOTAL AGY ='                                       
         ICM   R2,15,TAGYCTR                                                    
         EDIT  (R2),(11,P+17),0,ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
* WRITE ALL ADVERTISER RECS NOW                                                 
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR ADVERTISER                      
         MVI   KEY,X'08'                                                        
         GOTO1 HIGH                                                             
*                                                                               
         MVC   0(130,R4),SPACES          MOVE IN SPACES                         
         MVC   130(120,R4),SPACES                                               
         MVC   250(250,R4),0(R4)                                                
         B     RUND50                                                           
         SPACE                                                                  
RUND40   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND50   CLI   KEY,X'08'                                                        
         BNE   RUND60                                                           
         CLC   KEY+25(2),RCREPFL                                                
         CLC   KEY+25(2),=C'K3'                                                 
         BNE   RUND40                                                           
         BAS   RE,GETADV                                                        
         MVI   KLRECTP3,C'B'                                                    
         MVC   KLDDSADV,KEY+21                                                  
         MVC   KLKZADEQ,RADVKATZ    KATZ EQUIV AGENCY                           
         MVC   KLADVNM,RADVNAME                                                 
         PUT   INTFILE,WRK                                                      
         L     R1,TADVCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TADVCTR                                                       
         B     RUND40                                                           
*                                                                               
RUND60   MVC   P+5(11),=CL11'TOTAL ADV ='                                       
         ICM   R2,15,TADVCTR                                                    
         EDIT  (R2),(11,P+17),0,ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
* WRITE ALL SALES PERSON RECS NOW                                               
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR SALESP                          
         MVI   KEY,X'06'                                                        
         GOTO1 HIGH                                                             
*                                                                               
         MVC   0(130,R4),SPACES          MOVE IN SPACES                         
         MVC   130(120,R4),SPACES                                               
         MVC   250(250,R4),0(R4)                                                
         B     RUND80                                                           
         SPACE                                                                  
RUND70   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND80   CLI   KEY,X'06'                                                        
         BNE   RUND90                                                           
         CLC   KEY+22(2),RCREPFL                                                
         CLC   KEY+22(2),=C'K3'                                                 
         BNE   RUND70                                                           
         BAS   RE,GETMAN                                                        
         SR    R0,R0                                                            
         ICM   R0,7,RSALMRG                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   KLRECTP4,C'C'                                                    
         MVC   KLDDSSAL,KEY+24                                                  
         UNPK  KLKZSPEQ,DUB                                                     
         MVC   KLSALNM,RSALNAME                                                 
         SPACE                                                                  
         PUT   INTFILE,WRK                                                      
         L     R1,TSALCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TSALCTR                                                       
         B     RUND70                                                           
         DROP  R4                                                               
RUND90   DS   0H                                                                
         MVC   P+5(11),=CL11'TOTAL SLS ='                                       
         ICM   R2,15,TSALCTR                                                    
         EDIT  (R2),(11,P+17),0,ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
         LA    R2,MISTABCT                                                      
         L     R3,=A(MISTABLE)                                                  
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         SR    R0,R0                                                            
RUND94   OC    0(5,RF),0(RF)                                                    
         BZ    RUND96                                                           
         BCTR  R0,0                                                             
         LA    RF,L'MISTABLE(,RF)                                               
         BCT   RE,RUND94                                                        
RUND96   LPR   R0,R0                                                            
         BZ    RUND200                                                          
         GOTO1 =V(QSORT),DMCB,(R3),(R0),L'MISTABLE,L'MISTABLE,0                 
         SPACE                                                                  
RUND100  OC    0(5,R3),0(R3)                                                    
         BZ    RUND200                                                          
         MVC   P+1(4),=C'ADV='                                                  
         CLI   0(R3),C'D'          THIS AN ADV                                  
         BE    RUND120                                                          
         MVC   P+1(4),=C'AGY='                                                  
         CLI   0(R3),C'G'          THIS AN AGY                                  
         BE    RUND120                                                          
         MVC   P+1(4),=C'SAL='                                                  
         CLI   0(R3),C'S'          THIS AN SAL                                  
         BE    *+6                                                              
         DC    H'0'                                                             
RUND120  MVC   P+5(4),1(R3)                                                     
         SR    R4,R4                                                            
         ICM   R4,3,5(R3)                                                       
         EDIT  (R4),(7,P+20),0,ZERO=NOBLANK,COMMAS=YES                          
         GOTO1 REPORT                                                           
         LA    R3,L'MISTABLE(,R3)                                               
         BCT   R2,RUND100                                                       
         DC    H'0'                                                             
RUND200  GOTO1 REPORT                                                           
         MVC   P+1(18),=CL18'*** END OF REQ ***'                                
         MVC   P+13(2),=C'UN'                                                   
         GOTO1 REPORT                                                           
         SPACE                                                                  
         CLOSE (INTFILE,)            CLOSE KATZ FILE                            
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
GBEXT    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        CHKINVEL --- IF WE HAVE AN ORDERED ELEMENT X'03' FOR A MONTH,          
*                      DO WE HAVE AN INVOICE ELEMENT X'04' FOR THE SAME         
*                      PERIOD?  IF WE HAVE A X'04' EL, LOOK FOR A X'03'         
*                                                                               
*        R3  ->  CURRENT ELEMENT                                                
*                                                                               
CHKINVEL NTR1                                                                   
*                                                                               
         LA    R4,RCONELEM                                                      
         ZIC   R5,0(R3)                                                         
         CLI   0(R3),3             HAVE A 3, LOOK FOR A 4                       
         BE    CIEL10              HAVE A 4, LOOK FOR A 3                       
         BCTR  R5,0                                                             
         B     CIEL11                                                           
CIEL10   EQU   *                                                                
         LA    R5,1(R5)                                                         
CIEL11   EQU   *                                                                
         SR    R6,R6               USE R6 AS ORDERED EL COUNT                   
         SR    R7,R7               USE R7 AS ORDERED DOL ACCUM                  
*                                                                               
CIEL50   EQU   *                                                                
         ZICM  R0,1(R4),1                                                       
         BZ    CIEL70              ZERO IS END OF REC W/O A FIND                
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    CIEL70                                                           
         ZIC   R0,0(R4)                                                         
         CR    R0,R5                                                            
         BNE   CIEL50                                                           
*                                                                               
         CLC   2(2,R4),2(R3)       CHECK MONTH-OF-SERVICE                       
         BNE   CIEL50                                                           
*                                                                               
         CLC   4(2,R4),COMPDT      FOR A HISTORICAL RUN CHECK ACTIVITY          
         BNL   CIEL60                                                           
         LA    R6,1(R6)            BUMP COUNT                                   
         CLI   0(R4),X'03'         ACCUM ORDERED DOLLARS ONLY                   
         BNE   CIEL50                                                           
         A     R7,6(R4)            AND ACCUM DOLLARS                            
         B     CIEL50                                                           
CIEL60   EQU   *                                                                
         CLI   0(R4),X'04'         AN ACTUAL BUCKET FOUND RETURNS FOUND         
         BNE   CIEL70              IF IN THE SAME WEEK                          
         CLC   4(2,R4),COMPDT                                                   
         BE    CIELBAD                                                          
CIEL70   EQU   *                                                                
         LTR   R6,R6                                                            
         BNZ   CIELBAD                                                          
*                                                                               
CIELGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     CIELEXIT                                                         
CIELBAD  EQU   *                                                                
         LA    R0,1                                                             
CIELEXIT EQU   *                                                                
         STC   R6,ORDCNT                                                        
         ST    R7,ORDAMT                                                        
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        CHKSAMEW --- IF WE HAVE AN INVOICE ELEMENT X'04' FOR A MONTH,          
*                      DO WE HAVE AN ORDERED ELEMENT X'03' FOR THE SAME         
*                      PERIOD ON THE SAME WEEK WITH NO PREVIOUS X'03'S          
*                                                                               
*        R3  ->  CURRENT ELEMENT                                                
*                                                                               
CHKSAMEW NTR1                                                                   
*                                                                               
         LA    R4,RCONELEM                                                      
         CLI   0(R3),3             HAVE A 3, GET OUT                            
         BE    CWKGOOD                                                          
         SR    R6,R6               USE R6 AS ORDERED EL COUNT                   
         SR    R7,R7               USE R7 AS ORDERED DOL ACCUM                  
*                                                                               
CWK50    EQU   *                                                                
         ZICM  R0,1(R4),1                                                       
         BZ    CWKGOOD             ZERO IS END OF REC W/O A FIND                
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    CWKGOOD                                                          
         CLI   0(R4),X'03'                                                      
         BNE   CWK50                                                            
         CLC   2(2,R4),2(R3)       CHECK MONTH-OF-SERVICE                       
         BNE   CWK50                                                            
         LA    R6,1(R6)                                                         
*                                                                               
         CLC   4(2,R4),4(R3)       CHECK ACTIVITY WEEK                          
         BE    CWK60               EQUAL IS GET OUT WITH ERROR                  
         A     R7,6(R4)                                                         
         B     CWK50               NOT EQUAL IS GET NEXT                        
*                                                                               
CWK60    EQU   *                                                                
         CLI   RECALC,0                                                         
         BE    CWK70                                                            
         A     R7,6(R4)                                                         
*                                                                               
CWK70    EQU   *                                                                
         STC   R6,ORDCNT                                                        
         ST    R7,ORDAMT                                                        
         B     CWKBAD                                                           
*                                                                               
CWKGOOD  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
CWKBAD   EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        LOCAL EDIT ROUTINES TO PRODUCED ZERO FILLED SIGN OVERPUNCHED           
*         NUMBERS FOR THE TAPE                                                  
*                                                                               
*                                                                               
*        P1   =  A(INPUT)                                                       
*        P2   =  A(OUTPUT)                                                      
*        P3   =  (DATA WIDTH, 6 OR 9)                                           
*        P4   =  (DO OVERPUCH FLAG, 0 = YES)                                    
*                                                                               
EDITOUT  NTR1                                                                   
*                                                                               
         L     R2,0(R1)            GET INPUT ADDR                               
         L     R3,4(R1)            GET OUTPUT ADDR                              
         L     R4,8(R1)            GET OUTPUT WIDTH                             
         L     R8,12(R1)           GET OVERPUCH FLAG                            
         SR    R5,R5               USE R5 AS A NEG FLAG                         
*                                                                               
         ICM   R6,15,0(R2)         INPUT NEG?                                   
         BNM   EOUT10              NO, MOVE ON                                  
*                                                                               
         SR    R6,R6               YES, WORK WITH POS NUMBER AND                
         BCTR  R6,0                 SET FLAG                                    
         X     R6,0(R2)                                                         
         LA    RF,1                ADD 1 TO POSITIVE NUMBER                     
         AR    R6,RF               NOTE:  DO 'AR' INSTRUCTION BECAUSE           
*                                     LARGE NEGATIVE NUMBERS LOSE               
*                                     HIGH-ORDER BYTE IN FORMAT                 
*                                     LA RX,1(RX)                               
         LA    R5,1                                                             
EOUT10   EQU   *                                                                
*                                                                               
         LA    R7,9                                                             
         CR    R4,R7                                                            
         BE    EOUT20                                                           
         EDIT  (R6),(6,(R3)),ZERO=NOBLANK                                       
         B     EOUT30                                                           
EOUT20   EQU   *                                                                
         EDIT  (R6),(9,(R3)),ZERO=NOBLANK                                       
EOUT30   EQU   *                                                                
         LR    R1,R4               COPY INPUT LEN FOR ZERO FILLING              
         LTR   R8,R8               CHECK OVERPUCH FLAG                          
         BNZ   EOUT50              NON-ZERO IS DON'T DO                         
         AR    R4,R3               POINT TO THE LAST CHARACTER                  
         BCTR  R4,0                                                             
         LTR   R5,R5                                                            
         BZ    EOUT50                                                           
         NI    0(R4),X'DF'         ONLY DO NEG OVERPUCH = DF                    
EOUT50   EQU   *                                                                
         CLI   0(R3),C' '                                                       
         BNE   EOUT60                                                           
         MVI   0(R3),C'0'                                                       
         LA    R3,1(R3)                                                         
         BCT   R1,EOUT50                                                        
EOUT60   EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        DOPUT --- LOGICAL OR IN SPACES TO TAKE CARE OR NULLS                   
*                   AND PUT THE RECORD                                          
*                                                                               
DOPUT    NTR1                                                                   
*                                                                               
         LA    R4,WRK                                                           
         USING KLTPREC,R4                                                       
         B     DPUT20                                                           
*                                                                               
* INITIAL CHECK THAT THERE WAS NO 92 DATA *                                     
*                                                                               
         LA    R0,12                                                            
         LA    R1,KLMONCTR+96                                                   
DPUT10   CP    0(4,R1),=P'0'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*        LA    R1,4(,R1)                                                        
*        BCT   R0,DPUT10                                                        
DPUT20   DS    0H                                                               
*                                                                               
         PUT   INTFILE,WRK                                                      
*                                                                               
         ICM   R1,15,PUTCNT                                                     
         A     R1,=F'1'                                                         
         STCM  R1,15,PUTCNT                                                     
*                                                                               
*    IF REQUESTOR = 'SPEC PRINT' CALL MODULE TO DISPLAY TAPE                    
*       RECORD ON PRINTOUT                                                      
*                                                                               
*        CLC   =C'3090394',KLCON#                                               
*        BE    DUMPIT20                                                         
         CLC   =C'SPEC PRINT',QUESTOR                                           
         BNE   DPUTGOOD                                                         
DUMPIT20 GOTO1 =A(DUMPTAPE),DMCB,(RC)                                           
*                                                                               
DPUTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        READ ADVERTISER/AGENCY/PRODUCT/STATION FOR OUTPUT REC                  
*                                                                               
READSUB  NTR1                                                                   
*                                                                               
READS08  LA    R2,ADVTABCT                                                      
         L     R3,=A(ADVTABLE)                                                  
         LA    R4,WRK                                                           
         USING KLTPRECD,R4                                                      
READS10  CLC   TADVC(4),0(R3)                                                   
         BE    READS16                                                          
         OC    0(4,R3),0(R3)                                                    
         BZ    READS14                                                          
         LA    R3,L'ADVTABLE(,R3)                                               
         BCT   R2,READS10                                                       
         L     RE,=A(ADVTABLE)                                                  
         L     RF,=A(L'ADVTABLE*ADVTABCT)                                       
         XCEF                                                                   
         B     READS08                                                          
         SPACE                                                                  
READS14  MVC   0(4,R3),TADVC                                                    
         MVC   9(20,R3),=CL20'NAME NOT FOUND'     DEFAULT                       
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR ADVERTISER                      
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),TADVC                                                  
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'08'                                                        
         BNE   READS16                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   READS16                                                          
         BAS   RE,GETADV                                                        
* KLKZADEQ FROM ADV                                                             
         MVC   4(5,R3),KLKZADEQ                                                 
         MVC   9(20,R3),RADVNAME    MOVE IN ADVERTISER NAME                     
READS16  DS    0H                                                               
         MVC   KLADVNAM,9(R3)                                                   
         CLC   KLADVNAM,=CL20'NAME NOT FOUND'     DEFAULT                       
         BNE   READS18                                                          
         XC    DUB,DUB                                                          
         MVI   DUB,C'D'            INDICATE ADV                                 
         MVC   DUB+1(4),0(R3)                                                   
         BAS   RE,PUTMISS                                                       
READS18  LA    R2,AGYTABCT                                                      
         L     R3,=A(AGYTABLE)                                                  
READS20  CLC   TAGYC(4),0(R3)                                                   
         BNE   READS21                                                          
         CLC   RCONKAOF,4(R3)                                                   
         BE    READS26                                                          
READS21  OC    0(4,R3),0(R3)                                                    
         BZ    READS22                                                          
         LA    R3,L'AGYTABLE(,R3)                                               
         BCT   R2,READS20                                                       
         L     RE,=A(AGYTABLE)                                                  
         L     RF,=A(L'AGYTABLE*AGYTABCT)                                       
         XCEF                                                                   
         B     READS18                                                          
         SPACE                                                                  
READS22  MVC   0(4,R3),TAGYC                                                    
         MVC   4(2,R3),RCONKAOF                                                 
         MVC   6(20,R3),=CL20'NAME NOT FOUND'     DEFAULT                       
         MVC   26(13,R3),SPACES                                                 
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),TAGYC                                                  
         MVC   KEY+23(2),RCONKAOF  AGENCY CITY CODE                             
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'0A'                                                        
         BNE   READS26                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   READS26                                                          
         BAS   RE,GETAGY                                                        
         MVC   6(33,R3),RAGYNAM1                                                
         MVI   KEY,X'1A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    READS24                                                          
         L     RE,AGYACT                                                        
         LA    RE,1(,RE)                                                        
         ST    RE,AGYACT                                                        
         B     READS26                                                          
READS24  BAS   RE,GETAGY                                                        
         MVC   39(2,R3),RAGY2TER    NEW TERRITORY                               
         OC    RAGY2TER,RAGY2TER    NEW TERRITORY *TEMP*                        
         BZ    READS26              *TEMP*                                      
         L     RE,TERRCT                                                        
         LA    RE,1(,RE)                                                        
         ST    RE,TERRCT                                                        
         SPACE                                                                  
READS26  DS    0H                                                               
         MVC   KLAGYNAM,6(R3)                                                   
         MVC   KLTERRCD,39(R3)                                                  
         CLC   KLAGYNAM(20),=CL20'NAME NOT FOUND'     DEFAULT                   
         BNE   READS28                                                          
         XC    DUB,DUB                                                          
         MVI   DUB,C'G'            INDICATE AGY                                 
         MVC   DUB+1(4),0(R3)                                                   
         BAS   RE,PUTMISS                                                       
         SPACE                                                                  
READS28  DS    0H                                                               
         BAS   RE,READSAL          GO GET SALESPERSON ANME                      
         SPACE                                                                  
READS40  DS    0H                                                               
         SPACE                                                                  
RDSUB20  DS    0H                                                               
         CLC   RCONPRD,SPACES      IS THERE A PROD CODE?                        
         BNE   RDSUB35             YES--- GO READ PROD RECORD                   
*                                                                               
         LA    R1,RCONELEM                                                      
         SR    R0,R0                                                            
RDSUB30  ICM   R0,1,1(R1)                                                       
         BZ    RDSUBOK                                                          
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    RDSUBOK                                                          
         CLI   0(R1),5            FIND PRODUCT NAME ELEMENT                     
         BNE   RDSUB30                                                          
         MVC   KLPRDNAM,RCONEXPR-RCONEXEL(R1)    PRODUCT NAME                   
         B     RDSUBOK                                                          
*                                                                               
RDSUB35  XC    KEY,KEY            READ PRODUCT RECORD                           
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),RCONPRD                                                
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'09'                                                        
         BNE   RDSUBOK                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUBOK                                                          
         BAS   RE,GETPROD                                                       
         MVC   KLPRDNAM,RPRDNAME              PRODUCT DESCRIPTION               
         CLC   KLMCON#,SPACES      ALREADY HAVE MASTER CONTRACT #               
         BNE   RDSUBOK                                                          
         MVC   KLMCON#,RPRDNET#                                                 
         SPACE                                                                  
RDSUBOK  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
* READ SALESPERSON NAME                                                         
*                                                                               
READSAL  NTR1                                                                   
RDSAL10  LA    R2,SALTABCT                                                      
         L     R3,=A(SALTABLE)                                                  
RDSAL20  CLC   SVSALESP(3),0(R3)                                                
         BE    RDSAL60                                                          
         OC    0(3,R3),0(R3)                                                    
         BZ    RDSAL30                                                          
         LA    R3,L'SALTABLE(,R3)                                               
         BCT   R2,RDSAL20                                                       
         L     RE,=A(SALTABLE)                                                  
         L     RF,=A(L'SALTABLE*SALTABCT)                                       
         XCEF                                                                   
         B     RDSAL10                                                          
         SPACE                                                                  
RDSAL30  MVC   0(3,R3),SVSALESP                                                 
         MVC   8(20,R3),=CL20'NAME NOT FOUND'     DEFAULT                       
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR SALESP                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),RCREPFL                                                
         MVC   KEY+24(3),SVSALESP                                               
         GOTO1 HIGH                                                             
         CLI   KEY,X'06'                                                        
         BNE   RDSAL60                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSAL60                                                          
         BAS   RE,GETMAN                                                        
         SR    R0,R0                                                            
         ICM   R0,7,RSALMRG                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(5,R3),DUB                                                      
         MVC   8(20,R3),RSALNAME                                                
RDSAL60  MVC   KLSALNAM,8(R3)                                                   
         SPACE                                                                  
         CLC   KLSALNAM(20),=CL20'NAME NOT FOUND'     DEFAULT                   
         BNE   RDSAL80                                                          
         XC    DUB,DUB                                                          
         MVI   DUB,C'S'            INDICATE SALES                               
         MVC   DUB+1(3),0(R3)                                                   
         BAS   RE,PUTMISS                                                       
RDSAL80  XIT1                                                                   
         SPACE                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (FILE GETS)                                     
*                                                                               
         SPACE                                                                  
GETAGY   LA    RF,RAGYREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETMAN   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETTEAM  LA    RF,RTEMREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETSTAT  LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETPROD  LA    RF,RPRDREC                                                       
*                                                                               
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         L     R2,AIOAREA                                                       
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
*                                                                               
         SPACE                                                                  
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION                
         BZ    DM020               NO - ERROR                                   
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     GBEXT                                                            
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    GBEXT                                                            
*                                                                               
DM020    MVC   WORK(24),=C'***DATA MANAGER ERROR***'                            
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO SAVE MISSING ADV, AGY, SALES CODES                          
*                                                                               
         SPACE                                                                  
PUTMISS  NTR1                                                                   
         LA    R2,MISTABCT                                                      
         L     R3,=A(MISTABLE)                                                  
PUTMIS20 CLC   DUB(5),0(R3)                                                     
         BE    PUTMIS50                                                         
         OC    0(5,R3),0(R3)                                                    
         BZ    PUTMIS40                                                         
         LA    R3,L'MISTABLE(,R3)                                               
         BCT   R2,PUTMIS20                                                      
         DC    H'0'                                                             
PUTMIS40 MVC   0(5,R3),DUB                                                      
         SPACE                                                                  
PUTMIS50 SR    R1,R1                                                            
         ICM   R1,3,5(R3)                                                       
         LA    R1,1(,R1)                                                        
         STCM  R1,3,5(R3)                                                       
         MVC   P+1(4),=C'ADV='                                                  
         CLI   DUB,C'D'            THIS AN ADV                                  
         BE    PUTMIS60                                                         
         MVC   P+1(4),=C'AGY='                                                  
         CLI   DUB,C'G'            THIS AN AGY                                  
         BE    PUTMIS60                                                         
         MVC   P+1(4),=C'SAL='                                                  
         CLI   DUB,C'S'            THIS AN SAL                                  
         BE    *+6                                                              
         DC    H'0'                                                             
PUTMIS60 MVC   P+5(4),1(R3)                                                     
         MVC   P+12(L'KLCON#),KLCON#                                            
         GOTO1 REPORT                                                           
PUTMISX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TRACE DATA MANAGER CALLS                                    
*                                                                               
         SPACE                                                                  
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,MTRACKEY                                                      
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
         B     GBEXT                                                            
         SPACE                                                                  
         GETEL R6,34,BYTE                                                       
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECOMRTN                                                       
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*  TAPE FOR INTEREP DATA                                                        
         SPACE                                                                  
INTFILE  DCB   DDNAME=INTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00600,                                            X        
               BLKSIZE=24000,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
SPSALTAB DC    C'*FM',C'*RL',C'*SP',C'*SY'                                      
SPSALCT  EQU   (*-SPSALTAB)/3                                                   
         SPACE                                                                  
OFFNTAB  DS    0CL5                                                             
         DC    C'NY',C'001'                                                     
         DC    C'CH',C'002'                                                     
         DC    C'AT',C'010'                                                     
         DC    C'BO',C'015'                                                     
         DC    C'DA',C'025'                                                     
         DC    C'DV',C'028'                                                     
         DC    C'DE',C'030'                                                     
         DC    C'HO',C'032'                                                     
         DC    C'LA',C'040'                                                     
         DC    C'MI',C'046'                                                     
         DC    C'MN',C'048'                                                     
         DC    C'PH',C'050'                                                     
         DC    C'PO',C'052'                                                     
         DC    C'SL',C'055'                                                     
         DC    C'SF',C'060'                                                     
         DC    C'SE',C'065'                                                     
         DC    C'TO',C'107'                                                     
OFFNTABN EQU   (*-OFFNTAB)/5                                                    
         SPACE                                                                  
KDIVTAB  DS   0CL3                                                              
         DC    C'BFB'                                                           
         DC    C'CRC'                                                           
         DC    C'EAE'                                                           
         DC    C'KFH'                                                           
         DC    C'KUK'                                                           
         DC    C'K4S'                                                           
         DC    C'RSA'                                                           
         DC    C'S3M'                                                           
KDIVTABN EQU   (*-KDIVTAB)/3                                                    
         SPACE                                                                  
* 2ND POSITION IS DDS CONTYP CODE, 1ST POSITION IS CODE WE SEND THEM            
* IF OUR CODE IS NOT IN THE TABLE, WE SEND THEM A BLANK                         
         SPACE                                                                  
CONTYPTB DC    CL2' S'             SPOT                                         
         DC    CL2'DD'             DIRECT                                       
         DC    CL2'SA'             SPORTS                                       
         DC    CL2'FF'             FARM                                         
         DC    CL2'KN'             NETWORK                                      
         DC    CL2'RR'             RELIGION                                     
CONTYPTL EQU   (*-CONTYPTB)/2                                                   
         EJECT                                                                  
         EJECT                                                                  
RELO     DS    F                                                                
SAVEDRB  DS    F                                                                
ORDAMT   DS    F                   ORDERED DOLLARS UP TO ACT WEEK               
*                                                                               
TDOLLARS DC    PL8'0'              DOLLARS SENT                                 
TROUND   DC    PL8'0'              ROUNDED DOLLARS SENT                         
CROUND   DC    PL8'0'              CONTRACT ROUNDED DOLLARS SENT                
*                                                                               
PUTCNT   DC    F'0'                                                             
CONCNT   DC    F'0'                                                             
TALLYCT  DC    F'0'                                                             
TAGYCTR  DC    F'0'                                                             
TADVCTR  DC    F'0'                                                             
TSALCTR  DC    F'0'                                                             
REPCONCT DC    F'0'                                                             
TERRCT   DC    F'0'                                                             
AGYACT   DC    F'0'                                                             
*                                                                               
SVDREP   DC    CL2' '                                                           
*                                                                               
RECALC   DS    CL1                 RECALC FLAG, 0=NO                            
ORDCNT   DS    CL1                                                              
COMPDT   DS    CL2                                                              
ENDDT    DS    CL3                                                              
GROSSDT  DS    CL3                                                              
GROSSDT2 DS    CL6                                                              
TEMPDATE DS    CL6                                                              
TEMPEND  DS    CL6                                                              
CONEND   DS    CL3                END OF BROADCAST MONTH FOR THIS CON           
CONENDC  DS    CL6                CHARACTER - END OF BC  FOR THIS CON           
COMMAND  DS    CL8                                                              
STAIFACE DS    CL10               INTERFACE CODE FROM STATION RECORD            
STARANK  DS    CL1                STATION RANK                                  
STAMKT   DS    CL20               STATION MARKET RANK                           
BRDDTS   DS    CL12               OUTPUT DATES FROM GETBROAD                    
MTRACKEY DS    CL32                                                             
STAMKTNM DS    CL20                                                             
WORK2    DS    CL50               ANOTHER WORK AREA FOR DEMO NAMES              
TADVC    DS    CL5                                                              
TAGYC    DS    CL4                                                              
SVSALESP DS    CL3                                                              
SVSALEP1 DS    CL3                                                              
WRK      DS    CL600              LENGTH OF RECORD TO BE WRITTEN                
       ++INCLUDE REKTZRDM                                                       
         SPACE                                                                  
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE RECOMTAB                                                       
       ++INCLUDE RECOMIO                                                        
         EJECT                                                                  
       ++INCLUDE REREP3L03                                                      
         LTORG                                                                  
         DC    C'*ADVTAB*'                                                      
ADVTABLE DC   4000XL29'00'    1-4 TADVC 4-9 KATZ EQU 10-29 ADV NAME             
ADVTABCT EQU  4000                                                              
         DC    C'*AGYTAB*'                                                      
AGYTABLE DC   2000XL41'00'    1-4 TAGYC 5-6 RCONKAOF 7-39 AGY NAME              
AGYTABCT EQU  2000                  40-41 TERRITORY CODE                        
         DC    C'*SALTAB*'                                                      
SALTABLE DC   2000XL28'00'    1-3 RCONSAL 4-8 RSALMRG 9-28 SALE NAME            
SALTABCT EQU  2000                                                              
MISTABLE DC   2000XL7'00'                                                       
MISTABCT EQU   2000                                                             
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*        INITIAL --- PROCESS TO START THE TAPE                                  
*                                                                               
INITIAL  NMOD1 0,INITIAL                                                        
*                                                                               
         L     RC,FILEC                                                         
         L     R2,ADCONLST                                                      
         L     R3,MASTC-ADCONSD(R2)                                             
         LA    R4,MCIO-MASTD(,R3)                                               
         MVC   QSTART-QRECORD(12,R4),Q3XTRFRM-QREC3D+160(R4)                    
         MVC   P(80),0(R4)                                                      
         GOTO1 REPORT                                                           
         MVC   P(80),80(R4)                                                     
         GOTO1 REPORT                                                           
         MVC   P(80),160(R4)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    CONCNT,CONCNT                                                    
         XC    TALLYCT,TALLYCT                                                  
         XC    PUTCNT,PUTCNT                                                    
         ZAP   TDOLLARS,=P'0'                                                   
         ZAP   TROUND,=P'0'                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
*                                                                               
         MVC   TEMPDATE,QASAT                                                   
         GOTO1 GETDAY,DMCB,QASAT,FULL                                           
         CLI   DMCB,1             ARE WE MONDAY?                                
         BE    INIT10             GOOD -                                        
*                                 NEED TO GET MONDAY PREV                       
         ZIC   R1,DMCB                                                          
         S     R1,=F'1'                                                         
         LNR   R2,R1                                                            
         GOTO1 ADDAY,DMCB,QASAT,TEMPDATE,(R2)                                   
         MVC   QASAT,TEMPDATE                                                   
INIT10   EQU   *                                                                
*                                                                               
*    SET UP DATE TABLE USED IN FILCON'S SETTING UP OF MONTABLE                  
*                                                                               
         L     R3,AMONARCH                                                      
         USING MONARCHD,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,QASAT),(2,MONACT+2)                               
*                                                                               
INIT20   EQU   *                                                                
         L     R6,AMONFORC                                                      
         MVC   0(2,R6),MONACT+2                                                 
*                                     FILCON NEEDS LAST YEAR                    
         MVC   MONACT+4(4),MONACT     EVEN THOUGH THESE DATES AREN'T            
         MVC   QSTART+4(2),=C'01'     (THE ABOVE COMMENT MAY MEAN               
         MVC   QEND+4(2),=C'01'       SOMETHING TO SOMEONE)                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDT)                                   
         GOTO1 (RF),(R1),(0,QASAT),(2,COMPDT)                                   
*                                                                               
         SR    R0,R0                                                            
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  REMONARCHD                                                                   
*  REGENALL                                                                     
*  REREPWORKD                                                                   
*  REREPMODES                                                                   
*  REGENCOM                                                                     
*  DEDBLOCK                                                                     
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE REMONARCHD                                                     
       ++INCLUDE REGENALL1A                                                     
         ORG   RPRDELMX            DO FREAK OUT REPORTER SPACEEND               
       ++INCLUDE REGENAGY2                                                      
RCTLRECD DSECT                                                                  
       ++INCLUDE REGENCTL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
COMDSCT  DSECT                                                                  
       ++INCLUDE REGENCOM                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DSECT TO COVER TAPE RECORDS                                                   
         SPACE                                                                  
KLTPRECD DSECT                                                                  
       ++INCLUDE REGENKTP1A                                                     
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
QREC3D   DSECT                                                                  
       ++INCLUDE REGENREQ3                                                      
*                              500 TOTAL                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREP3L02 05/01/02'                                      
         END                                                                    
