*          DATA SET REREP3H02S AT LEVEL 159 AS OF 05/01/02                      
*          DATA SET REREP3H02  AT LEVEL 109 AS OF 07/17/97                      
*PHASE RE3H02A,*                                                                
         TITLE 'RE3H02 - REREP3H02 - KATZ INTERFACE TAPE'                       
**********************************************************************          
* UNDER MVS, AN UNLABELLED TAPE IS CREATED BY MEANS OF LABEL=(,NL)   *          
* PARAMETER ON THE APPROPRIATE DD STATEMENT FOR THE OUTPUT TAPE.     *          
**********************************************************************          
         SPACE 2                                                                
*********************************************************************           
*                                                                   *           
*   REREP3H02 - RE3H02 - KATZ TAPE                                  *           
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
* JAN20/98 (BU ) --- INITIAL ENTRY (FROM 3G02)                      *           
*                                                                   *           
* JAN20/98 (BU ) --- KILL 'NO COMMISSION' MESSAGE                   *           
*                                                                   *           
* JUL28/00 (BU ) --- FIX Y2K BUG                                    *           
*                                                                   *           
* JAN30/02 (BU ) --- CHECK RER IN 2F ELEMENT                        *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE3H02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3H02,R9,RR=R2                                              
         ST    R2,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
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
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*        INITIAL --- PROCESS TO START THE TAPE                                  
*                                                                               
INITIAL  NTR1                                                                   
****                                                                            
* DEMOCON FIX                                                                   
         GOTOX LOADER,DMCB,=CL8'T00AE0',0                                       
         MVC   VDEMOCON,4(R1)                                                   
         OC    VDEMOCON,VDEMOCON                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
****                                                                            
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    CONCNT,CONCNT                                                    
         XC    PUTCNT,PUTCNT                                                    
*                                                                               
         XC    RTOTCHGR,RTOTCHGR                                                
         XC    RTOTCHCM,RTOTCHCM                                                
         XC    RTOTABGR,RTOTABGR                                                
         XC    RTOTABCM,RTOTABCM                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
*                                                                               
         OPEN  (INTFILE,OUTPUT)                                                 
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
         MVC   ASATDT,MONACT+2                                                  
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
         GOTO1 DATCON,DMCB,(0,QASAT),(2,COMPDT)                                 
*                                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         DROP  R3                                                               
         EJECT                                                                  
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
         SPACE 1                                                                
SINTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        POST --- PROCESS THE CURRENT CONTRACT                                  
*                                                                               
POST     NTR1                                                                   
*                                                                               
*   TEST                                                                        
*        CLC   =X'00169616',RCONKCON                                            
*        BNE   POST300                                                          
*   TEST END                                                                    
*                                                                               
         L     R1,CONCNT                                                        
         A     R1,=F'1'                                                         
         ST    R1,CONCNT          CONTRACT COUNTER                              
*                                                                               
         LA    R1,RCONELEM         LOOK FOR EXTENDED DESCRIPTIN ELEM            
         SR    R0,R0                                                            
POST02   ICM   R0,1,1(R1)                                                       
         BZ    POST03                                                           
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    POST03                                                           
         CLI   0(R1),X'2F'         EXCLUDE RER ELEM?                            
         BNE   POST02              NO  - CHECK NEXT                             
         TM    RCONFRER-RCONRER(R1),X'01'                                       
*                                  CHECK STATUS, EXCLUDE RER?                   
         BO    POST300             YES,GET OUT, DON'T PRINT                     
*                                                                               
POST03   XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+12)                           
         GOTO1 GETBROAD,DMCB,WORK+12,WORK,GETDAY,ADDAY                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,CONEND)                                
         MVC   CONENDC(6),WORK+6                                                
         XC    WORK,WORK                                                        
*                                                                               
         LA    R6,WRK                                                           
         USING TAPED,R6                                                         
         MVC   TAPELN(130),SPACES          MOVE IN SPACES                       
         MVC   TAPELN+130(130),SPACES                                           
         MVC   TAPELN+260(40),SPACES                                            
*                                                                               
         MVC   TREP,QREP          MOVE REP ONTO TAPE                            
         MVC   TSTAT,RCONKSTA     AND STATION                                   
         MVC   TINTFACE,STAIFACE  AND INTERFACE CODE                            
         MVC   TMARKET,STAMKT     AND MARKET NAME                               
         MVC   TSRANK,STARANK     AND RANK                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,QASAT),(X'20',TTRANDT)                            
**       MVC   TTRANDT,QASAT      TRANSACTION DATE                              
         MVC   TTYPE(2),SPACES                                                  
         MVC   TTYPE(1),RCONTYPE  CONTRACT TYPE                                 
*                                                                               
         MVC   DUB(4),RCONKCON                                                  
         MVI   DUB+4,X'0F'                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   TCON,WORK          CONTRACT NUMBER                               
*                                                                               
         MVC   TSALC,RCONSAL      SALESPERSON CODE                              
         MVC   TSALNM,RSALNAME    MOVE ON TAPE SALES PERSON                     
*                                                                               
         MVC   TOFFC,RCONKOFF                                                   
         MVC   TOFFCNM,ROFFNAME    MOVE IN OFFICE NAME                          
         MVC   TOFFREG,ROFFREG     MOVE IN OFFICE REGION                        
*                                                                               
****     MVC   TTEAMC,RCONTEM                                                   
****     MVC   TTEAMNM,RTEMNAME             TEAM NAME                           
****     MVC   TTEAMDN,RTEMDVNM             DIVISION NAME                       
*                                                                               
         MVC   TDEVSP(5),SPACES    CLEAR DEVELOPMENTAL CODES                    
         LA    R1,RCONELEM         LOOK FOR DEVELOP ELEMENT                     
         SR    R0,R0                                                            
POST05   ICM   R0,1,1(R1)                                                       
         BZ    POST15                                                           
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    POST15                                                           
         CLI   0(R1),X'18'         DEVELOPMENTAL ELEMENT?                       
         BNE   POST05              NO  - GO BACK FOR NEXT                       
         MVC   TDEVSP(5),RCONDVSP-RCONDVEL(R1)                                  
*                                  YES - LOAD BOTH CODES AT ONCE                
*                                                                               
POST15   EQU   *                                                                
         MVC   TCATCD(5),SPACES             LOAD CATEGORY CODE                  
         MVC   TCATCD(2),RCONCTGY           CATEGORY CODE                       
*                                                                               
         MVC   TOPTION1(1),QOPTION1         LOAD REQ OPT1                       
*                                                                               
         LA    R1,RCONELEM                                                      
         SR    R0,R0                                                            
POST20   ICM   R0,1,1(R1)                                                       
         BZ    POST100                                                          
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    POST100                                                          
         CLI   0(R1),X'10'        FIND DEMOGRAPHICS IN BOP ELEM                 
         MVC   TDEMO,SPACES                                                     
         BNE   POST20                                                           
         LA    R1,RCONBPDM-RCONBPEL(R1)      POINT TO BOP DEMOS                 
         CLC   QUESTOR(8),=C'$KIPDEMO'       **TEST**                           
         BE    POST100                       **TEST**                           
         CLI   0(R1),X'FF'                   TEXT OR CODE?                      
         BE    POST30                                                           
         MVC   TDEMO,0(R1)                   DEMOGRAPHICS TEXT                  
         B     POST100                                                          
POST30   EQU   *                                                                
         LA    R4,DEMBLOCK                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
         MVI   DBSELMED,C'R'                                                    
*                                                                               
         LA    R5,WORK2                                                         
         XC    WORK2(30),WORK2                                                  
         MVC   0(L'RCONBPDM-1,R5),1(R1)          DEMOS + ENDING ZERO            
         LA    R3,2                                                             
POST40   CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,POST40                                                        
*                                                                               
         LA    R5,WORK2                                                         
         GOTO1 VDEMOCON,DMCB,(2,(R5)),(9,TDEMO),(0,DBLOCKD)                     
         DROP  R4                                                               
*                                                                               
POST100  EQU   *                                                                
         CLI   QOPTION1,C'M'      ARE WE WEEKLY OR MONTHLY?                     
         BE    POST200                                                          
***** WEEKLY *****                                                              
         LA    R3,RCONELEM                                                      
POST130  SR    R0,R0                                                            
         ICM   R0,1,1(R3)                                                       
         BZ    POST150            CHECK 04'S                                    
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    POST150            CHECK 04'S                                    
         CLI   0(R3),3                                                          
         BNE   POST130                                                          
*                                                                               
         MVI   RECALC,0                                                         
         MVI   TTRANST,C'E'           X'03' ARE ESTIMATES                       
         CLC   COMPDT,RCONBKWK-RCONBKEL(R3)                                     
         BNE   POST130                                                          
         CLC   ENDDT(2),RCONBKYR-RCONBKEL(R3)                                   
         BL    POST130                                                          
         BAS   RE,CHKINVEL            SEE IF A '04' EL EXISTS                   
         BNZ   POST130                                                          
         MVC   GROSSDT(2),RCONBKYR-RCONBKEL(R3)                                 
         MVI   GROSSDT+2,1                                                      
         GOTO1 DATCON,DMCB,(3,GROSSDT),(X'20',GROSSDT2)                         
         MVC   CHAGROS(4),RCONBKAM-RCONBKEL(R3)                                 
         BAS   RE,GETAMTS                                                       
         LA    R8,INTFILE          A(INTFILE) FOR PUT ROUTINE                   
         BAS   RE,DOPUT                                                         
         GOTO1 =A(RUNTOT)                                                       
         B     POST130                                                          
         SPACE 4                                                                
POST150  EQU   *                                                                
         CLI   QOPTION2,C'E'     ESTIMATES ONLY?                                
         BE    POST300           YES, GET OUT                                   
*                                                                               
         LA    R3,RCONELEM                                                      
POST155  EQU   *                                                                
         LR    R4,R3                                                            
         SR    R0,R0                                                            
         ICM   R0,1,1(R3)                                                       
         BZ    POST300            EXIT                                          
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    POST300            EXIT                                          
         CLI   0(R3),4                                                          
         BNE   POST155                                                          
*                                                                               
         CLC   CONEND(2),RCONSTYR-RCONSTEL(R3) THIS EL WITHIN CONTRACT?         
         BL    POST155                                                          
*                                                                               
         MVI   RECALC,0           FIRST FIND OF AN X'04' IS RECALC              
         MVI   TTRANST,C'A'           X'04'S ARE ACTUALS                        
         CLC   COMPDT,RCONSTWK-RCONSTEL(R3)                                     
         BNE   POST155                                                          
         CLC   ENDDT(2),RCONSTYR-RCONSTEL(R3)                                   
         BL    POST155                                                          
         CR    R3,R4                                                            
         BE    POST160                                                          
         CLC   0(4,R3),0(R4)      FIRST OR SECOND+ EL FOR MONTH                 
         BNE   POST165             OF SERVICE                                   
POST160  EQU   *                                                                
         MVI   RECALC,1                                                         
POST165  EQU   *                                                                
         MVC   GROSSDT(2),RCONSTYR-RCONSTEL(R3)                                 
         MVI   GROSSDT+2,1                                                      
         GOTO1 DATCON,DMCB,(3,GROSSDT),(X'20',GROSSDT2)                         
         MVC   CHAGROS(4),RCONSTAM-RCONSTEL(R3)                                 
*                                                                               
         ZICM  R0,CHAGROS,4        CHECK ACTIVITY AMOUNT                        
         BNZ   POST170             IF NON-ZERO JUST MOVE ON                     
         BAS   RE,CHKINVEL         ELSE GET PREVIOUSLY SENT ORDERED             
         BZ    POST155                                                          
         MVI   RECALC,2                                                         
         B     POST175                                                          
POST170  EQU   *                                                                
         XC    ORDAMT(4),ORDAMT                                                 
         BAS   RE,CHKSAMEW         SEE IF ORDERED AND ACT'ED IN SAME WK         
         BZ    POST175                                                          
         CLI   ORDCNT,1                                                         
         BNE   POST173                                                          
         MVI   RECALC,3                                                         
         B     POST175                                                          
POST173  EQU   *                                                                
         CLI   RECALC,0                                                         
         BNE   POST173A                                                         
         MVI   RECALC,5                                                         
         B     POST175                                                          
POST173A EQU   *                                                                
         MVI   RECALC,6                                                         
POST175  EQU   *                                                                
         BAS   RE,GETAMTS                                                       
         LA    R8,INTFILE          A(INTFILE) FOR PUT ROUTINE                   
         BAS   RE,DOPUT                                                         
         GOTO1 =A(RUNTOT)                                                       
         B     POST155                                                          
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
         SPACE 3                                                                
****** MONTHLY ********                                                         
POST200  EQU   *                                                                
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
POST202  EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST204             FOUND                                        
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST202             GO BACK FOR NEXT                             
POST204  EQU   *                                                                
         LR    R8,R2               SET A(BUCKETS WITHIN MONTH)                  
         LA    R8,BUCKDISP(R8)     PASS MONTH CONTROLS                          
         CLC   CONENDC(4),0(R2)    MONTH DATE IN TABLE: YYMM                    
         BL    POST300                                                          
         BAS   RE,GETRANST         GET TRANSACTION STATUS                       
         SR    R5,R5                                                            
         SR    R7,R7                                                            
         L     R5,PRASATOR(R8)     PRIOR AS AT ORDERED                          
*                                                                               
         TM    FLAG6(R2),X'01'                                                  
         BO    POST210                                                          
*                                                                               
         L     R5,TOTORD(R8)       TOTAL ORDERED                                
         A     R5,CUASATIN(R8)     CURRENT AS AT INVOICED                       
         B     POST250                                                          
POST210  EQU   *                                                                
         LA    R3,RCONELEM                                                      
POST220  EQU   *                                                                
         ZICM  R0,1(R3),1                                                       
         BZ    POST250                                                          
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    POST250                                                          
         CLI   0(R3),4                                                          
         BNE   POST220                                                          
*                                                                               
*   TEST                                                                        
*        MVC   P+1(07),=C'04 ELT='                                              
*        MVC   P+10(10),0(R3)                                                   
*        MVC   P+22(06),=C'ENDDT='                                              
*        MVC   P+30(2),ENDDT                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   ENDDT(2),RCONBKYR-RCONBKEL(R3)                                   
*                                  CHECK ELEMENT'S MONTH OF SERVICE             
         BNE   POST220             NOT MONTH OF SERVICE                         
         CLC   ASATDT(2),RCONBKWK-RCONBKEL(R3)                                  
*                                  CHECK ELEMENT'S AS-AT DATE                   
         BL    POST220             AS-AT EARLIER THAN ELT ACT DATE              
         ZICM  RF,RCONBKAM-RCONBKEL(R3),4                                       
         BNZ   POST230                                                          
         LA    R7,1                                                             
         B     POST220                                                          
POST230  EQU   *                                                                
         SR    R7,R7                                                            
         AR    R5,RF               ACCUMULATE INVOICE DOLLARS                   
         B     POST220                                                          
POST250  EQU   *                                                                
         LTR   R5,R5                                                            
         BZ    POST300                                                          
         LTR   R7,R7                                                            
         BNZ   POST300                                                          
*                                                                               
         GOTO1 GETBROAD,DMCB,(R2),BRDDTS                                        
         GOTO1 DATCON,DMCB,(0,BRDDTS+6),(X'20',TSERVDT)                         
***      MVC   TSERVDT,BRDDTS+6   END DATE = MONTH OF SERVICE                   
*                                                                               
         ST    R5,ABSGROS                                                       
         XC    ABSCOMD,ABSCOMD    FOR COMMISSION AMT                            
         MVI   DMCB+23,C'Y'        SET $00 COMMISSION FLAG                      
         GOTO1 DOCOMM,DMCB,(R2),(R5),ABSCOMD,ABSCOMR,1                          
         GOTO1 EDITOUT,DMCB,ABSGROS,TGROSS,9,0                                  
         GOTO1 EDITOUT,DMCB,ABSCOMD,TCOMMAT,9,0                                 
         GOTO1 EDITOUT,DMCB,ABSGROS,TCHGROSS,9,0                                
         GOTO1 EDITOUT,DMCB,ABSCOMD,TCHCOMAT,9,0                                
         GOTO1 EDITOUT,DMCB,ABSCOMR,TCOMMRT,6,1                                 
         LA    R8,INTFILE          A(INTFILE) FOR PUT ROUTINE                   
         BAS   RE,DOPUT                                                         
         GOTO1 =A(RUNTOT)                                                       
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
*        GETAMTS --- GET THE OTHER DOLLAR AMOUNTS                               
*                                                                               
GETAMTS  NTR1                                                                   
*                                                                               
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
*                                                                               
GETAMT02 EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    GETAMT05            FOUND                                        
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     GETAMT02            GO BACK FOR NEXT                             
GETAMT05 EQU   *                                                                
         CLC   0(4,R2),GROSSDT2                                                 
         BE    GETAMT10                                                         
         CLI   0(R2),0                                                          
         BE    GETAMTX                                                          
         CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    GETAMTX                                                          
         LA    R2,NEXTBUCK(R2)                                                  
         B     GETAMT05                                                         
*                                                                               
GETAMT10 EQU   *                                                                
         LR    R4,R2               SET A(BUCKETS IN MONTH)                      
         LA    R4,BUCKDISP(R4)     SKIP MONTH CONTROLS                          
*                                                                               
         GOTO1 GETBROAD,DMCB,(R2),BRDDTS                                        
         GOTO1 DATCON,DMCB,(0,BRDDTS+6),(X'20',TSERVDT)                         
****     MVC   TSERVDT,BRDDTS+6   END DATE = MONTH OF SERVICE                   
*                                                                               
         L     R5,CHAGROS                                                       
         XC    CHACOMD,CHACOMD                                                  
         MVI   DMCB+23,C'Y'        SET $00 COMMISSION FLAG                      
         GOTO1 DOCOMM,DMCB,(R2),(R5),CHACOMD,CHACOMR,1                          
*                                                                               
         CLI   RECALC,3              SPECIAL BLOCK FOR 1 ORDERED EL             
         BNE   GETAMT20              AND 1 INV EL BOTH IN SAME WEEK             
         MVC   ABSGROS(4),CHAGROS                                               
         MVC   ABSCOMD(4),CHACOMD                                               
         MVC   ABSCOMR(4),CHACOMR                                               
         B     GETAMT29                                                         
GETAMT20 EQU   *                                                                
         CLI   RECALC,2                                                         
         BE    GETAMT22                                                         
         CLI   RECALC,5                                                         
         BNE   GETAMT24                                                         
GETAMT22 L     R5,ORDAMT                                                        
         XC    ORDAMT(4),ORDAMT                                                 
         B     GETAMT28                                                         
GETAMT24 EQU   *                                                                
         CLI   RECALC,1                                                         
         BE    GETAMT25                                                         
         CLI   RECALC,6                                                         
         BNE   GETAMT26                                                         
GETAMT25 EQU   *                                                                
         L     R5,CUASATIN(R4)     CURRENT AS AT INVOICED                       
         B     GETAMT28                                                         
GETAMT26 EQU   *                                                                
         L     R5,TOTORD(R4)       TOTAL ORDERED                                
         TM    FLAG6(R2),X'01'                                                  
         BO    GETAMT28                                                         
         A     R5,CUASATIN(R4)                                                  
GETAMT28 EQU   *                                                                
         ST    R5,ABSGROS                                                       
         XC    ABSCOMD,ABSCOMD    FOR COMMISSION AMT                            
         MVI   DMCB+23,C'Y'        SET $00 COMMISSION FLAG                      
         GOTO1 DOCOMM,DMCB,(R2),(R5),ABSCOMD,ABSCOMR,1                          
GETAMT29 EQU   *                                                                
*                                                                               
         CLI   TTRANST,C'E'       DO ACTUAL/INVOICE SEPERATLY                   
         BE    GETAMT40                                                         
*                                                                               
         CLI   RECALC,0            RECALC FLAG ON?                              
         BE    GETAMT30                                                         
         CLI   RECALC,5            SPECIAL ORDER AND ACT SAME WEEK              
         BE    GETAMT30             WITH MULTIPLE ORDERED ELEMENTS              
         CLI   RECALC,2                                                         
         BNE   GETAMT40            NO                                           
GETAMT30 EQU   *                                                                
         OC    ABSGROS,ABSGROS     ANY ABSOLUTE GROSS?                          
         BNZ   GETAMT32            YES - CHECK NO FURTHER                       
         OC    ABSCOMD,ABSCOMD     ANY ABSOLUTE COMM $?                         
*                                  REALLY SHOULDN'T BE: NO GROSS                
         BNZ   GETAMT32            YES                                          
         OC    ABSCOMR,ABSCOMR     ANY ABSOLUTE COMM RATE?                      
         BNZ   GETAMT32            YES                                          
         MVC   ABSCOMR,CHACOMR     SET COMM RATES EQUAL                         
GETAMT32 EQU   *                                                                
         L     R1,CHAGROS                                                       
         S     R1,ABSGROS                                                       
         ST    R1,ABSGROS                                                       
         L     R1,CHACOMD                                                       
         S     R1,ABSCOMD                                                       
         ST    R1,ABSCOMD                                                       
*                                                                               
         L     R1,CHAGROS                                                       
         L     R2,CHACOMD                                                       
         L     R3,ABSGROS                                                       
         L     R4,ABSCOMD                                                       
         L     R5,ABSCOMR                                                       
         ST    R1,ABSGROS                                                       
         ST    R2,ABSCOMD                                                       
         ST    R3,CHAGROS                                                       
         ST    R4,CHACOMD                                                       
         ST    R5,CHACOMR                                                       
GETAMT40 EQU   *                                                                
         OC    CHACOMR,CHACOMR     ANY COMMISSION RATE?                         
         BNZ   GETAMT42            YES                                          
         OC    ABSCOMD,ABSCOMD     ANY ABSOLUTE COMM $?                         
         BZ    GETAMT42            NO  - NO RATE NEEDED                         
         MVC   CHACOMR,ABSCOMR     YES - USE CHANGED COMM RATE                  
GETAMT42 EQU   *                                                                
         GOTO1 EDITOUT,DMCB,ABSGROS,TGROSS,9,0                                  
         GOTO1 EDITOUT,DMCB,ABSCOMD,TCOMMAT,9,0                                 
         GOTO1 EDITOUT,DMCB,CHAGROS,TCHGROSS,9,0                                
         GOTO1 EDITOUT,DMCB,CHACOMD,TCHCOMAT,9,0                                
         GOTO1 EDITOUT,DMCB,CHACOMR,TCOMMRT,6,1                                 
*                                                                               
GETAMTX  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
* THIS PROCEDURE IS USED FOR THE MONTHLY TAPE ONLY                              
GETRANST NTR1                                                                   
         MVI   TTRANST,C'E'       NO MATCHING X'04' FOUND                       
         GOTO1 DATCON,DMCB,(0,0(R2)),(3,COMPDT3)                                
*                                                                               
         LA    R1,RCONELEM                                                      
         SR    R0,R0                                                            
GETRAN10 ICM   R0,1,1(R1)                                                       
         BZ    GETRAN99                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    GETRAN99                                                         
         CLI   0(R1),4            IS THERE AN '04' ELEM?                        
         BNE   GETRAN10                                                         
         CLC   COMPDT3(2),RCONSTYR-RCONSTEL(R1)   YEAR&MONTH OF SERVICE         
         BNE   GETRAN10                                                         
*                                                                               
         MVI   TTRANST,C'A'       YES- HAVE MATCH                               
*                                                                               
GETRAN99 EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        RPTDONE --- FINISH THE REPORT                                          
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         CLOSE (INTFILE,)            CLOSE INTEREP FILE                         
         SPACE 1                                                                
         MVC   P+1(22),=CL22'CONTRACTS PROCESSED ='                             
         EDIT  CONCNT,(7,P+24),ZERO=NOBLANK                                     
         MVC   P+35(40),=CL40'INTERFACE RECORDS WRITTEN TO THE TAPE ='          
         EDIT  PUTCNT,(5,P+77),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(24),=CL24'CHANGED GROSS DOLLARS ='                           
         ICM   R2,15,RTOTCHGR+4                                                 
         EDIT  (R2),(11,P+26),2,FLOAT=-,ZERO=NOBLANK                            
         MVC   P+50(30),=CL30'CHANGED COMMISSION DOLLARS ='                     
         ICM   R2,15,RTOTCHCM+4                                                 
         EDIT  (R2),(11,P+82),2,FLOAT=-,ZERO=NOBLANK                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(25),=CL25'ABSOLUTE GROSS DOLLARS ='                          
         ICM   R2,15,RTOTABGR+4                                                 
*                                                                               
*   TEST:  WHEN VALUE OVERFLOWS POSITIVE REGISTER VALUE                         
**                                                                              
**       L     R2,=F'3129796800'                                                
**                                                                              
*   TEST END                                                                    
*                                                                               
         LTR   R2,R2               NUMBER OVERFLOW?                             
         BM    RPTD0080            YES - ALTER IT                               
         EDIT  (R2),(11,P+26),2,FLOAT=-,ZERO=NOBLANK                            
         B     RPTD0160                                                         
RPTD0080 EQU   *                                                                
         LR    R3,R2                                                            
         SR    R2,R2                                                            
         D     R2,=F'100'                                                       
         EDIT  (R3),(11,P+23),FLOAT=-,ZERO=NOBLANK                              
RPTD0160 EQU   *                                                                
         MVC   P+50(31),=CL31'ABSOLUTE COMMISSION DOLLARS ='                    
         ICM   R2,15,RTOTABCM+4                                                 
         EDIT  (R2),(11,P+82),2,FLOAT=-,ZERO=NOBLANK                            
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(18),=CL18'*** END OF RUN ***'                                
         GOTO1 REPORT                                                           
*                                                                               
RPTDGOOD EQU   *                                                                
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
         OC    000(130,R6),SPACES                                               
         OC    130(130,R6),SPACES                                               
         OC    260(040,R6),SPACES                                               
*                                                                               
         CLC   TCHGROSS-TAPED(TADVC-TCHGROSS,R6),SPACES                         
         BE    DPUT10                                                           
         CLC   TCHGROSS-TAPED(TADVC-TCHGROSS,R6),=36C'0'                        
         BNE   DPUT20                                                           
*                                                                               
DPUT10   EQU   *                                                                
         XC    ABSGROS,ABSGROS                                                  
         XC    ABSCOMD,ABSCOMD                                                  
         XC    CHAGROS,CHAGROS                                                  
         XC    CHACOMD,CHACOMD                                                  
         B     DPUTGOOD                                                         
*                                                                               
DPUT20   EQU   *                                                                
         BAS   RE,READSUB         READ REST OF INFO FOR PUT REC                 
*                                                                               
         PUT   (R8),(R6)                                                        
*                                                                               
         ICM   R1,15,PUTCNT                                                     
         A     R1,=F'1'                                                         
         STCM  R1,15,PUTCNT                                                     
*                                                                               
*    IF REQUESTOR = 'SPEC PRINT' CALL MODULE TO DISPLAY TAPE                    
*       RECORD ON PRINTOUT                                                      
*                                                                               
         CLC   =C'SPEC PRINT',QUESTOR                                           
         BNE   DPUTGOOD                                                         
         GOTO1 =A(DUMPTAPE),DMCB,(RC)                                           
*                                                                               
DPUTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                             +300                                              
*                                                                               
*        READ ADVERTISER/AGENCY/PRODUCT/STATION FOR OUTPUT REC                  
*                                                                               
READSUB  NTR1                                                                   
*                                                                               
         MVC   TADVC,RCONKADV               ADVERTISER CODE                     
         MVC   TADVNM,=C'NAME NOT FOUND'     DEFAULT                            
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR ADVERTISER                      
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),TADVC                                                  
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'08'                                                        
         BNE   RDSUB10                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUB10                                                          
         BAS   RE,GETADV                                                        
         MVC   TADVNM,RADVNAME    MOVE IN ADVERTISER NAME                       
RDSUB10  DS    0H                                                               
         MVC   TAGYC,RCONKAGY                AGENCY CODE                        
         MVC   TAGYCTY,RCONKAOF              AGENCY CITY CODE                   
         MVC   TAGYNM,=C'NAME NOT FOUND'     DEFAULT                            
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),TAGYC                                                  
         MVC   KEY+23(2),TAGYCTY                                                
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'0A'                                                        
         BNE   RDSUB20                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUB20                                                          
         BAS   RE,GETAGY                                                        
         MVC   TAGYNM,RAGYNAM1                                                  
         MVI   KEY,X'1A'           ACCESS 2NDARY AGENCY REC FOR TERR            
         GOTO1 HIGH                                                             
         CLI   KEY,X'1A'                                                        
         BNE   RDSUB20                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUB20                                                          
         BAS   RE,GETAGY                                                        
         LA    RF,RAGYREC                                                       
         USING RAGY2REC,RF                                                      
         OC    RAGY2TER,RAGY2TER   ANY CODE THERE?                              
         BZ    RDSUB20             NO  - DON'T INSERT BINARY ZERO               
         MVC   TTERRIT,RAGY2TER    INSERT TERRITORY CODE                        
*                                                                               
         DROP  RF                                                               
*                                                                               
RDSUB20  DS    0H                                                               
         CLC   RCONPRD,SPACES      IS THERE A PROD CODE?                        
         BNE   RDSUB35             YES--- GO READ PROD RECORD                   
*                                                                               
         MVC   TPRODCD,SPACES                                                   
         LA    R1,RCONELEM                                                      
         SR    R0,R0                                                            
RDSUB30  ICM   R0,1,1(R1)                                                       
         BZ    RDSUBOK                                                          
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    RDSUBOK                                                          
         CLI   0(R1),5            FIND PRODUCT NAME ELEMENT                     
         BNE   RDSUB30                                                          
         MVC   TPRODDS,RCONEXPR-RCONEXEL(R1)     PRODUCT NAME                   
         B     RDSUBOK                                                          
*                                                                               
RDSUB35  MVC   TPRODCD,RCONPRD              PRODUCT CODE                        
         XC    KEY,KEY            READ PRODUCT RECORD                           
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),TPRODCD                                                
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'09'                                                        
         BNE   RDSUBOK                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUBOK                                                          
         BAS   RE,GETPROD                                                       
         MVC   TPRODDS,RPRDNAME               PRODUCT DESCRIPTION               
         MVC   TNETCON,RPRDNET#               PRODUCT NET NUMBER                
         OC    TNETCON,SPACES                                                   
         LA    R1,RPRDELEM                                                      
         ZIC   RF,RPRDELLN                                                      
         AR    R1,RF                                                            
         CLI   0(R1),X'02'                                                      
         BNE   RDSUBOK                                                          
         MVC   TPOINT(3),RPRDNPNT-RPRDNELM(R1)                                  
         OC    TPOINT(3),SPACES                                                 
         SPACE                                                                  
RDSUBOK  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
         EJECT                                                                  
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
         B     LINKFILE                                                         
*                                                                               
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
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
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR*************'           
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
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
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECOMRTN                                                       
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*  TAPE FOR INTEREP DATA                                                        
         SPACE 1                                                                
INTFILE  DCB   DDNAME=INTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00300,                                            X        
               BLKSIZE=30000,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    F                                                                
SAVEDRB  DS    F                                                                
ORDAMT   DS    F                   ORDERED DOLLARS UP TO ACT WEEK               
*                                                                               
CHAGROS  DS    F                   CHANGED GROSS                                
CHACOMD  DS    F                   CHANGED COMM DOLLARS                         
CHACOMR  DS    F                   CHANGED COMM RATE                            
ABSGROS  DS    F                   ABSOLUTE GROSS                               
ABSCOMD  DS    F                   ABSOLUTE COMM DOLLARS                        
ABSCOMR  DS    F                   ABSOLUTE COMM RATE                           
*                                                                               
RTOTCHGR DS    D                   RUN TOTAL $ - CHANGED GROSS                  
RTOTCHCM DS    D                   RUN TOTAL $ - CHANGED COMMISSION             
RTOTABGR DS    D                   RUN TOTAL $ - ABSOLUTE GROSS                 
RTOTABCM DS    D                   RUN TOTAL $ - ABSOLUTE COMMISSION            
*                                                                               
PUTCNT   DS    F                                                                
CONCNT   DS    F                                                                
*                                                                               
VDEMOCON DS    A                   * NEW CODE TO FIX DEMOCON                    
*                                                                               
RECALC   DS    CL1                 RECALC FLAG, 0=NO                            
ORDCNT   DS    CL1                                                              
COMPDT   DS    CL2                                                              
COMPDT3  DS    CL3                                                              
ENDDT    DS    CL3                                                              
ASATDT   DS    CL2                                                              
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
WORK2    DS    CL50               ANOTHER WORK AREA FOR DEMO NAMES              
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
DEMBLOCK DS    CL256              ADVERTISED LEN OF DBLOCK                      
         DS    CL8                DBLOCK SLOP-OVER                              
WRK      DS    CL300              LENGTH OF RECORD TO BE WRITTEN                
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE RECOMTAB                                                       
       ++INCLUDE RECOMIO                                                        
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REREP3G03                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  REMONARCHD                                                                   
*  REGENALL1                                                                    
*  REREPWORKD                                                                   
*  REREPMODES                                                                   
*  REGENCOM                                                                     
*  DEDBLOCK                                                                     
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE REMONARCHD                                                     
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
COMDSCT  DSECT                                                                  
       ++INCLUDE REGENCOM                                                       
AGY2DSCT DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DSECT TO COVER TAPE RECORDS                                                   
         SPACE 1                                                                
TAPED    DSECT                                                                  
TAPELN   DS    0CL300                                                           
TREP     DS    CL02           +000 REP                                          
TSTAT    DS    CL05           +002 STATION                                      
TINTFACE DS    CL10           +007 STATION INTERFACE CODE                       
TOFFC    DS    CL02           +017 CONTRACT OFFICE CODE                         
TOFFCNM  DS    CL20           +019 CONTRACT OFFICE NAME                         
TCOMMRT  DS    CL06           +039 COMMISSION RATE                              
TSERVDT  DS    CL06           +045 MONTH OF SERVICE DATE                        
TTRANDT  DS    CL06           +051 TRANSACTION DATE                             
TCHGROSS DS    CL09           +057 CHANGED GROSS AMT                            
TCHCOMAT DS    CL09           +066 CHANGED COMMISSION AMT                       
TGROSS   DS    CL09           +075 ABSOLUTE GROSS                               
TCOMMAT  DS    CL09           +084 ABSOLUTE COMMISSION AMOUNT                   
TADVC    DS    CL04           +093 ADVERTISER CODE                              
TADVNM   DS    CL20           +097 ADVERTISER NAME                              
TAGYC    DS    CL04           +117 AGENCY CODE                                  
TAGYCTY  DS    CL02           +121 AGENCY CITY CODE                             
TAGYNM   DS    CL20           +123 AGENCY NAME                                  
TTYPE    DS    CL02           +143 TYPE                                         
TCON     DS    CL08           +145 CONTRACT NUMBER                              
TTRANST  DS    CL01           +153 TRANSACTION STATUS                           
TSALC    DS    CL03           +154 SALESPERSON CODE                             
TSALNM   DS    CL20           +157 SALESPERSON NAME                             
*********TTEAMC   DS    CL02           +177 TEAM CODE                           
*********TTEAMNM  DS    CL10           +179 TEAM NAME                           
*********TTEAMDN  DS    CL10           +189 TEAM DIVISION NAME                  
TDEVSP   DS    CL03           +177 DEVELOPMENTAL SALESPERSON CODE               
TDEVCTYP DS    CL02           +180 DEVELOPMENTAL CONTRACT TYPE                  
TTERRIT  DS    CL02           +182 TERRITORY CODE                               
         DS    CL15           +184 SPARE                                        
TCATCD   DS    CL05           +199 CATEGORY CODE                                
TPRODCD  DS    CL03           +204 PRODUCT CODE                                 
TPRODDS  DS    CL20           +207 PRODUCT DESCRIPTION                          
TDEMO    DS    CL20           +227 DEMOGRAPHICS                                 
TNETCON  DS    CL08           +247 NETWORK CONTRACT NUMBER                      
TPOINT   DS    CL03           +255 POINTPERSON CODE                             
TMARKET  DS    CL20           +258 STATION MARKET NAME                          
TSRANK   DS    CL01           +278 STATION RANK                                 
TOFFREG  DS    CL02           +279 OFFICE REGION                                
TOPTION1 DS    CL01           +281 REQUEST OPTION1, 'W' OR 'M'                  
         DS    CL18           +282 **** SPARE ****                              
         EJECT                                                                  
*                                                                               
*        RUNTOT --- KEEP RUNNING TOTAL DOLLARS                                  
*                                                                               
         CSECT                                                                  
RUNTOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,RTOTLIST                                                      
         LA    R2,4                DO FOUR TIMES                                
*                                                                               
RTOT10   EQU   *                                                                
         L     R3,0(R1)            A(THIS CONTRACT AMOUNT)                      
         L     R4,4(R1)            A(RUNNING TOTAL AMOUNT)                      
         L     R5,0(R3)            THIS CONTRACT AMOUNT                         
         A     R5,4(R4)            ADD IN RUNNING TOTAL                         
         BNO   RTOT20              CHECK FOR OVERFLOW                           
         L     RF,0(R4)                                                         
         A     RF,=F'1'                                                         
         ST    RF,0(R4)                                                         
RTOT20   EQU   *                                                                
         ST    R5,4(R4)                                                         
*                                                                               
         XC    0(4,R3),0(R3)                                                    
*                                                                               
         LA    R1,8(R1)                                                         
         BCT   R2,RTOT10                                                        
*                                                                               
         XC    TGROSS,TGROSS                                                    
         XC    TCOMMAT,TCOMMAT                                                  
         XC    TCOMMRT,TCOMMRT                                                  
         XC    TCHGROSS,TCHGROSS                                                
         XC    TCHCOMAT,TCHCOMAT                                                
*                                                                               
RTOTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         XIT1                                                                   
*                                                                               
RTOTLIST DC    AL4(ABSGROS),AL4(RTOTABGR)                                       
         DC    AL4(ABSCOMD),AL4(RTOTABCM)                                       
         DC    AL4(CHAGROS),AL4(RTOTCHGR)                                       
         DC    AL4(CHACOMD),AL4(RTOTCHCM)                                       
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159REREP3H02S05/01/02'                                      
         END                                                                    
