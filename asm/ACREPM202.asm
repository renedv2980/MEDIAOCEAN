*          DATA SET ACREPM202  AT LEVEL 051 AS OF 01/28/13                      
*PHASE ACM202A                                                                  
*&&US                                                                           
*INCLUDE DMDMGRL                                                                
*&&                                                                             
APG      TITLE 'APG PROCESSING SYSTEM'                                          
***********************************************************************         
* GET PROCESSING TIMES/ SET FILE TYPES                                *         
***********************************************************************         
         SPACE 1                                                                
ACM202   CSECT                                                                  
         PRINT NOGEN                                                            
ACM2     NMOD1 0,**ACM202,R9,R8,R7                                              
         L     RC,0(R1)                                                         
         USING ACWORKD,RC                                                       
         ICM   RA,15,SPACEND                                                    
         BNZ   *+12                                                             
         BAS   RE,LOAD             FIRST TIME LOAD PHASES                       
         B     *-12                                                             
         USING MAND,RA                                                          
         CLI   MODE,PROCSPEC                                                    
         BE    XIT                                                              
         CLI   RCTRACE,YES         IS THE TRACE SWITCH ON                       
         BNE   M200                                                             
         AP    TRCCNT,=P'1'        COUNT RECORD TRACES                          
         CP    TRCCNT,TRCMAX       HAVE WE PRINTED ENOUGH                       
         BNH   M200                                                             
         MVI   RCTRACE,NO          TURN OFF THE TRACE                           
*                                                                               
         USING MASTD,R5                                                         
M200     L     R5,ADMASTC                                                       
         MVC   UPSI,MCUPSI         TEST/TRACE OPTIONS                           
         TM    UPSI,TIME           ELASPED TIME BY ROUTINE                      
         BNO   M201                                                             
         OC    ADTIME,ADTIME       DID I EXIT WITHOUT SAVING TIME               
         BZ    *+6                                                              
         DC    H'0'                WRONG EXIT                                   
*                                                                               
         LA    R3,ASIDFLD                                                       
         EXTRACT (R3),'S',FIELDS=(ASID)                                         
         L     R2,ASIDFLD                                                       
         LOCASCB ASID=(R2)                                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         ST    R0,TIMIN                                                         
         S     R0,TIMOUT           GET TIME IN MONACC                           
         A     R0,TMONACC                                                       
         ST    R0,TMONACC          CUMULATIVE TIME                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET ROUTINE FOR CURRENT MODE                                        *         
***********************************************************************         
         SPACE 1                                                                
M201     MVI   FCRESET,C'Y'                                                     
         LA    R0,SPACEND+4                                                     
         ST    R0,AMNKEY           A(LAST KEY FOR SOME HOOKS)                   
         L     RF,AMONACC                                                       
         LA    RF,ACMACDIR-ACMD(RF)                                             
         GOTO1 DATAMGR,DMCB,DMKEY,(RF),(R0)                                     
*                                                                               
         CLI   SE,0                TEST LOCAL DATAMGR SET                       
         BE    *+26                                                             
         MVI   FCRESET,C'N'                                                     
         XC    LDMGR,DATAMGR       SWITCH TO LOCAL DATAMGR                      
         XC    DATAMGR,LDMGR                                                    
         XC    LDMGR,DATAMGR                                                    
*                                                                               
         LA    R1,LASTMODE                                                      
         CLC   MODE,LASTMODE       IF IT'S SAME MODE                            
         BE    M205                OK TO PROCESS                                
         LA    RF,MODETAB          ELSE, GO THRU TABLE                          
*                                                                               
M203     CLC   MODE,0(RF)          MATCH THE MODE                               
         BE    M204                                                             
         CLI   0(RF),X'FF'         END OF TABLE, TAKE IT                        
         BE    M204                                                             
         LA    RF,L'MODETAB(RF)                                                 
         B     M203                                                             
*                                                                               
M204     MVC   LASTMODE,0(RF)                                                   
M205     MVC   ADTIME,4(R1)        A(TIME TABLE)                                
         SR    RF,RF                                                            
         ICM   RF,3,1(R1)          GET A(ROUTINE)                               
         AR    RF,RB                                                            
         BR    RF                  AND AWAY WE GO                               
         TITLE 'BASE - FIRST FOR RUN'                                           
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RNF00    L     R5,ATABLOW          A(TABLE DEFINITIONS)                         
         MVI   BYTE,0              BYTE=0 FOR MAIN, H FOR HIGH                  
*                                                                               
RNF02    SR    R0,R0                                                            
         A     R0,8(R5)            ADD LENGTH OF ALL TABLES                     
         LA    R5,16(R5)                                                        
         CLI   0(R5),X'FF'         END OF TABLES DEFINITIONS                    
         BNE   *-12                                                             
*MNNEW   CLI   BYTE,C'H'                                                        
*MNNEW   BE    RNF02A                                                           
         ST    R0,MAINLEN          SAVE LENGTH                                  
         GETMAIN R,LV=(0)          GET SOME STORAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                NOT ENOUGH STORAGE AVAILABLE                 
         ST    R1,MAINBGN          SAVE START OF TABLE                          
         L     R5,ATABLOW                                                       
*MNNEW   B     RNF03                                                            
*                                                                               
RNF02A   DS    0H                                                               
*MNNEW   GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
*MNNEW   LTR   RF,RF               TEST IF STORAGE ACQUIRED                     
*MNNEW   BZ    *+6                 YES                                          
*MNNEW   DC    H'0'                                                             
*MNNEW   L     R5,ATABHI                                                        
*                                                                               
RNF03    DS    0H                                                               
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
*                                                                               
RNF04    LR    R2,R1               SAVE A(START OF BUFFER)                      
         SR    R3,R3                                                            
         ICM   R3,3,12(R5)         DISPLACEMENT INTO MAND                       
         AR    R3,RA                                                            
*                                                                               
RNF05    MVC   0(8,R2),0(R5)       TABLE NAME TO START OF AREA                  
         LA    R2,8(R2)            R2 = A(THE DATA AREA)                        
         ST    R2,0(R3)            A(THIS AREA INTO MAND)                       
         L     R3,8(R5)            LENGTH OF THIS AREA                          
         SH    R3,=H'8'            ADJUST FOR NAME                              
*                                                                               
RNF07    SR    RE,RE                                                            
         IC    RF,14(R5)           PADDING CHARACTER                            
         SLL   RF,24                                                            
         MVCL  R2,RE               INITIALIZE THE AREA                          
         L     R3,8(R5)            R3 = LENGTH OF THIS AREA                     
         AR    R1,R3               R1 TO NEXT AREA                              
         LA    R5,16(R5)                                                        
         CLI   0(R5),X'FF'         END OF LIST                                  
         BNE   RNF03                                                            
*MNNEW   CLI   BYTE,C'H'           HIGH CORE ALLOCATED                          
*MNNEW   BE    RNF17               ALL DONE                                     
*                                                                               
RNF15    DS    0H                                                               
*MNNEW   L     R5,ADMASTC                                                       
*MNNEW   USING MASTD,R5                                                         
*MNNEW   MVC   MCUSRDMP+8(4),MAINBGN START OF STORAGE AREA                      
*MNNEW   STCM  R1,15,MCUSRDMP+12     SET END OF STORGE AREA                     
*MNNEW   L     R5,ATABHI             NOW DO HIGH                                
*MNNEW   MVI   BYTE,C'H'                                                        
*MNNEW   B     RNF02                                                            
*                                                                               
RNF17    DS    0H                                                               
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         MVC   DUB,=CL8'T00A7D'    LOAD IN TSAROFF                              
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4(4),DMCB+4    TEST LOAD WAS OK                             
         BNZ   *+6                 YES                                          
         DC    H'0'                CAN'T LOAD TSAROFF                           
         MVC   TSAROFF,4(R1)       SAVE A(TSAROFF)                              
*                                                                               
         MVI   RNSW,0              INIT RUN SWITCH                              
         MVI   RQSW,0              AND REQ SWITCH                               
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'164'                                                 
         MVC   HEADHOOK,AHEAD      A(HEAD HOOK)                                 
         ST    RC,BASERC           A(ACWORKC FOR HOOK)                          
         ZAP   FOHR,=P'0'          INIT FIXED OVERHEAD RATE                     
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   SYSEQU,ACMSYS       ACMSUSA OR ACMSEUR                           
         MVC   ADDCB,ACMADDIR                                                   
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         L     R1,AZRO             R1 = A(ZERO LINE)                            
         LA    R0,NAC              R0 = NUMBER OF ACCUMULATOR COLUMNS           
         ZAP   0(PFL,R1),=P'0'     CLEAR A LINE OF ZEROS                        
         LA    R1,PFL(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 ADDICTAT,DMCB,C'LU  ',DICI,DICO                                  
         B     XXIT                                                             
         DROP  R4,R5,RF                                                         
         TITLE 'BASE - FIRST FOR REQUEST'                                       
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
RQF00    TM    RQSW,RQBGN          BEGIN FIRST REQUEST                          
         BO    RQF01                                                            
         MVI   CLRLV,LSREQ                                                      
         BAS   RE,CLR              CLEAR ALL ACCUM LINES                        
         OI    RQSW,RQBGN+RQFLG    NEXT TIME PROCESS REQUESTS                   
         MVI   FCRDACC,NO          THIS TIME NO PROCESSING                      
         MVI   RCREQREP,NO         NO REQUEST REPORT                            
*                                                                               
         OC    LDMGR,LDMGR         TEST SECOND DATAMGR                          
         BZ    XXIT                                                             
         CLI   SE,0                                                             
         BNE   XXIT                                                             
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         MVC   SE,MCIDSENO         SET SE NUMBER FOR LOCAL DATAMGR              
         GOTO1 LDMGR,DMCB,OPEN,ACFILE,ACFILEL    OPEN LOCAL COPY                
         XC    LDMGR,DATAMGR       SWITCH TO LOCAL DATAMGR                      
         XC    DATAMGR,LDMGR                                                    
         XC    LDMGR,DATAMGR                                                    
         B     XXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CONTROL MULTIPLE LEDGERS/SYSTEMS                                    *         
***********************************************************************         
         SPACE 1                                                                
RQF01    TM    RQSW,RQFLG          IS IT THE FIRST LEDGER                       
         BNO   RQF03               NO, 2ND, 3RD... LEDGER                       
         GOTO1 AINIT               FIRST TIME INITIALIZATION                    
         TM    RQSW,RQERR          ERROR ON REQUEST CARD                        
         BO    XXIT                                                             
         NI    RQSW,ALL-RQFLG      SET FOR NEXT LEDGER                          
*                                                                               
         NI    SRTSW,ALL-SRTEO     TURNOFF EOF                                  
         LH    R2,SRTLNGTH         LENGTH OF SORT                               
         LA    R4,SORTCARD                                                      
         LA    R6,15(R4)                                                        
         EDIT  (R2),(3,0(R6)),FILL=0                                            
         LH    R2,RECLNGTH         LENGTH OF RECORD                             
         LA    R5,RECCARD                                                       
         LA    R6,21(R5)                                                        
         EDIT  (R2),(3,0(R6)),FILL=0                                            
         GOTO1 ADSORTER,DMCB,(R4),(R5),0                                        
*                                                                               
         MVI   SLOPTN,0            SUPERLEDGER CONTROL SWITCH                   
         MVI   HPSW,0              CLEAR PERCENT SWITCH                         
         L     RF,ACNL                                                          
         CLC   CONLEDG,SPACES      ANY SUPERLEDGER                              
         BE    *+6                                                              
         BASR  RE,RF               INITIALIZE CONTROL LEDGER                    
         BAS   RE,SETRL            INITIALIZE ROWLIST LEDGER                    
         CLI   HOOKMODE,0          DOES HOOK WANT MONACC MODES                  
         BE    *+8                                                              
         BAS   RE,GOHK             GO TO THE HOOK                               
*                                                                               
RQF03    MVI   LGSW,0              SET LEDGER CONTROL                           
         TM    RNSW,RNNC           NEW COSTING                                  
         BNO   *+8                                                              
         OI    LGSW,LGNC           SET NEW COST FOR LEDGER                      
         CLC   QUNIT(2),COSTUL TEST CLIENT COSTING LEDGER                       
         BNE   *+8                                                              
         OI    LGSW,LGCL                                                        
         CLC   QUNIT(2),PROJUL     TEST PROJECT CONTROL LEDGER                  
         BNE   *+8                                                              
         OI    LGSW,LGPJ                                                        
         CLC   QUNIT(2),CLIAUL     TEST CLIENT ANALYSIS LEDGER                  
         BNE   *+8                                                              
         OI    LGSW,LGTE                                                        
         CLC   QUNIT(2),PERAUL     TEST STAFF ANALYSIS LEDGER                   
         BNE   *+8                                                              
         OI    LGSW,LGTE                                                        
         CLC   QUNIT(2),PERSUL     TEST PERSONNEL LEDGER                        
         BNE   RQF04                                                            
         OI    LGSW,LGPE                                                        
*&&UK*&& B     RQF04                                                            
*                                                                               
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         LR    R2,R1               CLEAR PERCALL BLOCK                          
         LA    R3,PERLNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL                                                 
         OI    PERFLAGS,PERRECCK                                                
         GOTO1 PERCALL             GET PERSON BLOCK                             
         TM    PERERR,PERNOREC     TEST NO RECORDS                              
         BO    *+8                                                              
         OI    LGSW,LGPR           USING PERSON RECORDS                         
         DROP  R1                                                               
*                                                                               
RQF04    SR    R0,R0               GET RELATIVE NUMBER FOR FIRST MONTH          
         SR    R1,R1                                                            
         IC    R0,MONLIST+4        YEAR                                         
         SRDL  R0,4                R0=DECADE, R1=YEAR                           
         MH    R0,=H'10'           X'A0' BECOMES X'100'                         
         SRL   R1,28               YEAR TO LOW ORDER                            
         AR    R0,R1               ADD YEAR TO DECADE                           
         MH    R0,=H'12'                                                        
         IC    R1,MONLIST+5        MONTH                                        
         CLI   MONLIST+5,X'09'     CONVERT MONTH TO BINARY                      
         BNH   *+8                                                              
         SH    R1,=H'6'                                                         
         AR    R0,R1                                                            
         STCM  R0,7,BASMNTH        SAVE BASE NUMBER                             
*                                                                               
         LA    R3,PEDTE                                                         
         BAS   RE,GRELMON          GET END MONTH                                
         SRL   R3,3                                                             
         STC   R3,RELEND           SAVE LAST RELATIVE ACCUM NUMBER              
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ACMHSTR,MONLIST+4   FIRST BUCKET(HISTORY) MONTH                  
         MVC   ACMHEND,MONLIST+214 LAST MONTH                                   
         TM    OPT1,FTD            FORCING TRANSACTION DATE?                    
         BO    RQF06               DON'T SET MONACC TRANSACTION FILTER          
         MVC   ACMTSTR,MONLIST+4   SAME FOR TRANSACTIONS                        
         MVI   ACMTSTR+2,1         FIRST DAY OF MONTH                           
         MVC   ACMTEND,MONLIST+214                                              
         MVI   ACMTEND+2,1         FIRST DAY OF MONTH, GET LAST DAY             
         GOTO1 DATCON,DMCB,(X'31',ACMTEND),(1,ACMTEND),(1,0)                    
         CLI   BBFSW,YES           IF WANT BBF SET TRANS. PRIOR TO STR          
         BNE   RQF05                                                            
         OI    RDSW,RDMWR          FORCE TRANS PRIOR TO MONLIST TBL             
         XC    ACMTSTR,ACMTSTR     ALL TRANS. PRIOR                             
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'    FIRST DAY OF MONTH                           
         GOTO1 DATCON,DMCB,(X'30',WORK),(1,ACMTEND),(5,0)                       
         MVI   FCSETBAL,YES                                                     
*                                                                               
RQF05    CLI   POSTMOS,YES         POSTING BY MOS?                              
         BNE   RQF06               DON'T SET MONACC TRANSACTION FILTER          
         XC    ACMTSTR,ACMTSTR     ALL TRANS. PRIOR                             
         MVC   ACMTEND,ACMEFFS     AND AFTER                                    
         OI    ACMINDS,ACMIMOSR                                                 
         MVC   ACMMSTR,MONLIST+4   FILTER BY MOS                                
         MVC   ACMMEND,MONLIST+214                                              
*                                                                               
RQF06    MVC   METHOD,ACMCAM       SET METHOD                                   
         MVC   MTHDNME,SPACES      CLEAR NAME                                   
         MVC   DKEY,SPACES         READ METHOD RECORD                           
         LA    R6,DKEY                                                          
         USING CMTRECD,R6                                                       
         MVI   CMTKTYP,CMTKTYPQ    TYPE                                         
         MVI   CMTKSUB,CMTKSUBQ                                                 
         MVC   CMTKCPY,RCCOMPFL    COMPANY                                      
         GOTO1 ADMGR,READ                                                       
         BNE   RQF07                                                            
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         BAS   RE,GETNAME          GET METHOD NAME                              
         MVC   MTHDNME,WORK                                                     
*                                                                               
RQF07    BAS   RE,GADTE            GET AGENCY ALLOCATION DATE                   
         MVI   FCRDACC,YES         SET READ CONTROL SWITCHES                    
         MVI   FCRDHIST,NO                                                      
         MVI   FCRDTRNS,NO                                                      
         MVI   FCGENBUK,NO                                                      
         TM    RDSW,RDGEN          PROCESS NEW STYLE BUCKETS                    
         BZ    *+8                                                              
         MVI   FCGENBUK,YES                                                     
         TM    RDSW,RDBKT          PROCESS BUCKETS                              
         BZ    *+8                                                              
         MVI   FCRDHIST,YES                                                     
         TM    RDSW,RDDTL+RDMWR    TEST READING TRANACTIONS                     
         BZ    XXIT                                                             
         MVI   FCRDTRNS,YES                                                     
         TM    LGSW,LGPE           TEST PERSON LEDGER                           
         BZ    XXIT                                                             
         TM    RNSW,RNTMS          TEST TMS                                     
         BZ    XXIT                                                             
         MVI   FCRDTIME,YES                                                     
         B     XXIT                                                             
         DROP  R2,R6                                                            
         TITLE 'BASE - FIRST FOR LEVEL A, B, C'                                 
***********************************************************************         
* FIRST FOR LEVEL A, B, C                                             *         
***********************************************************************         
         SPACE 1                                                                
LAF00    MVI   ACTSW,0             TURNOFF ALL ACTIVITY SWITCHES                
         BAS   RE,GADTE            GET ALLOCATION DATE FOR OFFICE               
         SR    RF,RF                                                            
         IC    RF,RELEND           RELATIVE NUMBER OF END MONTH                 
         SLL   RF,1                X 2                                          
         L     R1,AOFFDTE          OFFICE AS AT DATES                           
         AR    RF,R1                                                            
         MVC   ASOFDTE,0(RF)       AS OF DATE FOR END MONTH                     
         L     R6,ADLVASTA                                                      
         USING RSTELD,R6                                                        
         MVC   CURRCG1,RSTCOSTG    COST GROUP - LEVEL 1                         
         L     R6,ADHEIRA                                                       
         B     LFX                                                              
*                                                                               
LBF00    NI    ACTSW,ALL-(ACTLB+ACTLC+ACTLD)                                    
         L     R6,ADLVBSTA                                                      
         MVC   CURRCG2,RSTCOSTG     COST GROUP - LEVEL 2                        
         L     R6,ADHEIRB                                                       
         B     LFX                                                              
*                                                                               
LCF00    NI    ACTSW,ALL-(ACTLC+ACTLD)                                          
         L     R6,ADLVCSTA                                                      
         MVC   CURRCG3,RSTCOSTG     COST GROUP - LEVEL 3                        
         L     R6,ADHEIRC                                                       
         DROP  R6                                                               
*                                                                               
         USING ACTRECD,R6                                                       
LFX      MVI   CLRLV,LSACC+LSBUD                                                
         BAS   RE,CLR              CLEAR ALL ACCOUNT AND BUDGET LINES           
         MVI   TSRSTAT,0                                                        
         MVC   ACTACC,ACTKEY                                                    
         MVC   CURRACC,ACTACC                                                   
         BAS   RE,GETNAME                                                       
         MVC   CURRNAME,WORK                                                    
         MVC   CURRNUM,ACTACC+3    PRESET NUMBER=ACCOUNT                        
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   CURRFLT,ACMFLTS                                                  
         TM    RDSW,RDBUD          READ BUDGETS                                 
         BNO   *+8                                                              
         BAS   RE,BDGT             GET THE BUDGETS                              
         B     XXIT                                                             
         DROP  R6,RF                                                            
         TITLE 'BASE - PROCESS ACCOUNT'                                         
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PA00     ZAP   BBF,=P'0'                                                        
         TM    RDSW,RDBKT          PROCESS BUCKETS                              
         BZ    *+8                                                              
         MVI   FCRDHIST,YES                                                     
         MVI   ACSW,0              INIT THE ACCOUNT LEVEL SWITCHES              
         NI    ACTSW,ALL-ACTLD     ACTIVITY SWITCH                              
         MVI   CLRLV,LSACC                                                      
         XC    HIRE,HIRE                                                        
         MVI   TERM,ALL                                                         
         MVI   TSRSTAT,0                                                        
         BAS   RE,CLR              CLEAR ALL ACCOUNT LINES                      
         TM    LGSW,LGPE                                                        
         BNO   *+10                                                             
         GOTO1 APSD                POST BILLING RATES                           
         L     R6,ADACC                                                         
         USING ACTRECD,R6                                                       
         MVC   ACTACC,ACTKEY       C/U/L/ACCOUNT                                
         MVC   CURRACC,ACTKEY                                                   
         BAS   RE,GETNAME          GET ACCOUNT NAME                             
         MVC   CURRNAME,WORK                                                    
         MVC   CURRNUM,ACTACC+3    ACCOUNT CODE                                 
         DROP  R6                                                               
*                                                                               
         TM    RNSW,RNNC           NEW COSTING                                  
         BNO   *+8                                                              
         BAS   RE,GPHR             GET PERSONAL HOURLY RATES                    
         GOTO1 ATOTH               GET TOTAL HOURS                              
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   CURRFLT,ACMFLTS     FILTERS 1,2,3,4 AND 5                        
         DROP  RF                                                               
*                                                                               
         USING RSTELD,R6                                                        
         L     R6,ADACCSTA                                                      
         MVC   CURRCG,RSTCOSTG     COSTING GROUP                                
         CLI   CURRCG,C' '         FROM ACCOUNT                                 
         BH    *+10                                                             
         MVC   CURRCG,CURRCG3      OR LEVEL C                                   
         CLI   CURRCG,C' '                                                      
         BH    *+10                                                             
         MVC   CURRCG,CURRCG2      OR LEVEL B                                   
         CLI   CURRCG,C' '                                                      
         BH    *+10                                                             
         MVC   CURRCG,CURRCG1      OR LEVEL A                                   
         MVC   CURRANAL,RSTANAL    ANALYSIS CODE                                
         MVC   CURRNUM,SPACES      CLEAR SPECIAL NUMBER                         
         MVC   CURRBBFD,SPACES     BALANCE FRWD DATE                            
         XC    CURRHIRE,CURRHIRE   SET HIRED TO LOWEST DATE                     
         MVC   CURRTERM,=24X'FF'   SET TERM TO HIGHEST DATE                     
         DROP  R6                                                               
*                                                                               
         L     R6,ADACC                                                         
         LA    R6,ACCORFST(R6)                                                  
         SR    R0,R0                                                            
*                                                                               
         USING OTHELD,R6                                                        
PA02     CLI   0(R6),X'23'                                                      
         BNE   PA03                                                             
         MVC   CURRNUM(9),OTHNUM  SPECIAL NUMBER                                
         B     PA10                                                             
         DROP  R6                                                               
*                                                                               
         USING APOELD,R6                                                        
PA03     CLI   0(R6),X'33'         PEELED DATE                                  
         BNE   PA05                                                             
         OC    APOLBDT,APOLBDT                                                  
         BZ    PA10                                                             
         GOTO1 DATCNV,DMCB,(1,APOLBDT),CURRBBFD                                 
         B     PA10                                                             
         DROP  R6                                                               
*                                                                               
         USING EMPELD,R6                                                        
PA05     CLI   0(R6),X'56'                                                      
         BNE   PA10                                                             
         TM    LGSW,LGPE           IS IT PERSONNEL LEDGER                       
         BNO   PA10                IF NOT SKIP IT                               
         GOTO1 DATCNV,DMCB,(1,EMPHIR),CURRHIRE                                  
         MVC   HIRE,EMPHIR                                                      
         OC    EMPTRM,EMPTRM   DO WE HAVE A TERM DATE                           
         BZ    PA10                NO                                           
         GOTO1 DATCNV,DMCB,(1,EMPTRM),CURRTERM                                  
         MVC   TERM,EMPTRM                                                      
*                                                                               
PA10     IC    R0,1(R6)            GET NEXT ELEMENT                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   PA02                                                             
         DROP  R6                                                               
*                                                                               
PA12     TM    LGSW,LGPR           TEST PERSON RECORDS                          
         BNO   PA13                                                             
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         MVC   PERALEDG,ADLEDGER                                                
         MVC   PERADACC,ADACC                                                   
         GOTO1 PERCALL             GET PERSON BLOCK                             
         DROP  R1                                                               
*                                                                               
PA13     CLI   HOOKMODE,0          DOES HOOK WANT MONACC MODES                  
         BE    *+8                 NO HOOK                                      
         BAS   RE,GOHK             GO TO THE HOOK                               
         L     R6,ADACC                                                         
         TM    RDSW,RDBUD          READ BUDGETS                                 
         BNO   *+8                                                              
         BAS   RE,BDGT                                                          
         B     XXIT                                                             
         TITLE 'BASE - PROCESS SUB-ACCOUNT'                                     
***********************************************************************         
* PROCESS A SUB-ACCOUNT                                                         
***********************************************************************         
         SPACE 1                                                                
PSA00    MVI   FCRDHIST,NO         DON'T WANT MASTER TO PASS PROCHIST           
         MVC   CURRNT,SPACES       CLEAR TYPE OF 1N TIME                        
         MVC   CURRMBK,SPACES      METHOD AND TYPE OF DATA                      
         L     R6,ADSUBAC                                                       
         USING CACRECD,R6                                                       
         TM    RNSW,RNNC           NEW COSTING                                  
         BNO   PSA10                                                            
         MVC   CURRMBK,CACKBTYP    SAVE METHOD AND BUCKET                       
         CLI   CURRBKT,C' '                                                     
         BNH   *+14                                                             
         CLC   CURRMTHD,METHOD     MATCH METHOD                                 
         BNE   XXIT                                                             
         MVI   SBSW,0              SUB-ACCOUNT SWITCHES                         
         CLC   CACKCUNT(2),DIRLUL                                               
         BNE   *+8                                                              
         OI    SBSW,SBDC           DIRECT                                       
         CLC   CACKCUNT(2),INDLUL                                               
         BNE   *+8                                                              
         OI    SBSW,SBIC           INDIRECT                                     
         CLC   CACKCUNT(2),OVRHUL                                               
         BNE   *+8                                                              
         OI    SBSW,SBOH           OVERHEAD                                     
*                                                                               
PSA10    MVI   BFLT1,0             SET BUCKET CONTROL                           
         MVI   BFLT2,0                                                          
         MVI   BTYPE,0                                                          
         CLI   AHOURS,YES          DO THEY WANT ADJUSTED HOURS                  
         BNE   *+8                                                              
         OI    BFLT1,BTFAH         SET ADJ. HOURS FILTER                        
         TM    LGSW,LGPJ           IS IT PROJECT CONTROL LEDGER                 
         BNO   *+8                                                              
         OI    BFLT1,BTFPC         SET P.C. FILTER                              
         TM    LGSW,LGCL           IS IT THE CLIENT LEDGER                      
         BNO   *+8                                                              
         OI    BFLT1,BTFCL                                                      
         TM    LGSW,LGPE           IS IT THE PERSON LEDGER                      
         BNO   *+8                                                              
         OI    BFLT1,BTFPE                                                      
         DROP  R6                                                               
*                                                                               
         LA    R6,ACCORFST(R6)                                                  
         USING CACELD,R6                                                        
         MVC   CURRCON,CACCNT      CONTRA CODE                                  
         MVC   CURRCNAM,SPACES     AND NAME                                     
         MVC   CURRJOB,SPACES      CLEAR JOB                                    
         MVC   CURRTE2,SPACES                                                   
*                                                                               
         TM    LGSW,LGTE           TEST T&E LEDGER                              
         BNO   PSA15                                                            
         LA    R0,12                                                            
         LA    R1,CURRCON+1                                                     
         CLI   0(R1),C'-'          LOOK FOR DASH FOR T2                         
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     PSA15                                                            
         LA    R1,1(R1)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,T2LEN          TEST ALREADY HAVE T2 LENGTH                  
         BNZ   PSA13                                                            
         LA    RF,CURRCON+L'CURRCON-1                                           
         CLI   0(RF),C' '          FIND LENGTH OF T2 CODE                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         LA    RF,1(RF)                                                         
         SR    RF,R1                                                            
         STC   RF,T2LEN            SAVE LENGTH OF T2 CODE                       
PSA13    BCTR  RF,0                                                             
*MN      EX    RF,*+4                                                           
         MVC   CURRTE2(0),0(R1)    EXTRACT T2 CODE                              
         EX    RF,*-6                                                           
*MN                                                                             
*                                                                               
PSA15    SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         BM    *+8                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURRCNAM(0),CACNAME                                              
         CLI   CURRCON+2,C'C'     CLIENT TIME                                   
         BNE   *+12                                                             
         OI    BFLT2,BTFCLT                                                     
         B     PSA20                                                            
         DROP  R6                                                               
*                                                                               
         BAS   RE,NOCLI            SET TYPE FOR NON-CLIENT TIME                 
         CLI   CURRNT,C'L'         LEAVE                                        
         BNE   *+8                                                              
         OI    BFLT2,BTFLVE                                                     
         CLI   CURRNT,C'P'         PERSONAL                                     
         BNE   *+8                                                              
         OI    BFLT2,BTFPER                                                     
         CLI   CURRNT,C'N'         OTHER NON-CLIENT                             
         BNE   *+8                                                              
         OI    BFLT2,BTFNON                                                     
*                                                                               
PSA20    BAS   RE,CNTRLV           SET CONTRA LEVELS                            
         CLI   HOOKMODE,0          DOES HOOK WANT MONACC MODES                  
         BE    PSA30               NO MODE HOOK                                 
         LA    R6,SRCHARG          BUILD SEARCH ARGUMENT FOR TO ACCOUNT         
         USING ARGD,R6                                                          
         MVC   ARGACC,ACTACC+1     ACCOUNT                                      
         MVC   ARGFLT,CURRFLT      FILTERS                                      
         MVC   ARGBKT,CURRBKT      BUCKET TYPE                                  
         MVC   ARGCON,CURRCON+1    CONTRA                                       
         DROP  R6                                                               
         GOTO1 ASRCH               CHECK FOR SUBSTITUTION                       
         TM    SLOPTN,SLFND        WAS IT FOUND                                 
         BNO   XXIT                NOT FOUND                                    
         BAS   RE,GOHK             GO TO THE HOOK                               
         B     XXIT                FINISHED PROCESSING                          
         TITLE 'BASE - PROCESS SUB-ACCOUNT (HISTORIES)'                         
***********************************************************************         
* PROCESS SUB-ACCOUNT (HISTORY)                                                 
***********************************************************************         
         SPACE 1                                                                
PSA30    L     R6,ADSUBAC                                                       
         LA    R6,ACCORFST(R6)                                                  
         USING BUKELD,R6                                                        
         NI    SBSW,ALL-SBBA       TURNOFF BUCKET ACTIVITY                      
*                                                                               
PSA40    SR    R0,R0                                                            
         IC    R0,BUKLN                                                         
         AR    R6,R0                                                            
         CLI   BUKEL,0             TEST FOR EOR                                 
         BE    PSA50               YES                                          
         CLI   BUKEL,BUKELQ        TEST FOR BUCKET ELEMENT                      
         BNE   PSA40               NO                                           
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   BUKMOS,ACMHSTR                                                   
         BL    PSA40                                                            
         CLC   BUKMOS,ACMHEND                                                   
         BH    PSA40                                                            
         DROP  RF                                                               
*                                                                               
         LA    R3,BUKYEAR                                                       
         BAS   RE,GRELMON          GET RELATIVE MONTH NUMBER                    
         MVC   BYTE,BUCKTYPE       HANDLE DEBITS                                
         CLI   CURRBKT,C' '        NEW METHOD BUCKET                            
         BNH   *+14                                                             
         MVC   BYTE,CURRBKT                                                     
         MVI   BTYPE,BTTNC                                                      
         ZAP   DUB,BUKDR                                                        
         ZAP   DUB2,BUKCR                                                       
         CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         CP    DUB2,=P'0'                                                       
         BE    PSA40                                                            
         BAS   RE,PSA70            POST DEBITS AND CREDITS                      
         TM    PCSW,PCBD           OPTION TO POST BY DATE                       
         BNO   PSA40                                                            
         MVC   CURRDATE(2),BUKYEAR                                              
         MVI   CURRDATE+2,X'01'                                                 
         GOTO1 DATCNV,DMCB,(1,CURRDATE),CURRDATE                                
         OI    PCSW,PCPT           POST THIS ONE BY DATE                        
         TM    SBSW,SBBA           TEST ACTIVITY                                
         BNO   *+8                                                              
         BAS   RE,PSB              POST TO CONTRA SUMMARY BUFFER                
         NI    PCSW,ALL-PCPT       TURNOFF POST THIS SWITCH                     
         NI    SBSW,ALL-SBBA       TURNOFF BUCKET ACTIVITY                      
         B     PSA40                                                            
*                                                                               
PSA50    TM    ACSW,ACBR+ACHR      ANY BILLING RATES FOR THIS ACCOUNT           
         BZ    PSA60                                                            
         CLI   BUCKTYPE,C'H'       IS IT HOURS BUCKET                           
         BNE   PSA60                                                            
         BAS   RE,BLAMS            COMPUTE BILLING RATE * HOURS                 
*                                                                               
PSA60    TM    SBSW,SBBA           TEST ACTIVITY                                
         BNO   *+8                                                              
         BAS   RE,PSB              POST TO CONTRA SUMMARY BUFFER                
         B     XXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* POST DEBIT AND CREDIT TO ACCUMULATOR LINE                           *         
***********************************************************************         
         SPACE 1                                                                
PSA70    STM   RE,R6,SVRE                                                       
         L     RF,ABUCKLST         LOOK UP BUCKET POSTING LIST                  
         USING BTD,RF                                                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
PSA72    CLC   BTCDE,BYTE          MATCH BUCKET CODE                            
         BNE   PSA73                                                            
         CLC   BTTYP,BTYPE         TYPE FILTER                                  
         BE    PSA75                                                            
*                                                                               
PSA73    IC    R0,BTLEN            LENGTH OF ENTRY                              
         AR    RF,R0               BUMP TO NEXT                                 
         CLI   0(RF),X'FF'         END OF TABLE                                 
         BNE   PSA72                                                            
         B     PSA90                                                            
*                                                                               
PSA75    IC    R0,BTNUM            NUMBER OF POSTING RULES                      
         LA    R2,BTRUL            R2 = POSTING RULE                            
         USING BTRUL,R2                                                         
*                                                                               
PSA77    TM    RQSW,RQFLT          TOTAL TIME RECORDS IN USE                    
         BNO   PSA78                                                            
         CLI   BTLNE,LNCLT         DON'T POST SOME TIME BUCKETS                 
         BL    PSA78                                                            
         CLI   BTLNE,LNTLP         LINE 17, 18, 19 & 20                         
         BH    PSA78                                                            
         B     PSA85                                                            
*                                                                               
PSA78    CLI   BTFALL,0            ANY FILTERS                                  
         BE    PSA79                                                            
         IC    R1,BFLT1            TEST BUCKETS FILTERS                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BTFALL,0                                                         
         BNO   PSA85               ALL MUST BE TRUE                             
*                                                                               
PSA79    CLI   BTANY,0             FILTER 2                                     
         BE    PSA81                                                            
         IC    R1,BFLT2            TEST BUCKETS FILTERS                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BTANY,0                                                          
         BZ    PSA85               ANY MAY BE TRUE                              
*                                                                               
PSA81    SR    R4,R4                                                            
         IC    R4,BTLNE            LINE NUMBER                                  
         TM    LGSW,LGPE           IF 1R                                        
         BNZ   PSA83               ALWAYS POST FROM BUCKETS                     
         TM    RDSW,RDDTL          PROCESSING DETAIL RECORDS                    
         BNO   PSA83               IF NOT OK TO POST                            
         L     R5,ALNSTL                                                        
         AR    R5,R4               R5 = LINE STATUS BYTE                        
         TM    0(R5),LSTRN         IS THIS A DETAIL LINE                        
         BO    PSA85               YES, DON'T POST HISTORY                      
*                                                                               
PSA83    LA    RF,DUB                                                           
         TM    BTACT,BTADDD+BTSUBD DEALING WITH DEBITS                          
         BNZ   *+8                                                              
         LA    RF,DUB2             OR CREDITS                                   
         CP    0(8,RF),=P'0'                                                    
         BE    PSA85                                                            
         TM    BTACT,BTADDD+BTADDC ADDING                                       
         BNZ   *+10                                                             
         MP    0(8,RF),=P'-1'      SUBTRACTING                                  
         ZAP   PAMT,0(8,RF)                                                     
         BAS   RE,PACC             POST AND SET ACTIVE FLAG                     
         OI    SBSW,SBBA           SET BUCKET ACTIVITY                          
*                                                                               
PSA85    LA    R2,BTRLQ(R2)        ADD TO NEXT ACCUM                            
         BCT   R0,PSA77                                                         
*                                                                               
PSA90    LM    RE,R6,SVRE          ALL DONE                                     
         BR    RE                                                               
         DROP  RF,R2                                                            
         TITLE 'BASE - PROCESS TRANSACTIONS'                                    
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
PTN00    MVI   TNSW,0              TRANSACTIONS SWITCH                          
         CLI   HOOKMODE,0                                                       
         BNE   XXIT                HOOK WILL BUILD SORT RECORDS                 
         L     R6,ADTRANS                                                       
         CLI   0(R6),TRNELQ                                                     
         BNE   XXIT                                                             
         ZAP   CURRHRS,=P'0'       CURRENT HOURS                                
         MVC   CURRDU,SPACES       DATE USED                                    
         MVC   CURRAD,SPACES       DATE ADDED TO FILE                           
         MVC   CURRMD,SPACES       DATE MARKED BY G/L UPDATE                    
         MVC   CURREP,SPACES       EXTRA PAY ELEMENT                            
         MVC   CURROTH,SPACES      SPECIAL ELEMENT                              
         MVC   CURRJOB,SPACES      JOB                                          
         MVC   CURRTSK,SPACES      TASK                                         
         MVC   CURRTIME,SPACES     TIME (B, N OR R)                             
         MVC   CURRBKT,SPACES      BUCKET TYPE                                  
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   ATRSEL,ACMTATRS     A(TRANSACTION STATUS ELEMENT)                
         DROP  RF                                                               
         ICM   R6,15,ATRSEL                                                     
         BZ    PTN01                                                            
         USING TRSELD,R6                                                        
         GOTO1 DATCNV,DMCB,(2,TRSDATE),CURRAD                                   
         OC    TRSUPDT,TRSUPDT    DATED UPDATED TO G/L                          
         BZ    PTN01                                                            
         GOTO1 DATCNV,DMCB,(2,TRSUPDT),CURRMD                                   
         DROP  R6                                                               
*                                                                               
PTN01    L     R6,ADTRANS                                                       
         SH    R6,DATADISP                                                      
         USING TRNRECD,R6                                                       
         OC    TRNKEY+ACCOPEEL(ACCOPLEN),TRNKEY+ACCOPEEL                        
         BNZ   XXIT                                                             
         OC    TRNKEY+ACCOUSED(ACCOULEN),TRNKEY+ACCOUSED                        
         BZ    PTN03                                                            
         LA    RF,TRNKEY+ACCOUSED                                               
         GOTO1 DATCNV,DMCB,(2,(RF)),CURRDU                                      
         DROP  R6                                                               
*                                                                               
         USING TRNELD,R6                                                        
PTN03    L     R6,ADTRANS                                                       
         MVC   CURRTDTE,TRNDATE   TRANSACTION DATE                              
         MVC   CURRREF,TRNREF     REFERENCE                                     
         MVC   CURRTYPE,TRNTYPE   TYPE                                          
         MVC   CURRSTAT,TRNSTAT   STATUS                                        
         MVC   CURROFC,TRNANAL    OFFICE OR WORKCODE                            
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   CURRMOS,ACMMDTE     MONTH OF SERVICE                             
         MVC   CURRBTCH,TRNBTCH    BATCH REFERENCE                              
         DROP  RF                                                               
*                                                                               
         MVC   WORK(2),CURRMOS     MOS YEAR/MONTH(PWOS)                         
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)                                  
         MVC   CURRBYM,WORK+6      BATCH YYMM                                   
         GOTO1 DATCON,DMCB,(1,WORK),(9,WORK+6)                                  
         MVC   CURRBMY,WORK+6      BATCH MMM/YY                                 
*                                                                               
         MVI   CURRNARR,C' '                                                    
         MVC   CURRNARR+1(L'CURRNARR-1),CURRNARR                                
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=H'29'                                                        
         BM    *+8                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURRNARR(0),TRNNARR                                              
         ZAP   CURRNET,TRNAMNT     TRANSACTION AMOUNT                           
         ZAP   CURRGRS,TRNAMNT     GROSS                                        
         ZAP   CURRCOM,TRNAMNT     COMMISSION                                   
         ZAP   CURRCSD,=P'0'       NO C.D. - FOR NOW                            
         GOTO1 DATCNV,DMCB,(1,CURRTDTE),CURRDATE                                
         MVC   CURRDUE,CURRDATE    DUE DATE - DEFAULT IS TRANSACTION            
         LA    R3,CURRMOS                                                       
         CLI   POSTMOS,YES         POST BY MONTH OF SERVICE                     
         BE    PTN05                                                            
         LA    R3,PEDTE            PACKED END DATE                              
         TM    OPT1,FTD            OPTION TO FORCE TRANS DATE TO QEND           
         BO    PTN05                                                            
         LA    R3,CURRTDTE         POST BY TRANSACTION DATE                     
*                                                                               
PTN05    BAS   RE,GRELMON          GET RELATIVE MONTH NUMBER                    
         EJECT                                                                  
PTN07    SR    R0,R0               PROCESS REMAINING ELEMENTS                   
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PTN65                                                            
         LA    RE,PTNELT           TRANSACTION ELEMENT TABLE                    
PTN09    ICM   RF,15,2(RE)                                                      
         CLC   0(1,R6),0(RE)       MATCH ELEMENT TO TABLE                       
         BNE   PTN13                                                            
         CLI   1(RE),0                                                          
         BER   RF                                                               
         CLC   SCITYPE-SCIELD(1,R6),1(RE)                                       
         BER   RF                                                               
PTN13    LA    RE,L'PTNELT(RE)                                                  
         CLI   0(RE),EOT                                                        
         BNE   PTN09                                                            
         B     PTN07                                                            
         DROP  R6                                                               
*                                                                               
         CNOP  0,4                                                              
PTNELT   DS    0XL6                                                             
         DC    AL1(OTHELQ),AL1(0),AL4(PTNOTH)                                   
         DC    AL1(MDTELQ),AL1(0),AL4(PTNMDT)                                   
         DC    AL1(SCIELQ),CL1'D',AL4(PTNSCD)                                   
         DC    AL1(SCIELQ),CL1'G',AL4(PTNSCG)                                   
*&&UK                                                                           
         DC    AL1(MXPELQ),AL1(0),AL4(PTNMXP)                                   
         DC    AL1(SCIELQ),CL1'C',AL4(PTNSCC)                                   
         DC    AL1(SCIELQ),CL1'H',AL4(PTNSCH)                                   
         DC    AL1(SCIELQ),CL1'T',AL4(PTNSCT)                                   
         DC    AL1(SCIELQ),CL1'X',AL4(PTNSCX)                                   
*&&                                                                             
*&&US                                                                           
         DC    AL1(PRTELQ),AL1(0),AL4(PTNPRT)                                   
         DC    AL1(XPYELQ),AL1(0),AL4(PTNXPY)                                   
         DC    AL1(PCIELQ),AL1(0),AL4(PTNPCI)                                   
         DC    AL1(DUEELQ),AL1(0),AL4(PTNDUE)                                   
         DC    AL1(SCIELQ),CL1'C',AL4(PTNSCC)                                   
         DC    AL1(SCIELQ),CL1'E',AL4(PTNSCE)                                   
         DC    AL1(SCIELQ),CL1'H',AL4(PTNSCH)                                   
*&&                                                                             
         DC    AL1(EOT)                                                         
         CNOP  0,4                                                              
         EJECT                                                                  
         USING OTHELD,R6                                                        
PTNOTH   MVC   CURROSBR,OTHNUM     NUMBER(9) AND PROF(4)                        
         B     PTNMDT3                                                          
         DROP  R6                                                               
*                                                                               
         USING MDTELD,R6                                                        
PTNMDT   MVC   CURROMOS,MDTMOS                                                  
         MVC   CURRONUM,MDTPRD                                                  
PTNMDT3  CLI   CURROMOS,X'80'      TEST MEDIA YEAR                              
         BL    PTN07                                                            
         CLI   CURROMOS+1,X'01'    AND MEDIA MONTH                              
         BL    PTN07                                                            
         CLI   CURROMOS+1,X'12'                                                 
         BH    PTN07                                                            
         MVC   WORK(2),CURROMOS    MEDIA YEAR/MONTH(PWOS)                       
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)   YYMM                           
         MVC   CURROMM,WORK+8      MEDIA MONTH                                  
         GOTO1 DATCON,DMCB,(1,WORK),(9,WORK+6)                                  
         MVC   CURROMMY,WORK+6     BATCH MMM/YY                                 
         B     PTN07                                                            
         DROP  R6                                                               
*&&US                                                                           
         USING PRTELD,R6                                                        
PTNPRT   EQU   *                                                                
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME                                
         BZ    *+8                                                              
         MVI   CURRTIME,C'B'                                                    
         TM    PRTSTAT,PRTSNOTQ    NON-CLIENT                                   
         BZ    *+8                                                              
         MVI   CURRTIME,C'N'                                                    
         TM    PRTSTAT,PRTSRTEQ    SPECIAL NON-CLIENT                           
         BZ    *+8                                                              
         MVI   CURRTIME,C'R'                                                    
         B     PTN07                                                            
         DROP  R6                                                               
*&&                                                                             
         USING XPYELD,R6                                                        
PTNXPY   MVC   CURREP,0(R6)        EXTRA PAY ELEMENT                            
         ZAP   CURRCSD,XPYCD       SAVE  CURRNET CD                             
         B     PTN07                                                            
         DROP  R6                                                               
*                                                                               
         USING MXPELD,R6                                                        
PTNMXP   MVC   CURREP,0(R6)        EXTRA PAY ELEMENT                            
         B     PTN07                                                            
         DROP  R6                                                               
*                                                                               
         USING SCIELD,R6                                                        
PTNSCD   ZAP   CURRCSD,SCIAMNT     CASH DISCOUNT                                
         B     PTN07                                                            
*                                                                               
PTNSCG   ZAP   CURRGRS,SCIAMNT     GROSS AMOUNT                                 
         B     PTN07                                                            
         SPACE 1                                                                
*&&UK                                                                           
PTNSCC   LA    R4,LNCAM            UK - CAMT                                    
         ZAP   PAMT,SCIAMNT                                                     
         BAS   RE,PACC                                                          
         B     PTN07                                                            
*                                                                               
PTNSCH   DS    0H                  UK -HOURS                                    
PTNSCT   LA    R4,LNCHR                                                         
         ZAP   PAMT,SCIAMNT                                                     
         BAS   RE,PACC                                                          
         B     PTN07                                                            
*                                  SPECIAL FOR UK                               
PTNSCX   LA    R4,LNVAT            VAT                                          
         ZAP   PAMT,SCIAMNT                                                     
         BAS   RE,PACC                                                          
         B     PTN07                                                            
*&&                                                                             
         DROP  R6                                                               
         SPACE 1                                                                
*&&US                                                                           
         USING PCIELD,R6                                                        
PTNPCI   MVC   CURRJOB,PCICLI+3    PROJECT CONTROL                              
         CLI   PCILN,X'22'                                                      
         BL    PTN07                                                            
         MVC   CURRTSK,PCITSK                                                   
         B     PTN07                                                            
         DROP  R6                                                               
*                                                                               
         USING DUEELD,R6                                                        
PTNDUE   OC    DUEDATE,DUEDATE     DUE DATE                                     
         BZ    PTN07                                                            
         GOTO1 DATCNV,DMCB,(2,DUEDATE),CURRDUE                                  
         B     PTN07                                                            
         DROP  R6                                                               
*                                                                               
         USING SCIELD,R6                                                        
PTNSCC   ZAP   CURRCOM,SCIAMNT     COMMISSION                                   
         B     PTN07                                                            
*                                                                               
PTNSCE   TM    RDSW,RDDTL          PROCESSING DETAILS                           
         BNO   PTN07               NO, DON'T POST DETAIL                        
         CLI   SCILN,09                                                         
         BNH   PTNSCE1                                                          
         LA    R4,LNEXN            R4 = EXPENDITURE NET                         
         ZAP   PAMT,SCIADMN        EXPENDITURE NET                              
         BAS   RE,PACC                                                          
PTNSCE1  LA    R4,LNEXG            EXPENDITURE GROSS                            
         ZAP   PAMT,SCIAMNT                                                     
         BAS   RE,PACC                                                          
         B     PTN07                                                            
*                                                                               
PTNSCH   CLC   CURRMOS,PEDTE       TEST MOS AFTER END DATE                      
         BH    PTN07               DON'T COUNT HOURS                            
         ICM   RF,15,ATRSEL                                                     
         BZ    PTN07                                                            
         ZAP   CURRHRS,SCIAMNT     SAVE HOURS                                   
         LA    R4,LNTHR            TRANSACTION HOURS                            
         ZAP   PAMT,SCIAMNT                                                     
         BAS   RE,PACC                                                          
         SR    R4,R4                                                            
         TM    TRSSTAT2-TRSELD(RF),X'04'                                        
         BZ    *+8                                                              
         LA    R4,LNMSH            MISSING HOURS                                
         TM    TRSSTAT2-TRSELD(RF),X'08'                                        
         BZ    *+8                                                              
         LA    R4,LNADH            ADJUSTED HOURS                               
         LTR   R4,R4               ANY ACCUM LINE?                              
         BZ    PTNSCH1             NO                                           
         BAS   RE,PACC                                                          
         TM    TRSSTAT2-TRSELD(RF),X'08'                                        
         BO    PTN07               DON'T COUNT ADJUSTMENTS                      
PTNSCH1  TM    OPT1,MWR            ANY MWR NEEDED?                              
         BNO   PTN07                                                            
         TM    RQSW,RQFLT          TOTAL RECORDS IN USE                         
         BO    PTN07                                                            
         L     R5,AMANWK           WEEK DATE ARRAY                              
         USING MNWKD,R5                                                         
         LH    RF,MNWKS            NUMBER OF ENTRIES                            
         CLC   CURRTDTE,MNWKSTR    TEST BEFORE START                            
         BL    PTN07               SKIP IT                                      
PTNSCH2  CLC   CURRTDTE,MNWKEND    IS TRANSACTION PAST DATE IN ARRAY            
         BH    *+18                DID NOT WORK THIS WEEK                       
         MVI   MNWKHIT,C'H'        MARK A HIT                                   
         AP    MNWKHRS,CURRHRS     ADD THE HOURS                                
         B     PTN07               LEAVE                                        
         LA    R5,MNWKLNQ(R5)      BUMP TO NEXT DATE IN ARRRAY                  
         BCT   RF,PTNSCH2          AND CHECK TRANSACTION AGAINST IT             
         B     PTN07               NO HIT - PASSED THE LAST DATE                
         DROP  R5,R6                                                            
*&&                                                                             
         EJECT                                                                  
PTN65    TM    RDSW,RDDTL          IF NOT PROCESSING DETAIL                     
         BNO   PTNX                DON'T POST                                   
         LA    R4,LNDRS            POSITION TO LINE 1 (DEBITS)                  
         TM    CURRSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R4,LNCRS            OR LINE 2 (CREDITS)                          
         ZAP   PAMT,CURRNET        NET DEBIT/CREDIT                             
         BAS   RE,PACC                                                          
         LA    R4,LNCMM            POST COMMISSION                              
         ZAP   PAMT,CURRCOM                                                     
         BAS   RE,PACC                                                          
         LA    R4,LNCSD            CASH DISCOUNT                                
         ZAP   PAMT,CURRCSD                                                     
         BAS   RE,PACC                                                          
         LA    R4,LNGRB            GROSS BILLING                                
         ZAP   PAMT,CURRGRS                                                     
         BAS   RE,PACC                                                          
*                                                                               
         TM    ACSW,ACHR           STANDARD HOURLY RATES ?                      
         BZ    *+8                                                              
         BAS   RE,BLAMT                                                         
         TM    ACSW,ACPH           PERSONAL HOURLY RATES ?                      
         BZ    *+8                                                              
         BAS   RE,PHRDLR           GET PERSONAL HOURLY DOLLARS                  
*                                                                               
PTN90    TM    RDSW,RDBKT          READING BUCKETS                              
         BO    PTN93               HOLD IN BUFFER, 'TIL LAST                    
         MVI   PSTLV,LSTRN+LSACC   POST TRANSACTION AND ACCOUNT DATA            
         BAS   RE,MLTP             POST IT NOW                                  
         MVI   CLRLV,LSTRN                                                      
         BAS   RE,CLR              CLEAR DETAIL LINES                           
         B     PTNX                                                             
*                                                                               
PTN93    BAS   RE,PSB              POST TO CONTRA SUMMARY BUFFER                
PTNX     B     XXIT                                                             
         TITLE 'BASE - PROCESS TIME RECORDS'                                    
***********************************************************************         
* PROCESS TIME                                                        *         
***********************************************************************         
         SPACE 1                                                                
PTM00    L     R6,ADTRANS          TIME ELEMENT                                 
         USING TIMELD,R6                                                        
         CLI   TIMEL,TIMELQ                                                     
         BNE   XXIT                                                             
         CLI   TIMETYP,TIMEINP     TEST ELEMENT TYPE                            
         BNE   XXIT                                                             
         CLC   TIMMOA,MONLIST+4    TEST BEFORE START                            
         BL    XXIT                                                             
         CLC   TIMMOA,MONLAST+4    TEST AFTER END                               
         BH    XXIT                                                             
*                                                                               
         MVC   CURRBKT,SPACES      BUCKET TYPE                                  
         MVC   CURRMOS,TIMMOA      SAVE CURRENT MOA                             
         LA    R3,CURRMOS                                                       
         BAS   RE,GRELMON          GET RELATIVE NUMBER FOR THIS MONTH           
*                                                                               
         MVC   CURRJOB,TIMACC+2                                                 
         MVC   CURRTSK,TIMTSK                                                   
         ZAP   CURRHRS,TIMHRS      SAVE HOURS                                   
         TM    TIMTTYP,TIMTCB                                                   
         BZ    *+8                                                              
         MVI   CURRTIME,C'B'                                                    
         TM    TIMTTYP,TIMTCR                                                   
         BZ    *+8                                                              
         MVI   CURRTIME,C'R'                                                    
         TM    TIMTTYP,TIMTCN                                                   
         BO    *+8                                                              
         TM    TIMTTYP,TIMTNC                                                   
         BZ    *+8                                                              
         MVI   CURRTIME,C'N'                                                    
         ZAP   PAMT,TIMHRS                                                      
         LA    R4,LNTHR            TRANSACTION HOURS                            
         BAS   RE,PACC                                                          
         TM    TIMIND,TIMIADJ      TEST ADJUSTED                                
         BNO   PTM05                                                            
         LA    R4,LNADH            ADJUSTED HOURS                               
         BAS   RE,PACC                                                          
         B     PTMX                ADJUSTMENTS CAN'T BE MISSING                 
*                                                                               
PTM05    L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R2,ACMALTN          R2=A(TIME RECORD)                            
         DROP  RF                                                               
*                                                                               
         USING TIMRECD,R2                                                       
         L     R5,AMANWK           R5=A(CALENDAR)                               
         USING MNWKD,R5                                                         
*                                                                               
PTM07    CLC   TIMKPEDT,MNWKSTR    TEST W/E TO CALENDAR                         
         BL    PTMX                                                             
         CLC   TIMKPEDT,MNWKEND                                                 
         BNH   *+12                                                             
         LA    R5,MNWKLNQ(R5)                                                   
         B     PTM07                                                            
         CLC   CURRMOS,MNWKMOA     TEST CURRENT MONTH AFTER                     
         BNH   PTM09               MONTH WORKED                                 
         LA    R4,LNMSH            ADD TO MISSING HOURS                         
         BAS   RE,PACC                                                          
*                                                                               
PTM09    TM    OPT1,MWR            ANY MWR NEEDED?                              
         BNO   PTMX                                                             
         MVI   MNWKHIT,C'H'        MARK A HIT                                   
         AP    MNWKHRS,CURRHRS     ADD THE HOURS                                
PTMX     B     PTN90                                                            
         DROP  R2,R5,R6                                                         
         TITLE 'BASE - LAST FOR ACCOUNT'                                        
***********************************************************************         
* LAST FOR ACCOUNT                                                    *         
***********************************************************************         
         SPACE 1                                                                
AL00     CLI   HOOKMODE,0                                                       
         BNE   XXIT                HOOK WILL BUILD SORT RECORDS                 
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         ZAP   BBF,ACMABAL         ZAP BFF FROM MONACC                          
         CP    BBF,=P'0'                                                        
         BE    AL01                                                             
         MVI   PSTLV,LSNON         POST NON-LINE DATA AND ACC DATA              
         BAS   RE,MLTP             POST BBF                                     
         ZAP   BBF,=P'0'           AND CLEAR IT                                 
*                                                                               
AL01     TM    OPT1,MWR            MAN-WEEK-RATIOS                              
         BNO   AL03                                                             
         BAS   RE,PMW              POST WEEKS WORKED ACCUMULATORS               
*                                                                               
AL03     L     R6,ACONSR           A(CONTRA SUMMARY RECORD)                     
         USING CONSD,R6                                                         
         XC    CONSKEY(CONSKLNQ),CONSKEY                                        
         XC    LASTCON,LASTCON                                                  
         OI    PCSW,PCMWR          POST MWR ROWS ONLY AT ACC LAST               
         MVI   CHASW,0             LEVEL CHANGE CONTROL                         
         GOTO1 ABUFC,TSRMGET                                                    
         BE    AL05                                                             
         MVI   CONSKEY,X'FF'       SET EOF INDICATOR                            
         B     AL15                                                             
*                                                                               
AL05     MVI   CLRLV,LSSUB+LSTRN   SET TO CLEAR SUB-ACCOUNT LINES               
         BAS   RE,CLR              CLEAR THEM                                   
         MVC   CURRBKT,CONSBKT     BUCKET TYPE                                  
         MVC   CURRCON+1(14),CONSCON                                            
         MVC   CURRCNAM,CONSNAM    CURRENT CONTRA/NAME/DATE                     
         MVC   CURRDATE,SPACES                                                  
         MVI   CURRNT,C' '                                                      
         BAS   RE,CNTRLV           SET CONTRA LEVELS                            
         BAS   RE,NOCLI            SET NON-CLIENT TYPE                          
         MVI   PSTLV,LSSUB+LSTRN+LSACC                                          
         TM    PCSW,PCBD           POST AT BUCKET LEVEL                         
         BNO   *+14                                                             
         MVC   CURRDATE(6),CONSDTE                                              
         B     AL07                                                             
         MVC   CURRJOB,CONSCPJ                                                  
         MVC   CURRTSK,CONTSK                                                   
         MVC   CURRTIME,CONTTIM                                                 
*                                                                               
AL07     SR    R4,R4                                                            
         IC    R4,CONSLNO          PICK UP LINE NUMBER                          
         LR    R5,R4                                                            
         A     R5,ALNSTL                                                        
         OI    0(R5),LSACT         SET LINE ACTIVE                              
         MH    R4,WLINE                                                         
         A     R4,AACCUM           R4 = A(ACCUMULATOR LINE)                     
         LA    R5,WLN              WIDTH OF LINE                                
         LA    RE,CONSACS                                                       
         LR    RF,R5                                                            
         MVCL  R4,RE               SETUP ACCUM LINE                             
         GOTO1 ABUFC,TSRMNXT                                                    
         BE    *+8                                                              
         MVI   CONSKEY,X'FF'       SET EOF INDICATOR                            
         CLC   CONSCON,CURRCON+1   TEST SAME CONTRA                             
         BNE   AL09                NO - POST IT                                 
         CLC   CONSBKT,CURRBKT     SAME BUCKET TYPE                             
         BNE   AL09                                                             
         CLC   CONSCPJ,CURRJOB     SAME JOB OR DATE                             
         BNE   AL09                                                             
         CLC   CONTSK,CURRTSK      SAME TASK                                    
         BNE   AL09                                                             
         CLC   CONTTIM,CURRTIME    SAME TYPE OF TIME                            
         BE    AL07                                                             
*                                                                               
AL09     CLC   CURRCON,LASTCON     TEST SAME CONTRA                             
         BE    AL10                                                             
         OI    CHASW,CHACON        SET CHANGE OF CONTRA                         
         CLC   CURRCON+1(2),COSTUL                                              
         BNE   AL10                                                             
         SR    RE,RE               TEST CHANGE OF CLIENT                        
         IC    RE,CLIPOS                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              REPLACE THESE OLD INSTRUCTIONS               
         B     *+10                                                             
         CLC   CURRCON+3(0),LASTCON+3 TEST SAME CLIENT                          
         BE    *+8                                                              
         OI    CHASW,CHACLI        SET CHANGE OF CLIENT                         
*                                                                               
AL10     TM    ACSW,ACPH           PERSONAL HOURLY RATES ?                      
         BZ    *+8                                                              
         BAS   RE,CCST             GET CURRENT COST DATA                        
         BAS   RE,MLTP             POST CURRENT LINES                           
         MVC   LASTCON,CURRCON     SAVE LAST CONTRA POSTED                      
         MVI   CHASW,0                                                          
*                                                                               
AL11     CLI   CONSKEY,X'FF'       WAS THAT THE LAST TIME                       
         BNE   AL05                NO, MUST GET NEXT BUFFER RECORD              
*                                                                               
AL15     NI    PCSW,ALL-PCMWR                                                   
         MVI   CLRLV,LSACC+LSSUB+LSTRN                                          
         BAS   RE,CLR              CLEAR THEM                                   
*                                                                               
         TM    LGSW,LGPE                                                        
         BNO   *+10                                                             
         GOTO1 APSD                POST BILLING RATES                           
         MVI   PSTLV,LSACC         POST ACCOUNT DATA                            
         BAS   RE,MLTP             LAST FOR ACCOUNT                             
*                                                                               
AL50     L     R6,ADACC                                                         
         MVI   GPRLV,GPRLVD        SET ACTIVITY LEVEL D                         
         BAS   RE,GPR              GENERATE A PROFORMA REPORT                   
         B     XXIT                                                             
         DROP  R6                                                               
         TITLE 'BASE - LAST FOR LEVEL C, B, A'                                  
***********************************************************************         
* LAST FOR LEVEL A, B, C                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R6                                                       
LCL00    L     R6,ADHEIRC                                                       
         MVI   GPRLV,GPRLVC                                                     
         B     LLX                                                              
*                                                                               
LBL00    L     R6,ADHEIRB                                                       
         MVI   GPRLV,GPRLVB                                                     
         B     LLX                                                              
*                                                                               
LAL00    L     R6,ADHEIRA                                                       
         MVI   GPRLV,GPRLVA                                                     
*                                                                               
LLX      MVC   ACTACC,ACTKEY                                                    
         MVC   CURRACC,ACTACC                                                   
         BAS   RE,GETNAME                                                       
         MVC   CURRNAME,WORK                                                    
         MVC   CURRNUM,ACTACC+3    PRESET NUMBER=ACCOUNT                        
         BAS   RE,GPR              GENERATE THE PROFORMA REPORT                 
         B     XXIT                                                             
         DROP  R6                                                               
         TITLE 'BASE - SET CONTRA LEVELS'                                       
***********************************************************************         
* SET CONTRA LEVEL                                                    *         
***********************************************************************         
         SPACE 1                                                                
CNTRLV   STM   RE,R6,SVRE                                                       
         XC    CURRCNTR(L'CURRCNTR*4),CURRCNTR                                  
         L     R3,LCNTRLV          A(LEVELS FOR LAST CONTRA U/L)                
         CLC   LCNTRUL,CURRCON+1   SAME CONTRA UNIT/LEDGER                      
         BE    CNTRLV7                                                          
         XC    LCNTRLV,LCNTRLV     CLEAR SAVED ADDRESS                          
         MVC   LCNTRUL,CURRCON+1   SAVE NEW CONTRA U/L                          
         L     R3,ALEVPOOL         SEARCH LEDGER POOL                           
         USING LDGLD,R3                                                         
*                                                                               
CNTRLV3  CLC   LDGLUNL,CURRCON+1   LOOK UP LEDGER IN POOL                       
         BE    CNTRLV7                                                          
*&&UK                                                                           
         CLC   CURRCON+1(2),PROJUL IF CONTRA IS 1J                              
         BNE   CNTRLV5                                                          
         CLC   LDGLUNL,PRODUL      THEN USE SJ                                  
         BE    CNTRLV7                                                          
*&&                                                                             
CNTRLV5  LA    R3,LDGLLNQ(R3)                                                   
         CLI   0(R3),0             END OF TABLE (FUNNY CONTRA)                  
         BE    CNTRLVX                                                          
         B     CNTRLV3                                                          
*                                                                               
CNTRLV7  LTR   R3,R3                                                            
         BZ    CNTRLVX             NO LEDGER ENTRY                              
         ST    R3,LCNTRLV          SAVE ADDRESS OF LEDGER LEVEL                 
         LA    RF,CURRCNTR         RF = CONTRA LEVELS                           
         SR    R0,R0                                                            
         IC    R0,LDGLNUM          NUMBER OF LEVELS(CHARACTER)                  
         SLL   R0,28                                                            
         SRL   R0,28               R0=NUMBER OF LEVELS                          
         LA    R3,LDGLLEV          R3=LEVEL ENTRY                               
         USING LDGLLEV,R3                                                       
         LA    R5,FRC1                                                          
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,LDGLLEN          R1=LENGTH OF LEVEL                           
         AH    R1,=H'2'            PLUS CUL                                     
*                                                                               
CNTRLV11 STC   R5,0(RF)            SET TYPE                                     
         MVC   1(2,RF),LDGLDSP     SAVE DSP AND LENGTH                          
         MVC   3(L'CURRCAC1,RF),SPACES                                          
         AR    R1,RE               ADD LENGTH OF LEVEL                          
*MN      EX    R1,*+4                                                           
         MVC   3(0,RF),CURRCON     MOVE CONTRA KEY                              
         EX    R1,*-6                                                           
*MN                                                                             
         LA    R3,L'LDGLLEV(R3)                                                 
         LA    RF,L'CURRCNTR(RF)   RF=NEXT CONTRA LEVEL FIELD                   
         IC    RE,LDGLLEN          LENGTH OF NEXT LEVEL                         
         LA    R5,1(R5)                                                         
         BCT   R0,CNTRLV11                                                      
*                                                                               
CNTRLVX  LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R3                                                               
         TITLE 'BASE - PROCESS A SUB-ACCOUNT'                                   
***********************************************************************         
* PROCESS SUBACCOUNT BREAK                                            *         
***********************************************************************         
         SPACE 1                                                                
PSB      TM    PCSW,PCBD+PCPT      IF OPTION TO POST BY BUCKET DATE             
         BMR   RE                  BUT NOT THIS TIME - SKIP POSTING             
         TM    OPT1,PCT            ARE PERCENTAGES IN USE                       
         BO    PSB01                                                            
         LR    R0,RE                                                            
         MVI   PSTLV,LSSUB+LSACC   POST SUBACCOUNT AND ACCOUNT DATA             
         BAS   RE,MLTP                                                          
         MVI   CLRLV,LSSUB                                                      
         BAS   RE,CLR              CLEAR SUB-ACCOUNTS                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PSB01    NTR1  ,                   WRITE ACCUM LINES TO BUFFER                  
         L     R6,ACONSR           A(CONTRA SUMMARY RECORD)                     
         USING CONSD,R6                                                         
         XC    CONSKEY(CONSKLNQ),CONSKEY                                        
         MVC   CONSBKT,CURRBKT     BUCKET TYPE                                  
         MVC   CONSCON,CURRCON+1   SAVE CONTRA-ACCOUNT                          
         MVC   CONSNAM,CURRCNAM                                                 
         TM    PCSW,PCBD           POST AT BUCKET LEVEL                         
         BNO   *+14                                                             
         MVC   CONSDTE,CURRDATE    SAVE YYMMDD                                  
         B     PSB02                                                            
         OC    CURRJOB,SPACES                                                   
         MVC   CONSCPJ,CURRJOB                                                  
         TM    PCSW,PCTSK          POST BY TASK                                 
         BNO   PSB02                                                            
         MVC   CONTSK,CURRTSK                                                   
         MVC   CONTTIM,CURRTIME                                                 
*                                                                               
PSB02    LA    R0,LNMX             NUMBER OF LINES                              
         L     R6,AZRO                                                          
         LA    R6,WLN(R6)          R6=CURRENT LINE                              
         MVI   BYTE,0              KEEP TRACK OF LINE NUMBERS                   
*                                                                               
PSB09    L     R2,AZRO             R2=A(THE FIRST LINE) ZEROS                   
         SR    R5,R5                                                            
         IC    R5,BYTE             LINE NUMBER                                  
         LA    R5,1(R5)                                                         
         STC   R5,BYTE             START WITH LINE 1                            
         A     R5,ALNSTL           A(THE LINE STATUS LIST)                      
         TM    0(R5),LSACT+LSBUF   TEST ACTIVITY/BUFF LINE                      
         BNO   PSB15                                                            
         LA    R3,WLN              R3=WIDTH OF LINE                             
         LR    R4,R6               R4=(CURRENT ACCUM LINE)                      
         LR    R5,R3                                                            
         STM   R2,R6,PARM          SAVE REGISTERS                               
         CLCL  R4,R2               IS LINE ALL ZEROS?                           
         BE    PSB15               ALL ZEROS - SKIP IT                          
*                                                                               
PSB11    L     R6,ACONSR                                                        
         MVC   CONSLNO,BYTE        LINE NUMBER                                  
         LM    R3,R5,PARM+4                                                     
         LA    R2,CONSACS                                                       
         MVCL  R2,R4               ACCUMULATORS TO SUMMARY RECORD               
         GOTO1 ABUFC,TSRMPUT                                                    
         CLI   CURRBKT,C' '                                                     
         BH    PSB14                                                            
         TM    ACSW,ACPH           PERSONAL HOURLY RATES ?                      
         BZ    PSB14                                                            
         L     R3,AHRSTAB          TABLE OF LINE TO SAVE                        
*                                                                               
PSB12    CLI   0(R3),X'FF'                                                      
         BE    PSB14                                                            
         CLC   CONSLNO,0(R3)       MATCH THE LINE NUMBER                        
         BE    PSB13                                                            
         LA    R3,2(R3)                                                         
         B     PSB12                                                            
*                                                                               
PSB13    MVC   CONSLNO,1(R3)       USE WORK LINE                                
         BASR  RE,RF                                                            
         MVI   CONSBKT,C'1'        SAVE LINE FOR SALARY COMPUTATIONS            
         BASR  RE,RF                                                            
         MVI   CONSBKT,C'2'        SAVE LINE FOR PENSION COMPUTATIONS           
         BASR  RE,RF                                                            
         MVI   CONSBKT,C'3'        SAVE LINE FOR BENEFIT COMPUTATIONS           
         BASR  RE,RF                                                            
         MVC   CONSBKT,CURRBKT     RESTORE BUCKET TYPE                          
*                                                                               
PSB14    LM    R2,R6,PARM                                                       
         MVCL  R4,R2               CLEAR LINE TO ZEROS                          
*                                                                               
PSB15    LA    R6,WLN(R6)          R6 TO NEXT LINE                              
         BCT   R0,PSB09                                                         
         B     XIT                                                              
         DROP  R6                                                               
         TITLE 'BASE - LAST FOR REQUEST'                                        
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         SPACE 1                                                                
RQL00    GOTO1 AINIT               ANY MORE LEDGERS                             
         TM    RQSW,RQERR          INIT ERROR                                   
         BO    RQL25               SKIP THIS REQUEST                            
         CLI   READLEDG,C' '                                                    
         BE    RQL06               NO MORE TO READ                              
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     COME BACK TO REQUEST FIRST                   
*&&US                                                                           
         TM    RNSW,RNTT           TEST TOTAL TIME RECORDS                      
         BNO   RQL05                                                            
         L     R3,ADQSTACK         SET UP CLIENT FILTER FROM QSELECT            
         USING ACQD,R3                                                          
         CLC   QUNIT(2),PERSUL     UNIT/LEDGER=1R                               
         BNE   RQL05                                                            
         CLI   FCPERSEC,FCPERCLI   CONTRA OFFICE FILTERING                      
         BNE   *+8                                                              
         OI    RQSW,RQFLT          SET FILTERING SWITCH                         
         CLC   QSELECT,SPACES                                                   
         BE    RQL05                                                            
         MVC   ACQTYP1,SPACES                                                   
         MVC   ACQFLT1,SPACES                                                   
         LA    R6,DKEY             TEST QSELECT IS A CLIENT CODE                
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),COSTUL                                                
         MVC   ACTKACT(L'QSELECT),QSELECT                                       
         GOTO1 ADMGR,HIGH                                                       
         BNE   RQL05                                                            
         LA    RE,ACQTYP1                                                       
         CLI   ACQCONT2,C'C'       IS THERE A THIRD CARD?                       
         BNE   RQL02               NO,  SO SET ONE UP                           
         CLI   0(RE),C' '          YES, SO WAS THERE INPUT ALREADY?             
         BE    *+8                 NO,  SO OK TO PUT THIS DATA                  
         LA    RE,ACQTYP2          YES, SO PUT IN NEXT POSSIBLE FIELD           
*                                                                               
RQL02    MVI   0(RE),ACQCNTR       SET UP CONTRA FILTER BY ACMASTER             
         MVC   1(L'ACQFLT1,RE),ACTKUNT                                          
         MVI   ACQCONT1,C'C'                                                    
         MVI   ACQCONT2,C'C'                                                    
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         CLI   MCRQNUM,3                                                        
         BNL   *+8                                                              
         MVI   MCRQNUM,3                                                        
         OI    RQSW,RQFLT          SET FILTERING SWITCH                         
         DROP  R3,R5,R6                                                         
*&&                                                                             
RQL05    TM    OPT1,FTD            DON'T SET MONACC DATE FILTER                 
         BO    XXIT                IF FORCEING TRANSACTIONS DATES               
         CLI   BBFSW,YES           DO WE WANT BBF?                              
         BNE   XXIT                NO SO DON'T SET MOS DATES                    
         SR    R3,R3                                                            
         IC    R3,PERIOD           INDEX INTO MONLIST                           
         BCTR  R3,0                                                             
         MH    R3,=H'6'            BUMP UP TO DATE                              
         LA    R3,MONLIST(R3)      POINT TO START DATE MINUS 1 MONTH            
         MVC   QMOSSTRT,0(R3)      SET TO START DATE, GETING DETAIL             
         MVC   QMOSEND,MONLIST+210     BETWEEN MOS DATES                        
         B     XXIT                                                             
*                                                                               
RQL06    TM    SRTSW,SRTAC         ANY ACTIVITY ?                               
         BNO   RQL12                                                            
         GOTO1 APUT                PUT LAST RECORDS TO SORT                     
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
*        MVC   BOXWIDTH+3(1),SYSWIDTH                                           
         GOTO1 AREPS               PRINT THE REPORTS                            
*                                                                               
RQL12    GOTO1 ADSORTER,DMCB,=C'END'                                            
*                                                                               
RQL25    MVI   RQSW,0              INITIALIZE REQUEST SWITCH                    
         MVI   RCREQREP,YES        PRINT NEXT REQUEST REPORT                    
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,0           NEXT TIME MONACC WILL PASS REQFRST           
         B     XXIT                                                             
         DROP  R4,RF                                                            
         TITLE 'BASE - LAST FOR RUN'                                            
***********************************************************************         
* LAST FOR RUN                                                        *         
***********************************************************************         
         SPACE 1                                                                
RNL00    TM    UPSI,TIME                                                        
         BNO   RNL05                                                            
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         MVI   FORCEHED,YES                                                     
         MVC   XP+1(7),=C'ROUTINE'                                              
         MVC   XP+15(7),=C'ENTERED'                                             
         MVC   XP+25(4),=C'TIME'                                                
         GOTO1 ACREPORT                                                         
         LA    R4,TTAB                                                          
*                                                                               
RNL03    EDIT  (P4,4(R4)),(7,XP+15)                                             
         L     R0,0(R4)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C'.'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVI   WORK+5,C'.'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         MVC   XP+25(8),WORK       CPU TIME                                     
         MVC   XP+1(8),8(R4)       ROUTINE                                      
         GOTO1 ACREPORT                                                         
         LA    R4,16(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   RNL03                                                            
*                                                                               
RNL05    LM    R0,R1,MAINLEN       LENGTH AND ADDRESS                           
         FREEMAIN R,LV=(0),A=(1)                                                
         B     XIT                                                              
         DROP  R6                                                               
         TITLE 'BASE - PROCESS BUDGETS'                                         
***********************************************************************         
* CONTROL BUDGET I/O                                                  *         
* R6 = A(ACCOUNT RECORD AT ANY LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BDGT     NTR1  ,                                                                
         LR    R5,R6               SAVE A(INPUT RECORD)                         
         LA    R6,DKEY                                                          
         USING BUDRECD,R6                                                       
         XC    BUDKEY,BUDKEY       BUILD KEY FOR BUDGET RECORD                  
         MVI   BUDKTYP,X'1B'                                                    
         MVC   BUDKCPY(15),0(R5)                                                
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY(15),SPACES                                              
         MVC   KEYSAVE,BUDKEY                                                   
         GOTO1 ADMGR,HIGH                                                       
         LA    R6,DIR                                                           
         CLC   BUDKCPY(15),0(R5)                                                
         BE    BDGT7                                                            
         B     BDGTX                                                            
*                                                                               
BDGT3    GOTO1 ADMGR,RSEQ                                                       
         CLC   BUDKCPY(15),0(R5)   SAME ACCOUNT                                 
         BNE   BDGT5               POST LAST                                    
         CLC   BUDKEY(33),BDGTKEY  CHANGE OF 'CONTRA'                           
         BNE   BDGT5               POST LAST                                    
         CLC   BUDKBCKT,BDGTKEY+(BUDKBCKT-BUDRECD)   CHANGE OF BUCKET           
         BE    BDGT7               GET NEXT                                     
*                                                                               
BDGT5    MVC   BDGTKEY,BUDKEY      THE CURRENT KEY                              
         MVC   CURROFC,SPACES                                                   
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   BYTE,ACMOFPOS       OFFICE POSITION                              
         NI    BYTE,X'FF'-X'40'                                                 
         CLI   BYTE,1              CHECK OFFICE IN KEY                          
         BL    BDGT6                                                            
         CLI   BYTE,12                                                          
         BH    BDGT6                                                            
         SR    R1,R1                                                            
         TM    ACMOFPOS,X'40'       2 BYTE OFFICE                               
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         DROP  RF                                                               
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         LA    RF,2(R5,RF)         RF = A(OFFICE CODE)                          
*MN      EX    R1,*+4                                                           
         MVC   CURROFC(0),0(RF)                                                 
         EX    R1,*-6                                                           
*MN                                                                             
*                                                                               
BDGT6    MVI   PSTLV,LSBUD         POST BUDGET DATA                             
         CLI   HOOKMODE,0          IF THERE'S A HOOK                            
         BNE   *+8                 DON'T POST THE BUDGET                        
         BAS   RE,MLTP             MULTIPLE POSTINGS                            
         MVI   CLRLV,LSBUD         SET TO CLEAR BUDGETS                         
         BAS   RE,CLR              CLEAR BUDGET LINES                           
         CLC   BDGTKEY+1(15),0(R5) FINISHED ACCOUNT                             
         BNE   BDGTX                                                            
         LA    R6,DKEY                                                          
         MVC   BUDKEY,BDGTKEY                                                   
         L     RF,AMONACC                                                       
         LA    RF,ACMACDIR-ACMD(RF)                                             
         GOTO1 DATAMGR,DMCB,DMKEY,(RF),WORK                                     
         CLC   BUDKEY,WORK         IS IT THE SAME RECORD?                       
         BE    BDGT7                                                            
         GOTO1 ADMGR,HIGH          CHANGED BY MLTP, READ LAST RECORD            
*                                                                               
BDGT7    LA    R6,DIR                                                           
         BAS   RE,BDGP             POST THIS BUDGET RECORD                      
         MVC   BDGTKEY,BUDKEY                                                   
         B     BDGT3                                                            
*                                                                               
BDGTX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* POST TO ALL BUDGETS LINES                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R6                                                       
BDGP     NTR1  ,                                                                
         MVC   CURRCON,BUDKCCPY    CONTRA                                       
         MVC   CURRWC,BUDKWORK     WORKCODE                                     
         MVC   CURRBKT,BUDKBCKT    BUCKET TYPE                                  
         OC    CURRBKT,SPACES                                                   
         MVC   CURRCNAM,SPACES                                                  
         MVI   CURRTYPE,0                                                       
         MVI   CURRSTAT,0                                                       
         LA    R3,BUDGLIST         POSITION R3 TO MATCHING BUDGET               
         LA    R4,LNBG1            ACCUMULATOR LINE (41-55)                     
         LA    R0,15                                                            
*                                                                               
BDGP3    LA    R6,DIR                                                           
         CLC   0(2,R3),BUDKBUDN    MATCH ON NUMBER                              
         BNE   BDGP5                                                            
         L     R6,AIO1                                                          
         CLC   BUDKEY,DIR          TEST BUDGET RECORD IN IO                     
         BE    BDGP3A                                                           
         GOTO1 ADMGR,GETR                                                       
BDGP3A   CLI   HOOKMODE,0          IS THERE A HOOK                              
         BE    BDGP4               NO HOOK                                      
         LR    R5,R6               SAVE R6                                      
         LA    R6,SRCHARG          BUILD SEARCH ARGUMENT FOR TO ACCOUNT         
         USING ARGD,R6                                                          
         MVC   ARGACC,ACTACC+1     ACCOUNT                                      
         MVC   ARGFLT,CURRFLT      FILTERS                                      
         MVC   ARGBKT,CURRBKT      BUCKET TYPE                                  
         MVC   ARGCON,CURRCON+1    CONTRA                                       
         GOTO1 ASRCH               CHECK FOR SUBSTITUTION                       
         LR    R6,R5               RESTORE R6                                   
         TM    SLOPTN,SLFND        WAS IT FOUND                                 
         BNO   BDGP5               NOT FOUND                                    
         MVI   MODE,PROCBUD                                                     
         BAS   RE,GOHK             GO TO THE HOOK                               
         B     BDGP5                                                            
*                                                                               
BDGP4    BAS   RE,BDGL             POST BUDGET ELEMENTS                         
*                                                                               
BDGP5    LA    R3,2(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,BDGP3                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* POST BUDGETS ELEMENTS BY MONTH                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R6                                                       
BDGL     NTR1  ,                                                                
         LA    R6,BUDRFST          NOW LOOK FOR AMOUNTS ELEMENTS                
BDGL1    CLI   0(R6),0                                                          
         BE    BDGTX                                                            
         CLI   0(R6),X'1D'                                                      
         BE    BDGL5                                                            
BDGL3    SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     BDGL1                                                            
*                                                                               
         USING BAMELD,R6                                                        
BDGL5    CLC   BAMMNTH,MONLIST+4   TEST BEFORE START                            
         BL    BDGL3                                                            
         CLC   BAMMNTH,MONLAST+4   AFTER END                                    
         BH    BDGL3                                                            
         LA    R3,BAMMNTH          GET RELATIVE MONTH NUMBER                    
         BAS   RE,GRELMON                                                       
         TM    PCSW,PCBD           POSTING BY BUCKET DATE                       
         BNO   BDGL11                                                           
         MVC   CURRDATE(2),BAMMNTH                                              
         MVI   CURRDATE+2,1                                                     
         GOTO1 DATCNV,DMCB,(1,CURRDATE),CURRDATE                                
         OI    PCSW,PCPT           POST THIS TIME                               
         BAS   RE,PSB              POST SUB-ACCOUNT                             
         NI    PCSW,ALL-PCPT                                                    
*                                                                               
BDGL11   ZAP   PAMT,BAMBUDG                                                     
         BAS   RE,PACC             ADD INTO ACCUMS                              
         B     BDGL3                                                            
         DROP  R6                                                               
         TITLE 'BASE - SET TYPE FOR NON-CLIENT TIME'                            
***********************************************************************         
* SET TYPE FOR NON-CLIENT TIME                                        *         
***********************************************************************         
         SPACE 1                                                                
NOCLI    CLC   CURRCON+1(2),NONCUL                                              
         BNER  RE                                                               
         MVI   CURRNT,C'N'         DEFAULT IS OTHER NON-CLIENT                  
         TM    RNSW,RNNC           NEW COSTING                                  
         BNO   NOCLI07                                                          
         L     R3,APELVT           TABLE OF PE AND LEAVE ACCOUNTS               
*                                                                               
NOCLI05  CLI   0(R3),X'FF'         END OF TABLE                                 
         BER   RE                                                               
         CLC   CURRCON+1(14),0(R3) MATCH CONTRA                                 
         BNE   *+12                                                             
         MVC   CURRNT,14(R3)        SET TYPE FROM TABLE                         
         BR    RE                                                               
         LA    R3,PETLN(R3)                                                     
         B     NOCLI05                                                          
*                                                                               
NOCLI07  CLC   CURRCON+2(2),=C'NL' LEAVE TIME                                   
         BNE   *+10                                                             
         MVI   CURRNT,C'L'                                                      
         BR    RE                                                               
         CLC   CURRCON+2(2),=C'NP' PERSONAL                                     
         BNER  RE                                                               
         MVI   CURRNT,C'P'                                                      
         BR    RE                                                               
         TITLE 'BASE - COMPUTE BILLING AMOUNTS'                                 
***********************************************************************         
* COMPUTE BILLING AMOUNTS (RATE * HOURS)                              *         
***********************************************************************         
         SPACE 1                                                                
BLAMS    LR    R0,RE                                                            
         LH    R1,=Y(LNBLR*WLN)    R1 = BILLING RATE LINE                       
         LH    R2,=Y(LNHRS*WLN)    R2 = HOURS                                   
         LH    R3,=Y(LNBRH*WLN)    R3 = RESULT (RATE * HOURS)                   
         LA    R4,LNBRH                                                         
         BAS   RE,BLAMALL                                                       
         LH    R1,=Y(LNSTR*WLN)    R1 = STANDARD RATE LINE                      
         LH    R2,=Y(LNHRS*WLN)    R2 = HOURS                                   
         LH    R3,=Y(LNST$*WLN)    R3 = STANDARD DOLLARS                        
         LA    R4,LNST$                                                         
         BAS   RE,BLAMALL                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
BLAMT    LR    R0,RE                                                            
         LH    R1,=Y(LNSTR*WLN)    R1 = STANDARD RATE LINE                      
         LH    R2,=Y(LNTHR*WLN)    R2 = TRANSACTION HOURS                       
         LH    R3,=Y(LNST$*WLN)    R3 = STANDARD DOLLARS                        
         LA    R4,LNST$                                                         
         BAS   RE,BLAMALL                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
BLAMALL  LA    RF,NAC              NUMBER OF COLUMNS                            
         A     R1,AZRO                                                          
         A     R2,AZRO                                                          
         A     R3,AZRO                                                          
         A     R4,ALNSTL           R4=STATUS BYTE                               
BLAMALL1 ZAP   WRK,0(PFL,R2)       HOURS                                        
         MP    WRK,0(PFL,R1)       * RATE                                       
         SRP   WRK,64-2,5          ROUNDED                                      
         ZAP   0(PFL,R3),WRK                                                    
         CP    0(PFL,R3),=P'0'                                                  
         BE    *+8                                                              
         OI    0(R4),LSACT         SET ACTIVITY                                 
         LA    R1,PFL(R1)                                                       
         LA    R2,PFL(R2)                                                       
         LA    R3,PFL(R3)                                                       
         BCT   RF,BLAMALL1                                                      
         BR    RE                                                               
         TITLE 'BASE - COMPUTE PERSONAL HOURLY DOLLARS'                         
***********************************************************************         
* COMPUTE PERSONAL HOURLY DOLLARS(RATES * HOURS)                      *         
***********************************************************************         
         SPACE 1                                                                
PHRDLR   LR    R0,RE                                                            
         SR    R3,R3                                                            
         IC    R3,RELMON           DISPLACEMENT TO MONTH COLUMN                 
         SLL   R3,3                X WIDTH OF COLUMN                            
         SR    R5,R5                                                            
         IC    R5,RELEND           DISPLACEMENT TO END MONTH COLUMN             
         SLL   R5,3                X WIDTH OF COLUMN                            
         LH    RF,=Y(LNTHR*WLN)    TRANSACTION HOURS                            
         A     RF,AZRO             PLUS START                                   
         AR    RF,R3               PLUS COLUMN                                  
         CP    0(PFL,RF),=P'0'     NO HOURS                                     
         BE    PHRDLRX                                                          
         TM    BFLT2,BTFLVE+BTFPER LEAVE OR PERSONAL                            
         BNZ   PHRDLRX             HAVE NO COST                                 
         LH    R1,=Y(LNPSR*WLN)    R1 = SALARY RATE                             
         LH    R2,=Y(LNPS$*WLN)    R2 = SALARY DOLLARS                          
         LA    R6,LNPS$                                                         
         BAS   RE,PHRDLR9                                                       
         LH    R1,=Y(LNPBR*WLN)    R1 = BENEFIT RATE                            
         LH    R2,=Y(LNPB$*WLN)    R2 = BENEFIT DOLLARS                         
         LA    R6,LNPB$                                                         
         BAS   RE,PHRDLR9                                                       
         LH    R1,=Y(LNPPR*WLN)    R1 = PENSION RATE                            
         LH    R2,=Y(LNPP$*WLN)    R2 = PENSION DOLLARS                         
         LA    R6,LNPP$                                                         
         BAS   RE,PHRDLR9                                                       
         LH    R1,=Y(LNPTR*WLN)    R1 = TOTAL RATE                              
         LH    R2,=Y(LNPT$*WLN)    R1 = TOTAL DOLLARS                           
         LA    R6,LNPT$                                                         
         BAS   RE,PHRDLR9                                                       
PHRDLRX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PHRDLR9  A     R1,AZRO             ADD START OF ACCUMULATORS                    
         AR    R1,R5               PLUS END COLUMN FOR YTD RATE                 
         ZAP   WRK,0(PFL,R1)       RATE                                         
         MP    WRK,0(PFL,RF)       X TRANSACTION HOURS                          
         SRP   WRK,64-4,5          ROUNDED                                      
         A     R6,ALNSTL           R6=LINE STATUS BYTE                          
         A     R2,AZRO             ADD START OF ACCUMULATORS                    
         AR    R2,R3               PLUS COLUMN FOR PERIOD                       
         ZAP   0(PFL,R2),WRK       SAVE DOLLAR AMOUNT                           
         CP    0(PFL,R2),=P'0'                                                  
         BE    *+8                                                              
         OI    0(R6),LSACT         SET ACTIVITY                                 
         BR    RE                                                               
         TITLE 'BASE - COMPUTE CURRENT MONTH COST'                              
***********************************************************************         
* COMPUTE CURRENT MONTH COST/ADJ COST/MIS COST                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CCSTD,R6                                                         
CCST     CLI   CURRBKT,C' '                                                     
         BNHR  RE                                                               
CCST1    NTR1 ,                                                                 
         L     R6,ASPBTAB                                                       
CCST5    CLC   CCSTTYP,CURRBKT      MATCH BUCKET TYPE                           
         BE    CCST7                                                            
CCST6    LA    R6,CCSTLQ(R6)                                                    
         CLI   0(R6),EOT            END OF TABLE                                
         BNE   CCST5                                                            
         B     XIT                 UNKNOWN BUCKET TYPE                          
*                                                                               
CCST7    ST    R6,SVR6                                                          
         LA    RF,YTD               COMPUTE CURRENT COST                        
         BAS   RE,CCAL                                                          
         LA    RF,LASTYTD           AND LAST YEAR                               
         BAS   RE,CCAL                                                          
         MVI   BYTE,0                                                           
         LM    RE,R6,CCSTADH                                                    
         A     R1,AZRO              R1 = HOURS                                  
         A     R2,AZRO              R2 = TOTAL DOLLARS                          
         A     R3,AZRO              R3 = CURRATE RATE                           
         A     R4,AZRO              R4 = MISSING HOURS                          
         A     R5,AZRO              R5 = MISSING DOLLARS                        
         A     R6,AZRO              R6 = CURRENT COST                           
         A     RE,AZRO              RE = ADJ. HOURS                             
         A     RF,AZRO              RF = ADJ. DOLLARS                           
*                                   R0 = NUMBER OF COLUMNS                      
*                                                                               
CCST9    ZAP   0(PFL,R6),0(PFL,R2) CUR$ = TOT$                                  
         CP    0(PFL,R2),=P'0'     TOT$                                         
         BE    CCST11                                                           
         CP    0(PFL,R1),=P'0'     HOURS                                        
         BE    CCST11                                                           
         ZAP   WRK,0(PFL,R2)                                                    
         MP    WRK,=P'10000'                                                    
         DP    WRK,0(PFL,R1)       TOT$ /HOURS                                  
         ZAP   0(PFL,R3),WRK(7)    CUR RATE                                     
*                                                                               
         ZAP   WRK,0(PFL,R3)       RATE                                         
         MP    WRK,0(PFL,R4)       MISS HOURS                                   
         SRP   WRK,64-4,5          ROUNDED                                      
         ZAP   0(PFL,R5),WRK       MISS $                                       
         ZAP   WRK,0(PFL,R3)       RATE                                         
         MP    WRK,0(PFL,RE)       ADJ  HOURS                                   
         SRP   WRK,64-4,5          ROUNDED                                      
         ZAP   0(PFL,RF),WRK       ADJ  $                                       
         SP    0(PFL,R6),0(PFL,R5) LESS MISS $                                  
         SP    0(PFL,R6),0(PFL,RF) LESS ADJ  $                                  
*                                                                               
CCST11   LA    R1,PFL(R1)                                                       
         LA    R2,PFL(R2)                                                       
         LA    R3,PFL(R3)                                                       
         LA    R4,PFL(R4)                                                       
         LA    R5,PFL(R5)                                                       
         LA    R6,PFL(R6)                                                       
         LA    RE,PFL(RE)                                                       
         LA    RF,PFL(RF)                                                       
         BCT   R0,CCST9                                                         
*                                                                               
         L     R6,SVR6                                                          
         LA    R0,L'CCSTLN#                                                     
         LA    R4,CCSTLN#          SET ACTIVITY FOR EACH LINE                   
*                                                                               
CCST13   SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         A     R1,ALNSTL                                                        
         OI    0(R1),LSACT                                                      
         LA    R4,1(R4)                                                         
         BCT   R0,CCST13                                                        
         B     CCST6                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         USING CCSTD,R6                                                         
CCAL     SR    R0,R0                                                            
         IC    R0,1(RF)            END MONTH NUMBER                             
         SR    R1,R1                                                            
         IC    R1,0(RF)            START MONTH NUMBER                           
         BCTR  R1,0                                                             
         SR    R0,R1               R0 = NUMBER OF MONTHS                        
         SLL   R1,3                R1 = DISPLACEMENT TO COLUMN                  
         L     R2,CCSTRTE          R2 = RATE ACCUMS(CURRENT)                    
         A     R2,AZRO                                                          
         AR    R2,R1                                                            
         LR    R3,R2                                                            
         SH    R3,=H'8'            R3 = RATE ACCUMS(CURRENT-1)                  
         L     R4,CCSTTHR          R4 = HOURS(CURRENT)                          
         A     R4,AZRO                                                          
         AR    R4,R1                                                            
         L     R5,CCSTTO$          R5 = CURRENT COST(TOTAL)                     
         A     R5,AZRO                                                          
         AR    R5,R1                                                            
         ZAP   DUB,=P'0'           YTD HOURS                                    
         ZAP   DUB2,=P'0'          YTD-1 HOURS                                  
         MVI   BYTE,0                                                           
*                                                                               
CCAL3    AP    DUB,0(PFL,R4)       ADD THIS MONTHS HOURS                        
         ZAP   WRK,DUB             YTD HOURS                                    
         MP    WRK,0(PFL,R2)       X YTD RATE                                   
         SRP   WRK,64-4,5          ROUNDED                                      
         ZAP   0(PFL,R5),WRK       YTD COST                                     
         CP    0(PFL,R5),=P'0'                                                  
         BE    *+8                                                              
         MVI   BYTE,C'A'           SET ACTIVITY                                 
         CLC   0(1,RF),1(RF)       ONE MONTH?                                   
         BE    CCAL5                                                            
         ZAP   WRK,DUB2            YTD-1 HOURS                                  
         MP    WRK,0(PFL,R3)       X YTD-1 RATE                                 
         SRP   WRK,64-4,5          ROUNDED                                      
         SP    0(PFL,R5),WRK       YTD LESS YTD-1 COST                          
         ZAP   DUB2,DUB            SET YTD-1 HOURS                              
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,CCAL3                                                         
*                                                                               
CCAL5    CLI   BYTE,C'A'           TEST ACTIVITY                                
         BNER  RE                                                               
         SR    R1,R1               SET ACTIVITY IN LINE STATUS BYTE             
         IC    R1,CCSTLNT                                                       
         A     R1,ALNSTL                                                        
         OI    0(R1),LSACT                                                      
         BR    RE                                                               
         DROP  R6                                                               
         TITLE 'BASE - POST MANWEEKS WORKED'                                    
***********************************************************************         
* POST MANWEEKS WORKED                                                *         
***********************************************************************         
         SPACE 1                                                                
PMW      NTR1  ,                                                                
         LH    R0,MNWKS            NUMBER OF WEEKS                              
         L     R5,AMANWK           WEEK DATE ARRAY, 156 ENTRIES                 
         USING MNWKD,R5                                                         
         LA    R1,LNWKP            SET ACTIVITY FOR THIS LINE                   
         A     R1,ALNSTL                                                        
         OI    0(R1),LSACT                                                      
         LH    R2,=Y(LNWKP*WLN)    POSSIBLE WEEKS WORKED LINE                   
         A     R2,AZRO                                                          
         LH    R4,=Y(LNMTS*WLN)    MISSING TIMESHEET LINE                       
         A     R4,AZRO                                                          
         LH    R6,=Y(LNWKW*WLN)    WEEKS WORKED LINE                            
         A     R6,AZRO                                                          
         XC    HALF,HALF                                                        
*                                                                               
PMW3     SR    R3,R3                                                            
         IC    R3,MNWKNUM          RELATIVE MONTH NUMBER                        
         SLL   R3,3                X 8                                          
*                                                                               
PMW5     LA    RF,0(R3,R2)                                                      
         AP    0(8,RF),=P'1'       COUNT THE WEEKS POSSIBLE                     
         CLI   MNWKHIT,C'H'        DID HE WORK THIS WEEK                        
         BNE   PMW6                MISSED THIS WEEK                             
         LA    RF,0(R3,R6)                                                      
         AP    0(8,RF),=P'100'     ADD TO WEEKS WORKED                          
         MVI   HALF,C'H'           SET ACTIVITY WEEK WORKED                     
         CP    MNWKHRS,=P'0'       DID HOURS NET TO ZERO                        
         BNE   PMW9                                                             
*                                                                               
PMW6     CLC   HIRE,MNWKEND                                                     
         BH    PMW9                HIRED AFTER END OF PERIOD                    
         CLC   TERM,MNWKSTR                                                     
         BL    PMW9                TERMINATED BEFORE START                      
         TM    LGSW,LGPR           TEST PERSON RECORDS IN USE                   
         BNO   PMW8                                                             
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         SR    RF,RF                                                            
         ICM   RF,1,PERLNUM                                                     
         BZ    PMW9                                                             
         LA    R1,PERLVALS                                                      
         USING PERLVALS,R1                                                      
PMW6A    CLC   PERLSTD,MNWKEND                                                  
         BH    PMW7                                                             
         CLC   PERLENDD,MNWKSTR                                                 
         BNL   PMW8                                                             
PMW7     LA    R1,PERLVLEN(R1)                                                  
         BCT   RF,PMW6A                                                         
         B     PMW9                                                             
PMW8     LA    RF,0(R3,R4)                                                      
         AP    0(8,RF),=P'100'     ADD 1 TO MISSING                             
         MVI   HALF+1,C'H'         SET ACTIVITY FOR MISSING LINE                
*                                                                               
PMW9     LA    R5,MNWKLNQ(R5)      BUMP TO NEXT WEEK                            
         BCT   R0,PMW3                                                          
         CLI   HALF,C'H'           TEST ACTIVITY                                
         BNE   *+16                                                             
         LA    R1,LNWKW            SET ACTIVITY IN STATUS LINE                  
         A     R1,ALNSTL                                                        
         OI    0(R1),LSACT                                                      
         CLI   HALF+1,C'H'                                                      
         BNE   *+16                                                             
         LA    R1,LNMTS            SET ACTIVITY IN STATUS LINE                  
         A     R1,ALNSTL                                                        
         OI    0(R1),LSACT                                                      
         B     XIT                                                              
         DROP  R1,R5                                                            
         TITLE 'BASE - GENERATE PROFORMA REPORT'                                
***********************************************************************         
* GENERATE PROFORMA REPORT                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R6                                                       
GPR      CLC   GPRLDG,ACTKEY+1     TEST LEDGER TO CONTROL REPORT                
         BNER  RE                                                               
         SR    R1,R1                                                            
         IC    R1,GPRLV            CURRENT ACCOUNT LEVEL                        
         EX    R1,*+6                                                           
         BNOR  RE                                                               
         TM    ACTSW,0             TEST ACTIVITY AT THIS LEVEL                  
         EX    R1,*+6                                                           
         BNOR  RE                                                               
         TM    GPRSW,0             TEST CORRECT LEVEL FOR REPORT                
*                                                                               
         USING SLPD,R2                                                          
GPR1     NTR1  ,                                                                
         OI    PCSW,PCGPR          SET POSTING PROFORMA SWITCH                  
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
         L     R2,ASLP             SUPERLEDGER TABLE                            
         L     R0,SLPNUM           NUMBER OF SUPERLEDGER ENTRIES                
*                                                                               
GPR3     CLC   GPRLDG,SLPFA1C      TEST LEDGER CODE                             
         BNE   GPR5                                                             
         TM    SLPSTAT,SLPGPR      ALREADY POSTED THIS ACCOUNT                  
         BO    GPR5                                                             
         MVC   CURRCON(1),RCCOMPFL SET FROM COMPANY AND ACCOUNT                 
         MVC   CURRCON+1(14),SLPFC1C                                            
         MVC   CURRBKT,SLPBKT                                                   
         MVI   PSTLV,LSACC         POST ACCOUNT DATA                            
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         BAS   RE,MLTP             POST TO ALL REPORTS                          
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
*                                                                               
GPR5     NI    SLPSTAT,ALL-SLPGPR  CLEAR GPR SWITCH                             
         LA    R2,SLPLNQ(R2)       BUMP TO NEXT SUPERLEDGER ENTRY               
         BCT   R0,GPR3                                                          
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         NI    PCSW,ALL-PCGPR      CLEAR POST CONTROL                           
         B     XIT                                                              
         DROP  R2,R6                                                            
         TITLE 'BASE - REPORT POSTING CONTROL'                                  
***********************************************************************         
* CONTROL MULITPLE POSTINGS THRU SUPERLEDGER LINKED ENTRIES           *         
***********************************************************************         
         SPACE 1                                                                
MLTP     NTR1  ,                                                                
         XC    ASLNTRY,ASLNTRY     CLEAR ADDRESS OF CURRENT ENTRY               
         OC    ACTACC,ACTACC       FIRST TIME - NO ACCOUNT                      
         BZ    XIT                                                              
         NI    HOOKSTAT,ALL-HOOKSSND   TURN OFF SECOND TIME SWITCH              
         TM    OPT1,FOH                OPTION TO USE FIXED O/H                  
         BNO   MLTP1                   NO, OK TO POST                           
         TM    PCSW,PCFOH              POSTING FIXED O/H NOW                    
         BO    MLTP1                   YES, ALL DONE                            
         CLC   CURRCON+1(2),OVRHUL     IS THIS OVERHEAD                         
         BE    XIT                     SKIP IT                                  
         CLC   CURRCON+1(2),INDLUL                                              
         BE    XIT                                                              
*                                                                               
MLTP1    TM    SLOPTN,SLINIT       IS THERE A SUPERLEGDER                       
         BO    MLTP2               POST THE LINKED LIST                         
         BAS   RE,POST             POST ONE ITEM                                
         B     MLTP6               AND CHECK FOR FIXED OVERHEAD                 
*                                                                               
         USING ARGD,R6                                                          
MLTP2    LA    R6,SRCHARG          BUILD SEARCH ARGUMENT FOR TO ACCOUNT         
         MVC   ARGACC,ACTACC+1     ACCOUNT                                      
         MVC   ARGFLT,CURRFLT      FILTERS                                      
         MVC   ARGBKT,CURRBKT      BUCKET TYPE                                  
         MVC   ARGCON,CURRCON+1    CONTRA                                       
         GOTO1 ASRCH               CHECK FOR SUBSTITUTION                       
*                                                                               
         USING SLPD,R2                                                          
         TM    SLOPTN,SLFND            WAS IT FOUND                             
         BNO   XIT                     NOT FOUND                                
         MVC   ASLNTRY,ARGRTN          SAVE ADDRESS OF CURRENT ENTRY            
         L     R2,ASLPWRK              SAVE CURRENT ENTRY                       
         MVC   CURRSIGN,SLPSIGN        SET SIGN                                 
         MVC   CURRACC+1(14),SLPTACC   ACCOUNT CODE                             
         MVC   CURRNAME,SLPTNME        ACCOUNT NAME                             
         BAS   RE,POST                 POST TO THE "TO" ACCOUNT                 
         OC    SLPLNK,SLPLNK           IS THERE A LINK ENTRY                    
         BZ    XIT                     NO LIST                                  
         OI    HOOKSTAT,HOOKSSND       TURN ON SECOND PASS THRU POST            
*MNNEW   LA    RF,*+10                 SWITCH TO 31 BIT MODE                    
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
*                                                                               
         USING LNKD,R5                                                          
         L     R5,SLPLNK                                                        
         L     R0,LNKNUM           NUMBER OF ENTRIES IN THIS LIST               
         LR    R1,R5                                                            
         AHI   R1,(LNKCHLD-LNKD)   R1 TO FIRST LINK FIELD                       
*                                                                               
MLTP3    ICM   R2,15,0(R1)         A(SUPERLEDGER ENTRY)                         
         STCM  R2,15,ASLNTRY       SAVE CURRENT ENTRY                           
         L     RE,ASLPWRK                                                       
         MVC   0(SLPLNQ,RE),0(R2)                                               
         MVI   CURRSIGN,C'+'       ASSUME POSTIVE                               
         TM    0(R1),X'80'         IS IT A NEGATIVE                             
         BNO   *+8                                                              
         MVI   CURRSIGN,C'-'       MAKE IT A SUBTRACT                           
         MVC   CURRACC+1(14),SLPTACC ACCOUNT CODE                               
         MVC   CURRNAME,SLPTNME         "     NAME                              
*                                                                               
         TM    PCSW,PCGPR          POSTING GPR                                  
         BNO   MLTP4                                                            
         TM    SLPSTAT,SLPGPR      ALREADY POSTED THIS ACCOUNT                  
         BO    MLTP5                                                            
*                                                                               
MLTP4    DS    0H                                                               
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         BAS   RE,POST                                                          
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
*                                                                               
MLTP5    AHI   R1,4                                                             
         BCT   R0,MLTP3                                                         
*                                                                               
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
*                                                                               
MLTP6    TM    OPT1,FOH            OPTION TO USE FIXED O/H                      
         BNO   XIT                 NO, OK TO GETOUT                             
         TM    PCSW,PCFOH          JUST POSTED O/H                              
         BO    MLTP7               YES, ALL DONE                                
         CLC   CURRCON+1(2),DIRLUL IS THIS DIRECT LABOR                         
         BNE   XIT                 NOT TIME TO POST IT                          
         LA    RE,CNSLTT           LIST OF CONSULTANTS                          
         CLC   CURRCON+3(1),0(RE)  IF CONSULTANT- EXCLUDE                       
         BE    XIT                                                              
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    *+12                PROCESS THIS CONTRA                          
         LA    RE,1(,RE)           NEXT CONSULTANT                              
         B     *-22                                                             
*                                                                               
         MVC   DIRCON,CURRCON          SAVE CONTRA FOR DIRECT                   
         MVC   CURRCON+1(14),SPACES                                             
         MVC   CURRCON+1(2),OVRHUL     CHANGE CONTRA TO O/H                     
         MVC   CURRCON+3(5),DIRCON+4   OFFICE/DEPARTMENT                        
         OI    PCSW,PCFOH              SET O/H POSTING SWITCH                   
         BAS   RE,MLTP                 NOW POST THE OVERHEAD                    
         B     XIT                                                              
*                                                                               
MLTP7    MVC    CURRCON,DIRCON     RESTORE DIRECT CONTRA                        
         NI     PCSW,ALL-PCFOH     TURNOFF POSTING O/H SWITCH                   
         B      XIT                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* GENERATE RECORDS FOR EACH REPORT IN STACK                           *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
POST     NTR1  ,                                                                
         LA    R2,1                START WITH REPORT 1                          
*                                                                               
POST2    STC   R2,REPCODE                                                       
         BCTR  R2,0                                                             
         MHI   R2,RPTLN            REPORT NUMBER X LENGTH                       
         A     R2,ARSTACK          START OF STACK                               
         TM    CHASW,CHACON        TEST NEW CONTRA ACCOUNT                      
         BNO   *+8                                                              
         NI    RSMWR,ALL-RSMPCON   CLEAR CONTRA-POSTED                          
         TM    CHASW,CHACLI        TEST NEW CLIENT                              
         BNO   *+8                                                              
         NI    RSMWR,ALL-RSMPCLI   CLEAR CLIENT-POSTED                          
*&&US                                                                           
         TM    RDSW,RDDTL          FOR DETAIL POSTING ONLY                      
         BNO   POST3                                                            
         TM    RSOPT2,RSNOREVL     ELIMINATED REVERSALS                         
         BNO   POST3                                                            
         TM    CURRSTAT,X'20'      IS IT A REVERSAL?                            
         BO    POST17              YES, SKIP IT                                 
*&&                                                                             
POST3    TM    LPLSW,LPLPL         OK TO PROCESS LITERAL POOL                   
         BNO   POST5                                                            
         MVI   BYTE,CPOOLRS        GET RIGHT SIDE                               
         BAS   RE,GETLPL           FROM LITERAL POOL                            
         BNE   *+10                                                             
         MVC   RSRIGHT,WORK        MOVE TO RSTACK ENTRY                         
*                                                                               
         XC    RSFOOT1,RSFOOT1                                                  
         XC    RSFOOT2,RSFOOT2                                                  
         MVI   BYTE,CPOOLFL        GET FOOTLINE                                 
         BAS   RE,GETLPL                                                        
*                                                                               
POST5    CLI   FCTR,YES            HAS CONV FACTOR BEEN EDITED                  
         BNE   *+8                 YES (IF FCTR=N)                              
         BAS   RE,SCNVR                                                         
         LA    R1,RSIGNORE         CHECK IGNORE CONDITIONS                      
         LA    R0,6                                                             
*                                                                               
POST6    CLI   0(R1),0                                                          
         BE    POST8                                                            
         MVC   CONCODE,0(R1)                                                    
         BAS   RE,CONEX                                                         
         BE    POST17                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,POST6                                                         
*                                                                               
POST8    LA    R1,RSFILTER         CHECK FILTER CONDITIONS                      
         LA    R0,6                                                             
*                                                                               
POST10   CLI   0(R1),0                                                          
         BE    POST12                                                           
         MVC   CONCODE,0(R1)                                                    
         BAS   RE,CONEX                                                         
         BNE   POST17                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,POST10                                                        
*                                                                               
POST12   NI    PCSW,ALL-PCACT      TURNOFF ACTIVITY SWITCH                      
         BAS   RE,BCLS             BUILD COLUMNS                                
         TM    PCSW,PCGPR          POSTING THE PROFORMA OPTION                  
         BO    POST13              POST ZERO DATA                               
         TM    OPT1,SZD            INCLUDE ZERO DATA LINES                      
         BO    POST13                                                           
         TM    PCSW,PCACT          TEST ANY ACTIVITY                            
         BNO   POST17                                                           
*                                                                               
POST13   SR    R1,R1                                                            
         IC    R1,RSCOPY                                                        
*                                                                               
POST14   STC   R1,COPYNO           HANDLE MULTIPLE COPIES                       
         LH    R3,LISTNUM          NUMBER IN RLPOOL                             
*                                                                               
POST15   STH   R3,LISTPNT                                                       
         BAS   RE,BSR              BUILD SORT RECORD                            
         CLC   RLLEDG,SPACES       SEE IF WE HAVE LIST                          
         BE    POST16                                                           
         CLC   RLLEDG+2(2),READLEDG                                             
         BNE   POST16              FOR THIS LEDGER                              
         BCT   R3,POST15                                                        
*                                                                               
POST16   BCT   R1,POST14                                                        
*                                                                               
POST17   SR    R2,R2                                                            
         IC    R2,REPCODE                                                       
         LA    R2,1(,R2)                                                        
         C     R2,NRSTACK                                                       
         BNH   POST2                                                            
*                                                                               
POST20   NI    LPLSW,ALL-LPLPL     TURN OFF PROCESS AFTER FIRST PASS            
         MVI   FCTR,NO             LIKEWISE WITH CURR CONV FACTOR               
         B     XIT                                                              
         DROP  R2                                                               
         TITLE 'BASE - POST COLUMN DETAILS'                                     
***********************************************************************         
* ROUTINE TO BUILD COLUMNS                                            *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
BCLS     NTR1  ,                                                                
         CLI   FORMBASE,C'C'       TEST CALENDAR BASIS                          
         BNE   *+8                                                              
         BAS   RE,CALBSE           CALENDAR BASIS                               
         L     R0,RSNCOLS          NUMBER OF COLUMNS                            
         LA    R3,RSCOLS           COLUMN SPEC                                  
         LA    R4,SORTCOLS                                                      
         MVI   CURRCOL,0                                                        
*                                                                               
BCLS5    ZAP   0(8,R4),=P'0'                                                    
         BAS   RE,COLS             GET TYPE AND CHECK CONDITIONS                
         CP    0(8,R4),=P'0'       CHECK ACTIVITY                               
         BE    BCLS7                                                            
         OI    PCSW,PCACT          SET ACTIVITY INDICATOR                       
         ST    R0,SVR0                                                          
         BAS   RE,CADJ             SPECIAL COLUMN ADJUSTMENT                    
         L     R0,SVR0                                                          
*                                                                               
BCLS7    L     R5,ATYPL            A(CURRENT TYPELIST ENTRY)                    
         USING TYPLD,R5                                                         
         TM    TYPLS1,TYPNAC       NON-ADDITIVE COLUMN                          
         BNO   *+8                                                              
         NI    7(R4),X'FF'-X'08'   FLAG AS NO ADDITION                          
         LA    R3,29(R3)           COLUMN SPEC                                  
         LA    R4,8(R4)            COLUMN BUCKET                                
         IC    RF,CURRCOL          COLUMN NUMBER                                
         LA    RF,1(RF)                                                         
         STC   RF,CURRCOL                                                       
         BCT   R0,BCLS5                                                         
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SCAN LINE 18 FOR BUCKETS WITH 0 VALUE. CLEAR CORRESPONDING LINE 37  *         
* BUCKETS. NO TOTAL TIME IN A MONTH WILL NOW MEAN NO STANDARD TIME.   *         
***********************************************************************         
         SPACE 1                                                                
CALBSE   LR    R0,RE                                                            
         L     R1,ATYPCTT          CHANGE PERCENT FROM 18 TO 37                 
         MVI   0(R1),LNSWH                                                      
         CLI   ADJHOURS,YES        DO WE NEED TO ADJUST STNDRD HOURS            
         BNER  RE                  NO                                           
         LH    RE,=Y(LNC30*WLN)    LINE 30 CONTAINS COPY OF STNDRD HOUR         
         A     RE,AZRO             RE=A(STANDARD LINE)                          
         LA    RF,WLN              RF=WIDTH OF A LINE                           
         LH    R4,=Y(LNSWH*WLN)    LINE 37(STANDARD WORK HOURS)                 
         A     R4,AZRO                                                          
         LR    R5,RF                                                            
         MVCL  R4,RE               RESTORE STANDARD WORK HOURS                  
         LH    RE,=Y(LNTLT*WLN)    RE=LINE 18 (TOTAL TIME)                      
         A     RE,AZRO                                                          
         LH    RF,=Y(LNSWH*WLN)    RF=LINE 37(STANDARD WORK HOURS)              
         A     RF,AZRO                                                          
         LA    R3,NAC              NUMBER OF MNTHLY BKTS EACH LINE              
*                                                                               
CALBSE3  CP    0(8,RE),=P'0'                                                    
         BNE   *+10                                                             
         ZAP   0(8,RF),=P'0'                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R3,CALBSE3                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK COLUMN CONDITIONS AND GET COLUMN TYPE                         *         
*  R2=RSTACK                                                          *         
*  R3=RSCOLS                                                          *         
*  R4=SORTCOL                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
COLS     NTR1  ,                                                                
         MVI   CURRTYPE,KWBL0      PRESET TO BAL0                               
         MVC   ATYPL,ATYPEBL0      DEFAULT IS BAL0                              
         MVI   REVSIGN,C'+'        PRESET TO ADD                                
         MVC   CURRPER,=X'0100'    FOR PERIOD                                   
         XC    FORMULA,FORMULA                                                  
         XC    FORMULA2,FORMULA2                                                
         LA    R1,RSALLCOL         'ALL' COLUMNS                                
         LA    R0,3                                                             
*                                                                               
COLS3    ST    R0,SVR0                                                          
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         BAS   RE,COLV             SET COLUMN VALUES                            
         BNE   XIT                                                              
         LA    R1,2(R1)                                                         
         L     R0,SVR0                                                          
         BCT   R0,COLS3                                                         
*                                                                               
         OC    0(8,R3),0(R3)       IF COL HAS NO KEYS                           
         BZ    XIT                 LEAVE IT EMPTY                               
         MVC   ORIGKEYS,0(R3)      SAVE THE KEY DATA                            
         LA    R1,ORIGKEYS         (IT MAY BE CHANGED BY HOOK)                  
         LA    R0,4                                                             
*                                                                               
COLS5    ST    R0,SVR0             "SPECIFIC" COLUMNS                           
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         BAS   RE,COLV             SET COLUMN VALUES                            
         BNE   XIT                                                              
         LA    R1,2(R1)                                                         
         L     R0,SVR0                                                          
         BCT   R0,COLS5                                                         
*                                                                               
         ZAP   WORK(16),=P'0'      VALUE IS CARRIED IN WORK(16)                 
         ZAP   WORK+16(16),=P'0'                                                
         L     R5,ATYPL            ADDRESS OF CURRENT TYPE ENTRY                
         USING TYPLD,R5                                                         
         TM    TYPLS1,TYPNON       IS IT NON-LINE DATA (IE. BBF)                
         BO    COLS13                                                           
         SR    RF,RF                                                            
         IC    RF,TYPLLN           LINE NUMBER                                  
         A     RF,ALNSTL           + START OF LINE TABLE                        
         SR    R1,R1                                                            
         IC    R1,PSTLV            POST LEVEL                                   
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RF),0             TEST LEVEL WE WANT TO POST                   
         BZ    XIT                                                              
*                                                                               
         TM    TYPLS1,TYPMWR       TEST MWR COLUMN                              
         BNO   COLS15                                                           
         TM    PCSW,PCMWR          TEST TIME TO POST MAN-WEEK-RATIOS            
         BNO   XIT                                                              
         LA    R1,RSMPCON          SET TO TEST CONTRA                           
         TM    RSMWR,RSMCLI        TEST CLIENT LEVEL REPORT                     
         BNO   *+8                                                              
         LA    R1,RSMPCLI          SET FOR CLIENT LEVEL                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    RSMWR,0             TEST MWR POSTED FOR THIS LEVEL               
         BNO   COLS15              NOT POSTED - DO IT NOW                       
         B     XIT                                                              
*                                                                               
COLS13   TM    PSTLV,LSNON         POSTING NON-LINE DATA?                       
         BNO   XIT                 NO                                           
         B     COLS23                                                           
*                                                                               
COLS15   SR    R3,R3                                                            
         IC    R3,TYPLLN           R3 = ACCUMULATOR LINE NUMBER                 
         BAS   RE,PCOL             GET VALUE INTO DUB                           
         MVC   BYTE,TYPLOP         SET OPERATOR                                 
         BAS   RE,OPR              APPLY OPERATOR                               
         SR    R6,R6                                                            
         ICM   R6,3,TYPLFM                                                      
         BZ    COLS19                                                           
         AR    R6,RA               R6 TO FORMULA TABLE                          
*                                                                               
COLS17   IC    R3,0(R6)            R3 = LINE NUMBER                             
         BAS   RE,PCOL             GET VALUE INTO DUB                           
         MVC   BYTE,1(R6)          SET OPERATOR                                 
         BAS   RE,OPR              APPLY OPERATOR                               
         LA    R6,2(R6)            NEXT FORMULA ENTRY                           
         CLI   0(R6),X'FF'                                                      
         BNE   COLS17                                                           
*                                                                               
COLS19   CP    WORK+24(8),=P'0'    WAS COMPLEX EQUATION USED                    
         BE    *+10                NO -                                         
         ZAP   WORK+8(8),WORK+24(8) YES-USE WORK+24 NOT WORK+8                  
         MVC   0(8,R4),WORK+8      RETURN RESULT IN A(R4)                       
         CLI   BUCKD,YES           ROUND COMING OUT OF ACCUMS                   
         BNE   XIT                                                              
*                                                                               
         CLI   CURRTYPE,KWHRS      BUCKET$ SHOULD NOT ROUND HOURS               
         BL    COLS21              OR PCT THAT RELATE TO HOURS                  
         CLI   CURRTYPE,KWPCT                                                   
         BNH   XIT                                                              
         CLI   CURRTYPE,KWMWR                                                   
         BL    COLS21                                                           
         CLI   CURRTYPE,KWCLH                                                   
         BNH   XIT                                                              
*                                                                               
COLS21   SRP   0(8,R4),62,5        ROUND TO NEAREST DOLLAR                      
         SRP   0(8,R4),2,0         00 FOR PENNY POSITIONS                       
         B     XIT                                                              
*                                                                               
COLS23   ZAP   0(8,R4),BBF                                                      
         CLI   BUCKD,YES           ROUND COMING OUT OF ACCUMS                   
         BNE   COLS25                                                           
         SRP   0(8,R4),62,5        ROUND TO NEAREST DOLLAR                      
         SRP   0(8,R4),2,0         00 FOR PENNY POSITIONS                       
*                                                                               
COLS25   CLI   CURRSIGN,C'+'                                                    
         BE    *+10                                                             
         MP    0(8,R4),=P'-1'                                                   
         CLI   REVSIGN,C'-'        OR A 'REV'                                   
         BNE   *+10                                                             
         MP    0(8,R4),=P'-1'                                                   
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET VALUES FROM COLUMN KEYS                              *         
*  R1=RSALLCOLS OR RSCOLS                                             *         
***********************************************************************         
         SPACE 1                                                                
COLV     LR    R0,RE                                                            
COLV1    CLI   0(R1),9             TYPE EXPRESSION                              
         BNE   COLV3                                                            
         MVC   CURRTYPE,1(R1)      TYPE                                         
         SR    R5,R5                                                            
         IC    R5,CURRTYPE         TYPE CODE                                    
         SLL   R5,3                X 8                                          
         A     R5,ATYPELST         + START OF TABLE                             
         ST    R5,ATYPL            SAVE ADDRESS OF CURRENT                      
         B     YESR                                                             
*                                                                               
COLV3    CLI   0(R1),10            USER HOOK                                    
         BNE   COLV5                                                            
         MVI   HOOKTYPE,1          TYPE 1 IS COLUMN HOOK                        
         MVC   HOOKNUM,1(R1)       PASS THE HOOK NUMBER                         
         LA    RF,ORIGKEYS                                                      
         ST    RF,HOOKACOL                                                      
         LR    R5,R1                                                            
         OI    HOOKSTAT,HOOKSPRC   SET HOOK IN PROCESS                          
         GOTO1 AHOOKCDE,DMCB,(RA)                                               
         NI    HOOKSTAT,ALL-HOOKSPRC                                            
         LR    R1,R5                                                            
         CLI   0(R1),10            DID USER MODIFY KEY                          
         BE    YESR                NO, OK TO EXIT                               
         CLI   0(R1),0             NO VALUE                                     
         BE    YESR                                                             
         B     COLV1               THEN GO BACK TO RE-INTERPRET                 
*                                                                               
COLV5    CLI   0(R1),14            PERIOD                                       
         BH    COLV7                                                            
         MVC   CURRPER,0(R1)                                                    
         B     YESR                                                             
*                                                                               
COLV7    MVC   CONCODE,1(R1)       SAVE CONDITION CODE                          
         CLI   0(R1),17            IF CODES                                     
         BNE   COLV9                                                            
         BAS   RE,CONEX                                                         
         BE    YESR                                                             
         B     NOR                                                              
*                                                                               
COLV9    CLI   0(R1),18            NOT CODES                                    
         BNE   COLV11                                                           
         BAS   RE,CONEX                                                         
         BE    NOR                                                              
         B     YESR                                                             
*                                                                               
COLV11   CLI   0(R1),19           REVERSE                                       
         BNE   COLV13                                                           
         MVC   REVACC,CURRACC     USE UNSUBSTITUTED ACCOUNT CODES               
         MVC   CURRACC,ACTACC     TO SEE IF REVERSE IS ON                       
         BAS   RE,CONEX            REV CODES                                    
         BNE   *+8                                                              
         MVI   REVSIGN,C'-'        REVERSE SIGN IF SATISFIED                    
         MVC   CURRACC,REVACC                                                   
         B     YESR                                                             
*                                                                               
COLV13   CLI   0(R1),21            21 - 29 ARE CANNED FORMULAS                  
         BL    COLV21                                                           
         CLI   0(R1),29                                                         
         BH    COLV21                                                           
         LA    RF,FORMULA          FIRST                                        
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RF,FORMULA2         OR SECOND                                    
         MVC   0(L'FORMULA,RF),0(R1)  SAVE FORMULA                              
         B     YESR                                                             
*                                                                               
COLV21   B     YESR                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ACCUMULATE PERIOD DATA INTO DUB                          *         
*  R3=LINE NUMBER                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
PCOL     STM   RE,R6,SVRE                                                       
         CH    R3,=H'99'           99 MEANS COMPLEX EQUATION                    
         BER   RE                                                               
         CH    R3,=H'98'           98 MEANS COMPLEX DIVIDE BY 100               
         BNE   *+12                                                             
         ZAP   WORK(16),=P'100'    A FIXED VALUE FOR MWR                        
         BR    RE                                                               
         LTR   R3,R3               ZERO CODE MEANS                              
         BNZ   *+12                                                             
         ZAP   DUB,=P'1000000'     A FIXED VALUE FOR PERCENTS                   
         BR    RE                                                               
*                                                                               
         ZAP   DUB,=P'0'                                                        
         LR    R4,R3               SAVE ACCUM NUMBER IN R4                      
         SR    RF,RF                                                            
         IC    RF,CURRCOL                                                       
         LA    RF,RSCOPTN(RF)      RF TO COLUMN OPTION                          
         MH    R3,WLINE                                                         
         A     R3,AACCUM                                                        
         LA    R6,NEXTPER          FUTURE                                       
         CLI   CURRPER,11                                                       
         BE    PCOL3                                                            
         LA    R6,NEXTYTD                                                       
         CLI   CURRPER,12                                                       
         BE    PCOL3                                                            
         LA    R6,NEXTROLL                                                      
         CLI   CURRPER,13                                                       
         BE    PCOL3                                                            
         LA    R6,NEXTFISC                                                      
         CLI   CURRPER,14                                                       
         BE    PCOL3                                                            
         SR    R6,R6                                                            
         IC    R6,CURRPER          FIND START/END MONTH PAIR                    
         BCTR  R6,0                1=PERIOD, 2=YTD, 3=ROLL, 4=FISCAL            
*                                  5=-P    , 6=-Y , 7=-R ,  5=-F                
         MH    R6,=H'40'                                                        
         LA    R6,PERIOD(R6)       POSITION INTO DATE CHUNK                     
         CLI   CURRPER,7           -R OR -MON                                   
         BNE   PCOL3                                                            
         CLI   CURRPER+1,12        JUST WANT -MON CASE                          
         BNE   PCOL3                                                            
         SR    R5,R5                                                            
         IC    R5,CMONLYR          CURRENT MONTH LAST YEAR                      
         B     PCOL4                                                            
*                                                                               
PCOL3    SR    R5,R5                                                            
         IC    R5,CURRPER+1        THEN PICK UP SPECIFIC DATE                   
*                                                                               
PCOL4    SLL   R5,1                                                             
         AR    R6,R5               R6=A(START MONTH NUMBER)                     
         SR    R5,R5                                                            
         ICM   R5,1,0(R6)          START MONTH NUMBER                           
         BZ    PCOLX                                                            
         BCTR  R5,0                                                             
         LR    R1,R5                                                            
         MH    R1,=H'6'            MONTH NO.(-1) * LENGTH OF ENTRY              
         LA    R1,MONLIST(R1)      R1 = TO MONTH ENTRY                          
         SLL   R5,3                MONTH NO.(-1) * 8                            
         AR    R3,R5               R3=A(START ACCUM)                            
         SR    R5,R5                                                            
         IC    R5,1(R6)            R5=END MONTH NUMBER                          
         SR    RE,RE                                                            
         IC    RE,0(R6)            R0=START MONTH NUMBER                        
         SR    R5,RE                                                            
         LA    R5,1(R5)            R5=NUMBER OF MONTHS                          
*                                                                               
PCOL5    TM    0(RF),RSCSTOP       IS STOP OPTION ON?                           
         BNO   PCOL7                                                            
         CLC   0(4,R1),QEND        STOP AT END DATE                             
         BH    PCOL13                                                           
*                                                                               
PCOL7    CLI   ADJHOURS,YES        ADJUST STAND HOURS OPT ON (DOREMUS)          
         BNE   PCOL9               NO- PUT IT TO DUB                            
*&&US                                                                           
         CLC   ALPHAID,=C'HK'      IS IT HILL & KNOWLTON?                       
         BE    PCOL9               IGNORE HIRE AND TERM DATE INFO               
*&&                                                                             
         CH    R4,=H'37'           IS THIS STANDARD HOURS ACCUM LINE            
         BNE   PCOL9               NO-CONTINUE                                  
         CLC   0(4,R1),CURRHIRE    CHECK YYMM AGAINST HIRE DATE                 
         BNH   PCOL11              IF EQUAL OR LOWER WE DONT WANT IT            
         CLC   0(4,R1),CURRTERM    CHECK YYMM AGAINST TERM DATE                 
         BNL   PCOL13              IF EQUAL OR HIGHER WE DONT WANT IT           
*                                                                               
PCOL9    AP    DUB,0(8,R3)                                                      
PCOL11   LA    R3,8(R3)                                                         
         LA    R1,6(R1)                                                         
         BCT   R5,PCOL5                                                         
*                                                                               
PCOL13   CLI   CURRSIGN,C'+'                                                    
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         CLI   REVSIGN,C'-'                                                     
         BNE   PCOLX                                                            
         MP    DUB,=P'-1'                                                       
*                                                                               
PCOLX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY OPERATORS TO VALUE IN DUB                                     *         
***********************************************************************         
         SPACE 1                                                                
OPR      LR    R0,RE                                                            
         LA    R1,OPTAB                                                         
OPR03    CLC   BYTE,0(R1)          MATCH OPERATOR                               
         BE    OPR04                                                            
         LA    R1,L'OPTAB(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   OPR03                                                            
         DC    H'0'                INVALID OPERATOR                             
*                                                                               
OPR04    SR    RF,RF                                                            
         ICM   RF,3,1(R1)          ADDRESS OF OPERATION ROUTINE                 
         AR    RF,RB                                                            
         BASR  RE,RF               PERFORM OPERATION                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
OPADD    AP    WORK(16),DUB             ADD                                     
         BR    RE                                                               
*                                                                               
OPADX    AP    WORK+16(16),WORK+8(8)    ADD RESULTS INTO INTERMED ACCUM         
         ZAP   WORK(16),=P'0'           CLEAR FOR NEXT PART OF EQUATION         
         BR    RE                                                               
*                                                                               
OPSUB    SP    WORK(16),DUB             SUBTRACT                                
         BR    RE                                                               
*                                                                               
OPSUX    SP    WORK+16(16),WORK+8(8)    SUB RESULTS FROM INTERMED ACCUM         
         ZAP   WORK(16),=P'0'           CLEAR FOR NEXT PART OF EQUATION         
         BR    RE                                                               
*                                                                               
OPMLT    MP    WORK(16),DUB             MULTIPLY                                
         BR    RE                                                               
*                                                                               
OPMLX    MP    WORK+16(16),WORK+8(8)    SUB RESULTS FROM INTERMED ACCUM         
         ZAP   WORK(16),=P'0'           CLEAR FOR NEXT PART OF EQUATION         
         BR    RE                                                               
*                                                                               
OPDIV    CP    DUB,=P'0'                DONT DIVIDE BY ZERO                     
         BNZ   *+12                                                             
         ZAP   WORK(16),=P'0'                                                   
         BR    RE                                                               
         MP    WORK(16),=P'2'           DIVIDE (ROUNDED)                        
         DP    WORK(16),DUB                                                     
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         CP    WORK+8(8),=P'0'                                                  
         BL    *+10                                                             
         AP    WORK+8(8),=P'1'                                                  
         DP    WORK(16),=PL8'2'                                                 
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         BR    RE                                                               
*                                                                               
OPDIX    CP    WORK(16),=P'0'           DONT DIVIDE BY ZERO                     
         BZR   RE                                                               
         MP    WORK+16(16),=P'2'        DOUBLE DIVIDEND(RESULT 1ST EQU)         
         DP    WORK+16(16),WORK+8(8)    DIVIDE BY (RESULT OF 2ND)               
         MVC   WORK+24(8),WORK+16       MOVE QUOTIENT OVER REMAINDER            
         XC    WORK+16(8),WORK+16       CLEAR 1ST 8 BYTES                       
         CP    WORK+24(8),=P'0'         IS IT NEGATIVE ?                        
         BL    *+10                                                             
         AP    WORK+24(8),=P'1'         NO  - ADD 1 TO IT                       
         DP    WORK+16(16),=PL8'2'      DIVIDE IT BACK DOWN BY 2                
         MVC   WORK+24(8),WORK+16       MOVE QUOTIENT OVER REMAINDER            
         XC    WORK+16(8),WORK+16                                               
         ZAP   WORK(16),=P'0'           CLEAR FOR NEXT PART OF EQUATION         
         BR    RE                                                               
*                                                                               
OPTAB    DS    0CL3                OPERATION ROUTINE                            
         DC    C'+',AL2(OPADD-ACM2)                                             
         DC    C'A',AL2(OPADX-ACM2)                                             
         DC    C'-',AL2(OPSUB-ACM2)                                             
         DC    C'S',AL2(OPSUX-ACM2)                                             
         DC    C'X',AL2(OPMLT-ACM2)                                             
         DC    C'M',AL2(OPMLX-ACM2)                                             
         DC    C'/',AL2(OPDIV-ACM2)                                             
         DC    C'D',AL2(OPDIX-ACM2)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SPECIAL ADJUSTMENTS TO EXTRACTED VALUES                             *         
***********************************************************************         
         SPACE 1                                                                
CADJ     LR    R0,RE                                                            
         CLI   CURRTYPE,KW007      PERCENTS                                     
         BNE   CADJ3                                                            
         CLI   FORMBASE,C'C'       WITH CALENDAR BASE                           
         BNE   CADJ3                                                            
         CP    0(8,R4),=P'1000000'   SHOULD NOT EXCEED 100PCT                   
         BL    CADJ3                                                            
         ZAP   0(8,R4),=P'1000000'                                              
*                                                                               
CADJ3    CLI   CURRTYPE,KWGRS      GROSS PCT                                    
         BNE   CADJ7                                                            
         CLC   CURRCON+2(2),=C'NP' DOESNT APPLY FOR PERSONAL TIME               
         BE    CADJ5                                                            
         CLC   CURRCON+2(2),=C'NL' OR LEAVE OF ABSENCE                          
         BNE   CADJ7                                                            
*                                                                               
CADJ5    ZAP   0(8,R4),=P'0'                                                    
*                                                                               
CADJ7    CLI   CURRTYPE,KWNET      NET  PCT                                     
         BNE   CADJ9                                                            
         CLC   CURRCON+2(2),=C'NL' DOESNT APPLY FOR LEAVE                       
         BNE   CADJ9                                                            
         ZAP   0(8,R4),=P'0'                                                    
*                                                                               
CADJ9    OC    FORMULA,FORMULA     IS THERE A SUPERCAL ON COL?                  
         BZ    CADJ11              NO                                           
         MVC   CONCODE,FORMULA+1   YES SO GET CONCODE NUMBER                    
         BAS   RE,CONEX            CHECK CONCODES FOR CURRENT ACCT.             
         BNE   CADJ11              NO SO GET OUT                                
         BAS   RE,FORMX            YES SO PERFORM FORMULA                       
*                                                                               
CADJ11   OC    FORMULA2,FORMULA2   IS THERE ANOTHER SUPERCAL?                   
         BZ    CADJ13              NO SO OUT WE GO                              
         MVC   FORMULA,FORMULA2                                                 
         MVC   CONCODE,FORMULA+1   GET CON CODE FOR 2ND FORMULA                 
         BAS   RE,CONEX            CHECK CONCODES FOR CURRENT ACCT.             
         BNE   CADJ13              NO SO GET OUT                                
         BAS   RE,FORMX            YES SO PERFORM FORMULA                       
*                                                                               
CADJ13   MVC   WORK(3),=3X'F0'     IS THERE A CONVERSION, THERE                 
         MVZ   WORK(3),QBILGRUP    SOMETIMES IS FOR CURRENCY YOU KNOW           
         CLC   WORK(3),=3X'F0'     AND IT SITS IN QBILGRUP                      
         BNE   CADJX               NO THERE ISN'T                               
         CLI   CURRTYPE,KWHRS      HOURS AND PCTS ARE NO DIFFERENT              
         BL    CADJ15              IN OTHER COUNTRIES YOU KNOW, SO              
         CLI   CURRTYPE,KWPCT      DON'T CONVERT THESE.                         
         BNH   CADJX                                                            
*                                                                               
CADJ15   PACK  WORK(3),QBILGRUP(3)                                              
         ZAP   WORK+3(10),0(8,R4)                                               
         MP    WORK+3(10),WORK(3)                                               
         AP    WORK+3(10),=P'5'                                                 
         MVO   WORK+3(10),WORK+3(9)                                             
         AP    WORK+3(10),=P'5'                                                 
         MVO   WORK+3(10),WORK+3(9)                                             
         ZAP   0(8,R4),WORK+3(10)                                               
*                                                                               
CADJX    LR    RE,R0                                                            
         BR    RE                                                               
         TITLE 'BASE - BUILD THE SORT RECORD'                                   
***********************************************************************         
* BUILD SORT RECORD - KEY                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
BSR      NTR1  ,                                                                
         XC    SORTLEVS,SORTLEVS                                                
         MVC   SORTREP,REPCODE                                                  
         MVC   SORTCOPY,COPYNO                                                  
         MVI   SORTTYPE,0                                                       
         LA    R1,RSROWS                                                        
         LA    R3,SORTLEVS                                                      
         L     R0,RSNROWS                                                       
         LA    R4,1                R4=LEVEL NUMBER                              
         LA    R5,SORTNAMS                                                      
*                                                                               
BSR03    BAS   RE,GTFRM            GET "FROM" ROW                               
         CLI   FROMNME,C' '        DID WE ALSO GET NAME                         
         BH    *+8                                                              
         BAS   RE,GTFRN            GET "FROM" NAME                              
         MVC   0(1,R3),REPCODE     DEFAULT IS TO USE REPORT                     
         MVC   1(1,R3),COPYNO                             AND COPY NO.          
         MVC   2(14,R3),FROM       ALONG WITH EXTRACT KEY                       
         ST    R0,SVR0                                                          
         CH    R4,RSLPROW          FOR ROW 1 OR LPOOLROW ONLY                   
         BNE   BSR05               SEE IF THERE'S A CPOOL OVERRIDE              
         MVI   BYTE,CPOOLLS        SET TO GET LEFT SIDE                         
         BAS   RE,GETLPL           GET LITERAL                                  
         BNE   BSR05               NONE FOUND                                   
         MVC   FROMNME,SPACES                                                   
         MVC   FROMNME(L'CPOOLM),WORK    SAVE IN FROM NAME                      
*                                                                               
BSR05    L     R0,SVR0                                                          
         MVC   0(36,R5),FROMNME    AND EXTRACT NAME                             
         CLC   FROM,SPACES         IF FROM IS BLANK                             
         BNE   BSR07                                                            
         CLI   0(R1),51            ONLY FOR 'SOFT' CONTRAS                      
         BL    BSR07                                                            
         CLI   0(R1),54            51-54=C1-C4                                  
         BH    BSR07                                                            
         SH    R3,=H'14'                                                        
         MVC   16(14,R3),0(R3)     PICK UP VALUE FROM PREVIOUS ROW              
         MVC   0(14,R3),SPACES     AND CLERR PREVIOUS                           
         SH    R5,=H'36'           SAME FOR NAME                                
         MVC   36(36,R5),0(R5)                                                  
         MVC   0(36,R5),SPACES                                                  
         LA    R5,36(R5)                                                        
         LA    R3,14(R3)                                                        
*                                                                               
BSR07    CLI   RSRECAP,0           BUT IF RECAP IS SPECIFIED                    
         BE    BSR09                                                            
         STC   R4,DUB                                                           
         CLC   DUB(1),RSRECAP+1    AND LEVEL HAS NOT BEEN ATTAINED              
         BH    BSR09                                                            
         MVC   0(1,R3),RSRECAP     PUT ORIGINAL REPORT NUMBER IN KEY            
         SR    RE,RE                                                            
         IC    RE,RSRECAP          IF THERE ARE MORE COPIES OF THE              
         BCTR  RE,0                                                             
         A     RE,AREPCOPY         RECAP THAN ORIGINAL REPORT                   
         CLC   0(1,RE),COPYNO      PUT SUBSEQUENT COPIES IN WITH                
         BNL   BSR09               THE LAST COPY OF ORIGINAL                    
         MVC   1(1,R3),0(RE)                                                    
*                                                                               
BSR09    LA    R1,3(R1)                                                         
         LA    R3,16(R3)                                                        
         LA    R4,1(R4)                                                         
         LA    R5,36(R5)                                                        
         BCT   R0,BSR03                                                         
*                                                                               
         CLI   RSHKPUT,31                                                       
         BNE   BSR11               NO HOOK                                      
         MVC   HOOKTYPE,RSHKPUT    TYPE 31 IS PUT HOOK                          
         LA    R4,SORTREP                                                       
         ST    R4,HOOKAREC         A(SORT RECORD)                               
         MVC   HOOKNUM,RSHKPUT+2   PASS THE HOOK NUMBER                         
         OI    HOOKSTAT,HOOKSPRC   SET HOOK IN PROCESS                          
         GOTO1 AHOOKCDE,DMCB,(RA)                                               
         BZ    BSR11               PUT RECORD TO SORT                           
         NI    HOOKSTAT,ALL-HOOKSPRC                                            
         B     XIT                                                              
*                                                                               
BSR11    NI    HOOKSTAT,ALL-HOOKSPRC                                            
         GOTO1 APUT                                                             
         SPACE 2                                                                
***********************************************************************         
* CONTROL OUTPUT OF EXTRA RECORDS CAUSED                              *         
* BY PRESENCE OF VERTICAL PERCENTAGES                                 *         
***********************************************************************         
         SPACE 1                                                                
         OC    RSVERT,RSVERT                                                    
         BZ    XIT                                                              
         LA    R3,SORTLEVS+144                                                  
         LA    R4,RSVERT+8                                                      
         LA    R5,SORTNAMS                                                      
         AH    R5,=H'324'                                                       
         LA    R0,9                                                             
*                                                                               
BSR15    XC    0(16,R3),0(R3)                                                   
         XC    0(36,R5),0(R5)                                                   
         STC   R0,SORTTYPE                                                      
         CLI   0(R4),0                                                          
         BE    BSR17                                                            
         GOTO1 APUT                                                             
*                                                                               
BSR17    SH    R3,=H'16'                                                        
         SH    R5,=H'36'                                                        
         BCTR  R4,0                                                             
         BCT   R0,BSR15                                                         
         B     XIT                                                              
         DROP  R2                                                               
         TITLE 'BASE - EXTRACT DATA FROM ''FROM'' EXPRESSION'                   
***********************************************************************         
* EXTRACT DATA FROM 'FROM' EXPRESSION                                 *         
***********************************************************************         
         SPACE 1                                                                
GTFRM    NTR1  ,                                                                
         LR    R2,R1                                                            
         CLI   2(R2),0             IF LENGTH OF CODE IS ZERO                    
         BNE   GFRM03                                                           
         CLI   0(R2),FRT2          (ONLY FOR T2 OF COURSE)                      
         BNE   GFRM03                                                           
         MVC   2(1,R2),T2LEN       THEN USE SAVED T2 LENGTH                     
*                                                                               
GFRM03   MVC   FROM,SPACES         R2=A(FROM,DISP,LEN)                          
         MVC   FROMACC,SPACES                                                   
         MVC   FROMNME,SPACES                                                   
*                                                                               
GFRM05   SR    R1,R1                                                            
         IC    R1,0(R2)            FROM CODE                                    
         BCTR  R1,0                LESS ONE                                     
         SLL   R1,2                X 4                                          
         L     RF,AFROMLST                                                      
         LA    R1,0(R1,RF)         A(ROUNTINE OR B/DISP)                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                 UNDEFINED                                    
         DC    H'0'                                                             
         CLI   0(R1),0             A(SPECIAL ROUTINE)                           
         BE    GFRM19                                                           
         MVC   GFRM07+2(2),0(R1)   BASE/DISP FOR R3                             
         MVC   GFRM09+2(2),2(R1)   SAME FOR R4                                  
GFRM07   LA    R3,0                                                             
GFRM09   LA    R4,0                                                             
*                                                                               
GFRM11   CLI   0(R2),FRC1          C1, C2, C3 OR C4                             
         BL    GFRM12                                                           
         CLI   0(R2),FRC4                                                       
         BH    GFRM12                                                           
         LR    R2,R3                                                            
         SH    R2,=H'6'            R2=TYPE/DISP/LEN                             
         OC    0(3,R2),0(R2)       CONTRA DOESN'T MATCH SPEC                    
         BZ    XIT                                                              
*                                                                               
GFRM12   SR    R1,R1                                                            
         IC    R1,1(R2)            ADD IN DISPLACEMENT                          
         CLI   0(R2),FRT1          IF T1 CODE                                   
         BNE   GFRM13                                                           
         SR    R0,R0                                                            
         IC    R0,T1DISP           ADD DISPLACEMENT FROM A86 PROFILE            
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
*                                                                               
GFRM13   AR    R3,R1                                                            
         ICM   R1,1,2(R2)          PICK UP LENGTH                               
         BNZ   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         BCTR  R1,0                                                             
         CH    R1,=Y(L'FROM-1)                                                  
         BNH   *+8                                                              
         LH    R1,=Y(L'FROM-1)                                                  
*MN      EX    R1,*+4                                                           
         MVC   FROM(0),0(R3)                                                    
         EX    R1,*-6                                                           
*MN                                                                             
         CLI   0(R2),FREP          IF EP WE WANT THIS AS THE NAME TOO           
         BNE   GFRM17                                                           
         MVC   FROMNME(L'FROM),FROM                                             
*                                                                               
GFRM17   LTR   R4,R4               DO WE NEED FULL ACCOUNT                      
         BZ    XIT                                                              
         SR    R3,R4               FIGURE OUT EXTRA LENGTH                      
         AR    R1,R3                                                            
         CH    R1,=Y(L'FROMACC-1)                                               
         BNH   *+8                                                              
         LH    R1,=Y(L'FROMACC-1)                                               
*MN      EX    R1,*+4                                                           
         MVC   FROMACC(0),0(R4)                                                 
         EX    R1,*-6                                                           
*MN                                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SPECIAL FROM ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
GFRM19   ICM   RF,15,0(R1)         ROUTINE NUMBER                               
         SLL   RF,2                                                             
         B     *(RF)                                                            
         DC    F'0'                OLD T2 CODE                                  
         B     GFRL                                                             
         EJECT                                                                  
***********************************************************************         
* GET FROM RL (ROW LIST)                                              *         
***********************************************************************         
         SPACE 1                                                                
GFRL     L     R3,ARLPOOL                                                       
         LH    R1,LISTPNT           ITEM IN LIST                                
         BCTR  R1,0                                                             
         MH    R1,=Y(RLTLN)         X WIDTH OF ITEM                             
         LA    R3,2(R1,R3)          R3=ITEM IN ROWLIST                          
         B     GFRM11                                                           
         EJECT                                                                  
***********************************************************************         
* LOOK UP NAME OF 'FROM'                                              *         
***********************************************************************         
         SPACE 1                                                                
GTFRN    NTR1  ,                                                                
         LR    R2,R1                                                            
         CLI   0(R2),FRJB          JOB                                          
         BE    GTFRN05                                                          
         CLI   0(R2),FRWC          WC                                           
         BE    GTFRN15                                                          
         CLI   0(R2),FRTK          TK                                           
         BE    GTFRN15                                                          
         CLI   0(R2),FRT1          T1                                           
         BE    GTFRN15                                                          
         CLI   0(R2),FRT2          T2                                           
         BE    GTFRN17                                                          
         CLI   0(R2),FRRL          RL                                           
         BE    GTFRN19                                                          
         CLI   0(R2),FRAL          AL                                           
         BE    GTFRN21                                                          
         CLI   0(R2),FRCL          CL                                           
         BE    GTFRN21                                                          
*                                                                               
         SR    RF,RF               INITIALIZE TO ZERO                           
         CLI   0(R2),FRF5          F5                                           
         BE    GTFRN25                                                          
         CLI   0(R2),FRF4          F4                                           
         BE    GTFRN26                                                          
         CLI   0(R2),FRF3          F3                                           
         BE    GTFRN27                                                          
         CLI   0(R2),FRF2          F2                                           
         BE    GTFRN28                                                          
         CLI   0(R2),FRF1          F1                                           
         BE    GTFRN29                                                          
         CLC   FROMACC,SPACES                                                   
         BE    XIT                                                              
         LA    R3,CURRNAME         DOES IT MATCH ACCOUNT                        
         CLC   FROMACC,CURRACC                                                  
         BE    GTFRN03                                                          
         LA    R3,CURRCNAM         OR CONTRA ACCOUNT                            
         CLC   FROMACC,CURRCON                                                  
         BNE   GTFRN07                                                          
*                                                                               
GTFRN03  CLC   0(36,R3),SPACES     CHECK THAT NAME IS SIGNIFICANT               
         BNE   GTFRN20                                                          
         B     GTFRN07                                                          
*                                                                               
GTFRN05  MVC   FROMACC,SPACES      SET JOB KEY                                  
         MVC   FROMACC(1),RCCOMPFL                                              
         MVC   FROMACC+1(2),PRODUL                                              
         MVC   FROMACC+3(12),CURRJOB                                            
*                                                                               
GTFRN07  L     R1,ANAMEOPT                                                      
         LA    R0,OPTMX            MAX IN TABLE                                 
*                                                                               
GTFRN09  CLI   0(R1),0             END OF TABLE                                 
         BE    GTFRN11                                                          
         LA    R3,14(R1)           R3 TO NAME                                   
         CLC   0(14,R1),FROMACC+1  IS CODE IN OPTIMIZATION POOL                 
         BE    GTFRN20                                                          
         LA    R1,OPTLN(R1)                                                     
         BCT   R0,GTFRN09                                                       
*                                                                               
GTFRN11  MVC   DKEY,SPACES                                                      
         MVC   DKEY(15),FROMACC                                                 
*&&UK                                                                           
         CLI   0(R2),FRCA          IF FROM IS CA                                
         BE    GTFRN12                                                          
         CLI   0(R2),FRC1          CONTRA LEVEL 1, 2, 3 OR 4                    
         BL    GTFRN13                                                          
         CLI   0(R2),FRC4                                                       
         BH    GTFRN13                                                          
*                                                                               
GTFRN12  CLC   DKEY+1(2),PROJUL    AND CUL IS 1J                                
         BNE   GTFRN13                                                          
         MVI   DKEY+1,C'S'         THEN UNIT IS REALLY S                        
*&&                                                                             
GTFRN13  GOTO1 ADMGR,HIGH          READ "FROM" ACCOUNT                          
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         BAS   RE,GETNAME          GET THE NAME                                 
         BAS   RE,PUTPOOL          ADD IT TO NAME POOL                          
         LA    R3,WORK                                                          
         B     GTFRN20                                                          
*                                                                               
GTFRN15  MVC   WORK,SPACES         WC, TK, T1                                   
         MVC   WORK(2),FROM                                                     
         LA    R3,WORK                                                          
         BAS   RE,LKWC             GET WC NAME FOR "FROM"                       
         BE    GTFRN20                                                          
         MVC   WORK(2),FROM+1                                                   
         BAS   RE,LKWC             OR FOR ("FROM" +1)                           
         BE    GTFRN20                                                          
         B     XIT                                                              
*                                                                               
GTFRN17  LA    R3,CURRCNAM         T2 USES CONTRA NAME                          
         B     GTFRN20                                                          
*                                                                               
GTFRN19  L     R3,ARLPOOL          RL=55                                        
         LH    R1,LISTPNT          (PICK UP N'TH ITEM IN LIST)                  
         BCTR  R1,0                                                             
         MH    R1,=Y(RLTLN)        X WIDTH OF ITEM                              
         LA    R3,14(R1,R3)                                                     
*                                                                               
GTFRN20  MVC   FROMNME,0(R3)                                                    
         B     XIT                                                              
*                                                                               
GTFRN21  L     R3,AMEDN            MEDIA NAME TABLE                             
         MVC   FROMNME(12),FROM    DEFAULT WILL BE FROM CODE                    
*                                                                               
GTFRN23  CLC   FROM(2),0(R3)       MATCH CODE                                   
         BNE   *+14                                                             
         MVC   FROMNME(15),2(R3)   RETURN MEDIA NAME                            
         B     XIT                                                              
         LA    R3,17(R3)                                                        
         CLI   0(R3),X'FF'         TEST EOT                                     
         BNE   GTFRN23                                                          
         B     XIT                                                              
*                                                                               
GTFRN25  AHI   RF,1                RF = 4 IF F5                                 
GTFRN26  AHI   RF,1                RF = 3 IF F4                                 
GTFRN27  AHI   RF,1                RF = 2 IF F3                                 
GTFRN28  AHI   RF,1                RF = 1 IF F2                                 
*                                                                               
GTFRN29  L     RE,FILTBLOC         RF = 0 IF F1                                 
         MHI   RF,60               POINT TO THE FILTER LEVEL WE WANT            
         AR    RE,RF               POINT TO FILTER DATA                         
PA01     OC    0(16,RE),0(RE)      ANY NAME?                                    
         BZ    XIT                                                              
         MVC   FROMNME(30),16(RE)                                               
         B     XIT                                                              
         TITLE 'PUT NAME TO NAME POOL'                                          
***********************************************************************         
* ADD NAME TO THE NAME POOL                                           *         
***********************************************************************         
         SPACE 1                                                                
PUTPOOL  NTR1  ,                                                                
         L     R2,ANAMEOPT         RECORD ACCOUNT HIT IN POOL                   
         LA    R0,OPTMX            MAX IN TABLE                                 
*                                                                               
PP3      CLI   0(R2),0             IS THERE A SPACE AVAILABLE                   
         BE    PP5                                                              
         LA    R2,OPTLN(R2)        LOOK TO NEXT SLOT                            
         BCT   R0,PP3              IF TABLE IS FULL                             
         L     R2,ANAMEOPT         CLEAR THE TABLE                              
         LH    R3,=Y(OPTLN*OPTMX)  LENGTH TABLE                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR TABLE                                  
         L     R2,ANAMEOPT         SET TO FIRST ENTRY                           
*                                                                               
PP5      MVC   0(14,R2),FROMACC+1  SAVE A/C CODE                                
         MVC   14(36,R2),WORK           AND NAME                                
         B     XIT                                                              
         TITLE 'BASE - EXECUTE CONDITIONALS - CONTROL IF/OR/AND'                
***********************************************************************         
* EXECUTE CONDITIONALS - CONTROL IF/OR/AND                            *         
***********************************************************************         
         SPACE 1                                                                
CONEX    NTR1  ,                                                                
         TM    GEXSW,GEXT2         DOES T2 LENGTH NEED TO BE FIXED              
         BNO   CONEX9                                                           
         L     R3,ACONEXE          START OF EXECUTABLE CODE                     
         LA    R3,8(R3)            + EXIT ROUTINES                              
         SR    R1,R1                                                            
CONEX1   AR    R3,R1                                                            
         CLI   0(R3),EOT           END OF CODE                                  
         BE    CONEX9                                                           
         LA    R1,2                                                             
         CLI   0(R3),X'07'         IS IT A NO-OP?                               
         BE    CONEX1                                                           
         LA    R1,4                                                             
         CLI   0(R3),X'47'         IS IT A BRANCH?                              
         BE    CONEX1                                                           
         LA    R1,6                                                             
         CLI   0(R3),X'D5'         IS IT A COMPARE                              
         BE    *+6                                                              
         DC    H'0'                UNKNOWN INSTRUCTION                          
         CLI   1(R3),X'FF'         WAS INPUT LENGTH 0 - T2(0)                   
         BNE   CONEX1                                                           
         CLC   2(2,R3),=S(CURRTE2)                                              
         BNE   CONEX1                                                           
         SR    RF,RF                                                            
         IC    RF,T2LEN            T2 LENGTH                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         STC   RF,1(R3)            SAVE IN GENERATED CODE                       
         B     CONEX1                                                           
*                                                                               
CONEX9   NI    GEXSW,ALL-GEXT2                                                  
         MVC   FROM,SPACES                                                      
         L     R1,ACONEXE      R1 = BASE REGISTER FOR CODE                      
         L     R5,ACONLTL      R5 = LITERAL POOL                                
         L     RF,ACONADS      LOOK IN TABLE FOR COND ADDR                      
         SR    R2,R2                                                            
         IC    R2,CONCODE                                                       
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         AR    R2,RF           R2 TO A(OF CODE FOR THIS CONDITION)              
         ICM   RF,15,0(R2)     RF = THE EXECUTABLE CODE                         
         SR    RE,RE                                                            
         IC    RE,1(RF)        LENGTH OF FROM                                   
         MVC   CONFRM+4(2),2(RF) BASE AND DISP OF FROM DATA                     
         EX    RE,CONFRM       MOVE "FROM" DATA TO FROM                         
         BASR  RE,RF           TEST AND SET CC                                  
         B     XIT                                                              
*                                                                               
*MN CONFRM   MVC   FROM(0),0                                                    
*MN THE INSTRUCTION BELOW IS BEING CONSTRUCTED ON THE FLY                       
*MN THE SECOND REFERENCE TO FROM GETS WRITTEN OVER BY PRIOR MVC                 
*MN MVC   CONFRM+4(2),2(RF)                                                     
CONFRM   MVC   FROM(0),FROM                                                     
         TITLE 'BASE - ROUTINE SETS UP ROWLIST POOL'                            
***********************************************************************         
* SET UP ROWLIST POOL                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETRL    NTR1                                                                   
         CLC   RLLEDG,SPACES                                                    
         BE    XIT                                                              
         L     R2,ARLPOOL                                                       
         LA    R0,RLTMX            MAX IN RLPOOL                                
         SR    R3,R3                                                            
         LA    R6,DKEY                                                          
         USING ACTRECD,R6                                                       
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),RCCOMPFL                                                 
         MVC   DKEY+1(2),RLLEDG                                                 
         MVI   DKEY+14,X'41'       SKIP PAST LEDGER                             
         GOTO1 ADMGR,HIGH                                                       
         B     SRL4                                                             
*                                                                               
SRL2     GOTO1 ADMGR,RSEQ                                                       
*                                                                               
SRL4     GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         CLC   1(2,R6),RLLEDG                                                   
         BE    SRL6                                                             
         L     R2,ARLPOOL                                                       
         STH   R3,LISTNUM                                                       
         B     XIT                                                              
*                                                                               
SRL6     BAS   RE,GETNAME                                                       
         MVC   0(14,R2),1(R6)      SAVE ACCOUNT NUMBER                          
         MVC   14(36,R2),WORK           AND NAME                                
         AH    R2,=Y(RLTLN)                                                     
         LA    R3,1(R3)                                                         
         BCT   R0,SRL2                                                          
         DC    H'0'                TABLE IS FULL                                
         DROP  R6                                                               
         TITLE 'BASE - SET CONVERSION RATE'                                     
***********************************************************************         
* SET CONVERSION RATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
SCNVR    CLC   RSRIGHT,SPACES      IF NO REPRIGHT, NO SHOW.                     
         BER   RE                                                               
         LA    R1,RSRIGHT+19       STICK ON CONVERSION DATA                     
         CLI   0(R1),C' '                                                       
         BNER  RE                  NO ROOM                                      
         MVC   WORK(3),=3X'F0'                                                  
         MVZ   WORK(3),QBILGRUP                                                 
         CLC   WORK(3),=3X'F0'                                                  
         BNER  RE                  FACTOR NOT VALID NUMERIC                     
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    *-6                                                              
         PACK  WORK(3),QBILGRUP(3)                                              
         EDIT  (P3,WORK),(4,1(R1)),2,COMMAS=NO,ZERO=NOBLANK                     
         BR    RE                                                               
         DROP  R2                                                               
         TITLE 'BASE - CHANGE REPRIGHT NAME WITH CPOOL ENTRIES'                 
***********************************************************************         
* CHANGE REPRIGHT NAME WITH CPOOL ENTRIES                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
GETLPL   LR    R0,RE                                                            
         MVC   WORK,SPACES                                                      
         LA    RF,LPLRS            SET TO TEST RIGHT SIDE                       
         CLI   BYTE,CPOOLRS                                                     
         BE    GETLPL3                                                          
         LA    RF,LPLLS            SET FOR LEFT SIDE                            
         CLI   BYTE,CPOOLLS                                                     
         BE    GETLPL3                                                          
         LA    RF,LPLFL            MUST BE FOOTLINE                             
*                                                                               
GETLPL3  EX    RF,*+8                                                           
         B     *+8                                                              
         TM    LPLSW,0             ANY OF THAT TYPE IN POOL                     
         BNO   NOR                                                              
         L     R6,ACPOOL                                                        
         USING CPOOLD,R6                                                        
*                                                                               
GETLPL5  CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    NOR                                                              
         CLI   CPOOLR,99           RPT 99 IS 'ALL' REPORTS                      
         BE    *+14                                                             
         CLC   CPOOLR,REPCODE      MATCH REPORT NUMBER                          
         BNE   GETLPL9                                                          
         CLC   CPOOLSD,BYTE        IS IT THE TYPE REQUESTED                     
         BNE   GETLPL9                                                          
         MVC   CONCODE,CPOOLC      SET CONDITION CODE                           
         BAS   RE,CONEX            DOES IT MATCH CONDITION                      
         BNE   GETLPL9                                                          
         CLI   CPOOLSD,CPOOLFL     IS IT A FOOTLINE                             
         BE    GETLPL7                                                          
         MVC   WORK(L'CPOOLM),CPOOLM  SET THE LITERAL                           
         B     YESR                                                             
*                                                                               
GETLPL7  CLI   RSFOOT1,0           IS THERE A FIRST FOOT                        
         BNE   *+14                YES, THIS IS SECOND                          
         MVC   RSFOOT1,CPOOLM      SAVE FIRST                                   
         B     GETLPL9             GET NEXT                                     
         MVC   RSFOOT2,CPOOLM      SAVE SECOND                                  
         B     YESR                AND LEAVE                                    
*                                                                               
GETLPL9  LA    R6,CPOOLNQ(R6)                                                   
         B     GETLPL5                                                          
         DROP  R2,R6                                                            
         TITLE 'BASE - SET MONTH NUMBER/ADD TO ACCUMULATOR LINE'                
***********************************************************************         
* GET RELATIVE MONTH NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
GRELMON  SR    R0,R0               GET RELATIVE NUMBER FOR THIS MONTH           
         SR    R1,R1                                                            
         IC    R0,0(R3)            PACKED YEAR                                  
         SRDL  R0,4                R0=DECADE, R1=YESR                           
         MH    R0,=H'10'           X'A0' BECOMES X'100'                         
         SRL   R1,28               YEAR TO LOW ORDER                            
         AR    R0,R1                                                            
         MH    R0,=H'12'                                                        
         IC    R1,1(R3)            PACKED MONTH                                 
         CLI   1(R3),X'09'         CONVERT MONTH TO BINARY                      
         BNH   *+8                                                              
         SH    R1,=H'6'                                                         
         AR    R0,R1                                                            
         ICM   R1,7,BASMNTH                                                     
         SR    R0,R1                                                            
         LR    R3,R0                                                            
         STC   R3,RELMON           RELATIVE MONTH NUMBER                        
         SLL   R3,3                X WIDTH OF ACCUMULATOR                       
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* POST AMOUNT TO ACCUMULATOR LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
PACC     CP    PAMT,=P'0'                                                       
         BER   RE                                                               
         ST    RE,FULL                                                          
         LR    RE,R4                                                            
         A     RE,ALNSTL           R4 STATUS BYTE                               
         OI    0(RE),LSACT         SET LINE ACTIVITY                            
         S     RE,ALNSTL                                                        
         MH    RE,WLINE            LINE NUMBER X WIDTH                          
         A     RE,AZRO             PLUS START OF ACCUMS                         
         AR    RE,R3               DISPLACEMENT TO COLUMN                       
         AP    0(8,RE),PAMT                                                     
         L     RE,FULL                                                          
         BR    RE                                                               
         TITLE 'BASE - CLEAR ACCUMULATOR LINES'                                 
***********************************************************************         
* CLEAR ACCUMULATOR LINES                                             *         
***********************************************************************         
         SPACE 1                                                                
CLR      STM   RE,R6,SVRE                                                       
         L     R2,ALNSTL           A(LINE STATUS LIST)                          
         LA    R2,1(R2)            START AT LINE 1                              
         LA    R0,LNMX             MAX LINES                                    
         SR    R6,R6                                                            
         IC    R6,CLRLV            STATUS BIT                                   
         LA    R1,1                LINE NUMBER                                  
*                                                                               
CLR1     CLI   CLRLV,LSREQ         CLEAR ALL FOR REQUEST                        
         BE    CLR5                                                             
         CLI   0(R2),LSYES         ALWAYS CLEAR                                 
         BE    CLR5                                                             
         TM    0(R2),LSACT         TEST LINE ACTIVITY                           
         BNO   CLR7                                                             
         TM    0(R2),LSNOT         LINE IS NOT TO BE CLEARED                    
         BO    CLR7                                                             
         EX    R6,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0             TEST THIS LINE STATUS                        
         BZ    CLR7                                                             
*                                                                               
CLR5     L     RE,AZRO             RE=A(ZERO'S)                                 
         LA    RF,WLN              RF=WIDTH OF A LINE                           
         LR    R5,RF               R5=WIDTH OF A LINE                           
         LR    R4,R1               LINE NUMBER                                  
         MH    R4,WLINE            X WIDTH                                      
         AR    R4,RE               PLUS START                                   
         MVCL  R4,RE               MOVE IN THE ZERO LINE                        
         NI    0(R2),ALL-LSACT     TURN OFF ACTIVITY SWITCH                     
*                                                                               
CLR7     LA    R2,1(R2)            NEXT LINE STATUS                             
         LA    R1,1(R1)            LINE NUMBER                                  
         BCT   R0,CLR1                                                          
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'BASE - LOOK UP WORKCODE NAMES'                                  
***********************************************************************         
* LOOK UP WORK CODE / TASK NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
LKWC     NTR1  ,                                                                
         L     R3,AWCTAB           A(WORKCODE TABLE)                            
         CLI   0(R3),0             FIRST TIME                                   
         BNE   LKWC12                                                           
         MVC   DKEY,SPACES         READ WORKCODE RECORDS                        
         LA    R6,DKEY                                                          
         USING WCORECD,R6                                                       
         MVI   WCOKTYP,WCOKTYPQ    TYPE                                         
         MVC   WCOKCPY,RCCOMPFL    COMPANY                                      
         MVC   WCOKUNT(2),QUNIT    ON THIS LEDGER                               
         GOTO1 ADMGR,HIGH                                                       
         LA    R0,WCTMX            WORKCODE TABLE MAX                           
*                                                                               
LKWC03   MVI   0(R3),X'FF'         MARKS END OF TABLE                           
         CLC   DIR(WCOKWRK-WCORECD),0(R6)                                       
         BNE   LKWC10                                                           
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         LA    R6,WCORFST                                                       
         SR    RE,RE                                                            
LKWC04   CLI   0(R6),0                                                          
         BE    LKWC05                                                           
         CLI   0(R6),X'12'                                                      
         BE    *+14                                                             
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     LKWC04                                                           
*                                                                               
         USING WCOELD,R6                                                        
         MVC   0(2,R3),WCOCODE     ADD CODE                                     
         MVC   2(15,R3),WCODESC    AND DESCRIPTION TO TABLE                     
         LA    R3,WCTLN(R3)                                                     
         BCT   R0,LKWC05                                                        
         DC    H'0'                TABLE IS FULL                                
*                                                                               
LKWC05   GOTO1 ADMGR,RSEQ                                                       
         LA    R6,DKEY                                                          
         B     LKWC03                                                           
*                                                                               
LKWC10   L     R3,AWCTAB                                                        
LKWC12   CLI   0(R3),X'FF'                                                      
         BE    NOX                 END OF TABLE                                 
         CLC   0(2,R3),WORK        MATCH WORKCODE                               
         BE    *+12                                                             
         LA    R3,WCTLN(R3)                                                     
         B     LKWC12                                                           
         MVC   WORK(15),2(R3)      REPLACE WITH NAME                            
         B     YESX                                                             
         DROP  R6                                                               
         TITLE 'BASE - GET NAMES'                                               
***********************************************************************         
* ROUTINE TO DIG OUT NAMES FROM RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         L     RF,AIO1                                                          
         LA    R2,ACCRFST-ACCRECD(R6)                                           
         CR    RF,R6                                                            
         BE    *+8                                                              
         LA    R2,ACCORFST(R6)                                                  
         SR    R0,R0                                                            
GETNME3  CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'20'                                                      
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETNME3                                                          
*                                                                               
         USING NAMELD,R2                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
*MN      EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         EX    R1,*-6                                                           
*MN                                                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET AGENCY / OFFICE ALLOCATION DATE                                           
***********************************************************************         
         SPACE 1                                                                
GADTE    LR    R0,RE                                                            
         XC    DKEY,DKEY                                                        
         LA    R6,DKEY                                                          
         USING CAHRECD,R6                                                       
         MVI   CAHKTYP,CAHKTYPQ    TYPE                                         
         MVI   CAHKSUB,CAHKSUBQ    SUB                                          
         MVC   CAHKCPY,RCCOMPFL    COMPANY                                      
         MVC   CAHKMTHD,METHOD     METHOD                                       
         L     R3,AAGYDTE                                                       
         L     R4,AOFFDTE                                                       
         MVC   0(NAC*2,R4),0(R3)                                                
         CLI   MODE,REQFRST        FIRST TIME GET COMPANY DATE                  
         BE    GADTE3                                                           
         L     RF,ADACC                                                         
         MVC   CAHKOFC,3(RF)       LOOK FOR OFFICE                              
*                                                                               
GADTE3   GOTO1 ADMGR,READ                                                       
         BNE   GADTEX                                                           
         L     R6,AIO1                                                          
         GOTO1 ADMGR,GETR                                                       
         SR    R1,R1                                                            
         LA    R6,CAHRFST                                                       
*                                                                               
         USING DOAELD,R6                                                        
GADTE5   CLI   0(R6),0                                                          
         BE    GADTEX                                                           
         CLI   DOAEL,DOAELQ        FIND 81 ELEMENT                              
         BE    GADTE9                                                           
GADTE7   IC    R1,DOALN                                                         
         AR    R6,R1                                                            
         B     GADTE5                                                           
*                                                                               
GADTE9   LA    RE,MONLIST+4        YEAR/MONTH(PWOS                              
         LA    RF,NAC              NUMBER OF COLUMNS                            
         L     R3,AAGYDTE                                                       
         L     R4,AOFFDTE                                                       
*                                                                               
GADTE11  CLC   DOAPER,0(RE)        MATCH PERIOD                                 
         BL    GADTE7              BEFORE START                                 
         BE    GADTE13                                                          
         LA    R3,2(R3)            NEXT AGENCY ENTRY                            
         LA    R4,2(R4)            OFFICE ENTRY                                 
         LA    RE,6(RE)            MONTH LIST                                   
         BCT   RF,GADTE11                                                       
         B     GADTE7              NEXT ELEMENT                                 
*                                                                               
GADTE13  CLI   MODE,REQFRST                                                     
         BNE   *+10                                                             
         MVC   0(2,R3),DOADTE      SAVE DATE OF ALLOCATION(AGENCY)              
         CLC   DOADTE,0(R4)        IS OFFICE LEVEL LATER                        
         BNH   GADTE7                                                           
         MVC   0(2,R4),DOADTE      SET OFFICE LEVEL                             
         B     GADTE7                                                           
*                                                                               
GADTEX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R6                                                               
         TITLE 'DATE CONVERSION'                                                
***********************************************************************         
* TO PROVIDE ALL DATES 3 WAYS                                         *         
*  PARM 1 BYTE 0   = INPUT DATE TYPE                                  *         
*              1-3 = A(INPUT DATE)                                    *         
*  PARM 2 BYTE 1-3 = A(OUTPUT FIELD)                                  *         
*                    +0  YYMMDD                                       *         
*                    +6  MMM/YY                                       *         
*                    +12 MMMDD/YY                                     *         
***********************************************************************         
         SPACE 1                                                                
DATCNV   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CLI   0(R1),2             COMPRESSED                                   
         BNE   DATCNV2                                                          
         GOTO1 DATCON,DMCB,(2,(R2)),(0,0(R3)) YYMMDD                            
         B     DATCNV5                                                          
*                                                                               
DATCNV2  CLI   0(R1),1             PACKED                                       
         BE    *+6                                                              
         DC    H'0'                ADD CODE FOR NEW INPUT TYPE                  
         GOTO1 DATCON,DMCB,(1,(R2)),(0,0(R3)) YYMMDD                            
*                                                                               
DATCNV5  GOTO1 DATCON,DMCB,(0,0(R3)),(9,6(R3))     MMM/YY                       
         GOTO1 DATCON,DMCB,(0,0(R3)),(11,12(R3))   MMMDD/YY                     
         B     XIT                                                              
         TITLE 'CANNED FORMULA PROCESSING'                                      
***********************************************************************         
* PROCESS FORMULAS                                                    *         
***********************************************************************         
         SPACE 1                                                                
FORMX    NTR1  ,                                                                
         L     R6,AFRMPOOL                                                      
         SR    R1,R1                                                            
         IC    R1,FORMULA            BRING DOWN BY 20                           
         SH    R1,=H'20'                                                        
         STC   R1,FORMULA                                                       
F01      CLC   0(1,R6),FORMULA                                                  
         BE    F02                                                              
F01A     LA    R6,1(R6)                                                         
         CLI   0(R6),C'#'                                                       
         BE    F01B                                                             
         CLI   0(R6),X'FF'                                                      
         BE    FXXIT                                                            
         B     F01A                                                             
F01B     LA    R6,1(R6)                                                         
         B     F01                                                              
*                                                                               
F02      CLI   1(R6),C' '          FORMULA IS FOR POS AND NEG NUMBERS           
         BE    F02A                GO FOR IT                                    
         CP    0(8,R4),=P'0'       OTHERWISE LOOK FOR MATCHING FORMULA          
         BL    FNEGATIV                                                         
*                                                                               
         CLI   1(R6),C'P'          THIS ONE POSITIVE                            
         BE    F02A                THEN GO FOR IT                               
         B     F01A                OR GO BACK AND LOOK FOR ONE                  
*                                                                               
FNEGATIV CLI   1(R6),NO            THIS ONE NEGATIVE                            
         BE    F02A                THEN GO FOR IT                               
         B     F01A                OR GO BACK AND LOOK FOR ONE                  
*                                                                               
F02A     CLI   2(R6),C' '          ADDL SELECTOR BLANK                          
         BE    F02B                YES, TAKE IT                                 
         CLC   FROM(1),2(R6)       ELSE ONLY IF EQUAL TO SAVED CHAR.            
         BNE   F01A                                                             
F02B     LA    R6,3(R6)                                                         
F03      LA    R5,OPTBL                                                         
*MN F03A     CLC   0(R5),X'FF'                                                  
F03A     CLI   0(R5),X'FF'                                                      
         BNE   F04                                                              
         ABEND 122                                                              
F04      CLC   0(1,R6),0(R5)                                                    
         BNE   F05                                                              
         MVC   FULL,=F'0'                                                       
         MVC   FULL+1(3),1(R5)                                                  
         L     RF,FULL                                                          
         BASR  RE,RF                                                            
         LA    R6,5(R6)                                                         
         CLI   0(R6),C'#'                                                       
         BE    FXIT                 END OF FORMULA, GET OUT                     
         CLI   0(R6),X'FF'                                                      
         BE    FXIT                 END OF FORMULA, GET OUT                     
         B     F03                                                              
F05      LA    R5,4(R5)                                                         
         B     F03A                                                             
*                                                                               
FADD     DS    0H                                                               
         AP    0(8,R4),1(4,R6)                                                  
         BR    RE                                                               
*                                                                               
FSUB     DS    0H                                                               
         SP    0(8,R4),1(4,R6)                                                  
         BR    RE                                                               
*                                                                               
FMULT    DS    0H                                                               
         ZAP   WORK(16),0(8,R4)                                                 
         MP    WORK(16),1(4,R6)                                                 
         ZAP   0(8,R4),WORK+8(8)                                                
         BR    RE                                                               
*                                                                               
FDIV     DS    0H                                                               
         ZAP   WORK(16),0(8,R4)                                                 
         ZAP   DUB,1(4,R6)                                                      
         MP    WORK(16),=P'10'                                                  
         DP    WORK(16),DUB                                                     
         MVN   FIVE,WORK+7                                                      
         AP    WORK(8),FIVE                                                     
         MVO   WORK+9(7),WORK(7)                                                
         CLI   BUCKD,YES           IF BUCKET$ GET RID OF CENTS NOW              
         BNE   FDIV2                                                            
         MVO   WORK+8(8),WORK+8(7)                                              
         AP    WORK+8(8),FIVE                                                   
         MVO   WORK+8(8),WORK+8(7)                                              
         MP    WORK+8(8),=P'100'                                                
FDIV2    ZAP   0(8,R4),WORK+8(8)                                                
         BR    RE                                                               
FIVE     DC    PL1'5'                                                           
*                                                                               
FXIT     SR    R1,R1                                                            
         IC    R1,FORMULA                                                       
         AH    R1,=H'20'           RESTORE TO ORIGINAL KEY VALUE                
         STC   R1,FORMULA                                                       
FXXIT    B     XIT                                                              
*                                                                               
OPTBL    DC    X'01',AL03(FADD)     CANNED CALC OPERATOR TABLE                  
         DC    X'02',AL03(FSUB)                                                 
         DC    X'03',AL03(FMULT)                                                
         DC    X'04',AL03(FDIV)                                                 
         DC    X'FF'                                                            
         TITLE 'GET PERSONAL HOURLY RATES'                                      
***********************************************************************         
* GET PERSONAL HOURLY RATES                                           *         
***********************************************************************         
         SPACE 1                                                                
GPHR     NTR1  ,                                                                
         MVC   DKEY,SPACES                                                      
         LA    R6,DKEY                                                          
         USING CPRRECD,R6                                                       
         MVI   CPRKTYP,CPRKTYPQ    TYPE                                         
         MVI   CPRKSUB,CPRKSUBQ    SUB                                          
         L     R1,ADACC                                                         
         MVC   CPRKCPY(15),0(R1)   C\U\L\ACCOUNT                                
         GOTO1 ADMGR,READ          READ RATE WITHOUT METHOD FIRST               
         BE    GPHR02              FOUND                                        
         MVC   CPRKMTHD,METHOD     TRY READING RATE WITH METHOD                 
         GOTO1 ADMGR,READ                                                       
         BNE   GPHRX               NOT FOUND, NO RATE                           
*                                                                               
GPHR02   GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         LA    R4,CPRRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING PHRELD,R4                                                        
GPHR03   CLI   0(R4),0             GET HOURLY RATE ELEMENTS                     
         BE    GPHRX                                                            
         CLI   0(R4),PHRELQ                                                     
         BE    GPHR07                                                           
GPHR05   IC    R0,PHRLN                                                         
         AR    R4,R0                                                            
         B     GPHR03                                                           
*                                                                               
GPHR07   CLC   PHRMTH,METHOD                                                    
         BNE   GPHR05                                                           
         LA    RE,MONLIST+4        YEAR/MONTH(PWOS)                             
         LA    RF,NAC              NUMBER OF COLUMNS                            
         SR    R3,R3                                                            
*                                                                               
GPHR09   CLC   PHRYR(2),0(RE)      MATCH PERIOD                                 
         BL    GPHR05              BEFORE START                                 
         BE    GPHR11                                                           
         LA    RE,6(RE)            NEXT MONLIST ENTRY                           
         LA    R3,PFL(R3)          DISPLACEMENT TO COLUMN                       
         BCT   RF,GPHR09           AFTER END                                    
         B     GPHR05                                                           
*                                                                               
GPHR11   IC    RF,PHRNUM           NUMBER OF MINI ELEMENTS                      
         LA    R2,PHRNTRY                                                       
*                                                                               
         USING PHRNTRY,R2                                                       
GPHR15   LA    RE,PHRTAB                                                        
GPHR17   CLC   PHRTYPE,0(RE)                                                    
         BE    GPHR19                                                           
         LA    RE,L'PHRTAB(RE)                                                  
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   GPHR17                                                           
         B     GPHR21              NEXT MINI ELEMENT                            
*                                                                               
GPHR19   SR    R6,R6                                                            
         ICM   R6,1,3(RE)                                                       
         A     R6,ALNSTL                                                        
         OI    0(R6),LSACT         SET LINE ACTIVE                              
         SR    R6,R6                                                            
         ICM   R6,3,1(RE)          LINE NUMBER                                  
         A     R6,AZRO             PLUS START                                   
         AR    R6,R3               AND DISPLACMENT TO COLUMN                    
         ZAP   0(PFL,R6),PHRRATE   RATE TO ACCUM LINE                           
         OI    ACSW,ACPH                                                        
*                                                                               
GPHR21   LA    R2,PHRLN2Q(R2)      NEXT MINI ELEMENT                            
         BCT   RF,GPHR15                                                        
         B     GPHR05              NEXT ELEMENT                                 
         DROP  R2,R4,R6                                                         
*                                                                               
GPHRX    B     XIT                                                              
*                                                                               
PHRTAB   DS    0CL4                                                             
         DC    AL1(PHRTSAL),AL2(LNPSR*WLN),AL1(LNPSR)   SALARY                  
         DC    AL1(PHRTBEN),AL2(LNPBR*WLN),AL1(LNPBR)   BENEFIT                 
         DC    AL1(PHRTPEN),AL2(LNPPR*WLN),AL1(LNPPR)   PENSION                 
         DC    AL1(PHRTTOT),AL2(LNPTR*WLN),AL1(LNPTR)   TOTAL                   
         DC    X'FF'                                                            
         TITLE 'BASE - HOOK ROUTINES'                                           
***********************************************************************         
* ROUTINE TO PASS MONACC MODES TO THE HOOK                            *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                                                             
GOHK     ST    RE,GOHK-4                                                        
         MVC   HOOKNUM,HOOKMODE    TELL HOOK WHICH ROUTINE                      
         OI    HOOKSTAT,HOOKSPRC   SET HOOK IN PROCESS                          
         GOTO1 AHOOKCDE,DMCB,(RA)                                               
         NI    HOOKSTAT,ALL-HOOKSPRC                                            
         L     RE,GOHK-4                                                        
         BR    RE                                                               
         TITLE 'BASE - LOAD SUPPORT PHASES'                                     
***********************************************************************         
* ROUTINE TO LOAD OTHER APG PHASES                                    *         
***********************************************************************         
         SPACE 1                                                                
LOAD     NTR1  ,                                                                
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         LA    R2,PHASE03                                                       
         SR    R0,R0                                                            
*                                                                               
LOAD3    MVC   DUB,0(R2)           LOAD IN PHASE                                
         MVC   DUB+6(1),MCTEST2    TEST PHASE                                   
         OI    DUB+6,X'40'                                                      
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4(4),DMCB+4    TEST LOAD WAS OK                             
         BNZ   LOAD4               YES                                          
         MVC   DUB,0(R2)           LOAD IN NON-TEST PHASE                       
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4(4),DMCB+4    TEST LOAD WAS OK                             
         BNZ   LOAD4               YES                                          
         DC    H'0'                CAN'T LOAD                                   
*                                                                               
LOAD4    ICM   R3,15,4(R1)         START OF PHASE                               
         OC    MCUSRDMP(4),MCUSRDMP                                             
         BZ    *+12                                                             
         CLM   R3,15,MCUSRDMP                                                   
         BH    *+8                                                              
         STCM  R3,15,MCUSRDMP      ADD PHASE TO DUMP LIST                       
         A     R0,0(R1)            ADD LENGTH                                   
         OC    8(4,R2),8(R2)       IS THIS APG CSECT                            
         BZ    *+12                                                             
         L     RA,4(R1)            RA TO MANC                                   
         ST    RA,SPACEND                                                       
         MVC   8(4,R2),4(R1)       SAVE A(PHASE)                                
         LA    R2,12(R2)           LOAD NEXT PHASE                              
         CLI   0(R2),EOT           TEST END OF TABLE                            
         BNE   LOAD3                                                            
         ICM   R3,15,MCUSRDMP                                                   
         AR    R3,R0                                                            
         STCM  R3,15,MCUSRDMP+4    END OF PHASE                                 
*                                                                               
         LA    R2,PHASE02          RESOLVE ADDRESSES                            
         LA    R4,ENTRYTAB                                                      
         LR    R3,RB                                                            
         B     LOAD7                                                            
*                                                                               
LOAD5    ICM   R3,15,8(R2)         R3 = LOAD ADDRESS                            
         ICM   R4,15,0(R3)         R4 = DISPLACEMENT TO ENTRY TABLE             
         BZ    LOAD9                                                            
         AR    R4,R3               R4 = ENTRY TABLE                             
*                                                                               
LOAD7    SR    R5,R5                                                            
         ICM   R5,3,0(R4)          R5 = DISP FROM START OF PHASE                
         AR    R5,R3               R5 = ABSOLUTE A(DATA/ROUTINE)                
         SR    R6,R6                                                            
         ICM   R6,3,2(R4)          R6 = DISP FROM  START OF MAND                
         AR    R6,RA               R6 = ADDRESS OF SAVE AREA                    
         ST    R5,0(R6)            SAVE ADDRESS IN MAIN CSECT                   
         LA    R4,4(R4)                                                         
         CLI   0(R4),EOT                                                        
         BNE   LOAD7                                                            
*                                                                               
LOAD9    LA    R2,12(R2)           LOAD NEXT PHASE                              
         CLI   0(R2),EOT           TEST END OF TABLE                            
         BNE   LOAD5                                                            
         L     R5,ADMASTC                                                       
         MVC   MCAPHAS3,PHASE03+8                                               
         MVC   MCAPHAS4,PHASE04+8                                               
         B     XIT                                                              
         DROP  R5                                                               
         TITLE 'BASE - EXIT ROUTINES'                                           
***********************************************************************         
* EXIT ROUTINES                                                       *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
YESX     CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NOX      LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
YESR     CR    RB,RB                                                            
         B     *+6                                                              
*                                                                               
NOR      LTR   RB,RB                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OPTION TO CAPTURE RUN TIMES                                         *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
XXIT     CLI   SE,0                TEST LOCAL DATAMGR SET                       
         BE    *+22                                                             
         XC    LDMGR,DATAMGR       SWITCH TO GLOBAL DATAMGR                     
         XC    DATAMGR,LDMGR                                                    
         XC    LDMGR,DATAMGR                                                    
*                                                                               
         TM    UPSI,TIME                                                        
         BNO   XXIT3                                                            
         L     R4,ADTIME                                                        
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,TERROR           ERROR                                        
         AP    4(4,R4),=P'1'       ADD 1 TO TIMES IN                            
         LA    R3,ASIDFLD                                                       
         EXTRACT (R3),'S',FIELDS=(ASID)                                         
         L     R2,ASIDFLD                                                       
         LOCASCB ASID=(R2)                                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         ST    R0,TIMOUT           SAVE TIME OUT                                
         S     R0,TIMIN            SUBTRACT TIME IN                             
         A     R0,0(R4)            ADD TO PREVIOUS TIME IN THIS ROUTINE         
         ST    R0,0(R4)            SAVE UPDATED TOTAL                           
         XC    ADTIME,ADTIME                                                    
         AP    TMONACC+4(4),=P'1'  COUNT TIMES TO MONACC                        
*                                                                               
XXIT3    DS    0H                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         TITLE 'BASE - DATA CONSTANTS'                                          
         SPACE 1                                                                
***********************************************************************         
* CONSTANTS FOR TIME ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
MODETAB  DS    0CL8                                                             
         DC    AL1(PROCTRNS),AL2(PTN00-ACM2),AL1(0),AL4(TPRCT)                  
         DC    AL1(PROCTIME),AL2(PTM00-ACM2),AL1(0),AL4(TPTIM)                  
         DC    AL1(PROCSBAC),AL2(PSA00-ACM2),AL1(0),AL4(TSBAF)                  
         DC    AL1(PROCACC),AL2(PA00-ACM2),AL1(0),AL4(TPRCA)                    
         DC    AL1(ACCLAST),AL2(AL00-ACM2),AL1(0),AL4(TACCL)                    
         DC    AL1(LEVCFRST),AL2(LCF00-ACM2),AL1(0),AL4(TLVCF)                  
         DC    AL1(LEVCLAST),AL2(LCL00-ACM2),AL1(0),AL4(TLVCL)                  
         DC    AL1(LEVBFRST),AL2(LBF00-ACM2),AL1(0),AL4(TLVBF)                  
         DC    AL1(LEVBLAST),AL2(LBL00-ACM2),AL1(0),AL4(TLVBL)                  
         DC    AL1(LEVAFRST),AL2(LAF00-ACM2),AL1(0),AL4(TLVAF)                  
         DC    AL1(LEVALAST),AL2(LAL00-ACM2),AL1(0),AL4(TLVAL)                  
         DC    AL1(REQFRST),AL2(RQF00-ACM2),AL1(0),AL4(TREQF)                   
         DC    AL1(REQLAST),AL2(RQL00-ACM2),AL1(0),AL4(TREQL)                   
         DC    AL1(RUNFRST),AL2(RNF00-ACM2),AL1(0),AL4(TRUNF)                   
         DC    AL1(RUNLAST),AL2(RNL00-ACM2),AL1(0),AL4(TRUNL)                   
         DC    X'FF',AL2(XXIT-ACM2),AL1(0),AL4(TOTHR)                           
         SPACE 1                                                                
ASIDFLD  DC    F'0'                                                             
TIMIN    DC    F'0'                                                             
TIMOUT   DC    F'0'                                                             
ADTIME   DC    F'0'                                                             
*                                                                               
TTAB     DS    0F                                                               
TRUNF    DC    F'0',PL4'0',CL8'RUNFIRST'                                        
TREQF    DC    F'0',PL4'0',CL8'REQFIRST'                                        
TLVAF    DC    F'0',PL4'0',CL8'LEVAFRST'                                        
TLVBF    DC    F'0',PL4'0',CL8'LEVBFRST'                                        
TLVCF    DC    F'0',PL4'0',CL8'LEVCFRST'                                        
TPRCA    DC    F'0',PL4'0',CL8'PROCACC '                                        
TSBAF    DC    F'0',PL4'0',CL8'SBACFRST'                                        
TPRCT    DC    F'0',PL4'0',CL8'PROCTRNS'                                        
TPTIM    DC    F'0',PL4'0',CL8'PROCTIME'                                        
TACCL    DC    F'0',PL4'0',CL8'ACCLAST '                                        
TLVCL    DC    F'0',PL4'0',CL8'LEVCLAST'                                        
TLVBL    DC    F'0',PL4'0',CL8'LEVBLAST'                                        
TLVAL    DC    F'0',PL4'0',CL8'LEVALAST'                                        
TREQL    DC    F'0',PL4'0',CL8'REQLAST '                                        
TRUNL    DC    F'0',PL4'0',CL8'RUNLAST '                                        
TOTHR    DC    F'0',PL4'0',CL8'OTHER   '                                        
TMONACC  DC    F'0',PL4'0',CL8'MONACC  '                                        
THOOKC   DC    F'0',PL4'0',CL8'HOOKCODE'                                        
TERROR   DC    F'0',PL4'0',CL8'ERRORS  '                                        
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0F                                                               
PHASE02  DC    CL8'ACM202 ',A(0)                BASE PHASE                      
PHASE03  DC    CL8'ACM203 ',A(0)                INITIALIZATION                  
PHASE04  DC    CL8'ACM204 ',A(0)                REPORTS                         
PHASE05  DC    CL8'ACM205 ',A(0)                SUBROUTINES                     
PHASE06  DC    CL8'ACM206 ',A(1)                TABLES                          
         DC    X'FF'                                                            
*                                                                               
ENTRYTAB DC    AL2(PUT-ACM202),AL2(APUT-MAND)                                   
         DC    X'FF'                                                            
*                                                                               
DICI     DS    0X                  INPUT DICTIONARY LIST                        
         DCDDL AC#TOTFR,L'LTOTFOR                                               
         DCDDL AC#RPT,L'LREPORT                                                 
         DCDDL AC#APGH1,L'LHEAD1                                                
DICIX    DC    AL1(0)                                                           
         SPACE 1                                                                
LASTCON  DS    CL15                                                             
ORIGKEYS DS    CL8                                                              
REVSIGN  DS    CL1                                                              
REVACC   DS    CL15                                                             
PAMT     DS    PL8                 POSTING AMOUNT                               
CHASW    DS    XL1                 POSTING LEVEL CHANGE CONTROL                 
CHACON   EQU   X'80'               CHANGE OF CONTRA                             
CHACLI   EQU   X'40'               CHANGE OF CLIENT                             
*                                                                               
TRCCNT   DC    PL2'0'                                                           
TRCMAX   DC    PL2'500'                                                         
FORMULA  DC    X'0000'                                                          
FORMULA2 DC    X'0000'                                                          
*                                                                               
ATRSEL   DS    F        A(TRANSACTION STATUS ELEMENT)                           
ATYPL    DS    A        A(CURRENT TYPELIST ENTRY)                               
ASOFDTE  DS    XL2      AS OF DATE FOR END MONTH ALLOCATION                     
DIRCON   DS    CL15     DIRECT LABOR CONTRA                                     
LISTPNT  DS    H                                                                
LISTNUM  DS    H        NUMBER IN RL POOL                                       
BBF      DS    PL8      BALANCE BROUGHT FORWARD                                 
LCNTRLV  DS    A        A(LEDGER ENTRY FOR CONTRA U/L)                          
LCNTRUL  DS    CL2      LAST CONTRA U/L                                         
SALAREA  DS    CL100                                                            
BDGTKEY  DS    CL42                                                             
LASTMODE DS    CL8                                                              
*                                                                               
BFLT1    DS    XL1       BUCKET CONTROL SWITCH -1                               
BFLT2    DS    XL1                              -2                              
BTYPE    DS    XL1       BUCKET TYPE FILTER                                     
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,NNN,A),FORMAT=BI,WORK=1 '                    
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=NNN '                                  
         EJECT                                                                  
LDMGR    DC    V(DATAMGR)          LOCAL COPY OF DATAMGR                        
         ENTRY UTL                                                              
UTL      DC    F'0',X'00'                                                       
         ORG   UTL+4                                                            
SE       DS    X                                                                
*                                                                               
OPEN     DC    C'OPEN'                                                          
ACFILE   DC    C'ACCOUNT'                                                       
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R9,R8,R7                                                      
         TITLE 'PUT - RECORD TO SORT/MERGE'                                     
***********************************************************************         
* PUT RECORD TO SORTER                                                *         
* R2 =  REPORT STACK ENTRY                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
PUT      DS    0D                                                               
         NMOD1 0,**PUT**,R9                                                     
         L     RC,BASERC                                                        
         MVC   SVASORT,APGSORT     SAVE APGSORT                                 
         TM    HOOKSTAT,HOOKSPRC   IS THIS A HOOK CALL                          
         BNO   PUT00                                                            
         MVC   APGSORT,4(R1)      HOOKS CALL TO MERGE                           
         L     R4,APGSORT                                                       
         B     PUT20                                                            
*                                                                               
PUT00    CLI   MODE,REQLAST        IS IT LAST TIME                              
         BE    PUT23               CLEAR BUFFER                                 
         TM    PCSW,PCFOH          POSTING FIXED OVERHEAD?                      
         BNO   PUT03                                                            
         LA    RE,SORTCOLS         MULTIPLE EACH COLUMN X PCT.                  
         L     R0,HIGHCOL                                                       
*                                                                               
PUT01    OC    0(8,RE),0(RE)       SKIP ZEROS                                   
         BNZ   *+10                                                             
         ZAP   0(8,RE),=P'0'                                                    
*                                                                               
         TM    7(RE),X'08'         AND NON-ADDITIVE                             
         BNO   PUT02                                                            
         ZAP   WRK,0(8,RE)         AMOUNT                                       
         MP    WRK,FOHR            X RATE                                       
         SRP   WRK,64-4,5          ROUNDED                                      
         ZAP   0(8,RE),WRK         BACK TO SORTCOLS                             
*                                                                               
PUT02    LA    RE,8(RE)                                                         
         BCT   R0,PUT01                                                         
*                                                                               
PUT03    OI    SRTSW,SRTNO         SET TO ASSUME NOT PUT TO SORT                
         ICM   RF,15,ASLNTRY       A(CURRENT SUPERLEDGER ENTRY)                 
         BZ    PUT04                                                            
         CLI   SORTTYPE,0          IS IT A VERTICAL PERCENT                     
         BNE   PUT04                                                            
*&&US                                                                           
*MNNEW   LA    RE,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RE,=X'80000000'                                                  
*MNNEW   BSM   0,RE                                                             
*                                                                               
         TM    SLPHPCT-SLPD(RF),HPDID+HPDIV                                     
         BZ    PUT03A                                                           
         L     R1,RSNROWS          GET LAST BYTE OF LAST ROW                    
         SLL   R1,4                                                             
         LA    R1,SORTLEVS(R1)                                                  
         BCTR  R1,0                                                             
         MVC   0(1,R1),SLPHPCT-SLPD(RF)      OPERATION CODE                     
*                                                                               
PUT03A   DS    0H                                                               
*MNNEW   LA    RE,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RE                                                             
*&&                                                                             
PUT04    TM    UPSI,TRCPUTS        TRACE PUTS TO SORT                           
         BNO   *+8                                                              
         BAS   RE,DMPSRT           DUMP THE SORT RECORDS                        
*                                                                               
PUT07    DS    0H                                                               
*MN      CLC   SORTKEY,0           NO KEY                                       
         CLI   SORTKEY,0           NO KEY                                       
         BE    PUT37               SKIP IT                                      
         L     R4,APGSORT          USE MYIO TO BUILD                            
         LR    RE,R4                                                            
         LA    RF,RECLN                                                         
         XCEFL                                                                  
         L     R1,HIGHROW          NUMBER OF ROWS                               
         SLL   R1,4                X LENGTH OF KEY (16)                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SORTKEY(0),SPACES                                                
         BE    PUT37                                                            
         LH    RF,DSPREP           DISPLACEMENT TO REPORT CODE                  
         AR    RF,R4                                                            
         MVC   0(4,RF),SORTREP     REPORT/COPY/TYPE/SPARE                       
         LH    RF,DSPCOL           DISPLACEMENT TO COLUMNS                      
         AR    RF,R4                                                            
         MVC   0(256,RF),SORTCOLS  COLUMNS                                      
         LH    R5,DSPNME           R4 = LEVELS                                  
         AR    R5,R4               R5 = NAMES                                   
         LA    RE,SORTLEVS                                                      
         LA    RF,SORTNAMS                                                      
         LA    R0,1                CURRENT ROW                                  
         LA    R6,RSNAMSQ          ROWS TO BE SORTED BY NAME                    
         LA    R3,SRTFLN           SORT FIELD LENGTHS                           
         SR    R1,R1                                                            
*                                                                               
PUT11    MVC   0(2,R4),0(RE)       REPORT/COPY                                  
         CLM   R0,1,0(R6)          DOES THIS ROW USE NAME SEQ                   
         BNE   PUT13                                                            
         MVC   2(36,R4),0(RF)      NAME                                         
         MVC   38(14,R4),2(RE)                                                  
         MVC   0(14,R5),2(RE)      CODE (LEVEL)                                 
         LA    R6,1(,R6)                                                        
         B     PUT15                                                            
*                                                                               
PUT13    MVC   2(14,R4),2(RE)      CODE (LEVEL)                                 
         MVC   0(36,R5),0(RF)      NAME                                         
*                                                                               
PUT15    IC    R1,0(,R3)           LENGTH OF SORT FIELD                         
         LA    R4,0(R1,R4)         LEVELS                                       
         LA    RE,16(,RE)                                                       
         LA    R5,36(,R5)          NAMES                                        
         LA    RF,36(,RF)                                                       
         LA    R3,1(,R3)           SORT FIELD LENGTHS                           
         AHI   R0,1                ROW NUMBER                                   
         C     R0,HIGHROW                                                       
         BNH   PUT11                                                            
*                                                                               
         L     R4,APGSORT                                                       
         LA    RE,RSHKSRT                                                       
         CLI   0(RE),CMSRHK        IS IT SORTHOOK (27)                          
         BNE   PUT16               NO                                           
         MVI   HOOKTYPE,CMSRHK     SORTHOOK (27)                                
         MVC   HOOKNUM,2(RE)       PASS THE HOOK NUMBER FOR THIS REPORT         
         ST    R4,HOOKAREC         A(OF THIS REPORTS HOOK MODULE)               
         OI    HOOKSTAT,HOOKSPRC   SET HOOK IN PROCESS                          
         GOTO1 AHOOKCDE,DMCB,(RA)                                               
         BZ    PUT16                                                            
         NI    HOOKSTAT,ALL-HOOKSPRC                                            
         B     PUT18                   DON'T PUT                                
*                                                                               
PUT16    NI    HOOKSTAT,ALL-HOOKSPRC                                            
         NI    SRTSW,ALL-SRTNO     TURN-OFF NOT PUT SWITCH                      
         ICM   RF,15,ASLNTRY       A(CURRENT SUPERLEDGER ENTRY)                 
         BZ    PUT17                                                            
*MNNEW   LA    RE,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RE,=X'80000000'                                                  
*MNNEW   BSM   0,RE                                                             
         OI    SLPSTAT-SLPD(RF),SLPGPR   POSTED THIS ACCOUNT                    
*MNNEW   LA    RE,*+6                    SET 24 BIT MODE                        
*MNNEW   BSM   0,RE                                                             
*                                                                               
PUT17    OI    ACTSW,ALL           ALL LEVELS ACTIVE                            
         OI    SRTSW,SRTAC         TURN-ON SORT ACTIVITY                        
*                                                                               
PUT18    TM    PCSW,PCMWR          TEST POSTING MWR                             
         BNO   PUT19                                                            
         LA    R1,RSMPCON          SET TO TEST CONTRA                           
         TM    RSMWR,RSMCLI        TEST CLIENT LEVEL REPORT                     
         BNO   *+8                                                              
         LA    R1,RSMPCLI          SET FOR CLIENT LEVEL                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    RSMWR,0             SET POSTED LEVEL                             
*                                                                               
PUT19    TM    SRTSW,SRTNO         DON'T PUT TO SORTER ?                        
         BO    PUT37                                                            
*                                                                               
PUT20    LA    R1,SORTBLK                                                       
         USING TSARD,R1                                                         
         LA    R2,TBUFREC          MOVE RECORD TO TBUFREC                       
         LH    R3,RECLNGTH                                                      
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
         LA    R4,TBUFREC                                                       
         ST    R4,TSAREC           RECORD TO BE READ                            
         MVI   TSOFFACT,TSARDH     READ FOR RECORD                              
         OC    TSPRECN(4),TSPRECN  ANY RECORDS IN BUFFER                        
         BZ    PUT21               NO, JUST ADD IT                              
         GOTO1 TSAROFF                                                          
         TM    TSERRS,TSERNF       RECORD FOUND                                 
         BNO   PUT31               YES, ADD ACCUMS FROM NEW RECORD              
*                                                                               
PUT21    L     R4,APGSORT                                                       
         ST    R4,TSAREC                                                        
         MVI   TSOFFACT,TSAADD     ADD NEW RECORD                               
         GOTO1 TSAROFF                                                          
         BE    PUT37               RECORD ADDED, ALL DONE                       
         TM    TSERRS,TSEEOF       BUFFER FULL?                                 
         BO    *+6                                                              
         DC    H'0'                ERROR                                        
*                                                                               
         GOTO1 ADSORTER,PARM,=C'PUT',(R4),0 PUT TO SORTER                       
         TM    UPSI,TRCMRGE        TRACE PUTS TO SORTER                         
         BNO   *+8                                                              
         BAS   RE,DMPMRG                                                        
*                                                                               
PUT23    LA    RE,TBUFREC          CLEAR TBUFREC                                
         LH    RF,=Y(L'TBUFREC)                                                 
         XCEFL                                                                  
         LA    R4,TBUFREC                                                       
         LA    R1,SORTBLK          PUT ALL TSAR RECORDS TO MERGER               
         ST    R4,TSAREC           SET A(TSAR RECORD)                           
         MVI   TSOFFACT,TSARDH     READ FOR FIRST RECORD                        
         OC    TSPRECN(4),TSPRECN  ANY RECORDS IN BUFFER                        
         BZ    PUT27               NO NEED TO READ                              
*                                                                               
PUT25    GOTO1 TSAROFF                                                          
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    PUT27                                                            
         GOTO1 ADSORTER,PARM,=C'PUT',(R4),0 PUT TO SORTER                       
         TM    UPSI,TRCMRGE        TRACE PUTS TO SORTER                         
         BNO   *+8                                                              
         BAS   RE,DMPMRG                                                        
         LA    R1,SORTBLK                                                       
         MVI   TSOFFACT,TSANXT                                                  
         B     PUT25                                                            
*                                                                               
PUT27    MVI   TSOFFACT,TSAINI     INIT AGAIN                                   
         MVC   TSAREC,=A(SRTLN)    SIZE OF BUFFER                               
         GOTO1 TSAROFF                                                          
         B     PUT37                                                            
*                                                                               
PUT31    L     R5,APGSORT          ADD THIS REOCRD TO TSAR RECORD               
         AH    R4,DSPCOL           DISPLACEMENT TO COLUMNS(OLD)                 
         AH    R5,DSPCOL           NEW                                          
         L     R0,HIGHCOL          NUMBER OF COLUMNS(MAX)                       
*                                                                               
PUT32    TM    7(R4),X'08'         TEST NON-ADDITIVE COLUMN                     
         BNO   PUT33                                                            
         TM    7(R5),X'08'                                                      
         BNO   PUT33                                                            
         AP    0(8,R4),0(8,R5)     ADD ACCUMULATORS                             
         B     PUT35                                                            
*                                                                               
PUT33    OC    0(8,R4),0(R4)       TEST BINARY ZERO                             
         BZ    *+14                                                             
         CLC   0(8,R4),NADD        TEST NON-ADDITIVE ZERO                       
         BNE   *+10                                                             
         MVC   0(8,R4),0(R5)                                                    
*                                                                               
PUT35    LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,PUT32                                                         
*                                                                               
         LA    R1,SORTBLK                                                       
         LA    R4,TBUFREC                                                       
         MVI   TSOFFACT,TSAWRT     WRITE BACK NEW RECORD                        
         GOTO1 TSAROFF                                                          
         BE    PUT37                                                            
         DC    H'0'                                                             
*                                                                               
PUT37    MVC   APGSORT,SVASORT     RESTORE APGSORT                              
*                                                                               
PUTX     XIT1                                                                   
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* DUMP THE SORT RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
DMPSRT   CP    TRCPCNT,=P'200'     ALREADY PRINTED  THE MAX                     
         BHR   RE                                                               
         STM   RE,R6,SVRE                                                       
*                                                                               
         USING BOXD,R4                                                          
*        L     R4,ADBXAREA                                                      
*        MVC   BOXWIDTH,=F'132'                                                 
         MVI   RCSUBPRG,2          TURN OFF BOXES                               
         LA    R0,SORTLNQ                                                       
         LA    RF,=C'PUTSORT'                                                   
         LA    R8,SORTREP                                                       
*                                                                               
         GOTO1 PRNTBL,DMCB,(7,(RF)),(R8),                              X        
               C'DUMP',(R0),=C'2D',(C'P',PRINT)                                 
*                                                                               
         AP    TRCPCNT,=P'1'                                                    
*        MVC   BOXWIDTH+3(1),SYSWIDTH                                           
         MVI   RCSUBPRG,0                                                       
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP THE SORT RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
DMPMRG   CP    TRCMCNT,=P'200'     ALREADY PRINTED  THE MAX                     
         BHR   RE                                                               
         STM   RE,R6,SVRE                                                       
*                                                                               
         USING BOXD,R4                                                          
         LR    R3,R4               R3=A(SORTER RECORD)                          
*        L     R4,ADBXAREA                                                      
*        MVC   BOXWIDTH,=F'132'                                                 
         MVI   RCSUBPRG,2          TURN OFF BOXES                               
         LH    R0,RECLNGTH                                                      
         LA    RF,=C'SORTER'                                                    
         GOTO1 PRNTBL,DMCB,(6,(RF)),(R3),                              X        
               C'DUMP',(R0),=C'2D',(C'P',PRINT)                                 
*                                                                               
         AP    TRCMCNT,=P'1'                                                    
*        MVC   BOXWIDTH+3(1),SYSWIDTH                                           
         MVI   RCSUBPRG,0                                                       
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
TRCPCNT  DC    PL2'0'                                                           
TRCMCNT  DC    PL2'0'                                                           
NADD     DC    XL7'00',X'04'                                                    
*                                                                               
SVASORT  DS    A                   SAVE ADDRESS OF APGSORT                      
TBUFREC  DS    CL(SORTLNQ)                                                      
*                                                                               
         LTORG                                                                  
         DROP  RB,R9                                                            
         EJECT                                                                  
         ENTRY SSB                                                              
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOSTAT2                                                         
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
       ++INCLUDE ACAPGGEND                                                      
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACREPM202 01/28/13'                                      
         END                                                                    
