*          DATA SET ACREPNB05  AT LEVEL 016 AS OF 07/31/12                      
*PHASE ACNB05C                                                                  
ACNB05   TITLE 'BILL POSTING ROUTINES'                                          
         PRINT NOGEN                                                            
ACNB05   CSECT                                                                  
BGN      DS    0X                                                               
         NMOD1 0,**NB05,RA                                                      
         USING NBILD,R8                                                         
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         L     RC,BASERC                                                        
         USING ACWORKD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,PROCACC                                                     
         BE    ACCF                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEVALAST                                                    
         BE    LVAL                                                             
         CLI   MODE,LEVBLAST                                                    
         BE    LVBL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
RUNF     LARL  RF,WRKF                                                          
         ST    RF,AWRKF                                                         
         XC    WKID,WKID                                                        
         LA    RF,WKID                                                          
         USING UKRECD,RF                                                        
         MVC   UKUSRID,ORIGINUM    ORIGIN ID                                    
         MVI   UKSYSPRG,C'A'                                                    
         MVC   UKSYSPRG+1(2),PGMCDE                                             
*                                                                               
         PACK  DUB(2),RCDATE+3(3)  DAY                                          
         MVC   UKDAY,DUB                                                        
         MVI   UKCLASS,C'P'         POSTING FILE                                
         OI    UKFLAG,X'01'        ALLOW DUPLICATE FILES                        
         DROP  RF                                                               
*                                                                               
         GOTOR AWRKF,WRKOPNQ                                                    
*                                                                               
         ZAP   POSTRECS,PZERO                                                   
         ZAP   POSTDR,PZERO                                                     
         ZAP   POSTCR,PZERO                                                     
         ZAP   MARKED,PZERO                                                     
*                                                                               
         LA    R1,GRPBUK           CLEAR GROUP TOTALS                           
         LA    R0,NALLBK                                                        
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         BCT   R0,*-10                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ACCOUNT FIRST                                                       *         
***********************************************************************         
ACCF     DS    0H                                                               
         LA    R1,BUKS                                                          
         LA    R0,NBUKS                                                         
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         BCT   R0,*-10                                                          
*                                                                               
         XC    BTCH,BTCH           SOMETIMES TRAILING ZEROS                     
         MVC   BTCH(2),BILMOSC                                                  
         MVC   BTCH2,SPACES        SOMETIMES TRAILING SPACES                    
         MVC   BTCH2(2),BILMOSC                                                 
*                                                                               
         L     R6,APMDEL                                                        
         USING PMDELD,R6                                                        
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         LA    R3,COMMAC           COMMISSION ACCOUNT                           
         USING ACTRECD,R3                                                       
         MVC   ACTKCULA,PMDCOM1                                                 
         CLI   GOTBC,C' '          COMMISSION ACCOUNT IN GOBLOCK?               
         BNH   *+10                                                             
         MVC   ACTKCULA,GOTBC                                                   
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'ACTKCULA),ACTKCULA                                        
         GOTO1 ADMGR,ACCRDQ        READ FOR COMMISSION ACCOUNT                  
         MVC   AIO,AIO4                                                         
         GOTO1 ADMGR,ACCGETQ                                                    
*                                                                               
         LA    R3,PRCDAC                  PRINT CD ACCOUNT                      
         MVC   ACTKCULA,SPACES                                                  
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),PRCDDUL         DEAULT UL                             
         MVC   ACTKACT(L'PRCDDAC),PRCDDAC DEFAULT ACCOUNT                       
         CLI   PMDPRCD,C' '               ANY PRINT CD ACCOUNT ?                
         BNH   *+10                                                             
         MVC   ACTKCULA,PMDPRCD           YES, SAVE IT                          
*                                                                               
         LA    R3,CSHDAC                  CASH DISCOUNT                         
         MVC   ACTKCULA,SPACES                                                  
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),CSHDDUL         DEAULT UL                             
         MVC   ACTKACT(L'CSHDDAC),CSHDDAC DEFAULT ACCOUNT                       
         CLI   PMDCSHD,C' '               YES, ANY CD ACCOUNT ?                 
         BNH   *+10                                                             
         MVC   ACTKCULA,PMDCSHD                                                 
*                                                                               
         L     R2,ADPROFIL                                                      
         USING PPRELD,R2                                                        
         LA    R3,COSTAC           COSTING ACCOUNT                              
         MVC   ACTKCULA,PPRCOST                                                 
*                                                                               
         L     R2,AIO4             INCOME ACCOUNT                               
         MVI   ELCODE,RSTELQ                                                    
         BRAS  RE,GETELN                                                        
         BNE   ACCF15                                                           
*                                                                               
         USING RSTELD,R2                                                        
         OC    RSTCCTR,SPACES                                                   
         CLI   RSTCCTRR,0          IF ZERO START AT COSTING+7                   
         BE    *+20                                                             
         LA    R4,ACTKLDG                                                       
         LLC   R0,RSTCCTRR         INSERT STARTING POINT NUMBER                 
         AR    R4,R0               POINT TO NEW STARTING POINT                  
         B     *+8                                                              
         LA    R4,ACTKACT+4        COSTING ACCOUNT                              
         LA    RF,3                                                             
         LA    R5,RSTCCTR                                                       
*                                                                               
ACCF9    CLI   0(R5),C' '                                                       
         BE    *+10                                                             
         MVC   0(1,R4),0(R5)       REPLACE OVERRIDES IN COSTING KEY             
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   RF,ACCF9                                                         
*                                                                               
ACCF15   LA    R3,REVNAC                  REVENUE ACCOUNT                       
         MVC   ACTKCULA,SPACES                                                  
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),REVNDUL         REVENUE DEAULT UL                     
         MVC   ACTKACT(L'REVNDAC),REVNDAC DEFAULT ACCOUNT                       
         CLI   PMDANAL,C' '               ANY OVERRIDE ?                        
         BNH   *+10                                                             
         MVC   ACTKACT(L'PMDANAL),PMDANAL                                       
         CLI   PMDLN,PMDLN2Q              IS THIS A NEW ELEMENT ?               
         BL    *+18                       NO                                    
         CLI   PMDCOST,C' '               ANY COST ANALYSIS ACCOUNT ?           
         BNH   *+10                                                             
         MVC   ACTKACT(L'PMDCOST),PMDCOST                                       
*                                                                               
         L     R2,AIO4                                                          
         MVI   ELCODE,SPAELQ       GET SPECIAL POSTING ELEMENT                  
         BRAS  RE,GETELN                                                        
         B     *+8                                                              
         BRAS  RE,NEXTEL                                                        
         BNE   ACCF17                                                           
         USING SPAELD,R2                                                        
         CLI   SPATYPE,SPATANAL     LOOK FOR ANALYSIS POINTER                   
         BNE   *-12                                                             
         MVC   ACTKACT(L'SPAAULA-2),SPAAULA                                     
*                                                                               
ACCF17   MVC   GBILAC,REVNAC       11 ACCOUNT = 12 ACCOUNT                      
         LA    R3,GBILAC                                                        
         MVI   ACTKLDG,C'1'                                                     
*                                                                               
         MVC   ACCCDE,RECVAC                                                    
         GOTO1 AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   RECVAN,ACCNAM       SAVE ACC NAME NAME                           
*                                                                               
         MVC   ACCCDE,COSTAC                                                    
         GOTO1 AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   COSTAN,ACCNAM       SAVE NAME                                    
*                                                                               
         MVC   ACCCDE,COMMAC       GET COMMISSION ACCOUNT NAME                  
         GOTO1 AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   INCN,ACCNAM         SAVE NAME                                    
*                                                                               
ACCF21   DS    0H                                                               
         MVC   SKACC,COMMAC                                                     
         MVI   SKACC+(ACTKLDG-ACTRECD),C'K'  SET SK ACCOUNT                     
*                                                                               
         XC    SALEAC,SALEAC       CLEAR SALES ACCOUNT                          
         XC    SALEAN,SALEAN                                                    
         L     R2,ADHEIRB          GET SALES ACCOUNT FROM PRODUCT               
         MVI   ELCODE,SANELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ACCF23                                                           
         USING SANELD,R2                                                        
         MVC   SALEAC,SANCODE                                                   
         MVC   SALEAN,SANNAME                                                   
*                                                                               
ACCF23   MVC   MEDNAM,SPACES       MEDIA NAME                                   
         MVC   MEDNAM(L'MEDNS),MEDNS                                            
         CLC   REVNAC+3(12),=CL12'3'                                            
         BNE   *+16                                                             
         MVC   MEDNAM,SPACES                                                    
         MVC   MEDNAM(L'PRODCTN),PRODCTN  'PRODUCTION'                          
*                                                                               
         MVC   RECEIVBL,RECVAC     RECEIVABLE ACCOUNT                           
         OC    AGYBLDR,AGYBLDR     BILLING DEBIT ACCOUNT(STUDIO JOBS)           
         BZ    *+10                                                             
         MVC   RECEIVBL,AGYBLDR                                                 
         B     XIT                                                              
         DROP  R2,R3,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
ACCL     DS    0H                                                               
         CLI   RETLSQN,0           TEST SECOND PASS FOR RETAIL                  
         BNE   ACCL9               YES, CAN'T REVERSE SK                        
*                                                                               
         TM    WKFOPT,WKFPRTW      TEST PRINTING WORKER FILE                    
         BNO   ACCL2               NO,                                          
         L     R3,APHEAD                                                        
         XC    0(L'PHEAD,R3),0(R3) ADD AN ACCOUNT HEADER                        
         USING FFTELD,R3                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTWFSA    START ACCOUNT                                
         MVC   FFTDATA(L'QUESTOR),QUESTOR                                       
         MVC   FFTDATA+L'QUESTOR(L'CPJ),CPJ                                     
         LA    R1,L'QUESTOR+L'CPJ                                               
         STC   R1,FFTDLEN          DATA LENGTH                                  
         AHI   R1,FFTLN1Q+1                                                     
         STC   R1,FFTLN            ELEMENT LENGTH                               
         AHI   R1,4                RECORD LENGTH                                
         L     RF,APREC                                                         
         XC    0(4,RF),0(RF)                                                    
         STCM  R1,3,0(RF)                                                       
         GOTOR AWRKF,WRKADDQ       ADD ACCOUNT HEADER ELEMENT                   
         DROP  R3                                                               
*                                                                               
ACCL2    MVI   PTASB2,0                                                         
         L     R5,ABTRN             LOOK FOR SK ITEMS                           
         USING BTRND,R5                                                         
         ICM   R0,15,NBTRN                                                      
ACCL3    OC    BTRNUSD,BTRNUSD     TEST ALREADY USED                            
         BNZ   ACCL7               SKIP IT                                      
         TM    BTRNSTA,BTRNSSK     TEST SK ITEM                                 
         BNO   ACCL5               NO,                                          
         TM    BTRNSTA,BTRNSASR    TEST ASP REVERSAL                            
         BO    ACCL5               YES, SKIP IT                                 
         GOTO1 RVSKP,DMCB,(R5)     REVERSE SK                                   
*                                                                               
ACCL5    CLI   BTRNTYP,TRNTEPRD    TEST EST. PROD.                              
         BNE   ACCL7                                                            
         GOTO1 RVEPP,DMCB,(R5)     REVERSE EP                                   
         OI    JFLG,JFREP          SET REVERSE EP                               
*                                                                               
ACCL7    LA    R5,BTRNLQ(R5)                                                    
         BCT   R0,ACCL3                                                         
*                                                                               
ACCL9    MVI   PTASB1,PTASCASH     SET PTA STATUS                               
         TM    PGMSTA,PGMUNB       UNBILLING                                    
         BNO   *+8                                                              
         OI    PTASB1,PTASREVS                                                  
*                                                                               
         BAS   RE,BLDTAX           BUILD LIST OF TAX ACCOUNTS                   
*                                                                               
         ICM   R7,15,CRTLR         CURRENT RETAILER ENTRY                       
         BZ    ACCL15                                                           
         USING RTLD,R7                                                          
         MVC   CRTLACC,RTLDIS      SET RETAILER: ACCOUNT CODE                   
         MVC   CRTLNME,RTLNME                    NAME                           
*                                                                               
         ZAP   SVCINTL$,CINTL$     SAVE INTERNAL INCOME $                       
         ZAP   CINTL$,RTLNET+(JXBSKI-JXBK)(L'JXBK)                              
         ZAP   NLINT$,RTLNET+(JXBNET-JXBK)(L'JXBK)                              
         SP    NLINT$,CINTL$        LESS CURRENT INTERNAL INCOME                
*                                                                               
         MVC   SVRECEIV,RECEIVBL   SAVE DEFAULT ACCOUNTS                        
         MVC   SVCOSTAC,COSTAC                                                  
*                                                                               
         CLI   RTLRECV,C' '        SET RETAILER - RECEIVABLE                    
         BNH   *+10                                                             
         MVC   RECEIVBL,RTLRECV                                                 
         CLI   RTLCOST,C' '        COSTING                                      
         BNH   *+10                                                             
         MVC   COSTAC,RTLCOST                                                   
*                                                                               
ACCL15   BAS   RE,BILLP            NORMAL BILL POSTINGS                         
         BAS   RE,CANTP            CANADIAN TAX POSTINGS                        
         BAS   RE,INTLP            INTERNAL INCOME POSTINGS                     
         BAS   RE,ICOMP            INTER-COMPANY PAYABLES POSTINGS              
*                                                                               
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BNO   ACCL17              NO,                                          
         MVI   PTYPE,BILLPQ        SET BILL TYPE                                
         LARL  R4,GRPS3AP                                                       
         GOTOR BLDAPE,DMCB,(R4)    BUILD APE LIST                               
         BRAS  RE,GRPAPE           ADD TO GROUP LSIT                            
*                                                                               
ACCL17   LTR   R7,R7               TEST RETAIL SCHEME                           
         BZ    ACCL19              NOT RETAIL                                   
         OI    PTASB2,PTASRETB     AFTER FIRST MARK NOT UNBILLABLE              
         ZAP   CINTL$,SVCINTL$     RESTORE INTERNAL INCOME $                    
         MVC   RECEIVBL,SVRECEIV   RESTORE DEFAULT ACCOUNTS                     
         MVC   COSTAC,SVCOSTAC                                                  
*                                                                               
ACCL19   TM    RUNOPT,RUNTST       RUN=TEST                                     
         BZ    XIT                 NO, SKIP TEST                                
         CLI   RCPOSTNG,NO         WANT POSTINGS                                
         BE    XIT                 NO, SKIP TEST                                
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BO    XIT                 YES, CAN'T TEST TOTAL YET                    
         CP    POSTDR,POSTCR                                                    
         BE    ACCL21                                                           
         GOTOR AWRKF,WRKCLSQ       CLOSE WORKER FILE                            
         GOTOR AWRKF,WRKKEPQ       KEEP WORKER FILE                             
         DC    H'0'                FILE OUT OF BALANCE                          
*                                                                               
ACCL21   DS    0H                                                               
         J     XIT                                                              
                                                                                
         DROP  R5,R7                                                            
         EJECT                                                                  
***********************************************************************         
* LEVEL A LAST                                                        *         
***********************************************************************         
LVAL     DS    0H                                                               
         MVC   PRD,SPACES                                                       
         B     LVBL                                                             
         EJECT                                                                  
***********************************************************************         
* LEVEL B LAST                                                        *         
***********************************************************************         
LVBL     DS    0H                                                               
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BNO   LVBL5                                                            
*                                                                               
         MVC   MEDNS,PRODCTN                                                    
         TM    OPTB,OPTBMED        MEDIA ONLY                                   
         BNO   *+10                                                             
         MVC   MEDNS,PRNTMED       'PRINT NEDIA'                                
*                                                                               
         LA    R0,BK$              MOVE GROUP BUCKEST                           
         LA    R1,BKLNQ                                                         
         LA    RE,GRPBUK                                                        
         LA    RF,BKLNQ                                                         
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,SETAMTS          SET AMOUNT STATUS                            
         MVC   JOB,=CL6'GROUP'                                                  
         MVC   JOBNAM,SPACES                                                    
*                                                                               
         GOTOR MAIN,GROUPQ                                                      
*                                                                               
LVBL5    LA    R1,GRPBUK           CLEAR GROUP TOTALS                           
         LA    R0,NALLBK                                                        
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         BCT   R0,*-10                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
REQL     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
RUNL     DS    0H                                                               
         TM    WKFOPT,WKFNONE      TEST NO FILE ALREADY SET                     
         BO    XIT                 YES,                                         
*                                                                               
         CP    POSTRECS,PZERO      TEST ZERO POSTINGS                           
         BE    *+12                YES,                                         
         CLI   RCPOSTNG,NO         TEST POSTING=NO                              
         BNE   *+12                NO,                                          
         OI    WKFOPT,WKFNONE      SET NO FILE                                  
         B     XIT                                                              
*                                                                               
         L     R3,APHEAD                                                        
         XC    0(L'PHEAD,R3),0(R3)                                              
         USING PSSUBFD,R3          ADD THE POSTING TRAILER                      
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC,=C'PROD. BILLING  '                                     
         ZAP   PSSBRECS,POSTRECS                                                
         ZAP   PSSBCASH,POSTDR                                                  
         TM    RUNOPT,RUNTST       RUN=TEST                                     
         BO    RUNL03              YES, FILE MUST BE IN BALANCE                 
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         BO    RUNL03              YES, FILE MUST BE IN BALANCE                 
         B     RUNL05              FOR LIVE OVERNIGHT - CAN'T DIE               
*                                                                               
RUNL03   CP    POSTDR,POSTCR                                                    
         BE    *+6                                                              
         DC    H'0'                FILE OUT OF BALANCE                          
*                                                                               
RUNL05   LA    RF,PSSUBFL+4                                                     
         SLL   RF,16                                                            
         L     RE,APRLEN                                                        
         STCM  RF,15,0(RE)                                                      
         DROP  R3                                                               
*                                                                               
         GOTOR AWRKF,WRKADDQ       ADD TOTAL RECORD                             
         GOTOR AWRKF,WRKCLSQ       CLOSE WORKER FILE                            
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         BNO   XIT                 NO,                                          
         GOTOR AWRKF,WRKKEPQ       KEEP WORKER FILE                             
         BRAS  RE,ADDTRN                                                        
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REVERSE SK POSTINGS                                                 *         
***********************************************************************         
RVSKP    NTR1  ,                                                                
         L     R5,0(R1)                                                         
         USING BTRND,R5                                                         
         MVC   SKRV,BTRND          COPY TO LOCAL STORAGE                        
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
         ZAP   NET$,JOBNET                                                      
         AP    CINTL$,JOBNET       CURRENT INTERNAL INCOME                      
         BAS   RE,SETAMTS                                                       
         MVC   DA,BTRNDA           GET THE TRANSACTION                          
         MVC   AIO,AIO4                                                         
         GOTO1 ADMGR,ACCGETQ                                                    
         L     R2,AIO4                                                          
*                                                                               
         LA    R3,SKRACC                                                        
         USING ACTRECD,R3                                                       
         MVC   ACTKCULA,BTRNCA     GET SK ACCOUNT FROM CONTRA                   
         MVI   PTYPE,RVSKPQ        ASSUME CONTRA SK                             
         CLC   ACTKUNT(2),SKUL     IS CONTRA SK ?                               
         BNE   RVSKP5              IF NOT, MUST BE TYPE 49                      
         MVI   ELCODE,SCIELQ                                                    
         BRAS  RE,GETELN                                                        
RVSKP3   BNE   RVSKP7                                                           
         USING SCIELD,R2                                                        
         CLI   SCITYPE,SCITCOMM    GET OVERRIDE AMOUNT (IF PRESENT)             
         BE    *+12                                                             
         BRAS  RE,NEXTEL                                                        
         B     RVSKP3                                                           
         ZAP   NET$,SCIAMNT                                                     
         B     RVSKP7                                                           
*                                                                               
RVSKP5   MVC   ACTKULA,SPACES      FOR TYPE 49 GET SK FROM ELEMENT              
         MVI   PTYPE,RV1RPQ        CONTRA 1R                                    
         MVI   ELCODE,SPDELQ                                                    
         BRAS  RE,GETELN                                                        
         BE    *+6                                                              
         DC    H'0'                NO SPDEL ?                                   
         USING SPDELD,R2                                                        
         LLC   R1,SPDLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKULA(0),SPDACCS  GET SK ACCOUNT                               
         CLC   ACTKUNT(2),SKUL                                                  
         BNE   XIT                 NOT SK - JUST SKIP IT                        
*                                                                               
RVSKP7   MVC   ANLINA,ACTKCULA     SET SI ACCOUNT FROM SK                       
         MVI   ANLINA+2,C'I'                                                    
         MVC   SIAC,SPACES                                                      
         GOTO1 AANAL               GET ANALYSIS ACCOUNTS (11 & 12)              
         CLI   ANLGBA+(ACTKACT-ACTKEY),C' '                                     
         BNH   *+10                                                             
         MVC   SIAC,ANLGBA+(ACTKACT-ACTKEY)                                     
         MVC   WRKCCUR,BTRNWC      SET WORKCODE FOR RFL ELEMENT                 
*                                                                               
         LLC   R1,PTYPE            SET TYPE                                     
         GOTOR MAIN                MAKE POSTINGS                                
         ST    R5,CBTRN                                                         
         GOTO1 AADDREP,INTINV      ADD RECORD FOR INTERNAL INVOICE RPT          
         B     XIT                                                              
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
RV1RS1K  DS    0H                                                               
RVSKS2K  NTR1  ,                   ** HOOK FOR SI POSTING **                    
         L     R7,APTRAN                                                        
         USING TRNELD,R7                                                        
         LA    R5,SKRV             USE LOCAL COPY                               
         USING BTRND,R5                                                         
         CLI   PTYPE,RV1RPQ        IS IT 1R CONTRA ?                            
         BNE   XIT                                                              
         TM    BTRNSTA,BTRNSNC     IS IT NON-COMM ?                             
         BNO   XIT                                                              
         OI    TRNSTAT,TRNSNOCM    MAKE SI POSTING NON-COMM                     
         B     XIT                                                              
         DROP  R5,R7                                                            
         EJECT                                                                  
***********************************************************************         
* REVERSE ESTIMATED PRODUCTION                                        *         
***********************************************************************         
RVEPP    NTR1  ,                                                                
         L     R5,0(R1)                                                         
         USING BTRND,R5                                                         
         MVC   EPRV,BTRND          COPY TO LOCAL STORAGE                        
         LA    R3,EPCRAC                                                        
         USING ACTRECD,R3                                                       
         MVC   ACTKCULA,BTRNCA                                                  
         MVC   ACCCDE,BTRNCA                                                    
         GOTO1 AGETNAM             GET CONTRA NAME                              
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   DA,BTRNDA           MUST GET RECORD TO GET CHECK CONTRA          
         MVC   AIO,AIO4                                                         
         GOTO1 ADMGR,ACCGETQ                                                    
         L     R2,AIO4                                                          
         MVI   ELCODE,SPDELQ       CHECK FOR CREDIT TO CASH ACCOUNT             
         BRAS  RE,GETELN                                                        
         BNE   RVEPP3                                                           
         USING SPDELD,R2                                                        
         MVC   ACTKULA,SPACES                                                   
         LLC   R1,SPDLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKULA(0),SPDACCS                                               
*                                                                               
RVEPP3   LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
         ZAP   NET$,JOBNET                                                      
         MP    NET$,=P'-1'                                                      
         AP    T47N$,JOBNET                                                     
         AP    T47C$,JOBCOM                                                     
*                                                                               
         GOTOR MAIN,RVEPPQ         MAKE POSTINGS                                
         B     XIT                                                              
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
RVEPS1K  NTR1  ,                   ** HOOK FOR SJ POSTING **                    
         L     R7,APTRAN                                                        
         USING TRNELD,R7                                                        
         LA    R5,EPRV             USE LOCAL COPY                               
         USING BTRND,R5                                                         
         TM    BTRNSTA,BTRNSNC     IS IT NON-COMM ?                             
         BNO   XIT                                                              
         OI    TRNSTAT,TRNSNOCM    MAKE SI POSTING NON-COMM                     
         B     XIT                                                              
*                                                                               
RVEPNAK  NTR1  ,                   ** HOOK FOR NARRATIVE **                     
         L     R7,APTRAN                                                        
         USING TRNELD,R7                                                        
         MVC   TRNNARR(L'EPRCMT),EPRCMT  SET REVERSE COMMENT                    
         LA    R1,L'EPRCMT+TRNLN1Q                                              
         STC   R1,TRNLN            SET DEFAULT LENGTH                           
         LA    R5,EPRV             LOCAL COPY                                   
         ST    R5,CBTRN            SET CURRENT ENTRY                            
         GOTO1 AXTRT,GTRNNRQ       GET NARRATIVE                                
         LLC   R1,DMCB             LENGTH OF NARRATIVE                          
         SHI   R1,L'EPCMT          LESS LENGTH OF EP COMMENT                    
         LTR   R1,R1                                                            
         BNP   XIT                                                              
         BCTR  R1,0                                                             
         LA    R3,TRNNARR+L'EPRCMT                                              
         MVI   0(R3),C' '                                                       
         L     RE,DMCB             RE=A(EP NARRATIVE)                           
         AHI   RE,L'EPCMT                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)       EPR NARRATIVE + USER NARRATIVE               
         LA    R1,1+L'EPRCMT+TRNLN1Q(R1)                                        
         STC   R1,TRNLN            SET ELEMENT LENGTH                           
         B     XIT                                                              
         DROP  R5,R7                                                            
         EJECT                                                                  
***********************************************************************         
* HOOK FOR ASP POSTINGS                                               *         
***********************************************************************         
         USING PCD,R5                                                           
ASPHK    NTR1  ,                                                                
         L     R4,APHEAD                                                        
         USING PSHEADD,R4                                                       
         L     R7,APTRAN                                                        
         USING TRNELD,R7                                                        
         MVC   TRNNARR(L'RETNARR),RETNARR   SET NARRATIVE                       
         TM    ASPIND,ASPISKC      TEST SK POSTINGS COMPLETE                    
         BO    ASPHK5              YES, SKIP THE REST                           
*                                                                               
         CLC   ASPACC+1(2),=C'SK'  TEST ASP ACCOUNT IS SK                       
         BNE   ASPHK3              NO,                                          
         TM    ASPIND,ASPIBIL      BILLING THIS TIME                            
         BO    ASPHK2              YES, DO POSTINGS                             
         TM    PCSTAT,PCSASK       TEST THIS IS SK POSTING                      
         BNO   XIT                 NO, DO IT                                    
         OI    ASPIND,ASPISKC      SET COMPLETE                                 
         J     XIT                 DO FIRST SK POSTING                          
*                                                                               
ASPHK2   TM    PCSTAT,PCSASI       TEST THIS IS SI POSTING                      
         JNO   XIT                 NO, IT'S OK                                  
         MVC   PSHDAUL,=C'SI'      MAKE IT SI                                   
         J     XIT                                                              
*                                                                               
ASPHK3   TM    PCSTAT,PCSASK       TEST THIS IS SK POSTING                      
         JNO   XIT                 NO,                                          
ASPHK5   MVI   TRNEL,0             YES, SKIP IT                                 
         J     XIT                                                              
*                                                                               
RETNARR  DC    C'** CURRENT MONTHS LABOR CONTINGENCY'                           
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* REGULAR BILL POSTINGS                                               *         
***********************************************************************         
BILLP    NTR1  ,                                                                
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         MVC   BK$(BK$LQ),DUE                                                   
         SP    GRS$,MED$           SUBTRACT MEDIA FROM GROSS                    
         AP    PAY$,POBCUR                                                      
*                                                                               
         ZAP   XCSD$,CSD$                                                       
         TM    JXBLTYP,TOTL+ONEL                                                
         BZ    *+10                                                             
         ZAP   XCSD$,JOTOTCSD                                                   
*                                                                               
         ZAP   NLINT$,NET$         NET                                          
         SP    NLINT$,CINTL$        LESS CURRENT INTERNAL INCOME                
         BAS   RE,SETAMTS                                                       
*                                                                               
         TM    OPTC,OPTCPESK       %EST ONLY -INCOME TO SK                      
         BNO   BILLP2                                                           
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BO    BILLP2              YES, SKIP SK COMMISSION                      
         ZAP   SKCOM$,COM$                                                      
*                                                                               
BILLP2   ZAP   TCOM$,SK2SICOM      AMOUNT FROM SK TO SI                         
         ZAP   TCMSK$,SK2SICOM                                                  
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BO    BILLP3              DON'T ADD COMMISSION                         
         AP    TCMSK$,COM$         ADD SKCOMMISSION                             
         TM    OPTC,OPTCPESK       POST %EST INCOME TO SK                       
         BO    BILLP3                                                           
         AP    TCOM$,COM$          +COMMISSIOM = TOTAL COMMISSION               
*                                                                               
BILLP3   SP    SKCOM$,SK2SICOM                                                  
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BNO   BILLP5                                                           
         SP    GRS$,COM$           GROSS LESS COMMISSION                        
         SP    RCV$,COM$                                                        
         SP    GRSZ$,COM$                                                       
*                                                                               
BILLP5   L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
*                                                                               
         TM    OPT4,OPT4XTFB       EXCLUDE TAX FROM BILLING AMOUNT              
         BO    *+10                                                             
         AP    GRS$,TAX$                                                        
         ZAP   GRSZ$,GRS$                                                       
*                                                                               
         CLI   GOZERO,YES                                                       
         BNE   *+10                                                             
         ZAP   GRSZ$,PZERO                                                      
*                                                                               
         GOTO1 AINTRNL,INTLELMQ    BUILD INTERNAL INCOME ELEMENTS               
         TM    OPTB,OPTBMED        MEDIA ONLY                                   
         BNO   *+10                                                             
         MVC   MEDNS,PRNTMED       'PRINT NEDIA'                                
         GOTOR MAIN,BILLPQ                                                      
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BNO   BILLP9                                                           
*                                                                               
         LA    RE,GRPBUK           ADD BILL TOTALS TO GROUP                     
         LA    RF,BK$                                                           
         LA    R0,NALLBK                                                        
         AP    0(L'BK$,RE),0(L'BK$,RF)                                          
         LA    RE,L'BK$(RE)                                                     
         LA    RF,L'BK$(RF)                                                     
         BCT   R0,*-14                                                          
*                                                                               
BILLP9   B     XIT                                                              
*                                                                               
SVRECEIV DS    XL(L'RECEIVBL)                                                   
SVCOSTAC DS    XL(L'COSTAC)                                                     
SVCINTL$ DS    XL(L'CINTL$)                                                     
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CANADIAN TAX POSTINGS                                               *         
***********************************************************************         
CANTP    NTR1  ,                                                                
         MVI   TAXTYP,C'G'                                                      
         L     R4,AGSTBUF                                                       
         USING CTDLD,R4                                                         
CANTP3   CLI   0(R4),0                                                          
         BE    CANTP5                                                           
         MVC   CTAXG,0(R4)                                                      
         LA    R6,CTAXGST                                                       
         USING CTXD,R6                                                          
         LA    R7,CTAXGST$                                                      
         USING CTAXD,R7                                                         
         BAS   RE,CANTPALL                                                      
         LA    R4,CTDLLNQ(R4)                                                   
         B     CANTP3                                                           
*                                                                               
CANTP5   MVI   TAXTYP,C'P'                                                      
         L     R4,APSTBUF                                                       
CANTP7   CLI   0(R4),0                                                          
         BE    XIT                                                              
         MVC   CTAXP,0(R4)                                                      
         LA    R6,CTAXPST                                                       
         LA    R7,CTAXPST$                                                      
         BAS   RE,CANTPALL                                                      
         LA    R4,CTDLLNQ(R4)                                                   
         B     CANTP7                                                           
*                                                                               
CANTPALL LR    R0,RE                                                            
         BAS   RE,SETTAX           SET TAX AMOUNTS                              
         CP    TAXTAX$,PZERO       TEST ANY TAX                                 
         BE    CANTPX              NO, SKIP POSTING                             
CANTPAL3 GOTOR MAIN,CANTPQ         MAKE TAX POSTINGS                            
*                                                                               
CANTPX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* INTERNAL INCOME POSTINGS                                            *         
*  NOTE: IN ORDER TO KEEP THE POSTINGS IN THE SAME SEQUENCE AS THE    *         
*        AC21 WE WILL PROCESS WC'S THAT HAVE PREVIOUS AMOUNTS FIRST   *         
*        THEN WC'S THAT HAVE CURRENT POSTINGS.                        *         
*        IF THIS GOES LIVE AND THE AC21 IS NO LONGER - YOU CAN DO THIS*         
*        IN ONE PASS INSTEAD OF 2. THE SEQUENCE IS NOT IMPORTANT.     *         
***********************************************************************         
INTLP    NTR1  ,                                                                
         MVI   PTYPE,INTLPQ        SET FLAG FOR ANAL ROUTINE IN BASE            
         MVI   INTLPASS,0          FIRST DO "PREVIOUS"                          
INTLP1   GOTO1 AINTRNL,INTLFSTQ    GET DATA FOR INTERNAL/INCOME POSTING         
         BNE   XIT                 NO INTERNAL INCOME                           
*                                                                               
INTLP3   CP    INTINAM,PZERO       SKIP ZERO POSTINGS                           
         BE    INTLP9                                                           
         CLC   INTLPASS,INTSTAT    TEST STATUS                                  
         BNE   INTLP9              NO, SKIP IT FOR NOW                          
*                                                                               
INTLP7   MVC   WRKCCUR,INTWKC      SET WORKCODE                                 
         ZAP   COM$,INTINAM                                                     
         GOTOR MAIN,INTLPQ         MAKE INCOME POSTINGS                         
*                                                                               
INTLP9   GOTO1 AINTRNL,INTLNXTQ    GET NEXT INCOME DATA                         
         BE    INTLP3                                                           
         CLI   INTLPASS,INTSCUR    FINISHED SECOND PASS                         
         BE    XIT                                                              
         MVI   INTLPASS,INTSCUR    SET SECOND PASS(CURRENT ITEMS)               
         B     INTLP1                                                           
         EJECT                                                                  
***********************************************************************         
* INTERCOMPANY POSTINGS                                               *         
***********************************************************************         
ICOMP    NTR1  ,                                                                
         TM    AGYSTAT,AGYSICMP    WANT INTERCOMPANY POSTINGS ?                 
         BNO   XIT                                                              
         TM    AGYSTAT,AGYSXJOB    IS IT AN X-JOB?                              
         BO    XIT                 YES, NO INTERCOMPANY POSTINGS                
*                                                                               
ICOMP1   MVC   ICNSTUD,AGYSTUD                                                  
         MVC   ICNJOB,CPJ                                                       
         MVC   ICOMCR,AGYICCR      INTERCOMPANY CREDIT ACCOUNT                  
         OC    ICOMCR,ICOMCR                                                    
         BNZ   *+10                                                             
         MVC   ICOMCR,AGYSTVND                                                  
*                                                                               
         GOTO1 AICMPY,ICMPFSTQ      GET FIRST INTERCOMPANY ITEM                 
         BNE   XIT                                                              
*                                                                               
ICOMP3   ZAP   NET$,AGYAMNT                                                     
         AP    ICMPT$,NET$                                                      
         GOTOR MAIN,ICOMP1Q         MAKE DEBIT TO SJ                            
         GOTO1 AADDREP,STUINP                                                   
         GOTO1 AICMPY,ICMPNXTQ      GET NEXT W/C ITEM                           
         BE    ICOMP3                                                           
*                                                                               
         ZAP   NET$,ICMPT$         SET TOTAL FOR CREDIT POSTING                 
         LARL  R3,ICOMS2                                                        
         USING PCD,R3                                                           
*                                                                               
         LHI   RF,AGYICCR-NBILD                                                 
         OC    AGYICCR,AGYICCR     IS THERE A SPECIFIC CREDIT ?                 
         BZ    *+8                                                              
         STCM  RF,3,PCACCT                                                      
         ICM   RF,3,PCACCT                                                      
         LA    RF,NBILD(RF)                                                     
         MVC   WORK(L'ACTKCULA),0(RF)   SAVE CREDIT ACCOUNT IN WORK             
         LA    R2,WORK                                                          
         USING ACTRECD,R2                                                       
         LA    R5,ICOMTAB          FIX CONTRA, BASED ON ACCOUNT                 
         CLC   ACTKUNT(2),0(R5)                                                 
         BE    ICOMP5                                                           
         LA    R5,L'ICOMTAB(R5)                                                 
         CLI   0(R5),C' '                                                       
         BNE   *-18                                                             
*                                                                               
ICOMP5   MVC   PCCNTR,2(R5)        SET CONTRA ACCOUNT                           
         MVC   PCCNME,4(R5)        AND CONTRA NAME                              
*                                                                               
ICOMP7   ZAP   DUB,ICMPT$                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   ICOMPAMT,DUB                                                     
         GOTOR MAIN,ICOMP2Q     MAKE CREDIT TO VENDOR                           
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
ICOMS1K  NTR1  ,              HOOK FOR INTERCOMPANY DEBIT POSTING               
         L     R7,APTRAN                                                        
         USING TRNELD,R7                                                        
         TM    AGYSTAT,AGYSIHLD     PUT POSTING ON HOLD ?                       
         BNO   XIT                                                              
         OI    TRNSTAT,TRNSHOLD                                                 
         B     XIT                                                              
*                                                                               
ICOMS2K  NTR1  ,              HOOK FOR INTERCOMPANY CREDIT POSTING              
         L     R4,APHEAD                                                        
         USING PSHEADD,R4                                                       
         CLC   PSHDAUL,=C'SR'      FOR SR AND SB                                
         BE    *+14                                                             
         CLC   PSHDAUL,=C'SB'                                                   
         BNE   XIT                                                              
         L     R7,APTRAN                                                        
         OI    TRNSTAT,DR          MAKE IT A DEBIT                              
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'          AND REVERSE SIGN                             
         ZAP   TRNAMNT,DUB                                                      
         B     XIT                                                              
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* MAIN POSTING ROUTINE                                                *         
*  NTRY R1=POSTING TYPE                                               *         
***********************************************************************         
MAIN     NTR1  ,                                                                
         STC   R1,PTYPE            POSTING TYPE                                 
         L     R3,APOSTAB                                                       
         XR    RF,RF                                                            
MAIN3    CLC   PTYPE,0(R3)         MATCH POSTING TYPE                           
         BE    MAIN5                                                            
         IC    RF,1(R3)            BUMP R3 TO NEXT TYPE ENTRY                   
         AR    R3,RF                                                            
         CLI   0(R3),EOT                                                        
         BNE   MAIN3                                                            
         DC    H'0'                UNKNOWN POSTING TYPE                         
*                                                                               
MAIN5    LA    R3,2(R3)            R3=A(POSTING LIST)                           
         LA    R2,CPYC                                                          
         USING CPYELD,R2                                                        
*                                                                               
MAIN7    XR    R5,R5                                                            
         ICM   R5,3,0(R3)                                                       
         AR    R5,RB                                                            
*                                                                               
         USING PCD,R5                                                           
MAIN9    TM    PCSTAT,PCSCOST      IS THIS A COST POSTING ?                     
         BNO   *+12                                                             
         TM    CPYSTAT1,CPYSCOST   ARE THEY ON COSTING ?                        
         BNO   MAIN11              NO, SKIP IT                                  
         MVI   POBPOST,C'N'        SET % OF BILL TO NO                          
         NI    ASPIND,ALL-(ASPIPOS)                                             
         TM    PCSTAT,PCSASP       TEST ASP                                     
         BNO   *+8                                                              
         OI    ASPIND,ASPIPOS      SET ASP POSTING IN PROGRESS                  
*                                                                               
         TM    PCSTAT,PCSPOB       TEST PERCENT OF BILL POSTING                 
         BNO   MAIN10                                                           
         CP    POBCUR,PZERO        TEST ANY AMOUNT                              
         BE    MAIN11              NO, SKIP IT                                  
         MVI   POBPOST,C'Y'        SET % OF BILL FLAG                           
*                                                                               
MAIN10   XR    RF,RF                                                            
         ICM   RF,3,PCCOND         CONDITION TEST                               
         BZ    MAIN13              NONE, GO POST                                
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         BE    MAIN13              PASSED CONDITION TEST - POST                 
*                                                                               
MAIN11   LA    R5,PCLNQ(R5)        TEST NEXT ENTRY                              
         CLI   0(R5),EOT                                                        
         BE    MAIN15                                                           
         B     MAIN9                                                            
*                                                                               
MAIN13   GOTOR POST,DMCB,PCD       BUILD THE POSTING RECORD                     
         L     R7,APTRAN           R2 = A(TRANSACTION AREA)                     
         USING TRNELD,R7                                                        
         TM    PCSTAT,PCNOZRO      TEST NO ZERO POSTING                         
         BNO   *+14                                                             
         CP    TRNAMNT,PZERO       TEST ZERO POSTING                            
         BE    MAIN15              YES, SKIP IT                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,PCHOOK         IS THERE A SPECIAL HOOK ?                    
         BZ    *+8                                                              
         AR    RF,RB                                                            
         BASR  RE,RF               GO TO THE HOOK                               
         CLI   TRNEL,0             TEST HOOK DELETED POSTING                    
         BE    MAIN15              SKIP IT                                      
*                                                                               
         GOTOR AWRKF,WRKADDQ                                                    
*                                                                               
         AP    POSTRECS,PONE                                                    
         LA    RF,POSTDR                                                        
         TM    TRNSTAT,TRNSDR      ADD DEBITS TO CASH TOTAL                     
         BO    *+8                                                              
         LA    RF,POSTCR                                                        
         AP    0(L'POSTDR,RF),TRNAMNT                                           
*                                                                               
MAIN15   LA    R3,2(R3)                                                         
         CLI   0(R3),EOT                                                        
         BNE   MAIN7                                                            
         B     XIT                                                              
         DROP  R2,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* POSTINGS                                                            *         
*  PARM 1 = A(POST CONTROL ENTRY) - SEE PCD                           *         
***********************************************************************         
POST     NTR1  ,                                                                
         L     R5,0(R1)                                                         
         USING PCD,R5                                                           
*                                                                               
         L     RE,APHEAD                                                        
         XC    0(L'PHEAD,RE),0(RE) CLEAR HEADER                                 
         L     RF,APTRAN                                                        
         XC    0(L'PTRAN,RF),0(RF) AND TRANACTION                               
         L     R3,APSHDLM          R3=A(HEADER MASK)                            
         USING ELMD,R3                                                          
*                                                                               
         LA    R4,ELMDATA                                                       
         USING ELMDATA,R4                                                       
         LA    R6,PCACCT           R6 = HEADER ELEMENT DATA                     
         LA    R0,4                ACCT/WORKCODE/CONTRA/CONTRA NAME             
         MVC   ELMDSRC,0(R6)       MOVE SOURCE TO MASK                          
         LA    R4,L'ELMDATA(R4)                                                 
         LA    R6,L'PCACCT(R6)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         GOTOR BELM,DMCB,APHEAD,ELMD,0  BUILD HEADER                            
*                                                                               
         L     R3,ATRNLM           R3 = A(MAIN TRANSACTIONS DETAIL)             
         XR    R6,R6                                                            
         ICM   R6,3,PCTRAN         R6 = A(THIS TRANSACTION DETAIL)              
         AR    R6,RB                                                            
         MVC   ELMELN,ELMELN-ELMD(R6) SET ELEMENT LENGTH                        
         LA    R6,ELMDATA-ELMD(R6)                                              
         CLI   0(R6),EOT           TEST OVERRIDES                               
         BNE   *+6                                                              
         XR    R6,R6                                                            
         GOTOR BELM,DMCB,APTRAN,ELMD,(R6) BUILD TRANSACTION ELEMENT             
*                                                                               
         L     R2,APTRAN                                                        
         USING TRNELD,R2                                                        
         LLC   R1,TRNLN                                                         
         AR    R2,R1                                                            
         XC    0(2,R2),0(R2)       SET R2=A(ELEMENT AREA)                       
         XR    R6,R6                                                            
         ICM   R6,3,PCELEM         R6=LIST OF ELEMENTS                          
         BZ    POST9                                                            
         AR    R6,RB                                                            
*                                                                               
         USING XLD,R6                                                           
POST3    CLI   XLD,EOT             TEST END OF ELEMENT LIST ?                   
         BE    POST9                                                            
         GOTOR XLMS,DMCB,(R2),XLD  ADD THE ELEMENT(S)                           
*                                                                               
POST5    OC    0(2,R2),0(R2)       TEST ANY ELEMENT                             
         BZ    POST7                                                            
         XR    R1,R1                                                            
         ICM   R1,1,1(R2)          R1=LENGTH OF ELEMENT                         
         BNZ   *+6                                                              
         DC    H'0'                BAD ELEMENT                                  
         AR    R2,R1               SET R2 TO END OF ELEMENTS                    
         B     POST5                                                            
*                                                                               
POST7    LA    R6,XLLNQ(R6)                                                     
         B     POST3                                                            
*                                                                               
POST9    TM    SOONS,SOONRUN       TEST SOON RUN                                
         BNO   POST13                                                           
         L     R4,APHEAD                                                        
         USING PSHEADD,R4                                                       
         USING FFTELD,R2                                                        
         XC    FFTEL(FFTDATA-FFTELD),FFTEL                                      
         MVI   FFTEL,FFTELQ        ADD CONTRA NAME FOR UPDATE                   
         MVI   FFTLN,L'PSHDSBNM+(FFTDATA-FFTELD)                                
         MVI   FFTTYPE,FFTTPNAM                                                 
         MVI   FFTDLEN,L'PSHDSBNM                                               
         MVC   FFTDATA(L'PSHDSBNM),PSHDSBNM                                     
         LLC   R1,FFTLN                                                         
         AR    R2,1                                                             
         XC    0(2,R2),0(R2)                                                    
*                                                                               
POST13   L     RF,APREC                                                         
         L     RE,APRLEN                                                        
         SR    R2,RF               GET LENGTH                                   
         AHI   R2,1                                                             
         SLL   R2,16                                                            
         STCM  R2,15,0(RE)                                                      
         B     XIT                                                              
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD THE EXTRA POSTING ELEMENTS                                    *         
*  PARM 1  =  A(START OF ELEMENT AREA)                                *         
*       2  =  A(EXTRA ELEMENT ENTRY - XLD)                            *         
***********************************************************************         
XLMS     NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING XLD,R3                                                           
         SR    RF,RF                                                            
         ICM   RF,3,XLCNDL         ANY CONDITIONALS ?                           
         BZ    XLMS3                                                            
         AR    RF,RB                                                            
         BASR  RE,RF               TEST CONDITION                               
         BNE   XIT                                                              
*                                                                               
XLMS3    XR    R4,R4                                                            
         ICM   R4,3,XLELM          ELEMENT DETAIL MASK                          
         AR    R4,RB                                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,XLHOOK         TEST HOOK ROUTINE                            
         BZ    XLMS5                                                            
         AR    RF,RB                                                            
         GOTO1 (RF),DMCB,(R2),(R4) GO TO HOOK ROUTINE                           
         B     XIT                                                              
*                                                                               
XLMS5    CLI   XLSTA,0             TEST SOFT BUILD                              
         BNE   XLMS7               NO                                           
         GOTOR BELM,DMCB,(R2),(R4),0 BUILD ELEMENT                              
         B     XIT                                                              
*                                                                               
XLMS7    TM    XLSTA,XLSAPE        TEST ANALYSIS POINTER ENTRY                  
         BNO   XLMS8                                                            
         GOTOR BLDAPE,DMCB,(R4)    BUILD APE LIST                               
         GOTOR APELR,DMCB,(R2)     ADD APE LIST                                 
         B     XIT                                                              
*                                                                               
XLMS8    TM    XLSTA,XLSGRP        TEST GROUP ANALYSIS POINTER ENTRY            
         BNO   XLMS9                                                            
         GOTOR APELR,DMCB,(X'80',(R2))  ADD APE LIST                            
         B     XIT                                                              
*                                                                               
XLMS9    XR    R4,R4                                                            
         ICM   R4,3,XLGBL          DISPLACEMENT TO ELEMENT POOL                 
         LA    RF,NBILD            SET FOR GLOBAL                               
         TM    XLSTA,XLSGBL        TEST GLOBAL                                  
         BO    *+6                                                              
         LR    RF,RB               SET LOCAL                                    
         AR    R4,RF                                                            
         ICM   R4,15,0(R4)                                                      
*                                                                               
XLMS11   XC    0(2,R2),0(R2)                                                    
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),EOT                                                        
         BE    XIT                                                              
         XR    R1,R1                                                            
         ICM   R1,1,1(R4)          R1=LENGTH OF ELEMENT                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R4)       MOVE ELEMENT DATA TO POSTING                 
*                                                                               
         LR    RF,R2                                                            
         LA    R4,1(R1,R4)                                                      
         LA    R2,1(R1,R2)                                                      
         XC    0(2,R2),0(R2)                                                    
         CLI   0(RF),LNKELQ        TEST STUDIO LINK ELEMENT                     
         BE    XIT                 YES, CAN HAVE ONLY ONE                       
         B     XLMS11                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD AN ELEMENT                                                    *         
*  PARM 1  =  A(START OF ELEMENT AREA)                                *         
*       2  =  A(ELEMENT DETAIL MASK - ELMD)                           *         
*       3  =  A(ELEMENT OVERIDES)                                     *         
***********************************************************************         
BELM     NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING ELMD,R3                                                          
         LLC   R1,ELMELN                                                        
         AHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       CLEAR ELEMENT AREA +2                        
         MVC   0(1,R2),ELMCODE     CODE                                         
         MVC   1(1,R2),ELMELN      LENGTH                                       
         LA    R5,ELMDATA                                                       
*                                                                               
         USING ELMDATA,R5                                                       
BELM3    CLC   ELMDSP,ELMELN       TEST DATA PASSED END OF ELEMENT              
         BH    BELM19              THEN WE'RE DONE                              
         OC    ELMDSRC,ELMDSRC     TEST ANY SOURCE DATA                         
         BZ    BELM11              LEAVE AS BINARY ZEROS                        
         CLI   ELMDATA,0           IS IT SPECIAL DATA ?                         
         BE    BELM13                                                           
         LLC   R6,ELMDSP           R6=DISPLACEMENT TO OUTPUT AREA               
         AR    R6,R2               + START                                      
         LLC   R1,ELMDLN           R1=LENGTH OF OUTPUT FIELD                    
         BCTR  R1,0                                                             
         CLI   ELMTYP,ELMTIMM      IS IT IMMEDIATE DATA ?                       
         BNE   BELM5                                                            
         MVC   0(1,R6),ELMDFLD     MOVE IMMEDIATE DATA                          
         B     BELM11                                                           
*                                                                               
BELM5    CLI   ELMTYP,ELMTPKD      IS IT PACKED DATA ?                          
         BE    *+12                                                             
         CLI   ELMTYP,ELMTPBD      IS IT PACKED INPUT - BINARY OUT              
         BNE   BELM9                                                            
         LLC   RE,ELMDFLD          FIELD EQUATE                                 
         MHI   RE,DDLNQ                                                         
         A     RE,ADDTAB                                                        
         USING DDD,RE                                                           
         LLC   R1,DDLEN            LENGTH OF DATA                               
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,DDDIS                                                       
         LA    RF,NBILD(RF)                                                     
         DROP  RE                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,0(0,RF)         CASH FIELD TO DUB                            
         CLI   ELMTYP,ELMTPBD      TEST PACKED INPUT - BINARY OUT               
         BE    BELM7                                                            
         IC    R1,ELMDLN           RESET LENGTH OF ELEMENT FIELD                
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R6),DUB         ZAP THE CASH FIELD                           
         B     BELM11                                                           
*                                                                               
BELM7    CVB   RF,DUB                                                           
         STCM  RF,15,0(R6)                                                      
         B     BELM11                                                           
*                                                                               
BELM9    XR    RF,RF                                                            
         ICM   RF,3,ELMDSRC                                                     
         LA    RF,NBILD(RF)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RF)       MOVE DATA TO DESTINATION                     
*                                                                               
BELM11   LA    R5,L'ELMDATA(R5)                                                 
BELM13   CLC   ELMCON,=Y(ELMCONQ)  IS IT A CONDITION TEST ?                     
         BE    BELM15                                                           
         CLC   ELMPOP,=Y(ELMPOPQ)  IS IT A POSTING OPTION ?                     
         BE    BELM14                                                           
         CLC   ELMROU,=Y(ELMROUQ)  IS IT A SPECIAL ROUTINE?                     
         BNE   BELM17                                                           
         SR    RF,RF                                                            
         ICM   RF,3,ELMROUTE                                                    
         AR    RF,RB                                                            
         LA    R5,L'ELMROUL(R5)    BUMP TO NEXT ITEM                            
         BASR  RE,RF               GOTO SPECIAL ROUTINE                         
         B     BELM17                                                           
*                                                                               
BELM14   MVC   BYTE,ELMPTYPE       SAVE POSTING TYPE                            
         LA    R5,L'ELMPOPL(R5)    BUMP TO NEXT ITEM                            
         CLC   BYTE,PTYPE          TEST TYPE                                    
         BE    BELM3               YES, GO PROCESS THE FOLLOWING DATA           
         B     BELM16                                                           
*                                                                               
BELM15   SR    RF,RF               PROCESS CONDITIONAL LIST                     
         ICM   RF,3,ELMCNDL                                                     
         AR    RF,RB                                                            
         LA    R5,L'ELMCONL(R5)    BYPASS THIS SET                              
         BASR  RE,RF               PROCESS CONDITION TEST(S)                    
         BE    BELM3               PASSED - APPLY POSTING RULE                  
*                                                                               
BELM16   CLI   ELMCON,EOT          SKIP TO NEXT COND'L OR POST TYPE             
         BE    BELM19                                                           
         CLC   ELMCON,=Y(ELMCONQ)                                               
         BE    BELM15              NEXT CONDITIONAL                             
         CLC   ELMCON,=Y(ELMPOPQ)                                               
         BE    BELM14              OR POSTING TYPE                              
         LA    R5,L'ELMDATA(R5)                                                 
         B     BELM16                                                           
*                                                                               
BELM17   CLI   ELMCON,EOT                                                       
         BNE   BELM3                                                            
*                                                                               
BELM19   LTR   R4,R4               TEST ANY OVERRIDES                           
         BZ    XIT                                                              
         LR    R5,R4                                                            
         XR    R4,R4                                                            
         B     BELM3               PROCESS THE OVERRIDES                        
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD USER FIELDS TO POSTING TABLE                                    *         
*  PARM 1 = A(OUTPUT AREA)                                            *         
*       2 = A(ELEMENT DETAIL MASK OR ZERO)                            *         
***********************************************************************         
UFSLR    NTR1  ,                                                                
         L     R3,0(R1)                                                         
         XR    R4,R4                                                            
         L     R5,JXAUSRF                                                       
         USING USRFD,R5                                                         
*                                                                               
UFSLR3   CLI   0(R5),EOT                                                        
         BE    XIT                                                              
         LA    R2,USRFLM                                                        
         USING UFSELD,R2                                                        
         TM    UFSSTAT,UFSSPRCV    PASS TO RECEIVABLES                          
         BNO   UFSLR5                                                           
         LLC   R1,UFSLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),UFSELD                                                   
         LA    R3,1(R1,R3)                                                      
         XC    0(2,R3),0(R3)                                                    
         AHI   R4,1                COUNT NUMBER ADDED TO POSTING                
*                                                                               
UFSLR5   LA    R5,USRFLNQ(R5)                                                   
         CHI   R4,6                TO COMPLY WITH 'OLD' MAX IS 6                
         BL    UFSLR3                                                           
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD GST/PST AND SUBSIDIARY TAX ELEMENTS                             *         
*  PARM 1 = A(OUTPUT AREA)                                            *         
*       2 = A(ELEMENT DETAIL MASK OR ZERO)                            *         
***********************************************************************         
SCITGR   DS    0H                                                               
VBILR    MVI   TAXTYP,C'G'         SET FOR GST ELEMENT                          
         B     TAXR                                                             
SCITPR   DS    0H                                                               
PBILR    MVI   TAXTYP,C'P'         SET FOR PST ELEMENT                          
*                                                                               
TAXR     CLI   CTAXOPT,0           ANY CANADIAN TAX ?                           
         BER   RE                                                               
         NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING ELMD,R3                                                          
         L     R4,AGSTBUF                                                       
         TM    CTAXOPT,CTAXOGRP    TEST GROUP BILL                              
         BZ    *+8                 NO,                                          
         L     R4,AGSTGRP          YES, USE GROUP TOTAL                         
         LA    R5,CTAXG                                                         
         CLI   TAXTYP,C'G'                                                      
         BE    *+12                                                             
         L     R4,APSTBUF                                                       
         TM    CTAXOPT,CTAXOGRP    TEST GROUP BILL                              
         BZ    *+8                 NO,                                          
         L     R4,APSTGRP          YES, USE GROUP TOTAL                         
         LA    R5,CTAXP                                                         
*                                                                               
TAXR2    CLI   0(R4),0                                                          
         JE    XIT                                                              
         MVC   0(CTDLLNQ,R5),0(R4)                                              
*                                                                               
         USING CTDLD,R5                                                         
         LA    R6,CTDLTXI                                                       
         USING CTXD,R6                                                          
         LA    R7,CTDLTXB                                                       
         USING CTAXD,R7                                                         
         BAS   RE,SETTAX                                                        
*                                                                               
TAXR3    CLI   ELMCODE,SCIELQ                                                   
         BNE   TAXR4                                                            
         CP    TAXTAX$,PZERO       TEST ANY TAX                                 
         BE    TAXR7               NO, SKIP IT                                  
         B     TAXR5                                                            
*                                                                               
TAXR4    CLI   ELMCODE,VBIELQ                                                   
         BNE   TAXR5                                                            
         CP    TAXBAS$,PZERO                                                    
         BE    TAXR7                                                            
*                                                                               
TAXR5    GOTOR BELM,DMCB,(R2),ELMD,0   BUILD ELEMENT                            
         LLC   R1,1(R2)            BUMP TO NEXT AREA                            
         AR    R2,R1                                                            
         XC    0(2,R2),0(R2)                                                    
*                                                                               
TAXR7    LA    R4,CTDLLNQ(R4)      GET NEXT ENTRY                               
         B     TAXR2                                                            
         DROP  R3,R5,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* SET TAX AMOUNTS                                                     *         
***********************************************************************         
         USING CTXD,R6                                                          
         USING CTAXD,R7                                                         
SETTAX   NTR1  ,                                                                
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         MVC   TXACCT,CTXACCT      TAX ACCOUNT                                  
         MVC   TXRATE,CTXRATE      RATE                                         
         MVC   TXTYPE,CTXTYPE      TYPE                                         
         MVC   TXPROV,CTXPROV      PROVINCE                                     
         MVC   TXEFFD,CTXEFFD      EFFECTIVE DATE                               
*                                                                               
         ZAP   TAXTAX$,CTAXTAX     TAX                                          
         ZAP   TAXCOM$,CTAXCOM     COMMISSION                                   
         ZAP   TAXBAS$,CTAXGRS     BASIS=GROSS                                  
         SP    TAXBAS$,TAXTAX$      LESS   TAX                                  
         ZAP   TAXGRS$,CTAXGRS     SET GROSS                                    
         ICM   RF,15,CRTLR         TEST RETAILER                                
         BZ    XIT                                                              
*                                                                               
         USING RTLD,RF                                                          
         LA    R2,TAXTAX$          ADJUST TAX BUCKETS                           
         LA    R0,4                                                             
*                                                                               
SETTAX7  CP    0(L'TAXTAX$,R2),PZERO                                            
         BE    SETTAX9                                                          
         ZAP   PL16,0(L'TAXTAX$,R2)                                             
         MP    PL16,RTLPCT         * PERCENT                                    
         SRP   PL16,64-6,5         ROUNDED                                      
         ZAP   0(L'TAXTAX$,R2),PL16                                             
*                                                                               
SETTAX9  LA    R2,L'TAXTAX$(R2)                                                 
         BCT   R0,SETTAX7                                                       
         B     XIT                                                              
*                                                                               
         DROP  RF,R7,R6,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ADD SUBSIDIARY POSTING ELEMENT                                      *         
*  PARM 1 = A(OUTPUT AREA)                                            *         
*       2 = A(ELEMENT DETAIL MASK OR ZERO)                            *         
***********************************************************************         
SPDLR    NTR1  ,                                                                
         OC    AGYICCR,AGYICCR     TEST CREDIT ACCOUNT ?                        
         BZ    XIT                 DON'T ADD ELEMENT                            
         L     R2,0(R1)                                                         
         USING SPDELD,R2                                                        
         MVI   SPDEL,SPDELQ                                                     
         LA    RF,AGYICCR+L'AGYICCR-1                                           
         LA    R1,L'AGYICCR        GET LENGTH OF ACCOUNT                        
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPDACCS(0),AGYICCR+1                                             
         AHI   R1,3                                                             
         STC   R1,SPDLN                                                         
         AR    R2,R1                                                            
         XC    0(2,R2),0(R2)                                                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF ACCOUNTS FOR ANALYSIS ELEMENT                        *         
*  PARM 1 = A(ANALYSIS POINTER LIST)                                  *         
***********************************************************************         
BLDAPE   NTR1  ,                                                                
         L     R3,0(R1)                                                         
         USING APED,R3                                                          
         SR    R5,R5                                                            
         LA    R2,ANLPACT                                                       
*                                                                               
BLDAPE3  TM    APESTA,APESCA       TEST COSTING ACCOUNT                         
         BZ    *+12                                                             
         TM    CPYC+(CPYSTAT1-CPYELD),CPYSCOST   TEST COSTING                   
         BZ    BLDAPE5             NO, SKIP IT                                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,APECNDL        TEST ANY CONDITIONAL                         
         BZ    *+12                NO, CONDITIONS                               
         AR    RF,RB                                                            
         BASR  RE,RF               CONDITIONAL ROUTINE                          
         BNE   BLDAPE5                                                          
*                                                                               
         MVI   ANLPSTAT-ANLPACT(R2),0                                           
         TM    APESTA,APESDR       TEST DEBIT                                   
         BZ    *+8                                                              
         OI    ANLPSTAT-ANLPACT(R2),APENSDR                                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,APEACCT                                                     
         LA    RF,NBILD(RF)                                                     
         MVC   ANLPCODE-ANLPACT(L'ANLPCODE,R2),1(RF)                            
         AHI   R5,1                                                             
         LA    R2,L'ANLPACT(R2)                                                 
*                                                                               
BLDAPE5  LA    R3,APELNQ(R3)                                                    
         CLI   APED,EOT                                                         
         BNE   BLDAPE3                                                          
         CLI   PTYPE,BILLPQ        TEST REGULAR BILL POSTINGS                   
         BE    BLDAPE7                                                          
         CLI   PTYPE,CANTPQ        TEST GST/PST POSTINGS                        
         BNE   BLDAPE11                                                         
*                                                                               
BLDAPE7  XR    R0,R0                                                            
         ICM   R0,1,ANLTNUM        ANY TAX ACCOUNTS ?                           
         BZ    BLDAPE11                                                         
         LA    RE,ANLTTAB                                                       
BLDAPE9  MVI   ANLPSTAT-ANLPACT(R2),0                                           
         MVC   ANLPCODE-ANLPACT(L'ANLPCODE,R2),ANLPCODE-ANLPACT(RE)             
         AHI   R5,1                                                             
         LA    R2,L'ANLPACT(R2)                                                 
         LA    RE,L'ANLPACT(RE)                                                 
         BCT   R0,BLDAPE9                                                       
*                                                                               
BLDAPE11 STC   R5,ANLPNUM          SAVE NUMBER OF ITEMS                         
         LA    RF,ANLPMX                                                        
         CR    R5,RF               TEST TABLE OVERFLOW                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF TAX ACCOUNTS FOR ANALYSIS ELEMENT                    *         
***********************************************************************         
BLDTAX   NTR1  ,                                                                
         MVI   ANLTNUM,0                                                        
         CLI   CTAXOPT,0                                                        
         BE    XIT                                                              
         L     R4,AGSTBUF                                                       
         USING CTDLD,R4                                                         
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BAS   RE,BLDTX            ADD TAX ACCOUNTS TO LIST                     
*                                                                               
         TM    CTAXOPT,CTAXOPST    TEST PST                                     
         BZ    XIT                 NO PST                                       
         L     R4,APSTBUF                                                       
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BAS   RE,BLDTX            ADD TAX ACCOUNTS TO LIST                     
         B     XIT                                                              
*                                                                               
BLDTX    DS    0H                                                               
BLDTX3   LA    R7,CTDLTXI                                                       
         USING CTXD,R7                                                          
         LA    R5,ANLTTAB                                                       
         XR    R3,R3                                                            
         ICM   R3,1,ANLTNUM                                                     
         BZ    BLDTX7                                                           
*                                                                               
BLDTX5   CLC   ANLPCODE-ANLPACT(L'ANLPCODE,R5),CTXACCT+1 TEST IN TABLE          
         BE    BLDTX9             YES, DON'T ADD AGAIN                          
         LA    R5,L'ANLTTAB(R5)                                                 
         BCT   R3,BLDTX5                                                        
*                                                                               
BLDTX7   MVC   ANLPCODE-ANLPACT(L'ANLPCODE,R5),CTXACCT+1  SAVE NEW ITEM         
         IC    R3,ANLTNUM                                                       
         LA    R3,1(R3)                                                         
         STC   R3,ANLTNUM                                                       
         LA    RF,ANLTMX                                                        
         CR    R3,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTX9   LA    R4,CTDLLNQ(R4)      GET NEXT GST/PST                             
         CLI   0(R4),0                                                          
         BNE   BLDTX3                                                           
         BR    RE                                                               
*                                                                               
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ANALYSIS POINTER ELEMENT                                        *         
*  PARM 1 = X'80'     USE GROUP ENTRIES                               *         
*         = A(OUTPUT AREA)                                            *         
***********************************************************************         
APELR    NTR1  ,                                                                
         LLC   R0,ANLPNUM          R0=NUMBER OF TABLE ENTRIES                   
         LA    R5,ANLPACT          R5=A(TABLE OF ACCOUNTS)                      
         TM    0(R1),X'80'         TEST WANT GROUP TABLE                        
         BNO   APELR2                                                           
         IC    R0,GRPANLN                                                       
         L     R5,AGRPANL                                                       
*                                                                               
APELR2   LTR   R0,R0               TEST ANYTHING IN TABLE                       
         JZ    XIT                                                              
         L     R2,0(R1)                                                         
         USING APEELD,R2                                                        
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
*                                                                               
APELR3   LLC   RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL SUB ELEMENT)                      
         MVC   APENSTAT,ANLPSTAT-ANLPACT(R5)                                    
         MVC   APENACT,ANLPCODE-ANLPACT(R5)                                     
*                                                                               
APELR5   LA    R3,APENACT+L'APENACT-1                                           
         CLI   0(R3),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(R3),0             AND DROP TRAILING SPACES                     
         BCT   R3,*-12                                                          
         LA    R1,APENACT                                                       
         SR    R3,R1               R3=LENGTH FOR EX                             
         L     RF,APHEAD                                                        
         USING PSHEADD,RF                                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   APENACT(0),PSHDAUNT TEST SAME AS POSTING ACCOUNT                 
         BE    APELR7              YES, DON'T ADD POINTER                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   APENACT(0),PSHDSUNT TEST SAME AS CONTRA ACCOUNT                  
         BE    APELR7              YES, DON'T ADD POINTER                       
         LA    R3,APELN2Q+1(R3)                                                 
         STC   R3,APENLEN                                                       
         DROP  RF                                                               
*                                                                               
         LLC   RE,APELN                                                         
         AR    RE,R3                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         LA    RE,1(RE)                                                         
         STC   RE,APENUM                                                        
*                                                                               
APELR7   LA    R5,L'ANLPACT(R5)    BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,APELR3                                                        
*                                                                               
         LLC   R1,APELN                                                         
         AR    R2,R1                                                            
         XC    0(2,R2),0(R2)                                                    
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
GETELN   AH    R2,NEWDISP                                                       
         J     FIRSTEL                                                          
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* SUB ROUTINES                                                                  
***********************************************************************         
SETAMTS  MVI   AMTSTA,AMTSML       SET AMOUNT STATUS TO SMALL                   
         ZAP   DUB,NET$            POSITIVE NET AMOUNT                          
         OI    DUB+(L'DUB-1),X'0F'                                              
         CP    DUB,P21ML           TEST OVER 21 MILLION                         
         BL    *+8                 NO                                           
         MVI   AMTSTA,AMTLRG       YES,SET STATUS TO LARGE                      
         ZAP   DUB,GRS$            POSITIVE GROSS AMOUNT                        
         OI    DUB+(L'DUB-1),X'0F'                                              
         CP    DUB,P21ML           TEST OVER 21 MILLION                         
         BL    *+8                 NO                                           
         MVI   AMTSTA,AMTLRG       YES,SET STATUS TO LARGE                      
         ZAP   DUB,CINTL$          POSITIVE INTL AMOUNT                         
         OI    DUB+(L'DUB-1),X'0F'                                              
         CP    DUB,P21ML           TEST OVER 21 MILLION                         
         BL    *+8                 NO                                           
         MVI   AMTSTA,AMTLRG       YES,SET STATUS TO LARGE                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONDITIONAL TESTS                                                             
***********************************************************************         
IFLT21ML CLI   AMTSTA,AMTSML       TEST SMALL AMT < 21,000,000.00               
         BNE   XNO                                                              
         B     XYES                                                             
*                                                                               
IFGT21ML CLI   AMTSTA,AMTLRG       TEST LARGE   > 21,000,000.00                 
         BNE   XNO                                                              
         B     XYES                                                             
*                                                                               
IFSTUD   TM    JOBTYP,JOBTSTUD     STUDIO JOB                                   
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFNSTSAL TM    JOBTYP,JOBTSTUD     NOT STUDIO JOB                               
         BO    XNO                                                              
         CLI   SALEAC,0            AND THERE'S A SALES ACCOUNT(USE IT)          
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFSALE   CLI   SALEAC,0            IS THERE A SALES ACCOUNT ?                   
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFCTAX   CLI   CTAXOPT,0           CANADIAN TAX OPTION NE ZERO                  
         BNE   XYES                                                             
         B     XNO                                                              
*                                                                               
IFSKINC  CP    SKCOM$,PZERO        ANY SK INCOME ?                              
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFP2SK   TM    OPTC,OPTCPESK       POST %EST INCOME TO SK                       
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFNOSI   TM    OPTC,OPTCPESK       NO SI INCOME ?                               
         BO    XNO                                                              
         B     XYES                                                             
*                                                                               
*                                                                               
IFCSD    CP    CSD$,PZERO          ANY CASH DISCOUNT ?                          
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFRETL   TM    JXSTAT1,JXS1DIST    RETAIL DISTRIBUTION ?                        
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFHOLD   TM    AGYSTAT,AGYSIHLD     PUT POSTING ON HOLD ?                       
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFPAYLG  LA    RF,=C'SVSXSYSW  '                                                
         CLC   ICOMCR+1(2),0(RF)   IF CREDIT IS PAYABLE LEDGER                  
         BE    XYES                PAYABLE ELEMENT                              
         LA    RF,2(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNE   *-18                                                             
         B     XNO                                                              
*                                                                               
IFICMGT  CLC   ICOMCR+1(2),=C'SR'  IF INTERCO CREDII IS SR                      
         BE    *+14                                                             
         CLC   ICOMCR+1(2),=C'SB'  OR SB                                        
         BNE   XNO                                                              
         CLI   AMTSTA,AMTLRG       AMOUNT IS LARGE                              
         BE    XYES                                                             
         B     XNO                                                              
*                                                                               
IFICMLT  CLC   ICOMCR+1(2),=C'SR'  IF INTERCO CREDII IS SR                      
         BE    *+14                                                             
         CLC   ICOMCR+1(2),=C'SB'  OR SB                                        
         BNE   XNO                                                              
         CLI   AMTSTA,AMTSML        < 21,000,000.00                             
         BE    XYES                                                             
         B     XNO                                                              
*                                                                               
IFRECSB  CLC   RECEIVBL+1(2),=C'SB' RECEIVABLE IS SB                            
         BNE   XNO                                                              
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BO    XNO                                                              
         B     XYES                                                             
*                                                                               
IFNGRP   TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BO    XNO                                                              
         B     XYES                                                             
*                                                                               
IFRECNSB TM    JOBTYP,JOBTSTUD     STUDIO JOB                                   
         BO    XNO                                                              
         CLC   RECEIVBL+1(2),=C'SB' RECEIVABLE NOT SB                           
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFPOB    CLI   POBPOST,C'Y'        TEST PERCENT OF BILL                         
         BE    XYES                                                             
         B     XNO                                                              
*                                                                               
IFGRP    TM    ASPIND,ASPIPOS+ASPINOT SKIP IF ASP POST & NOT BILLED             
         BO    XNO                                                              
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFMEDO   TM    OPTB,OPTBMED        TEST MEDIA ONLY                              
         BO    XYES                                                             
         B     XNO                                                              
*                                                                               
IFT47    CP    T47N$,PZERO         TEST ANY TYPE 47 REVERSAL                    
         BE    XNO                                                              
         B     XYES                                                             
*                                                                               
IFBILL   TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         BO    XNO                 YES, NOT BILLING                             
         B     XYES                                                             
*                                                                               
IFBILLA  TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         BO    XNO                 YES, NOT BILLING                             
         TM    ASPIND,ASPIPOS+ASPIBIL TEST ASP POSTING BEING BILLED             
         BNO   XNO                    NO,                                       
         B     XYES                                                             
*                                                                               
IFASP    TM    JXASPIND,JXASPRTE   TEST BILL HAS ASP                            
         BNO   XNO                 NO,                                          
         B     XYES                                                             
*                                                                               
IFASPP   TM    ASPIND,ASPIPOS      TEST ASP POSTING                             
         BNO   XNO                 NO,                                          
         B     XYES                                                             
*                                                                               
IFASPB   TM    ASPIND,ASPIPOS+ASPIBIL TEST ASP POSTING BEING BILLED             
         BNO   XNO                    NO,                                       
         B     XYES                                                             
*                                                                               
IFASPN   TM    ASPIND,ASPIPOS+ASPINOT TEST ASP POSTING NOT BEING BILLED         
         BNO   XNO                    NO,                                       
         B     XYES                                                             
*                                                                               
IFSOON   TM    SOONS,SOONRUN          TEST IS SOON BILLING                      
         BNO   XNO                    NO,                                       
         B     XYES                                                             
*                                                                               
XYES     CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
XNO      LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINE                                                       *         
***********************************************************************         
ERACNF   MVI   ERRNUM,ERRACNF      ACCOUNT NOT FOUND                            
         J     ERALL                                                            
ERACNB   MVI   ERRNUM,ERRACNB      NO BALANCE ELEMENT                           
         J     ERALL                                                            
ERALL    GOTO1 AADDREP,NONBIL      ADD TO JOB NON-BILLABLE REPORT               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND STORAGE                                               *         
***********************************************************************         
APREC    DC    A(PREC)                                                          
APRLEN   DC    A(PRLEN)                                                         
APHEAD   DC    A(PHEAD)                                                         
APTRAN   DC    A(PTRAN)                                                         
*                                                                               
ANLPMX   EQU   30                  MAX NUMBER OF SUBACCOUNTS                    
         DS    0H                                                               
ANLPNUM  DS    XL1                 NUMBER IN TABLE                              
*                                                                               
         DS    0H                                                               
ANLPACT  DS    0CL15               TABLE OF ACCOUNTS FOR X'C0' ELEMENT          
ANLPSTAT DS    XL1                 STATUS (SEE APENSTAT)                        
ANLPCODE DS    CL14                                                             
         ORG   ANLPACT                                                          
ANLPTAB  DS    (ANLPMX)CL(L'ANLPACT)                                            
*                                                                               
ANLTMX   EQU   10                  CANADIAN TAX ACCOUNTS                        
         DS    0H                                                               
ANLTNUM  DS    XL1                 NUMBER IN TABLE                              
         DS    0H                                                               
ANLTTAB  DS    (ANLTMX)CL(L'ANLPACT)                                            
                                                                                
*                                                                               
ADDTAB   DC    A(DDTAB)                                                         
APSHDLM  DC    A(PSHDLM)                                                        
ATRNLM   DC    A(TRNLM)                                                         
APOSTAB  DC    A(POSTAB)                                                        
*                                                                               
INTLPASS DS    XL1                                                              
POBPOST  DS    CL1                                                              
ASPPOST  DS    CL1                                                              
*                                                                               
TAXTYP   DS    CL1                 TAX TYPE G OR P                              
*                                                                               
AMTSTA   DS    CL1                 AMOUNT STATUS                                
AMTSML   EQU   C'S'                 LESS THAN 21,000,000                        
AMTLRG   EQU   C'L'                 GT THAN 21,000,000                          
*                                                                               
EPRCMT   DC    C'**AUTO REVERSE E.P. '                                          
EPCMT    DC    C'**ESTIMATED PRODUCTION '                                       
*                                                                               
DR       EQU   TRNSDR              DEBIT                                        
CR       EQU   0                   CREDIT                                       
*                                                                               
SIAC     DC    CL12' '                                                          
SKUL     DC    C'SK'                                                            
*                                                                               
PRCDDUL  DC    C'SI'               PRINT CD - DEFAULT UNIT/LEDGER               
PRCDDAC  DC    C'3'                PRINT CD - DEFAULT ACCOUNT                   
*                                                                               
CSHDDUL  DC    C'SI'               CASH DIS - DEFAULT UNIT/LEDGER               
CSHDDAC  DC    C'MD'               CASH DIS - DEFAULT ACCOUNT                   
*                                                                               
ICOMTAB  DS    0XL6                CONTRA TABLE                                 
         DC    C'SB',AL2(JOBK-NBILD,JOBNAM-NBILD)                               
         DC    C'SC',AL2(AGYLNK-NBILD,AGYLNAM-NBILD)                            
         DC    C'SR',AL2(MEDN-NBILD,SPACE-NBILD)                                
         DC    C'  ',AL2(AGYLCLK-NBILD,AGYLCLN-NBILD)                           
         DC    AL1(EOT)                                                         
*                                                                               
TRNLNQ   EQU   TRNLN1Q+L'TRNNARR                                                
*                                                                               
WKOPEN   DC    CL8'OPEN'                                                        
WKADD    DC    CL8'ADD'                                                         
WKCLOSE  DC    CL8'CLOSE'                                                       
WKKEEP   DC    CL8'KEEP'                                                        
*                                                                               
COMMAND  DS    CL8                                                              
*                                                                               
GRPBUK   DS    0D                  GROUP TOTALS                                 
         DS    (NALLBK)PL(L'BUKS)                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN LIST OF ANALYSIS POINTERS FOR GROUP BILLS                            
***********************************************************************         
GRPAPE   NTR1  BASE=*,LABEL=*                                                   
         LARL  RF,ANLPNUM                                                       
         XR    R1,R1                                                            
         ICM   R1,1,0(RF)          R1=# ITEMS IN ACCOUNT TABLE                  
         JZ    XIT                                                              
         LARL  R2,ANLPACT          R2=A(ACCOUNT TABLE)                          
*                                                                               
GRPAPE3  L     R4,AGRPANL          R4=A(GROUP TABLE)                            
         XR    R3,R3                                                            
         ICM   R3,1,GRPANLN        R3=# ITEMS IN GROUP TABLE                    
         BZ    GRPAPE7                                                          
GRPAPE5  CLC   0(L'ANLPACT,R4),0(R2) TEST ALREADY IN TABLE                      
         BE    GRPAPE9               YES, SKIP IT                               
         LA    R4,L'ANLPACT(R4)                                                 
         BCT   R3,GRPAPE5                                                       
*                                                                               
GRPAPE7  MVC   0(L'ANLPACT,R4),0(R2)  ADD TO GROUP TABLE                        
         LLC   R3,GRPANLN                                                       
         LA    R3,1(R3)                                                         
         STC   R3,GRPANLN                                                       
         LA    RF,MXGRPA                                                        
         CR    R3,RF               TEST TABLE OVERFLOW                          
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRPAPE9  LA    R2,L'ANLPACT(R2)    GET NEXT IN ACCOUNT TABLE                    
         BCT   R1,GRPAPE3                                                       
         J     XIT                                                              
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTIONS FOR "SOON" RUN                                     *         
***********************************************************************         
ADDTRN   NTR1  BASE=*,LABEL=*                                                   
         GOTOR AWRKF,WRKINXQ                                                    
*                                                                               
ADDTRN3  GOTOR AWRKF,WRKREDQ                                                    
         LARL  R3,PHEAD                                                         
         USING PSSUBFD,R3                                                       
         CLI   PSSBEL,PSSBELQ      TEST TRAILER                                 
         BE    ADDTRN9                                                          
         CLI   PSSBEL,PSHDELQ      TEST POSTING ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PSHEADD,R3                                                       
         LA    R5,PSHEADD                                                       
         SR    R0,R0                                                            
         IC    R0,PSHDLEN                                                       
         AR    R5,R0               R1=A(NEXT ELEMENT)                           
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        TEST IF A TRANSACTION ELEMENT                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ATRNR            CLEAR TRANSACTION AREA                       
         LA    RF,TIOLQ                                                         
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,ATRNR                                                         
         USING TRNRECD,R2          BUILD TRANSACTION SORT KEY                   
         MVC   TRNKCULA,PSHDACC                                                 
         MVC   TRNKOFF,PSHDANAL                                                 
         OC    TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,PSHDSBAC                                                
         MVC   TRNKDATE,TRNDATE                                                 
         MVC   TRNKREF,TRNREF                                                   
         MVC   TRNKSBR,TRNSUB                                                   
*                                                                               
         XR    R0,R0                                                            
         LA    R1,TRNELD           LOCATE END OF INPUT RECORD                   
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   *-10                                                             
*                                                                               
         LA    R0,TRNRFST                                                       
         AHI   R1,1                                                             
         SR    R1,R5                                                            
         LA    RE,TRNELD                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE ELEMENTS TO TRANSACTION RECORD          
*                                                                               
         L     R2,ATRNR                                                         
         LA    R1,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
ADDTRN5  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST EOR                                     
         BNE   ADDTRN5                                                          
                                                                                
         AHI   R1,1                SET RECORD LENGTH                            
         LA    R0,TRNRECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
ADDTRN7  MVC   DKEY,TRNRECD                                                     
         GOTO1 ADMGR,ACCHIDQ                                                    
         CLC   DIR(L'TRNKEY),DKEY                                               
         BNE   ADDTRN8                                                          
         XR    R0,R0                                                            
         IC    R0,TRNKSBR                                                       
         AHI   R0,1                                                             
         STC   R0,TRNKSBR                                                       
         B     ADDTRN7                                                          
*                                                                               
ADDTRN8  L     R3,ATRNR                                                         
         ST    R3,AIO                                                           
         GOTO1 ADMGR,ACCADDQ                                                    
         B     ADDTRN3                                                          
*                                                                               
ADDTRN9  DS    0H                                                               
         J     XIT                                                              
*                                                                               
WKINDX   DC    CL8'INDEX'                                                       
WKREAD   DC    CL8'READ'                                                        
*                                                                               
AIN      DC    A(IN)                                                            
*                                                                               
ATRNR    DC    A(TRNR)                                                          
         LTORG                                                                  
*                                                                               
         DROP RB                                                                
         EJECT                                                                  
***********************************************************************         
* WORKER FILE                                                         *         
***********************************************************************         
WRKF     NTR1  BASE=*,LABEL=*                                                   
         CLI   RCPOSTNG,NO                                                      
         JE    XIT                                                              
         LR    RF,R1               ACTION NUMBER                                
         BCTR  RF,0                                                             
         MHI   RF,L'CMDTAB                                                      
         LA    RF,CMDTAB(RF)                                                    
         OC    WKFOPT,8(RF)                                                     
         LARL  R0,PREC                                                          
         GOTO1 WORKER,DMCB,(RF),APOSTB,WKID,(R0)                                
         TM    DMCB+8,X'C0'                                                     
         JZ    XIT                                                              
*                                  ERROR ON ADD TO WORKER FILE                  
         MVC   P,SPACES                                                         
         MVC   P(42),=C'*WORKER FILE FULL - STOP ALL FILE MARKERS*'             
         TM    DMCB+8,X'80'                                                     
         BO    *+10                                                             
         MVC   P(42),=C'*WORKER FILE DISK ERROR - CANNOT ADD ID = '             
         L     R2,DMCB+8           ADDRESS OF KEY                               
         MVC   DUB(2),0(R2)                                                     
         LH    R3,DUB                                                           
         CVD   R3,DUB                                                           
         UNPK  DUB+2(6),DUB                                                     
         OI    DUB+7,X'F0'                                                      
         MVC   P+42(3),DUB+5                                                    
         MVC   P+45(23),=C', REPLY = ''OK'' FOR DUMP'                           
WKFLE3   GOTO1 LOGIO,WORK,1,(68,P)                                              
         GOTO1 (RF),(R1),0,(2,HALF)                                             
         CLC   HALF,=C'OK'                                                      
         BNE   WKFLE3                                                           
         DC    H'0'                NOW DIE                                      
*                                                                               
CMDTAB   DS    0XL9                                                             
         DC    CL8'OPEN    ',AL1(0)                                             
         DC    CL8'ADD     ',AL1(0)                                             
         DC    CL8'CLOSE   ',AL1(WKFCLSD)                                       
         DC    CL8'KEEP    ',AL1(WKFKEPT)                                       
         DC    CL8'INDEX   ',AL1(0)                                             
         DC    CL8'READ    ',AL1(0)                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* POSTING RECORD                                                      *         
***********************************************************************         
PREC     DS    0F                  POSTING RECORD                               
PRLEN    DS    XL2                 LENGTH                                       
         DS    XL2                                                              
PHEAD    DS    XL(PSHEADL)         POSTING HEADER                               
PTRAN    DS    XL(TRNLNQ)                  TRANSACTION                          
         DS    XL2000                      ADDITIONAL ELEMENTS                  
*                                                                               
         DC    CL8'**TRNR**'      TRANSACTION RECORD                            
TRNR     DS    XL2000                                                           
TIOLQ    EQU   *-TRNR                                                           
*                                                                               
IN       DCB   DDNAME=IN,DSORG=PS,RECFM=VB,LRECL=1992,BLKSIZE=16000,   X        
               MACRF=PM                                                         
         EJECT                                                                  
***********************************************************************         
* DATA DEFINITIONS - SEE DDD                                          *         
***********************************************************************         
QP       EQU   ELMTPKD             PACKED                                       
QX       EQU   ELMTPBD             CONVERT PACKED TO BINARY                     
QI       EQU   ELMTIMM             IMMEDIATE                                    
*                                                                               
         DS    0D                                                               
DDTAB    DS    0XL(DDLNQ)                                                       
QPZRO    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(PZERO-NBILD),AL1(L'PZERO,DDTPK)                              
QNET$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(NET$-NBILD),AL1(L'NET$,DDTPK)                                
QCOM$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(COM$-NBILD),AL1(L'COM$,DDTPK)                                
QRCV$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(RCV$-NBILD),AL1(L'RCV$,DDTPK)                                
QGRS$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(GRS$-NBILD),AL1(L'GRS$,DDTPK)                                
QGRSZ$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(GRSZ$-NBILD),AL1(L'GRSZ$,DDTPK)                              
QCSD$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(CSD$-NBILD),AL1(L'CSD$,DDTPK)                                
QXCSD$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(XCSD$-NBILD),AL1(L'XCSD$,DDTPK)                              
QPAY$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(PAY$-NBILD),AL1(L'PAY$,DDTPK)                                
QMED$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(MED$-NBILD),AL1(L'MED$,DDTPK)                                
QSHARPCT EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(SHARPCT-NBILD),AL1(L'SHARPCT,DDTPK)                          
QINTL$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(INTL$-NBILD),AL1(L'INTL$,DDTPK)                              
QCINTL$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(CINTL$-NBILD),AL1(L'CINTL$,DDTPK)                            
QSKCOM$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(SKCOM$-NBILD),AL1(L'SKCOM$,DDTPK)                            
QTCOM$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TCOM$-NBILD),AL1(L'TCOM$,DDTPK)                              
QTCMSK$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TCMSK$-NBILD),AL1(L'TCMSK$,DDTPK)                            
QSK2SI$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(SK2SICOM-NBILD),AL1(L'SK2SICOM,DDTPK)                        
QP21ML   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(P21ML-NBILD),AL1(L'P21ML,DDTPK)                              
QTAX$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TAX$-NBILD),AL1(L'TAX$,DDTPK)                                
QTAXTAX$ EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TAXTAX$-NBILD),AL1(L'TAXTAX$,DDTPK)                          
QTAXGRS$ EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TAXGRS$-NBILD),AL1(L'TAXGRS$,DDTPK)                          
QTAXCOM$ EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TAXCOM$-NBILD),AL1(L'TAXCOM$,DDTPK)                          
QTAXBAS$ EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(TAXBAS$-NBILD),AL1(L'TAXBAS$,DDTPK)                          
QGST$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(GST$-NBILD),AL1(L'GST$,DDTPK)                                
QPST$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(PST$-NBILD),AL1(L'PST$,DDTPK)                                
QNLINT$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(NLINT$-NBILD),AL1(L'NLINT$,DDTPK)                            
QPOB$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(POBCUR-NBILD),AL1(L'POBCUR,DDTPK)                            
QICOMP$  EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(ICOMPAMT-NBILD),AL1(L'ICOMPAMT,DDTPK)                        
QT47N$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(T47N$-NBILD),AL1(L'T47N$,DDTPK)                              
QT47C$   EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(T47C$-NBILD),AL1(L'T47C$,DDTPK)                              
QASP$    EQU   ((*-DDTAB)/DDLNQ)                                                
         DC    AL2(ASPAMT-NBILD),AL1(L'ASPAMT,DDTPK)                            
         TITLE 'TABLES TO CONTROL POSTINGS'                                     
POSTAB   DS    0H                                                               
RVSKTAB  DC    AL1(RVSKPQ,RVSKTABX-RVSKTAB)                                     
         DC    AL2(RVSKS1-BGN)     DEBIT  SK                                    
         DC    AL2(RVSKS2-BGN)     CREDIT SI                                    
         DC    AL2(RVSKC1-BGN)     CREDIT 12/1C                                 
         DC    AL2(RVSKC2-BGN)     DEBIT  1C/12                                 
         DC    AL1(EOT)                                                         
RVSKTABX EQU   *                                                                
*                                                                               
RV1RTAB  DC    AL1(RV1RPQ,RV1RTABX-RV1RTAB) TYPE 49                             
         DC    AL2(RV1RS1-BGN)     CREDIT SI  CREDIT COMES FIRST                
         DC    AL2(RV1RS2-BGN)     DEBIT  SK                                    
         DC    AL2(RV1RC1-BGN)     CREDIT 12/1C                                 
         DC    AL2(RV1RC2-BGN)     DEBIT  1C/12                                 
         DC    AL1(EOT)                                                         
RV1RTABX EQU   *                                                                
*                                                                               
RVEPTAB  DC    AL1(RVEPPQ,RVEPTABX-RVEPTAB)                                     
         DC    AL2(RVEPS1-BGN)     DEBIT  SJ                                    
         DC    AL2(RVEPS2-BGN)     CREDIT CONTRA                                
         DC    AL1(EOT)                                                         
RVEPTABX EQU   *                                                                
*                                                                               
BILLTAB  DC    AL1(BILLPQ,BILLTABX-BILLTAB)                                     
         DC    AL2(BILLS1-BGN)     CREDIT SJ                                    
         DC    AL2(BILLS2-BGN)     CREDIT SI                                    
         DC    AL2(BILLS4-BGN)     CREDIT SK                                    
         DC    AL2(BILLS3-BGN)     DEBIT  SR                                    
         DC    AL2(BILLC1-BGN)     CREDIT 12/1C                                 
         DC    AL2(BILLC2-BGN)     CREDIT 11/1C                                 
         DC    AL2(BILLC3-BGN)     DEBIT  1C/12                                 
         DC    AL2(BILLC4-BGN)     DEBIT  1C/11                                 
         DC    AL2(BILLA1-BGN)     DEBIT JOB (ASP)                              
         DC    AL2(BILLA2-BGN)     CREDIT SK (ASP) IF SK ACCOUNT                
         DC    AL2(BILLA3-BGN)     DEBIT  SK (ASP) IF SK ACCOUNT                
         DC    AL2(BILLA4-BGN)     CREDIT SI (ASP)                              
         DC    AL2(BILLA5-BGN)     CREDIT 12/1C (ASP)                           
         DC    AL2(BILLA6-BGN)     DEBIT  1C/12 (ASP)                           
*                                                                               
         DC    AL2(BILLS5-BGN)     DEBIT  SJ (PERCENT OF BILL)                  
         DC    AL2(BILLS6-BGN)     CREDIT SV (PERCENT OF BILL)                  
         DC    AL1(EOT)                                                         
BILLTABX EQU   *                                                                
*                                                                               
CANTTAB  DC    AL1(CANTPQ,CANTTABX-CANTTAB)                                     
         DC    AL2(CANS1-BGN)      CREDIT SG                                    
         DC    AL1(EOT)                                                         
CANTTABX EQU   *                                                                
*                                                                               
INTLTAB  DC    AL1(INTLPQ,INTLTABX-INTLTAB)                                     
         DC    AL2(INTLS1-BGN)     DEBIT  - OPTIONAL ACCOUNT                    
         DC    AL2(INTLS2-BGN)     CREDIT - OPTIONAL ACCOUNT                    
         DC    AL2(INTLC1-BGN)     DEBIT  1C/ CONTRA 12                         
         DC    AL2(INTLC2-BGN)     CREDIT 12/ CONTRA 1C                         
         DC    AL1(EOT)                                                         
INTLTABX EQU   *                                                                
*                                                                               
ICM1TAB  DC    AL1(ICOMP1Q,ICM1TABX-ICM1TAB)                                    
         DC    AL2(ICOMS1-BGN)     DEBIT  SJ BY WORKCODE                        
         DC    AL1(EOT)                                                         
ICM1TABX EQU   *                                                                
*                                                                               
ICM2TAB  DC    AL1(ICOMP2Q,ICM2TABX-ICM2TAB)                                    
         DC    AL2(ICOMS2-BGN)     CREDIT VENDOR - ONE POSTING                  
         DC    AL1(EOT)                                                         
ICM2TABX EQU   *                                                                
*                                                                               
GRPTAB   DC    AL1(GROUPQ,GRPTABX-GRPTAB)                                       
         DC    AL2(GRPS1-BGN)      DEBIT  SR                                    
         DC    AL1(EOT)                                                         
GRPTABX  EQU   *                                                                
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR SK REVERSAL'                                         
***********************************************************************         
* POSTING TABLES  - FOR REVERSE SK POSTINGS                           *         
***********************************************************************         
RVSKS1   DS    0H                  ** SK DEBIT **                               
         DC    AL2(SKRACC-NBILD)   SK ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     JOB CODE                                     
         DC    AL2(JOBNAM-NBILD)   JOB NAME                                     
         DC    AL2(RVSKS1T-BGN)    TRANSACTION                                  
         DC    AL2(RVSKS1E-BGN)    ELEMENTS                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCNOZRO,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE)                                    
         DC    AL2((SKRV-NBILD)+(BTRNDTE-BTRND))                                
         DC    AL1(TRNREF-TRNELD,L'TRNREF)                                      
         DC    AL2((SKRV-NBILD)+(BTRNREF-BTRND))                                
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS1E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RVSKS1AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS1AP DS    0H                                                               
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLRVA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RVSKS2   DS    0H                  ** SI CREDIT **                              
         DC    AL2(ANLINA-NBILD)   SI ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(SALEAC-NBILD)   CONTRA SALES ACCOUNT                         
         DC    AL2(SALEAN-NBILD)                                                
         DC    AL2(RVSKS2T-BGN)    TRANSACTION                                  
         DC    AL2(RVSKS2E-BGN)    ELEMENTS                                     
         DC    AL2(IFNSTSAL-BGN)   TEST NOT STUDIO JOB AND SALES ACC.           
         DC    AL2(RVSKS2K-BGN)    A(HOOK)                                      
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL2(ANLINA-NBILD)   SI ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     CONTRA IS JOB                                
         DC    AL2(JOBNAM-NBILD)                                                
         DC    AL2(RVSKS2T-BGN)    TRANSACTION                                  
         DC    AL2(RVSKS2E-BGN)    ELEMENTS                                     
         DC    AL2(0)              DEFAULT CONTRA IS JOB                        
         DC    AL2(RVSKS2K-BGN)    A(HOOK)                                      
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS2E  DS    0H                                                               
         DC    AL1(0),AL2(SCIGZLM-BGN,0,0)                                      
         DC    AL1(0),AL2(RFLLM-BGN,0,0)                                        
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(XLSAPE),AL2(RVSKS2AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RVSKS2AP DS    0H                                                               
         DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLRVA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RVSKC1   DS    0H                  ** 12 CREDIT **                              
         DC    AL2(ANLRVA-NBILD)   12 - CLIENT REVENUE                          
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(COSTAC-NBILD)   1C - COSTING                                 
         DC    AL2(CLINAM-NBILD)                                                
         DC    AL2(RVSKC1T-BGN)                                                 
         DC    AL2(RVSKC1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC1E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RVSKC1AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC1AP DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RVSKC2   DS    0H                    ** 1C DEBIT **                             
         DC    AL2(COSTAC-NBILD)     1C - CLIENT COSTING                        
         DC    AL2(SPACE-NBILD)      NO W/C                                     
         DC    AL2(ANLRVA-NBILD)     12 - CLIENT REVENUE                        
         DC    AL2(ANLRVN-NBILD)                                                
         DC    AL2(RVSKC2T-BGN)                                                 
         DC    AL2(RVSKC2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC2E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RVSKC2AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RVSKC2AP DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR SK REVERSAL- TYPE 49 -CONTRA 1R'                     
***********************************************************************         
* POSTING TABLES  - FOR REVERSE SK TYPE 49                            *         
***********************************************************************         
RV1RS1   DS    0H                  ** SI CREDIT **                              
         DC    AL2(ANLINA-NBILD)   SI ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(SALEAC-NBILD)   CONTRA SALES ACCOUNT                         
         DC    AL2(SALEAN-NBILD)                                                
         DC    AL2(RV1RS1T-BGN)    TRANSACTION                                  
         DC    AL2(RV1RS1E-BGN)    ELEMENTS                                     
         DC    AL2(IFNSTSAL-BGN)   TEST NOT STUDIO JOB AND SALES ACC.           
         DC    AL2(RV1RS1K-BGN)    A(HOOK)                                      
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL2(ANLINA-NBILD)   SI ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     CONTRA IS JOB                                
         DC    AL2(JOBNAM-NBILD)                                                
         DC    AL2(RV1RS1T-BGN)    TRANSACTION                                  
         DC    AL2(RV1RS1E-BGN)    ELEMENTS                                     
         DC    AL2(0)              DEFAULT CONTRA IS JOB                        
         DC    AL2(RV1RS1K-BGN)    A(HOOK)                                      
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q)                                              
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNREF-TRNELD,L'TRNREF),AL2(BILNUM-NBILD)                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS1E  DS    0H                                                               
         DC    AL1(0),AL2(SCIGZLM-BGN,0,0)                                      
         DC    AL1(0),AL2(RFLLM-BGN,0,0)                                        
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(XLSAPE),AL2(RV1RS1AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS1AP DS    0H                                                               
         DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLRVA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RV1RS2   DS    0H                  ** SK DEBIT **                               
         DC    AL2(SKRACC-NBILD)   SK ACCOUNT                                   
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     JOB CODE                                     
         DC    AL2(JOBNAM-NBILD)   JOB NAME                                     
         DC    AL2(RV1RS2T-BGN)    TRANSACTION                                  
         DC    AL2(RV1RS2E-BGN)    ELEMENTS                                     
         DC    AL2(0)                                                           
         DC    AL2(RV1RS1K-BGN)                                                 
         DC    AL1(PCNOZRO,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q)                                              
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE)                                    
         DC    AL2((SKRV-NBILD)+(BTRNDTE-BTRND))                                
         DC    AL1(TRNREF-TRNELD,L'TRNREF),AL2(BILNUM-NBILD)                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS2E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RV1RS2AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RV1RS2AP DS    0H                                                               
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLRVA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RV1RC1   DS    0H                  ** 12 CREDIT **                              
         DC    AL2(ANLRVA-NBILD)   12 - CLIENT REVENUE                          
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(COSTAC-NBILD)   1C - COSTING                                 
         DC    AL2(CLINAM-NBILD)                                                
         DC    AL2(RV1RC1T-BGN)                                                 
         DC    AL2(RV1RC1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q)                                              
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC1E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RV1RC1AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC1AP DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RV1RC2   DS    0H                    ** 1C DEBIT **                             
         DC    AL2(COSTAC-NBILD)     1C - CLIENT COSTING                        
         DC    AL2(SPACE-NBILD)      NO W/C                                     
         DC    AL2(ANLRVA-NBILD)     12 - CLIENT REVENUE                        
         DC    AL2(ANLRVN-NBILD)                                                
         DC    AL2(RV1RC2T-BGN)                                                 
         DC    AL2(RV1RC2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q)                                              
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL1(QI),C' '                               
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC2E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(RV1RC2AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
RV1RC2AP DC    AL1(APESDR),AL2(SKRACC-NBILD,0)                                  
         DC    AL1(0),AL2(ANLINA-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR EP REVERSAL'                                         
***********************************************************************         
* POSTING TABLES  - FOR REVERSE EP POSTINGS                           *         
***********************************************************************         
RVEPS1   DS    0H                  ** SJ DEBIT **                               
         DC    AL2(JOBK-NBILD)     JOB ACCOUNT                                  
         DC    AL2((EPRV-NBILD)+(BTRNWC-BTRND))                                 
         DC    AL2((EPRV-NBILD)+(BTRNCA-BTRND))   CONTRA                        
         DC    AL2(ACCNAM-NBILD)    CONTRA NAME                                 
         DC    AL2(RVEPS1T-BGN)    TRANSACTION                                  
         DC    AL2(RVEPS1E-BGN)    ELEMENTS                                     
         DC    AL2(0)                                                           
         DC    AL2(RVEPS1K-BGN)    HOOK                                         
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
RVEPS1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE)                                    
         DC    AL2((EPRV-NBILD)+(BTRNDTE-BTRND))                                
         DC    AL1(TRNREF-TRNELD,L'TRNREF)                                      
         DC    AL2((EPRV-NBILD)+(BTRNREF-BTRND))                                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTEPRV)                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNANAL-TRNELD,L'TRNANAL)                                    
         DC    AL2((EPRV-NBILD)+(BTRNWC-BTRND))                                 
         DC    AL2(ELMROUQ),AL2(RVEPNAK-BGN)        NARRATIVE HOOK              
         DC    AL1(EOT)                                                         
*                                                                               
RVEPS1E  DS    0H                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
RVEPS2   DS    0H                  ** CONTRA CREDIT **                          
         DC    AL2(EPCRAC-NBILD)   EP CREDIT ACCOUNT                            
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(PRDK-NBILD)     PRODUCT CODE                                 
         DC    AL2(PRDNAM-NBILD)           NAME                                 
         DC    AL2(RVEPS2T-BGN)    TRANSACTION                                  
         DC    AL2(RVEPS2E-BGN)    ELEMENTS                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
RVEPS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE)                                    
         DC    AL2((EPRV-NBILD)+(BTRNDTE-BTRND))                                
         DC    AL1(TRNREF-TRNELD,L'TRNREF)                                      
         DC    AL2((EPRV-NBILD)+(BTRNREF-BTRND))                                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTEPRV)                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH2-NBILD)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL2(ELMROUQ),AL2(RVEPNAK-BGN)        NARRATIVE HOOK              
         DC    AL1(EOT)                                                         
*                                                                               
RVEPS2E  DS    0F                                                               
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR REGULAR BILL POSTINGS'                               
***********************************************************************         
* POSTING DETAIL TABLES  - FOR BILL POSTINGS                          *         
***********************************************************************         
BILLS1   DS    0H                  ** SJ CREDIT **                              
         DC    AL2(JOBCDE-NBILD)   JOB CODE                                     
         DC    AL2(WCBILLQ-NBILD)  WORKCODE 99                                  
         DC    AL2(CRTLACC-NBILD)  RETAIL ACCOUNT                               
         DC    AL2(CRTLNME-NBILD)  RETAILER  NAME                               
         DC    AL2(BILLS1T-BGN)                                                 
         DC    AL2(BILLS1E-BGN)                                                 
         DC    AL2(IFRETL-BGN)                                                  
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL2(JOBCDE-NBILD)   JOB CODE                                     
         DC    AL2(WCBILLQ-NBILD)  WORKCODE 99                                  
         DC    AL2(RECEIVBL-NBILD) RCV ACCOUNT                                  
         DC    AL2(CLINAM-NBILD)   CLIENT NAME                                  
         DC    AL2(BILLS1T-BGN)                                                 
         DC    AL2(BILLS1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
BILLS1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN2Q)                                              
         DC    AL1(EOT)                                                         
*                                                                               
BILLS1E  DS    0H                                                               
         DC    AL1(0),AL2(VBILM-BGN,0,VBILR-BGN)                                
         DC    AL1(0),AL2(TRSLM99-BGN,0,0)                                      
         DC    AL1(0),AL2(DUELM-BGN,0,0)                                        
         DC    AL1(0),AL2(PTALM-BGN,0,0)                                        
         DC    AL1(0),AL2(PBILM-BGN,0,PBILR-BGN)                                
         DC    AL1(0),AL2(BSCLM-BGN,0,0)                                        
         DC    AL1(0),AL2(PIDLM-BGN,0,0)                                        
         DC    AL1(0),AL2(FFTESTPM-BGN,IFT47-BGN,0)                             
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(0),AL2(CPJRLM-BGN,IFRETL-BGN,0)                              
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(XLSGBL),AL2(ABESEL-NBILD,0,0)                                
         DC    AL1(XLSGBL),AL2(AINCEL-NBILD,0,0)                                
         DC    AL1(XLSAPE),AL2(BILLS1AP-BGN,0,0)                                
         DC    AL1(EOT)                                                         
*                                                                               
BILLS1AP DS    0H                                                               
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,IFRETL-BGN)                       
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLS2   DS    0H                  ** SI CREDIT **                              
         DC    AL2(COMMAC-NBILD)   INCOME ACCOUNT                               
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     JOB CODE                                     
         DC    AL2(JOBNAM-NBILD)        NAME                                    
         DC    AL2(BILLS2T-BGN)                                                 
         DC    AL2(BILLS2E-BGN)                                                 
         DC    AL2(IFSTUD-BGN)     IF STUDIO JOB                                
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL2(COMMAC-NBILD)   INCOME ACCOUNT                               
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(SALEAC-NBILD)   SALES ACCOUNT                                
         DC    AL2(SALEAN-NBILD)         NAME                                   
         DC    AL2(BILLS2T-BGN)                                                 
         DC    AL2(BILLS2E-BGN)                                                 
         DC    AL2(IFSALE-BGN)     IF SALES ACCOUNT                             
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL2(COMMAC-NBILD)   INCOME ACCOUNT                               
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(PRDK-NBILD)     PRODUCT CODE                                 
         DC    AL2(PRDNAM-NBILD)           NAME                                 
         DC    AL2(BILLS2T-BGN)                                                 
         DC    AL2(BILLS2E-BGN)                                                 
         DC    AL2(0)              DEFAULT                                      
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
BILLS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QTCOM$)                     
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLS2E  DS    0H                                                               
         DC    AL1(0),AL2(SPALM-BGN,IFRETL-BGN,0)                               
         DC    AL1(0),AL2(SCIGLM-BGN,0,0)                                       
         DC    AL1(0),AL2(SCITGLM-BGN,0,SCITGR-BGN)                             
         DC    AL1(0),AL2(SCITPLM-BGN,0,SCITPR-BGN)                             
         DC    AL1(XLSGBL),AL2(AINCEL-NBILD,0,0)                                
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(XLSAPE),AL2(BILLS2AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLS2AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,IFRECNSB-BGN)                              
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLS3   DS    0H                  ** SB DEBIT  **                              
         DC    AL2(RECEIVBL-NBILD) RECEIVABLE ACCOUNT                           
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     JOB CODE                                     
         DC    AL2(SPACE-NBILD)                                                 
         DC    AL2(BILLS3T-BGN)                                                 
         DC    AL2(BILLS3E-BGN)                                                 
         DC    AL2(IFRECSB-BGN)    IF RECEIVABLE IS SB                          
         DC    AL2(0)                                                           
         DC    AL1(PCNOZRO,0)                                                   
*                                                                               
*                                  ** SR DEBIT  **                              
         DC    AL2(RECEIVBL-NBILD) RECEIVABLE ACCOUNT                           
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(MEDN-NBILD)     MEDIA NAME IS CONTRA                         
         DC    AL2(SPACE-NBILD)    CONTRA NAME IS SPACES                        
         DC    AL2(BILLS3T-BGN)                                                 
         DC    AL2(BILLS3E-BGN)                                                 
         DC    AL2(IFNGRP-BGN)     IF NOT A GROUP                               
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
BILLS3T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QRCV$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLS3E  DS    0H                                                               
         DC    AL1(0),AL2(OTHLM-BGN,0,0)                                        
         DC    AL1(0),AL2(SPALM-BGN,IFRETL-BGN,0)                               
         DC    AL1(0),AL2(CPJLM-BGN,0,0)                                        
         DC    AL1(0),AL2(SCIDLM-BGN,IFCSD-BGN,0)                               
         DC    AL1(0),AL2(SCIILM-BGN,0,0)                                       
         DC    AL1(0),AL2(SCITGLM-BGN,0,SCITGR-BGN)                             
         DC    AL1(0),AL2(SCITPLM-BGN,0,SCITPR-BGN)                             
         DC    AL1(0),AL2(DUELM-BGN,0,0)                                        
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(0,0,UFSLR-BGN)                                        
         DC    AL1(XLSGBL),AL2(AINCEL-NBILD,0,0)                                
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(XLSAPE),AL2(BILLS3AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLS3AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLS4   DS    0H                  ** SK CREDIT **                              
         DC    AL2(SKACC-NBILD)    SK (INCOME ACCRUAL)                          
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     PRODUCT CODE                                 
         DC    AL2(JOBNAM-NBILD)   PRODUCT JOB NAME                             
         DC    AL2(BILLS4T-BGN)                                                 
         DC    AL2(BILLS4E-BGN)                                                 
         DC    AL2(IFSKINC-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
BILLS4T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QSKCOM$)                    
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLS4E  DS    0H                                                               
         DC    AL1(0),AL2(SPALM-BGN,IFRETL-BGN,0)                               
         DC    AL1(XLSAPE),AL2(BILLS4AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLS4AP DS    0H                                                               
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLC1   DS    0H                  ** 12 CREDIT ** CONTRA 1C                    
         DC    AL2(REVNAC-NBILD)    12X - CLIENT REVENUE                        
         DC    AL2(SPACE-NBILD)     NO W/C                                      
         DC    AL2(COSTAC-NBILD)          COSTING ACCOUNT                       
         DC    AL2(CLINAM-NBILD)          CLIENT NAME                           
         DC    AL2(BILLC1T-BGN)                                                 
         DC    AL2(BILLC1E-BGN)                                                 
         DC    AL2(IFNOSI-BGN)       IF NO SI  TO INCOME                        
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLC1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QTCOM$)                     
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLC1E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(BILLC1AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLC1AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLC2   DS    0H                  ** 11 CREDIT ** CONTRA 1C                    
         DC    AL2(GBILAC-NBILD)      11X - CLIENT BILLING                      
         DC    AL2(SPACE-NBILD)     NO W/C                                      
         DC    AL2(COSTAC-NBILD)      COSTING ACCOUNT                           
         DC    AL2(CLINAM-NBILD)      CLIENT NAME                               
         DC    AL2(BILLC2T-BGN)                                                 
         DC    AL2(BILLC2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLC2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QGRSZ$)                     
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLC2E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(BILLC2AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLC2AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLC3   DS    0H                  ** 1C DEBIT ** CONTRA 12                     
         DC    AL2(COSTAC-NBILD)      1C - COSTING                              
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(REVNAC-NBILD)      12 - CLIENT REVENUE                       
         DC    AL2(MEDNAM-NBILD)           MEDIA NAME                           
         DC    AL2(BILLC3T-BGN)                                                 
         DC    AL2(BILLC3E-BGN)                                                 
         DC    AL2(IFNOSI-BGN)       SKIP IF NO SI COMMISSION                   
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLC3T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QTCOM$)                     
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLC3E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(BILLC3AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLC3AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLC4   DS    0H                  ** 1C DEBIT ** CONTRA 11                     
         DC    AL2(COSTAC-NBILD)      1C - COSTING                              
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(GBILAC-NBILD)      11 - CLIENT BILLING                       
         DC    AL2(MEDNAM-NBILD)           MEDIA NAME                           
         DC    AL2(BILLC4T-BGN)                                                 
         DC    AL2(BILLC4E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLC4T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QGRSZ$)                     
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
BILLC4E  DS    0H                                                               
         DC    AL1(XLSAPE),AL2(BILLC4AP-BGN,0,0)                                
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(0),AL2(FFTLONG-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
BILLC4AP DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLS5   DS    0H                  ** SJ DEBIT ** CONTRA SV                     
         DC    AL2(JOBK-NBILD)        SJ- JOB                                   
         DC    AL2(POBWRK-NBILD)      PERCENT OF BILL W/C                       
         DC    AL2(POBACC-NBILD)      SV - ACCOUNT                              
         DC    AL2(POBACN-NBILD)           NAME                                 
         DC    AL2(BILLS5T-BGN)                                                 
         DC    AL2(BILLS5E-BGN)                                                 
         DC    AL2(IFPOB-BGN)                                                   
         DC    AL2(0)                                                           
         DC    AL1(PCSPOB,0)                                                    
         DC    AL1(EOT)                                                         
*                                                                               
BILLS5T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTPOBL)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QPOB$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(POBWRK-NBILD)                  
         DC    AL1(TRNNARR-TRNELD,1),AL2(BZERO-NBILD)                           
         DC    AL1(EOT)                                                         
*                                                                               
BILLS5E  DS    0H                                                               
         DC    AL1(0),AL2(PTALM-BGN,IFBILL-BGN,0)                               
         DC    AL1(0),AL2(TRSLM-BGN,IFBILL-BGN,0)                               
         DC    AL1(0),AL2(PAKLM-BGN,0,0)                                        
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
BILLS6   DS    0H                  ** SV CREDIT ** CONTRA SJ                    
         DC    AL2(POBACC-NBILD)      SV ACCOUNT                                
         DC    AL2(SPACE-NBILD)                                                 
         DC    AL2(JOBK-NBILD)        SJ - ACCOUNT                              
         DC    AL2(JOBNAM-NBILD)           NAME                                 
         DC    AL2(BILLS6T-BGN)                                                 
         DC    AL2(BILLS6E-BGN)                                                 
         DC    AL2(IFPOB-BGN)                                                   
         DC    AL2(0)                                                           
         DC    AL1(PCSPOB,0)                                                    
         DC    AL1(EOT)                                                         
*                                                                               
BILLS6T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTPOBL)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QPOB$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,1),AL2(BZERO-NBILD)                           
         DC    AL1(EOT)                                                         
*                                                                               
BILLS6E  DS    0H                                                               
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(OTHLM-BGN,0,0)                                        
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  ASP POSTINGS                                 
BILLA1   DS    0H                  ** SJ DEBIT ** CONTRA SI OR SK               
         DC    AL2(JOBK-NBILD)        SJ- JOB                                   
         DC    AL2(ASPWRK-NBILD)      ASP W/C                                   
         DC    AL2(ASPACC-NBILD)      SI ACCOUNT                                
         DC    AL2(ASPACN-NBILD)         NAME                                   
         DC    AL2(BILLA1T-BGN)                                                 
         DC    AL2(BILLA1E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSASP,0)                                                    
         DC    AL1(EOT)                                                         
*                                                                               
BILLA1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(ASPWRK-NBILD)                  
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA1E  DS    0H                                                               
         DC    AL1(0),AL2(PTALM-BGN,IFBILL-BGN,0)                               
         DC    AL1(0),AL2(TRSLM-BGN,IFBILLA-BGN,0)                              
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLA2   DS    0H                  ** SK CREDIT **  IF SK ACCOUNT               
         DC    AL2(ASPACC-NBILD)                                                
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     PRODUCT CODE                                 
         DC    AL2(JOBNAM-NBILD)   PRODUCT JOB NAME                             
         DC    AL2(BILLA2T-BGN)                                                 
         DC    AL2(BILLA2E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSASP+PCSASK,0)                                             
         DC    AL1(EOT)                                                         
*                                                                               
BILLA2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA2E  DS    0H                                                               
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BILLA3   DS    0H                  ** SK DEBIT  **  IF SK ACCOUNT               
         DC    AL2(ASPACC-NBILD)                                                
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     PRODUCT CODE                                 
         DC    AL2(JOBNAM-NBILD)   PRODUCT JOB NAME                             
         DC    AL2(BILLA3T-BGN)                                                 
         DC    AL2(BILLA3E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSASP+PCSASK,0)                                             
         DC    AL1(EOT)                                                         
*                                                                               
BILLA3T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA3E  DS    0H                                                               
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
BILLA4   DS    0H                  ** SI CREDIT **                              
         DC    AL2(ASPACC-NBILD)                                                
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(JOBK-NBILD)     PRODUCT CODE                                 
         DC    AL2(JOBNAM-NBILD)   PRODUCT JOB NAME                             
         DC    AL2(BILLA4T-BGN)                                                 
         DC    AL2(BILLA4E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSASP+PCSASI,0)                                             
         DC    AL1(EOT)                                                         
*                                                                               
BILLA4T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR+TRNSNOCM)                
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA4E  DS    0H                                                               
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(RFLLM-BGN,0,0)                                        
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
BILLA5   DS    0H                  ** 12 CREDIT ** CONTRA 1C                    
         DC    AL2(ASPRVA-NBILD)    12X - CLIENT REVENUE                        
         DC    AL2(SPACE-NBILD)     NO W/C                                      
         DC    AL2(COSTAC-NBILD)          COSTING ACCOUNT                       
         DC    AL2(CLINAM-NBILD)          CLIENT NAME                           
         DC    AL2(BILLA5T-BGN)                                                 
         DC    AL2(BILLA5E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSCOST+PCSASP,0)                                            
         DC    AL1(EOT)                                                         
*                                                                               
BILLA5T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA5E  DS    0H                                                               
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
BILLA6   DS    0H                  ** 1C DEBIT ** CONTRA 12                     
         DC    AL2(COSTAC-NBILD)      1C - COSTING                              
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(ASPRVA-NBILD)      12 - CLIENT REVENUE                       
         DC    AL2(ASPRVN-NBILD)           NAME                                 
         DC    AL2(BILLA6T-BGN)                                                 
         DC    AL2(BILLA6E-BGN)                                                 
         DC    AL2(IFASP-BGN)                                                   
         DC    AL2(ASPHK-BGN)                                                   
         DC    AL1(PCSCOST+PCSASP,0)                                            
         DC    AL1(EOT)                                                         
*                                                                               
BILLA6T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'RETNARR)                                    
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIINV)                   
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QASP$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(TRNNARR-TRNELD,L'RETNARR),AL2(BZERO-NBILD)                   
         DC    AL1(EOT)                                                         
*                                                                               
BILLA6E  DS    0H                                                               
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
         TITLE 'TABLES FOR CANADIAN TAX POSTINGS'                               
***********************************************************************         
* POSTING DETAIL TABLES  - FOR TAX POSTINGS                           *         
***********************************************************************         
CANS1    DS    0H                  ** SG CREDIT **                              
         DC    AL2(TXACCT-NBILD)    TAX ACCOUNT                                 
         DC    AL2(SPACE-NBILD)      NO W/C                                     
         DC    AL2(JOBK-NBILD)       JOB CODE                                   
         DC    AL2(JOBNAM-NBILD)     NAME                                       
         DC    AL2(CANS1T-BGN)                                                  
         DC    AL2(CANS1E-BGN)                                                  
         DC    AL2(IFCTAX-BGN)                                                  
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
CANS1T   DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QTAXTAX$)                   
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
CANS1E   DS    0H                                                               
         DC    AL1(0),AL2(SCITNLM-BGN,0,0)                                      
         DC    AL1(XLSAPE),AL2(CANS1AP-BGN,0,0)                                 
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
CANS1AP  DS    0H                                                               
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(APESDR),AL2(RECEIVBL-NBILD,0)                                
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
         TITLE 'TABLES FOR INTERNAL INCOME POSTINGS'                            
***********************************************************************         
* POSTING DETAIL TABLES  - FOR INTERNAL INCOME -                      *         
***********************************************************************         
INTLS1   DS    0H                  ** SI/SB DEBIT **                            
         DC    AL2(INTDRACC-NBILD)   INCOME DEBIT                               
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(JOBK-NBILD)        PRODUCT CODE                              
         DC    AL2(JOBNAM-NBILD)      PRODUCT JOB NAME                          
         DC    AL2(INTLS1T-BGN)                                                 
         DC    AL2(INTLS1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
INTLS1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QCOM$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
INTLS1E  DS    0H                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
INTLS2   DS    0H                  ** SI CREDIT **                              
         DC    AL2(INTCRACC-NBILD)    INCOME CREDIT                             
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(JOBK-NBILD)        PRODUCT CODE                              
         DC    AL2(JOBNAM-NBILD)      PRODUCT JOB NAME                          
         DC    AL2(INTLS2T-BGN)                                                 
         DC    AL2(INTLS2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
INTLS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QCOM$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
INTLS2E  DS    0H                                                               
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(0),AL2(SCIGZLM-BGN,0,0)                                      
         DC    AL1(0),AL2(RFLLM-BGN,0,0)                                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
INTLC1   DS    0H                  ** 1C DEBIT ** CONTRA 12                     
         DC    AL2(COSTAC-NBILD)      1C - COSTING                              
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(ANLRVA-NBILD)      12 - CLIENT REVENUE                       
         DC    AL2(ANLRVN-NBILD)         - ACCOUNT NAME                         
         DC    AL2(INTLC1T-BGN)                                                 
         DC    AL2(INTLC1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
INTLC1T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QCOM$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
INTLC1E  DS    0F                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
INTLC2   DS    0H                  ** 12 CREDIT ** CONTRA 1C                    
         DC    AL2(ANLRVA-NBILD)      12X - CLIENT REVENUE                      
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(COSTAC-NBILD)            COSTING ACCOUNT                     
         DC    AL2(CLINAM-NBILD)            CLIENT NAME                         
         DC    AL2(INTLC2T-BGN)                                                 
         DC    AL2(INTLC2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PCSCOST,0)                                                   
         DC    AL1(EOT)                                                         
*                                                                               
INTLC2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+L'TRNBLTYP)                                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QCOM$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(OFC-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
INTLC2E  DS    0H                                                               
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR INTERCOMPANY POSTINGS'                               
***********************************************************************         
* POSTING DETAIL TABLES  - FOR INTERCOMPANY                           *         
***********************************************************************         
ICOMS1   DS    0H                  ** SJ DEBIT ** BY WORKCODE                   
         DC    AL2(AGYLNK-NBILD)        JOB ACCOUNT                             
         DC    AL2(AGYWC-NBILD)         WORKCODE                                
         DC    AL2(AGYSTVND-NBILD)      VENDOR                                  
         DC    AL2(AGYSTVNM-NBILD)      NAME                                    
         DC    AL2(ICOMS1T-BGN)                                                 
         DC    AL2(ICOMS1E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(ICOMS1K-BGN)                                                 
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
ICOMS1T  DS    0H                                                               
         DC    AL1(TRNELQ,121)                                                  
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIPOS)                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNANAL-TRNELD,L'TRNANAL),AL2(AGYWC-NBILD)                   
         DC    AL1(TRNNARR-TRNELD,L'ICNARR),AL2(ICNARR-NBILD)                   
         DC    AL1(TRNNARR-TRNELD+L'ICNARR,30),AL2(SPACE-NBILD)                 
         DC    AL1(TRNNARR-TRNELD+L'ICNARR+30,30),AL2(SPACE-NBILD)              
         DC    AL1(TRNNARR-TRNELD+L'ICNARR+60,14),AL2(SPACE-NBILD)              
         DC    AL1(EOT)                                                         
*                                                                               
ICOMS1E  DS    0H                                                               
         DC    AL1(0),AL2(0,0,SPDLR-BGN)                                        
         DC    AL1(0),AL2(PAKLM-BGN,IFPAYLG-BGN,0)                              
         DC    AL1(0),AL2(GDALM-BGN,IFHOLD-BGN,0)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  INTERCOMPANY FOR VENDOR CREDIT               
ICOMS2   DS    0H                  ** SV CREDIT ** TOTAL                        
         DC    AL2(ICOMCR-NBILD)      CREDIT ACCOUNT                            
         DC    AL2(SPACE-NBILD)       NO W/C                                    
         DC    AL2(AGYLCLK-NBILD)       AS IS CONTRA                            
         DC    AL2(AGYLCLN-NBILD)             AND NAME                          
         DC    AL2(ICOMS2T-BGN)                                                 
         DC    AL2(ICOMS2E-BGN)                                                 
         DC    AL2(0)                                                           
         DC    AL2(ICOMS2K-BGN)                                                 
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
ICOMS2T  DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q+1)                                            
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTIPOS)                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(AGYOFC-NBILD)                  
         DC    AL1(TRNNARR-TRNELD,1),AL2(SPACE-NBILD)                           
         DC    AL1(EOT)                                                         
*                                                                               
ICOMS2E  DS    0H                                                               
         DC    AL1(XLSGBL),AL2(ALNKEL-NBILD,IFSTUD-BGN,0)                       
         DC    AL1(0),AL2(MDPLM-BGN,IFICMGT-BGN,0)                              
         DC    AL1(0),AL2(MDTLM-BGN,IFICMLT-BGN,0)                              
         DC    AL1(EOT)                                                         
         TITLE 'TABLES FOR GROUP BILL TOTAL POSTING'                            
***********************************************************************         
* POSTING DETAIL TABLES  - FOR GROUP POSTINGS                         *         
***********************************************************************         
GRPS1    DS    0H                  ** SR DEBIT  **                              
         DC    AL2(RECEIVBL-NBILD) RECEIVABLE ACCOUNT                           
         DC    AL2(SPACE-NBILD)    NO W/C                                       
         DC    AL2(MEDN-NBILD)     MEDIA NAME IS CONTRA                         
         DC    AL2(SPACE-NBILD)    CONTRA NAME IS SPACES                        
         DC    AL2(GRPS1T-BGN)                                                  
         DC    AL2(GRPS1E-BGN)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
GRPS1T   DS    0H                                                               
         DC    AL1(TRNELQ,TRNLN1Q)                                              
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,DR)                         
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QRCV$)                      
         DC    AL1(TRNOFFC-TRNELD,L'TRNOFFC),AL2(BOFC-NBILD)                    
         DC    AL1(EOT)                                                         
*                                                                               
GRPS1E   DS    0H                                                               
         DC    AL1(0),AL2(SCIDLM-BGN,IFCSD-BGN,0)                               
         DC    AL1(0),AL2(SCIILM-BGN,0,0)                                       
         DC    AL1(0),AL2(SCITGLM-BGN,0,SCITGR-BGN)                             
         DC    AL1(0),AL2(SCITPLM-BGN,0,SCITPR-BGN)                             
         DC    AL1(0),AL2(DUELM-BGN,0,0)                                        
         DC    AL1(0),AL2(MDPLM-BGN,IFGT21ML-BGN,0)                             
         DC    AL1(0),AL2(MDTLM-BGN,IFLT21ML-BGN,0)                             
         DC    AL1(XLSGRP),AL2(0,0,0)                                           
         DC    AL1(0),AL2(FFTWKLM-BGN,0,0)                                      
         DC    AL1(EOT)                                                         
*                                                                               
GRPS3AP  DS    0H                                                               
         DC    AL1(0),AL2(JOBK-NBILD,0)                                         
         DC    AL1(0),AL2(COMMAC-NBILD,0)                                       
         DC    AL1(0),AL2(SKACC-NBILD,IFSKINC-BGN)                              
         DC    AL1(APESDR),AL2(COSTAC-NBILD,0)                                  
         DC    AL1(0),AL2(REVNAC-NBILD,0)                                       
         DC    AL1(0),AL2(GBILAC-NBILD,0)                                       
         DC    AL1(EOT)                                                         
         TITLE 'ELEMENT TABLES'                                                 
***********************************************************************         
* ELEMENT DETAIL TABLES - HEADER AND TRANSACTIONS - SEE ELMD          *         
***********************************************************************         
PSHDLM   DS    0H                  POSTING HEADER                               
         DC    AL1(PSHDELQ,PSHEADL)                                             
         DC    AL1(PSHDACC-PSHEADD,L'PSHDACC),AL2(0)   ACCOUNT                  
         DC    AL1(PSHDANAL-PSHEADD,L'PSHDANAL),AL2(0) WRKC                     
         DC    AL1(PSHDSBAC-PSHEADD,L'PSHDSBAC),AL2(0) CONTRA                   
         DC    AL1(PSHDSBNM-PSHEADD,L'PSHDSBNM),AL2(0) CONTRA-NAME              
         DC    AL1(EOT)                                                         
*                                                                               
TRNLM    DS    0H                  TRANSACTION                                  
         DC    AL1(TRNELQ,TRNLN2Q)                                              
         DC    AL1(TRNDATE-TRNELD,L'TRNDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(TRNREF-TRNELD,L'TRNREF),AL2(BILNUM-NBILD)                    
         DC    AL1(TRNTYPE-TRNELD,L'TRNTYPE),AL1(QI,TRNTBILL)                   
         DC    AL1(TRNSTAT-TRNELD,L'TRNSTAT),AL1(QI,CR)                         
         DC    AL1(TRNBTCH-TRNELD,L'TRNBTCH),AL2(BTCH-NBILD)                    
         DC    AL1(TRNAMNT-TRNELD,L'TRNAMNT),AL1(QP,QNET$)                      
         DC    AL1(TRNANAL-TRNELD,L'TRNANAL),AL2(WCBILLQ-NBILD)                 
         DC    AL1(TRNBLTYP-TRNELD,L'TRNBLTYP),AL2(BILTNAM-NBILD)               
         DC    AL1(TRNBLCOM-TRNELD,L'TRNBLCOM),AL1(QP,QCOM$)                    
         DC    AL1(TRNBLCD-TRNELD,L'TRNBLCD),AL1(QP,QCSD$)                      
         DC    AL1(TRNBLPAY-TRNELD,L'TRNBLPAY),AL1(QP,QPAY$)                    
         DC    AL1(TRN2DAY-TRNELD,L'TRN2DAY),AL2(TODAY2-NBILD)                  
         DC    AL1(TRNSK2SI-TRNELD,L'TRNSK2SI),AL1(QP,QPZRO)                    
         DC    AL1(TRNMDCST-TRNELD,L'TRNMDCST),AL1(QP,QMED$)                    
         DC    AL1(TRNAM2SK-TRNELD,L'TRNAM2SK),AL1(QP,QPZRO)                    
         DC    AL1(TRNQTFLT-TRNELD,L'TRNQTFLT),AL2(TRNSFLT-NBILD)               
         DC    AL1(TRNRTPCT-TRNELD,L'TRNRTPCT),AL1(QP,QSHARPCT)                 
         DC    AL1(TRNBTYPE-TRNELD,L'TRNBTYPE),AL2(BILTNUM-NBILD)               
         DC    AL1(TRNBINTI-TRNELD,L'TRNBINTI),AL1(QP,QCINTL$)                  
         DC    AL1(TRNBLVT1-TRNELD,L'TRNBLVT1),AL1(QP,QPZRO)                    
         DC    AL1(TRNBLVT2-TRNELD,L'TRNBLVT2),AL1(QP,QPZRO)                    
         DC    AL1(TRNBLVT3-TRNELD,L'TRNBLVT3),AL1(QP,QPZRO)                    
         DC    AL1(TRNBLVT4-TRNELD,L'TRNBLVT4),AL1(QP,QPZRO)                    
         DC    AL1(TRNBLVT5-TRNELD,L'TRNBLVT5),AL1(QP,QPZRO)                    
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFP2SK-BGN)                                     
         DC    AL1(TRNSK2SI-TRNELD,L'TRNSK2SI),AL2(BZERO-NBILD)                 
         DC    AL1(TRNAM2SK-TRNELD,L'TRNAM2SK),AL1(QP,QSKCOM$)                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ELEMENT DETAIL TABLES - OPTIONAL ELEMENTS - SEE ELMD                *         
***********************************************************************         
BSCLM    DS    0H                  BILLING SOURCE                               
         DC    AL1(BSCELQ,BSCLNQ)                                               
         DC    AL1(BSCBSRC-BSCELD,L'BSCBSRC),AL2(MEDNS-NBILD)                   
         DC    AL1(BSCBOFF-BSCELD,L'BSCBOFF),AL2(BOFC-NBILD)                    
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFGRP-BGN)      GROUP BILL                      
         DC    AL1(BSCBSRC-BSCELD,L'BSCBSRC),AL2(PRODCTN-NBILD)                 
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFMEDO-BGN)     MEDIA ONLY                      
         DC    AL1(BSCBSRC-BSCELD,L'BSCBSRC),AL2(PRNTMED-NBILD)                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CPJLM    DS    0H                  CLIENT/PRODUCT/JOB                           
         DC    AL1(CPJELQ,CPJLNQ)                                               
         DC    AL1(CPJTYPE-CPJELD,L'CPJTYPE),AL1(QI,CPJTJOB)                    
         DC    AL1(CPJCLI-CPJELD,L'CPJCLI),AL2(CLI-NBILD)                       
         DC    AL1(CPJPRO-CPJELD,L'CPJPRO),AL2(PRD-NBILD)                       
         DC    AL1(CPJJOB-CPJELD,L'CPJJOB),AL2(JOB-NBILD)                       
         DC    AL1(CPJWRK-CPJELD,L'CPJWRK),AL2(SPACE-NBILD)                     
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CPJRLM   DS    0H                  CPJ/RECEIVABLE                               
         DC    AL1(CPJELQ,CPJLNQ)                                               
         DC    AL1(CPJTYPE-CPJELD,L'CPJTYPE),AL1(QI,CPJTREC)                    
         DC    AL1(CPJRULA-CPJELD,L'CPJRULA),AL2(RECEIVBL-NBILD+1)              
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
DUELM    DS    0H                  DUE DATE                                     
         DC    AL1(DUEELQ,DUELNQ)                                               
         DC    AL1(DUEDATE-DUEELD,L'DUEDATE),AL2(DUEDT2-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
FFTWKLM  DS    0H                  FREE FORM TEXT(WORKCODE/AMOUNT)              
         DC    AL1(FFTELQ,((FFTDATA-FFTELD)+L'FFTWORK+L'FFTWAMT))               
         DC    AL1(FFTTYPE-FFTELD,L'FFTTYPE),AL1(QI,FFTTWRKC)                   
         DC    AL1(FFTSEQ-FFTELD,L'FFTSEQ),AL2(0)                               
         DC    AL1(FFTDLEN-FFTELD,L'FFTDLEN)                                    
         DC    AL1(QI,L'FFTWORK+L'FFTWAMT)                                      
         DC    AL1(FFTWORK-FFTELD,L'FFTWORK),AL2(WCBILLQ-NBILD)                 
         DC    AL1(FFTWAMT-FFTELD,L'FFTWAMT),AL1(QP,QNET$)                      
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)    PERCENT OF BILL                   
         DC    AL1(FFTWORK-FFTELD,L'FFTWORK),AL2(POBWRK-NBILD)                  
         DC    AL1(FFTWAMT-FFTELD,L'FFTWAMT),AL1(QP,QPOB$)                      
         DC    AL1(EOT)                                                         
*                                                                               
FFTESTPM DS    0H                  FREE FORM TEXT(ESTIMATE/PRODUCTION)          
         DC    AL1(FFTELQ,((FFTDATA-FFTELD)+L'FFTESTN+L'FFTESTC))               
         DC    AL1(FFTTYPE-FFTELD,L'FFTTYPE),AL1(QI,FFTTESTP)                   
         DC    AL1(FFTSEQ-FFTELD,L'FFTSEQ),AL2(0)                               
         DC    AL1(FFTDLEN-FFTELD,L'FFTDLEN)                                    
         DC    AL1(QI,L'FFTESTN+L'FFTESTC)                                      
         DC    AL1(FFTESTN-FFTELD,L'FFTESTN),AL1(QP,QT47N$)                     
         DC    AL1(FFTESTC-FFTELD,L'FFTESTC),AL1(QP,QT47C$)                     
         DC    AL1(EOT)                                                         
*                                                                               
FFTLONG  DS    0H                  FREE FORM TEXT(LONG INVOICE NUMBER)          
         DC    AL1(FFTELQ,((FFTDATA-FFTELD)+9))                                 
         DC    AL1(FFTTYPE-FFTELD,L'FFTTYPE),AL1(QI,FFTTINVN)                   
         DC    AL1(FFTSEQ-FFTELD,L'FFTSEQ),AL2(0)                               
         DC    AL1(FFTDLEN-FFTELD,L'FFTDLEN)                                    
         DC    AL1(QI,9)                                                        
         DC    AL1(FFTDATA-FFTELD,1),AL2(MEDIA-NBILD)                           
         DC    AL1((FFTDATA+1)-FFTELD,1),AL1(QI,C'-')                           
         DC    AL1((FFTDATA+2)-FFTELD,2),AL2(BILNUM-NBILD)                      
         DC    AL1((FFTDATA+4)-FFTELD,1),AL1(QI,C'-')                           
         DC    AL1((FFTDATA+5)-FFTELD,4),AL2((BILNUM+2)-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
GDALM    DS    0H                  GENERAL DATE ELEMENT                         
         DC    AL1(GDAELQ,GDALNQ)                                               
         DC    AL1(GDATYPE-GDAELD,L'GDATYPE),AL1(QI,GDATBHLD)                   
         DC    AL1(GDADATE-GDAELD,L'GDADATE),AL2(TODAY1-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
MDPLM    DS    0H                  MEDIA TRANSFER(PACKED)                       
         DC    AL1(MDPELQ,MDPLNQ)                                               
         DC    AL1(MDPSYS-MDPELD,L'MDPSYS),AL1(QI,MDPSPROD)                     
         DC    AL1(MDPMED-MDPELD,L'MDPMED),AL2(MEDIA-NBILD)                     
         DC    AL1(MDPCLI-MDPELD,L'MDPCLI),AL2(CLI-NBILD)                       
         DC    AL1(MDPPRD-MDPELD,L'MDPPRD),AL2(PRD-NBILD)                       
         DC    AL1(MDPJOB-MDPELD,L'MDPJOB),AL2(JOB-NBILD)                       
         DC    AL1(MDPMOS-MDPELD,L'MDPMOS),AL2(BILMOSP-NBILD)                   
         DC    AL1(MDPDSCP-MDPELD,L'MDPDSCP),AL2(JOBNAM-NBILD)                  
         DC    AL1(MDPFDTE-MDPELD,L'MDPFDTE),AL2(TTLSDAY2-NBILD)                
         DC    AL1(MDPTDTE-MDPELD,L'MDPTDTE),AL2(TTLEDAY2-NBILD)                
*                                                                               
*                                     DEFAULT IS ZEROS                          
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QPZRO)                        
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QPZRO)                        
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QPZRO)                        
         DC    AL1(MDPCD-MDPELD,L'MDPCD),AL1(QP,QPZRO)                          
         DC    AL1(MDPINTL-MDPELD,L'MDPINTL),AL1(QP,QPZRO)                      
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QP,QPZRO)                      
         DC    AL1(MDPVAT-MDPELD,L'MDPVAT),AL1(QP,QPZRO)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(BILLPQ,0)    BILL                               
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QGRS$)                        
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QNLINT$)                      
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QTCMSK$)                      
         DC    AL1(MDPCD-MDPELD,L'MDPCD),AL1(QP,QXCSD$)                         
         DC    AL1(MDPINTL-MDPELD,L'MDPINTL),AL1(QP,QCINTL$)                    
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QP,QRCV$)                      
         DC    AL1(MDPVAT-MDPELD,L'MDPVAT),AL1(QP,QGST$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(GROUPQ,0)    GROUP BILL                         
         DC    AL1(MDPMED-MDPELD,L'MDPMED),AL1(QI),C' '                         
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QGRS$)                        
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QNLINT$)                      
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QTCMSK$)                      
         DC    AL1(MDPCD-MDPELD,L'MDPCD),AL1(QP,QXCSD$)                         
         DC    AL1(MDPINTL-MDPELD,L'MDPINTL),AL1(QP,QCINTL$)                    
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QP,QRCV$)                      
         DC    AL1(MDPVAT-MDPELD,L'MDPVAT),AL1(QP,QGST$)                        
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)    PERCENT OF BILL                   
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QPOB$)                        
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QPOB$)                        
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QPZRO)                        
         DC    AL1(MDPCD-MDPELD,L'MDPCD),AL1(QP,QPZRO)                          
         DC    AL1(MDPINTL-MDPELD,L'MDPINTL),AL1(QP,QPOB$)                      
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QX,QPZRO)                      
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFASPP-BGN)    ASP                              
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QASP$)                        
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QASP$)                        
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QPZRO)                        
         DC    AL1(MDPCD-MDPELD,L'MDPCD),AL1(QP,QPZRO)                          
         DC    AL1(MDPINTL-MDPELD,L'MDPINTL),AL1(QP,QASP$)                      
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QX,QPZRO)                      
*                                                                               
         DC    AL2(ELMPOPQ),AL1(RVSKPQ,0)    REVERSE SK                         
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QNET$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(RV1RPQ,0)    REVERSE 1R                         
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QNET$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(INTLPQ,0)    INTERNAL INCOME                    
         DC    AL1(MDPCOM-MDPELD,L'MDPCOM),AL1(QP,QCOM$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(ICOMP2Q,0)   INTER-COMPANY                      
         DC    AL1(MDPGRS-MDPELD,L'MDPGRS),AL1(QP,QICOMP$)                      
         DC    AL1(MDPNET-MDPELD,L'MDPNET),AL1(QP,QICOMP$)                      
         DC    AL1(MDPRECV-MDPELD,L'MDPRECV),AL1(QP,QICOMP$)                    
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFSTUD-BGN)    STUDIO JOB                       
         DC    AL1(MDPMED-MDPELD,L'MDPMED),AL2(AGYLJOB-NBILD)                   
         DC    AL1(MDPCLI-MDPELD,L'MDPCLI),AL2(AGYLCLI-NBILD)                   
         DC    AL1(MDPPRD-MDPELD,L'MDPPRD),AL2(AGYLPRO-NBILD)                   
         DC    AL1(MDPJOB-MDPELD,L'MDPJOB),AL2(AGYLJOB-NBILD)                   
         DC    AL1(MDPDSCP-MDPELD,L'MDPDSCP),AL2(AGYLNAM-NBILD)                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
MDTLM    DS    0H                  MEDIA TRANSFER(BINARY)                       
         DC    AL1(MDTELQ,MDTLNQ)                                               
         DC    AL1(MDTSYS-MDTELD,L'MDTSYS),AL1(QI,MDTSPROD)                     
         DC    AL1(MDTMED-MDTELD,L'MDTMED),AL2(MEDIA-NBILD)                     
         DC    AL1(MDTCLI-MDTELD,L'MDTCLI),AL2(CLI-NBILD)                       
         DC    AL1(MDTPRD-MDTELD,L'MDTPRD),AL2(PRD-NBILD)                       
         DC    AL1(MDTJOB-MDTELD,L'MDTJOB),AL2(JOB-NBILD)                       
         DC    AL1(MDTMOS-MDTELD,L'MDTMOS),AL2(BILMOSP-NBILD)                   
         DC    AL1(MDTDSCP-MDTELD,L'MDTDSCP),AL2(JOBNAM-NBILD)                  
         DC    AL1(MDTFDTE-MDTELD,L'MDTFDTE),AL2(TTLSDAY2-NBILD)                
         DC    AL1(MDTTDTE-MDTELD,L'MDTTDTE),AL2(TTLEDAY2-NBILD)                
*                                                                               
         DC    AL2(ELMPOPQ),AL1(BILLPQ,0)    BILL                               
         DC    AL1(MDTGRS-MDTELD,L'MDTGRS),AL1(QX,QGRS$)                        
         DC    AL1(MDTNET-MDTELD,L'MDTNET),AL1(QX,QNLINT$)                      
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QTCMSK$)                      
         DC    AL1(MDTCD-MDTELD,L'MDTCD),AL1(QX,QXCSD$)                         
         DC    AL1(MDTINTL-MDTELD,L'MDTINTL),AL1(QX,QCINTL$)                    
         DC    AL1(MDTRECV-MDTELD,L'MDTRECV),AL1(QX,QRCV$)                      
         DC    AL1(MDTVAT-MDTELD,L'MDTVAT),AL1(QX,QGST$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(GROUPQ,0)    GROUP BILL                         
         DC    AL1(MDTMED-MDTELD,L'MDTMED),AL1(QI),C' '                         
         DC    AL1(MDTGRS-MDTELD,L'MDTGRS),AL1(QX,QGRS$)                        
         DC    AL1(MDTNET-MDTELD,L'MDTNET),AL1(QX,QNLINT$)                      
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QTCMSK$)                      
         DC    AL1(MDTCD-MDTELD,L'MDTCD),AL1(QX,QXCSD$)                         
         DC    AL1(MDTINTL-MDTELD,L'MDTINTL),AL1(QX,QCINTL$)                    
         DC    AL1(MDTRECV-MDTELD,L'MDTRECV),AL1(QX,QRCV$)                      
         DC    AL1(MDTVAT-MDTELD,L'MDTVAT),AL1(QX,QGST$)                        
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)    PERCENT OF BILL                   
         DC    AL1(MDTGRS-MDTELD,L'MDTGRS),AL1(QX,QPOB$)                        
         DC    AL1(MDTNET-MDTELD,L'MDTNET),AL1(QX,QPOB$)                        
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QPZRO)                        
         DC    AL1(MDTCD-MDTELD,L'MDTCD),AL1(QX,QPZRO)                          
         DC    AL1(MDTINTL-MDTELD,L'MDTINTL),AL1(QX,QPOB$)                      
         DC    AL1(MDTRECV-MDTELD,L'MDTRECV),AL1(QX,QPZRO)                      
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFASPP-BGN)    ASP                              
         DC    AL1(MDTGRS-MDTELD,L'MDTGRS),AL1(QX,QASP$)                        
         DC    AL1(MDTNET-MDTELD,L'MDTNET),AL1(QX,QASP$)                        
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QPZRO)                        
         DC    AL1(MDTCD-MDTELD,L'MDTCD),AL1(QX,QPZRO)                          
         DC    AL1(MDTINTL-MDTELD,L'MDTINTL),AL1(QX,QASP$)                      
         DC    AL1(MDTRECV-MDTELD,L'MDTRECV),AL1(QX,QPZRO)                      
*                                                                               
         DC    AL2(ELMPOPQ),AL1(RVSKPQ,0)    REVERSE SK                         
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QNET$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(RV1RPQ,0)    REVERSE 1R                         
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QNET$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(INTLPQ,0)    INTERNAL INCOME                    
         DC    AL1(MDTCOM-MDTELD,L'MDTCOM),AL1(QX,QCOM$)                        
*                                                                               
         DC    AL2(ELMPOPQ),AL1(ICOMP2Q,0)   INTER-COMPANY                      
         DC    AL1(MDTGRS-MDTELD,L'MDTGRS),AL1(QX,QICOMP$)                      
         DC    AL1(MDTNET-MDTELD,L'MDTNET),AL1(QX,QICOMP$)                      
         DC    AL1(MDTRECV-MDTELD,L'MDTRECV),AL1(QX,QICOMP$)                    
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFSTUD-BGN)     STUDIO JOB                      
         DC    AL1(MDPMED-MDPELD,L'MDPMED),AL2(AGYLJOB-NBILD)                   
         DC    AL1(MDPCLI-MDPELD,L'MDPCLI),AL2(AGYLCLI-NBILD)                   
         DC    AL1(MDPPRD-MDPELD,L'MDPPRD),AL2(AGYLPRO-NBILD)                   
         DC    AL1(MDPJOB-MDPELD,L'MDPJOB),AL2(AGYLJOB-NBILD)                   
         DC    AL1(MDPDSCP-MDPELD,L'MDPDSCP),AL2(AGYLNAM-NBILD)                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
OTHLM    DS    0H                  OTHER                                        
         DC    AL1(OTHELQ,OTHLN1Q)                                              
         DC    AL1(OTHNUM-OTHELD,L'OTHNUM),AL2(CPJ-NBILD+3)                     
         DC    AL1(OTHPROF-OTHELD,L'OTHPROF),AL2(SPACE-NBILD)                   
         DC    AL1(OTHPROF-OTHELD,1),AL1(QI),C'J'                               
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)  PERCENT OF BILL                     
         DC    AL1(OTHNUM-OTHELD+3,3),AL2(SPACE-NBILD)                          
         DC    AL1(OTHNUM-OTHELD+6,6),AL2(CPJ-NBILD+6)                          
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
PBILM    DS    0H                  PST BILLED                                   
         DC    AL1(PBIELQ,PBILNQ)                                               
         DC    AL1(PBITYPE-PBIELD,L'PBITYPE),AL2(TXTYPE-NBILD)                  
         DC    AL1(PBIRATE-PBIELD,L'PBIRATE),AL2(TXRATE-NBILD)                  
         DC    AL1(PBIPRV-PBIELD,L'PBIPRV),AL2(TXPROV-NBILD)                    
         DC    AL1(PBIPST-PBIELD,L'PBIPST),AL1(QP,QTAXTAX$)                     
         DC    AL1(PBIGROSS-PBIELD,L'PBIGROSS),AL1(QP,QTAXGRS$)                 
         DC    AL1(PBICOMM-PBIELD,L'PBIGROSS),AL1(QP,QTAXCOM$)                  
         DC    AL1(PBIACCT-PBIELD,L'PBIACCT),AL2(TXACCT-NBILD)                  
         DC    AL1(PBIDATE-PBIELD,L'VBIDATE),AL2(TXEFFD-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
PAKLM    DS    0H                  PAYABLE                                      
         DC    AL1(PAKELQ,PAKLNQ)                                               
         DC    AL1(PAKACC-PAKELD,L'PAKACC),AL2(ICOMCR-NBILD)                    
         DC    AL1(PAKOFF-PAKELD,L'PAKOFF),AL2(AGYOFC-NBILD)                    
         DC    AL1(PAKCON-PAKELD,L'PAKCON),AL2(AGYLCLK-NBILD)                   
         DC    AL1(PAKDATE-PAKELD,L'PAKDATE),AL2(BILDT1-NBILD)                  
         DC    AL1(PAKREF-PAKELD,L'PAKREF),AL2(BILNUM-NBILD)                    
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)    PERCENT OF BILL                   
         DC    AL1(PAKACC-PAKELD,L'PAKACC),AL2(POBACC-NBILD)                    
         DC    AL1(PAKOFF-PAKELD,L'PAKOFF),AL2(OFC-NBILD)                       
         DC    AL1(PAKCON-PAKELD,L'PAKCON),AL2(JOBCDE-NBILD)                    
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
PTALM    DS    0H                  PRODUCTION TRANSACTION ACTIVIY               
         DC    AL1(PTAELQ,PTARLN1Q)                                             
         DC    AL1(PTADATE-PTAELD,L'PTADATE),AL2(TODAY2-NBILD)                  
         DC    AL1(PTATYPE-PTAELD,L'PTATYPE),AL1(QI,PTATRAL)                    
         DC    AL1(PTASTAT1-PTAELD,L'PTASTAT1),AL2(PTASB1-NBILD)                
         DC    AL1(PTASTAT2-PTAELD,L'PTASTAT1),AL2(PTASB2-NBILD)                
         DC    AL1(PTANET-PTAELD,L'PTANET),AL1(QP,QNET$)                        
         DC    AL1(PTANETF-PTAELD,L'PTANETF),AL1(QP,QPZRO)                      
         DC    AL1(PTACUR-PTAELD,L'PTACUR),AL2(CURCOD-NBILD)                    
         DC    AL1(PTAMOA-PTAELD,L'PTAMOA),AL2(BILDT1-NBILD)                    
         DC    AL1(PTACDSC-PTAELD,L'PTACDSC),AL1(QP,QPZRO)                      
         DC    AL1(PTARCORT-PTAELD,L'PTARCORT),AL1(QP,QPZRO)                    
         DC    AL1(PTARCOM-PTAELD,L'PTARCOM),AL1(QP,QPZRO)                      
         DC    AL1(PTARDATE-PTAELD,L'PTARDATE),AL2(TODAY2-NBILD)                
         DC    AL1(PTARBLNO-PTAELD,L'PTARBLNO),AL2(BILNUM-NBILD)                
         DC    AL1(PTARBLDT-PTAELD,L'PTARBLDT),AL2(BILDT2-NBILD)                
         DC    AL1(PTARCODE-PTAELD,L'PTARCODE),AL2(BILTNUM-NBILD)               
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFPOB-BGN)      PERCENT OF BILL                 
         DC    AL1(PTANET-PTAELD,L'PTANET),AL1(QP,QPOB$)                        
         DC    AL1(PTACUR-PTAELD,L'PTACUR),AL2(BZERO-NBILD)                     
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFASPB-BGN)      ASP BILLED                     
         DC    AL1(PTANET-PTAELD,L'PTANET),AL1(QP,QASP$)                        
         DC    AL1(PTACUR-PTAELD,L'PTACUR),AL2(BZERO-NBILD)                     
         DC    AL1(PTARTYPE-PTAELD,L'PTARTYPE),AL1(QI),C'R'                     
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFASPN-BGN)      ASP NOT BILLED                 
         DC    AL1(PTADATE-PTAELD,L'PTADATE),AL2(BZERO-NBILD)                   
         DC    AL1(PTATYPE-PTAELD,L'PTATYPE),AL2(BZERO-NBILD)                   
         DC    AL1(PTASTAT1-PTAELD,L'PTASTAT1),AL2(BZERO-NBILD)                 
         DC    AL1(PTANET-PTAELD,L'PTANET),AL1(QP,QPZRO)                        
         DC    AL1(PTACUR-PTAELD,L'PTACUR),AL2(BZERO-NBILD)                     
         DC    AL1(PTARDATE-PTAELD,L'PTARDATE),AL2(BZERO-NBILD)                 
         DC    AL1(PTARBLNO-PTAELD,L'PTARBLNO),AL2(BZERO-NBILD)                 
         DC    AL1(PTARBLDT-PTAELD,L'PTARBLDT),AL2(BZERO-NBILD)                 
         DC    AL1(PTARCODE-PTAELD,L'PTARCODE),AL2(BZERO-NBILD)                 
         DC    AL1(PTAMOA-PTAELD,L'PTAMOA),AL2(BZERO-NBILD)                     
         DC    AL1(PTARFORM-PTAELD,1),AL2(BZERO-NBILD)                          
         DC    AL1(PTARFORM-PTAELD+1,1),AL2(BZERO-NBILD)                        
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFGRP-BGN)      GROUP BILL                      
         DC    AL1(PTARFORM-PTAELD,1),AL2(FORMCDE-NBILD)                        
         DC    AL1(PTARFORM-PTAELD+1,1),AL2(GRPLCDE-NBILD)                      
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
RFLLM    DS    0H                  R. L. FILTER                                 
         DC    AL1(RFLELQ,RFLLNQ+L'WRKCCUR)                                     
         DC    AL1(RFLTYPE-RFLELD,L'RFLTYPE),AL1(QI,RFLWC)                      
         DC    AL1(RFLDATA-RFLELD,L'WRKCCUR),AL2(WRKCCUR-NBILD)                 
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFASPP-BGN)    ASP                              
         DC    AL1(RFLDATA-RFLELD,L'WRKCCUR),AL2(ASPWRK-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCIDLM   DS    0H                  SUBSIDIARY - DISCOUNT                        
         DC    AL1(SCIELQ,SCILN1Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITCDSC)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QCSD$)                      
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCIGLM   DS    0H                  SUBSIDIARY - GROSS                           
         DC    AL1(SCIELQ,SCILN1Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITGRSS)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QGRSZ$)                     
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCIGZLM  DS    0H                  SUBSIDIARY - GROSS - ZERO                    
         DC    AL1(SCIELQ,SCILN1Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITGRSS)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QPZRO)                      
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCIILM   DS    0H                  SUBSIDIARY - INCOME                          
         DC    AL1(SCIELQ,SCILN1Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITINCA)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QTCOM$)                     
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCITNLM  DS    0H                  SUBSIDIARY - TAX - NET                       
         DC    AL1(SCIELQ,SCILN1Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITGLEV)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QTAXBAS$)                   
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCITGLM  DS    0H                  SUBSIDIARY - TAX - GST                       
         DC    AL1(SCIELQ,SCILN3Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITTAXP)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QTAXTAX$)                   
         DC    AL1(SCIBASE-SCIELD,L'SCIBASE),AL1(QP,QTAXBAS$)                   
         DC    AL1(SCISUBPT-SCIELD,L'SCISUBPT),AL2(TXTYPE-NBILD)                
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SCITPLM  DS    0H                  SUBSIDIARY - TAX - PST                       
         DC    AL1(SCIELQ,SCILN3Q)                                              
         DC    AL1(SCITYPE-SCIELD,L'SCITYPE),AL1(QI,SCITTQST)                   
         DC    AL1(SCIAMNT-SCIELD,L'SCIAMNT),AL1(QP,QTAXTAX$)                   
         DC    AL1(SCIBASE-SCIELD,L'SCIBASE),AL1(QP,QTAXBAS$)                   
         DC    AL1(SCISUBPR-SCIELD,L'SCISUBPR),AL2(TXPROV-NBILD)                
         DC    AL1(SCISUBPT-SCIELD,L'SCISUBPT),AL2(TXTYPE-NBILD)                
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
SPALM    DS    0H                  SPECIAL POSTING (RETAIL)                     
         DC    AL1(SPAELQ,SPALNQ)                                               
         DC    AL1(SPATYPE-SPAELD,L'SPATYPE),AL1(QI,SPATPART)                   
         DC    AL1(SPAAULA-SPAELD,L'SPAAULA),AL2(CRTLACC+1-NBILD)               
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
TRSLM    DS    0H                  TRANSACTION STATUS                           
         DC    AL1(TRSELQ,TRSLNQ)                                               
         DC    AL1(TRSUDAT-TRSELD,L'TRSUDAT),AL2(TODAY2-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
TRSLM99  DS    0H                  TRANSACTION STATUS FOR 99                    
         DC    AL1(TRSELQ,TRSLNQ)                                               
         DC    AL1(TRSUSER-TRSELD,L'TRSUSER),AL2(USERID-NBILD)                  
*                                                                               
         DC    AL2(ELMCONQ),AL2(IFSOON-BGN)     IF SOON                         
         DC    AL1(TRSSTAT3-TRSELD,L'TRSSTAT3),AL1(QI,TRSSOON)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
VBILM    DS    0H                  GST BILLED                                   
         DC    AL1(VBIELQ,VBILNQ)                                               
         DC    AL1(VBITYPE-VBIELD,L'VBITYPE),AL2(TXTYPE-NBILD)                  
         DC    AL1(VBIRATE-VBIELD,L'VBIRATE),AL2(TXRATE-NBILD)                  
         DC    AL1(VBIVAT-VBIELD,L'VBIVAT),AL1(QP,QTAXTAX$)                     
         DC    AL1(VBIGROSS-VBIELD,L'VBIGROSS),AL1(QP,QTAXGRS$)                 
         DC    AL1(VBICOMM-VBIELD,L'VBIGROSS),AL1(QP,QTAXCOM$)                  
         DC    AL1(VBIACCT-VBIELD,L'VBIACCT),AL2(TXACCT-NBILD)                  
         DC    AL1(VBIDATE-VBIELD,L'VBIDATE),AL2(TXEFFD-NBILD)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
PIDLM    DS    0H                  PERSON ID                                    
         DC    AL1(PIDELQ,PIDLNQ)                                               
         DC    AL1(PIDNO-PIDELD,L'PIDNO),AL2(USERPID-NBILD)                     
         DC    AL1(EOT)                                                         
                                                                                
         TITLE 'DSECTS'                                                         
***********************************************************************         
* DSECT TO COVER POSTING CONTROL TABLE                                *         
***********************************************************************         
PCD      DSECT                                                                  
PCACCT   DS    AL2                 ACCOUNT                                      
PCWORK   DS    AL2                 WORKCODE                                     
PCCNTR   DS    AL2                 CONTRA ACCOUNT                               
PCCNME   DS    AL2                 CONTRA NAME                                  
PCTRAN   DS    AL2                 TRANSACTION DEFINITION                       
PCELEM   DS    AL2                 EXTRA ELEMENTS                               
PCCOND   DS    AL2                 CONDITIONAL                                  
PCHOOK   DS    AL2                 HOOK                                         
PCSTAT   DS    XL1                 STATUS                                       
PCSCOST  EQU   X'80'                COST ACCOUNTING POSTING                     
PCNOZRO  EQU   X'40'                DON'T MAKE A ZERO POSTING                   
PCSPOB   EQU   X'20'                PERCENT OF BILL POSTING                     
PCSASP   EQU   X'10'                ASP POSTING                                 
PCSASK   EQU   X'08'                ASP SK POSTING                              
PCSASI   EQU   X'04'                ASP SI POSTING                              
         DS    XL1                 N/D                                          
PCLNQ    EQU   *-PCD                                                            
                                                                                
                                                                                
***********************************************************************         
* DSECT TO COVER THE EXTRA ELEMENTS TABLE                             *         
***********************************************************************         
XLD      DSECT                                                                  
XLSTA    DS    XL1                 DATA TYPE                                    
XLSLOC   EQU   X'80'                LOCAL STORAGE AREA                          
XLSGBL   EQU   X'40'                GLOBAL STORAGE                              
XLSAPE   EQU   X'20'                ANALYSIS POINTER                            
XLSGRP   EQU   X'10'                GROUP ANALYSIS POINTER                      
XLELM    DS    AL2                 EXTRA ELEMENT DETAIL - SEE ELMD              
         ORG   XLELM                                                            
XLLOC    DS    AL2                 LOCAL AREA                                   
         ORG   XLELM                                                            
XLGBL    DS    AL2                 GLOBAL STORAGE AREA                          
         ORG   XLELM                                                            
XLAPE    DS    AL2                 ANALYSIS POINTER                             
*                                                                               
XLCNDL   DS    AL2                 CONDITIONAL LIST                             
XLHOOK   DS    AL2                 HOOK                                         
XLLNQ    EQU   *-XLD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ELEMENT DETAIL TABLE                                 *         
***********************************************************************         
ELMD     DSECT                                                                  
ELMCODE  DS    AL1                 ELEMENT CODE                                 
ELMELN   DS    AL1                 ELEMENT LENGTH                               
*                                                                               
ELMDATA  DS    0XL4                                                             
ELMDSP   DS    AL1                 DISPLACEMENT TO DATA IN ELEMENT              
ELMDLN   DS    AL1                 DATA LENGTH                                  
ELMDSRC  DS    AL2                 DATA SOURCE                                  
         ORG   ELMDSRC                                                          
ELMTYP   DS    XL1                 DATA TYPE (IF NOT BASE DISPLACEMENT)         
ELMTIMM  EQU   C'I'                IMMEDIATE (DATA FOLLOWS)                     
ELMTPKD  EQU   C'P'                PACKED DATA  (FIELD EQUATE FOLLOWS)          
ELMTPBD  EQU   C'X'                CONVERT PACKED TO BINARY                     
ELMDFLD  DS    XL1                 DATA OR FIELD EQUATE                         
*                                                                               
         ORG   ELMDATA                                                          
ELMCONL  DS    0XL4                                                             
ELMCON   DS    AL2                 FLAG FOR CONDITIONAL                         
ELMCONQ  EQU   0                                                                
ELMCNDL  DS    AL2                 A(CONDITIONAL LIST)                          
*                                                                               
         ORG   ELMDATA                                                          
ELMROUL  DS    0XL4                                                             
ELMROU   DS    AL2                 SPECIAL ROUTINE                              
ELMROUQ  EQU   1                                                                
ELMROUTE DS    AL2                 HOOK TO SPECIAL ROUTINE                      
*                                                                               
         ORG   ELMDATA                                                          
ELMPOPL  DS    0XL4                                                             
ELMPOP   DS    AL2                 POSTING OPTION                               
ELMPOPQ  EQU   2                                                                
ELMPTYPE DS    AL1                 POSTING MODES                                
         DS    XL1                 N/D                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ANALYSIS POINTER ENTRY                               *         
***********************************************************************         
APED     DSECT                                                                  
APESTA   DS    XL1            STATUS                                            
APESDR   EQU   X'80'           DEBIT                                            
APESCA   EQU   X'40'           COST ACCOUNTING                                  
APEACCT  DS    XL2            ACCOUNT                                           
APECNDL  DS    XL2            CONDITIONAL                                       
APELNQ   EQU   *-APED                                                           
         EJECT                                                                  
NBILD    DSECT                                                                  
* ACREPNBC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBC                                                       
         PRINT ON                                                               
* ACREPNBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBD                                                       
         PRINT ON                                                               
JXBLKD   DSECT                                                                  
* ACJAXD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACJAXD                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPNB05 07/31/12'                                      
         END                                                                    
