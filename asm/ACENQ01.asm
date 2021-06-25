*          DATA SET ACENQ01    AT LEVEL 030 AS OF 10/16/18                      
*PHASE T62001B                                                                  
*INCLUDE PUBVAL                                                                 
***********************************************************************         
* US LEVELS                                                           *         
* ---------                                                           *         
* ID   LVL DATE    JIRA         DESCRIPTION                           *         
* ---- --- ------- ------------ --------------------------------------*         
* RGUP 030 12OCT18 <SPEC-28550> REPORT ON CREDIT CARD PAYMENT METHOD  *         
*                               CCARD                                 *         
*                                                                     *         
***********************************************************************         
T62001   TITLE 'ACCOUNT ENQUIRY - CREDITOR'                                     
T62001   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**ENQ1**,R6,R7,CLEAR=YES,RR=RE                                 
*                                                                               
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         L     RF,ATIA                                                          
         LHI   RE,CRBLOCK-TIAD                                                  
         AR    RF,RE               RF=A(CREDITOR SAVE BLOCK)                    
         ST    RF,ACRBLOCK                                                      
         MVI   CRFLAG,0                                                         
*                                                                               
         L     RE,ORELO                                                         
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL           GRID COLUMN TABLE                            
         L     RF,=A(CONDAT)                                                    
         AR    RF,RE                                                            
         ST    RF,ACONDAT          COLUMN FORMAT ROUTINE                        
         L     RF,=A(CONTOT)                                                    
         AR    RF,RE                                                            
         ST    RF,ACONTOT          GRID COLUMN TOTAL FORMAT ROUTINE             
         L     RF,=A(CATRTAB)                                                   
         AR    RF,RE                                                            
         ST    RF,ACATRTAB         COLUMN ATTRIBUTE TABLE                       
         L     RF,=A(SPCGR)                                                     
         AR    RF,RE                                                            
         ST    RF,ASPCGR           SPECIAL GRID COLUMN ROUTINE                  
*                                                                               
         L     R2,=A(DCMIX)        ->   DICTATE MIXED CASE TABLE                
         A     R2,ORELO                                                         
         GOTO1 VDICTATE,DMCB,C'LL  ',(R2),DSMIX                                 
         L     R2,=A(DCCAP)        ->   DICTATE UPPER CASE TABLE                
         A     R2,ORELO                                                         
         GOTO1 (RF),(R1),C'L   ',(R2),DSCAP                                     
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY ?                     
         BO    MAIN05              NO,  SKIP                                    
         BAS   RE,FSTDIS           YES, PERFORM FIRST DISPLAY FUNCTIONS         
         BNE   ERRXIT              ON   ERROR, EXIT                             
         OI    DISPFLAG,NOTFRSTQ   SET  NOT FIRST TIME FLAG ON                  
         B     MAIN60              READ RECORD FOR ACCOUNT                      
*                                                                               
MAIN05   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    MAIN11               YES                                         
         CLC   DISLINE,DISSTART    START OF SCREEN ?                            
         BNE   MAIN11               NO,SKIP                                     
         GOTO1 ABLDDIS             BUILD COL HEADINGS AND DISPLACEMENTS         
         MVI   DISATRIB,HILIGHTQ                                                
         GOTO1 ADISPLAY,DISATRIB                                                
         BE    MAIN11                                                           
         DC    H'0'                                                             
*                                                                               
MAIN11   TM    DISPFLAG,NORECQ     ANY RECORDS TO DISPLAY ?                     
         BZ    MAIN12               NO,SKIP                                     
         TM    OVRSTAT,OVRREVQ     REVERSAL ?                                   
         BO    MAIN35                                                           
         B     MAINX                                                            
*                                                                               
MAIN12   TM    DISPFLAG,ALLREADQ   ALL RECORDS READ ?                           
         BO    MAIN30              YES                                          
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL ?                            
         BO    MAIN20              YES                                          
*                                                                               
MAIN15   GOTO1 AIO,IOREAD+IOACCDIR+IO1 RESTORE READ SEQUENCE                    
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    MAIN130                                                          
         DC    H'0'                                                             
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALREADY GOT RECORD IN TSAR ?         
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAIN32                                                           
         TM    DETFLAG,DETFENDQ                                                 
         BO    MAIN52                                                           
         L     R0,ATSARREC         LOAD TSAR RECORD                             
         L     RE,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     MAIN33                                                           
*                                                                               
MAIN32   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
MAIN33   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR ONTO DUMMIES FOR GRIDS           
         GOTO1 ADISPLAY,DISATRIB                                                
         BE    MAIN38                                                           
*                                                                               
MAIN35   BAS   RE,TOTLINE                                                       
         OI    DETFLAG,DETFENDQ                                                 
         B     MAINX                                                            
*                                                                               
MAIN38   MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(,RF)                                                        
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN20                                                           
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL ?                            
         BZ    MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(EA2MNYVE) TOO MANY ITEMS, TRY DIS=.MERGE=Y          
         BAS   RE,TOTLINE                                                       
         B     ERRXIT                                                           
*                                                                               
MAIN50   BAS   RE,TBLDDIS                                                       
         BE    MAIN51                                                           
         TM    DISPFLAG,TSARFULQ                                                
         BO    ERRXIT                                                           
         B     MAINX                                                            
*                                                                               
MAIN51   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ ?                 
         BZ    MAIN60                                                           
MAIN52   BAS   RE,TOTLINE                                                       
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAINX                                                            
         OI    DETFLAG,DETFENDQ                                                 
         BAS   RE,FGRMTSAR                                                      
         GOTO1 ADISPLAY,DISATRIB                                                
         B     MAINX                                                            
*                                                                               
MAIN55   MVC   KEYSAVE,IOKEY                                                    
*                                                                               
MAIN60   GOTO1 AIO,IOSEQ+IOACCDIR+IO1 READ NEXT RECORD FOR ACCOUNT              
         BE    MAIN65                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED ?                            
         BO    MAIN130                                                          
         DC    H'0'                                                             
*                                                                               
         USING TRNRECD,R3                                                       
MAIN65   LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING RECORD)              
         CLC   TRNKCULA,IOKEYSAV   DIFFERENT ACCOUNT ?                          
         BE    MAIN70                                                           
         TM    CRFLAG,CRFULLQ      CREDIT TABLE FULL ?                          
         BZ    MAIN106                                                          
         B     MAIN104                                                          
*                                                                               
MAIN70   TM    CRFLAG,CRFULLQ      CREDIT TABLE FULL ?                          
         BZ    MAIN71                                                           
         CLC   CRKEY,TRNKEY        WITHIN CURRENT CREDIT RANGE ?                
         BNE   MAIN104                                                          
*                                                                               
MAIN71   SR    R1,R1                                                            
         ICM   R1,1,CONTLEN        R1=L'(CONTRA CODE)                           
         BZ    MAIN100                                                          
         LA    RF,L'TRNKULC                                                     
         LA    RE,TRNKULC                                                       
*                                                                               
MAIN72   CLI   0(RE),C' '          FIND NON-BLANK CHARACTER                     
         BH    MAIN74                                                           
         LA    RE,1(,RE)                                                        
         BCT   RF,MAIN72                                                        
         B     MAIN75                                                           
*                                                                               
MAIN74   CR    R1,RF               CONTRA FILTER LONGER ?                       
         BH    MAIN75                                                           
         BCTR  R1,0                                                             
         EXCLC R1,CONTRA,0(RE)                                                  
         BNE   MAIN75                                                           
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER ?                            
         BNE   MAIN100                                                          
         B     MAIN55                                                           
*                                                                               
MAIN75   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER ?                            
         BE    MAIN100                                                          
         B     MAIN55                                                           
*                                                                               
MAIN100  LA    R3,IOKEY            R3=A(RECORD KEY)                             
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD ?                         
         BNH   MAIN55                                                           
         BAS   RE,FILTKEY          APPLY FILTERING TO TRAN KEY (ACCDIR)         
         BNE   MAIN55              DO WE WANT TO KEEP THIS RECORD?              
         MVC   DADDRESS,TRNKDA     DISK ADDRESS                                 
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN101                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED ?                            
         BO    MAIN130                                                          
*                                                                               
MAIN101  L     R3,AIO1                                                          
         GOTO1 AOFFTRN             APPLY SECURITY CHECK/FILTER                  
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         CLI   OFFLFLAG,0          REJECTED BECAUSE OF OFFICE ?                 
         BNE   MAIN60                                                           
         BAS   RE,FILTER           APPLY FILTERING TO TRAN REC (ACCMST)         
         BNE   MAIN60              DO WE WANT TO KEEP THIS RECORD ?             
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
         LA    RF,TRNRFST                                                       
         TM    TRNSTAT-TRNELD(RF),TRNSDR DEBIT?                                 
         BZ    MAIN102                                                          
         BAS   RE,GETDRDET         GET DEBIT DETAILS                            
         B     MAIN60                                                           
*                                                                               
MAIN102  TM    CRFLAG,CRFULLQ                                                   
         BO    MAIN60                                                           
         L     RF,=A(GETCRDET)                                                  
         A     RF,ORELO                                                         
         BASR  RE,RF               GET CREDIT DETAILS                           
         BNE   MAIN103             FITTED IN TABLE ?                            
         MVC   CRKEY,IOKEY         CURRENT CREDIT KEY                           
         B     MAIN60                                                           
*                                                                               
MAIN103  MVC   KEYSAVE2,IOKEYSAV   START KEY WHEN ROOM FOR MORE CREDITS         
         OI    CRFLAG,CRFULLQ                                                   
         B     MAIN60                                                           
*                                                                               
MAIN104  MVC   KEYSAVE,KEYSAVE2                                                 
         BAS   RE,TBLDDIS                                                       
         BE    MAIN105                                                          
         TM    DISPFLAG,TSARFULQ                                                
         BO    ERRXIT                                                           
         B     MAINX                                                            
*                                                                               
MAIN105  MVI   CRFLAG,0                                                         
         MVC   IOKEY,KEYSAVE2                                                   
         MVI   ANXTLINE,C'Y'                                                    
         B     MAIN15                                                           
*                                                                               
MAIN106  BAS   RE,TBLDDIS                                                       
*                                                                               
         OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR ?               
         BNZ   MAIN125                                                          
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         TM    OVRSTAT,OVRREVQ     REVERSALS FOUND ?                            
         BZ    MAINX                                                            
*                                                                               
MAIN125  OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BAS   RE,TOTLINE          DEAL WITH TOTAL LINE                         
         TM    CRFLAG,CRSFULLQ                                                  
         BO    *+8                                                              
         OI    DETFLAG,DETFENDQ                                                 
         B     MAINX                                                            
*                                                                               
MAIN130  OI    DISPFLAG,DISIOMAX   MAX IO REACHED                               
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         ZAP   DEBTOT,=P'0'                                                     
         ZAP   CRETOT,=P'0'                                                     
         ZAP   DISTOT,=P'0'                                                     
         MVI   CURRFLAG,0          CURRENCY FLAG                                
         MVC   CURRLAST,SPACES     LAST CURRENCY CODE                           
         MVI   CRFLAG,0            CREDITOR FLAG                                
         MVC   SVCACN,SPACES       CLEAR CONTRA NAME                            
         MVC   CONTRA,SPACES       CLEAR CONTRA CODE FILTER                     
         MVI   CONTLEN,0           LENGTH OF INPUT CONTRA FILTER                
         MVI   NEGCONT,0           NEGATIVE FILTER FLAG                         
         MVC   RECCOUNT,=H'1'                                                   
         MVI   DETFLAG,0                                                        
         L     R0,ACRBLOCK         CLEAR CREDITOR BLOCK                         
         LHI   R1,L'CRBLOCK                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,=V(PUBVAL)       STORE ADDRESS OF PUBVAL                      
         A     RF,ORELO                                                         
         ST    RF,VPUBVAL                                                       
*                                                                               
         GOTO1 AUNITLDG,SPROUNIT   READ PRODUCTION UNIT/LEDG                    
         CLI   LEDGTLVA,0                                                       
         BE    FSTDERR                                                          
         MVC   CLEN(L'CLEN+L'CPLEN+L'CPJLEN),LEDGTLVA LEDGER STRUCTURE          
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        ANYTHING INPUT ?                             
         BZ    FSTD10                                                           
         LA    RE,FLDDATA                                                       
         CLI   0(RE),NEGFILTR      NEGATIVE FILTER ?                            
         BNE   FSTD05                                                           
         MVI   NEGCONT,NEGFILTR    SET AS NEGATIVE FILTER                       
         LA    RE,1(RE)                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SH    RF,=H'1'                                                         
         BZ    FSTDERR             NO ACCOUNT ENTERED ?                         
*                                                                               
FSTD05   STC   RF,CONTLEN          LENGTH OF CONTRA CODE INPUT                  
         BCTR  RF,0                                                             
         EXMVC RF,CONTRA,0(RE)                                                  
         GOTO1 ACNAME              ATTEMPT TO FIND CONTRA NAME                  
*                                                                               
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(EGIFMISS) MISSING INPUT                             
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN        R4=L'(KEY FIELD INPUT)                       
         BZ    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFLONG) INPUT TOO LONG                            
         CLI   FLDILEN,L'ACTKULA                                                
         BH    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EALDGINV) INVALID LEDGER                            
         CLI   BASKEY,C'S'         SUBSIDIARY UNIT ?                            
         BNE   FSTDERR                                                          
*                                                                               
         USING LEDGTABD,RF                                                      
         L     RF,ALDGTAB          RF=A(LEDGER TABLE)                           
*                                                                               
FSTD11   CLI   LEDGTYPE,EOT        END OF TABLE ?                               
         BE    FSTDERR                                                          
         CLC   LEDGTYPE,FLDDATA+1  MATCH ON LEDGER ?                            
         BE    FSTD11A                                                          
         LA    RF,LEDGLNQ(,RF)     BUMP RF                                      
         B     FSTD11                                                           
*                                                                               
FSTD11A  TM    LEDGATTR,LEDGCRD    CREDITOR LEDGER ?                            
         BZ    FSTDERR                                                          
         DROP  RF                                                               
*                                                                               
         MVC   UNILDG,FLDDATA                                                   
         GOTO1 AUNITLDG,FLDDATA    READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EAWRNGLV) WRONG LEVEL ACCOUNT                       
         LA    RF,LEDGTLVA         RF=A(HIERARCHY LEVELS)                       
         LR    RE,RF               RE=RF                                        
         SR    R1,R1               R1=MINIMUM LENGTH OF ACCOUNT                 
*                                                                               
FSTD11B  CLI   0(RF),X'0C'         FULL HIERARCHY ?                             
         BE    FSTD11C                                                          
         LA    RF,1(,RF)           NO BUMP RF                                   
         B     FSTD11B                                                          
*                                                                               
FSTD11C  CR    RF,RE               ONE LEVEL LEDGER ?                           
         BE    FSTD12                                                           
         BCTR  RF,0                RF=A(MINIMUM LENGTH)                         
         IC    R1,0(,RF)                                                        
         LR    RE,R4               RE=L'(INPUT)                                 
         SHI   RE,L'ACTKUNT+L'ACTKLDG SUBTRACT UNIT/LEDGER LENGTH               
         CR    RE,R1               IS INPUT LONG ENOUGH ?                       
         BNH   FSTDERR                                                          
*                                                                               
         USING ACTRECD,R3                                                       
FSTD12   LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO        COMPANY                                      
         BCTR  R4,0                                                             
         EXMVC R4,ACTKUNT,FLDDATA  ACCOUNT CODE                                 
         MVC   FVMSGNO,=AL2(EAIFNTFN) ACCOUNT NOT FOUND                         
*                                                                               
         CLI   ACTKLDG,C'P'        DEAL WITH PUB NUMBERS                        
         BE    FSTD13                                                           
         CLI   ACTKLDG,C'Q'                                                     
         BNE   FSTD15                                                           
*                                                                               
FSTD13   CLI   FLDILEN,6           ASSUME PRINT REPS ARE LESS                   
         BL    FSTD15              THAN 6 LONG                                  
         CLI   IOKEY+9,C'*'        ACCOUNTS CREATED AS OFFICE OVERRIDES         
         BE    FSTD15                                                           
         LR    R4,R1                                                            
         MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    FSTD15                                                           
         MVC   IOKEY,KEYSAVE                                                    
         MVC   IOKEY+4(20),SPACES                                               
         GOTO1 VPUBVAL,DMCB,((R4),BASKEY+1),(1,IOKEY+4)                         
         CLI   0(R1),X'FF'                                                      
         BE    FSTDERR                                                          
*                                                                               
FSTD15   GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    FSTD20                                                           
         TM    IOERR,IOERNF                                                     
         BO    FSTDERR             LOW LEVEL ACCOUNT NOT FOUND                  
         DC    H'0'                                                             
*                                                                               
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC) SECURITY LOCKOUT                          
         GOTO1 AOFFACC             TEST OFFICE SECURITY                         
         CLI   OFFLFLAG,OFFLSEC    SECURITY LOCKOUT ?                           
         BE    FSTDERR                                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    FSTD24               YES                                         
         LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
         MVI   SCRLLADJ,2          ADJUSTMENT VALUE FOR PAGE UP                 
*                                                                               
         LA    R2,ENQDAT2H                                                      
         MVC   FLDDATA(L'ENQDAT2),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC ACCOUNT                                 
         LA    R1,FLDDATA+L'MX@ACC-1                                            
*                                                                               
FSTD22   CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,FSTD22                                                        
         MVI   1(R1),C'='                                                       
*                                                                               
FSTD24   L     R3,AIO1                                                          
         GOTO1 ADISACC                                                          
*                                                                               
         LA    RF,ACTRFST                                                       
FSTD30   CLI   0(RF),EOR           END OF RECORD ?                              
         BE    FSTD50                                                           
         CLI   0(RF),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    FSTD45                                                           
FSTD35   SR    R0,R0                                                            
         IC    R0,1(,RF)                                                        
         AR    RF,R0                                                            
         B     FSTD30                                                           
FSTD45   MVC   PAYTYP,RSTANAL-RSTELD(RF) ANALYSIS CODE                          
         B     FSTD35                                                           
*                                                                               
FSTD50   LA    R2,ENQDAT2H-ENQDAT1H(,R2)                                        
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    FSTD51               YES                                         
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD52                                                           
FSTD51   GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD52   GOTO1 ABLOVDIS                                                         
         BNE   FSTDERR                                                          
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    FSTD54               YES                                         
         GOTO1 ABLDDIS             BUILD COL HEADINGS AND DISPLACEMENTS         
         MVI   DISATRIB,HILIGHTQ                                                
         GOTO1 ADISPLAY,DISATRIB                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FSTD54   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         USING OPTVALSD,RF                                                      
FSTD55   L     RF,AOPTVALS         R2=A(OPTION VALUES)                          
         CLI   OREVERSE,0                                                       
         BNE   FSTDX                                                            
         CLI   PREVS,C'Y'          PROFILE TO SHOW REVERSALS                    
         BNE   FSTDX                                                            
         MVI   OREVERSE,C'Y'                                                    
         DROP  RF                                                               
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     FSTDEX                                                           
FSTDERR  LTR   RB,RB                                                            
FSTDEX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FILTER TRANSACTION KEYS (ACCDIR)                             *         
* ON ENTRY R3=A(TRANSACTION KEY)                                      *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
*                                                                               
         USING OPTVALSD,R2                                                      
         USING TRNRECD,R3                                                       
*                                                                               
FILTKEY  NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         TM    TRNKSTAT,TRNSREVS   REVERSAL ?                                   
         BZ    *+8                                                              
         OI    OVRSTAT,OVRREVQ     ACCOUNT HAS REVERSED TRANSACTIONS            
*                                                                               
         CLI   TRNKREF+MBVOFFSQ,MBVINDIQ 127+ TRANSACTIONS ?                    
         BE    FILTK10             ACCMST MUST BE USED FOR FILTER               
         OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILTK10                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),+        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK10  TM    TRNKSTAT,TRNSDRFT   DRAFT ?                                      
         BZ    FILTK20                                                          
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK30                                                          
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK30                                                          
         B     FILTKRJX                                                         
*                                                                               
FILTK20  CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
FILTK30  TM    TRNKSTA2,TRNSPEEL    PEELED?                                     
         BZ    FILTK40                                                          
         CLI   OPEELED,C'Y'                                                     
         BE    FILTKX                                                           
         CLI   OPEELED,C'O'                                                     
         BE    FILTKX                                                           
         B     FILTKRJX                                                         
*                                                                               
FILTK40  CLI   OPEELED,C'O'                                                     
         BE    FILTKRJX                                                         
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     FILTKEX                                                          
FILTKRJX LTR   RB,RB                                                            
FILTKEX  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*        FILTER TRANSACTIONS (ACCMST)                                 *         
* ON ENTRY R3=A(TRANSACTION RECORD)                                   *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
         USING TRNRECD,R3                                                       
         USING TRNELD,R4                                                        
FILTER   NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
*                                                                               
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT ON RECORD)                
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILT40                                                           
         LA    RF,TRNKREF          RF=A(TRANSACTION REF)                        
*                                                                               
         USING FFTELD,RE                                                        
         ICM   RE,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
         BZ    FILT30                                                           
         SR    R0,R0                                                            
FILT10   CLI   FFTEL,0                                                          
         BNE   FILT30                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTKREF    KEY REF NUMBER FOR BANK VOID?                
         BE    FILT20                                                           
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     FILT10                                                           
FILT20   LA    RF,FFTDATA                                                       
         DROP  RE                                                               
FILT30   GOTO1 ASCOMP,DMCB,(RF),(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),   X        
               OREFFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT40   TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BO    FILTX                                                            
*                                                                               
         MVI   OTHFLAG,0           INITIALISE OTHERS FLAG                       
         MVI   DUEFLAG,0           INITIALISE DUE DATE FLAG                     
         MVI   AFPFLAG,0           INITIALISE ARTISTE FEE FLAG                  
         MVI   MXPFLAG,0           INITIALISE MXPELD FLAG                       
*-----------------------------------                                            
* TRANSACTION FILTERS                                                           
*-----------------------------------                                            
FILT60   OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE ?              
         BZ    FILT70                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,      X        
               ODATEFI                                                          
         BNE   FILTREJX                                                         
*                                                                               
FILT70   OC    OMOA,OMOA           FILTERING ON TRANSACTION MOS                 
         BZ    FILT80                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNRSMOS,TRNRSMOS),OMOAST,OMOAEN,OMOAFI           
         BNE   FILTREJX                                                         
*                                                                               
FILT80   TM    TRNSTAT,TRNSAUTH    AUTHORISED ?                                 
         BZ    FILT90                                                           
         CLI   OAUTH,C'N'                                                       
         BE    FILTREJX                                                         
         B     FILT100                                                          
*                                                                               
FILT90   CLI   OAUTH,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT100  TM    TRNSTAT,TRNSHOLD    HELD ?                                       
         BZ    FILT110                                                          
         CLI   OHELD,C'N'                                                       
         BE    FILTREJX                                                         
         B     FILT120                                                          
*                                                                               
FILT110  CLI   OHELD,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT120  TM    TRNSTAT,TRNSAPPR    SELECTED ?                                   
         BZ    FILT130                                                          
         CLI   OSELECT,C'N'                                                     
         BE    FILTREJX                                                         
         B     FILT140                                                          
*                                                                               
FILT130  CLI   OSELECT,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
FILT140  OC    OBATCH,OBATCH       BATCH REF FILTER ?                           
         BZ    FILT150                                                          
         GOTO1 ASCOMP,DMCB,TRNBREF,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT150  SR    RF,RF               INPUT TYPE FILTER ?                          
         ICM   RF,1,OBTYPEVL                                                    
         BZ    FILT170                                                          
         SR    RE,RE                                                            
         IC    RE,TRNTYPE                                                       
         CR    RE,RF                                                            
         BE    FILT160                                                          
         CLI   OBTYPEFI,NEGFILTR                                                
         BNE   FILTREJX                                                         
         B     FILT170                                                          
*                                                                               
FILT160  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTREJX                                                         
*                                                                               
FILT170  OC    ODUE,ODUE           FILTERING ON DUE DATE ?                      
         BZ    *+8                                                              
         OI    DUEFLAG,DUERQDQ     DUE DATE ELEMENT REQUIRED FLAG ON            
*                                                                               
         OC    ORUNNO,ORUNNO       FILTERING ON RUN NUMBER ?                    
         BZ    *+8                                                              
         OI    AFPFLAG,AFPRQDQ     ARTISTE FEE ELEMENT REQUIRED FLAG ON         
*                                                                               
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT ?                        
         BNZ   FILT180                                                          
         OC    OPROD,OPROD         FILTERING ON PRODUCT ?                       
         BNZ   FILT180                                                          
         OC    OJOB,OJOB           FILTERING ON JOB ?                           
         BNZ   FILT180                                                          
         OC    OESTIMAT,OESTIMAT   FILTERING ON ESTIMATE ?                      
         BNZ   FILT180                                                          
         OC    OSRC,OSRC           FILTERING ON SOURCE ?                        
         BNZ   FILT180                                                          
*        BZ    FILT230                                                          
         OC    OMMOS,OMMOS         FILTERING ON MEDIA MOS?                      
         BZ    FILT230                                                          
*                                                                               
FILT180  GOTO1 ABLDSRC                                                          
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT ?                        
         BZ    FILT190                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OCLINLN1,OCLINVL1),       X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT190  OC    OPROD,OPROD         FILTERING ON PRODUCT ?                       
         BZ    FILT200                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OPRODLN1,OPRODVL1),       X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT200  OC    OJOB,OJOB           FILTERING ON JOB ?                           
         BZ    FILT210                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OJOBLN1,OJOBVL1),         X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT210  OC    OESTIMAT,OESTIMAT   FILTERING ON ESTIMATE ?                      
         BZ    FILT220                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OESTLN1,OESTVL1),         X        
               (OESTLN2,OESTVL2),OESTFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT220  OC    OSRC,OSRC           FILTERING ON SOURCE ?                        
         BZ    FILT225                                                          
         CLC   SRCWORK,SPACES                                                   
         BE    FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK,(OSRCLN1,OSRCVL1),(OSRCLN2,OSRCVL2),X        
               OSRCFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT225  OC    OMMOS,OMMOS          FILTERING ON MEDIA MOS?                     
         BZ    FILT230                                                          
         GOTO1 ADCOMP,DMCB,(2,SRCFMMD),OMMOSST,OMMOSEN,OMMOSFI                  
         BNE   FILTREJX                                                         
*-----------------------------------                                            
* CHECK ELEMENTS ON TRANSACTION                                                 
*-----------------------------------                                            
FILT230  LA    R4,TRNRFST           R4=A(TRANSACTION ELEMENT)                   
         MVC   TRANCURR,COMPCURR    SET TRANSACTION CURRENCY                    
         ZAP   TRANAMNT,TRNAMNT     AGENCY TRANSACTION AMOUNT                   
*                                                                               
FILT250  SR    R0,R0                                                            
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
*                                                                               
         CLI   0(R4),EOR            END OF RECORD ?                             
         BE    FILT390                                                          
         CLI   0(R4),OTHELQ         OTHERS ELEMENT ?                            
         BE    FILT260                                                          
         CLI   0(R4),TRSELQ         TRANSACTION STATUS ELEMENT ?                
         BE    FILT270                                                          
         CLI   0(R4),DUEELQ         DUE DATE ELEMENT ?                          
         BE    FILT330                                                          
         CLI   0(R4),MXPELQ         MEDIA EXTRA PAYMENT ELEMENT ?               
         BE    FILT340                                                          
         CLI   0(R4),AFCELQ         ACCOUNT FOREIGN CURRENCY ELEMENT ?          
         BE    FILT350                                                          
         CLI   0(R4),AFPELQ         ARTISTE FEE ELEMENT ?                       
         BE    FILT360                                                          
         CLI   0(R4),FFTELQ         FREE FORM TEXT ELEMENT ?                    
         BE    FILT380                                                          
         B     FILT250                                                          
*-----------------------------------                                            
* OTHERS ELEMENT X'23'                                                          
*-----------------------------------                                            
         USING OTHELD,R4                                                        
FILT260  OI    OTHFLAG,OTHELEQ      OTHERS ELEMENT FOUND                        
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* TRANSACTION STATUS ELEMENT X'60'                                              
*-----------------------------------                                            
         USING TRSELD,R4                                                        
FILT270  CLI   OCONT,0              CONTRA'D OPTION ?                           
         BE    FILT280                                                          
         TM    TRSSTAT,TRSSOFFS     CONTRA'D ?                                  
         BZ    FILT275                                                          
         CLI   OCONT,C'N'           NO CONTRA'D ITEMS REQUIRED ?                
         BE    FILTREJX                                                         
         B     FILT280                                                          
*                                                                               
FILT275  CLI   OCONT,C'O'           ONLY CONTRA'D ITEMS REQUIRED ?              
         BE    FILTREJX                                                         
*                                                                               
FILT280  OC    OSTATDT,OSTATDT      FILTERING ON STATEMENT DATE ?               
         BZ    FILT285                                                          
         GOTO1 ADCOMP,DMCB,(L'TRSBSTDT,TRSBSTDT),OSTATST,OSTATEN,      X        
               OSTATFI                                                          
         BNE   FILTREJX                                                         
*                                                                               
FILT285  TM    TRSSTAT,TRSSVOID     ITEM VOIDED ?                               
         BZ    FILT290                                                          
         CLI   OVOID,C'N'                                                       
         BE    FILTREJX                                                         
         B     FILT295                                                          
*                                                                               
FILT290  CLI   OVOID,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT295  OC    OACT,OACT            FILTERING ON ACTIVITY DATE ?                
         BZ    FILT300                                                          
         OC    TRSDATE,TRSDATE                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT300  OC    OUSED,OUSED          FILTERING ON USED DATE ?                    
         BZ    FILT305                                                          
         OC    TRSUDAT,TRSUDAT                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPDAT) USED DATE                   
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OUSEST,OUSEEN,OUSEFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT305  TM    TRNRSTAT,TRNSREVS    REVERSAL ?                                  
         BZ    FILT320                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT250                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT250                                                          
         OC    OMOA,OMOA            FILTERING ON TRANSACTION MOS ?              
         BZ    FILTREJX                                                         
         OC    TRSRMOS,TRSRMOS      IF NO REV MOS ASSUME SAME AS TRAN           
         BZ    FILTREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOAST,OMOAEN,OMOAFI             
         BE    FILTREJX                                                         
         B     FILT250                                                          
*                                                                               
FILT320  CLI   OREVERSE,C'O'                                                    
         BE    FILTREJX                                                         
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* DUE DATE ELEMENT X'61'                                                        
*-----------------------------------                                            
         USING DUEELD,R4                                                        
FILT330  OC    ODUE,ODUE            FILTERING ON DUE DATE ?                     
         BZ    FILT250                                                          
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),ODUEST,ODUEEN,ODUEFI             
         BNE   FILTREJX                                                         
         OI    DUEFLAG,DUEELEQ      DUE DATE ELEMENT FOUND                      
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* MEDIA EXTRA PAYEMENT ELEMENT                                                  
*-----------------------------------                                            
         USING MXPELD,R4                                                        
FILT340  SR    RF,RF                                                            
         ICM   RF,7,MXPSER+1                                                    
         BZ    FILT250                                                          
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(7,RE),DUB                                                      
         MVC   0(1,RE),MXPSER                                                   
         GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   X        
               OSERFI                                                           
         BNE   FILTREJX                                                         
         OI    MXPFLAG,MXPELEQ     MATCHING MEDIA SRRIAL NUM FOUND              
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* ACCOUNT FOREIGN CURRENCY ELEM                                                 
*-----------------------------------                                            
         USING AFCELD,R4                                                        
FILT350  TM    AFCXSTAT,AFCXSMEM    MEMO ITEM?                                  
         BO    FILT250                                                          
         MVC   TRANCURR,AFCCURR     OVERRIDE TRANSACTION CURRENCY               
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* ARTISTE FEE ELEMENT                                                           
*-----------------------------------                                            
         USING AFPELD,R4                                                        
FILT360  TM    AFPFLAG,AFPRQDQ      ARTISTE FEE ELE REQUIRED ?                  
         BZ    FILT250                                                          
         CLC   ORUNVL,AFPPAYNO      MATCH ON RUN NUMBER ?                       
         BE    FILT370                                                          
         CLI   ORUNFI,NEGFILTR                                                  
         BNE   FILTREJX                                                         
         B     FILT375                                                          
*                                                                               
FILT370  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTREJX                                                         
*                                                                               
FILT375  OI    AFPFLAG,AFPELEQ      ARTISTE FEE ELEMENT FOUND                   
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* FREE FORM TEXT ELEMENT X'DB'                                                  
*-----------------------------------                                            
         USING FFTELD,R4            FREE FORM TEXT ELEMENT                      
FILT380  CLI   FFTTYPE,FFTTACUR     ASSOCIATED CURRENCY ?                       
         BNE   FILT250                                                          
         MVC   TRANCURR,FFTDATA     OVERRIDE TRANSACTION CURRENCY               
         B     FILT250                                                          
*                                                                               
* INVOICE NUMBER (PRODUCTION LONG NUMBER)                                       
*                                                                               
FILT390  OC    OSERIAL,OSERIAL      LONG INVOICE # FILTER                       
         BZ    FILT400                                                          
         XC    WORK,WORK                                                        
         XC    TEMP,TEMP                                                        
*                                                                               
         ICM   R4,15,AFFTELD        RE=A(FREE FORM TEXT ELEMENT)                
         BZ    FILT396                                                          
FILT392  CLI   FFTEL,0                                                          
         BE    FILT396                                                          
*                                                                               
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTINVN     LONG INVOICE NUMBER                         
         BE    FILT394                                                          
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     FILT392                                                          
*                                                                               
FILT394  ZIC   RF,FFTDLEN           DATA LENGTH                                 
         LTR   RF,RF                                                            
         BZ    FILT396                                                          
*                                                                               
         CHI   RF,L'CRMIN           MAKE SURE NOT BIGGER THAN WE CAN            
         BNH   *+8                  HANDLE                                      
         LHI   RF,L'CRMIN                                                       
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),FFTDATA                                                  
*                                                                               
FILT394A LA    RF,WORK                                                          
         LA    R1,TEMP                                                          
*                                                                               
FILT395  CLI   0(R1),0              END OF DATA?                                
         BE    FILT398                                                          
         CLI   0(R1),C'-'           LOOK FOR DASHES                             
         BNE   *+12                                                             
         AHI   R1,1                 SKIP THIS CHARACTER                         
         B     FILT395                                                          
         MVC   0(1,RF),0(R1)        MOVE INTO WORK                              
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         B     FILT395                                                          
*                                                                               
* MEDIA INVOICE OR REFERENCE                                                    
*                                                                               
FILT396  ICM   RE,15,AXPYELD        IS EXTRA PAYMENT ELEMENT PRESENT ?          
         BZ    *+14                 NO:GET TRAN-REF                             
         MVC   TEMP(L'XPYINV),XPYINV-XPYELD(RE)                                 
         B     FILT394A             SKIP FOR '-' IN INVOICE NO.                 
         MVC   WORK(L'TRNKREF),TRNKREF                                          
*                                                                               
FILT398  TM    TWAFLG1,TWAFRINP    HERE FROM INPUT?                             
         BZ    FILT399             . NO, THEN SOFT FILTER                       
         SR    RE,RE                                                            
         ICM   RE,1,OSERLN1                                                     
         BZ    FILT400                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   OSERVL1(0),WORK                                                  
         BNE   FILTREJX                   . NO, THEN REJECT                     
         B     FILT400                                                          
*                                                                               
FILT399  GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   +        
               OSERFI                                                           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*----------------------------------                                             
* FINISHED WITH ELEMENTS                                                        
*----------------------------------                                             
FILT400  OC    OAMOUNT,OAMOUNT     FILTERING TRANSACTION AMOUNT ?               
         BZ    FILT450                                                          
         CP    OAMTVL,TRANAMNT     FILTER AMOUNT=TRANS AMOUNT ?                 
         BE    FILT440                                                          
         CLI   OAMTSIGN,0          HAS A '+' OR '-' BEEN SPECIFIED ?            
         BNE   FILT410                                                          
         CLI   OAMTRNG,0           HAS A '>' OR '<' BEEN SPECIFIED ?            
         BNE   FILT410                                                          
         ZAP   TEMPNUM,OAMTVL      SEE IF AMOUNT IS -VE EQUIVALENT              
         AP    TEMPNUM,TRANAMNT                                                 
         BZ    FILT440             YES IT IS                                    
         B     FILT430                                                          
*                                                                               
FILT410  CLI   OAMTRNG,C'>'        MORE THAN SIGN ?                             
         BNE   FILT420                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT440                                                          
         B     FILT430                                                          
*                                                                               
FILT420  CLI   OAMTRNG,C'<'        LESS THAN SIGN ?                             
         BNE   FILT430                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT440                                                          
*                                                                               
FILT430  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED ?                  
         BNE   FILTREJX                                                         
         B     FILT450                                                          
*                                                                               
FILT440  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED ?                  
         BE    FILTREJX                                                         
*                                                                               
FILT450  CP    TRANAMNT,=P'0'                                                   
         BNL   FILT460                                                          
         CLI   ONEGATIV,C'N'       DO WE ONLY WANT +VE NUMBERS/ZERO ?           
         BE    FILTREJX                                                         
         B     FILT470                                                          
*                                                                               
FILT460  CLI   ONEGATIV,C'O'       DO WE ONLY WANT -VE NUMBERS ?                
         BE    FILTREJX                                                         
*                                                                               
FILT470  CLI   ORECON,0            ANY RECONCILED FILTER ?                      
         BZ    FILT480                                                          
         CLI   ORECON,C'Y'         SAME AS DEFAULT SETING ?                     
         BE    FILT480                                                          
         CLI   OMERGE,C'Y'         IF NOT MERGING APPLY FILT LATER ON           
         BNE   FILT480                                                          
         GOTO1 RECFILT,DMCB,0      APPLY RECONCILED FILT TO TRANS               
         BNE   FILTREJX                                                         
         TM    DISPFLAG,DISIOMAX                                                
         BO    FILTX                                                            
*                                                                               
FILT480  TM    OTHFLAG,OTHRQDQ     IS OTHERS ELEMENT REQUIRED ?                 
         BZ    FILT490                                                          
         TM    OTHFLAG,OTHELEQ     HAS OTHERS ELEMENT BEEN FOUND ?              
         BZ    FILTREJX                                                         
*                                                                               
FILT490  CLC   SBNKUL,BASKEY       SC LEDGER DATE/REF= TRANS DATE/REF           
         BNE   FILT500                                                          
*                                                                               
FILT500  TM    DUEFLAG,DUERQDQ     IS DUE ELEMENT REQUIRED ?                    
         BZ    FILT510                                                          
         TM    DUEFLAG,DUEELEQ     HAS DUE ELEMENT BEEN FOUND ?                 
         BO    FILT510                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODUEST,ODUEEN,ODUEFI           
         BNE   FILTREJX                                                         
*                                                                               
FILT510  TM    MXPFLAG,MXPRQDQ     IS MXP ELEMENT REQUIRED ?                    
         BZ    FILT520                                                          
         TM    MXPFLAG,MXPELEQ     HAS MXP ELEMENT BEEN FOUND ?                 
         BZ    FILTREJX                                                         
*                                                                               
FILT520  TM    AFPFLAG,AFPRQDQ     ARTISTE FEE ELEMENT REQUIRED                 
         BZ    FILTX                                                            
         TM    AFPFLAG,AFPELEQ     HAS AFP ELEMENT BEEN FOUND ?                 
         BZ    FILTREJX                                                         
*                                                                               
FILTX    CR    RB,RB                                                            
         B     FILTEX                                                           
FILTREJX LTR   RB,RB                                                            
FILTEX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*        GET DEBIT DETAILS AND PLACE IN CRBLOCK                       *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
***********************************************************************         
         USING TRNRECD,R3                                                       
GETDRDET NTR1                                                                   
         L     R3,AIO1                                                          
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         USING TRNELD,R4                                                        
         LA    R4,TRNRFST                                                       
         CLI   TRNEL,TRNELQ        TRANSACTION ELEMENT ?                        
         BNE   *+12                                                             
         TM    TRNSTAT,TRNSREV                                                  
         BO    GETDRX                                                           
*                                                                               
         MVC   DRREF,TRNKREF       TRANSACTION REFERENCE                        
         CLI   TRNKREF+MBVOFFSQ,MBVINDIQ       127+ TRANSACTIONS ?              
         BNE   GETDR20                                                          
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RE                                                        
         ICM   RE,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
         BZ    GETDR20                                                          
         SR    R0,R0                                                            
*                                                                               
GETDR10  CLI   FFTEL,0                                                          
         BE    GETDR20                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   GETDR20                                                          
         CLI   FFTTYPE,FFTTKREF    KEY REF NUMBER FOR BANK VOID ?               
         BE    GETDR15                                                          
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     GETDR10                                                          
*                                                                               
GETDR15  MVC   DRREF,FFTDATA       REFERENCE WHICH MATCHES CREDIT               
         DROP  RE                                                               
*                                                                               
GETDR20  SR    R1,R1                                                            
         IC    R1,TRNKSBR          OLD DR TRNKSBR-1 IS CR TRNKSBR               
         BCTR  R1,0                                                             
*                                                                               
         USING MPYELD,R4                                                        
         ICM   R4,15,AMPYELD       R4=A(MANUAL PAYMENT ELEMENT)                 
         BZ    GETDR30                                                          
         CLI   MPYLN,MPYLN2Q       DR MYSUB PRESENT ? (POST 1989)               
         BL    GETDR30                                                          
         IC    R1,MPYSUB                                                        
*                                                                               
GETDR30  LA    R0,CRMAXQ           R0=(MAX NUM ENTRIES IN CREDITOR BLK)         
*                                                                               
         USING CRD,R2                                                           
         L     R2,ACRBLOCK         R2=A(CREDITOR BLOCK)                         
*                                                                               
GETDR40  LA    RF,TRNRFST          RF=A(FIRST ELEMENT)                          
         CLC   CROFFICE,TRNOFFC-TRNELD(RF) MATCH ON OFFICE ?                    
         BNE   GETDR45                                                          
         CLM   R1,1,CRKSREF        MATCH ON TRANSACTION SUB REFERENCE?          
         BNE   GETDR45                                                          
         CLC   CRTDATE,TRNKDATE    MATCH ON TRANSACTION DATE ?                  
         BNE   GETDR45                                                          
         CLC   CRTREF,DRREF        MATCH ON TRANSACTION REFERENCE ?             
         BNE   GETDR45                                                          
         LA    RF,L'TRNKULC                                                     
         LA    RE,TRNKULC                                                       
*                                                                               
GETDR42  CLI   0(RE),C' '                                                       
         BH    GETDR43                                                          
         LA    RE,1(,RE)                                                        
         BCT   RF,GETDR42                                                       
         B     GETDR45                                                          
*                                                                               
GETDR43  BCTR  RF,0                                                             
         EXCLC RF,CRTKEY,0(RE)     MATCH ON CONTRA ACCOUNT ?                    
         BE    GETDR50                                                          
*                                                                               
GETDR45  LA    R2,CRLNQ(,R2)       BUMP R2 TO NEXT ENTRY                        
         BCT   R0,GETDR40                                                       
*                                                                               
         CLI   MPYLN,MPYLN2Q       USING DEBIT MPYSUB ?                         
         BNL   GETDRX                                                           
         SH    R1,=H'1'            REDUCE SUB REFERENCE AND RETRY               
         BNM   GETDR30                                                          
         B     GETDRX                                                           
         DROP  R4                                                               
*                                                                               
GETDR50  TM    CRSTAT,CRCRPAYQ     CREDIT SET PAYMENT DETAILS?                  
         BO    GETDRX                                                           
         TM    CRSTAT,CRMATCHQ     MERGED CREDITS ?                             
         BO    GETDRX                                                           
         NI    CRSTAT,TURNOFF-CROLDDRQ  CLEAR OLD DEBIT SETTING                 
*                                                                               
         USING TRSELD,R4                                                        
         L     R4,ATRSELD          R4=A(TRANSACTION STATUS ELEMENT)             
         CLC   TRSDATE,CRDRDAT     EARLIER THAN LATEST DEBIT ?                  
         BNL   GETDR52                                                          
         OI    CRSTAT,CROLDDRQ     SET OLD DEBIT FOUND                          
         B     GETDR55                                                          
*                                                                               
GETDR52  MVC   CRDRDAT,TRSDATE                                                  
         DROP  R4                                                               
*                                                                               
         USING MPYELD,R4                                                        
         ICM   R4,15,AMPYELD       R4=A(MANUAL PAYMENT ELEMENT)                 
         BZ    GETDR55                                                          
         CLI   MPYLN,MPYLN3Q       CHEQUE PAID MORE THAN ONE INVOICE ?          
         BE    GETDR53                                                          
         AP    CRDRAMT,MPYAMNT                                                  
         B     GETDR54                                                          
*                                                                               
GETDR53  AP    CRDRAMT,MPYPART                                                  
*                                                                               
GETDR54  CLC   MPYNO,SPACES                                                     
         BNH   *+10                                                             
         MVC   CRCHEQUE,MPYNO      GET  CHEQUE NUMBER                           
         OC    MPYDTE,MPYDTE                                                    
         BZ    *+10                                                             
         MVC   CRCHQDAT,MPYDTE     CHEQUE DATE                                  
         MVC   CRBANK,MPYBNK       BANK ACCOUNT                                 
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
GETDR55  L     R4,ATRSELD          R4=A(TRANSACTION STATUS ELEMENT)             
         TM    CRSTAT,CROLDDRQ     OLD DEBIT ?                                  
         BO    GETDR60                                                          
         CLI   TRSMARK,TRSMBVQ     MARKER BANK/VOID ?                           
         BE    GETDR57                                                          
         CLI   TRSMARK,TRSMSBVQ    SUBSIDIARY BANK/VOID ?                       
         BNE   GETDR60                                                          
*                                                                               
GETDR57  MVC   CRCHEQUE,SPACES                                                  
         XC    CRCHQDAT,CRCHQDAT                                                
*                                                                               
GETDR60  MVC   BYTE1,TRSMARK                                                    
         NI    BYTE1,TURNOFF-TRSMUMQ  CLEAR NEGATIVE ACTION BIT                 
         CLI   BYTE1,TRSMBVQ       VOID/UNVOID ?                                
         BE    GETDRX                                                           
         CLI   BYTE1,TRSMSBVQ      SUBSIDIARTY VOID/UNVOID                      
         BE    GETDRX                                                           
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
         LA    R4,TRNRFST                                                       
         CLI   TRNEL,TRNELQ        TRANSACTION ELEMENT ?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TRNSTAT,TRNSREV                                                  
         BO    GETDRX                                                           
*                                                                               
         OC    CRCHQDAT,CRCHQDAT                                                
         BNZ   GETDRX                                                           
         CLI   TRNTYPE,CHEQUEQ     CHEQUE ?                                     
         BNE   GETDRX                                                           
         TM    CRSTAT,CROLDDRQ     OLD DEBIT ?                                  
         BO    GETDRX                                                           
         MVC   CRCHEQUE,TRNNARR                                                 
         CLC   TRNNARR+6(7),SPACES                                              
         BNH   GETDRX                                                           
*                                                                               
         MVC   DATEFORM,LANG                                                    
         LA    R5,8                                                             
         GOTO1 VPERVAL,DMCB,((R5),TRNNARR+6),(DATEFORM,WORK)                    
         CLI   4(R1),0                                                          
         BE    GETDR70                                                          
         CLI   4(R1),PVRCONE                                                    
         BNE   GETDRX                                                           
*                                                                               
GETDR70  L     R5,4(,R1)                                                        
         MVC   CRCHQDAT,PVALCSTA-PERVALD(R5)                                    
*                                                                               
GETDRX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         USING CRD,R5                                                           
TBLDDIS  NTR1                                                                   
         L     R5,ACRBLOCK                                                      
*                                                                               
TBLD1A   L     RF,ACRBLOCK         END OF BLOCK ?                               
         LHI   RE,L'CRBLOCK-CRLNQ                                               
         AR    RF,RE                                                            
         CR    R5,RF                                                            
         BNL   TBLDX                                                            
         OC    CRTDATE,CRTDATE     END OF BLOCK ?                               
         BZ    TBLD60                                                           
*                                                                               
         OC    CRUSEDAT,CRUSEDAT   USED DATE                                    
         BNZ   TBLD1A10                                                         
*                                                                               
         TM    CRSTAT,CRREVDQ      MARKED AS A REVERSAL ?                       
         BO    TBLD1A10                                                         
*                                                                               
         TM    CRSTAT,CRVOIDQ      CHEQUE VOIDED ?                              
         BZ    TBLD1A10                                                         
         XC    CRCHEQUE,CRCHEQUE                                                
         XC    CRCHQDAT,CRCHQDAT                                                
         ZAP   CRDRAMT,=P'0'                                                    
*                                                                               
         USING OPTVALSD,R2                                                      
TBLD1A10 L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         CP    CRDISC,=P'0'                                                     
         BNE   TBLD1A20                                                         
         CLI   ODISC,C'O'                                                       
         BE    TBLD1F                                                           
         B     TBLD1A30                                                         
*                                                                               
TBLD1A20 CLI   ODISC,C'N'                                                       
         BE    TBLD1F                                                           
*                                                                               
TBLD1A30 OC    CRUSEDAT,CRUSEDAT   USED DATE                                    
         BNZ   TBLD1A40                                                         
         CP    CRDRAMT,=P'0'                                                    
         BNE   TBLD1A40                                                         
         CLI   OPAID,C'O'                                                       
         BE    TBLD1F                                                           
         B     TBLD1A50                                                         
*                                                                               
TBLD1A40 CLI   OPAID,C'N'                                                       
         BE    TBLD1F                                                           
*                                                                               
TBLD1A50 TM    CRSTAT2,CRMANCHQ    MANUAL CHEQUE ?                              
         BO    TBLD1A60                                                         
         CLI   OMAN,C'O'                                                        
         BE    TBLD1F                                                           
         B     TBLD1A70                                                         
*                                                                               
TBLD1A60 CLI   OMAN,C'N'                                                        
         BE    TBLD1F                                                           
*                                                                               
TBLD1A70 OC    OPAYDATE,OPAYDATE   FILTERING ON PAYMENT DATE ?                  
         BZ    TBLD1B                                                           
         CLI   CRBTYPE,36                                                       
         BNE   TBLD1A80                                                         
         OC    CRUSEDAT,CRUSEDAT   USED DATE                                    
         BZ    TBLD1A80                                                         
         TM    CRSTAT,CRVOIDQ      CHEQUE VOIDED ?                              
         BO    TBLD1A80                                                         
         OC    CRCHQDAT,CRCHQDAT                                                
         BZ    TBLD1F                                                           
*                                                                               
TBLD1A80 DS    0H                                                               
         CLC   CRCHQDAT,MERGE                                                   
         BE    TBLD1F                                                           
         GOTO1 VDATCON,DMCB,(2,CRCHQDAT),(1,TEMPDAT)                            
*                                                                               
TBLD1A90 GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OPAYDST,OPAYDEN,OPAYDFI          
         BNE   TBLD1F                                                           
*                                                                               
TBLD1B   DS    0H                                                               
         OC    OPAYREF,OPAYREF                                                  
         BZ    TBLD1D                                                           
         MVC   WORK,CRCHEQUE                                                    
         TM    CRSTAT2,CRCONCRQ    CONTRA'D CREDIT ?                            
         BZ    *+10                NO,  SKIP                                    
         OC    WORK(L'MX@CTR),SPACES                                            
         GOTO1 ASCOMP,DMCB,WORK,(OPAYRLN1,OPAYRVL1),(OPAYRLN2,         X        
               OPAYRVL2),OPAYRFI                                                
         BNE   TBLD1F                                                           
*                                                                               
TBLD1D   CLI   ORECON,0            ANY RECONCILED FILTER ?                      
         BZ    TBLD1H                                                           
         CLI   ORECON,C'Y'         SAME AS DEFAULT SETING ?                     
         BE    TBLD1H                                                           
         CLI   OMERGE,C'Y'         IF MERGING ALREADY FILTERED TRAN             
         BE    TBLD1H                                                           
         GOTO1 RECFILT,DMCB,CRD    APPLY RECONCILED FILT TO CRD ENTRY           
         BNE   TBLD1F                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    TBLDX                                                            
         B     TBLD1H                                                           
         DROP  R2                                                               
*                                                                               
TBLD1F   LR    R0,R5                                                            
         LHI   R1,CRLNQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     TBLD60                                                           
*                                                                               
TBLD1H   L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TSARRECD,R2                                                      
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         MVC   TSARKYNO,TSCURRNO                                                
*                                                                               
         USING TSARDATD,R4                                                      
         LA    R4,TSARDATA                                                      
         ZAP   TSDDRBFW,DEBTOT     NET DEBITS                                   
         ZAP   TSDDR,=P'0'         DEBIT AMOUNT FOR ITEM                        
         OC    CRCHQDAT,CRCHQDAT   ITEM PAID/CONTR'D ?                          
         BZ    TBLDF70                                                          
         TM    CRSTAT,CRREVDQ      MARKED AS REVERSAL ?                         
         BO    TBLDF70             YES, NOT A DEBIT AMOUNT                      
*                                                                               
         AP    DEBTOT,CRNET                                                     
         AP    TSDDR,CRNET         DEBIT AMOUNT FOR ITEM                        
*                                                                               
TBLDF70  ZAP   TSDCRBFW,CRETOT     TOTAL CREDITS                                
         AP    CRETOT,CRCRAMT                                                   
         ZAP   TSDCR,CRCRAMT       CREDIT AMOUNT FOR ITEM                       
         ZAP   TSDDIBFW,DISTOT     DISCOUNT CARRIED FORWARD                     
         AP    DISTOT,CRDISC                                                    
         ZAP   TSDDI,CRDISC        DISCOUNT AMOUNT FOR ITEM                     
*                                                                               
         LA    R2,TSDDAT                                                        
         USING TSDCOLD,R2                                                       
*                                                                               
         LA    RF,BLOCHR           RF=A(COLUMN CHARACTER LIST)                  
*                                                                               
TBLD1    CLI   0(RF),EOT                                                        
         BE    TBLD30                                                           
*                                                                               
         USING CATRTABD,R3                                                      
         L     R3,ACATRTAB        R3=A(COLUMN ATTRIBUTE TABLE)                  
*                                                                               
TBLD5    CLI   CATRCHR,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CATRCHR,0(RF)                                                    
         BE    TBLD6                                                            
         LA    R3,CATRLNQ(,R3)                                                  
         B     TBLD5                                                            
*                                                                               
*---------------------*                                                         
* CREATE TSAR ELEMENT *                                                         
*---------------------*                                                         
TBLD6    MVC   TSDCTYP,CATRCHR     ELEMENT TYPE                                 
         SR    RE,RE                                                            
         ICM   RE,3,CATRDISP                                                    
         LR    R1,R5                                                            
         AR    R1,RE               R1=A(ACTUAL DATA)                            
         SR    RE,RE                                                            
         ICM   RE,1,CATRWID        RE=(WIDTH OF DATA)                           
         BNZ   TBLD7               VARIABLE LENGTH                              
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          ASSUME LENGTH PASSED AS 1ST BYTE             
         BZ    TBLD10                                                           
         LA    R1,1(,R1)           BUMP TO START OF ACTUAL DATA                 
*                                                                               
TBLD7    BCTR  RE,0                                                             
         TM    CATRSTA1,CATRSNLQ   NULL ENTRIES INCLUDED ?                      
         BO    TBLD8                                                            
         EXCLC RE,0(R1),SPACES     CHARACTER DATA ?                             
         BH    TBLD8               YES, GENERATE THIS ELEMENT                   
         BNE   TBLD7A              NOT  BLANK CONTINUE                          
         TM    CATRDTYP,CATRMOVQ   STRAIGHT MOVE ?                              
         BO    TBLD10              YES, SKIP THIS ENTRY                         
         TM    CATRSTA1,CATRSKPB   SKIP IF BLANK ?                              
         BO    TBLD10              YES, SKIP THIS ENTRY                         
*                                                                               
TBLD7A   EXOC  RE,0(R1),0(R1)      NULL DATA ?                                  
         BNZ   TBLD7C              NO,  CONTINUE                                
         TM    CATRSTA1,CATRSDI0   PRINT EVEN THOUGH ZERO ?                     
         BZ    TBLD7B              NO,  SKIP THIS ENTRY                         
*                                                                               
         CLI   CATRCHR,COLTSTAQ    TRANSACTION STATUS COLUMN ?                  
         BNE   *+8                 NO,  SKIP                                    
         OI    TSDZEROS,TSDZTXST   SAY  TRANSACTION STATUS IS ZERO              
*                                                                               
         CLI   CATRCHR,COLTSRFQ    TRANSACTION SUB-REFERENCE COLUMN ?           
         BNE   *+8                 NO,  SKIP                                    
         OI    TSDZEROS,TSDZTXSR   SAY  TRANSACTION SUB-REFERENCE ZERO          
*                                                                               
TBLD7B   B     TBLD10              SKIP THIS ENTRY                              
*                                                                               
TBLD7C   TM    CATRDTYP,CATRPAMQ   STD  6 BYTE PACKED FLD, ZERO=BLANK ?         
         BZ    TBLD7D              NO,  CONTINUE                                
         CP    0(6,R1),=P'0'       IS   IT ZERO ?                               
         BE    TBLD10              YES, SKIP THIS FIELD                         
         B     TBLD8               OUTPUT THE DATA                              
*                                                                               
TBLD7D   TM    CATRDTYP,CATRPAZQ   STD  6 BYTE PACKED FLD, ZERO=ZERO  ?         
         BZ    TBLD7E              NO,  CONTINUE                                
         CLI   CATRCHR,COLGSTQ     GST  ENTRY ?                                 
         BNE   TBLD8               NO,  OUTPUT THE DATA                         
         CP    0(6,R1),=P'0'       IS   IT ZERO ?                               
         BNE   TBLD8               NO,  OUTPUT THE DATA                         
         OI    TSDZEROS,TSDZGSTQ   SAY  GST IS ZERO                             
         B     TBLD10              SKIP THIS FIELD                              
*                                                                               
TBLD7E   CLI   CATRCHR,COLPSTQ     PST  ENTRY ?                                 
         BNE   TBLD8               NO,  OUTPUT THE DATA                         
*                                  PST  TAX CODE BLANK ?                        
         CLC   0(L'SCISUBPR,R1),SPACES                                          
         BH    TBLD8               NO,  OUTPUT THE DATA                         
*                                  AMOUNT ALSO ZERO ?                           
         CP    L'SCISUBPR(L'SCIAMNT,R1),=P'0'                                   
         BNE   TBLD8               NO,  OUTPUT THE DATA                         
         OI    TSDZEROS,TSDZPSTQ   SAY  PST IS ZERO                             
         B     TBLD10              SKIP THIS FIELD                              
*                                                                               
TBLD8    EXMVC RE,TSDCDAT,0(R1)    GET DATA                                     
         LA    RE,L'TSDCTYP+L'TSDCLEN+1(RE)                                     
         STC   RE,TSDCLEN                                                       
         LA    R2,0(RE,R2)                                                      
*                                                                               
TBLD10   LA    RF,1(,RF)                                                        
         B     TBLD1                                                            
         DROP  R3,R4                                                            
*                                                                               
TBLD30   MVI   TSDCTYP,EOR                                                      
         LA    R2,1(,R2)                                                        
*                                                                               
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC         R3=A(TSAR RECORD)                            
         LR    RF,R3                                                            
         SR    R2,RF                                                            
         STCM  R2,3,TSARLEN                                                     
*                                                                               
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR ONTO DUMMIES FOR GRIDS           
         MVC   TSARLINE,LINSUSED                                                
         DROP  R3                                                               
*                                                                               
         LR    R0,R5                                                            
         LHI   R1,CRLNQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    TBLD36               YES, THEN DON'T USE TSAR                    
         L     R0,ASVTSAR           STORE TSAR RECORD                           
         L     RE,ATSARREC                                                      
         LHI   R1,TSARRECL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     TBLD40               YES, THEN DON'T USE TSAR                    
*                                                                               
TBLD36   GOTO1 ATSARADD            ADD RECORD                                   
         BE    TBLD40                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    TBLD40                                                           
*                                  TOO MANY ITEMS, TRY DIS= OR MERGE=Y          
         MVC   FVMSGNO,=AL2(EA2MNYVE)                                           
         TM    DISPFLAG,TSARFULQ                                                
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    ENQFLAG,TFULL1ST    FIRST TIME TSAR FULL                         
*                                                                               
TBLD40   CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD ?          
         BL    TBLD50                                                           
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    TBLD45                                                           
         OI    CRFLAG,CRSFULLQ     SCREEN IS FULL                               
         B     TBLD50                                                           
*                                                                               
TBLD45   TM    DISPFLAG,TSARFULQ                                                
         BO    TBLD70                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
*                                                                               
TBLD50   MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(,RF)                                                        
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
TBLD60   LA    R5,CRLNQ(,R5)                                                    
         TM    DISPFLAG,DISIOMAX                                                
         BO    TBLDERRX                                                         
         TM    CRFLAG,CRSFULLQ                                                  
         BZ    TBLD80                                                           
*                                                                               
TBLD70   BAS   RE,TOTLINE                                                       
         B     TBLDERRX                                                         
*                                                                               
TBLD80   TM    DISPFLAG,TSARFULQ                                                
         BO    TBLDERRX                                                         
         B     TBLD1A                                                           
*                                                                               
TBLDERRX LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     TBLDEX                                                           
*                                                                               
TBLDX    CR    RB,RB                                                            
*                                                                               
TBLDEX   B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        RECONCILED FILTER                                            *         
* ON ENTRY R2=A(CREDITOR TABLE ENTRY)                                 *         
* ON EXIT CC NOT EQUAL MEANS REJECT THE CREDITOR ENTRY                *         
***********************************************************************         
         USING OPTVALSD,R4                                                      
RECFILT  NTR1                                                                   
         L     R4,AOPTVALS                                                      
         MVI   BYTE1,0                                                          
         XC    FULL,FULL                                                        
         XC    RECVALS(RECVLLNQ),RECVALS                                        
         ICM   RF,15,0(R1)         FILTER TRANSACTION OR CRD ENTRY ?            
         BNZ   RECF02                                                           
*                                                                               
*-------------------------------------*                                         
* GET DETAILS FROM VENDOR TRANSACTION *                                         
*-------------------------------------*                                         
         USING TRNRECD,R3                                                       
         L     R3,AIO1             R3=A(VENDOR TRANSACTION)                     
         GOTO1 ASETELE,TRNRFST     SET VENDOR ELEMENT ADDRESSES                 
*                                                                               
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNELD       R2=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RECOFF,TRNOFFC      OFFICE CODE                                  
*                                                                               
         USING MPYELD,R2                                                        
         ICM   R2,15,AMPYELD       R2=A(MANUAL PAYMENT ELEMENT)                 
         BZ    RECF40                                                           
         MVC   RECNO,MPYNO         PAYMENT NUMBER                               
         MVC   RECDATE,MPYDTE      PAYMENT DATE                                 
         MVC   RECBANK,MPYBNK      BANK ACCOUNT                                 
         B     RECF05                                                           
         DROP  R2                                                               
*                                                                               
*---------------------------------*                                             
* GET DETAILS FROM CREDITOR TABLE *                                             
*---------------------------------*                                             
         USING CRD,RF                                                           
RECF02   ST    RF,FULL             A(CRD TABLE ENTRY)                           
         MVC   RECOFF,CROFFICE     OFFICE                                       
         MVC   RECNO,CRCHEQUE      PAYMENT NUMBER                               
         MVC   RECDATE,CRCHQDAT    PAYMENT DATE                                 
         MVC   RECBANK,CRBANK      BANK ACCOUNT                                 
         DROP  RF                                                               
*                                                                               
RECF05   CLC   RECNO,SPACES        NO PAYMENT DETAILS SO NOT RECONCILED         
         BNH   RECF40                                                           
         LA    R3,IOKEY            R3=A(IOAREA 2)                               
         MVC   IOKEY,SPACES                                                     
         MVC   TRNKCPY,MYCO                                                     
         CLC   SBNKUL,RECBANK      BAD DATA MPYBNK CONTAINS COMPANY             
         BE    RECF07                                                           
         MVC   TRNKULA(L'TRNKULA-1),RECBANK+1                                   
         B     RECF08                                                           
*                                                                               
RECF07   MVC   TRNKULA,RECBANK     BANK UNIT/LEDGER/ACCOUNT                     
*                                                                               
RECF08   MVC   TRNKCCPY,MYCO       COMPANY CODE                                 
         SR    RF,RF                                                            
         IC    RF,BASKEYH+FLDILEN-FLDHDRD                                       
         BCTR  RF,0                                                             
         EXMVC RF,TRNKULC,BASKEY   CONTRA                                       
         GOTO1 VDATCON,DMCB,(2,RECDATE),(1,TRNKDATE) CHEQUE DATE                
         MVC   TRNKREF,RECNO       CHEQUE NUMBER                                
         MVI   TRNKSBR,0           FIRST TRANSACTION                            
         GOTO1 AIO,IOREAD+IOACCDIR+IO2 READ BANK TRANSACTION                    
         BE    RECF10                                                           
         TM    IOERR,IOMAX                                                      
         BO    RECF60                                                           
         CLI   IOERR,IOERNF                                                     
         BE    RECF25                                                           
*                                                                               
RECF10   GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    RECF12                                                           
         CLI   IOERR,IOMAX                                                      
         BE    RECF60                                                           
         DC    H'0'                                                             
*                                                                               
RECF12   L     R3,AIO2                                                          
         LA    R3,TRNRFST          R3=A(FIRST TRANSACTION)                      
         XR    R0,R0                                                            
*                                                                               
         USING TRSELD,R3                                                        
RECF20   CLI   TRSEL,0             EOR ?                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TRSEL,TRSELQ        TRANSACTION STATUS ELEMENT ?                 
         BE    RECF21                                                           
         IC    R0,TRSLN                                                         
         AR    R3,R0                                                            
         B     RECF20                                                           
*                                                                               
RECF21   TM    TRSSTAT,TRSSVOID    VOIDED ITEM ?                                
         BZ    RECF30                                                           
         GOTO1 AIO,IOSEQ+IOACCDIR+IO2 READ BANK TRANSACTION                     
         BE    RECF22                                                           
         CLI   IOERR,IOMAX                                                      
         BE    RECF60                                                           
*                                                                               
RECF22   CLC   IOKEY(TRNKSBR-TRNKEY),IOKEYSAV                                   
         BE    RECF10                                                           
*                                                                               
RECF25   MVI   BYTE1,X'FF'                                                      
*                                                                               
RECF30   OC    FULL,FULL           CRD CALL ?                                   
         BNZ   RECF33                                                           
         L     RF,AIO1             RE-ESTABLISH VENDOR TRANSACTION SEQ          
         MVC   IOKEY,0(RF)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 RESTORE VENDOR SEQUENCE                  
         BE    RECF32                                                           
         CLI   IOERR,IOMAX                                                      
         BE    RECF60                                                           
         DC    H'0'                                                             
*                                                                               
RECF32   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    RECF33                                                           
         CLI   IOERR,IOMAX                                                      
         BE    RECF60                                                           
         DC    H'0'                                                             
*                                                                               
RECF33   CLI   BYTE1,0                                                          
         BNE   RECF40                                                           
         L     R3,AIO2             R3=A(CHEQUE TRANSACTION)                     
*                                                                               
         USING TRNELD,R3                                                        
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TRNSTAT,TRNSBREC    RECONCILED ?                                 
         BZ    RECF40                                                           
         CLI   ORECON,C'N'         REJECT RECONCILED ITEMS ?                    
         BE    RECFREJX                                                         
         B     RECFX                                                            
*                                                                               
RECF40   CLI   ORECON,C'O'         REJECT UNRECONCILED ITEMS ?                  
         BE    RECFREJX                                                         
         B     RECFX                                                            
*                                                                               
RECF60   OI    DISPFLAG,DISIOMAX   MAX IO                                       
         B     RECFX                                                            
*                                                                               
RECFX    CR    RB,RB                                                            
         B     RECFEX                                                           
*                                                                               
RECFREJX LTR   RB,RB                                                            
*                                                                               
RECFEX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         XC    ATSADDR(ATSLNQ),ATSADDR                                          
         MVI   LINSUSED,1          NUMBER OF LINES USED                         
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TSARRECD,R2         MAP  TSAR RECORD                             
         USING TSARDATD,R4         MAP  TSAR DATA                               
FORM10   L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         LA    R4,TSARDATA         R4=A(TSAR DATA)                              
*                                                                               
         CLI   TSDZEROS,0          ANY SPECIAL CASES OF VALUE ZERO ?            
         BE    FORM35              NO, SKIP                                     
*                                                                               
         TM    TSDZEROS,TSDZTXST   TRANSACTION ELEMENT STATUS = 0 ?             
         BZ    FORM20              NO, SKIP                                     
         LA    RF,TXSTAT0          ->  CONSTANT FOR TX EL STATUS = 0            
         ST    RF,ATSTEST          SAVE A(DATA ELEMENT IN TSAR)                 
*                                                                               
FORM20   TM    TSDZEROS,TSDZTXSR   TRANSACTION SUB-REFERENCE = 0 ?              
         BZ    FORM25              NO, SKIP                                     
         LA    RF,TXSREF0          ->  CONSTANT FOR TX SUB-REF # = 0            
         ST    RF,ATSKSREF         SAVE A(DATA ELEMENT IN TSAR)                 
*                                                                               
FORM25   TM    TSDZEROS,TSDZGSTQ   GST = 0 ?                                    
         BZ    FORM30              NO, SKIP                                     
         LA    RF,TXGST0           ->  CONSTANT FOR GST = 0                     
         ST    RF,ATSGST           SAVE A(DATA ELEMENT IN TSAR)                 
*                                                                               
FORM30   TM    TSDZEROS,TSDZPSTQ   PST = 0 ?                                    
         BZ    FORM35              NO, SKIP                                     
         LA    RF,TXPST0           ->  CONSTANT FOR PST = 0                     
         ST    RF,ATSPST           SAVE A(DATA ELEMENT IN TSAR)                 
*                                                                               
         USING TSDCOLD,R2          MAP  TSAR COLUMN ELEMENTS                    
FORM35   LA    R2,TSDDAT           R2=A(TSAR COLUMN DATA)                       
         DROP  R4                                                               
*                                                                               
FORM40   CLI   TSDCTYP,EOR         END OF RECORD                                
         BE    FORM55                                                           
*                                                                               
         USING CATRTABD,R3                                                      
         L     R3,ACATRTAB         R3=A(COLUMN ATTRIBUTE TABLE)                 
*                                                                               
FORM45   CLI   CATRCHR,EOT         END OF TABLE ?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CATRCHR,TSDCTYP     MATCH ON COLUMN TYPE ?                       
         BE    FORM50                                                           
         LA    R3,CATRLNQ(,R3)                                                  
         B     FORM45                                                           
*                                                                               
FORM50   SR    RF,RF                                                            
         ICM   RF,3,CATRTSAD                                                    
         LA    RF,ATSADDR(RF)      RF=A(DISPLACEMENT TO TSAR DATA)              
         ST    R2,0(,RF)           SAVE ADDRESS OF TSAR DATA ELEMENT            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TSDCLEN          RF=A(LENGTH OF TSAR ELEMENT)                 
         AR    R2,RF                                                            
         B     FORM40                                                           
         DROP  R2                                                               
*                                                                               
FORM55   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    XIT                  YES                                         
         BRAS  RE,CONDAT           CONVERT DATA TO PRINT FORM                   
*                                                                               
FORMX    L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OEXP-OPTVALSD(RF),C'Y'                                           
         BE    *+8                                                              
         MVI   LINSUSED,1                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS              *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM02   MVI   LINSUSED,0               NUMBER OF LINES USED                    
         MVI   DISATRIB,0               DISPLAY ATTRIBUTES                      
         L     R0,ADUMLINE              CLEAR DUMMY SCREEN LINES                
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    DETFLAG,DETFENDQ                                                 
         BO    FGRM120                                                          
*                                                                               
         TM    OVRSTAT,OVRGINIT         ALREADY INITIALIZED?                    
         BO    FGRM110                   NO, THEN DO IT                         
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         B     FGRM02                                                           
*                                                                               
FGRM110  GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM120  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPLAY TOTAL LINE                                                  *         
***********************************************************************         
TOTLINE  NTR1                                                                   
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    TOTLX                YES                                         
         GOTO1 ASCRNCLR,DISLINE                                                 
         SR    RF,RF                                                            
         IC    RF,DISEND                                                        
         LA    RF,5(,RF)                                                        
         STC   RF,DISLINE          UPDATE DISPLAY END LINE COUNT                
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    TOTLX                YES                                         
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,ENQDATLH                                                      
         XC    FLDDATA(L'ENQDATL),FLDDATA                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
         TM    CURRFLAG,CURRSRTQ   SUPPRESS TOTALS ?                            
         BO    TOTLX                                                            
         TM    ENQFLAG,TFULL1ST    1ST  TIME TSAR FULL ?                        
         BO    TOTLX               YES, SKIP TOTALS                             
         LA    R2,FLDDATA                                                       
         DROP  R2                                                               
*                                                                               
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC         R3=A(TSAR RECORD)                            
         OC    TSARKYNO,TSARKYNO   ANY TSAR RECORD ?                            
         BNZ   TOTL03              YES, CONTINUE                                
         OC    TSLSTLIN,TSLSTLIN   ANY TSAR RECORDS ?                           
         BZ    TOTL05                                                           
         GOTO1 ATSARGET,TSLSTLIN   GET THE LAST TSAR RECORD                     
*                                                                               
TOTL03   LA    R3,TSARDATA                                                      
         USING TSARDATD,R3                                                      
         TM    DISPFLAG,NORECQ+TSARFULQ                                         
*        TM    DISPFLAG,NORECQ                                                  
         BNZ   TOTL05                                                           
         ZAP   DEBITS,TSDDRBFW                                                  
         ZAP   CREDITS,TSDCRBFW                                                 
         ZAP   DISCOUNT,TSDDIBFW                                                
         CLC   TSLSTLIN,TSLSTREC                                                
         BNE   TOTL10                                                           
         AP    DEBITS,TSDDR        NET DEBITS FOR ITEM                          
         AP    CREDITS,TSDCR       CREDITS FOR ITEM                             
         AP    DISCOUNT,TSDDI      DISCOUNT FOR ITEM                            
         B     TOTL10                                                           
         DROP  R3                                                               
*                                                                               
TOTL05   ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
         ZAP   DISCOUNT,=P'0'                                                   
*                                                                               
TOTL10   DS    0H                                                               
*                                                                               
TOTL11   MVC   0(L'MX@INVS,R2),MX@INVS                                          
         LA    R2,L'MX@INVS-1(,R2)                                              
*                                                                               
TOTL11A  CLI   0(R2),C' '                                                       
         BH    TOTL11B                                                          
         BCT   R2,TOTL11A                                                       
*                                                                               
TOTL11B  MVI   1(R2),C'='                                                       
         CLI   CURFLAG,CURSET                                                   
         BNE   TOTL12                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,CREDITS),(12,2(R2)),(RF),ALIGN=LEFT,MINUS=YES                
         B     TOTL14                                                           
*                                                                               
TOTL12   CURED (P8,CREDITS),(12,2(R2)),2,ALIGN=LEFT,MINUS=YES                   
*                                                                               
TOTL14   AR    R2,R0                                                            
         LA    R2,3(,R2)                                                        
         CLC   SFOLUL,BASKEY       FOLIO LEDGER ?                               
         BE    TOTL20                                                           
         MVC   0(L'MX@DISS,R2),MX@DISS                                          
         LA    R2,L'MX@DISS-1(,R2)                                              
*                                                                               
TOTL15   CLI   0(R2),C' '                                                       
         BH    TOTL15A                                                          
         BCT   R2,TOTL15                                                        
*                                                                               
TOTL15A  MVI   1(R2),C'='                                                       
         CLI   CURFLAG,CURSET                                                   
         BNE   TOTL16                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,DISCOUNT),(12,2(R2)),(RF),ALIGN=LEFT,MINUS=YES               
         B     TOTL18                                                           
*                                                                               
TOTL16   CURED (P8,DISCOUNT),(12,2(R2)),2,ALIGN=LEFT,MINUS=YES                  
*                                                                               
TOTL18   AR    R2,R0                                                            
         LA    R2,3(,R2)                                                        
*                                                                               
TOTL20   MVC   0(L'MX@NETPD,R2),MX@NETPD                                        
         LA    R2,L'MX@NETPD-1(,R2)                                             
*                                                                               
TOTL21   CLI   0(R2),C' '                                                       
         BH    TOTL21A                                                          
         BCT   R2,TOTL21                                                        
*                                                                               
TOTL21A  MVI   1(R2),C'='                                                       
         CLI   CURFLAG,CURSET                                                   
         BNE   TOTL22                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,DEBITS),(12,2(R2)),(RF),ALIGN=LEFT,MINUS=YES                 
         B     TOTL24                                                           
*                                                                               
TOTL22   CURED (P8,DEBITS),(12,2(R2)),2,ALIGN=LEFT,MINUS=YES                    
*                                                                               
TOTL24   AR    R2,R0                                                            
         LA    R2,3(,R2)                                                        
         MVC   0(L'MX@BAL,R2),MX@BAL                                            
         LA    R2,L'MX@BAL-1(,R2)                                               
*                                                                               
TOTL25   CLI   0(R2),C' '                                                       
         BH    TOTL25A                                                          
         BCT   R2,TOTL25                                                        
*                                                                               
TOTL25A  MVI   1(R2),C'='                                                       
         ZAP   BALANCE,CREDITS                                                  
         SP    BALANCE,DEBITS                                                   
         CLI   CURFLAG,CURSET                                                   
         BNE   TOTL26                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,BALANCE),(12,2(R2)),(RF),ALIGN=LEFT,MINUS=YES                
         B     TOTLX                                                            
*                                                                               
TOTL26   CURED (P8,BALANCE),(12,2(R2)),2,ALIGN=LEFT,MINUS=YES                   
*                                                                               
TOTLX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
NINES    DC    C'99999999999999999999'                                          
UNDERLIN DC    C'-----------'                                                   
SI       DC    C'SI'                                                            
SR       DC    C'SR'                                                            
MERGE    DC    C'********************'                                          
*                                                                               
TXSTAT0  DC    AL1(COLTSTAQ,3,0)   TRANSACTION STATUS       = X'00'             
TXSREF0  DC    AL1(COLTSRFQ,3,0)   TRANSACTION SUBREFERENCE = X'00'             
*                                  GST VALUE                = P'0'              
TXGST0   DC    AL1(COLGSTQ,8),PL6'0'                                            
*                                  PST VALUE                = P'0'              
TXPST0   DC    AL1(COLPSTQ,10),CL2'  ',PL6'0'                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,R6,R7                                                         
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*      CONVERT TSAR RECORD DATA TO TEXT OUTPUT                        *         
*                                                                     *         
*         ON INPUT:                                                   *         
*             R2 = ADDRESS OF TSAR COLUMN ELEMENT                     *         
*             R3 = ADDRESS OF COLUMN ATTRIBUTE ELEMENT OR GRID TAB    *         
*             R4 = ADDRESS OF COLUMN DISPLAY AREA                     *         
***********************************************************************         
CONDAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PCDRIVEN,PCGRIDQ           TEST RUNNING UNDER GRID?              
         BZ    COND50                     . YES                                 
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)               R8=A(LOCAL WORKING STORAGE)           
         XC    TEMP,TEMP                                                        
         USING GCTBLD,R3                                                        
         SR    R0,R0                                                            
         IC    R0,GCTELSCD                # OF COL ATTRIBUTE ENTRY              
         MHI   R0,CATRLNQ                                                       
         L     R3,ACATRTAB                                                      
         AR    R3,R0                      POINT TO COL ATTR ENTRY               
         B     COND60                                                           
         DROP  R3                                                               
*                                                                               
         USING CATRTABD,R3                COLUMN ATTRIBUTE TABLE                
COND50   L     R3,ACATRTAB                                                      
*                                                                               
COND60   CLI   CATRCHR,EOT                END OF TABLE ?                        
         BE    CONDX                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CATRTSAD                                                    
         LA    RF,ATSADDR(RF)             RF=A(TSAR ELEMENT ADDRESS)            
*                                                                               
         USING TSDCOLD,R2                                                       
         ICM   R2,15,0(RF)                R2=A(TSAR COLUMN DATA ELEM)           
         BZ    COND200                                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ           TEST RUNNING UNDER GRID?              
         BO    COND64                     . YES                                 
         SR    RE,RE                                                            
         ICM   RE,3,CATRDLIN              RE=(DSP TO DSP ON DSPLY LINE)         
         LA    RE,BLDSDSP(RE)             RE=(DSPLCMNT ON DISPLAY LINE)         
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                 RF=(DSP TO DSP ON DSPLY LINE)         
         BNZ   COND65                                                           
COND64   LA    RF,TEMP                                                          
         B     COND70                                                           
*                                                                               
COND65   BCTR  RF,0                                                             
         L     RE,ADUMLINE                                                      
         AR    RF,RE                      RF=A(DISPLAY POSITION)                
*                                                                               
COND70   SR    R0,R0                                                            
         ICM   R0,3,CATRFROU              FORMAT ROUTINE                        
         BZ    COND75                                                           
         LR    R4,RF                                                            
         LR    RF,R0                                                            
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         B     COND200                                                          
*                                                                               
COND75   CLI   CATRDTYP,CATRMOVQ          STRAIGHT MOVE ?                       
         BE    COND80                                                           
         CLI   CATRDTYP,CATRPDTQ          PACKED DATE ?                         
         BE    COND90                                                           
         CLI   CATRDTYP,CATRCDTQ          COMPRESSED DATE ?                     
         BE    COND95                                                           
         CLI   CATRDTYP,CATRMDTQ          MMMYY TYPE DATE ?                     
         BE    COND115                                                          
         CLI   CATRDTYP,CATRPAMQ          PACKED AMT(6 BYTES) ZERO=BLNK         
         BE    COND120                                                          
         CLI   CATRDTYP,CATRHEXQ          HEX OUTPUT                            
         BE    COND130                                                          
         CLI   CATRDTYP,CATRPAZQ          PACKED AMT(6 BYTES) ZERO=ZERO         
         BE    COND135                                                          
         CLI   CATRDTYP,CATRBIZQ          BIN AMT(1-4 BYTES) ZERO=ZERO          
         BE    COND150                                                          
         B     COND200                                                          
*-----------------------------------------                                      
* STRAIGHT MOVE                                                                 
*-----------------------------------------                                      
COND80   LR    R4,RF                      SAVE OUTPUT ADDRESS                   
         SR    R1,R1                                                            
         ICM   R1,1,TSDCLEN               COULD BE VARIABLE                     
         SHI   R1,TSDCDAT-TSDCOLD                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    CATRSTA1,CATRSGWD          NEED TO GET OUTPUT WIDTH?             
         BZ    COND85                     . NO, SKIP                            
         BAS   RE,CONCOL                  GET OUTPUT WIDTH                      
         CR    R1,R0                      GOT MORE DATA THAN REQUESTED?         
         BNH   COND85                     . NO, USE THE SHORTER AMOUNT          
         LR    R1,R0                      USE THE OUTPUT WIDTH                  
*                                                                               
COND85   BCTR  R1,0                                                             
         EXMVC R1,0(R4),TSDCDAT                                                 
         B     COND200                                                          
*-----------------------------------------                                      
* PACKED DATE                                                                   
*-----------------------------------------                                      
COND90   OC    TSDCDAT(L'TRNKDATE),TSDCDAT                                      
         BZ    COND200                                                          
         GOTO1 VDATCON,DMCB,(1,TSDCDAT),(17,(RF))                               
         B     COND200                                                          
*-----------------------------------------                                      
* COMPRESSED DATE                                                               
*-----------------------------------------                                      
COND95   OC    TSDCDAT(L'TRSUDAT),TSDCDAT                                       
         BZ    COND200                                                          
         CLC   TSDCDAT(2),MERGE1                                                
         BNE   COND110                                                          
         MVC   0(8,RF),MERGE1                                                   
         LA    RE,L'MX@MRGED-1                                                  
*                                                                               
COND100  LA    R1,MX@MRGED(RE)                                                  
         CLI   0(R1),C' '                                                       
         BH    COND105                                                          
         BCT   RE,COND100                                                       
         DC    H'0'                                                             
*                                                                               
COND105  LA    R0,8                                                             
         LA    R1,2(,RE)                                                        
         CR    R1,R0                                                            
         BH    CONDX                                                            
         SR    R0,RE                                                            
         SRA   R0,1                                                             
         AR    RF,R0                                                            
         EXMVC RE,0(RF),MX@MRGED                                                
         B     COND200                                                          
*                                                                               
COND110  GOTO1 VDATCON,DMCB,(2,TSDCDAT),(17,(RF))                               
         B     COND200                                                          
*-----------------------------------------                                      
* MMMYY TYPE DATE                                                               
*-----------------------------------------                                      
COND115  OC    TSDCDAT(L'TRNRSMOS),TSDCDAT                                      
         BZ    COND200                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TRNRSMOS),TSDCDAT                                          
         GOTO1 VDATCON,DMCB,(1,DUB),(9,(RF))                                    
         B     COND200                                                          
*-----------------------------------------                                      
* PACKED AMT (6 BYTES) ZERO=BLNK                                                
*-----------------------------------------                                      
COND120  MVI   CURFLAG,0                                                        
         L     RE,ATSARREC                RE=A(TSAR RECORD)                     
         LA    RE,TSARDATA-TSARRECD(,RE)                                        
         LR    R4,RF                                                            
*                                                                               
         MVI   CURFLAG,CURNA                                                    
         LA    R0,13                      DEFAULT OUTPUT WIDTH                  
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    COND124                                                          
         CURED (P6,TSDCDAT),((R0),(R4)),2,FLOAT=-,ALIGN=LEFT,          +        
               ZERO=BLANK,COMMAS=YES                                            
         B     COND200                                                          
*                                                                               
COND124  TM    CATRSTA1,CATRSGWD          NEED TO GET OUTPUT WIDTH?             
         BZ    *+8                        . NO, SKIP                            
         BAS   RE,CONCOL                  GET OUTPUT WIDTH                      
         CURED (P6,TSDCDAT),((R0),(R4)),2,MINUS=YES,ZERO=BLANK                  
         B     COND200                                                          
*-----------------------------------------                                      
* HEX OUTPUT                                                                    
*-----------------------------------------                                      
COND130  SR    R4,R4                                                            
         IC    R4,CATRWID-CATRTABD(R3)                                          
         GOTO1 VHEXOUT,DMCB,TSDCDAT,(RF),(R4),1,=C'MIX'                         
         B     COND200                                                          
*-----------------------------------------                                      
* PCKED AMT (6 BYTES) ZERO=ZERO                                                 
*-----------------------------------------                                      
COND135  MVI   CURFLAG,0                                                        
         L     RE,ATSARREC                RE=A(TSAR RECORD)                     
         LA    RE,TSARDATA-TSARRECD(,RE)                                        
         LR    R4,RF                                                            
*                                                                               
         MVI   CURFLAG,CURNA                                                    
         LA    R0,13                      DEFAULT OUTPUT WIDTH                  
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    COND138                                                          
         CURED (P6,TSDCDAT),((R0),(R4)),2,FLOAT=-,ALIGN=LEFT,COMMAS=Y           
         B     COND200                                                          
COND138  TM    CATRSTA1,CATRSGWD          NEED TO GET OUTPUT WIDTH?             
         BZ    *+8                        . NO, SKIP                            
         BAS   RE,CONCOL                  GET OUTPUT WIDTH                      
         CURED (P6,TSDCDAT),((R0),(R4)),2,MINUS=YES                             
         B     COND200                                                          
*-----------------------------------------                                      
* BINARY DISPLAY                                                                
*-----------------------------------------                                      
         USING CURPARMD,R1                MAP PARAMETER AREA                    
COND150  LA    R1,DMCB                    -> PARAMETER AREA                     
         XC    DMCB(CURPARML),DMCB        CLEAR PARAMETER AREA                  
         OI    CURPINPT,CURPISET+4        SAY BINARY INPUT VALUE,LEN=4          
         OI    CURPEDT1,CURPSYMN          CURSYMB=NO                            
*                                                                               
         TM    CATRSTA1,CATRSMTR          TRAILING MINUS SIGN ?                 
         BZ    *+8                        . NO, SKIP                            
         OI    CURPEDT1,CURPMINY          MINUS=YES                             
*                                                                               
         TM    CATRSTA1,CATRSMLE          LEADING MINUS SIGN?                   
         BZ    *+8                        . NO, SKIP                            
         OI    CURPEDT2,CURPFLON          FLOAT=-                               
*                                                                               
         TM    CATRSTA1,CATRSDI0          ZERO EQUALS ZERO                      
         BO    *+8                        YES, SKIP                             
         OI    CURPEDT1,CURPZERB          ZERO=BLANK                            
*                                                                               
         STCM  RF,7,CURPOADD              SAVE COLUMN ADDRESS                   
*                                                                               
         SR    RF,RF                      CLEAR REGISTER                        
         LA    RE,1                       ASSUME 1 BYTE  OF DATA                
         CLI   TSDCLEN,TSDCDAT-TSDCOLD+1  ONE BYTE OF DATA?                     
         BE    COND170                    . YES, READY FOR INSERT               
         OI    CURPEDT1,CURPCOMY          COMMAS=YES                            
         LA    RE,3                                                             
         CLI   TSDCLEN,TSDCDAT-TSDCOLD+2  TWO BYTES OF DATA?                    
         BNE   COND165                    . NO, CONTINUE                        
*                                                                               
         TM    CATRSTA1,CATRSMTR+CATRSMLE MINUS SIGN?                           
         BNZ   COND170                    . NO, READY FOR INSERT                
         LH    RF,TSDCDAT                 LOAD THE DATA WITH THE SIGN           
         B     COND175                    SAVE THE DATA                         
*                                                                               
COND165  LA    RE,7                       ASSUME 3 BYTES OF DATA                
         CLI   TSDCLEN,TSDCDAT-TSDCOLD+3  THREE BYTES OF DATA ?                 
         BE    COND170                    . YES, READY FOR INSERT               
         LA    RE,15                      ASSUME 4 BYTES OF DATA                
         CLI   TSDCLEN,TSDCDAT-TSDCOLD+4  FOUR BYTES OF DATA?                   
         BE    COND170                    . YES, READY FOR INSERT               
         DC    H'0'                       INVALID # OF BYTES OF DATA            
*                                                                               
COND170  EX    RE,CONDICM                 GET THE DATA                          
*                                                                               
COND175  STCM  RF,15,CURPIVAL             SAVE THE DATA                         
         BAS   RE,CONCOL                  GET THE WIDTH                         
         STC   R0,CURPOLEN                SAVE THE COLUMN WIDTH                 
         L     RF,CUREDIT                 -> DDCUREDIT  ROUTINE                 
         BASR  RE,RF                      CALL DDCUREDIT                        
         DROP  R1                                                               
*-----------------------------------------                                      
COND200  TM    PCDRIVEN,PCGRIDQ           TEST RUNNING UNDER GRID?              
         BZ    COND210                    . NO                                  
         LA    R1,TEMP                                                          
         LR    RF,R1                                                            
         LA    R1,L'TEMP-1(R1)            START WITH LAST BYTE                  
COND204  CR    R1,RF                                                            
         BE    COND206                                                          
         CLI   0(R1),C' '                 BACK UP UNTIL DATA                    
         BH    *+8                                                              
         BCT   R1,COND204                                                       
         SR    R1,RF                      SUBTRACT TO GET LENGTH                
         BP    *+6                                                              
COND206  SR    R1,R1                                                            
         AHI   R1,1                                                             
         J     XITR1                                                            
*                                                                               
COND210  LA    R3,CATRLNQ(,R3)                                                  
         B     COND60                                                           
*                                                                               
CONDX    J     XIT                                                              
*                                                                               
CONDICM  ICM   RF,0,TSDCDAT               INSERT DATA INTO REGISTER 15          
*----------------------------------------------------------------------         
*        GET COLUMN WIDTH SUBROUTINE FOR FORM ROUTINES                          
*         INPUT:R0=AVAILABLE                R4=IN USE                           
*               R1=IN USE                   R5=AVAILABLE                        
*               R2=TSAR COLUMN ELEMENT      RE=RETURN ADDRESS                   
*               R3=COL ATTRIBUTE TBL NTRY   RF=AVAILABLE                        
*                                                                               
*         OUTPUT:R0=WIDTH,RF=USED                                               
*----------------------------------------------------------------------         
         USING DISTABD,RF                 MAP DISPLAY COLUMNS TABLE             
CONCOL   L     RF,ADISTAB                 -> DISPLAY COLUMNS TABLE              
*                                                                               
CONC10   CLI   DISCHR,EOT                 END OF TABLE ?                        
         BNE   *+6                        . NO, CONTINUE                        
         DC    H'0'                       COLUMN NOT IN TABLE                   
*                                                                               
         CLC   DISCHR,TSDCTYP             RIGHT COLUMN TYPE?                    
         BE    CONC20                     . YES, GET THE COLUMN WIDTH           
         LA    RF,DISTABL(,RF)            -> NEXT COLUMN TABLE ENTRY            
         B     CONC10                     TEST THIS TABLE ENTRY                 
*                                                                               
CONC20   ZIC   R0,DISWID                  RETURN THE COLUMN WIDTH               
         BR    RE                         RETURN                                
         DROP  RF                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        FORMAT ROUTINES                                                        
*           ON INPUT:                                                           
*                R2 = ADDRESS OF TSAR COLUMN ELEMENT                            
*                R3 = ADDRESS OF COLUMN ATTRIBUTE ELEMENT                       
*                R4 = ADDRESS OF COLUMN DISPLAY AREA                            
*----------------------------------------------------------------------         
*        FORMAT RECORD NUMBER                                                   
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FTSNO    LR    R0,RE                                                            
         CURED (2,TSDCDAT),(4,(R4)),0                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT BATCH TYPE                                                      
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FBTTYPE  LR    R0,RE                                                            
         SR    RF,RF                                                            
         IC    RF,TSDCDAT                                                       
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FBTTYPE1                                                         
         CURED ((RF)),(3,(R4)),0                                                
         B     FBTTYPE2                                                         
FBTTYPE1 CURED ((RF)),(3,(R4)),0                                                
FBTTYPE2 LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT NARRATIVE LINES                                                 
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FNARR    LR    R0,RE                                                            
         SR    RF,RF                                                            
         IC    RF,TSDCLEN                 DATA WIDTH                            
         SHI   RF,TSDCDAT-TSDCOLD                                               
         BZ    FNARRX                                                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ           RUNNING UNDER GRIDS?                  
         BZ    FNARR10                    . NO                                  
         CHI   RF,L'TEMP                  ONLY SHOW WHAT WE CAN FIT             
         BNH   *+8                                                              
         LHI   RF,L'TEMP                                                        
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),TSDCDAT                                                  
         LA    R1,1(RF)                                                         
         B     FNARRX                                                           
*                                                                               
FNARR10  GOTO1 VCHOPPER,DMCB,((RF),TSDCDAT),('VECNARRQ',SCANBLK),10             
         ICM   RF,15,DMCB+8                                                     
         BZ    FNARRX                                                           
         SR    RE,RE                                                            
         IC    RE,LINSUSED                                                      
         CR    RE,RF                                                            
         BNL   *+8                                                              
         STC   RF,LINSUSED                                                      
         SR    RE,RE                                                            
         ICM   RE,3,CATRDLIN-CATRTABD(R3)                                       
         LA    RE,BLDSDSP(RE)                                                   
         CLI   0(RE),0                    NOT DISPLAYING?                       
         BE    FNARRX                                                           
         LA    R1,SCANBLK                                                       
*                                                                               
FNARR30  MVC   0(VECNARRQ,R4),0(R1)                                             
         LA    R4,L'DUMLIN1(,R4)                                                
         LA    R1,VECNARRQ(,R1)                                                 
         BCT   RF,FNARR30                                                       
*                                                                               
FNARRX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT STATUS LINES                                                    
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FSTAT    NTR1                                                                   
         LA    R0,UNSCANBK                CLEAR UNSCAN BLOCK                    
         LHI   R1,MAXUNSCN*UNSCNLNQ                                             
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,UNSCANBK                                                      
         LR    R0,RF                                                            
*                                                                               
         TM    TSDCDAT+1,TRNSDRFT         DRAFT TRANSACTION ?                   
         BZ    FSTAT1                                                           
         MVC   0(L'MX@DRAFT,RF),MX@DRAFT                                        
         LA    RF,L'MX@DRAFT-1(,RF)                                             
*                                                                               
FSTAT0   CLI   0(RF),C' '                                                       
         BH    FSTAT1                                                           
         BCT   RF,FSTAT0                                                        
*                                                                               
FSTAT1   TM    COMPSTA4,CPYSIREG          INVOICE REGISTER IN USE ?             
         BZ    FSTAT10                                                          
         CLI   BASKEY,C'S'                                                      
         BNE   FSTAT10                                                          
         CR    RF,R0                                                            
         BE    FSTAT2                                                           
         MVI   1(RF),C','                                                       
         LA    RF,2(,RF)                                                        
*                                                                               
FSTAT2   TM    TSDCDAT,TRNSAUTH           INVOICE AUTHORISED ?                  
         BZ    FSTAT3                                                           
         MVC   0(L'MX@ATHED,RF),MX@ATHED                                        
         LA    RF,L'MX@ATHED-1(,RF)                                             
         B     FSTAT4                                                           
FSTAT3   MVC   0(L'MX@UATH,RF),MX@UATH                                          
         LA    RF,L'MX@UATH-1(,RF)                                              
*                                                                               
FSTAT4   CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,FSTAT4                                                        
*                                                                               
FSTAT10  TM    TSDCDAT,TRNSHOLD           TRANSACTION HELD ?                    
         BZ    FSTAT20                                                          
         CR    RF,R0                                                            
         BE    FSTAT15                                                          
         MVI   1(RF),C','                                                       
         LA    RF,2(,RF)                                                        
FSTAT15  MVC   0(L'MX@HELD,RF),MX@HELD                                          
         LA    RF,L'MX@HELD-1(,RF)                                              
*                                                                               
FSTAT17  CLI   0(RF),C' '                                                       
         BH    FSTAT20                                                          
         BCT   RF,FSTAT17                                                       
*                                                                               
FSTAT20  TM    TSDCDAT,TRNSAPPR           SELECTED FOR PAYMENT ?                
         BZ    FSTAT30                                                          
         CR    RF,R0                                                            
         BE    FSTAT22                                                          
         MVI   1(RF),C','                                                       
         LA    RF,2(,RF)                                                        
FSTAT22  MVC   0(L'MX@APRVD,RF),MX@APRVD                                        
         LA    RF,L'MX@APRVD-1(,RF)                                             
*                                                                               
FSTAT27  CLI   0(RF),C' '                                                       
         BH    FSTAT30                                                          
         BCT   RF,FSTAT27                                                       
         DROP  R2                                                               
*                                                                               
FSTAT30  TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTAT32                                                          
         LR    R2,R0                                                            
         SR    RF,R2                                                            
         BP    FSTAT31                                                          
         XC    TEMP,TEMP                                                        
         J     XIT                                                              
FSTAT31  EXMVC RF,TEMP,0(R2)                                                    
         J     XIT                                                              
*                                                                               
FSTAT32  LR    R2,R0                                                            
         LA    R1,UNSCANBK                                                      
         LA    R5,1                                                             
*                                                                               
FSTAT35  LA    RE,VECXSTQ-1                                                     
*                                                                               
FSTAT40  LA    R1,0(RE,R1)                                                      
         CLI   0(R1),C','                                                       
         BE    FSTAT45                                                          
         CLI   0(R1),C' '                                                       
         BNH   FSTAT45                                                          
         BCT   RE,FSTAT40                                                       
*                                                                               
FSTAT45  BCTR  RE,0                                                             
         EXMVC RE,0(R4),0(R2)                                                   
         LA    R2,1(,R1)                                                        
         CR    R2,RF                                                            
         BNL   FSTAT50                                                          
         LA    R5,1(,R5)                                                        
         LA    R4,L'DUMLIN1(,R4)                                                
         B     FSTAT35                                                          
*                                                                               
FSTAT50  CLM   R5,1,LINSUSED                                                    
         BNH   *+8                                                              
         STC   R5,LINSUSED                                                      
         J     XIT                                                              
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT MOS/DATE                                                        
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FMOSDATE LR    R0,RE                                                            
         LA    RF,TSDCDAT                                                       
         OC    0(3,RF),0(RF)                                                    
         BZ    FMOSDX                                                           
*                                                                               
FMOSD10  GOTO1 VDATCON,DMCB,(1,0(RF)),(9,(R4))                                  
*                                                                               
FMOSDX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT PAYMENT PERIOD                                                  
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FPAYPER  NTR1                                                                   
         LA    R3,TSDCDAT                                                       
         CLC   TSDCDAT(2),MERGE1                                                
         BE    FPAYPX                                                           
         OC    0(6,R3),0(R3)                                                    
         BZ    FPAY20                                                           
         GOTO1 VDATCON,DMCB,(0,0(R3)),(17,(R4))                                 
         LA    RF,8(,R4)                                                        
*                                                                               
FPAY10   CLI   0(RF),C' '                                                       
         BH    FPAY20                                                           
         BCT   RF,FPAY10                                                        
*                                                                               
FPAY20   OC    6(6,R3),6(R3)                                                    
         BZ    FPAYPX                                                           
         MVI   1(RF),C'-'                                                       
         CLI   6(R3),C'-'                                                       
         BNE   *+8                                                              
         LA    R3,1(R3)            SKIP OVER DASH                               
         GOTO1 VDATCON,DMCB,(0,6(R3)),(17,2(RF))                                
*                                                                               
FPAYPX   J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT PST (CANADA)                                                    
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FPST     NTR1                                                                   
         MVC   WORK,SPACES                CLEAR WORK                            
         L     R1,=A(PRVTAB)                                                    
         A     R1,ORELO                                                         
*                                                                               
FPST10   CLI   0(R1),X'FF'                END OF TABLE?                         
         BE    FPST20                                                           
         CLC   TSDCDAT(L'SCISUBPR),0(R1)                                        
         BE    FPST15                                                           
         LA    R1,L'PRVTAB(,R1)                                                 
         B     FPST10                                                           
*                                                                               
FPST15   MVC   DUB(2),LARF                PASS DESCRIPTION BACK                 
         MVC   DUB+2(2),2(R1)                                                   
         EX    0,DUB                     EXECUTE LA RF,S(AC@XXX)                
         MVC   WORK(L'AC@QST),0(RF)                                             
         MVI   WORK+L'AC@QST,C'/'                                               
*                                                                               
FPST20   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FPST22                                                           
         CURED (P6,TSDCDAT+2),(16,WORK+4),2,FLOAT=-,ALIGN=LEFT,        +        
               COMMAS=YES                                                       
         B     FPST23                                                           
*                                                                               
FPST22   CURED (P6,TSDCDAT+2),(16,WORK+4),2,MINUS=YES,ALIGN=LEFT                
FPST23   LR    RF,R0                                                            
         LA    RF,L'AC@QST+1(,RF)                                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FPST30                                                           
         SR    RE,RE                                                            
         B     FPST32                                                           
FPST30   LA    RE,19                     RE IS ALWAYS>0 SINCE RE=19             
         SR    RE,RF                     MINUS (MAX 11 FROM TSDCDAT+2           
*                                        +1 FOR THE '.' +1 FOR THE SIGN         
*                                        -4 (SINCE L'AC@QST IS 3)               
FPST32   BCTR  RF,0                                                             
         LA    RE,0(RE,R4)                                                      
         EXMVC RF,0(RE),WORK                                                    
*                                                                               
FPSTX    J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------                                      
*        FORMAT CONTRA ACCOUNT                                                  
*-----------------------------------------                                      
         USING TSDCOLD,R2                                                       
FCACT    NTR1                                                                   
         LA    RF,TSDCDAT                                                       
         LA    RE,L'TRNKULC-1                                                   
         CLI   0(RF),C' '                                                       
         BNE   FCACT10                                                          
         LA    RF,L'TRNKUNT+L'TRNKLDG(,RF)                                      
         LA    RE,L'TRNKCACT-1                                                  
*                                                                               
FCACT10  EXMVC RE,0(R4),0(RF)                                                   
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
XITR1    XIT1  REGS=(R1)                                                        
LARF     DC    X'41F0'                                                          
MERGE1   DC    C'***************'                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
*      CONVERT TSAR RECORD TOTAL FOR GRID COLUMNS                     *         
*                                                                     *         
*         ON INPUT:                                                   *         
*             R3 = GRID TABLE                                         *         
***********************************************************************         
         USING TSARRECD,R4                                                      
CONTOT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)               R8=A(LOCAL WORKING STORAGE)           
         L     R4,ATSARREC                R2=A(TSAR RECORD)                     
         XC    TEMP,TEMP                                                        
         LA    R2,TEMP                                                          
*                                                                               
         TM    CURRFLAG,CURRSRTQ          SUPPRESS TOTALS?                      
         BO    COTOX                                                            
                                                                                
         USING GCTBLD,R3                                                        
         SR    R0,R0                                                            
         IC    R0,GCTELSCD                NUMBER OF COL ATTRIBUTE ENTRY         
         MHI   R0,CATRLNQ                                                       
         L     R3,ACATRTAB                                                      
         AR    R3,R0                      POINT TO COL ATTR ENTRY               
         USING CATRTABD,R3                COLUMN ATTRIBUTE TABLE                
*                                                                               
         OC    TSARKYNO,TSARKYNO          ANY TSAR RECORD?                      
         BNZ   COTO05                     . YES, CONTINUE                       
         OC    TSLSTLIN,TSLSTLIN          ANY TSAR RECORDS?                     
         BZ    COTO10                                                           
*                                                                               
         L     R0,ATSARREC                LOAD SAVED TSAR RECORD                
         L     RE,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
COTO05   LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         TM    DISPFLAG,NORECQ+TSARFULQ                                         
         BNZ   COTO10                                                           
         ZAP   DEBITS,TSDDRBFW                                                  
         ZAP   CREDITS,TSDCRBFW                                                 
         ZAP   DISCOUNT,TSDDIBFW                                                
         ZAP   BALANCE,CREDITS                                                  
         SP    BALANCE,DEBITS                                                   
         CLC   TSLSTLIN,TSLSTREC                                                
         BNE   COTO15                                                           
         AP    DEBITS,TSDDR               NET DEBITS FOR ITEM                   
         AP    CREDITS,TSDCR              CREDITS FOR ITEM                      
         AP    BALANCE,TSDCR                                                    
         SP    BALANCE,TSDDR                                                    
         AP    DISCOUNT,TSDDI             DISCOUNT FOR ITEM                     
         B     COTO15                                                           
         DROP  R4                                                               
*                                                                               
COTO10   ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
         ZAP   DISCOUNT,=P'0'                                                   
         ZAP   BALANCE,=P'0'                                                    
*                                                                               
COTO15   CLI   CATRCHR,COLCRSQ            CREDIT COLUMN?                        
         BNE   COTO30                     . NO                                  
         CLI   CURFLAG,CURSET                                                   
         BNE   COTO20                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,CREDITS),(12,TEMP),(RF),ALIGN=LEFT,FLOAT=-,         +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
COTO20   CURED (P8,CREDITS),(12,TEMP),2,ALIGN=LEFT,FLOAT=-,            +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
*                                                                               
COTO30   CLI   CATRCHR,COLDSCQ            DISCOUNT COLUMN?                      
         BNE   COTO50                     . NO                                  
         CLC   SFOLUL,BASKEY              FOLIO LEDGER ?                        
         BE    COTO50                                                           
         CLI   CURFLAG,CURSET                                                   
         BNE   COTO42                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,DISCOUNT),(12,TEMP),(RF),ALIGN=LEFT,FLOAT=-,        +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
COTO42   CURED (P8,DISCOUNT),(12,TEMP),2,ALIGN=LEFT,FLOAT=-,           +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
*                                                                               
COTO50   CLI   CATRCHR,COLNETQ            CREDIT COLUMN?                        
         BNE   COTO60                     . NO                                  
*                                                                               
         MVC   0(L'MX@NETPD,R2),MX@NETPD                                        
         LA    R2,L'MX@NETPD-1(,R2)                                             
COTO52   CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,COTO52                                                        
         MVI   1(R2),C'='                                                       
         LA    R2,2(R2)                                                         
*                                                                               
         CLI   CURFLAG,CURSET                                                   
         BNE   COTO55                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,DEBITS),(12,0(R2)),(RF),ALIGN=LEFT,FLOAT=-,         +        
               COMMAS=YES                                                       
         B     COTO56                                                           
COTO55   CURED (P8,DEBITS),(12,0(R2)),2,ALIGN=LEFT,FLOAT=-,            +        
               COMMAS=YES                                                       
COTO56   AHI   R0,L'MX@NETPD+1                                                  
         B     COTOX1                                                           
*                                                                               
COTO60   CLI   CATRCHR,COLBALQ            BALANCE COLUMN?                       
         BNE   COTO70                     . NO                                  
         CLI   CURFLAG,CURSET                                                   
         BNE   COTO62                                                           
         ICM   RF,15,CUREXC                                                     
         CURED (P8,BALANCE),(12,TEMP),(RF),ALIGN=LEFT,FLOAT=-,         +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
COTO62   CURED (P8,BALANCE),(12,TEMP),2,ALIGN=LEFT,FLOAT=-,            +        
               COMMAS=YES                                                       
         B     COTOX1                                                           
                                                                                
COTO70   B     COTOX                                                            
*                                                                               
COTOX1   LR    R1,R0                                                            
         J     XITR1                                                            
COTOX    LHI   R1,1                                                             
         J     XITR1                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        SPECIAL ROUTINES FOR GRID COLUMNS                            *         
*         ON INPUT:R3=GRID TABLE                                      *         
***********************************************************************         
         USING GCTBLD,R3                                                        
SPCGR    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)               R8=LOCAL WRKING STR                   
         L     R4,ATSARREC                R2=A(TSAR RECORD)                     
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
         XC    TEMP,TEMP                                                        
*                                                                               
         LA    R4,TSDDAT                                                        
         USING TSDCOLD,R4                                                       
SGR10    CLI   TSDCTYP,EOR                FIND DISK ADDRESS COLUMN              
         BE    SGREX                                                            
         CLI   TSDCTYP,COLDAQ                                                   
         BE    SGR20                                                            
         SR    RF,RF                                                            
         IC    RF,TSDCLEN                                                       
         AR    R4,RF                                                            
         B     SGR10                                                            
*                                                                               
SGR20    MVC   DADDRESS,TSDCDAT           GET DISK ADDRESS                      
         MVC   IODAOVER,TSDCDAT                                                 
         GOTO1 AIO,IOGET+IOACCMST+IO3     READ FOR TRANSACTION                  
         BE    SGR30                                                            
         TM    IOERR,IOMAX                MAX IOS REACHED?                      
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     SGRX                                                             
         DROP  R4                                                               
*                                                                               
SGR30    L     R5,AIO3                                                          
         USING TRNRECD,R5                                                       
         LA    R4,TRNRFST                                                       
SGR40    CLI   0(R4),0                                                          
         BE    SGREX                                                            
         CLI   0(R4),FFTELQ           X'DB' ?                                   
         BE    SGR60                                                            
         CLI   0(R4),MPYELQ           X'64' ?                                   
         BE    SGR80                                                            
         CLI   0(R4),GDAELQ           X'E5' ?                                   
         BE    SGR100                                                           
         CLI   0(R4),TRSELQ           X'60' ?                                   
         BE    SGR120                                                           
*                                                                               
SGR50    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SGR40                                                            
*                                                                               
         USING FFTELD,R4                                                        
*-----------------------------------------                                      
*        WORKCODES                                                              
*-----------------------------------------                                      
SGR60    CLI   GCTCOID,GCWC                                                     
         BNE   SGR50                                                            
         CLI   FFTTYPE,FFTTWRKC       WORKCODE                                  
         BNE   SGR50                                                            
         SR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         LA    R2,TEMP                                                          
         LA    R4,FFTDATA                                                       
         B     *+12                                                             
SGR70    MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(L'FFTWORK,R2),0(R4)                                            
         LA    R4,L'FFTWORK+L'FFTWAMT(R4)                                       
         LA    R2,L'FFTWORK(R2)                                                 
         SHI   RF,(L'FFTWORK+L'FFTWAMT)                                         
         BP    SGR70                                                            
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     SGRX                                                             
*                                                                               
*-----------------------------------------                                      
* PAYMENT METHOD CHECK OR EFT                                                   
*-----------------------------------------                                      
         USING MPYELD,R4                                                        
SGR80    DS    0H                                                               
         CLI   GCTCOID,GCPYMET                                                  
         BNE   SGR50                                                            
         CLC   MPYNO,SPACES                                                     
         BNH   SGRX                                                             
         MVC   TEMP(5),=CL5'CHECK'                                              
         CLI   MPYLN,MPYLN4Q                                                    
         BL    SGR90                                                            
*                                                                               
         TM    MPYBYTE,MPYEFT                                                   
         BZ    SGR85                                                            
         MVC   TEMP(5),=CL5'EFT  '                                              
         B     SGR90                                                            
*                                                                               
SGR85    TM    MPYBYTE,MPYPCRD                                                  
         BZ    SGR87                                                            
         MVC   TEMP(5),=CL5'PCARD'                                              
         B     SGR90                                                            
*                                                                               
SGR87    TM    MPYBYTE,MPYCCRD                                                  
         BZ    SGR90                                                            
         MVC   TEMP(5),=CL5'CCARD'                                              
*                                                                               
SGR90    LHI   R1,5                                                             
         B     SGRX                                                             
*                                                                               
*-----------------------------------------                                      
* APPROVAL METHODS                                                              
*-----------------------------------------                                      
*                                                                               
         USING GDAELD,R4                                                        
SGR100   CLI   GCTCOID,GCMETH                                                   
         BNE   SGR50                                                            
*                                                                               
         CLI   GDAEL,0             END OF RECORD ?                              
         BE    SGR50               YES                                          
         CLI   GDATYPE,GDAAPP      NO, APPROVAL TYPE?                           
         BNE   SGR50               NO                                           
         LA    R5,TRNRFST                                                       
         TM    TRNSTAT-TRNELD(R5),TRNSAPPR                                      
         BZ    SGR50                                                            
         OC    GDAAPPDT,GDAAPPDT   ANY DATE?                                    
         BZ    SGR50               NO                                           
         CLI   GDATSUB,GDAAPPAA    IS THIS AUTO APPROVED?                       
         BNE   SGR102              NO                                           
         MVC   TEMP(L'MX@AAPP),MX@AAPP-OVERWRKD(R8)                             
         B     SGR104                                                           
*                                                                               
SGR102   CLI   GDATSUB,GDAAPPMK    IS THIS MARKER APPROVED?                     
         BNE   SGR50                                                            
         MVC   TEMP(L'MX@MAPP),MX@MAPP-OVERWRKD(R8)                             
*                                                                               
SGR104   LHI   R1,12                                                            
         B     SGRX                                                             
*                                                                               
         USING TRSELD,R4                                                        
SGR120   CLI   GCTCOID,GCMETH                                                   
         BNE   SGR50                                                            
*                                                                               
         CLI   TRSEL,0             END OF RECORD ?                              
         BE    SGR50               YES                                          
         TM    TRSMARK,TRSMUMQ     IS ACTION "UNDONE"?                          
         BO    SGR50               YES, SKIP IT                                 
         CLI   TRSMARK,TRSMCSQ     LAST MARKER ACTION = APPROVE?                
         BNE   SGR50               NO                                           
         MVC   TEMP(L'MX@MAPP),MX@MAPP-OVERWRKD(R8)                             
         LHI   R1,12                                                            
         B     SGRX                                                             
*                                                                               
SGREX    MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
SGRX     XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         DROP  R3,R4,R5,RB                                                      
         EJECT                                                                  
***********************************************************************         
*        GET CREDIT DETAILS AND PLACE IN CRBLOCK                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     CREDIT DETAILS PUT IN TABLE                  *         
*          CC NOT EQUAL- NO ROOM IN TABLE FOR CREDIT DETAILS          *         
***********************************************************************         
         USING CRD,R2                                                           
         USING TRNRECD,R3                                                       
GETCRDET NMOD1 0,**GETC**                                                       
*                                                                               
         L     R3,AIO1                                                          
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
         LA    R0,CRMAXQ           R0=(MAX NUM ENTRIES IN CREDITOR BLK)         
         L     R2,ACRBLOCK         R2=A(CREDITOR BLOCK)                         
         L     RF,AOPTVALS                                                      
*                                                                               
GETCR05  OC    CRTDATE,CRTDATE     SPARE ENTRY ?                                
         BZ    GETCR05B                                                         
         CLI   OMERGE-OPTVALSD(RF),C'Y'                                         
         BNE   GETCR05A                                                         
         BAS   RE,CRMATCH          MATCH ON INVOICE DETAILS ?                   
         BE    GETCR10D                                                         
*                                                                               
GETCR05A LA    R2,CRLNQ(,R2)       BUMP TO NEXT ENTRY                           
         BCT   R0,GETCR05                                                       
         B     GETCRFX             CREDITOR BLOCK IS FULL                       
*                                                                               
GETCR05B ZAP   CRDRAMT,=P'0'       DEBIT AMOUNT                                 
         ZAP   CRDISC,=P'0'        DISCOUNT AMOUNT                              
         ZAP   CRNET,=P'0'         NET AMOUNT                                   
         ZAP   CRCRAMT,=P'0'       CREDIT AMOUNT                                
         ZAP   CRGST,=P'0'                                                      
         ZAP   CRPST+L'SCISUBPR(L'SCIAMNT),=P'0'                                
         ZAP   TRANACT,=P'0'       AGENCY TRANSACTION AMOUNT                    
*                                                                               
         MVC   CRTDATE,TRNKDATE    TRANSACTION DATE                             
         MVC   CRTREF,TRNKREF      TRANSACTION REF                              
*                                                                               
*        CLI   TRNKREF+MBVOFFSQ,MBVINDIQ       127+ TRANSACTIONS ?              
*        BNE   GETCR10                                                          
*                                                                               
*        USING FFTELD,RE                                                        
*        ICM   RE,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
*        BZ    GETCR10                                                          
*        SR    R0,R0                                                            
*                                                                               
*ETCR06  CLI   FFTEL,0                                                          
*        BE    GETCR10                                                          
*        CLI   FFTEL,FFTELQ                                                     
*        BNE   GETCR10                                                          
*        CLI   FFTTYPE,FFTTKREF    KEY REF NUMBER FOR BANK VOID ?               
*        BE    GETCR08                                                          
*        IC    R0,FFTLN                                                         
*        AR    RE,R0                                                            
*        B     GETCR06                                                          
*                                                                               
*ETCR08  MVC   CRTREF,FFTDATA      REFERENCE WHICH MATCHES CREDIT               
*        DROP  RE                                                               
*                                                                               
GETCR10  MVC   CRTRSTAT,TRNRSTAT          TRANSACTION STATUS                    
         MVC   CRULC,SPACES               CONTRA ACCOUNT                        
         LA    RF,L'TRNKULC                                                     
         LA    RE,TRNKULC                                                       
*                                                                               
GETCR10A CLI   0(RE),C' '                                                       
         BH    GETCR10B                                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,GETCR10A                                                      
         B     GETCR10C                                                         
*                                                                               
GETCR10B BCTR  RF,0                                                             
         EXMVC RF,CRULC,0(RE)                                                   
*                                                                               
GETCR10C MVC   CRTSARNO,RECCOUNT   TSAR RECORD NUMBER                           
         SR    RF,RF                                                            
         ICM   RF,3,RECCOUNT                                                    
         LA    RF,1(,RF)                                                        
         STCM  RF,3,RECCOUNT                                                    
         MVC   CRDA,DADDRESS       DISK ADDRESS                                 
         MVC   CRKSREF,TRNKSBR     KEY SUB REFERENCE                            
*                                                                               
GETCR10D GOTO1 ABLDSRC             GET SOURCE DETAILS                           
         CLC   SFOLUL,BASKEY       FOLIO LEDGER ?                               
         BE    GETCR11                                                          
         CLC   SPROUL,SRCWORK      PRODUCTION ?                                 
         BNE   GETCR10E                                                         
         GOTO1 SETFLD,DMCB,('SFMRGEQ',SRCWORK+L'SPROUL),(L'CRJOB,CRJOB)         
         B     GETCR11                                                          
*                                                                               
GETCR10E GOTO1 SETFLD,DMCB,('SFMRGEQ',SRCWORK),(L'CRSRC,CRSRC)                  
         B     GETCR12                                                          
*                                                                               
GETCR11  GOTO1 SETFLD,DMCB,('SFMRGEQ',SRCWORK+L'SPROUL),(L'CRJOB,CRSRC)         
*                                                                               
GETCR12  GOTO1 SETFLD,DMCB,('SFLOWQ',TRNRSMOS),(L'CRMOS,CRMOS)                  
         GOTO1 SETFLD,DMCB,('SFMRGEQ',SRCPERD),(L'CRPERD,CRPERD)                
         GOTO1 SETFLD,DMCB,('SFLOWQ',SRCFMMD),(L'CRMMOS,CRMMOS) MED MOS         
*                                                                               
         USING TRNELD,R4                                                        
GETCR40  ICM   R4,15,ATRNELD       R4=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AP    CRNET,TRNAMNT       NET AMOUNT                                   
         AP    CRCRAMT,TRNAMNT     CREDIT AMOUNT                                
         B     GETC40AB                                                         
*                                                                               
GETC40AA ICM   R4,15,ATRNELD       R4=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETC40AB AP    TRANACT,TRNAMNT     AGENCY TRANSACTION AMOUNT                    
         TM    CRSTAT,CRMATCHQ                                                  
         BZ    GETC40AC                                                         
*                                                                               
GETC40AC GOTO1 SETFLD,DMCB,('SFMRGEQ',TRNBREF),(L'CRBREF,CRBREF) BTCH #         
*                                                                               
GETC40AD GOTO1 SETFLD,DMCB,('SFMRGEQ',TRNTYPE),(L'CRBTYPE,CRBTYPE) TYPE         
         GOTO1 SETFLD,DMCB,('SFZEROQ',TRNSTAT),(L'CRTESTAT,CRTESTAT)            
         GOTO1 SETFLD,DMCB,('SFMRGEQ',TRNOFFC),(L'CROFFICE,CROFFICE)            
         TM    CRSTAT,CRMATCHQ     MERGED CREDITS ?                             
         BO    GETCR40A                                                         
         SR    RF,RF                                                            
         IC    RF,TRNLN            TRANSACTION ELEMENT LENGTH                   
         SHI   RF,TRNNARR-TRNELD                                                
         BZ    GETCR40A                                                         
         STC   RF,CRTNARLN         LENGTH OF TRANSACTION NARRATIVE              
         BCTR  RF,0                                                             
         EXMVC RF,CRTNARR,TRNNARR  TRANSACTION NARRATIVE                        
         DROP  R4                                                               
*                                                                               
         USING NAMELD,R4           NAME ELEMENT                                 
GETCR40A ICM   R4,15,ANAMELD       R4=A(NAME ELEMENT)                           
         BZ    GETCR41                                                          
         OC    CRPNAM(L'CRPNAM),CRPNAM                                          
         BNZ   GETCR41                                                          
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1    RF=X L(NAME)                                     
         EXMVC RF,CRPNAM,NAMEREC   SAVE NAME                                    
         LA    RF,1(,RF)                                                        
         STC   RF,CRPNAMLN         SAVE ACTUAL LENGTH OF NAME                   
         DROP  R4                                                               
*                                                                               
         USING DUEELD,R4                                                        
GETCR41  SR    R4,R4                                                            
         ICM   R4,15,ADUEELD       R4=A(DUE DATE ELEMENT)                       
         BZ    GETCR41A                                                         
         GOTO1 SETFLD,DMCB,('SFMRGEQ',DUEDATE),(L'CRDUDAT,CRDUDAT) DUE          
         DROP  R4                                                               
         B     GETCR41B                                                         
GETCR41A DS    0H                                                               
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(2,CRDUDAT)                            
         B     GETCR41B                                                         
*                                                                               
         USING MXPELD,R4                                                        
GETCR41B ICM   R4,15,AMXPELD       R4=A(MEDIA EXTRA PAYMENT ELEMENT)            
         BZ    GETCR50                                                          
         XC    WORK,WORK                                                        
         SR    RF,RF                                                            
         ICM   RF,7,MXPSER+1                                                    
         BZ    GETCR50                                                          
         LA    RE,WORK                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(7,RE),DUB                                                      
         B     GETCR41C                                                         
*                                                                               
GETCR41C MVC   0(1,RE),MXPSER                                                   
*                                                                               
         USING FFTELD,R4                                                        
GETCR50  ICM   R4,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
         BZ    GETCR56                                                          
GETCR52  CLI   FFTEL,0                                                          
         BE    GETCR56                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTINVN     LONG INVOICE NUMBER                         
         BE    GETCR54                                                          
         XR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     GETCR52                                                          
GETCR54  ZIC   RF,FFTDLEN           DATA LENGTH                                 
         LTR   RF,RF                                                            
         BZ    GETCR56                                                          
         CHI   RF,L'CRMIN           MAKE SURE NOT BIGGER THAN WE CAN            
         BNH   *+8                  HANDLE                                      
         LHI   RF,L'CRMIN                                                       
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FFTDATA                                                  
         GOTO1 SETFLD,DMCB,('SFMRGEQ',WORK),(L'CRMIN,CRMIN)                     
         DROP  R4                                                               
*                                                                               
GETCR56  CLI   TRNKLDG,C'P'                                                     
         BE    GETCR60                                                          
         CLI   TRNKLDG,C'Q'                                                     
         BE    GETCR60                                                          
         CLI   TRNKLDG,C'S'                                                     
         BE    GETCR60                                                          
         CLI   TRNKLDG,C'T'                                                     
         BE    GETCR60                                                          
         CLI   TRNKLDG,C'U'                                                     
         BNE   GETCR100                                                         
*                                                                               
         USING XPYELD,R4                                                        
GETCR60  SR    R4,R4                                                            
         ICM   R4,15,AXPYELD       R4=A(EXTRA PAYMENT ELEMENT)                  
         BZ    GETCR120                                                         
         CLC   CRMIN,SPACES                                                     
         BH    GETCR65                                                          
         GOTO1 SETFLD,DMCB,('SFMRGEQ',XPYINV),(L'XPYINV,CRMIN)                  
GETCR65  CLC   SFOLUL,BASKEY       FOLIO LEDGER ?                               
         BE    *+10                                                             
         AP    CRDISC,XPYCD        CASH DISCOUNT                                
         B     GETCR120                                                         
         DROP  R4                                                               
*                                                                               
         USING SCIELD,R4                                                        
GETCR100 ICM   R4,15,ASCIELD       R4=A(SUBSIDIARY CASH ELEMENT)                
         BZ    GETCR120                                                         
GETCR101 CLI   SCIEL,0             END OF RECORD ?                              
         BE    GETCR120            YES                                          
         CLI   SCIEL,SCIELQ        NO, SCIEL?                                   
         BNE   GETCR110            NO, GET NEXT                                 
         CLI   SCITYPE,SCITTAXP    CANADAIAN GST ?                              
         BNE   GETCR102                                                         
         AP    CRGST,SCIAMNT                                                    
         B     GETCR110                                                         
*                                                                               
GETCR102 CLI   SCITYPE,SCITTQST    CANADAIAN PST ?                              
         BNE   GETCR104                                                         
         MVC   CRPST(L'SCISUBPR),SCISUBPR TYPE                                  
         AP    CRPST+L'SCISUBPR(L'SCIAMNT),SCIAMNT AMOUNT                       
         B     GETCR110                                                         
*                                                                               
GETCR104 CLI   SCITYPE,SCITCDSC    CASH DISCOUNT ?                              
         BNE   GETCR110                                                         
         CLC   SFOLUL,BASKEY       FOLIO LEDGER?                                
         BE    GETCR110                                                         
         AP    CRDISC,SCIAMNT                                                   
*                                                                               
GETCR110 SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         AR    R4,RF                                                            
         B     GETCR101                                                         
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
GETCR120 ICM   R4,15,ATRSELD       R4=A(TRANSACTION STATUS ELEMENT)             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SETFLD,DMCB,('SFMRGEQ',TRSUDAT),(L'CRUSEDAT,CRUSEDAT)            
         GOTO1 SETFLD,DMCB,('SFMRGEQ',TRSDATE),(L'CRACTDAT,CRACTDAT)            
*                                                                               
         TM    TRSSTAT,TRSSVOID    TRANSACTION VOID ?                           
         BZ    GETCR130                                                         
         OI    CRSTAT,CRVOIDQ      MARK AS VOID                                 
*                                                                               
GETCR130 TM    TRSSTAT,TRSSOFFS    OFFSET/CONTRA'D ?                            
         BZ    GETCR165                                                         
         TM    CRSTAT,CRMATCHQ     MERGED CREDIT ?                              
         BZ    GETCR140                                                         
         MVC   CRBREF,SPACES                                                    
         MVC   CRBREF(L'MX@YES),MX@YES                                          
*                                                                               
GETCR140 GOTO1 SETFLD,DMCB,('SFMRGEQ',TRSUDAT),(L'CRCONDAT,CRCONDAT)            
*                                                                               
GETCR165 TM    TRNRSTAT,TRNSREVS   REVERSAL??                                   
         BZ    GETCR170                                                         
         OI    CRSTAT,CRREVDQ                                                   
         OC    TRSREVD,TRSREVD                                                  
         BZ    GETCR170                                                         
*        MVC   CRCHQDAT,TRSREVD                                                 
*                                                                               
GETCR170 TM    TRSSTAT,TRSSVOID    TRANSACTION VOID ?                           
         BO    GETCR220                                                         
         DROP  R4                                                               
*                                                                               
         USING MPYELD,R4                                                        
         XC    SVCHQDAT,SVCHQDAT                                                
         ICM   R4,15,AMPYELD       R4=A(MEDIA PAYMENT ELEMENT)                  
         BZ    GETCR220                                                         
*                                                                               
GETCR200 CLC   MPYNO,SPACES                                                     
         BNH   GETCR220                                                         
         CLI   MPYLN,MPYLN3Q       CHEQUE PAID MORE THAN ONE INVOICE ?          
         BE    GETCR205                                                         
         AP    CRDRAMT,MPYAMNT                                                  
         B     GETCR210                                                         
*                                                                               
GETCR205 AP    CRDRAMT,MPYPART                                                  
*                                                                               
GETCR210 CLC   SVCHQDAT,MPYDTE                                                  
         BH    GETCR220                                                         
         MVC   SVCHQDAT,MPYDTE                                                  
         MVC   CRBANK,MPYBNK       BANK ACCOUNT                                 
         GOTO1 SETFLD,DMCB,('SFMRGEQ',MPYDTE),(L'CRCHQDAT,CRCHQDAT)             
         OI    CRSTAT,CRCRPAYQ     CREDIT PAYMENT DETAILS                       
         GOTO1 SETFLD,DMCB,('SFMRGEQ',MPYNO),(L'CRCHEQUE,CRCHEQUE)              
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
GETCR220 ICM   R4,15,ATRSELD       R4=A(TRANSACTION STATUS ELEMENT)             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    CRSTAT,CRCRPAYQ     CREDIT PAYMENT DETAILS                       
         BZ    GETCR235                                                         
         TM    TRSSTAT,TRSSACHQ    OFFLINE ?                                    
         BZ    GETCR230                                                         
         TM    TRSSTAT2,TRSSPMNT   PAYMENT OR SYSTEM CHEQUE ?                   
         BO    GETCR240                                                         
         B     GETCR250                                                         
*                                                                               
GETCR230 CLI   TRSMARK,TRSMCMQ     MANUAL OR SYSTEM CHEQUE ?                    
         BE    GETCR260                                                         
         B     GETCR250                                                         
*                                                                               
GETCR235 OC    TRSUDAT,TRSUDAT     PAID ?                                       
         BNZ   GETCR250                                                         
         B     GETCR290                                                         
*                                                                               
GETCR240 OI    CRSTAT2,CRPAYQ      PAYMENT                                      
         B     GETCR290                                                         
*                                                                               
GETCR250 OI    CRSTAT2,CRSYSCHQ    SYSTEM CHEQUE                                
         B     GETCR290                                                         
*                                                                               
GETCR260 OI    CRSTAT2,CRMANCHQ    MANUAL CHEQUE                                
         B     GETCR290                                                         
         DROP  R4                                                               
*                                                                               
GETCR290 CLC   CRMIN,SPACES                                                     
         BH    GETCR300                                                         
         GOTO1 SETFLD,DMCB,('SFMRGEQ',TRNKREF),(L'TRNKREF,CRMIN)                
*                                                                               
GETCR300 B     GETCRX                                                           
GETCRFX  LTR   RF,RF                                                            
         B     GETCREX                                                          
GETCRX   CR    RB,RB                                                            
GETCREX  B     XIT1                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*        TEST IF CURRENT CREDIT MATCHES PREVIOUS ENTRY                *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
*          R2=A(CREDIT ENTRY)                                         *         
*          ASETELE ASSUMED TO HAVE BEEN CALLED FOR CURR TRANSACTION   *         
***********************************************************************         
         USING CRD,R2                                                           
         USING TRNRECD,R3                                                       
CRMATCH  NTR1                                                                   
*                                                                               
         CLC   CRTDATE,TRNKDATE    MATCH ON TRANSACTION DATE                    
         BNE   CRMAEX                                                           
         CLC   CRTREF,TRNKREF      MATCH ON TRANSACTION REF                     
         BNE   CRMAEX                                                           
         LA    RF,L'TRNKULC        RF =  LENGTH MINUS LEAD BLANKS               
         LA    RE,TRNKULC          RE -> 1ST BYTE IN TRNKULC                    
*                                                                               
CRMA05   CLI   0(RE),C' '                                                       
         BH    CRMA10                                                           
         LA    RE,1(,RE)                                                        
         BCT   RF,CRMA05                                                        
         B     CRMAEX                                                           
*                                                                               
CRMA10   BCTR  RF,0                                                             
         EXCLC RF,CRULC,0(RE)      MATCH ON CONTRA ACCOUNT ?                    
         BNE   CRMAEX                                                           
*                                                                               
         ICM   RF,15,AMPYELD       RF=A(MANUAL PAYMENT ELEMENT)                 
         BZ    CRMAEX                                                           
*                                                                               
         USING NAMELD,RF           NAME ELEMENT                                 
         ICM   RF,15,ANAMELD       R4=A(NAME ELEMENT)                           
         BNZ   CRMA20                                                           
         CLI   CRPNAMLN,0          PREVIOUS CREDIT SUNDRY CRED ?                
         BNE   CRMAEX                                                           
         B     CRMAX                                                            
*                                                                               
CRMA20   SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q          RF=L(NAME)                                   
         CLM   RF,1,CRPNAMLN       MATCH ON NAME LENGTHS ?                      
         BNE   CRMAEX                                                           
         BCTR  RF,0                                                             
         EXCLC RF,CRPNAM,NAMEREC   SAME SUPPLIER NAME ?                         
         BE    CRMAX                                                            
         DROP  RF                                                               
*                                                                               
CRMAEX   LTR   RB,RB                                                            
         B     XIT1                                                             
*                                                                               
CRMAX    OI    CRSTAT,CRMATCHQ     SET MATCHED CREDIT STATUS                    
         CR    RB,RB                                                            
         B     XIT1                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*        SET FIELD IF CONDITIONS ARE MET                              *         
* ON ENTRY PARAM 1 BYTE 0 SET TO CONDITION TYPE                       *         
*                 'SFMTCHQ'  KEEP IF MATCH ELSE SET BLANK             *         
*                 'SFMRGEQ'  KEEP IF SAME ELSE SET *****MERGE*****    *         
*                 'SFLOWQ'   KEEP LOWEST VALUE                        *         
*                 'SFZEROQ   KEEP IF SAME ELSE INIT WITH BIN ZEROS    *         
*          PARAM 1 BYTE 1-3  A(INPUT STRING)                          *         
*          PARAM 2 BYTE 0    L'(INPUT/OUPUT STRING)                   *         
*          PARAM 2 BYTE 1-3  A(OUTPUT STRING)                         *         
***********************************************************************         
SETFLD   NTR1                                                                   
*                                                                               
         LR    R2,R1               R2=A(PARAMETER LIST)                         
         SR    R1,R1                                                            
         IC    R1,4(,R2)           R1=L'(INPUT/OUTPUT STRING)                   
         BCTR  R1,0                                                             
         ICM   RE,15,0(R2)         RE=A(INPUT STRING)                           
         ICM   RF,15,4(R2)         RF=A(OUTPUT STRING)                          
*                                                                               
         EXOC  R1,0(RF),0(RF)      OUTPUT STRING CURRENTLY EMPTY ?              
         BZ    SETF30                                                           
         CLI   0(R2),SFMTCHQ       MATCH CONDITION ?                            
         BE    SETF25                                                           
         CLI   0(R2),SFMRGEQ       MERGE CONDITION ?                            
         BE    SETF10                                                           
         CLI   0(R2),SFZEROQ       MATCHED ELSE ZERO CONDITION ?                
         BE    SETF05                                                           
         CLI   0(R2),SFLOWQ        LOWEST VALUE CONDITION ?                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EXCLC R1,0(RE),0(RF)      INPUT STRING < OUPUT STRING ?                
         BNL   SETFX                                                            
         B     SETF30                                                           
*                                                                               
SETF05   EXCLC R1,0(RE),0(RF)      INPUT STRING MATCHES OUPUT STRING ?          
         BE    SETFX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)       OVERWRITE OUTPUT WITH BIN ZEROS              
         B     SETFX                                                            
*                                                                               
SETF10   EXCLC R1,0(RE),0(RF)      INPUT STRING MATCHES OUPUT STRING ?          
         BE    SETFX                                                            
         EXMVC R1,0(RF),MERGE2     OVERWRITE OUTPUT STRING WITH ******          
         LA    R3,L'MX@MRGED-1                                                  
*                                                                               
SETF15   LA    RE,MX@MRGED(R3)                                                  
         CLI   0(RE),C' '                                                       
         BH    SETF20                                                           
         BCT   R3,SETF15                                                        
         DC    H'0'                                                             
*                                                                               
SETF20   LA    RE,2(,R3)                                                        
         CR    RE,R1                                                            
         BH    SETFX                                                            
         SR    R1,R3                                                            
         SRA   R1,1                                                             
         LA    RF,0(R1,RF)                                                      
         EXMVC R3,0(RF),MX@MRGED   OVERWRITE OUTPUT STRING WITH ******          
         B     SETFX                                                            
*                                                                               
SETF25   EXCLC R1,0(RE),0(RF)      INPUT STRING MATCHES OUPUT STRING ?          
         BE    SETFX                                                            
         EXMVC R1,0(RF),SPACES     OVERWRITE OUTPUT STRING WITH SPACES          
         B     SETFX                                                            
*                                                                               
SETF30   EXMVC R1,0(RF),0(RE)      OVERWRITE OUTPUT STRING WITH INPUT           
         B     SETFX                                                            
*                                                                               
SETFX    DS    0H                                                               
*                                                                               
XIT1     XIT1  ,                                                                
*                                                                               
SFMTCHQ  EQU   X'80'               MATCHED CONDITION                            
SFMRGEQ  EQU   X'40'               MERGE CONDITION                              
SFLOWQ   EQU   X'20'               LOWEST CONDITION                             
SFZEROQ  EQU   X'20'               MATCHED CONDITION (BIN ZERO)                 
*                                                                               
MERGE2   DC    (L'MERGE)CL1'*'                                                  
         EJECT ,                                                                
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*        TABLES                                                       *         
***********************************************************************         
DCCAP    DS    0X                                                               
         DCDDL AC#ONT,3                                                         
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#HST,3                                                         
         DCDDL AC#DSCAD,3                                                       
         DCDDL AC#HBC,6                                                         
         DCDDL AC#HAL,6                                                         
         DCDDL AC#HSA,6                                                         
         DCDDL AC#HMA,6                                                         
         DCDDL AC#HPQ,6                                                         
         DCDDL AC#HON,6                                                         
         DCDDL AC#HPE,6                                                         
         DCDDL AC#HNB,6                                                         
         DCDDL AC#HNS,6                                                         
         DCDDL AC#HNF,6                                                         
         DC    AL1(EOT)                                                         
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH14,78                                                      
         DCDDL AC#ENH15,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#DRAFT,10                                                      
         DCDDL AC#CTR,9                                                         
         DCDDL AC#PYMNT,10                                                      
         DCDDL AC#CHK,10                                                        
         DCDDL AC#DATE,10                                                       
         DCDDL AC#BNK,10                                                        
         DCDDL AC#CHKTO,10                                                      
         DCDDL AC#PAYTO,10                                                      
         DCDDL AC#GROSS,7                                                       
         DCDDL AC#NET,7                                                         
         DCDDL AC#FLTX,7                                                        
         DCDDL AC#ORDER,7                                                       
         DCDDL AC#SUBR,7                                                        
         DCDDL AC#CMRCL,7                                                       
         DCDDL AC#JOB,7                                                         
         DCDDL AC#EXP,7                                                         
         DCDDL AC#SRC,7                                                         
         DCDDL AC#DUEDT,8                                                       
         DCDDL AC#SERNO,6                                                       
         DCDDL AC#RUNNO,7                                                       
         DCDDL AC#FACC,7                                                        
         DCDDL AC#ATTR,9                                                        
         DCDDL AC#ATHED,4                                                       
         DCDDL AC#UATH,6                                                        
         DCDDL AC#HELD,10                                                       
         DCDDL AC#SELED,10                                                      
         DCDDL AC#APRVD,10                                                      
         DCDDL AC#HOURS,5                                                       
         DCDDL AC#TYPE,2                                                        
         DCDDL AC#STT,2                                                         
         DCDDL AC#OTHER,2                                                       
         DCDDL AC#UDAT,2                                                        
         DCDDL AC#PELDT,2                                                       
         DCDDL AC#INVS,8                                                        
         DCDDL AC#DRS,8                                                         
         DCDDL AC#BAL,7                                                         
         DCDDL AC#NETPD,8                                                       
         DCDDL AC#SEQ,2                                                         
         DCDDL AC#ACCT,15                                                       
         DCDDL AC#ACCTS,15                                                      
         DCDDL AC#CHKC,20                                                       
         DCDDL AC#DPSON,20                                                      
         DCDDL AC#OFFSO,20                                                      
         DCDDL AC#WRTFA,20                                                      
         DCDDL AC#XFRFR,20                                                      
         DCDDL AC#XFRTO,20                                                      
         DCDDL AC#DATED,10                                                      
         DCDDL AC#MOS,10                                                        
         DCDDL AC#CTRD,10                                                       
         DCDDL AC#AMT,10                                                        
         DCDDL AC#DISS,4                                                        
         DCDDL AC#YES,4                                                         
         DCDDL AC#MRGED,7                                                       
         DCDDL AC#DISS,L'MX@DISSL                                               
         DCDDL AC#ACTYD,L'MX@ACTYD            ACTIVITY DATE                     
         DCDDL AC#PERD,L'MX@PERD              PERIOD                            
         DCDDL AC#REC,L'MX@REC                RECORD                            
         DCDDL AC#RSCDT,L'MX@RSCDT            CHECK DATE                        
         DCDDL AC#RSCNO,L'MX@RSCNO            CHECK NUMBER                      
         DCDDL AC#JOEST,L'MX@JOEST            JOB/ESTIMATE                      
         DCDDL AC#RSCOC,L'MX@RSCOC            CONTRA ACCOUNT                    
         DCDDL AC#RSDUD,L'MX@RSDUD            DUE DATE                          
         DCDDL AC#MOA,L'MX@MOA                MOA                               
         DCDDL AC#OFFST,L'MX@OFFST            OFFSET                            
         DCDDL AC#WC,L'MX@WC                  WORKCODE                          
         DCDDL AC#TYPE1,L'MX@TYPE1            TYPE                              
         DCDDL AC#MEDC,L'MX@MEDC              MEDIA                             
         DCDDL AC#INVC2,L'MX@INVCN            INVOICE NUMBER                    
         DCDDL AC#MMOS,L'MX@MMOS              MEDIA MOS                         
         DCDDL AC#TRAND,L'MX@TRAND            TRANS DATE                        
         DCDDL AC#PAYME,L'MX@PAYME            PAYMENT METHOD                    
         DCDDL AC#APRVM,L'MX@APRVM  APPROVAL METHOD                             
         DCDDL AC#AAPP,L'MX@AAPP    AUTO APPROVED                               
         DCDDL AC#MAPP,L'MX@MAPP    MARKER APPROVED                             
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        COLUMN ATTRIBUTES TABLE                                      *         
*                                                                     *         
*        COLUMNS TO BE GENERATED AND FIELDS TO BE ADDED TO THE        *         
*        TSAR RECORDS BASED UPON THE USER COLUMN OPTIONS              *         
*                                                                     *         
*        NOTE: MAKE SURE THAT THESE FIELDS WILL STILL FIT ON A        *         
*              TSAR RECORD.  IN THE US, IF ALL COLUMNS ARE            *         
*              CHOSEN (THE DEFAULT CASE), AND ALL THE VARIABLE        *         
*              FIELDS HAPPEN TO REQUIRE THEIR MAXIMUM FIELD           *         
*              LENGTH, THEN THE RECORD SHOULD STILL FIT INTO THE      *         
*              TSAR BUFFER (TSARREC).  I.E. THE MAX TSARDATD          *         
*              AREA SHOULD STILL FIT INTO TSAROVER.                   *         
*              IN THE UK, WE MIGHT NOT FIT; I.E. WE SHOULD            *         
*              CONSIDER INCREASING THE SIZE OF TSAROVER.              *         
***********************************************************************         
CATRTAB  DS    0AL1                                                             
*                                                                               
CATR1    DC    AL1(COLOFFQ,L'CROFFICE,CATRMOVQ,0)                     1         
         DC    AL2(CROFFICE-CRD,ATSOFF-ATSADDR,BLDSOFFC-BLDSDSP)                
         DC    AL2(0)                                                           
*                                                                               
CATR3    DC    AL1(COLCONQ,L'CRULC,0,0)                               3         
         DC    AL2(CRULC-CRD,ATSCONT-ATSADDR,BLDSCACT-BLDSDSP)                  
         DC    AL2(FCACT-CONDAT)                                                
*                                                                               
CATR4    DC    AL1(COLDATQ,L'CRTDATE,CATRPDTQ,0)                      4         
         DC    AL2(CRTDATE-CRD,ATSTDATE-ATSADDR,BLDSDATE-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATR5    DC    AL1(COLREFQ,L'CRTREF,CATRMOVQ,0)                       5         
         DC    AL2(CRTREF-CRD,ATSTREF-ATSADDR,BLDSREF-BLDSDSP)                  
         DC    AL2(0)                                                           
*                                                                               
CATR6    DC    AL1(COLMOSQ,L'CRMOS,CATRMDTQ,0)                        6         
         DC    AL2(CRMOS-CRD,ATSMOS-ATSADDR,BLDSMOS-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATR7    DC    AL1(COLBATQ,L'CRBREF,CATRMOVQ,0)                       7         
         DC    AL2(CRBREF-CRD,ATSBREF-ATSADDR,BLDSBREF-BLDSDSP)                 
         DC    AL2(0)                                                           
*                                                                               
CATR8    DC    AL1(COLNETQ,L'CRNET,CATRPAZQ,0)                        8         
         DC    AL2(CRNET-CRD,ATSAMT-ATSADDR,BLDSNET-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATRA    DC    AL1(COLCRSQ,L'CRCRAMT,CATRPAZQ,0)                      A         
         DC    AL2(CRCRAMT-CRD,ATSCRAMT-ATSADDR,BLDSCRAM-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATRC    DC    AL1(COLJOBQ,L'CRJOB,CATRMOVQ,0)                        C         
         DC    AL2(CRJOB-CRD,ATSJOB-ATSADDR,BLDSJOB-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATRD    DC    AL1(COLSRCQ,L'CRSRC,CATRMOVQ,0)                        D         
         DC    AL2(CRSRC-CRD,ATSSRC-ATSADDR,BLDSSRCA-BLDSDSP)                   
         DC    AL2(0)                                                           
*                                                                               
CATRE    DC    AL1(COLPYMQ,L'CRCHEQUE,CATRMOVQ,0)                     E         
         DC    AL2(CRCHEQUE-CRD,ATSCHEQU-ATSADDR,BLDSCHQ-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATRI    DC    AL1(COLNUMQ,L'CRTSARNO,0,0)                            I         
         DC    AL2(CRTSARNO-CRD,ATSTSRNO-ATSADDR,BLDSRNUM-BLDSDSP)              
         DC    AL2(FTSNO-CONDAT)                                                
*                                                                               
CATRJ    DC    AL1(COLDSCQ,L'CRDISPAY,CATRPAMQ,CATRSGWD)              J         
         DC    AL2(CRDISPAY-CRD,ATSDISPY-ATSADDR,BLDSDSCA-BLDSDSP)              
         DC    AL2(0)                                                           
*                                                                               
CATRK    DC    AL1(COLGSTQ,L'CRGST,CATRPAZQ,0)                        K         
         DC    AL2(CRGST-CRD,ATSGST-ATSADDR,BLDSGST-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATRL    DC    AL1(COLBALQ,L'CRBAL,CATRPAMQ,CATRSGWD)                 L         
         DC    AL2(CRBAL-CRD,ATSBAL-ATSADDR,BLDSBAL-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATRM    DC    AL1(COLADAQ,L'CRACTDAT,CATRCDTQ,0)                     M         
         DC    AL2(CRACTDAT-CRD,ATSACDAT-ATSADDR,BLDSADAT-BLDSDSP)              
         DC    AL2(0)                                                           
*                                                                               
CATRN    DC    AL1(COLBTYQ,L'CRBTYPE,0,0)                             N         
         DC    AL2(CRBTYPE-CRD,ATSBTYPE-ATSADDR,BLDSBTYP-BLDSDSP)               
         DC    AL2(FBTTYPE-CONDAT)                                              
*                                                                               
CATRP    DC    AL1(COLPSTQ,L'CRPST,0,0)                               P         
         DC    AL2(CRPST-CRD,ATSPST-ATSADDR,BLDSPST-BLDSDSP)                    
         DC    AL2(FPST-CONDAT)                                                 
*                                                                               
CATRQ    DC    AL1(COLXSTQ,L'CRTSTAT,0,CATRSNLQ)                      Q         
         DC    AL2(CRTSTAT-CRD,ATSTSTAT-ATSADDR,BLDSXST-BLDSDSP)                
         DC    AL2(FSTAT-CONDAT)                                                
*                                                                               
CATRR    DC    AL1(COLUSEQ,L'CRCONDAT,CATRCDTQ,0)                     R         
         DC    AL2(CRCONDAT-CRD,ATSUSDAT-ATSADDR,BLDSCTRD-BLDSDSP)              
         DC    AL2(0)                                                           
*                                                                               
CATRS    DC    AL1(COLMINQ,L'CRMIN,CATRMOVQ,0)                        S         
         DC    AL2(CRMIN-CRD,ATSMIN-ATSADDR,BLDSMIN-BLDSDSP)                    
         DC    AL2(0)                                                           
*                                                                               
CATRT    DC    AL1(COLPERDQ,L'CRPERD,0,0)                             T         
         DC    AL2(CRPERD-CRD,ATSPERD-ATSADDR,BLDSPERD-BLDSDSP)                 
         DC    AL2(FPAYPER-CONDAT)                                              
*                                                                               
CATRU    DC    AL1(COLNARRQ,0,0,CATRSKPB)                             U         
         DC    AL2(CRTNARLN-CRD,ATSTNARR-ATSADDR,BLDSNARR-BLDSDSP)              
         DC    AL2(FNARR-CONDAT)                                                
*                                                                               
CATRV    DC    AL1(COLMMOSQ,L'CRMMOS,0,0)                             V         
         DC    AL2(CRMMOS-CRD,ATSMMOS-ATSADDR,BLDSMMOS-BLDSDSP)                 
         DC    AL2(FMOSDATE-CONDAT)                                             
*                                                                               
CATRW    DC    AL1(COLPDUEQ,L'CRDUDAT,CATRCDTQ,0)                     W         
         DC    AL2(CRDUDAT-CRD,ATSDUDAT-ATSADDR,BLDSPDUE-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATRY    DC    AL1(COLPDATQ,L'CRCHQDAT,CATRCDTQ,0)                    Y         
         DC    AL2(CRCHQDAT-CRD,ATSTCDAT-ATSADDR,BLDSCDAT-BLDSDSP)              
         DC    AL2(0)                                                           
*                                                                               
CATR#    DC    AL1(COLTSTAQ,L'CRTESTAT,CATRHEXQ,CATRSDI0)             #         
         DC    AL2(CRTESTAT-CRD,ATSTEST-ATSADDR,BLDSTSTA-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATRLT   DC    AL1(COLTSRFQ,L'CRKSREF,CATRBIZQ,CATRSDI0)              <         
         DC    AL2(CRKSREF-CRD,ATSKSREF-ATSADDR,BLDSTSRF-BLDSDSP)               
         DC    AL2(0)                                                           
*                                                                               
CATRPA   DC    AL1(COLDAQ,L'CRDA,CATRHEXQ,0)                          (         
         DC    AL2(CRDA-CRD,ATSDA-ATSADDR,BLDSDA-BLDSDSP)                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
*                                                                               
         DS    0D                                                               
       ++INCLUDE ACPRVTABN                                                      
         EJECT ,                                                                
***********************************************************************         
*        DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD                           
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,99,L'MX@RSCOC,0)              CONTRA     - 3          
         DC    AL2(MX@RSCOC-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTITOT+GCTIOVER,0,0,0)                             
         DC    AL1((CATR3-CATRTAB)/CATRLNQ,L'MX@ACCT)                           
         DC    AL2(MX@ACCT-OVERWRKD)                                            
GCT3LQ   EQU   *-GCTCOL3                                                        
*        -----------------------------------                                    
GCTCOL4  DC    AL1(GCT4LQ,03,L'MX@TRAND,0)        TRANS DATE       - 4          
         DC    AL2(MX@TRAND-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1((CATR4-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCT4LQ   EQU   *-GCTCOL4                                                        
*        -----------------------------------                                    
GCTCOL5  DC    AL1(GCT5LQ,04,L'LC@REF,0)                REFERENCE  - 5          
         DC    AL2(LC@REF-WORKD,ACONDAT-OVERWRKD)                               
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1((CATR5-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCT5LQ   EQU   *-GCTCOL5                                                        
*        -----------------------------------                                    
GCTCOL1  DC    AL1(GCT1LQ,02,L'LC@OFFC,0)               OFFICE     - 1          
         DC    AL2(LC@OFFC-WORKD,ACONDAT-OVERWRKD)                              
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1((CATR1-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCT1LQ   EQU   *-GCTCOL1                                                        
*        -----------------------------------                                    
GCTCOLE  DC    AL1(GCTEQ,06,L'MX@RSCNO,0)               CHECK NUMBE- E          
         DC    AL2(MX@RSCNO-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRE-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTEQ    EQU   *-GCTCOLE                                                        
*        -----------------------------------                                    
GCTCOLY  DC    AL1(GCTYLQ,07,L'MX@RSCDT,0)              CHECK DATE - Y          
         DC    AL2(MX@RSCDT-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1((CATRY-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTYLQ   EQU   *-GCTCOLY                                                        
*        -----------------------------------                                    
GCTCOLA  DC    AL1(GCTALQ,08,L'LC@CRS,0)                CREDITS    - A          
         DC    AL2(LC@CRS-WORKD,ACONDAT-OVERWRKD)                               
         DC    AL1(GCTIROUT+GCTITOT+GCTIRTOT,0,GCTFNUM+GCTFRGHT,0)              
         DC    AL1((CATRA-CATRTAB)/CATRLNQ,0),AL2(ACONTOT-OVERWRKD)             
GCTALQ   EQU   *-GCTCOLA                                                        
*        -----------------------------------                                    
GCTCOLJ  DC    AL1(GCTJLQ,20,L'MX@DISSL,0)              DISCOUNT   - J          
         DC    AL2(MX@DISSL-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTITOT+GCTIRTOT+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1((CATRJ-CATRTAB)/CATRLNQ,0),AL2(ACONTOT-OVERWRKD)             
GCTJLQ   EQU   *-GCTCOLJ                                                        
*        -----------------------------------                                    
GCTCOL8  DC    AL1(GCT8LQ,09,L'MX@NET,0)                NET        - 8          
         DC    AL2(MX@NET-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTIOVER+GCTITOT+GCTIRTOT,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1((CATR8-CATRTAB)/CATRLNQ,0),AL2(ACONTOT-OVERWRKD)             
GCT8LQ   EQU   *-GCTCOL8                                                        
*        -----------------------------------                                    
GCTCOLL  DC    AL1(GCTLLQ,28,L'MX@BAL,0)                BALANCE    - L          
         DC    AL2(MX@BAL-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTITOT+GCTIRTOT+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1((CATRL-CATRTAB)/CATRLNQ,0),AL2(ACONTOT-OVERWRKD)             
GCTLLQ   EQU   *-GCTCOLL                                                        
*        -----------------------------------                                    
GCTCOLQ  DC    AL1(GCTQLQ,10,L'LC@STT,0)                STATUS     - Q          
         DC    AL2(LC@STT-WORKD,ACONDAT-OVERWRKD)                               
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1((CATRQ-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTQLQ   EQU   *-GCTCOLQ                                                        
*        -----------------------------------                                    
GCTCOL7  DC    AL1(GCT7LQ,11,L'LC@BATS,0)               BATCH      - 7          
         DC    AL2(LC@BATS-WORKD,ACONDAT-OVERWRKD)                              
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1((CATR7-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCT7LQ   EQU   *-GCTCOL7                                                        
*        -----------------------------------                                    
GCTCOL6  DC    AL1(GCT6LQ,12,L'MX@MOA,0)                MOA        - 6          
         DC    AL2(MX@MOA-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTIOVER,GCTIMONO,GCTFDAT,0)                        
         DC    AL1((CATR6-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCT6LQ   EQU   *-GCTCOL6                                                        
*        -----------------------------------                                    
GCTCOLN  DC    AL1(GCTNLQ,13,L'MX@TYPE1,0)              BATCH TYPE - N          
         DC    AL2(MX@TYPE1-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRN-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTNLQ   EQU   *-GCTCOLN                                                        
*        -----------------------------------                                    
GCTCOLM  DC    AL1(GCTMLQ,14,L'MX@ACTYD,0)              ACTIVITY   - M          
         DC    AL2(MX@ACTYD-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFDAT,0)                               
         DC    AL1((CATRM-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTMLQ   EQU   *-GCTCOLM                                                        
*        -----------------------------------                                    
GCTCOLC  DC    AL1(GCTCLQ,15,L'MX@JOEST,0)            JOB/ESTIMATE- C           
         DC    AL2(MX@JOEST-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRC-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTCLQ   EQU   *-GCTCOLC                                                        
*        -----------------------------------                                    
GCTCOLI  DC    AL1(GCTILQ,16,L'MX@REC,0)                RECORD #   - I          
         DC    AL2(MX@REC-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRI-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTILQ   EQU   *-GCTCOLI                                                        
*        -----------------------------------                                    
GCTCOLK  DC    AL1(GCTKLQ,17,L'UP@VATL,0)               GST        - K          
         DC    AL2(UP@VATL-WORKD,ACONDAT-OVERWRKD)                              
         DC    AL1(GCTIROUT,0,GCTFNUM+GCTFRGHT,GCTICAN)                         
         DC    AL1((CATRK-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTKLQ   EQU   *-GCTCOLK                                                        
*        -----------------------------------                                    
GCTCOLP  DC    AL1(GCTPLQ,18,L'AC@PST,0)                PST        - P          
         DC    AL2(AC@PST-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTICAN)                
         DC    AL1((CATRP-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTPLQ   EQU   *-GCTCOLP                                                        
*        -----------------------------------                                    
GCTCOLR  DC    AL1(GCTRLQ,31,L'MX@OFFST,0)        OFFSET (USED DATE)- R         
         DC    AL2(MX@OFFST-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRR-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTRLQ   EQU   *-GCTCOLR                                                        
*        -----------------------------------                                    
GCTCOLS  DC    AL1(GCTSLQ,30,L'MX@INVCN,0)              INVOICE    - S          
         DC    AL2(MX@INVCN-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRS-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTSLQ   EQU   *-GCTCOLS                                                        
*        -----------------------------------                                    
GCTCOLT  DC    AL1(GCTTLQ,19,L'MX@PERD,0)               PERIOD     - T          
         DC    AL2(MX@PERD-OVERWRKD,ACONDAT-OVERWRKD)                           
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1((CATRT-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTTLQ   EQU   *-GCTCOLT                                                        
*        -----------------------------------                                    
GCTWC    DC    AL1(GWCLQ,GCWC,L'MX@WC,0)                WORKCODE   - Z          
         DC    AL2(MX@WC-OVERWRKD,ASPCGR-OVERWRKD)                              
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GWCLQ    EQU   *-GCTWC                                                          
*        -----------------------------------                                    
GCTCOLU  DC    AL1(GCTULQ,21,L'LC@RSNAR,0)              NARRATIVE  - U          
         DC    AL2(LC@RSNAR-WORKD,ACONDAT-OVERWRKD)                             
         DC    AL1(GCTIROUT,0,GCTFSIZ,0)                                        
         DC    AL1((CATRU-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTULQ   EQU   *-GCTCOLU                                                        
*        -----------------------------------                                    
GCTCOLV  DC    AL1(GCTVLQ,22,L'MX@MMOS,0)               MMOS      - V           
         DC    AL2(MX@MMOS-OVERWRKD,ACONDAT-OVERWRKD)                           
         DC    AL1(GCTIROUT+GCTIOVER,GCTIMONO,GCTFDAT,0)                        
         DC    AL1((CATRV-CATRTAB)/CATRLNQ,0),AL2(0)                            
GCTVLQ   EQU   *-GCTCOLV                                                        
*        -----------------------------------                                    
GCTDUEV  DC    AL1(GDUELQ,23,L'MX@RSDUD,0)              DUE DATE  - W           
         DC    AL2(MX@RSDUD-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFDAT,0)                               
         DC    AL1((CATRW-CATRTAB)/CATRLNQ,0),AL2(0)                            
GDUELQ   EQU   *-GCTDUEV                                                        
*        -----------------------------------                                    
GCTPYMET DC    AL1(GCTPMLQ,GCPYMET,L'MX@PAYME,0)   PAYMENT METHOD -27           
         DC    AL2(MX@PAYME-OVERWRKD,ASPCGR-OVERWRKD)                           
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GCTPMLQ  EQU   *-GCTPYMET                                                       
*        -----------------------------------                                    
GCTAMET  DC    AL1(GCTAMELQ,GCMETH,L'MX@APRVM,0)  APPROVAL METHOD - %           
         DC    AL2(MX@APRVM-OVERWRKD,ASPCGR-OVERWRKD)                           
         DC    AL1(GCTIROUT+GCTIOVER,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GCTAMELQ EQU   *-GCTAMET                                                        
*----------------------------------------------------------------------         
* DDS ONLY                                                                      
*----------------------------------------------------------------------         
GCTCOLLT DC    AL1(GCTLTLQ,24,L'MX@SEQ,0)                SEQUENCE NUMB          
         DC    AL2(MX@SEQ-OVERWRKD,ACONDAT-OVERWRKD)                            
         DC    AL1(GCTIROUT+GCTIOVER,GCTIDDS,0,0)                               
         DC    AL1((CATRLT-CATRTAB)/CATRLNQ,0),AL2(0)                           
GCTLTLQ  EQU   *-GCTCOLLT                                                       
*                                                                               
GCTCOLPA DC    AL1(GCTPALQ,25,L'AC@DSCAD,0)              DISK ADDR - (          
         DC    AL2(AC@DSCAD-OVERWRKD,ACONDAT-OVERWRKD)                          
         DC    AL1(GCTIROUT+GCTIOVER,GCTIDDS,0,0)                               
         DC    AL1((CATRPA-CATRTAB)/CATRLNQ,0),AL2(0)                           
GCTPALQ  EQU   *-GCTCOLPA                                                       
*----------------------------------------------------------------------         
         DC    AL1(EOT)                                                         
*----------------------------------------------------------------------         
* GRID COLUMN EQUATES                                                           
*----------------------------------------------------------------------         
GCCTR    EQU   99                  CONTRA               - 3                     
GCOFF    EQU   2                   OFFICE               - 1                     
GCDATE   EQU   3                   DATE                 - 4                     
GCREF    EQU   4                   REFERENCE            - 5                     
GCCHKNO  EQU   6                   CHECK NUMBER         - E                     
GCCHKDT  EQU   7                   CHECK DATE           - Y                     
GCCRS    EQU   8                   CREDITS              - A                     
GCNET    EQU   9                   NET                  - 8                     
GCDISC   EQU   20                  DISCOUNT             - J                     
GCSTAT   EQU   10                  STATUS               - Q                     
GCBAT    EQU   11                  BATCH                - 7                     
GCMOA    EQU   12                  MOA                  - 6                     
GCBTYP   EQU   13                  BATCH TYPE           - N                     
GCACTD   EQU   14                  ACTIVITY             - M                     
GCJOBES  EQU   15                  JOB/ESTIMATE         - C                     
GCRECN   EQU   16                  RECORD #             - I                     
GCGST    EQU   17                  GST                  - K                     
GCPST    EQU   18                  PST                  - P                     
GCOFFSET EQU   31                  OFFSET (USED DATE)   - R                     
GCMINV   EQU   30                  MEDIA INVOICE        - S                     
GCPER    EQU   19                  PERIOD               - T                     
GCNARR   EQU   21                  NARRATIVE            - U                     
GCMMOS   EQU   22                  MMOS                 - V                     
GCDUDTE  EQU   23                  DUE DATE             - W                     
GCSEQ    EQU   24                  SEQUENCE NUMB                                
GCDADDR  EQU   25                  DISK ADDR            - (                     
GCWC     EQU   26                  WORKCODE                                     
GCPYMET  EQU   27                  PAYMENT METHOD                               
GCMETH   EQU   35                  APPROVAL METHOD                              
***********************************************************************         
*        EQUATES                                                      *         
***********************************************************************         
CHEQUEQ  EQU   X'81'                                                            
B37VOIDQ EQU   C'V'                                                             
MBVOFFSQ EQU   3                   OFFSET TO BANK VOID 127+ TRANS IND           
MBVINDIQ EQU   C'*'                OFFSET TO BANK VOID 127+ TRANS IND           
         EJECT ,                                                                
***********************************************************************         
*        OVERLAY WORK AREA DSECT                                      *         
***********************************************************************         
OVERWRKD DSECT                                                                  
ACRBLOCK DS    A                   A(CREDITOR BLOCK)                            
*                                                                               
ATSADDR  DS    0A                  A(DATA ELEMENTS ON TSAR RECORD)              
ATSOFF   DS    A                 1 A(OFFICE CODE)                               
ATSCONT  DS    A                 3 A(CONTRA ACCOUNT)                            
ATSTDATE DS    A                 4 A(TRANSACTION DATE)                          
ATSTREF  DS    A                 5 A(TRANSACTION REFERENCE)                     
ATSMOS   DS    A                 6 A(TRANSACTION MOS)                           
ATSBREF  DS    A                 7 A(BATCH REFERNCE)                            
ATSAMT   DS    A                 8 A(NET AMOUNT)                                
ATSCRAMT DS    A                 A A(CREDIT AMOUNT)                             
ATSJOB   DS    A                 C A(JOB ACCOUNT)                               
ATSSRC   DS    A                 D A(SOURCE ACCOUNT)                            
ATSCHEQU DS    A                 E A(CHEQUE NUMBER)                             
ATSTSRNO DS    A                 I A(TSAR RECORD NUMBER)                        
ATSDISPY DS    A                 J A(DISCOUNT)                                  
ATSGST   DS    A              US K A(CANADIAN GST)                              
ATSBAL   DS    A                 L A(BALANCE)                                   
ATSACDAT DS    A                 M A(ACTIVITY DATE)                             
ATSBTYPE DS    A                 N A(BATCH TYPE)                                
ATSPST   DS    A              US P A(CANADIAN PST TYPE/AMOUNT)                  
ATSTSTAT DS    A                 Q A(TRAN ELE STAT AND REC STAT)                
ATSUSDAT DS    A                 R A(USED DATE)                                 
ATSMIN   DS    A              US S A(MEDIA INV NUMBER)                          
ATSPERD  DS    A              US T A(MEDIA PAYMENT PERIOD)                      
ATSTNARR DS    A                 U A(NARRATIVE)                                 
ATSMMOS  DS    A              US V A(MEDIA MOS)                                 
ATSDUDAT DS    A              US W A(PAYMENT DUE DATE)                          
ATSTCDAT DS    A              US Y A(CHEQUE DATE)                               
ATSTEST  DS    A                 # A(TRANSACTION ELE STATUS)                    
ATSKSREF DS    A                 < A(TRANSACTION RECORD SUB REFERENCE)          
ATSDA    DS    A                 ( A(DISK ADDRESS OF TRANSACTION REC)           
ATSLNQ   EQU   *-ATSADDR                                                        
*                                                                               
ORELO    DS    A                                                                
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
ACONDAT  DS    A                   ADDRESS OF GRID DATA FORMAT ROUTINE          
ACONTOT  DS    A                   ADDRESS OF GRID TOTAL FORMAT ROUT            
ACATRTAB DS    A                   ADDRESS OF COLUMN ATTRIBURE TABLE            
ANXTLINE DS    A                   ADDRESS OF NEXT DUMMY LINE                   
ASPCGR   DS    A                   ADDRESS OF SPECIAL GRID COL ROUTINE          
CUREXC   DS    A                   A(EXCHANGE RATE RECORD)                      
*                                                                               
RECVALS  DS    0X                  RECONCILED FILTER VALUES                     
RECOFF   DS    CL(L'TRNOFFC)       OFFICE CODE                                  
RECNO    DS    CL(L'MPYNO)         PAYMENT NUMBER                               
RECDATE  DS    CL(L'MPYDTE)        PAYMENT DATE                                 
RECBANK  DS    CL(L'MPYBNK)        BANK ACCOUNT                                 
RECVLLNQ EQU   *-RECVALS                                                        
*                                                                               
BALANCE  DS    PL8                 BALANCE                                      
DISCOUNT DS    PL(L'DISTOT)        DISCOUNT AMOUNT                              
SVCHQDAT DS    CL(L'MPYDTE)        SAVED CHEQUE DATE                            
DADDRESS DS    XL(L'TRNKDA)        DISK ADDRESS                                 
DRREF    DS    CL(L'TRNKREF)       REF ON DR WHEN 127+ TRANS                    
*                                                                               
TEMPNUM  DS    PL8                 TEMPORARY NUMBER STORAGE                     
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
*                                                                               
ENQFLAG  DS    XL1                 LOCAL FIS SWITCHES                           
TFULL1ST EQU   X'80'               FIRST TIME TSAR FULL                         
*                                                                               
OTHFLAG  DS    XL1                 OTHERS FLAG                                  
OTHELEQ  EQU   X'01'               OTHERS ELEMENT FOUND                         
OTHRQDQ  EQU   X'02'               OTHERS ELEMENT REQUIRED                      
*                                                                               
DUEFLAG  DS    XL1                 DUE DATE FLAG                                
DUEELEQ  EQU   X'01'               DUE DATE ELEMENT FOUND                       
DUERQDQ  EQU   X'02'               DUE DATE ELEMENT REQUIRED                    
*                                                                               
AFPFLAG  DS    XL1                 ARTISTE FEE FLAG                             
AFPELEQ  EQU   X'01'               ARTISTE FEE ELEMENT FOUND                    
AFPRQDQ  EQU   X'02'               ARTISTE FEE ELEMENT REQUIRED                 
*                                                                               
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
TRANAMNT DS    CL(L'TRNAMNT)       AGENCY/LOCAL CURRENCY AMOUNT                 
TRANACT  DS    CL(L'TRNAMNT)       AGENCY AMOUNT                                
*                                                                               
CURFLAG  DS    XL1                 CURRENCY FLAG                                
CURDEF   EQU   X'80'               DEFAULT                                      
CURSET   EQU   X'40'               SET                                          
CURNA    EQU   X'20'               NORTH AMERICA                                
*                                                                               
MXPFLAG  DS    XL1                 MEDIA  EXTRA PAYMENT ELEMENT FLAG            
MXPELEQ  EQU   X'01'               MXPELD ELEMENT FOUND                         
MXPRQDQ  EQU   X'02'               MXPELD ELEMENT REQUIRED                      
**********************************************************************          
* DATA DICTIONARY                                                               
**********************************************************************          
DSMIX    DS    0C                                                               
MX@ENH14 DS    CL(L'ENQDAT1)                                                    
MX@ENH15 DS    CL(L'ENQDAT1)                                                    
MX@ACC   DS    CL9                                                              
MX@DRAFT DS    CL10                                                             
MX@CTR   DS    CL9                                                              
MX@PYMNT DS    CL10                                                             
MX@CHK   DS    CL10                                                             
MX@DATE  DS    CL10                                                             
MX@BNK   DS    CL10                                                             
MX@CHKTO DS    CL10                                                             
MX@PAYTO DS    CL10                                                             
MX@GROSS DS    CL7                                                              
MX@NET   DS    CL7                                                              
MX@FLTX  DS    CL7                                                              
MX@ORDER DS    CL7                                                              
MX@SUBR  DS    CL7                                                              
MX@CMRCL DS    CL7                                                              
MX@JOB   DS    CL7                                                              
MX@EXP   DS    CL7                                                              
MX@SRC   DS    CL7                                                              
MX@DUEDT DS    CL8                                                              
MX@SERNO DS    CL6                                                              
MX@RUNNO DS    CL7                                                              
MX@FACC  DS    CL7                                                              
MX@ATTR  DS    CL9                                                              
MX@ATHED DS    CL4                                                              
MX@UATH  DS    CL6                                                              
MX@HELD  DS    CL10                                                             
MX@SELED DS    CL10                                                             
MX@APRVD DS    CL10                                                             
MX@HOURS DS    CL5                                                              
MX@TYPE  DS    CL2,CL1                                                          
MX@STT   DS    CL2,CL1                                                          
MX@OTHER DS    CL2,CL1                                                          
MX@UDAT  DS    CL2,CL1                                                          
MX@PELDT DS    CL2,CL1                                                          
MX@INVS  DS    CL8                                                              
MX@DRS   DS    CL8                                                              
MX@BAL   DS    CL7                                                              
MX@NETPD DS    CL8                                                              
MX@SEQ   DS    CL2,CL1                                                          
MX@ACCT  DS    CL15                                                             
MX@ACCTS DS    CL15                                                             
MX@CHKC  DS    CL20                                                             
MX@DPSON DS    CL20                                                             
MX@OFFSO DS    CL20                                                             
MX@WRTFA DS    CL20                                                             
MX@XFRFR DS    CL20                                                             
MX@XFRTO DS    CL20                                                             
MX@DATED DS    CL10                                                             
MX@MOS   DS    CL10                                                             
MX@CTRD  DS    CL10                                                             
MX@AMT   DS    CL10                                                             
MX@DISS  DS    CL4                                                              
MX@YES   DS    CL4                                                              
MX@MRGED DS    CL7                                                              
MX@DISSL DS    CL8                                                              
*-----------------------------------------                                      
* GRIDS COLUMN HEADINGS                                                         
*-----------------------------------------                                      
MX@ACTYD DS    CL13              ACTIVITY DATE                                  
MX@PERD  DS    CL6               PERIOD                                         
MX@REC   DS    CL6               RECORD                                         
MX@RSCDT DS    CL10              CHECK DATE                                     
MX@RSCNO DS    CL12              CHECK NUMBER                                   
MX@JOEST DS    CL12              JOB/ESTIMATE                                   
MX@RSCOC DS    CL14              CONTRA ACCOUNT                                 
MX@RSDUD DS    CL8               DUE DATE                                       
MX@MOA   DS    CL3               MOA                                            
MX@OFFST DS    CL6               OFFSET                                         
MX@WC    DS    CL8               WORKCODE                                       
MX@TYPE1 DS    CL4               TYPE                                           
MX@MEDC  DS    CL6               MEDIA                                          
MX@INVCN DS    CL14              INVOICE NUMBER                                 
MX@MMOS  DS    CL9               MEDIA MOS                                      
MX@TRAND DS    CL10              TRANS DATE                                     
MX@PAYME DS    CL14              PAYMENT METHOD                                 
MX@APRVM DS    CL15              APPROVAL METHOD                                
MX@AAPP  DS    CL12              AUTO APPROVED                                  
MX@MAPP  DS    CL6               MARKER APPROVED                                
*-----------------------------------------                                      
DSCAP    DS    0C                                                               
AC@ONT   DS    CL3                                                              
AC@PST   DS    CL3                                                              
AC@QST   DS    CL3                                                              
AC@HST   DS    CL3                                                              
AC@DSCAD DS    CL3                                                              
AC@HBC   DS    CL6                                                              
AC@HAL   DS    CL6                                                              
AC@HSA   DS    CL6                                                              
AC@HMA   DS    CL6                                                              
AC@HPQ   DS    CL6                                                              
AC@HON   DS    CL6                                                              
AC@HPE   DS    CL6                                                              
AC@HNB   DS    CL6                                                              
AC@HNS   DS    CL6                                                              
AC@HNF   DS    CL6                                                              
         EJECT ,                                                                
***********************************************************************         
*        SCREEN ITEM LINES                                            *         
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CONT DS    CL(L'TRNKULA)       CONTRA CODE                                  
         DS    CL1                                                              
SCR1TDAT DS    CL8                 TRANSACTION DATE                             
         DS    CL1                                                              
SCR1TREF DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL1                                                              
SCR1BREF DS    CL6                 BATCH REFERENCE                              
         DS    CL1                                                              
SCR1OFF  DS    CL2                 OFFICE                                       
         DS    CL1                                                              
SCR1DESC DS    CL21                DECRIPTION                                   
         ORG   SCR1DESC+24                                                      
SCR1HRT  DS    0CL5                HOURS TITLE                                  
         ORG   SCR1DESC+L'SCR1DESC+1                                            
SCR1AMNT DS    CL13                AMOUNT                                       
SCR1SIGN DS    CL2                 DR/CR                                        
SCR1LNQ  EQU   *-SCRLIN1D                                                       
*                                                                               
SCRTOTD  DSECT                     COVER SCREEN TOTAL LINE                      
SCRTAMT  DS    CL13                NET AMOUNT                                   
         DS    CL1                                                              
SCRTDIS  DS    CL13                DISCOUNT AMOUNT                              
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@ACCT)                                                    
         DS    CL36                                                             
SCRHTOT  DS    CL12                HOURS TOTAL                                  
         DS    CL1                                                              
SCRTTOT  DS    CL12                TRANS TOTAL                                  
         EJECT                                                                  
***********************************************************************         
*        TSAR RECORD DSECTS                                           *         
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
*                                                                               
TSDZEROS DS    CL1                 COL FLD = 0, SO NOT ADDED TO TSAR            
TSDZTXST EQU   X'80'               .   TRANSACTION ELEMENT STATUS   - #         
TSDZTXSR EQU   X'40'               .   TRANSACTION SUB-REFERENC NUM - <         
TSDZGSTQ EQU   X'20'               .   GST                          - K         
TSDZPSTQ EQU   X'10'               .   PST                          - P         
*                                                                               
TSDDRBFW DS    PL8                 DEBITS CARRIED FORWARD                       
TSDCRBFW DS    PL8                 CREDITS CARRIED FORWARD                      
TSDDIBFW DS    PL8                 DISCOUNT CARRIED FORWARD                     
TSDDR    DS    PL(L'CRDRAMT)       NET DEBITS FOR THIS ITEM                     
TSDCR    DS    PL(L'CRCRAMT)       CREDITS FOR THIS ITEM                        
TSDDI    DS    PL(L'CRDISC)        DISCOUNT THIS ITEM                           
TSDDAT   DS    0X                  COLUMN DATA                                  
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSDCOLD  DSECT                                                                  
TSDCTYP  DS    X                                                                
TSDCLEN  DS    X                                                                
TSDCDAT  DS    0XL200                                                           
TSDCLNQ  EQU   *-TSDCOLD                                                        
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*        COLUMN ATTRIBUTE TABLE                                       *         
***********************************************************************         
*                                                                               
CATRTABD DSECT                     COLUMN ATTRIBUTE TABLE                       
CATRCHR  DS    X                   COLUMN CHARACTER                             
CATRWID  DS    X                   COLUMN DATA LENGTH 0 MEANS VARIABLE          
*                                                                               
CATRDTYP DS    X                   DISPLAY TYPE                                 
CATRMOVQ EQU   X'80'               STRAIGHT MOVE                                
CATRPDTQ EQU   X'40'               PACKED DATE                                  
CATRCDTQ EQU   X'20'               COMPRESSED DATE                              
CATRMDTQ EQU   X'10'               MMMYY DATE                                   
CATRPAMQ EQU   X'08'               STNDRD 6 BYTE PACKED FLD ZERO=BLANK          
CATRHEXQ EQU   X'04'               HEX OUTPUT                                   
CATRPAZQ EQU   X'02'               STNDRD 6 BYTE PACKED FLD ZERO=ZERO           
CATRBIZQ EQU   X'01'               BINARY 1, 2, 3, OR 4 FLD                     
*                                                                               
CATRSTA1 DS    X                   STATUS BYTE                                  
CATRSNLQ EQU   X'80'               INCLUDE NULL ENTRIES                         
CATRSGWD EQU   X'40'               GET  OUTPUT WIDTH (NOT STANDARD)             
CATRSKPB EQU   X'20'               SKIP IF BLANK                                
CATRSDI0 EQU   X'08'               ZERO EQUALS ZERO                             
CATRSMTR EQU   X'04'               FOR  NEGATIVE, TRAILING MINUS SIGN           
CATRSMLE EQU   X'02'               FOR  NEGATIVE, LEADING  MINUS SIGN           
*                                                                               
CATRDISP DS    AL2                 DISP TO DATA IN CRBLOCK ENTRY                
CATRTSAD DS    AL2                 DISP TO TSAR DATA ELEMENT ADDRESS            
CATRDLIN DS    AL2                 DISP TO DISP ON DISPLAY LINE                 
CATRFROU DS    AL2                 DISP TO FORMAT ROUTINE IF NON ZERO           
CATRLNQ  EQU   *-CATRTABD                                                       
         EJECT ,                                                                
***********************************************************************         
*        FIS WORK AREA                                                *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACENQWORK                                                      
         EJECT ,                                                                
***********************************************************************         
*        OVERLAY SAVE AREA IN THE TWA                                 *         
***********************************************************************         
*                                                                               
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
CRFLAG   DS    X                   CREDITOR FLAG                                
CRFULLQ  EQU   X'80'               CRBLOCK FULL                                 
CRSFULLQ EQU   X'40'               SCREEN FULL                                  
CRKEY    DS    CL(L'TRNKEY-1)      SAVED CREDIT KEY                             
UNILDG   DS    CL2                 UNIT AND LEDGER                              
DETFLAG  DS    X                                                                
DETFENDQ EQU   X'10'               EOD FOR GRIDS                                
RECCOUNT DS    H                   RECORD COUNT                                 
PAYTYP   DS    X                   PAYMENT TYPE                                 
CRVALS   DS    0PL8                CREDITOR VALUES                              
DEBTOT   DS    PL8                 TOTAL DEBITS                                 
CRETOT   DS    PL8                 TOTAL CREDITS                                
DISTOT   DS    PL8                 DISCOUT TOTAL                                
DEBITS   DS    PL8                 INVOICE DEBITS                               
CREDITS  DS    PL8                 INVOICE CREDITS                              
CRVALLNQ EQU   *-CRVALS                                                         
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*        DDCUREDIT PARAMETER BLOCK                                    *         
***********************************************************************         
*                                                                               
       ++INCLUDE DDCUREDITD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACENQ01   10/16/18'                                      
         END                                                                    
