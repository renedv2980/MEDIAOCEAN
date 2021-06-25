*          DATA SET ACENQ15    AT LEVEL 006 AS OF 10/19/16                      
*PHASE T62015A                                                                  
*INCLUDE VATICAN                                                                
*INCLUDE BINSRCH                                                                
T62015   TITLE 'ACCOUNT ENQUIRY - JOB CASHFLOW'                                 
T62015   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,*ENQ15**,R6,R7,CLEAR=YES,RR=RE                                 
         USING TWAD,RA                   RA=A(TWA)                              
         USING WORKD,R9                  R9=A(GLOBAL WORKING STORAGE)           
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)              R8=A(LOCAL WORKIN STORAGE)             
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE                     SET UP GRID COLUMN TABLE               
         ST    RF,AGCTBL                                                        
         L     RF,=A(GRDSP)                                                     
         AR    RF,RE                     SET UP GRID COLUMN TABLE               
         ST    RF,AGRDSP                                                        
*                                                                               
         LA    RF,PRVTAB                                                        
         ST    RF,APRVTAB                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
         GOTO1 VDICTATE,DMCB,C'L   ',DCCAP,DSCAP                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ         FIRST TIME FOR DISPLAY?                
         BO    MAIN10                    . NO                                   
         BAS   RE,FSTDIS                 FIRST DISPLAY FUNCTIONS                
         BNE   MAINERX                                                          
         TM    DISPFLAG,NORECQ           NO TRANSACTIONS ON ACCOUNT?            
         BO    MAINX                                                            
         TM    DISPFLAG,DISIOMAX         MAX IO'S?                              
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ         SET NOT FIRST TIME FLAG ON             
MAIN08   BAS   RE,TSARNOW                                                       
         BNE   MAINERX                                                          
         B     MAIN65                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE          GRIDS PROCESSING FINISHED?             
         BO    MAINXGX                                                          
         TM    DISPFLAG,NORECQ           NO RECORDS TO DISPLAY?                 
         BZ    *+12                                                             
         TM    OVRSTAT,OVRREVQ           REVERSAL?                              
         BZ    MAINX                                                            
         TM    DISPFLAG,ALLREADQ         ALL RECORDS READ?                      
         BO    MAIN20                    . YES                                  
*                                                                               
         OI    JDTFLAG,JDTRENQ                                                  
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN18                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     MAINX                                                            
*                                                                               
MAIN18   TM    JDTFLAG,JDTALLQ                                                  
         BZ    MAIN08                                                           
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC         ALREADY HAVE RECORD IN TSAR?           
         BH    MAIN50                    . NO                                   
*-----------------------------------                                            
* DISPLAY RECORD ALREADY BUILT                                                  
*-----------------------------------                                            
MAIN30   L     R0,ATSARREC               SAVE DUMMY TSAR RECORD                 
         LHI   R1,TSARRECL                                                      
         L     RE,ASVTSAR                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,FGRMTSAR                                                      
         B     MAIN110                                                          
*-----------------------------------                                            
MAIN50   TM    DISPFLAG,ALLREADQ         HAVE ALL RECORDS BEEN READ?            
         BO    MAINX                     . YES                                  
         TM    JDTFLAG,JDTRENQ           RE-ENTERING                            
         BZ    MAIN60                    . YES                                  
         NI    JDTFLAG,X'FF'-JDTRENQ                                            
         TM    JDTFLAG,JDTPROCQ          Record already processed               
         BZ    MAIN70                                                           
*                                                                               
MAIN60   NI    JDTFLAG,X'FF'-JDTPROCQ    TRANSACTION PROCESSED                  
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN64                                                           
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BZ    *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN64   MVC   KEYSAVE,IOKEY                                                    
         B     MAIN70                                                           
*                                                                               
         USING TEMPD,R1                                                         
         USING TRNRECD,R3                                                       
MAIN65   TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         L     R1,ATEMPBLK                                                      
         B     MAIN78                                                           
*                                                                               
MAIN70   SR    R0,R0                                                            
         ICM   R0,3,SVTEMPN                                                     
         MHI   R0,TEMPLNQ                                                       
         L     R1,ATEMPBLK                                                      
         AR    R1,R0                                                            
*                                                                               
         LA    R3,IOKEY                                                         
         CLC   TRNKCULA,IOKEYSAV                                                
         BNE   MAIN75                                                           
         CLC   TRNKWORK,TEMPWC                                                  
         BNE   MAIN75                                                           
         CLC   TRNKULC,TEMPCONT                                                 
         BNE   MAIN75                                                           
         CLC   TRNKDATE,TEMPIDT                                                 
         BNE   MAIN75                                                           
         CLC   TRNKREF,TEMPINV                                                  
         BNE   MAIN75                                                           
         B     MAIN90                                                           
*                                                                               
MAIN75   SR    R0,R0                                                            
         ICM   R0,3,SVTEMPN                                                     
         AHI   R0,1                      NEXT CONTRA TABLE NUMBER               
         STCM  R0,3,SVTEMPN                                                     
         MHI   R0,TEMPLNQ                DISPLACEMENT TO ENTRY                  
         L     R1,ATEMPBLK                                                      
         AR    R1,R0                     R1=A(CONTRA TABLE ENTRY)               
*                                                                               
MAIN78   CLC   SVTEMPN,TEMPBLKN                                                 
         BH    MAIN120                                                          
*                                                                               
         LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   TRNKCULA,IOKEYSAV                                                
         MVC   TRNKWORK,TEMPWC                                                  
         MVC   TRNKCCPY,MYCO                                                    
         MVC   TRNKULC,TEMPCONT                                                 
         MVC   TRNKDATE,TEMPIDT                                                 
         MVC   TRNKREF,TEMPINV                                                  
         DROP  R1                                                               
                                                                                
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    MAIN64                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     MAINX                                                            
*----------------------------------                                             
MAIN90   LA    R3,IOKEY                                                         
         CLC   TRNKDATE,SPACES           TRANSACTION RECORD?                    
         BNH   MAIN60                                                           
         BAS   RE,FILTKEY                APPLY FILTERING TO TRANS KEY           
         BNE   MAIN60                    KEEP THIS RECORD?                      
         MVC   DADDRESS,TRNKDA           DISK ADDRESS                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN95                                                           
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN95   L     R3,AIO1                   R3=A(IOAREA1 CONTAINING REC)           
         BAS   RE,FILTER                 APPLY FILTERING TO TRANS REC           
         BNE   MAIN60                    KEEP THIS RECORD?                      
         MVC   SAVEWC,TRNKWORK                                                  
*                                                                               
         CLC   TRNKWORK,PREVBILL         BILL?                                  
         BNE   MAIN98                    . NO                                   
         CLI   NEEDSUB,NESUBDRQ          NEED CHARGE SUBTOTAL?                  
         BNE   MAIN98                    . NO                                   
         BAS   RE,SUBTOT                                                        
         BNE   MAINX                                                            
*                                                                               
MAIN98   BAS   RE,BLDTSDAT               BUILD TSAR RECORD                      
         BNE   MAIN60                                                           
         TM    DISPFLAG,DISIOMAX         MAX IO'S?                              
         BO    MAINX                                                            
         OI    JDTFLAG,JDTPROCQ          TRANSACTION PROCESSED                  
*                                                                               
         CLC   TSCURRNO,TSNEXTST         DISPLAY THIS RECORD?                   
         BNL   MAIN110                                                          
         ICM   RF,3,TSCURRNO             UPDATE TSAR RECORD COUNTER             
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN50                                                           
*                                                                               
MAIN110  GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         BNE   MAINX                     SCREEN IS FULL                         
         MVC   TSLSTLIN,TSCURRNO         INCREMENT CURRENT TSAR REC NUM         
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*-----------------------------------                                            
* CHECK IF SUBTOTALS NEEDED                                                     
*-----------------------------------                                            
         L     R1,ATSARREC               R2=A(TSAR RECORD)                      
         USING TSARRECD,R1                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R1,TSARDATA                                                      
         USING TSARDATD,R1                                                      
*                                                                               
         CLI   TSDFMT,TSITEM1            DISPLAY ITEM?                          
         BE    MAIN116                   . YES                                  
         CLI   TSDFMT,TSTOTITM           TOTAL ITEM?                            
         BE    MAINX                     . YES                                  
         MVI   NEEDSUB,0                 FOR SUBTOTALS CLEAR NEEDSUB            
         TM    DISPFLAG,ALLREADQ         ALL RECORDS READ                       
         BO    MAIN140                   . YES                                  
         B     MAIN20                                                           
MAIN116  TM    TSDTEST,TRNSDR            DEBIT?                                 
         BO    *+12                      . NO                                   
         MVI   NEEDSUB,NESUBCRQ          NEED CREDIT SUBTOTAL                   
         B     MAIN20                                                           
         MVI   NEEDSUB,NESUBDRQ          NEED DEBIT SUBTOTAL                    
         B     MAIN20                                                           
         DROP  R1                                                               
*-----------------------------------                                            
MAIN120  OC    TSLSTREC,TSLSTREC         ANY RECORDS DISPLAYED SO FAR?          
         BNZ   MAIN130                                                          
         OI    DISPFLAG,NORECQ           NO RECORDS TO DISPLAY                  
         TM    OVRSTAT,OVRREVQ           ACCOUNT CONTAIN REVERSALS?             
         BNO   MAINX                                                            
*                                                                               
MAIN130  OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,SUBTOT                                                        
         BNE   MAINX                                                            
MAIN140  BAS   RE,TOTAL                  DEAL WITH TOTAL LINE                   
         B     MAINX                                                            
*                                                                               
MAINX    J     OKXIT                                                            
MAINERX  J     ERXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),0                                 
         GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         JE    OKXIT                     SCREEN IS FULL                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         LA    R1,JCVALS                                                        
         LHI   R0,(JCVALLNQ/L'JCVALS)                                           
FSTD02   ZAP   0(L'JCVALS,R1),=P'0'                                             
         LA    R1,L'JCVALS(R1)                                                  
         BCT   R0,FSTD02                                                        
*                                                                               
         XC    TEMPBLKN,TEMPBLKN                                                
         XC    SVTEMPN,SVTEMPN                                                  
         MVI   CURRFLAG,0                INIT CURRENCY FLAG                     
         MVC   CURRLAST,SPACES           LAST CURRENCY CODE                     
         MVI   JDTFLAG,0                 INIT JOB DETAIL FLAG                   
         MVI   COMBVATR,EOTFF            INTIT COMBINED VAT RATE                
         MVC   SAVEWC,SPACES             SAVED WC                               
         LA    RF,PREVBTOT               PREV BILLING TOTALS                    
         LA    R0,PREVBNMQ               # OF PREV BILL ACCUMULATORS            
         ZAP   0(L'PREVBTOT,RF),=P'0'                                           
         LA    RF,L'PREVBTOT(RF)                                                
         BCT   R0,*-10                                                          
         MVC   CONTRA,SPACES             CLEAR CONTRA CODE                      
         MVI   CONTLEN,0                 INPUT CONTRA CODE LENGTH               
         MVI   NEGCONT,0                 NEGATIVE CONTRA FILTER                 
         MVI   NEEDSUB,0                                                        
         MVI   INTFLAG,0                                                        
*                                                                               
         MVC   SVCACN,SPACES             AND CONTRA NAME                        
         LA    R2,BASCACH                R2=A(CONTRA ACCOUNT FIELD)             
         USING FLDHDRD,R2                                                       
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN              ANYTHING INPUT?                        
         BZ    FSTD10                                                           
         LA    RE,FLDDATA                                                       
         CLI   0(RE),NEGFILTR            NEGATIVE FILTER?                       
         BNE   FSTD05                                                           
         MVI   NEGCONT,NEGFILTR                                                 
         LA    RE,1(RE)                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SH    RF,=H'1'                                                         
         BZ    FSTDERR                   NO ACCOUNT ENTERED?                    
FSTD05   STC   RF,CONTLEN                LENGTH OF CONTRA CODE INPUT            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA(0),0(RE)                                                  
         GOTO1 ACNAME                    ATTEMPT TO FIND CONTRA NAME            
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,SPROUL,ACOMFACS,(0,0)             
         LA    R2,BASKEYH                R2=A(KEY FIELD)                        
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN              R4=L'(KEY FIELD INPUT)                 
         BZ    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   FLDILEN,L'ACTKULA         ENSURE LENGTH NOT TOO LONG             
         BH    FSTDERR                                                          
         GOTO1 AUNITLDG,SPROUNIT         READ UNIT/LEDGER RECORDS               
         BNE   FSTDERR                                                          
         TM    DISPFLAG,DISIOMAX         MAX IO'S                               
         BO    FSTDX                                                            
         MVC   FVMSGNO,=AL2(EAWRNGLV)                                           
         LA    RF,LEDGTLVA               RF=A(HIERARCHY LEVELS)                 
         LR    RE,RF                     RE=A(HIERARCHY LEVELS)                 
         SR    R1,R1                     R1=MINIMUM LENGTH OF ACCOUNT           
         CLI   0(RF),X'0C'               FULL HIERARCHY?                        
         BE    *+12                                                             
         LA    RF,1(RF)                  NOPE BUMP RF                           
         B     *-12                                                             
         CR    RF,RE                     YEP IS IT JUST ONE LEVEL?              
         BE    *+10                                                             
         BCTR  RF,0                      NO THEN GET MIN LENGTH                 
         IC    R1,0(RF)                                                         
         LR    RE,R4                     RE=L'(INPUT)                           
         CR    RE,R1                     IS INPUT LONG ENOUGH?                  
         BNH   FSTDERR                   NOPE, WRONG LEVEL ACCOUNT              
         BCTR  R4,0                                                             
         LA    R3,IOKEY                  R3=A(KEY FOR LOWLEVEL ACCOUNT)         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FLDDATA                                               
         MVC   FVMSGNO,=AL2(EAIFNTFN)                                           
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    FSTD20                                                           
         TM    IOERR,IOMAX                                                      
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     FSTDX                                                            
         TM    IOERR,IOERNF                                                     
         BO    FSTDERR                   LOW LEVEL ACCOUNT NOT FOUND            
         DC    H'0'                                                             
*                                                                               
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC)                                           
         GOTO1 AOFFACC                                                          
         BNE   FSTDERR                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    FSTDX                                                            
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
*                                                                               
         L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
         MVC   ACCNAM,SPACES                                                    
FSTD30   CLI   0(RF),EOR                 END OF RECORD?                         
         BE    FSTD50                                                           
         CLI   0(RF),NAMELQ              NAME ELEMENT?                          
         BE    FSTD40                                                           
         CLI   0(RF),JOBELQ              JOB ELEMENT?                           
         BE    FSTD45                                                           
*                                                                               
FSTD35   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD30                                                           
*                                                                               
         USING NAMELD,RF                                                        
FSTD40   SR    RE,RE                     GET ACCOUNT NAME                       
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAM(0),NAMEREC         ACCOUNT NAME                           
         LA    RE,1(RE)                                                         
         STC   RE,ACCNAMLN                                                      
         B     FSTD35                                                           
*                                                                               
         USING JOBELD,RF                 JOB ELEMENT                            
FSTD45   MVC   JOBSTAT,JOBSTA1           JOB STATUS                             
         B     FSTD35                                                           
         DROP  RF                                                               
*                                                                               
FSTD50   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
         L     RF,ADISPFK                                                       
         BASR  RE,RF                     DISPLAY PFKEY LINE                     
         DROP  R2                                                               
*                                                                               
         LA    R3,IOKEY                  READ FIRST CONTRA RECORD               
         USING TRNRECD,R3                                                       
         MVC   TRNKCCPY,MYCO                                                    
         L     R2,AOPTVALS               R2=A(OPTION VALUES)                    
         USING OPTVALSD,R2                                                      
         OC    OWCODE,OWCODE                                                    
         BZ    FSTD70                                                           
         CLI   OWCODEFI,NEGFILTR                                                
         BE    FSTD70                                                           
         MVC   TRNKWORK,OWCODEVL                                                
FSTD70   GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    FSTD80                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     FSTDX                                                            
*                                                                               
FSTD80   LA    R3,IOKEY                  R3=A(RECORD IO IOAREA 1)               
         LA    RF,L'TRNKCULA                                                    
         OC    OWCODE,OWCODE                                                    
         BZ    FSTD90                                                           
         CLI   OWCODEFI,NEGFILTR                                                
         BE    FSTD90                                                           
         LA    RF,L'TRNKWORK(RF)                                                
FSTD90   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKEY(0),IOKEYSAV                                               
         BE    *+12                                                             
         OI    DISPFLAG,NORECQ           NO RECORDS ON JOB                      
         B     FSTDX                                                            
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   FSTDX                                                            
         CLI   OALLTRAN,C'Y'             SHOW ALL TRANSACTIONS?                 
         BNE   FSTD100                                                          
*                                                                               
         CLI   ODRAFT,0                                                         
         BNE   *+8                                                              
         MVI   ODRAFT,C'Y'                                                      
         CLI   OREVERSE,0                                                       
         BNE   *+8                                                              
         MVI   OREVERSE,C'Y'                                                    
         CLI   OPEELED,0                                                        
         BNE   *+8                                                              
         MVI   OPEELED,C'Y'                                                     
*                                                                               
FSTD100  CLI   OREVERSE,0                                                       
         BNE   FSTDX                                                            
         CLI   PREVS,C'Y'                PROFILE TO SHOW REVERSALS              
         BNE   *+8                                                              
         MVI   OREVERSE,C'Y'                                                    
*                                                                               
FSTDX    J     OKXIT                                                            
FSTDERR  J     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        READ ALL RECORDS UP FRONT (FOR USE WITH DDS OPTION)          *         
* ON ENTRY AIOAREA1 CONTAINS CONTRA HEADER RECORD                     *         
***********************************************************************         
TSARNOW  NTR1                                                                   
         TM    JDTFLAG,JDTSTSQ                                                  
         BO    TSARN06                                                          
*                                                                               
TSARN04  MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    TSARN05                                                          
*        TM    IOERR,IOMAX               MAX IOS REACHED?                       
*        BO    *+6                                                              
         DC    H'0'                                                             
*        OI    DISPFLAG,DISIOMAX                                                
*        B     TSARNX                                                           
*                                                                               
TSARN05  OI    JDTFLAG,JDTSTSQ                                                  
TSARN06  LA    R3,IOKEY                                                         
         USING TRNRECD,R3                                                       
         MVC   KEYSAVE,TRNKEY            SAVE KEY FOR SEQ RESTORE               
         CLC   TRNKCULA,IOKEYSAV         REC BELONG TO CURRENT ACCOUNT?         
         BNE   TSARNX                                                           
         CLC   TRNKDATE,SPACES           TRANSACTION RECORD?                    
         BNH   TSARN04                                                          
*                                                                               
         BAS   RE,FILTKEY                CHECK FOR CONTRA FILTER                
         BNE   TSARN04                                                          
*                                                                               
         LA    R5,WORK                   R5=A(TABLE RECORD TO BE ADDED)         
         USING TEMPD,R5                                                         
         CLC   TRNKWORK,PREVBILL                                                
         BE    *+12                                                             
         MVI   TEMPDRCR,TEMPDRQ                                                 
         B     *+8                                                              
         MVI   TEMPDRCR,TEMPCRQ                                                 
         MVC   TEMPCONT,TRNKULC          CONTRA CODE                            
         MVC   TEMPINV,TRNKREF                                                  
         MVC   TEMPIDT,TRNKDATE                                                 
         MVC   TEMPWC,TRNKWORK                                                  
*                                                                               
         LA    R2,MAXTEMP                R2=(MAX # OF TABLE RECORDS)            
         LA    R3,TEMPLNQ                R3=L'(TABLE RECORD)                    
         SR    R4,R4                                                            
         ICM   R4,3,TEMPBLKN             R4=(# OF TABLE RECORDS SOFAR)          
         TM    JDTFLAG,JDTTBFQ           IS 'TEMPBLK' FULL?                     
         BZ    TSARN13                   . NO                                   
         GOTO1 =V(BINSRCH),DMCB,(0,(R5)),ATEMPBLK,(R4),(R3),(R3),(R2), +        
               RR=RB                                                            
         B     TSARN14                                                          
TSARN13  GOTO1 =V(BINSRCH),DMCB,(1,(R5)),ATEMPBLK,(R4),(R3),(R3),(R2), +        
               RR=RB                                                            
         SR    RF,RF                                                            
         ICM   RF,7,DMCB+1               RF=A(TABLE ENTRY/ZERO IF FULL)         
         BNZ   *+8                                                              
         OI    JDTFLAG,JDTTBFQ           TEMPBLK NOT BIG ENOUGH                 
TSARN14  CLI   DMCB,1                    REC NOT FOUND (POSSIBLY ADDED)         
         BNE   TSARN04                   . NO                                   
         TM    JDTFLAG,JDTTBFQ           IS TEMPBLK FULL                        
         BZ    TSARN16                                                          
         MVC   FVMSGNO,=AL2(AE$TMIIL)                                           
         OI    DISPFLAG,ALLREADQ                                                
         NI    DISPFLAG,X'FF'-NOTFRSTQ         FIRST TIME FOR DISPLAY?          
**************************                                                      
         XC    GRDPFA,GRDPFA                                                    
         OI    GRDPFAH+(FVOIND-FVIHDR),FVOXMT                                   
**************************                                                      
         B     TSARNERX                                                         
TSARN16  SR    RF,RF                                                            
         ICM   RF,15,DMCB+8                                                     
         STCM  RF,3,TEMPBLKN             NUMBER OF ENTRIES IN TABLE             
         B     TSARN04                                                          
         DROP  R3                                                               
*                                                                               
TSARNX   OI    JDTFLAG,JDTALLQ                                                  
         OC    TEMPBLKN,TEMPBLKN         HAVE WE ANY TSAR RECORDS?              
         BNZ   *+8                                                              
         OI    DISPFLAG,NORECQ           NO RECORDS FOUND FOR ACCOUNT           
         J     OKXIT                                                            
TSARNERX J     ERXIT                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        FILTER TRANSACTION KEYS (ACCDIR)                             *         
* ON ENTRY R3=A(TRANSACTION INDEX RECORD)                             *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
FILTKEY  NTR1                                                                   
*                                                                               
         USING TRNRECD,R3                                                       
         TM    TRNKSTAT,TRNSREVS         REVERSAL?                              
         BNO   *+8                                                              
         OI    OVRSTAT,OVRREVQ           REVERSED TRANSACTIONS                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,CONTLEN              RE=L'(CONTRA LEN)                      
         BZ    FILTK07                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),CONTRA         MATCHED ON CONTRA?                     
         BNE   FILTK06                                                          
         CLI   NEGCONT,NEGFILTR          NEGATIVE FILTER?                       
         BE    FILTKRJX                                                         
         B     FILTK07                                                          
FILTK06  CLI   NEGCONT,NEGFILTR          NEGATIVE FILTER?                       
         BNE   FILTKRJX                                                         
*                                                                               
FILTK07  L     R2,AOPTVALS               R2=A(OPTION VALUES)                    
         USING OPTVALSD,R2                                                      
         OC    OWCODE,OWCODE             FILTERING ON WORKCODE?                 
         BZ    FILTK20                                                          
         CLC   OWCODEVL,TRNKWORK                                                
         BE    FILTK10                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILTKRJX                                                         
         B     FILTK20                                                          
FILTK10  CLI   OWCODEFI,NEGFILTR                                                
         BE    FILTKRJX                                                         
*                                                                               
FILTK20  CLC   TRNKWORK,ORDER            ORDER RECORD?                          
         BE    FILTKRJX                  . YES, ELIMINATE ORDERS                
*                                                                               
         TM    TRNKSTAT,TRNSDRFT         DRAFT?                                 
         BZ    FILTK50                                                          
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK52                                                          
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK52                                                          
         B     FILTREJX                                                         
FILTK50  CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
FILTK52  TM    TRNKSTA2,TRNSPEEL         PEELED?                                
         BNO   FILTK54                                                          
         CLI   OPEELED,C'Y'                                                     
         BE    FILTK56                                                          
         CLI   OPEELED,C'O'                                                     
         BE    FILTK56                                                          
         B     FILTREJX                                                         
FILTK54  CLI   OPEELED,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
FILTK56  TM    TRNKSTA2,TRNSEXCL         CONTRA'D?                              
         BNO   FILTK58                                                          
         CLI   OCONT,C'Y'                                                       
         BE    FILTK60                                                          
         CLI   OCONT,C'O'                                                       
         BE    FILTK60                                                          
         B     FILTREJX                                                         
FILTK58  CLI   OCONT,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILTK60  OC    OREF,OREF                 REF FILTER ?                           
         BZ    FILTK70                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK70  CLC   TRNKWORK,ORDER            ORDER?                                 
**NOP    BNE   FILTK80                                                          
*        OC    OORDNO,OORDNO             ORDER FILTER?                          
*        BZ    FILTK80                                                          
*        GOTO1 ASCOMP,DMCB,TRNKREF,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),         
*              OORDFI                                                           
*        BNE   FILTKRJX                                                         
*                                                                               
FILTK80  OC    ODATE,ODATE               TRANSACTION DATE?                      
         BZ    FILTK90                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK90  OC    OMOS,OMOS                 TRANSACTION MOS                        
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   J     OKXIT                                                            
FILTKRJX J     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCMST RECORDS                                        *         
* ON ENTRY AIO1 CONTAINS ACCMST RECORD                                *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
FILTER   NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS               R2=A(OPTION VALUES)                    
         USING OPTVALSD,R2                                                      
         L     R3,AIO1                   R3=A(ACCMST RECORD)                    
         USING TRNRECD,R3                                                       
*                                                                               
         GOTO1 ASETELE,TRNRFST           SET ELEMENT ADDRESSES                  
*                                                                               
         USING TRNELD,R4                                                        
         ICM   R4,15,ATRNELD             R4=A(TRANSACTION ELEMENT)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRANCURR,COMPCURR         SET CURRENCY FROM AGENCY               
         ZAP   TRANAMNT,TRNAMNT          AGENCY TRANSACTION AMOUNT              
*                                                                               
         TM    TRNSTAT,TRNSAUTH          AUTHORISED?                            
         BNO   FILT130                                                          
         CLI   OAUTH,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT130  CLI   OAUTH,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT150  TM    TRNSTAT,TRNSDR            DEBIT/CREDIT?                          
         BNO   FILT160                                                          
         CLI   ODEBIT,C'N'                                                      
         BE    FILTREJX                                                         
         CLI   OCREDIT,C'O'                                                     
         BE    FILTREJX                                                         
         B     FILT165                                                          
FILT160  CLI   OCREDIT,C'N'                                                     
         BE    FILTREJX                                                         
         CLI   ODEBIT,C'O'                                                      
         BE    FILTREJX                                                         
*                                                                               
FILT165  TM    TRNSTAT,TRNSHOLD          HELD?                                  
         BNO   FILT170                                                          
         CLI   OHELD,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT170  CLI   OHELD,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
         TM    TRNSTAT,TRNSURG           URGENT TRANSACTION?                    
         BNO   FILT175                                                          
         CLI   OURGENT,C'N'                                                     
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT175  CLI   OURGENT,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
         OC    OBATCH,OBATCH             BATCH REF FILTER?                      
         BZ    FILT180                                                          
         GOTO1 ASCOMP,DMCB,TRNBTCH,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT180  SR    RF,RF                     INPUT TYPE FILTER?                     
         ICM   RF,1,OBTYPEVL                                                    
         BZ    FILT210                                                          
         SR    RE,RE                                                            
         IC    RE,TRNTYPE                                                       
         CR    RE,RF                                                            
         BE    FILT200                                                          
         CLI   OBTYPEFI,NEGFILTR                                                
         BNE   FILTREJX                                                         
         B     FILT210                                                          
FILT200  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTREJX                                                         
*                                                                               
FILT210  CLC   TRNANAL,PREVBILL                                                 
         BE    FILT212                                                          
         CLI   OBILLED,C'O'                                                     
         BE    *+12                                                             
         CLI   OBILLED,C'N'                                                     
         BNE   FILT220                                                          
         GOTO1 VPRORATA,DMCB,AIO1,0,ACOMFACS,0,PRATBLK,0                        
         LA    RF,PRATBLK                                                       
         USING PRORATAD,RF                                                      
         TM    PG$STAT,PG$FULLB+PG$PARTB                                        
         BZ    FILT215                                                          
*                                                                               
         ICM   RE,15,APRTELD                                                    
         BZ    FILT212                                                          
         TM    PRTSTAT-PRTELD(RE),PRTSBILQ IF BILLABLE TIME                     
         BNO   FILT212                                                          
         CP    TRNAMNT,=P'0'             AND ZERO DOLLAR                        
         BNE   FILT212                                                          
         OC    PG$LBLDT,PG$LBLDT                                                
         BZ    FILT215                                                          
*                                                                               
FILT212  CLI   OBILLED,C'N'                                                     
         BE    FILTREJX                                                         
         B     FILT220                                                          
FILT215  CLI   OBILLED,C'O'                                                     
         BE    FILTREJX                                                         
         B     FILT220                                                          
*                                                                               
FILT220  CLC   TRNKWORK,ORDER            ORDER TRAN?                            
         BE    FILT235                                                          
**NOP    OC    OORDNO,OORDNO                                                    
*        BZ    FILT235                                                          
*        OC    AFFNELD,AFFNELD           ORDER NUMBER REQUIRED                  
*        BZ    FILTREJX                                                         
*                                                                               
FILT235  OC    OSUBREF,OSUBREF           FILTERING ON SUB REF?                  
         BZ    *+14                                                             
         OC    AOTHELD,AOTHELD           OTHERS ELEMENT REQD                    
         BZ    FILTREJX                                                         
*                                                                               
         OC    OTIME,OTIME               FILTERING ON TIME?                     
         BZ    *+14                                                             
         OC    APRTELD,APRTELD           PERSONNEL RATE REQUIRED FLAG           
         BZ    FILTREJX                                                         
*                                                                               
         USING TRSELD,R4                 TRANSACTION STATUS ELEMENT             
         ICM   R4,15,ATRSELD                                                    
         BZ    FILT280                                                          
         OC    OACT,OACT                 FILTERING ON ACTIVITY DATE?            
         BZ    FILT250                                                          
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT250  OC    OUSED,OUSED               FILTERING ON USED DATE?                
         BZ    FILT260                                                          
         OC    TRSUDAT,TRSUDAT                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPDAT) USED DATE                   
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OUSEST,OUSEEN,OUSEFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT260  CLI   OTTYPE,0                  TIME TYPE FILTER?                      
         BE    FILT270                                                          
         CLI   OTTYPE,C'T'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTIME         TIME SHEET REGULAR?(US COST)           
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'M'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTMSS         TIME SHEET MISSING? (US COST)          
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'A'                                                      
         BNE   FILT270                                                          
         TM    TRSSTAT2,TRSSTADJ         TIME SHEET ADJUSTED? (US COST)         
         BNO   FILTREJX                                                         
*                                                                               
FILT270  TM    TRNRSTAT,TRNSREVS         REVERSAL?                              
         BNO   FILT275                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT280                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT280                                                          
         OC    OMOS,OMOS                 FILTERING ON TRANSACTION MOS?          
         BZ    FILTREJX                                                         
         OC    TRSRMOS,TRSRMOS           NO REV MOS ASSUME SAME AS TRAN         
         BZ    FILTREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTREJX                                                         
         B     FILT280                                                          
FILT275  CLI   OREVERSE,C'O'                                                    
         BE    FILTREJX                                                         
         B     FILT280                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                 OTHERS ELEMENT                         
FILT280  ICM   R4,15,AOTHELD                                                    
         BZ    FILT285                                                          
         OC    OSUBREF,OSUBREF           FILTERING ON SUB REFERENCE             
         BZ    FILT285                                                          
         GOTO1 ASCOMP,DMCB,OTHNUM,(OSUBRLN1,OSUBRVL1),(OSUBRLN2,OSUBRVLC        
               2),OSUBRFI                                                       
         BNE   FILTREJX                                                         
*                                                                               
         USING PRTELD,R4                 PERSONNEL RATE ELEMENT                 
FILT285  ICM   R4,15,APRTELD                                                    
         BZ    FILT290                                                          
         CLI   OTIME,0                   TIME FILTER?                           
         BE    FILT290                                                          
*                                                                               
         CLI   OTIME,C'B'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSBILQ          BILLABLE TIME (US COSTING)             
         BNO   FILTREJX                                                         
         CLI   OTIME,C'N'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSNOTQ          NON-BILLABLE TIME (US COSTING)         
         BNO   FILTREJX                                                         
         CLI   OTIME,C'R'                                                       
         BNE   FILT290                                                          
         TM    PRTSTAT,PRTSRTEQ          SPECIAL NON-BILL TIME(US COST)         
         BNO   FILTREJX                                                         
         B     FILT290                                                          
*                                                                               
         USING FFNELD,R4                                                        
FILT290  ICM   R4,15,AFFNELD                                                    
         BZ    FILT295                                                          
**NOP    OC    OORDNO,OORDNO             ORD NUMBER FILTER?                     
*        BZ    FILT295                                                          
*        MVC   ORD,SPACES                                                       
*        SR    RF,RF                                                            
*        IC    RF,FFNLN                                                         
*        SHI   RF,FFNLN1Q+1              ANYTHING ON IT?                        
*        BM    FILTREJX                                                         
*        EX    RF,*+4                                                           
*        MVC   ORD(0),FFNONUM                                                   
*        GOTO1 ASCOMP,DMCB,ORD,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),OORD         
*              FI                                                               
*        BNE   FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
         USING AFCELD,R4                 ACCOUNT FOREIGN CURRENCY ELEM          
FILT295  ICM   R4,15,AAFCELD                                                    
         BZ    FILT297                                                          
         TM    AFCXSTAT,AFCXSMEM         MEMO ITEM?                             
         BO    FILT295A                                                         
         CLI   OMEMOAFC,C'O'             ONLY REQUIRE MEMO ITEMS?               
         BE    FILTREJX                                                         
         B     FILT296                                                          
*                                                                               
FILT295A CLI   OMEMOAFC,C'Y'                                                    
         BE    FILT296                                                          
         CLI   OMEMOAFC,C'O'                                                    
         BNE   FILT300                                                          
*                                                                               
FILT296  MVC   TRANCURR,AFCCURR          OVERRIDE TRANSACTION CURRENCY          
         CLI   OVALUE,C'L'               SHOW LOCAL CURRENCY                    
         BNE   FILT300                                                          
         ZAP   TRANAMNT,AFCAMNT          LOCAL TRANSACTION AMOUNT               
         B     FILT300                                                          
*                                                                               
FILT297  CLI   OMEMOAFC,C'O'             MEMO ITEMS ONLY?                       
         BE    FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
FILT300  TM    SECFFLAG,SECFRATE         AUTHORISED FOR RATE?                   
         BO    FILT370                                                          
         OC    OAMOUNT,OAMOUNT           FILTERING TRANSACTION AMOUNT?          
         BZ    FILT350                                                          
         CP    OAMTVL,TRANAMNT           FILTER AMOUNT=TRANS AMOUNT?            
         BE    FILT340                                                          
         CLI   OAMTSIGN,0                HAS A'+'OR'-'BEEN SPECIFIED?           
         BNE   FILT310                                                          
         CLI   OAMTRNG,0                 HAS A'>'OR'<'BEEN SPECIFIED?           
         BNE   FILT310                                                          
         ZAP   DUB,OAMTVL                AMOUNT IS -VE EQUIVALENT?              
         AP    DUB,TRANAMNT                                                     
         BZ    FILT340                   . YES IT IS                            
         B     FILT330                                                          
FILT310  CLI   OAMTRNG,C'>'              MORE THAN SIGN?                        
         BNE   FILT320                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT340                                                          
         B     FILT330                                                          
FILT320  CLI   OAMTRNG,C'<'              LESS THAN SIGN?                        
         BNE   FILT330                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT340                                                          
FILT330  CLI   OAMTFI,NEGFILTR           NEGATIVE FILTER SPECIFIED?             
         BNE   FILTREJX                                                         
         B     FILT350                                                          
FILT340  CLI   OAMTFI,NEGFILTR           NEGATIVE FILTER SPECIFIED?             
         BE    FILTREJX                                                         
*                                                                               
FILT350  CP    TRANAMNT,=P'0'                                                   
         BNL   FILT360                                                          
         CLI   ONEGATIV,C'N'             ONLY WANT +VE NUMBERS/ZERO?            
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT360  CLI   ONEGATIV,C'O'             ONLY WANT -VE NUMBERS?                 
         BE    FILTREJX                                                         
*                                                                               
         OC    OCURRYVL,OCURRYVL         CURRENCY CODE FILTER?                  
         BZ    FILT370                                                          
         CLC   TRANCURR,OCURRYVL         MATCH ON CURRENCY CODE?                
         BNE   FILT365                                                          
         CLI   OCURRYFI,NEGFILTR         NEGATIVE FILTER?                       
         BE    FILTREJX                                                         
         B     FILT370                                                          
FILT365  CLI   OCURRYFI,NEGFILTR         NEGATIVE FILTER?                       
         BNE   FILTREJX                                                         
*                                                                               
FILT370  CLC   TRNKWORK,ORDER            ORDER TRANSACTION?                     
         BNE   FILTX                                                            
**nop    OC    OACT,OACT                 FILTERING ON ACTIVITY DATE?            
*        BNZ   FILTREJX                                                         
*        OC    OUSED,OUSED               FILTERING ON USED DATE?                
*        BNZ   FILTREJX                                                         
*        CLI   OREVERSE,C'O'             ONLY REVERSALS?                        
*        BE    FILTREJX                                                         
*                                                                               
FILTX    CR    RB,RB                                                            
         B     XIT                                                              
FILTREJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
BLDTSDAT NTR1                                                                   
         USING TRNRECD,R3                                                       
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         MVC   TRANCURR,COMPCURR                                                
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         XC    TSDACDAT,TSDACDAT   ACTIVITY DATE                                
         XC    TSDUSDAT,TSDUSDAT   USED DATE                                    
         XC    TSDPEDAT,TSDPEDAT   PEELED DATE                                  
         ZAP   TSDTAMNT,=P'0'                                                   
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         MVC   TSDDA,DADDRESS      DISK ADDRESS                                 
         MVC   TSDWORK,TRNKWORK    WORK CODE                                    
         MVC   TSDCONT,TRNKULC     CONTRA CODE                                  
         MVC   TSDTREF,TRNKREF     TRANSACTION REFERENCE NUMBER                 
         MVC   TSDTDAT,TRNKDATE    TRANSACTION DATE                             
         MVC   TSDTRST,TRNRSTAT    TRANSACTION RECORD STATUS                    
         MVC   TSDSBREF,TRNKSBR    TRANSACTION SUB-REF NUMBER                   
         MVC   TSDTMOA,TRNKSMOS    TRANSACTION MOA                              
         MVI   TSDTAMTY,0                                                       
         MVI   TSDALLOC,C' '                                                    
         TM    TRNRSTA2,TRNSEXCL   EXCLUDED?                                    
         BNO   *+8                                                              
         MVI   TSDALLOC,TSDALLFB   SET FULLY BILLED                             
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT OF TRANS RECORD)          
         USING TRNELD,R3                                                        
         LA    R4,TSDRFST          R4=A(FIRST ELEMENT FOR TSAR RECORD)          
*                                                                               
         CLC   TRNANAL,PREVBILL                                                 
         BE    BLDT05                                                           
         CLI   TSDALLOC,C' '                                                    
         BH    BLDT05                                                           
         GOTO1 VPRORATA,DMCB,AIO1,0,ACOMFACS,0,PRATBLK,0                        
         LA    RF,PRATBLK                                                       
         USING PRORATAD,RF                                                      
*                                                                               
         ICM   RE,15,APRTELD                                                    
         BZ    BLDT03                                                           
         TM    PRTSTAT-PRTELD(RE),PRTSBILQ IF BILLABLE TIME                     
         BNO   BLDT03                                                           
         CP    TRNAMNT,=P'0'               AND ZERO DOLLAR                      
         BNE   BLDT03                                                           
         OC    PG$LBLDT,PG$LBLDT                                                
         BZ    BLDT05                                                           
*                                                                               
BLDT03   TM    PG$STAT,PG$FULLB                                                 
         BNO   *+12                                                             
         MVI   TSDALLOC,TSDALLFB   SET FULLY BILLED                             
         B     BLDT05                                                           
         TM    PG$STAT,PG$PARTB                                                 
         BNO   BLDT05                                                           
         MVI   TSDALLOC,TSDALLPB   SET PART BILLED                              
*                                                                               
BLDT05   MVC   TSDBREF,TRNBTCH     BATCH REFERENCE                              
         MVC   TSDTEST,TRNSTAT     TRANSACTION ELEMENT STATUS                   
         MVC   TSDTTYPE,TRNTYPE    INPUT TYPE                                   
         ZAP   TRANAMNT,TRNAMNT    AGENCY TRANSACTION AMOUNT                    
*                                                                               
BLDT10   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
BLDT20   CLI   0(R3),EOR           END OF RECORD?                               
         BE    BLDT210                                                          
         CLI   0(R3),OTHELQ        OTHERS ELEMENT?                              
         BE    BLDT30                                                           
         CLI   0(R3),PRTELQ        PERSONNEL RATE  ELEMENT?                     
         BE    BLDT35                                                           
         CLI   0(R3),FFNELQ        FREE FORM NUMBER ELEMENT?                    
         BE    BLDT40                                                           
         CLI   0(R3),RATEVATQ      GENERAL RATE ELEMENT?                        
         BE    BLDT50                                                           
         CLI   0(R3),PXDELQ        POSTING XFER DETAIL ELEMENT?                 
         BE    BLDT60                                                           
         CLI   0(R3),VBIELQ        GST BILLED ELEMENT?                          
         BE    BLDT80                                                           
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMENT?                
         BE    BLDT85                                                           
         CLI   0(R3),TPRELQ        PRICE LIST TRANSACTION INFO ELEMENT?         
         BE    BLDT115                                                          
         CLI   0(R3),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    BLDT120                                                          
         CLI   0(R3),UNPELQ        UNIT PRICE ELEMENT?                          
         BE    BLDT125                                                          
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    BLDT130                                                          
         CLI   0(R3),PTAELQ        PRODUCTION ELEMENT?                          
         BE    BLDT160                                                          
         CLI   0(R3),PBIELQ        PST BILLED ELEMENT?                          
         BE    BLDT175                                                          
         B     BLDT10                                                           
*                                                                               
         USING OTHELD,R3           OTHERS ELEMENT                               
BLDT30   L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         SR    RF,RF                                                            
         IC    RF,OTHLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING PRTELD,R3           PERSONNEL RATE ELEMENT                       
BLDT35   SR    RF,RF                                                            
         IC    RF,PRTLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING FFNELD,R3           FREE FORM NUMBER ELEMENT                     
BLDT40   SR    RF,RF                                                            
         IC    RF,FFNLN                                                         
         CHI   RF,FFNLN1Q      ANYTHING ON IT?                                  
         BE    BLDT10                                                           
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING RATELD,R3           GENERAL RATE ELEMENT                         
BLDT50   SR    RF,RF                                                            
         IC    RF,RATLN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING PXDELD,R3           POSTING XFER DETAIL ELEMENT                  
BLDT60   SR    RF,RF                                                            
         IC    RF,PXDLN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING VBIELD,R3           VAT BILLED ELEMENT                           
BLDT80   SR    RF,RF                                                            
         IC    RF,VBILN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING SCIELD,R3           SUBSIDIARY CAHS ELEMENT                      
BLDT85   CLI   SCITYPE,SCITMEXP    MEMO EXPENSE?                                
         BNE   BLDT90                                                           
         CLI   TSDTTYPE,BAT70Q     EXPENSE BATCH?                               
         BE    *+8                                                              
         CLI   TSDTTYPE,BAT71Q     EXPENSE BATCH?                               
         BE    *+8                                                              
         CLI   TSDTTYPE,BAT21Q     EXPENSE BATCH?                               
         BE    *+8                                                              
         CLI   TSDTTYPE,BAT22Q     EXPENSE BATCH?                               
         BNE   BLDT90                                                           
         OI    TSDTAMTY,TSDTMEMQ   MEMO EXPENSE                                 
         ZAP   TRANAMNT,SCIAMNT    GET MEMO AMOUNT                              
BLDT90   L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   SCITYPE,SCITSJXP    AMOUNT POSTED TO EXPENSES?                   
         BNE   *+14                                                             
         AP    TSDTAMNT,SCIAMNT    EXPENSE AMOUNT                               
         B     BLDT10                                                           
         CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BNE   BLDT100                                                          
         L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   OVALUE-OPTVALSD(RF),C'B'                                         
         BE    BLDT110                                                          
BLDT100  CLI   SCITYPE,SCITFORD    FULLY MATCHED ORDER?                         
         BE    BLDT110                                                          
         CLI   SCITYPE,SCITPORD    PART MATCHED ORDER?                          
         BE    BLDT110                                                          
         L     RF,AIO1             RF=A(TRANSACTION RECORD)                     
         CLC   PREVBILL,TRNKWORK-TRNRECD(RF)                                    
         BNE   BLDT105                                                          
         CLI   SCITYPE,SCITCOMM    COMMISSION BILL?                             
         BE    BLDT110                                                          
         TM    SCITYPE,X'40'       VAT TYPE FOR BILL?                           
         BNO   BLDT110                                                          
BLDT105  CLI   AGYCTRY,CTRYUSA                                                  
         BE    BLDT106                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    BLDT106                                                          
         CLI   AGYCTRY,CTRYGER                                                  
         BNE   BLDT10                                                           
BLDT106  CLI   SCITYPE,SCITCDSC    CASH DISCOUNT?                               
         BE    BLDT110                                                          
         CLI   SCITYPE,SCITFTAX    FREELANCER?                                  
         BNE   BLDT10                                                           
*                                                                               
BLDT110  SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         B     BLDT200                                                          
*                                                                               
         USING TPRELD,R3           PRICE LIST INFO TRANSACTION ELEMENT          
BLDT115  SR    RF,RF                                                            
         IC    RF,TPRLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING TRSELD,R3           TRANSACTION STATUS ELEMENT                   
BLDT120  MVC   TSDACDAT,TRSDATE    ACTIVITY DATE                                
         MVC   TSDUSDAT,TRSUDAT    USED DATE                                    
         MVC   TSDPEDAT,TRSPDAT    PEELED DATE                                  
         MVC   TSDSTAT2,TRSSTAT2   STATUS BYTE 2                                
         B     BLDT10                                                           
*                                                                               
         USING UNPELD,R3           UNIT PRICE ELEMENT                           
BLDT125  SR    RF,RF                                                            
         IC    RF,UNPLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING OAMELD,R3           ORDER AMOUNT ELEMENT                         
BLDT130  AP    TRANAMNT,OAMAMNT    ADD ORDER AMOUNT                             
         SP    TRANAMNT,OAMIVAL    SUBTRACT INVOICE TODATE AMOUNT               
         OI    TSDTAMTY,TSDTORDQ   ORDER AMOUNT INCLUDED                        
         B     BLDT10                                                           
*                                                                               
         USING PTAELD,R3           PRODUCTION TRANSACTION ELEMENT               
BLDT160  CLI   PTATYPE,PTATRAL                                                  
         BNE   BLDT10                                                           
         CLC   TSDWORK,PREVBILL    C'99'                                        
         BE    BLDT10                                                           
*                                                                               
         USING PTDELD,R4           ADD MODIFIED ELEMENT                         
         MVC   PTDEL,PTAEL                                                      
         MVI   PTDLN,PTDLNQ                                                     
         CLC   PTARBLNO,SPACES                                                  
         BNH   BLDT10                                                           
         MVC   PTDBLNO,PTARBLNO    Bill Number                                  
         MVC   PTDBLDT,PTARBLDT    Bill Date                                    
         LA    R4,PTDLNQ(R4)                                                    
         B     BLDT10                                                           
         DROP  R3,R4                                                            
*                                                                               
         USING PBIELD,R3           PST BILLED ELEMENT                           
BLDT175  SR    RF,RF                                                            
         IC    RF,PBILN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*----------------------------------                                             
* MOVE WHOLE OF ELEMENT TO TSAR REC                                             
*----------------------------------                                             
BLDT200  BCTR  RF,0                MOVE WHOLE OF ELEMENT ONTO TSAR REC          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     BLDT10                                                           
         DROP  R3                                                               
*----------------------------------                                             
* CONTINUE WITH PROCESSING                                                      
*----------------------------------                                             
BLDT210  CLC   CURRLAST,SPACES     CURRENCY NOT SET?                            
         BE    BLDT212                                                          
         CLC   CURRLAST,TRANCURR   MIXED CURRENCIES DISPLAYED?                  
         BE    *+10                                                             
BLDT212  MVC   CURRLAST,TRANCURR                                                
         AP    TSDTAMNT,TRANAMNT   TRANSACTION AMOUNT LOCAL/AGENCY              
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNKEY(R3) R3=A(TRANSACTION ELEMNT)                   
         USING TRNELD,R3                                                        
         USING NARELD,R4                                                        
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SHI   RF,TRNLN1Q+1                                                     
         BM    BLDT230             NO NARRATIVE ON TRANSACTION                  
         MVI   NAREL,ELETSARQ      INTERNAL ELEMENT                             
         MVI   NARELTP,NARELQ      NARRATIVE ELEMENT                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NARREC(0),TRNNARR   GET NARRATIVE                                
         AHI   RF,NARLN1Q+1                                                     
         STC   RF,NARLN            ELEMENT LENGTH                               
         LA    R4,0(RF,R4)                                                      
*----------------------------------                                             
* Process Vendor Cashflow Info                                                  
*----------------------------------                                             
         USING CFWELD,R4                                                        
BLDT230  TM    TRNSTAT,TRNSDR      Production Debit?                            
         BZ    BLDT232             . NO, SKIP VENDOR INFO                       
         BRAS  RE,PROCVCF                                                       
         BNE   BLDT231                                                          
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    BLDTX                                                            
*        --------------------------Vendor Cashflow Filtering (w/Data)           
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS                                                      
         CLI   OPAID,C'O'          DISBURSED ITEMS ONLY?                        
         BNE   *+12                . NO                                         
         TM    CFWVIND,CFWIDBQ     . YES, ITEM DISBURSED?                       
         BZ    BLDTERX                    . NO, EXIT AND SKIP                   
         CLI   OPAID,C'N'          UNDISBURSED ITEMS?                           
         BNE   *+12                . NO                                         
         TM    CFWVIND,CFWIDBQ     . YES, ITEM UNDISBURSED?                     
         BO    BLDTERX                    . NO, EXIT AND SKIP                   
         CLI   OEXP,C'N'           EXTERNAL VENDORS?                            
         BE    BLDTERX             . NO                                         
*              --------------------                                             
         AP    DSBTOT,CFWVDIS                                                   
         MVI   CFWEL,ELETSARQ      INTERNAL ELEMENT                             
         MVI   CFWELTP,CFWVELQ     VENDOR CASHFLOW ELEMENT                      
                                                                                
         ZIC   RF,CFWVNOC          NUMBER OF CHECKS                             
         MHI   RF,CFWVCLNQ         TIMES LENGTH OF CHECK DATA                   
         AHI   RF,CFWVLNQ                                                       
         STC   RF,CFWLN                                                         
         LA    R4,0(RF,R4)                                                      
         B     BLDT234                                                          
*        --------------------------Vendor Cashflow Filtering (w/o data)         
         USING OPTVALSD,RF                                                      
BLDT231  L     RF,AOPTVALS         NO VENDOR CASHFLOW DATA FOUND                
         CLI   OPAID,C'O'          DISBURSED ITEMS ONLY?                        
         BE    BLDTERX             . YES, EXIT AND SKIP                         
         CLI   OEXP,C'O'           EXTERNAL VENDORS ONLY?                       
         BNE   *+12                . NO                                         
         TM    CFWVIND,CFWIVIQ     . YES, INTERNAL VENDOR?                      
         BO    BLDTERX                    . YES, EXIT AND SKIP                  
         CLI   OEXP,C'N'           EXCLUDE EXTERNAL VENDORS?                    
         BNE   *+12                . NO                                         
         TM    CFWVIND,CFWIVIQ     . YES, INTERNAL VENDOR?                      
         BZ    BLDTERX                    . YES, EXIT AND SKIP                  
         B     BLDT234                                                          
         DROP  RF                                                               
*----------------------------------                                             
* Process Receivable Cashflow                                                   
*----------------------------------                                             
BLDT232  BRAS  RE,PROCRCF                                                       
         BNE   BLDT234                                                          
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    BLDTX                                                            
         AP    CPMTOT,CFWAPPA                                                   
         MVI   CFWEL,ELETSARQ      INTERNAL ELEMENT                             
         MVI   CFWELTP,CFWRELQ     VENDOR CASHFLOW ELEMENT                      
                                                                                
         ZIC   RF,CFWRNOC          NUMBER OF CHECKS                             
         MHI   RF,CFWRCLNQ         LENGTH OF CHECK DATA                         
         AHI   RF,CFWRLNQ                                                       
         STC   RF,CFWLN            LENGTH OF ELEMENT                            
         LA    R4,0(RF,R4)                                                      
         DROP  R4                                                               
*----------------------------------                                             
* FINISH WITH DUMMY RECORD                                                      
*----------------------------------                                             
BLDT234  MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         CLI   TSDALLOC,C' '       DO WE HAVE AN ALLOCATION STATUS?             
         BH    BLDT240             YES                                          
         OC    TSDUSDAT,TSDUSDAT   USED DATE FILLED IT?                         
         BZ    *+8                 NO                                           
         MVI   TSDALLOC,TSDALLFB   YES, MUST BE FULLY BILLED                    
*                                                                               
BLDT240  LA    R4,1(R4)                                                         
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4               RF=L'(TSAR RECORD)                           
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
         BAS   RE,FGRMTSAR                                                      
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         TM    TSDTEST,TRNSDR      DEBIT?                                       
         BZ    BLDT244                                                          
         AP    DEBTOT,TSDTAMNT                                                  
         AP    TRNTOT,TSDTAMNT                                                  
         B     *+16                                                             
BLDT244  AP    CRETOT,TSDTAMNT                                                  
         SP    TRNTOT,TSDTAMNT                                                  
*                                                                               
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC         SAVE DUMMY TSAR RECORD                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
BLDTX    J     OKXIT                                                            
BLDTERX  J     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0                NUMBER OF DISPLAY LINES                
         MVI   DISATRIB,0                DISPLAY ATTRIBUTES                     
         L     R0,ADUMLINE               CLEAR DUMMY SCREEN LINES               
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE               R2=A(FIRST DUMMY SCREEN LINE)          
         L     R4,ATSARREC               R4=A(TSAR RECORD )                     
         USING TSARRECD,R4                                                      
*-----------------------------------                                            
* Initialize and output headings                                                
*-----------------------------------                                            
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB         DISPLAY DUMMY SCREEN LINES             
         B     FGRM10                                                           
*-----------------------------------                                            
* Check display line type                                                       
*-----------------------------------                                            
FGRM20   MVC   TEMPHTOT,SPACES                                                  
         ZAP   TEMPBAL,=P'0'                                                    
*                                                                               
         CLI   TSARFMT,TSTOTITM          TOTAL LINE ITEM?                       
         BE    FGRM30                                                           
         CLI   TSARFMT,TSSUBITM          SUBTOTAL LINE ITEM?                    
         BE    FGRM40                                                           
         CLI   TSARFMT,TSITEM1                                                  
         BNE   FGRMX                                                            
*-----------------------------------                                            
* Item line                                                                     
*-----------------------------------                                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         GOTO1 ASETELE,TSDRFST           SET ELEMENT ADDRESSES                  
*                                                                               
         TM    TSDTEST,TRNSDR                                                   
         BZ    *+14                                                             
         AP    TEMPBAL,TSDTAMNT                                                 
         B     *+10                                                             
         SP    TEMPBAL,TSDTAMNT                                                 
*                                                                               
         CLC   TSDWORK,=C'**'            ORDERS?                                
         BE    FGRMX                                                            
         CLC   TSDWORK,PREVBILL          BILLING (99'S)?                        
         BE    FGRM25                    . YES, SHOW SECONDARY COLUMNS          
         GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
         B     FGRMX                                                            
*                                                                               
FGRM25   BAS   RE,PREVBLNS               Process bill                           
         GOTO1 ADISGRD,DMCB,('DWN2ND',AGCTBL)                                   
         B     FGRMX                                                            
*-----------------------------------                                            
* Total line                                                                    
*-----------------------------------                                            
FGRM30   MVC   TEMPHTOT(L'MX@ACCT),MX@ACCT                                      
         GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         B     FGRMX                                                            
*-----------------------------------                                            
* Subtotal line                                                                 
*-----------------------------------                                            
FGRM40   MVC   TEMPHTOT(L'MX@TFOR),MX@TFOR                                      
         CLI   NEEDSUB,NESUBDRQ          NEED DEBIT SUBTOTAL                    
         BE    FGRM42                                                           
         CLI   NEEDSUB,NESUBCRQ          NEED CREDIT SUBTOTAL                   
         BE    FGRM44                                                           
         MVI   NEEDSUB,0                                                        
         B     FGRMX                                                            
FGRM42   MVC   TEMPHTOT+L'MX@TFOR(L'MX@CHGS),MX@CHGS                            
         B     *+10                                                             
FGRM44   MVC   TEMPHTOT+L'MX@TFOR(L'MX@BLG),MX@BLG                              
         GOTO1 ADISGRD,DMCB,('DWNSBT',AGCTBL)                                   
         B     FGRMX                                                            
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT STANDARD LINES FOR PREV BILLED IN DESCRIPTION FIELD   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
PREVBLNS NTR1                                                                   
         ICM   R4,15,ANARELD       ANY NARRATIVE TO DISPLAY?                    
         BZ    PREVBX                                                           
         SR    R0,R0                                                            
         IC    R0,NARLN-NARELD(R4)                                              
         LA    R4,NARLN1Q(R4)      R4=A(NARRATIVE AREA)                         
         USING NARINFOD,R4         DSECT FOR PREV BILLING INFO                  
         LR    R1,R0                                                            
         SHI   R1,NARLN1Q+1                                                     
         BM    PREVB51                                                          
         CHI   R1,L'NARBTYPE-1                                                  
         BNH   *+8                                                              
         LA    R1,L'NARBTYPE-1                                                  
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@TYPE1),MX@TYPE1                                        
         LA    RF,TEMP+L'MX@TYPE1-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),NARBTYPE                                                 
         LA    RF,2+1(RF,R1)                                                    
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF             DESCRIPTION INFO DISPLAY                  
*                                                                               
         CHI   R0,NARLN1Q+NARINLNQ    COMM/VAT IN SCIELDS OR NARR?              
         BNE   PREVB51                                                          
         CP    NARCAMT,=P'0'                                                    
         BE    PREVB05                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@CMN),MX@CMN                                            
         LA    RF,TEMP+L'MX@CMN-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,NARCAMT),(10,(RF)),2,MINUS=YES,ALIGN=LEFT                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
PREVB05  CP    NARDAMT,=P'0'                                                    
         BE    PREVB10                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@DISS),MX@DISS                                          
         LA    RF,TEMP+L'MX@DISS-1                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,NARDAMT),(10,(RF)),2,MINUS=YES,ALIGN=LEFT                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
PREVB10  ZAP   DUB,=P'0'                                                        
         LA    RF,NARPAMT1                                                      
         LA    R0,1                                                             
         AP    DUB,0(L'NARPAMT1,RF)                                             
         LA    RF,L'NARPAMT1(RF)                                                
         BCT   R0,*-10                                                          
         CP    DUB,=P'0'                                                        
         BE    PREVB25                                                          
         MVC   TEMP(L'MX@PAYBL),MX@PAYBL                                        
         LA    RF,TEMP+L'MX@PAYBL-1                                             
         BAS   RE,FMTAMT          KEYWORD=AMT                                   
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
PREVB25  CLC   TSCURRNO,TSLSTREC                                                
         BNH   PREVB26                                                          
         AP    PREVBCOM,NARCAMT                                                 
         AP    PREVBPAY,DUB                                                     
         AP    PREVBDIS,NARDAMT                                                 
*                                                                               
PREVB26  CLI   AGYCTRY,CTRYCAN                                                  
         BE    PREVB55                                                          
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    PREVB55                                                          
*                                                                               
         LA    RF,NARVAMT1         RF=A(VAT AMOUNT FIELDS)                      
         LA    RE,COMPVATR         RE=A(VAT RATE LIST)                          
         LA    R1,PREVBVAT         R1=A(VAT TOTALS FOR PREV BILL)               
         LA    R0,MAXVATQ          R0=(MAX NUM OF VAT RATES)                    
PREVB30  CP    0(L'NARVAMT1,RF),=P'0' HAVE WE GOT A VAT AMOUNT?                 
         BNE   PREVB40                                                          
         LA    RF,L'NARVAMT1(RF)                                                
         BCT   R0,PREVB30                                                       
         B     PREVB60                                                          
*                                                                               
PREVB40  MVC   VATRATE,0(RE)                                                    
         LA    R1,NARVAMT5                                                      
         CR    RF,R1               IS AMOUNT IN 5TH ACCUMULATOR?                
         BNE   PREVB50                                                          
         ICM   R1,15,ARATELD       SPECIAL VAT RATE OVERRIDE?                   
         BZ    PREVB50                                                          
         MVC   VATRATE,RATRATE-RATELD(R1) OVER WRITE VAT RATE                   
         MVC   0(L'VATRATE,RE),VATRATE    OVER WRITE SAVED VAT RATE             
         MVC   VATINDS,RATINDS-RATELD(R1) OVER WRITE VAT INDICATORS             
*                                                                               
PREVB50  MVC   TEMP,SPACES                                                      
         LR    R4,RF                                                            
         CURED (2,VATRATE),(5,TEMP),2,ALIGN=LEFT                                
         ORG   *-2                                                              
         TM    VATINDS,VATIDC3     IS RATE 3 DECIMALS?                          
         BNO   *+8                                                              
         MVI   11(R1),3                                                         
         BASR  RE,RF                                                            
*                                                                               
         LA    RF,TEMP+4                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'%'                                                       
         MVC   3(L'MX@VAT,RF),MX@VAT                                            
         LA    RF,3+L'MX@VAT-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,(R4)),(10,(RF)),2,ALIGN=LEFT,MINUS=YES                       
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
         LR    R1,R4                                                            
         BAS   RE,VATTOT           UPDATE VAT TOTALS FOR PREV BILL WC           
         B     PREVB60                                                          
*                                                                               
         USING SCIELD,R4                                                        
PREVB51  ICM   R4,15,ASCIELD       R4=A(SUBSIDIARY CASH ELEMENT)                
         BZ    PREVB60                                                          
PREVB52  CLI   SCIEL,EOR           END OF RECORD?                               
         BE    PREVB60                                                          
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BNE   PREVB60                                                          
         CLI   SCITYPE,SCITCOMM    COMMISSION?                                  
         BE    PREVB53                                                          
         TM    SCITYPE,X'40'       VAT TYPE?                                    
         BNO   PREVB53A                                                         
PREVB52A SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         AR    R4,RF                                                            
         B     PREVB52                                                          
*                                                                               
PREVB53  MVC   TEMP(L'MX@CMN),MX@CMN                                            
         LA    RF,TEMP+L'MX@CMN-1                                               
         ZAP   DUB,SCIAMNT                                                      
         BAS   RE,FMTAMT                                                        
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
         CLC   TSCURRNO,TSLSTREC                                                
         BNH   *+10                                                             
         AP    PREVBCOM,SCIAMNT                                                 
         B     PREVB52A                                                         
*                                                                               
PREVB53A CP    SCIGBIL,=P'0'       GROSS PAYABLE?                               
         BE    PREVB53B                                                         
         MVC   TEMP(L'MX@PAYBL),MX@PAYBL                                        
         LA    RF,TEMP+L'MX@PAYBL-1                                             
         ZAP   DUB,SCIGBIL                                                      
         BAS   RE,FMTAMT                                                        
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
         CLC   TSCURRNO,TSLSTREC                                                
         BNH   *+10                                                             
         AP    PREVBPAY,SCIGBIL                                                 
*                                                                               
PREVB53B CP    SCIVBIL,=P'0'       NO VAT?                                      
         BE    PREVB52A                                                         
         MVC   VATRULE,SCITYPE                                                  
         OI    VATRULE,X'40'       VAT TYPE RULE                                
         L     R1,AIO2                                                          
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK    LOOK UP VAT INFO                             
         MVC   VTCCPY,MYCO         COMPANY CODE                                 
         MVC   VTCOFFC,RECOFFC     OFFICE                                       
         MVC   VTCTYPE,VATRULE     VAT TYPE                                     
         MVC   VTCCOMF,ACOMFACS    A(COMFACS)                                   
         L     RF,ATSARREC         A(TSAR RECORD)                               
         LA    RF,TSARDATA-TSARRECD(RF)                                         
         USING TSARDATD,RF                                                      
         MVC   VTCINVD,TSDTDAT     TRANSACTION DATE                             
         DROP  RF                                                               
         GOTO1 =V(VATICAN),RR=RB                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VATRATE,VTCRATE     VAT RATE                                     
         MVC   VATINDS,VTCINDS     VAT INDICATORS                               
         DROP  R1                                                               
*                                                                               
         MVC   TEMP,SPACES                                                      
         CURED (2,VATRATE),(5,TEMP),2,ALIGN=LEFT                                
         ORG   *-2                                                              
         TM    VATINDS,VATIDC3     IS RATE 3 DECIMALS?                          
         BNO   *+8                                                              
         MVI   11(R1),3                                                         
         BASR  RE,RF                                                            
*                                                                               
         LA    RF,TEMP+4                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'%'                                                       
         MVC   3(L'MX@VAT,RF),MX@VAT                                            
         LA    RF,3+L'MX@VAT-1(RF)                                              
         ZAP   DUB,SCIVBIL                                                      
         BAS   RE,FMTAMT                                                        
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
         LA    R1,SCIVBIL                                                       
         BAS   RE,VATTOT                                                        
         B     PREVB52A                                                         
*                                                                               
         USING VBIELD,R4                                                        
PREVB55  ICM   R4,15,AVBIELD       R4=A(VAT BILLED ELEMENT)                     
         BZ    PREVB60                                                          
*                                                                               
PREVB56  CLI   VBIEL,EOR           END OF RECORD?                               
         BE    PREVB60                                                          
         CLI   VBIEL,VBIELQ        VAT BILLED ELEMENT?                          
         BE    PREVB58                                                          
*                                                                               
PREVB57  SR    RF,RF                                                            
         IC    RF,VBILN                                                         
         AR    R4,RF                                                            
         B     PREVB56                                                          
*                                                                               
PREVB58  MVC   VATNAME,MX@VAT      GST                                          
         MVC   VATRULE,VBITYPE     VAT CODE                                     
         MVC   VATRATE,VBIRATE     VAT RATE                                     
         MVC   VATINDS,VBIINDS     VAT INDICATORS                               
         ZAP   VATAMNT,VBIVAT      VAT AMOUNT                                   
         BRAS  RE,TAXFMT           FORMAT TAX LINE                              
*                                                                               
         LA    R1,VBIVAT                                                        
         BAS   RE,VATTOT                                                        
         B     PREVB57                                                          
*                                                                               
         USING PBIELD,R4                                                        
PREVB60  ICM   R4,15,APBIELD       R4=A(PST BILLED ELEMENT)                     
         BZ    PREVB100                                                         
*                                                                               
PREVB70  CLI   PBIEL,EOR           END OF RECORD?                               
         BE    PREVB100                                                         
         CLI   PBIEL,PBIELQ        PST BILLED ELEMENT?                          
         BE    PREVB90                                                          
*                                                                               
PREVB80  SR    RF,RF                                                            
         IC    RF,PBILN                                                         
         AR    R4,RF                                                            
         B     PREVB70                                                          
*                                                                               
PREVB90  MVC   VATNAME,SPACES                                                   
*                                                                               
         LA    R1,PRVTAB                                                        
PREVB91  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    PREVB95                                                          
         CLC   PBIPRV,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,L'PRVTAB(R1)                                                  
         B     PREVB91                                                          
*                                                                               
         MVC   WORK(2),LARF        PASS DESCRIPTION BACK                        
         MVC   WORK+2(2),2(R1)                                                  
         EX    0,WORK                                                           
         MVC   VATNAME,0(RF)                                                    
PREVB95  MVC   VATRULE,PBITYPE     PST CODE                                     
         MVC   VATRATE,PBIRATE     PST RATE                                     
         MVC   VATINDS,PBIINDS     PST INDICATORS                               
         ZAP   VATAMNT,PBIPST      PST AMOUNT                                   
         BRAS  RE,TAXFMT           FORMAT TAX LINE                              
*                                                                               
         LA    R1,PBIPST                                                        
         BAS   RE,VATTOT                                                        
         B     PREVB80                                                          
*                                                                               
PREVB100 DS    0H                                                               
*                                                                               
PREVBX   XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
*                                                                               
LARF     LA    RF,0                                                             
         EJECT                                                                  
***********************************************************************         
* INCREMENT VAT TOTALS FOR PREV BILLING WORKCODE                      *         
* ON ENTRY R1=A(VAT AMOUNT)                                           *         
*          'VATRATE' CONTAINS VAT RATE FOR AMOUNT                     *         
***********************************************************************         
VATTOT   NTR1                                                                   
*                                                                               
         CLC   TSCURRNO,TSLSTREC                                                
         BNH   VATTX                                                            
         LA    RF,COMBVATR         RF=A(COMBINED VAT RATE)                      
         LA    RE,PREVBVAT         R1=A(COMMISSION/VAT TOTALS)                  
         LA    R0,MAXNVATQ         MAX NUM OF VAT RATES (NEW VAT)               
VATT10   CLI   0(RF),EOTFF                                                      
         BE    VATT20                                                           
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    VATT15                                                           
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    VATT15                                                           
         CLC   0(L'COMBVATR,RF),VATRATE                                         
         BE    VATT30                                                           
         B     VATT19                                                           
VATT15   CLC   0(L'COMBVATR,RF),VATNAME                                         
         BE    VATT30                                                           
VATT19   LA    RF,L'COMBVATR(RF)                                                
         LA    RE,L'PREVBTOT(RE)                                                
         BCT   R0,VATT10                                                        
         B     XIT                        NO MORE VAT AMOUNTS ALLOWED           
*                                                                               
VATT20   CLI   AGYCTRY,CTRYUSA                                                  
         BE    VATT25                                                           
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    VATT25                                                           
         MVC   0(L'COMBVATR,RF),VATRATE                                         
         B     *+10                                                             
VATT25   MVC   0(L'COMBVATR,RF),VATNAME                                         
         MVI   L'COMBVATR(RF),EOTFF                                             
VATT30   AP    0(L'PREVBTOT,RE),0(6,R1)                                         
VATTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
SUBTOT   NTR1                                                                   
         L     R0,ATSARREC               CLEAR TSAR RECORD                      
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSTLNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO         SET TSAR REC NUMBER                    
         MVC   TSARLINE,LINSUSED         NUMBER OF LINES USED BY TOTAL          
         LA    R3,TSARDATA               R3=A(TSAR RECORD DATA)                 
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSSUBITM           SUBTOTAL ITEM                          
*                                                                               
         ZAP   TSTTDR,=P'0'              DEBIT TOTAL                            
         ZAP   TSTTCR,=P'0'              CREDIT TOTAL                           
         ZAP   TSTDITOT,=P'0'            DISBURSED TOTAL                        
         ZAP   TSTCPTOT,=P'0'            CLIENT PAYMENT TOTAL                   
*                                                                               
         CLI   NEEDSUB,NESUBDRQ          DEBIT SUBTOTAL NEEDED?                 
         BNE   SUBT10                                                           
         ZAP   TSTTDR,DEBTOT             DEBIT TOTAL                            
         ZAP   TSTTTOT,DEBTOT                                                   
         ZAP   TSTDITOT,DSBTOT           DISBURSED TOTAL                        
         ZAP   TEMPBAL,DEBTOT                                                   
         B     SUBT20                                                           
*                                                                               
SUBT10   CLI   NEEDSUB,NESUBCRQ          CREDIT SUBTOTAL NEEDED?                
         BNE   SUBTX                                                            
         ZAP   TSTTCR,CRETOT             CREDIT TOTAL                           
         ZAP   TSTTTOT,CRETOT                                                   
         ZAP   TSTCPTOT,CPMTOT           CLIENT PAYMENT TOTAL                   
         ZAP   TEMPBAL,=P'0'             TRANS TOTAL                            
         SP    TEMPBAL,CRETOT                                                   
*                                                                               
SUBT20   BAS   RE,FGRMTSAR                                                      
         MVC   TSTLINES,LINSUSED                                                
*                                                                               
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC               SAVE DUMMY TSAR RECORD                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
         GOTO1 ADISPLAY,DISATRIB         DISPLAY TOTAL LINE                     
         BNE   SUBTERX                                                          
         MVC   TSLSTLIN,TSCURRNO                                                
         ICM   RF,3,TSCURRNO             UPDATE TSAR RECORD COUNTER             
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         MVI   NEEDSUB,0                 Displayed SUBTOTAL                     
*                                                                               
SUBTX    B     OKXIT                                                            
SUBTERX  B     ERXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSTLNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         MVC   TSARLINE,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTTDR,DEBTOT       DEBIT TOTAL                                  
         ZAP   TSTTCR,CRETOT       CREDIT TOTAL                                 
         ZAP   TSTTTOT,TRNTOT      TRANS TOTAL                                  
         ZAP   TSTDITOT,DSBTOT     DISBURSED TOTAL                              
         ZAP   TSTCPTOT,CPMTOT     CLIENT PAYMENT TOTAL                         
         BAS   RE,FGRMTSAR                                                      
         MVC   TSTLINES,LINSUSED                                                
*                                                                               
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC         SAVE DUMMY TSAR RECORD                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   TOTX                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
         ICM   RF,3,TSCURRNO             UPDATE TSAR RECORD COUNTER             
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT KEYWORD=AMOUNT                                        *         
* ON ENTRY KEYWORD PLACED IN TEMP                                     *         
*          RF=A(TEMP+L'KEYWORD)                                       *         
*          DUB CONTAINS AMOUNT                                        *         
* ON EXIT  RF=A(TEMP+L'KEYWORD=AMOUNT)                                *         
***********************************************************************         
FMTAMT   NTR1                                                                   
*                                                                               
         LR    R2,RF                                                            
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C'='                                                       
         LA    R2,2(R2)                                                         
         CURED (P8,DUB),(17,WORK),2,MINUS=YES,ALIGN=LEFT                        
         LR    RE,R2                                                            
         AR    RE,R0                                                            
         LA    RF,TEMP                                                          
         SR    RE,RF                                                            
         LA    RF,20                                                            
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY OVERRIDE?                
         BNE   *+8                                                              
         LA    RF,17               SQUASH DESC FOR XTRA WIDE AMT COL            
         CR    RE,RF                                                            
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
         AR    R2,R0                                                            
         LR    RF,R2                                                            
         XIT1  REGS=(RF)                                                        
         EJECT                                                                  
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         J     XIT                                                              
ERXIT    LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
UNDERLIN DC    C'---------------'                                               
SI       DC    C'SI'                                                            
PREVBILL DC    C'99'                                                            
ORDER    DC    C'**'                                                            
*                                                                               
*                                                                               
BAT21Q   EQU   21                                                               
BAT22Q   EQU   22                                                               
BAT70Q   EQU   70                                                               
BAT71Q   EQU   71                                                               
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ACC,9                                                         
         DCDDL AC#DISS,7                                                        
         DCDDL AC#PAYBL,7                                                       
         DCDDL AC#FLTX,7                                                        
         DCDDL AC#ORDER,7                                                       
         DCDDL AC#SUBR,7                                                        
         DCDDL AC#TYPE,2                                                        
         DCDDL AC#STT,2                                                         
         DCDDL AC#UDAT,2                                                        
         DCDDL AC#DATE,2                                                        
         DCDDL AC#PELDT,2                                                       
         DCDDL AC#CR,2                                                          
         DCDDL AC#DR,2                                                          
         DCDDL AC#ACCT,15                                                       
         DCDDL AC#CMN,10                                                        
         DCDDL AC#VAT,5                                                         
         DCDDL AC#TYPE,5                                                        
         DCDDL AC#DSCAD,3                                                       
         DCDDL AC#BLGTY,L'MX@BLGTY       BILLING TYPE                           
         DCDDL AC#DRCR,L'MX@DRCR         DR/CR                                  
         DCDDL AC#ACTDT,L'MX@ACTDT       ACTIVITY DATE                          
         DCDDL AC#MOA,L'MX@MOA           MOA                                    
         DCDDL AC#TRANT,L'MX@TRANT       TRANS TYPE                             
         DCDDL AC#RSBRF,L'MX@RSBRF       BATCH REF                              
*                                                                               
         DCDDL AC#INVC2,L'MX@INVC2       Invoice Number                         
         DCDDL AC#RSIDT,L'MX@RSIDT       Invoice Date                           
         DCDDL AC#RSVCK,L'MX@RSVCK       Vendor Check Number                    
         DCDDL AC#RSVCD,L'MX@RSVCD       Vendor Check date                      
         DCDDL AC#RSVDI,L'MX@RSVDI       Disbursed Amount                       
         DCDDL AC#RSVUN,L'MX@RSVUN       Undisbursed Amount                     
         DCDDL AC#RSRCK,L'MX@RSRCK       Client Check Number                    
         DCDDL AC#RSRCD,L'MX@RSRCD       Client Check date                      
         DCDDL AC#RSINO,L'MX@RSINO       Bill Number                            
         DCDDL AC#RSBDT,L'MX@RSBDT       Bill Date                              
         DCDDL AC#RSRCU,L'MX@RSRCU       UNAPPLIED                              
         DCDDL AC#RSRCB,L'MX@RSRCB       APPLIED                                
         DCDDL AC#BAL,L'MX@BAL           BALANCE                                
         DCDDL AC#OFFST,L'MX@OFFST       OFFSET                                 
         DCDDL AC#GRSBD,L'MX@GRSBD       GROSS BILLED                           
         DCDDL AC#RSBCM,L'MX@RSBCM       BILLED COMMISSION                      
         DCDDL AC#URG,L'MX@URG           URGENT                                 
         DCDDL AC#HELD,L'MX@HELD         HELD                                   
         DCDDL AC#INT,L'MX@INT           INTERNAL                               
         DCDDL AC#APRVD,L'MX@APRVD       APPROVED                               
         DCDDL AC#RVRSL,L'MX@RVRSL       REVERSAL                               
         DCDDL AC#BNKVD,L'MX@BNKVD       VOIDED                                 
         DCDDL AC#QUERD,L'MX@QUERD       QUERIED                                
         DCDDL AC#WRTF,L'MX@WRTF         WRITE OFF                              
         DCDDL AC#XFR,L'MX@XFR           TRANSFER                               
         DCDDL AC#RSCCM,L'MX@RSCCM       Credit Commission                      
         DCDDL AC#RSCGR,L'MX@RSCGR       Credit Gross                           
         DCDDL AC#PART,L'MX@PART         Partial Payment                        
         DCDDL AC#RSVST,L'MX@RSVST       Vendor Status                          
         DCDDL AC#RSRCS,L'MX@RSRCS       Receivable Status                      
         DCDDL AC#PYMNT,L'MX@PYMNT       Payment                                
         DCDDL AC#CPM,L'MX@CPM           Client Payment                         
         DCDDL AC#CPAMT,L'MX@CPAMT       Client Payment Amount                  
         DCDDL AC#DRNET,L'MX@DRNET       Debits (Net)                           
         DCDDL AC#CRNET,L'MX@CRNET       Credits (Net)                          
         DCDDL AC#BANET,L'MX@BANET       Balance (Net)                          
         DCDDL AC#AMNET,L'MX@AMNET       Amount (Net)                           
         DCDDL AC#TFOR,L'MX@TFOR         Total for                              
         DCDDL AC#CHGS,L'MX@CHGS         Charges                                
         DCDDL AC#BLG,L'MX@BLG           Billing                                
         DCDDL AC#RSTAC,L'MX@XFRT        Transfer to Account                    
         DCDDL AC#RSTFC,L'MX@XFRF        Transfer from Account                  
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
DCCAP    DS    0X                                                               
         DCDDL AC#ONT,3                                                         
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
*PRVTAB                                                                         
       ++INCLUDE ACPRVTAB                                                       
         EJECT                                                                  
***********************************************************************         
*  PROCESS PRODUCTION VENDER INFORMATION                              *         
***********************************************************************         
PROCVCF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VDRKEY   USING TRNRECD,IOKEY                                                    
JOB      USING TRNELD,R3                                                        
         USING CFWELD,R4                                                        
         USING CFWVCD,R5                                                        
*                                                                               
         ICM   R3,15,ATRNELD                                                    
         XC    CFWVEL(CFWVLNQ),CFWVEL    INIT THE ELEMENT                       
         ZAP   CFWVDIS,=P'0'                                                    
         ZAP   CFWVUND,=P'0'                                                    
                                                                                
         LA    R5,CFWVLNQ(R4)                                                   
         XC    0(CFWVCLNQ,R5),0(R5)                                             
                                                                                
         ZAP   PRCFGRS,JOB.TRNAMNT       NET AMOUNT OF PROD TRANS               
                                                                                
         USING SCIELD,R6                                                        
         ICM   R6,15,ASCIELD                                                    
         BZ    PVCF20                                                           
         CLI   SCITYPE,SCITCOMM          Commission sciel?                      
         BNE   PVCF20                                                           
         SP    PRCFGRS,SCIAMNT           subtract commission                    
         DROP  R6                                                               
*----------------------------------------                                       
PVCF20   ICM   R6,15,APAKELD             X'D4'-Payable Key element              
         BNZ   PVCF30                                                           
         CLI   JOB.TRNTYPE,TRNTORD       Order?                                 
         BE    PVCFERX                   . yes, no info                         
         GOTOR VDATCON,DMCB,(1,JOB.TRNDATE),(2,CFWVCDI)                         
         OI    CFWVIND,CFWIVIQ                                                  
         B     PVCFERX                                                          
*                                                                               
         USING PAKELD,R6                 Prod Payable key element x'D4'         
PVCF30   XC    VDRKEY.TRNKEY,VDRKEY.TRNKEY                                      
         TM    COMPSTA4,CPYSOFF2         2 character offices                    
         BO    *+10                                                             
         MVC   PAKOFF,SPACES             Single office, not in key              
         MVC   VDRKEY.TRNKEY(TRNKEND-1),PAKACC    Payable key                   
         MVC   KEYSTORE,VDRKEY.TRNKEY                                           
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCDIR+IO2                                          
         BE    PVCF40                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     PVCFERX                                                          
         DROP  VDRKEY                                                           
*                                                                               
PVCF40   CLC   IOKEY(TRNKEND-1),KEYSTORE  Payable found?                        
         BNE   PVCF155                    . no                                  
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    PVCF50                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     PVCFERX                                                          
         DROP  R6                                                               
*                                                                               
         USING TRNRECD,R2                                                       
PVCF50   L     R2,AIO2                                                          
         LA    R2,TRNRFST                First element on payable               
PVCF52   CLI   0(R2),EOR                 End of record                          
         BE    PVCF155                                                          
         CLI   0(R2),TRNELQ              X'44'-Transaction Element              
         BE    PVCF60                                                           
         CLI   0(R2),TRSELQ              X'60'-Transaction Status Elem          
         BE    PVCF80                                                           
         CLI   0(R2),OTHELQ              X'23'-Others Element                   
         BE    PVCF100                                                          
         CLI   0(R2),MPYELQ              X'64'-Manual Payment Element           
         BE    PVCF120                                                          
PVCF55   SR    RF,RF                                                            
         IC    RF,1(,R2)                 next element                           
         AR    R2,RF                                                            
         B     PVCF52                                                           
*----------------------------------------                                       
* Transaction Element  X'44'                                                    
*----------------------------------------                                       
VNDR     USING TRNELD,R2                                                        
PVCF60   TM    VNDR.TRNSTAT,TRNSDR       Only need the credits                  
         BO    PVCF150                                                          
*                                                                               
         CLI   JOB.TRNTYPE,TRNTJBTX      Job Transfer X'34'?                    
         BNE   PVCF68                    . yes, do not match on batch           
         USING PXDELD,RE                                                        
         ICM   RE,15,APXDELD             First x'4e' elem                       
         BZ    PVCF70                    None                                   
PVCF64   CLI   PXDEL,0                   end of record                          
         BE    PVCF70                                                           
         CLI   PXDEL,PXDELQ              X'4E' - XFER DETAIL ELEM               
         BNE   *+12                                                             
         CLI   PXDTYPE,PXDTORG           C'O' - ORIGINAL JOB                    
         BE    PVCF66                                                           
         SR    R0,R0                                                            
         IC    R0,1(,RE)                 next element                           
         AR    RE,R0                                                            
         B     PVCF64                                                           
PVCF66   CLI   PXDLN,PXDLN2Q                                                    
         BL    PVCF70                                                           
         CLC   VNDR.TRNBTCH,PXDOBAT      Match on batch reference               
         BE    PVCF70                    Found matching batch ref               
         B     PVCF150                                                          
*                                                                               
PVCF68   CLC   VNDR.TRNBTCH,JOB.TRNBTCH  Match on batch reference               
         BNE   PVCF150                                                          
                                                                                
PVCF70   ZAP   CFWVUND,PRCFGRS                                                  
         ZAP   CFWVDIS,=P'0'                                                    
         MVC   CFWVSTA,VNDR.TRNSTAT      Status of transaction                  
         B     PVCF55                                                           
         DROP  RE,VNDR                                                          
*----------------------------------------                                       
* Transaction Status Element X'60'                                              
*----------------------------------------                                       
VNDR60   USING TRSELD,R2                                                        
*                                                                               
PVCF80   CLI   JOB.TRNTYPE,TRNTJBTX      Job Transfer X'34'?                    
         BNE   PVCF90                    . Yes, Do not check bseq               
         USING PXDELD,RE                                                        
         ICM   RE,15,APXDELD             First x'4e' elem                       
         BZ    PVCF55                    None                                   
PVCF84   CLI   PXDEL,EOR                 End Of Record                          
         BE    PVCF55                                                           
         CLI   PXDEL,PXDELQ              X'4E' - XFER DETAIL ELEM               
         BNE   *+12                                                             
         CLI   PXDTYPE,PXDTORG           C'O' - ORIGINAL JOB                    
         BE    PVCF86                                                           
         SR    R0,R0                                                            
         IC    R0,1(,RE)                 next element                           
         AR    RE,R0                                                            
         B     PVCF84                                                           
PVCF86   CLI   PXDLN,PXDLN2Q                                                    
         BL    PVCF55                                                           
         CLC   VNDR60.TRSBSEQ,PXDOSEQ    Match on original batch seq.           
         BE    PVCF55                                                           
         B     PVCF150                                                          
*                                                                               
JOB60    USING TRSELD,RF                                                        
PVCF90   ICM   RF,15,ATRSELD                                                    
         CLC   VNDR60.TRSBSEQ,JOB60.TRSBSEQ   Match on batch line #             
         BNE   PVCF150                                                          
         MVC   CFWVST2,VNDR60.TRSSTAT                                           
*                                                                               
         TM    CFWVSTA,TRNSREV           Reversal?                              
         BZ    PVCF94                    . No                                   
**AW     GOTOR VDATCON,DMCB,(2,VNDR60.TRSREVD),(2,CFWVCDT)                      
         B     PVCF55                                                           
PVCF94   TM    VNDR60.TRSSTAT,TRSSOFFS   Offset Item?                           
         BZ    PVCF55                    . No                                   
**AW     MVC   CFWVCDT,VNDR60.TRSUDAT    . Yes, Offset Date                     
         B     PVCF55                                                           
         DROP  RE                                                               
         DROP  JOB60,VNDR60                                                     
*----------------------------------------                                       
* Payable Others Element  X'23'                                                 
*----------------------------------------                                       
         USING OTHELD,R2                                                        
PVCF100  CLI   JOB.TRNTYPE,TRNTJBTX      Job Transfer X'34'?                    
         BNE   PVCF110                   . Yes, Do not check job                
         USING PXDELD,RE                                                        
         ICM   RE,15,APXDELD             first x'4e' elem                       
         BZ    PVCF110                   None                                   
PVCF104  CLI   PXDEL,EOR                 End Of Record                          
         BE    PVCF110                                                          
         CLI   PXDEL,PXDELQ              X'4E' - XFER DETAIL ELEM               
         BNE   *+12                                                             
         CLI   PXDTYPE,PXDTORG           C'O' - ORIGINAL JOB                    
         BE    PVCF108                                                          
         SR    RF,RF                                                            
         IC    RF,1(,RE)                 next element                           
         AR    RE,RF                                                            
         B     PVCF104                                                          
PVCF108  CLC   OTHNUM(3),PXDFRTOA+3      Product on payable = original          
         BNE   PVCF150                                                          
         CLC   OTHNUM+6(6),PXDFRTOA+6    Job on payable = original              
         BE    PVCF55                                                           
         B     PVCF150                                                          
*                                                                               
PVCF110  MVC   WORK(L'TRNKACT),BASKEY                                           
         OC    WORK(L'TRNKACT),SPACES                                           
         CLC   OTHNUM(3),WORK+3                                                 
         BNE   PVCF150                                                          
         CLC   OTHNUM+6(6),WORK+6                                               
         BE    PVCF55                                                           
         B     PVCF150                                                          
         DROP  RE                                                               
*----------------------------------------                                       
* Manual Payment Element  X'64'                                                 
*----------------------------------------                                       
         USING MPYELD,R2                                                        
PVCF120  OC    MPYDTE,MPYDTE             Is check still here?                   
         BZ    PVCF55                    must have been voided                  
         MVC   CFWVCNO,MPYNO             Vendor check number                    
         MVC   CFWVCDT,MPYDTE            Vendor check date                      
         ZAP   CFWVDIS,CFWVUND           Disbursed amount                       
         ZAP   CFWVUND,=P'0'                                                    
         OI    CFWVIND,CFWIDBQ           Yes, Disbursed                         
         GOTOR VDATCON,DMCB,(2,CFWVCDT),(0,WORK)                                
         B     PVCF55                                                           
*----------------------------------------                                       
* Clear out cashflow element, try again                                         
*----------------------------------------                                       
PVCF150  XC    CFWVEL(CFWVLNQ),CFWVEL    INIT THE ELEMENT                       
         ZAP   CFWVDIS,=P'0'                                                    
         ZAP   CFWVUND,=P'0'                                                    
         XC    CFWVCD(CFWVCLNQ),CFWVCD                                          
         GOTO1 AIO,IOSEQ+IOACCDIR+IO2    Try next with this key                 
         BE    PVCF40                                                           
*                                                                               
PVCF155  OC    CFWVCD(CFWVCLNQ),CFWVCD                                          
         BZ    PVCF156                                                          
         ZIC   R1,CFWVNOC                Update number of checks                
         AHI   R1,1                                                             
         STC   R1,CFWVNOC                                                       
         LA    R5,CFWVCLNQ(R5)           Next available for check info          
         XC    0(CFWVCLNQ,R5),0(R5)                                             
*                                                                               
PVCF156  L     R1,AIO1                                                          
         MVC   IOKEY,0(R1)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    PVCFX                                                            
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     PVCFERX                                                          
*                                                                               
PVCFERX  J     ERXIT                                                            
PVCFX    J     OKXIT                                                            
         LTORG                                                                  
         DROP  R2,R4,R5,R7,JOB                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  PROCESS PRODUCTION RECEIVABLE INFORMATION                          *         
***********************************************************************         
PROCRCF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RCVKEY   USING TRNRECD,IOKEY                                                    
JOB44    USING TRNELD,R3                                                        
         USING CFWELD,R4                                                        
         USING CFWRCD,R5                                                        
*                                                                               
         L     R2,AIO1                                                          
JOBKEY   USING TRNRECD,R2                                                       
         ICM   R3,15,ATRNELD                                                    
         XC    CFWREL(CFWRLNQ),CFWREL    INIT THE ELEMENT                       
         LA    R5,CFWRLNQ(R4)                                                   
         XC    0(CFWRCLNQ,R5),0(R5)                                             
         XC    RCVKEY.TRNKEY,RCVKEY.TRNKEY                                      
*----------------------------------------                                       
* Process credits                                                               
*----------------------------------------                                       
         ZAP   PRCFGRS,JOB44.TRNAMNT     Net amount of prod transaction         
         AP    PRCFGRS,JOB44.TRNBLCOM    Add commission for gross               
*                                                                               
         MVC   RCVKEY.TRNKCULA,JOBKEY.TRNKCULC   Receivable account             
         MVC   RCVKEY.TRNKREF,JOBKEY.TRNKREF                                    
         MVC   RCVKEY.TRNKDATE,JOBKEY.TRNKDATE                                  
         DROP  JOBKEY                                                           
*                                                                               
         LR    R2,R3                     A(TRNELQ)                              
PRCF50   CLI   0(R2),0                   end of record                          
         BE    PRCF80                                                           
         CLI   0(R2),CPJELQ              X'4F'-RCV account for retail           
         BE    PRCF60                                                           
         CLI   0(R2),BSCELQ              X'E3'-Billing Source element           
         BE    PRCF70                                                           
PRCF55   SR    RF,RF                                                            
         IC    RF,1(,R2)                 next element                           
         AR    R2,RF                                                            
         B     PRCF50                                                           
*----------------------------------------                                       
* Receivable Account for Retail Billing                                         
*----------------------------------------                                       
         USING CPJELD,R2                 X'4F'                                  
PRCF60   CLI   CPJTYPE,CPJTREC           elment type = receivable?              
         BNE   PRCF55                                                           
         MVC   RCVKEY.TRNKULA,CPJRULA                                           
         B     PRCF55                                                           
         DROP  R2                                                               
*----------------------------------------                                       
* Prod Billing Source Element  X'E3'                                            
*----------------------------------------                                       
         USING BSCELD,R2                                                        
PRCF70   MVC   RCVKEY.TRNKOFF(L'TRNKOFF+L'TRNKCULC),SPACES                      
         MVC   RCVKEY.TRNKCACT,BSCBSRC   billing Source                         
         TM    COMPSTA4,CPYSOFF2         2 character offices                    
         BZ    *+10                                                             
         MVC   RCVKEY.TRNKOFF,BSCBOFF    Office                                 
         B     PRCF55                                                           
         DROP  R2                                                               
*----------------------------------------                                       
* Read for receivable                                                           
*----------------------------------------                                       
PRCF80   XC    PRCPRTAB(MAXRCVTQ*L'PRCPRTAB),PRCPRTAB Transfer Table            
         MVC   KEYSTORE,RCVKEY.TRNKEY                                           
PRCF82   MVC   IOKEY,KEYSTORE                                                   
         GOTO1 AIO,IOHIGH+IOACCDIR+IO2                                          
         BE    PRCF90                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     PRCFOKX                                                          
PRCF90   ZAP   CFWUNAA,=P'0'                                                    
         ZAP   CFWAPPA,=P'0'                                                    
*                                                                               
PRCF96   CLC   IOKEY(TRNKEND-1),KEYSTORE       Receivable found?                
         BNE   PRCF240                                                          
*                                                                               
         LA    RF,PRCPRTAB               Receivable transfer table              
PRCF100  CLI   0(RF),0                   Any more transfer entries?             
         BE    PRCF114                   . no,process this receivable           
         CLI   0(RF),X'FF'               End of transfer table?                 
         BE    PRCF114                   . yes,process this receivable          
         CLC   RCVKEY.TRNKULA,0(RF)   Did we Transfer From?                     
         BE    PRCF110                   . yes                                  
         AHI   RF,L'PRCPRTAB             . no, check next entry                 
         B     PRCF100                                                          
*                                                                               
PRCF110  CLC   RCVKEY.TRNKSBR,L'TRNKULA(RF) Transfer From exact record?         
         BNH   PRCF214                      . yes, get next record              
*                                                                               
PRCF114  GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    PRCF120                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     PRCFOKX                                                          
*                                                                               
         USING TRNRECD,R2                                                       
PRCF120  L     R2,AIO2                   first element                          
         LA    R2,TRNRFST                first element                          
PRCF122  CLI   0(R2),EOR                 end of record                          
         BE    PRCF210                                                          
         CLI   0(R2),TRNELQ              X'44'-Transaction element              
         BE    PRCF130                                                          
         CLI   0(R2),TRSELQ              X'60'-Trans Status Element             
         BE    PRCF150                                                          
         CLI   0(R2),DUEELQ              X'61'-Rcvable Due Date                 
         BE    PRCF160                                                          
         CLI   0(R2),TRXELQ              X'75'-Transaction Extra Status         
         BE    PRCF170                                                          
         CLI   0(R2),RALELQ              X'D9'-Rcvable Allocation elem          
         BE    PRCF180                                                          
PRCF125  SR    RF,RF                                                            
         IC    RF,1(,R2)                 next element                           
         AR    R2,RF                                                            
         B     PRCF122                                                          
*----------------------------------------                                       
* Receivable Transaction Element                                                
*----------------------------------------                                       
         USING TRNELD,R2                                                        
PRCF130  TM    TRNSTAT,TRNSREV                                                  
         BZ    PRCF132                                                          
         OI    CFWRSTA,TRNSREV           Only want reversal status              
         CLI   TRNTYPE,TRNTCALC          TYPE 30 TRNS                           
         BE    PRCF214                   Skip this one                          
PRCF132  TM    TRNSTAT,TRNSDR                                                   
         BZ    PRCF140                                                          
         AP    CFWUNAA,TRNAMNT                                                  
         B     PRCF125                                                          
PRCF140  AP    CFWAPPA,TRNAMNT           Applied amount                         
         ZAP   PRPRAMT,TRNAMNT                                                  
         B     PRCF125                                                          
*----------------------------------------                                       
* Transaction Status Element                                                    
*----------------------------------------                                       
         USING TRSELD,R2                                                        
PRCF150  TM    CFWRSTA,TRNSREV                                                  
         BZ    PRCF125                                                          
**AW     GOTO1 VDATCON,DMCB,(2,TRSREVD),(1,CFWCCDT)                             
         B     PRCF125                                                          
*----------------------------------------                                       
* Receivable Due Date Element                                                   
*----------------------------------------                                       
         USING DUEELD,R2                 Receivable Due Date Element            
PRCF160  MVC   CFWCDDT,DUEDATE                                                  
         B     PRCF125                                                          
*----------------------------------------                                       
* Transaction Extra Status Element                                              
*----------------------------------------                                       
         USING TRXELD,R2                                                        
PRCF170  TM    TRXSTA1,TRXSRQRD          Queried?                               
         BZ    *+8                       . no                                   
         OI    CFWRSTA,TRXSRQRD                                                 
         TM    TRXSTA1,TRXSRHLD          Held?                                  
         BZ    *+8                       . no                                   
         OI    CFWRSTA,TRXSRHLD                                                 
         B     PRCF125                                                          
*----------------------------------------                                       
* Receivable Allocation Element                                                 
*----------------------------------------                                       
         USING RALELD,R2                 Receivable Allocation Element          
PRCF180  CLI   RALTYPE,RALTALC           Type 1 = Regular Allocation            
         BNE   PRCF184                                                          
         MVC   CFWCCNO,RALAREF           Client Check Number                    
         MVC   CFWCCDT,RALADAT           Client Check Date                      
         MVC   CFWCPDT,RALADEP           Client Deposit Date                    
         OI    CFWRIND,CFWICPQ                                                  
         B     PRCF125                                                          
*                                                                               
PRCF184  CLI   RALTYPE,RALTOFS           Type 2 = Offset/Contra'd               
         BNE   PRCF188                                                          
**AW     MVC   CFWCCDT,RALODAT           Offset Date                            
         OI    CFWRSTA,CFWRSOF           Status Offset                          
         SP    CFWAPPA,PRPRAMT           Applied amount                         
         B     PRCF125                                                          
*                                                                               
PRCF188  CLI   RALTYPE,RALTWOF           Type 3 = Write-Off                     
         BNE   PRCF192                                                          
**AW     MVC   CFWCCNO,RALWREF           Write-Off Reference                    
**AW     MVC   CFWCCDT,RALWDAT           Write-Off Date                         
         OI    CFWRSTA,CFWRSWO           Status W/O                             
         SP    CFWAPPA,PRPRAMT           Applied amount                         
         B     PRCF125                                                          
*                                                                               
PRCF192  CLI   RALTYPE,RALTTTO           Type 4 = Transfer To                   
         BNE   PRCF204                                                          
         L     RE,AIO2                   A(Current receivable tran)             
         LA    RF,PRCPRTAB               Receivable transfer table              
*                                                                               
         ZAP   CFWAPPA,=P'0'                                                    
         ZAP   CFWUNAA,=P'0'                                                    
         XC    CFWRNOC,CFWRNOC                                                  
         LA    R5,CFWRLNQ(R4)                                                   
         XC    0(CFWRCLNQ,R5),0(R5)                                             
*                                                                               
PRCF196  CLI   0(RF),0                   Next available space                   
         BE    PRCF200                                                          
         CLI   0(RF),X'FF'               End of transfer table?                 
         BE    PRCFERX                                                          
         CLC   0(L'TRNKULA,RF),1(RE)     Did we Transfer From before?           
         BE    PRCF200                   . yes                                  
         AHI   RF,L'PRCPRTAB             . no, check next entry                 
         B     PRCF196                                                          
*                                                                               
PRCF200  MVC   0(L'TRNKULA,RF),1(RE)     put account into rcvble tab            
         MVC   L'TRNKULA(L'TRNKSBR,RF),L'TRNKEY-1(RE) and subref                
         MVC   KEYSTORE+(TRNKULA-TRNKEY)(L'TRNKULA),RALTULA                     
         B     PRCF82                                                           
*                                                                               
PRCF204  CLI   RALTYPE,RALTTFR           Type 5 = Transfer From                 
         BNE   PRCF212                                                          
         OI    CFWRSTA,CFWRSTR           Status Transfer                        
         B     PRCF125                                                          
*                                                                               
PRCF210  OC    0(CFWRCLNQ,R5),0(R5)                                             
         BZ    PRCF212                                                          
         ZIC   R1,CFWRNOC                Update number of checks                
         AHI   R1,1                                                             
         STC   R1,CFWRNOC                                                       
         LA    R5,CFWRCLNQ(R5)           Next available for check info          
PRCF212  XC    0(CFWRCLNQ,R5),0(R5)                                             
*                                                                               
PRCF214  GOTO1 AIO,IOSEQ+IOACCDIR+IO2    Read for receivable                    
         BE    PRCF96                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         max IO'S                               
         B     PRCFOKX                                                          
*                                                                               
PRCF240  CLI   CFWRNOC,0                 Number of checks                       
         BE    PRCF300                                                          
         CP    CFWUNAA,=P'0'             Any Unapplied amount?                  
         BE    PRCF300                   . no                                   
         CP    CFWAPPA,=P'0'             Any applied amount?                    
         BE    PRCF300                   . no                                   
         CP    CFWUNAA,CFWAPPA           Fully applied?                         
         BE    *+12                      . Yes                                  
         NI    CFWRIND,X'FF'-CFWICPQ     . No, turn off client payment          
         OI    CFWRIND,CFWIPCPQ                turn on partial payment          
         CP    CFWUNAA,PRCFGRS           Unapplied equal to gross?              
         BE    PRCF245                   . Yes, no adjustment needed            
*                                                                               
         ZAP   TEMPBAL,PRCFGRS                                                  
         SRP   TEMPBAL,6,0               Multiply for percent                   
         DP    TEMPBAL,CFWUNAA           percent of gross to unapp              
         ZAP   DUB,TEMPBAL(L'TEMPBAL-L'CFWUNAA) Save percent                    
*                                                                               
         ZAP   TEMPBAL,CFWUNAA           Move unapplied into big field          
         MP    TEMPBAL,DUB               multiply by percent                    
         SRP   TEMPBAL,64-6,5            divide to get actual number            
         ZAP   CFWUNAA,TEMPBAL           put adjusted number in                 
*                                                                               
         ZAP   TEMPBAL,CFWAPPA           Applied total in big field             
         MP    TEMPBAL,DUB               multiply by percent                    
         SRP   TEMPBAL,64-6,5            divide to get actual number            
         ZAP   CFWAPPA,TEMPBAL           put adjusted number in                 
*----------------------------------------                                       
PRCF245  ZAP   TEMPBAL,JOB44.TRNAMNT     Net                                    
         SRP   TEMPBAL,6,0               Multiply for percent                   
         ZAP   DUB,=P'0'                                                        
         CP    PRCFGRS,=P'0'                                                    
         BE    PRCF247                                                          
         DP    TEMPBAL,PRCFGRS           percent of net to gross                
         ZAP   DUB,TEMPBAL(L'TEMPBAL-L'PRCFGRS) Save percent                    
*                                                                               
PRCF247  ZAP   TEMPBAL,CFWUNAA           Move unapplied into big field          
         MP    TEMPBAL,DUB               multiply by percent                    
         SRP   TEMPBAL,64-6,5            divide to get actual number            
         ZAP   CFWUNAA,TEMPBAL           put adjusted number in                 
*                                                                               
         ZAP   TEMPBAL,CFWAPPA           Applied total in big field             
         MP    TEMPBAL,DUB               multiply by percent                    
         SRP   TEMPBAL,64-6,5            divide to get actual number            
         ZAP   CFWAPPA,TEMPBAL           put adjusted number in                 
*----------------------------------------                                       
PRCF300  L     R1,AIO1                                                          
         MVC   IOKEY,0(R1)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    PRCF310                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         J     PRCFOKX                                                          
*                                                                               
PRCF310  CLI   CFWRNOC,0                                                        
         BE    *+8                                                              
PRCFOKX  J     OKXIT                                                            
PRCFERX  J     ERXIT                                                            
         DROP  R4,R5,JOB44,RCVKEY                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT TAX LINE                                                     *         
***********************************************************************         
TAXFMT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'VATNAME),VATNAME                                          
         LA    RF,TEMP+L'MX@VAT-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(L'VBITYPE,RF),VATRULE                                          
         LA    RF,4(RF)                                                         
         ST    RF,FULL                                                          
         CURED (2,VATRATE),(5,(RF)),2,ALIGN=LEFT                                
         ORG   *-2                                                              
         TM    VATINDS,VATIDC3     IS RATE 3 DECIMALS?                          
         BNO   *+8                                                              
         MVI   11(R1),3                                                         
         BASR  RE,RF                                                            
*                                                                               
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(2,RF),=C'%/'                                                   
         LA    RF,3(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,VATAMNT),(10,(RF)),2,ALIGN=LEFT,MINUS=YES                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         ST    RF,FULL                                                          
         LA    R1,TEMP                                                          
**NOP    BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
TAXFMTX  XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                                    
*         R3 = ADDRESS OF GRID TAB ENTRY                                        
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
GRDSP    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
         MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
*                                                                               
         L     R4,ATSARREC             R2=A(TSAR RECORD)                        
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*-----------------------------------                                            
* DR OR CR?                                                                     
*-----------------------------------                                            
         CLI   GCTCOID,GCDRCRQ      DR OR CR?                                   
         BNE   GRDSP05                                                          
*                                                                               
         TM    TSDTEST,TRNSDR                                                   
         BO    GRDSP02                                                          
         MVC   TEMP(L'MX@CR),MX@CR                                              
         LHI   R1,L'MX@CR                                                       
         B     GRDSPX                                                           
GRDSP02  MVC   TEMP(L'MX@DR),MX@DR                                              
         LHI   R1,L'MX@DR                                                       
         B     GRDSPX                                                           
*-----------------------------------                                            
* BILL NUMBER                                                                   
*-----------------------------------                                            
         USING PTDELD,R4                                                        
GRDSP05  CLI   GCTCOID,GCVBNO       BILL NUMBER?                                
         BNE   GRDSP10                                                          
         ICM   R4,15,APTAELD                                                    
         BZ    GRDSPX                                                           
*                                                                               
GRDSP06  CLI   PTDEL,PTAELQ         X'77'                                       
         BNE   GRDSPX                                                           
         LA    R2,TEMP                                                          
GRDSP07  CLC   0(L'PTDBLNO,R2),SPACES                                           
         BNH   GRDSP08                                                          
         CLC   0(L'PTDBLNO,R2),PTDBLNO                                          
         BE    GRDSP09                                                          
         MVC   6(2,R2),=C', '                                                   
         LA    R1,2(R1)                                                         
         CHI   R1,L'TEMP-7                                                      
         BL    *+12                                                             
         LHI   R1,L'TEMP                                                        
         B     GRDSPX                                                           
         LA    R2,8(R2)                                                         
         B     GRDSP07                                                          
*                                                                               
GRDSP08  MVC   0(L'PTDBLNO,R2),PTDBLNO                                          
*                                                                               
         LA    R1,6(R1)                                                         
         CHI   R1,L'TEMP-7                                                      
         BL    *+12                                                             
         LHI   R1,L'TEMP                                                        
         B     GRDSPX                                                           
GRDSP09  SR    RF,RF                                                            
         IC    RF,PTDLN                                                         
         AR    R4,RF                                                            
         B     GRDSP06                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* Client Check Date                                                             
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP10  CLI   GCTCOID,GCCCDT       CLIENT CHECK DATE?                          
         BNE   GRDSP20                                                          
         ICM   R4,15,ACFRELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWRNOC                                                     
         BZ    GRDSPX                                                           
         LA    R4,CFWRLNQ(R4)                                                   
         USING CFWRCD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP11  OC    CFWCCDT,CFWCCDT                                                  
         BNZ   GRDSP14                                                          
         LA    R4,CFWRCLNQ(R4)                                                  
         BCT   R5,GRDSP11                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP12  OC    CFWCCDT,CFWCCDT                                                  
         BZ    GRDSP15                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP14  GOTO1 VDATCON,DMCB,(1,CFWCCDT),(8,(R2))                                
         LA    R2,8(R2)                                                         
GRDSP15  LA    R4,CFWRCLNQ(R4)                                                  
         BCT   R5,GRDSP12                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Client Check NUMBER                                                           
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP20  CLI   GCTCOID,GCCCNO       CLIENT CHECK NUMBER?                        
         BNE   GRDSP40                                                          
         ICM   R4,15,ACFRELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWRNOC                                                     
         BZ    GRDSPX                                                           
         LA    R4,CFWRLNQ(R4)                                                   
         USING CFWRCD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP21  OC    CFWCCNO,CFWCCNO                                                  
         BNZ   GRDSP24                                                          
         LA    R4,CFWRCLNQ(R4)                                                  
         BCT   R5,GRDSP21                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP22  OC    CFWCCNO,CFWCCNO                                                  
         BZ    GRDSP25                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP24  MVC   0(L'CFWCCNO,R2),CFWCCNO                                          
         LA    R2,L'CFWCCNO(R2)                                                 
GRDSP25  LA    R4,CFWRCLNQ(R4)                                                  
         BCT   R5,GRDSP22                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Vendor Status                                                                 
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP40  CLI   GCTCOID,GCVSTA       VENDOR STATUS                               
         BNE   GRDSP60                                                          
         ICM   R4,15,ACFVELD                                                    
         BZ    GRDSP60                                                          
         SR    R5,R5                                                            
         ICM   R5,1,CFWVNOC                                                     
         BZ    GRDSP60                                                          
         XC    BYTE,BYTE                                                        
*                                                                               
         LA    R4,CFWVLNQ(R4)                                                   
         USING CFWVCD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP42  OC    CFWVCDI,CFWVCDI                                                  
         BNZ   GRDSP46                                                          
         TM    CFWVSTA,TRNSURG+TRNSAPPR+TRNSREV+TRNSHOLD                        
         BNZ   GRDSP46                                                          
         TM    CFWVST2,TRSSOFFS+TRSSVOID                                        
         BNZ   GRDSP46                                                          
GRDSP43  LA    R4,CFWVCLNQ(R4)                                                  
         BCT   R5,GRDSP42                                                       
         B     GRDSP59                                                          
*                                                                               
GRDSP46  TM    BYTE,X'80'                INTERNAL VENDOR INVOICE                
         BO    GRDSP48                                                          
         OC    CFWVCDI,CFWVCDI                                                  
         BZ    GRDSP48                                                          
         MVC   0(L'MX@INT,R2),MX@INT                                            
         MVI   L'MX@INT(R2),C','                                                
         LA    R2,L'MX@INT+1(R2)                                                
         OI    BYTE,X'80'                                                       
*                                                                               
GRDSP48  TM    BYTE,X'40'                URGENT                                 
         BO    GRDSP50                                                          
         TM    CFWVSTA,TRNSURG                                                  
         BZ    GRDSP50                                                          
         MVC   0(L'MX@URG,R2),MX@URG                                            
         MVI   L'MX@URG(R2),C','                                                
         LA    R2,L'MX@URG+1(R2)                                                
         OI    BYTE,X'40'                                                       
*                                                                               
GRDSP50  TM    BYTE,X'20'                APPROVED                               
         BO    GRDSP51                                                          
         TM    CFWVSTA,TRNSAPPR                                                 
         BZ    GRDSP51                                                          
         MVC   0(L'MX@APRVD,R2),MX@APRVD                                        
         MVI   L'MX@APRVD(R2),C','                                              
         LA    R2,L'MX@APRVD+1(R2)                                              
         OI    BYTE,X'20'                                                       
*                                                                               
GRDSP51  TM    BYTE,X'10'                REVERSAL                               
         BO    GRDSP52                                                          
         TM    CFWVSTA,TRNSREV                                                  
         BZ    GRDSP52                                                          
         MVC   0(L'MX@RVRSL,R2),MX@RVRSL                                        
         MVI   L'MX@RVRSL(R2),C','                                              
         LA    R2,L'MX@RVRSL+1(R2)                                              
         OI    BYTE,X'10'                                                       
*                                                                               
GRDSP52  TM    BYTE,X'08'                OFFSET                                 
         BO    GRDSP53                                                          
         TM    CFWVST2,TRSSOFFS                                                 
         BZ    GRDSP53                                                          
         MVC   0(L'MX@OFFST,R2),MX@OFFST                                        
         MVI   L'MX@OFFST(R2),C','                                              
         LA    R2,L'MX@OFFST+1(R2)                                              
         OI    BYTE,X'08'                                                       
*                                                                               
GRDSP53  TM    BYTE,X'04'                VOIDED                                 
         BO    GRDSP54                                                          
         TM    CFWVST2,TRSSVOID                                                 
         BZ    GRDSP54                                                          
         MVC   0(L'MX@BNKVD,R2),MX@BNKVD                                        
         MVI   L'MX@BNKVD(R2),C','                                              
         LA    R2,L'MX@BNKVD+1(R2)                                              
         OI    BYTE,X'04'                                                       
*                                                                               
GRDSP54  TM    BYTE,X'02'                HELD                                   
         BO    GRDSP55                                                          
         TM    CFWVSTA,TRNSHOLD                                                 
         BZ    GRDSP55                                                          
         MVC   0(L'MX@HELD,R2),MX@HELD                                          
         MVI   L'MX@HELD(R2),C','                                               
         LA    R2,L'MX@HELD+1(R2)                                               
         OI    BYTE,X'02'                                                       
*                                                                               
GRDSP55  B     GRDSP43                                                          
*                                                                               
GRDSP59  CLI   BYTE,0               ANYTHING DISPLAYED?                         
         BE    GRDSPX                                                           
         BCTR  R2,0                                                             
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Receivable Status                                                             
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP60  CLI   GCTCOID,GCRSTA       Receivable Status                           
         BNE   GRDSPX                                                           
         ICM   R4,15,ACFRELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWRNOC                                                     
         BZ    GRDSPX                                                           
         XC    BYTE,BYTE                                                        
*                                                                               
         LA    R4,CFWRLNQ(R4)                                                   
         USING CFWRCD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP64  CLI   CFWRSTA,0                                                        
         BNE   GRDSP68                                                          
GRDSP65  LA    R4,CFWRCLNQ(R4)                                                  
         BCT   R5,GRDSP64                                                       
         B     GRDSP99                                                          
*                                                                               
GRDSP68  TM    BYTE,X'80'                HELD                                   
         BO    GRDSP72                                                          
         TM    CFWRSTA,TRXSRHLD                                                 
         BZ    GRDSP72                                                          
         MVC   0(L'MX@HELD,R2),MX@HELD                                          
         MVI   L'MX@HELD(R2),C','                                               
         LA    R2,L'MX@HELD+1(R2)                                               
         OI    BYTE,X'80'                                                       
*                                                                               
GRDSP72  TM    BYTE,X'40'                QUERIED                                
         BO    GRDSP76                                                          
         TM    CFWRSTA,TRXSRQRD                                                 
         BZ    GRDSP76                                                          
         MVC   0(L'MX@QUERD,R2),MX@QUERD                                        
         MVI   L'MX@QUERD(R2),C','                                              
         LA    R2,L'MX@QUERD+1(R2)                                              
         OI    BYTE,X'40'                                                       
*                                                                               
GRDSP76  TM    BYTE,X'20'                REVERSAL                               
         BO    GRDSP80                                                          
         TM    CFWRSTA,TRNSREV                                                  
         BZ    GRDSP80                                                          
         MVC   0(L'MX@RVRSL,R2),MX@RVRSL                                        
         MVI   L'MX@RVRSL(R2),C','                                              
         LA    R2,L'MX@RVRSL+1(R2)                                              
         OI    BYTE,X'20'                                                       
*                                                                               
GRDSP80  TM    BYTE,X'10'                WRITE OFF                              
         BO    GRDSP84                                                          
         TM    CFWRSTA,CFWRSWO                                                  
         BZ    GRDSP84                                                          
         MVC   0(L'MX@WRTF,R2),MX@WRTF                                          
         MVI   L'MX@WRTF(R2),C','                                               
         LA    R2,L'MX@WRTF+1(R2)                                               
         OI    BYTE,X'10'                                                       
*                                                                               
GRDSP84  TM    BYTE,X'08'                OFFSET                                 
         BO    GRDSP88                                                          
         TM    CFWRSTA,CFWRSOF                                                  
         BZ    GRDSP88                                                          
         MVC   0(L'MX@OFFST,R2),MX@OFFST                                        
         MVI   L'MX@OFFST(R2),C','                                              
         LA    R2,L'MX@OFFST+1(R2)                                              
         OI    BYTE,X'08'                                                       
*                                                                               
GRDSP88  TM    BYTE,X'04'                TRANSFER                               
         BO    GRDSP95                                                          
         TM    CFWRSTA,CFWRSTR                                                  
         BZ    GRDSP95                                                          
         MVC   0(L'MX@XFR,R2),MX@XFR                                            
         MVI   L'MX@XFR(R2),C','                                                
         LA    R2,L'MX@XFR+1(R2)                                                
         OI    BYTE,X'04'                                                       
*                                                                               
GRDSP95  B     GRDSP65                                                          
*                                                                               
GRDSP99  CLI   BYTE,0               ANYTHING DISPLAYED?                         
         BE    GRDSPX                                                           
         BCTR  R2,0                                                             
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
GRDSPX   LTR   R1,R1                                                            
         BP    *+8                                                              
         LHI   R1,1                                                             
         J     XITR1                                                            
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
* JOB CASHFLOW GRID COLUMN TALBE - COVERED BY GCTBLD                            
***********************************************************************         
GCTBL    DS    0F                                                               
*        -----------------------------------------------                        
GCTCON   DC    AL1(GCONLQ,GCCONTRA,L'LC@CTRA,L'TSDCONT)    CONTRA               
         DC    AL2(LC@CTRA-WORKD,TSDCONT-TSARDATD)                              
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'TEMPHTOT),AL2(TEMPHTOT-OVERWRKD)                         
GCONLQ   EQU   *-GCTCON                                                         
*        -----------------------------------------------                        
GCTREF   DC    AL1(GREFLQ,GCREF,L'MX@INVC2,L'TSDTREF)      INVOICE              
         DC    AL2(MX@INVC2-OVERWRKD,TSDTREF-TSARDATD)     NUMBER               
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GREFLQ   EQU   *-GCTREF                                                         
*        -----------------------------------------------                        
GCTDATE  DC    AL1(GDATELQ,GCDATE,L'MX@RSIDT,1)            INVOICE              
         DC    AL2(MX@RSIDT-OVERWRKD,TSDTDAT-TSARDATD)     DATE                 
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GDATELQ  EQU   *-GCTDATE                                                        
*        -----------------------------------------------                        
GCTAMNT  DC    AL1(GAMTLQ,GCAMNT,L'MX@AMNET,L'TSDTAMNT)    AMOUNT               
         DC    AL2(MX@AMNET-OVERWRKD,TSDTAMNT-TSARDATD)                         
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,GCTIZAB)                 
         DC    AL1(0,L'TSTTTOT),AL2(TSTTTOT-TSARTOTD)                           
GAMTLQ   EQU   *-GCTAMNT                                                        
*        -----------------------------------------------                        
GCTVBNO  DC    AL1(GVBNOLQ,GCVBNO,L'MX@RSINO,0)            BILL NUMBER          
         DC    AL2(MX@RSINO-OVERWRKD,AGRDSP-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI1ST)                               
         DC    AL1(0,0),AL2(0)                                                  
GVBNOLQ  EQU   *-GCTVBNO                                                        
*        -----------------------------------------------                        
GCTVDQU  DC    AL1(GVDQULQ,GCVDQU,9,CFWIDBQ)               DISBURSED            
         DC    AL2(MX@RSVDI-OVERWRKD),AL1(ELETSARQ,CFWVIND-CFWELD)              
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,GCTIYON)                          
         DC    AL1(0,GCTI1ST+GCTIZAB)                                           
         DC    AL1(CFWVELQ,0),AL2(0)                                            
GVDQULQ  EQU   *-GCTVDQU                                                        
*        -----------------------------------------------                        
GCTVDIS  DC    AL1(GVDISLQ,GCVDIS,L'MX@RSVDI,L'CFWVDIS)   DISBURSED AMT         
         DC    AL2(MX@RSVDI-OVERWRKD),AL1(ELETSARQ,CFWVDIS-CFWELD)              
         DC    AL1(GCTITOT+GCTIELEM+GCTIELST+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,GCTI1ST+GCTIZAB)                            
         DC    AL1(CFWVELQ,L'TSTDITOT),AL2(TSTDITOT-TSARTOTD)                   
GVDISLQ  EQU   *-GCTVDIS                                                        
*        -----------------------------------------------                        
GCTVCDT  DC    AL1(GVCDTLQ,GCVCDT,L'MX@RSVCD,2)            VENDOR               
         DC    AL2(MX@RSVCD-OVERWRKD)                      CHECK                
         DC    AL1(ELETSARQ,((CFWVCDT-CFWVCD)+CFWVLNQ))    DATE                 
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0)                                
         DC    AL1(GCTFDAT+GCTFRGHT,GCTI1ST)                                    
         DC    AL1(CFWVELQ,0),AL2(0)                                            
GVCDTLQ  EQU   *-GCTVCDT                                                        
*        -----------------------------------------------                        
GCTVCNO  DC    AL1(GVCNOLQ,GCVCNO,L'MX@RSVCK,L'CFWVCNO)    VENDOR               
         DC    AL2(MX@RSVCK-OVERWRKD)                      CHECK                
         DC    AL1(ELETSARQ,((CFWVCNO-CFWVCD)+CFWVLNQ))    NUMBER               
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0,0,GCTI1ST)                      
         DC    AL1(CFWVELQ,0),AL2(0)                                            
GVCNOLQ  EQU   *-GCTVCNO                                                        
*        -----------------------------------------------                        
GCTVSTA  DC    AL1(GVSTALQ,GCVSTA,L'MX@RSVST,0)            VENDOR               
         DC    AL2(MX@RSVST-OVERWRKD,AGRDSP-OVERWRKD)      STATUS               
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI1ST)                               
         DC    AL1(0,0),AL2(0)                                                  
GVSTALQ  EQU   *-GCTVSTA                                                        
*        -----------------------------------------------                        
GCTRCPM  DC    AL1(GRCPMLQ,GCRCPM,L'MX@CPM,0)            Client Payment         
         DC    AL2(MX@CPM-OVERWRKD),AL1(ELETSARQ,CFWRIND-CFWELD)                
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,GCTIYON,0,GCTI2ND)                
         DC    AL1(CFWRELQ,0),AL2(0)                                            
GRCPMLQ  EQU   *-GCTRCPM                                                        
GCTRCP2  DC    AL1(GRCP2LQ,GCRCPM,L'MX@PYMNT,CFWICPQ)      Payment              
         DC    AL2(MX@PYMNT-OVERWRKD),AL1(ELETSARQ,CFWRIND-CFWELD)              
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,GCTIYON,0,GCTI2ND)                
         DC    AL1(CFWRELQ,0),AL2(0)                                            
GRCP2LQ  EQU   *-GCTRCP2                                                        
GCTRCPP  DC    AL1(GRCPPLQ,GCRCPM,L'MX@PART,CFWIPCPQ)      Partial              
         DC    AL2(MX@PART-OVERWRKD),AL1(ELETSARQ,CFWRIND-CFWELD)               
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,GCTIYON,0,GCTI2ND)                
         DC    AL1(CFWRELQ,0),AL2(0)                                            
GRCPPLQ  EQU   *-GCTRCPP                                                        
*        -----------------------------------------------                        
GCTCPAM  DC    AL1(GCPAMLQ,GCCPAM,L'MX@CPAMT,L'CFWAPPA)    Cli Pay Amt          
         DC    AL2(MX@CPAMT-OVERWRKD),AL1(ELETSARQ,CFWAPPA-CFWELD)              
         DC    AL1(GCTITOT+GCTIELEM+GCTIELST+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,GCTI2ND+GCTIZAB)                            
         DC    AL1(CFWRELQ,L'TSTCPTOT),AL2(TSTCPTOT-TSARTOTD)                   
GCPAMLQ  EQU   *-GCTCPAM                                                        
*        -----------------------------------------------                        
GCTCCDT  DC    AL1(GCCDTLQ,GCCCDT,L'MX@RSRCD,0)            CLIENT               
         DC    AL2(MX@RSRCD-OVERWRKD,AGRDSP-OVERWRKD)      CHECK                
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI2ND)          DATE                 
         DC    AL1(0,0),AL2(0)                                                  
GCCDTLQ  EQU   *-GCTCCDT                                                        
*        -----------------------------------------------                        
GCTCCNO  DC    AL1(GCCNOLQ,GCCCNO,L'MX@RSRCK,L'CFWCCNO)    CLIENT               
         DC    AL2(MX@RSRCK-OVERWRKD,AGRDSP-OVERWRKD)      CHECK                
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI2ND)          NUMBER               
         DC    AL1(0,0),AL2(0)                                                  
GCCNOLQ  EQU   *-GCTCCNO                                                        
*        -----------------------------------------------                        
GCTRSTA  DC    AL1(GRSTALQ,GCRSTA,L'MX@RSRCS,0)            RECEIVABLE           
         DC    AL2(MX@RSRCS-OVERWRKD,AGRDSP-OVERWRKD)      STATUS               
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GRSTALQ  EQU   *-GCTRSTA                                                        
*        -----------------------------------------------                        
GCTBILTY DC    AL1(GBILTYLQ,GCBILTY,L'MX@BLGTY,L'NARBTYPE) BILLING TYPE         
         DC    AL2(MX@BLGTY-OVERWRKD),AL1(ELETSARQ,NARLN1Q)                     
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0,0,GCTI2ND)                      
         DC    AL1(NARELQ,0),AL2(0)                                             
GBILTYLQ EQU   *-GCTBILTY                                                       
*        -----------------------------------------------                        
GCTWCD   DC    AL1(GWCDLQ,GCWCD,L'LC@WC2,L'TSDWORK)        WORKCODE             
         DC    AL2(LC@WC2-WORKD,TSDWORK-TSARDATD)                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
GWCDLQ   EQU   *-GCTWCD                                                         
*        -----------------------------------------------                        
GCTDRS   DC    AL1(GDRSLQ,GCDRS,L'MX@DRNET,L'TSDTAMNT)     DEBITS               
         DC    AL2(MX@DRNET-OVERWRKD,TSDTAMNT-TSARDATD)                         
         DC    AL1(GCTIOVER+GCTITOT,GCTIDR)                                     
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(TSDTEST-TSDTAMNT,L'TSTTDR),AL2(TSTTDR-TSARTOTD)              
GDRSLQ   EQU   *-GCTDRS                                                         
*        -----------------------------------------------                        
GCTBILD  DC    AL1(GBILDLQ,GCBILLED,1,L'TSDALLOC)          BILLED               
         DC    AL2(UP@BATL-WORKD,TSDALLOC-TSARDATD)                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
GBILDLQ  EQU   *-GCTBILD                                                        
*        -----------------------------------------------                        
GCTCRS   DC    AL1(GCRSLQ,GCCRS,L'MX@CRNET,L'TSDTAMNT)     CREDITS              
         DC    AL2(MX@CRNET-OVERWRKD,TSDTAMNT-TSARDATD)                         
         DC    AL1(GCTIOVER+GCTITOT,GCTICR)                                     
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(TSDTEST-TSDTAMNT,L'TSTTCR),AL2(TSTTCR-TSARTOTD)              
GCRSLQ   EQU   *-GCTCRS                                                         
*        -----------------------------------------------                        
GCTBAL   DC    AL1(GBALLQ,GCBAL,L'MX@BANET,L'TEMPBAL)      BALANCE              
         DC    AL2(MX@BANET-OVERWRKD,TEMPBAL-OVERWRKD)                          
         DC    AL1(GCTITOT+GCTIOVER,GCTISPDA)                                   
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,L'TSTTTOT),AL2(TSTTTOT-TSARTOTD)                           
GBALLQ   EQU   *-GCTBAL                                                         
*        -----------------------------------------------                        
GCTDRCR  DC    AL1(GDRCRLQ,GCDRCRQ,L'MX@DRCR,L'TSDTAMNT)   DR/CR                
         DC    AL2(MX@DRCR-OVERWRKD,AGRDSP-OVERWRKD)                            
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GDRCRLQ  EQU   *-GCTDRCR                                                        
*        -----------------------------------------------                        
GCTACDTE DC    AL1(GACDLQ,GCACTDTE,L'MX@ACTDT,2)           ACTIVITY             
         DC    AL2(MX@ACTDT-OVERWRKD,TSDACDAT-TSARDATD)    DATE                 
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GACDLQ   EQU   *-GCTACDTE                                                       
*        -----------------------------------------------                        
GCTBREF  DC    AL1(GBREFLQ,GCBREF,L'MX@RSBRF,L'TSDBREF)    BATCH REF            
         DC    AL2(MX@RSBRF-OVERWRKD,TSDBREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GBREFLQ  EQU   *-GCTBREF                                                        
*        -----------------------------------------------                        
GCTTRNTY DC    AL1(GTRNTYLQ,GCTRNTYP,L'MX@TRANT,1)         TRANS TYPE           
         DC    AL2(MX@TRANT-OVERWRKD,TSDTTYPE-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFCENT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GTRNTYLQ EQU   *-GCTTRNTY                                                       
*        -----------------------------------------------                        
GCTXFRT  DC    AL1(GXFRTLQ,GCXFRT,L'MX@XFRT,L'PXDFRTO)     TRANSFER             
         DC    AL2(MX@XFRT-OVERWRKD),AL1(PXDELQ,PXDFRTO-PXDELD) TO              
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)       ACCOUNT              
         DC    AL1(PXDTTO,0,0,0)                                                
GXFRTLQ  EQU   *-GCTXFRT                                                        
*        -----------------------------------------------                        
GCTXFRF  DC    AL1(GXFRFLQ,GCXFRF,L'MX@XFRF,L'PXDFRTO)     TRANSFER             
         DC    AL2(MX@XFRF-OVERWRKD),AL1(PXDELQ,PXDFRTO-PXDELD) FROM            
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)       ACCOUNT              
         DC    AL1(PXDTFROM,0,0,0)                                              
GXFRFLQ  EQU   *-GCTXFRF                                                        
*        -----------------------------------------------                        
GCTDA    DC    AL1(GDALQ,GCDADDR,L'MX@DSCAD,L'TSDDA)       DISK ADDRESS         
         DC    AL2(MX@DSCAD-OVERWRKD,TSDDA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIHEX+GCTIDDS,0,0)                                
         DC    AL1(0,0),AL2(0)                                                  
GDALQ    EQU   *-GCTDA                                                          
*        -----------------------------------------------                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
* JOB DETAIL GRID COLUMN EQUATES                                                
*---------------------------------------------------------------------          
GCWCD    EQU   1        WORK CODE                                               
GCCONTRA EQU   92       CONTRA                                                  
GCDATE   EQU   3        DATE                                                    
GCREF    EQU   4        REFERENCE                                               
GCBREF   EQU   5        BATCH REFERENCE                                         
GCAMNT   EQU   6        AMOUNT                                                  
GCDRCRQ  EQU   7        DR/CR                                                   
GCBILLED EQU   8        BILLED?                                                 
GCHOURS  EQU   9        HOURS                                                   
GCNARR   EQU   10       NARRATIVE                                               
GCBILTY  EQU   11       BILLING TYPE                                            
GCNETAM  EQU   12       NET AMOUNT                                              
GCGST    EQU   13       GST                                                     
GCACTDTE EQU   14       ACTIVITY DATE                                           
GCBRUND  EQU   15       BILLING RUN DATE                                        
GCPEELDT EQU   16       PEELED DATE                                             
GCTIMT   EQU   17       TIME TYPE                                               
GCAPE    EQU   18       ANALYSIS POINTER                                        
GCPST    EQU   19       PST                                                     
GCCOMM   EQU   20       COMMISSION                                              
GCORD    EQU   21       ORDER                                                   
GCDRAFT  EQU   22       DRAFT                                                   
GCMOA    EQU   23       MOA                                                     
GCSEQ    EQU   24       SEQUENCE #                                              
GCTRNTYP EQU   25       TRAN TYPE                                               
GCXFRD   EQU   26       TRANSFER DATE                                           
GCXFRT   EQU   27       TRANSFER TO                                             
GCXFRF   EQU   28       TRANSFER FROM                                           
GCUNI    EQU   29       UNIT                                                    
GCUNP    EQU   30       PRICE                                                   
GCHELD   EQU   31       HELD                                                    
GCVCNO   EQU   32       VENDOR CHECK NUMBER                                     
GCVCDT   EQU   33       VENDOR CHECK DATE                                       
GCVBNO   EQU   34       BILL NUMBER                                             
GCVBDT   EQU   35       BILL DATE                                               
GCVDIS   EQU   36       DISBURSED AMOUNT                                        
GCVUND   EQU   37       Vendor Balance                                          
GCCCNO   EQU   38       Client Check Number                                     
GCCCDT   EQU   39       Client Check Date                                       
GCCPAM   EQU   40       Applied Amount                                          
GCUNAA   EQU   41       Unapplied Amount                                        
GCVDQU   EQU   42       Disbursed?                                              
GCRCPM   EQU   43       Client Payment?                                         
GCDRS    EQU   44       DEBITS                                                  
GCCRS    EQU   45       CREDITS                                                 
GCBAL    EQU   46       BALANCE                                                 
GCVSTA   EQU   47       Vendor Status                                           
GCRSTA   EQU   48       Receivable Status                                       
GCGRSBIL EQU   49       Gross Billed                                            
GCINV    EQU   50       Invoice Number                                          
* DDS ONLY *                                                                    
GCTRNST  EQU   82       TRANSACTION STATUS                                      
GCDADDR  EQU   83       DISK ADDRESS                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
* WORKING STORAGE                                                               
*********************************************************************           
OVERWRKD DSECT                                                                  
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGRDSP   DS    A                   ADDRESS OF GRID SPECIAL COLUMN CALCS         
KEYSTORE DS    XL(L'TRNKEY)        TEMPORARY SAVED KEY                          
DADDRESS DS    CL(L'TRNKDA)        TRANSACTION DISK ADDRESS                     
ACCNAM   DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ACCNAMLN DS    X                   ACCOUNT NAME LENGTH                          
JOBSTAT  DS    XL(L'JOBSTA1)       JOB STATUS                                   
VATVL    DS    0C                                                               
VATNAME  DS    XL3                 VAT TYPE NAME                                
VATRULE  DS    XL(L'VTCTYPE)       VAT TYPE                                     
VATRATE  DS    XL(L'RATRATE)       VAT RATE                                     
VATINDS  DS    XL1                 INDICATOR BYTE                               
VATIDC3  EQU   TAXIDC3             X'20' = RATE IS 3-DECIMAL PLACES             
VATVLL1Q EQU   *-VATVL                                                          
VATAMNT  DS    PL(L'VBIVAT)        VAT AMOUNT                                   
ORD      DS    PL(L'FFNONUM)       TEMP ORDER NUM                               
TEMPBAL  DS    PL16                BALANCE (DEBITS-CREDITS)                     
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
TEMPHTOT DS    CL20                TEMPORARY TOTAL HEADER FIELD                 
INTFLAG  DS    X                   GENERAL INTERNAL FLAG                        
SCRFULLQ EQU   X'01'               SCREEN FULL                                  
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
TRANAMNT DS    CL(L'TRNAMNT)       TRANSACTION AMOUNT LOCAL/AGENCY              
*---------------------------------------------------------------------          
*      STORAGE FOR CASHFLOW                                                     
*---------------------------------------------------------------------          
PRCFGRS  DS    PL8             Gross Amount                                     
PRPRAMT  DS    PL8                                                              
PRCPRSTA DS    X               Process prod receivable status                   
PRCPAPB  EQU   X'40'           . Already Processed Bill                         
PRCPNOAD EQU   X'20'           . No adjusting needed                            
PRCPRUB  EQU   X'10'           . Already read for upfront billing               
*                                                                               
PRCPRTAB DS    (MAXRCVTQ)XL(L'TRNKULA+L'TRNKSBR) Rcvble transfer table          
         DC    X'FF'           END OF TABLE                                     
MAXRCVTQ EQU   5                                                                
*---------------------------------------------------------------------*         
* DATA DICTIONARY                                                               
*---------------------------------------------------------------------*         
DSMIX    DS    0C                                                               
MX@ACC   DS    CL9                                                              
MX@DISS  DS    CL7                                                              
MX@PAYBL DS    CL7                                                              
MX@FLTX  DS    CL7                                                              
MX@ORDER DS    CL7                                                              
MX@SUBR  DS    CL7                                                              
MX@TYPE  DS    CL2,CL1                                                          
MX@STT   DS    CL2,CL1                                                          
MX@UDAT  DS    CL2,CL1                                                          
MX@DATE  DS    CL2,CL1                                                          
MX@PELDT DS    CL2,CL1                                                          
MX@CR    DS    CL2,CL1                                                          
MX@DR    DS    CL2,CL1                                                          
MX@ACCT  DS    CL15                                                             
MX@CMN   DS    CL10                                                             
MX@VAT   DS    CL5                                                              
MX@TYPE1 DS    CL5                                                              
MX@DSCAD DS    CL3                                                              
MX@BLGTY DS    CL12                                                             
MX@DRCR  DS    CL3                                                              
MX@ACTDT DS    CL13              Activity Date                                  
MX@MOA   DS    CL3               MOA                                            
MX@TRANT DS    CL10              Trans Type                                     
MX@RSBRF DS    CL9               Batch Ref                                      
MX@INVC2 DS    CL14              Invoice Number                                 
MX@RSIDT DS    CL12              Invoice Date                                   
MX@RSVCK DS    CL14              Vendor Chk Num                                 
MX@RSVCD DS    CL15              Vendor Chk date                                
MX@RSVDI DS    CL16              Disbursed Amount                               
MX@RSVUN DS    CL18              Undisbursed Amount                             
MX@RSRCK DS    CL14              Client Chk Num                                 
MX@RSRCD DS    CL15              Client Chk date                                
MX@RSINO DS    CL11              Bill Number                                    
MX@RSBDT DS    CL9               Bill Date                                      
MX@RSRCU DS    CL18              Receivable Balance                             
MX@RSRCB DS    CL14              Applied Amount                                 
MX@BAL   DS    CL8               Balance                                        
MX@OFFST DS    CL6               Offset                                         
MX@GRSBD DS    CL12              Gross Billed                                   
MX@RSBCM DS    CL17              Billed Commission                              
MX@URG   DS    CL6               Urgent                                         
MX@HELD  DS    CL4               Held                                           
MX@INT   DS    CL8               Internal                                       
MX@APRVD DS    CL8               Approved                                       
MX@RVRSL DS    CL8               Reversal                                       
MX@BNKVD DS    CL6               Voided                                         
MX@QUERD DS    CL7               Queried                                        
MX@WRTF  DS    CL9               Write off                                      
MX@XFR   DS    CL8               Transfer                                       
MX@RSCCM DS    CL17              Credit Commission                              
MX@RSCGR DS    CL12              Credit Gross                                   
MX@PART  DS    CL7               Partial                                        
MX@RSVST DS    CL13              Vendor Status                                  
MX@RSRCS DS    CL17              Receivable Status                              
MX@PYMNT DS    CL7               Payment                                        
MX@CPM   DS    CL14              Client Payment                                 
MX@CPAMT DS    CL18              Client Payment Amt                             
MX@DRNET DS    CL12              Debits (Net)                                   
MX@CRNET DS    CL13              Credits (Net)                                  
MX@BANET DS    CL13              Balance (Net)                                  
MX@AMNET DS    CL12              Amount (Net)                                   
MX@TFOR  DS    CL10              Total for                                      
MX@CHGS  DS    CL7               Charges                                        
MX@BLG   DS    CL7               Billing                                        
MX@XFRT  DS    CL19              Transfer to Account                            
MX@XFRF  DS    CL21              Transfer from Account                          
*                                                                               
DSCAP    DS    0C                                                               
AC@ONT   DS    CL3                                                              
AC@PST   DS    CL3                                                              
AC@QST   DS    CL3                                                              
AC@HST   DS    CL3                                                              
*                                                                               
PRATBLK  DS    XL(PR$LNQ)          PRORATA BLOCK                                
*********************************************************************           
* BUFFERED INVOICE DATA DSECT (CONTRA/INVOICE #/INVOICE DATE)                   
*********************************************************************           
TEMPD    DSECT                     TEMP ENTRIES FOR READING UP FRONT            
TEMPDRCR DS    X                   DEBIT OR CREDIT                              
TEMPDRQ  EQU   X'00'               . DEBIT                                      
TEMPCRQ  EQU   X'FF'               . CREDIT                                     
TEMPCONT DS    CL(L'CHDKULA)       CONTRA ACCOUNT CODE                          
TEMPINV  DS    CL6                 INVOICE NUMBER                               
TEMPIDT  DS    CL3                 INVOICE DATE                                 
TEMPWC   DS    CL2                 WORKCODE                                     
TEMPLNQ  EQU   *-TEMPD                                                          
MAXTEMP  EQU   (L'TEMPBLK/TEMPLNQ)                                              
***********************************************************************         
* TSAR RECORD. NOTE THIS RECORD MAY HAVE ELEMENTS ATTACHED                      
* POSSIBLE GENFILE ELEMENTS INCLUDE                                             
* OTHELD 'OTHERS' ELEMENT                                                       
* FFNELD 'FREE FORM NUMBER' ELEMENT                                             
* RATELD 'GENERAL RATE' ELEMENT                                                 
* PXDELD 'POSTING XFER INFO' ELEMENT                                            
* SCIELD 'SUBSIDIARY CASH' ELEMENT                                              
* TPRELD 'PRICE LIST' ELEMENT                                                   
*                                                                               
* POSSIBLE INTERNAL ELEMENTS INCLUDE                                            
* NARELD 'TRANSACTION NARRATIVE ELEMENT                                         
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
*                                                                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDWORK  DS    CL(L'TRNKWORK)      WORK CODE                                    
TSDCONT  DS    CL(L'TRNKULC)       CONTRA CODE                                  
TSDTDAT  DS    PL(L'TRNKDATE)      TRANSACTION DATE                             
TSDTREF  DS    CL(L'TRNKREF)       TRANSACTION REFERENCE                        
TSDBREF  DS    CL(L'TRNBTCH)       BATCH REFERENCE                              
TSDSBREF DS    CL(L'TRNKSBR)       TRANSACTION SUB-REFERENCE                    
TSDTAMNT DS    PL(L'TRNAMNT)       TRANSACTION AMOUNT                           
TSDTAMTY DS    XL1                 TRANSACTION AMOUNT TYPE                      
TSDTORDQ EQU   X'80'               ORDER AMOUNTS INCLUDED                       
TSDTMEMQ EQU   X'40'               MEMO EXPENSE (JOB PROFITABILITY)             
TSDTRST  DS    XL(L'TRNRSTAT)      TRANSACTION RECORD STATUS                    
TSDTEST  DS    XL(L'TRNSTAT)       TRANSACTION ELEMENT STATUS                   
TSDTTYPE DS    CL(L'TRNTYPE)       INPUT TYPE                                   
TSDTMOA  DS    PL(L'TRNKSMOS)      MOA                                          
TSDACDAT DS    XL(L'TRSDATE)       ACTIVITY DATE                                
TSDUSDAT DS    XL(L'TRSUDAT)       USED DATE                                    
TSDPEDAT DS    XL(L'TRSPDAT)       PEELED DATE                                  
TSDSTAT2 DS    XL(L'TRSSTAT2)      TRANSACTION STATUS ELE STAT BYTE 2           
TSDALLOC DS    XL1                 ALLOCATION BYTE                              
TSDALLFB EQU   C'*'                FULLY ALLOCATED                              
TSDALLPB EQU   C'P'                PARTLY ALLOCATED                             
TSDDA    DS    CL(L'TRNKDA)        DISK ADDRESS                                 
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*----------------------------------------------------------------------         
* Total and Subtotal                                                            
*----------------------------------------------------------------------         
TSARTOTD DSECT                     TOTAL LINE                                   
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSSUBITM EQU   3                   SUBTOTAL ITEM TYPE                           
TSTTDR   DS    PL8                 DEBIT TOTAL                                  
TSTTCR   DS    PL8                 CREDIT TOTAL                                 
TSTTTOT  DS    PL8                 TRANSACTION AMOUNT                           
TSTDITOT DS    PL8                 DISBURSED TOTAL                              
TSTCPTOT DS    PL8                 CLIENT PAYMENT TOTAL                         
TSTLNQ   EQU   *-TSARTOTD                                                       
*----------------------------------------------------------------------         
* DSECT TO COVER INFO IN TRANS NARR                                             
*----------------------------------------------------------------------         
NARINFOD DSECT                     DSECT TO COVER INFO IN TRANS NARR            
NARBTYPE DS    CL15                BILLING TYPE                                 
NARCAMT  DS    PL6                 COMMISSION AMOUNT                            
NARDAMT  DS    PL6                 DISCOUNT AMOUNT                              
NARPAMT1 DS    PL6                 PAYABLE AMOUNT                               
         DS    XL30                WHO KNOWS WHAT                               
NARPNQ   EQU   (*-NARPAMT1)/L'NARPAMT1                                          
NARVAMT1 DS    PL6                 VAT USING RATE 1                             
NARVAMT2 DS    PL6                 VAT USING RATE 2                             
NARVAMT3 DS    PL6                 VAT USING RATE 3                             
NARVAMT4 DS    PL6                 VAT USING RATE 4                             
NARVAMT5 DS    PL6                 VAT USING RATE 5 OR SPECIAL RATE             
*                                                                               
NARINLNQ EQU   *-NARINFOD                                                       
*----------------------------------------------------------------------         
* MODIFIED PTAEL - DEBIT BILLS INFORMATION                                      
*----------------------------------------------------------------------         
PTDELD   DSECT                                                                  
PTDEL    DS    X                   X'77' MODIFIED PTAEL                         
*        EQU   X'77'               PTAEL                                        
PTDLN    DS    X                                                                
PTDBLNO  DS    CL6                                                              
PTDBLDT  DS    XL2                                                              
PTDLNQ   EQU   *-PTDELD                                                         
*----------------------------------------------------------------------         
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
APRVTAB  DS    A                   ADDRESS OF THE PROVINCE TABLE                
NEEDSUB  DS    C                   SUBTOTAL NEEDED?                             
NESUBDRQ EQU   C'D'                . NEED DEBIT SUBTOTAL                        
NESUBCRQ EQU   C'C'                . NEED CREDIT SUBTOTAL                       
SAVEWC   DS    CL(L'TRNKWORK)      WORK CODE                                    
SVTEMPN  DS    XL(L'TEMPBLKN)      GENERAL FLAG FOR OVERLAYS                    
JDTFLAG  DS    X                   GENERAL FLAG FOR OVERLAYS                    
JDTNSNQ  EQU   X'80'               . NO SEQUENTIAL NEEDED                       
JDTRENQ  EQU   X'40'               . RE-ENTERING                                
JDTALLQ  EQU   X'20'               . ALL RECORDS READ FOR INITIAL SORT          
JDTTBFQ  EQU   X'10'               . TEMPBLK FULL                               
JDTSTSQ  EQU   X'08'               . INITIAL SORT HAS STARTED                   
JDTPROCQ EQU   X'04'               . TRANSACTION PROCESSED                      
COMBVATR DS    (MAXNVATQ+1)XL6     VATNAME VATTYPE VATRATE                      
JCVALS   DS    0PL8                CREDITOR VALUES                              
DEBTOT   DS    PL8                 DEBIT TOTAL                                  
CRETOT   DS    PL8                 CREDIT TOTAL                                 
TRNTOT   DS    PL8                 TRANSACTION TOTAL                            
DSBTOT   DS    PL8                 DISBURSED TOTAL                              
CPMTOT   DS    PL8                 CLIENT PAYMENT TOTAL                         
JCVALLNQ EQU   *-JCVALS                                                         
PREVBTOT DS    0PL6                PREV BILL TOTALS                             
PREVBCOM DS    PL6                 PREV BILL COMMISSION                         
PREVBDIS DS    PL6                 PREV BILL DISCOUNT                           
PREVBPAY DS    PL6                 PREV BILL PAYABLE                            
PREVBVAT DS    (MAXNVATQ)PL6       PREV BILL VAT                                
PREVBLNQ EQU   *-PREVBTOT          LENGTH OF PREV BILLING ACCUMULATORS          
PREVBNMQ EQU   PREVBLNQ/L'PREVBTOT NUMBER OF PREV BILLING ACCUMULATORS          
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACENQ15   10/19/16'                                      
         END                                                                    
