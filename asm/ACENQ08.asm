*          DATA SET ACENQ08    AT LEVEL 009 AS OF 12/22/17                      
*PHASE T62008A                                                                  
T62008   TITLE 'ACCOUNT FIS - AGEING'                                           
T62008   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ8**,R5,R7,CLEAR=YES,RR=RE                                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE               SET UP GRID COLUMN TABLE                     
         ST    RF,AGCTBL                                                        
         L     RF,=A(GSPECR)                                                    
         AR    RF,RE                                                            
         ST    RF,AGSPECR                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10                                                           
         BAS   RE,FSTDIS           PERFORM FIRST DISPLAY FUNCTIONS              
         BNE   ERRXIT                                                           
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN220                                                          
*                                                                               
MAIN10   DS    0H                                                               
         L     RE,ATSARREC                                                      
         MVC   0(TSDLENQ,RE),SAVETSAR                                           
         XC    SAVETSAR,SAVETSAR                                                
         TM    OVRSTAT,OVRGDONE    GRIDS PROCESSING FINISHED?                   
         BO    MAINXGX                                                          
*                                                                               
         TM    DISPFLAG,NORECQ     NO TRANSACTIONS ON ACCOUNT?                  
         BO    MAINX                                                            
*                                                                               
         TM    DISPFLAG,DISIOMAX   RE-ENTER DUE TO MAX IO?                      
         BO    *+14                                                             
         MVC   DISLINE,DISSTART    SET LAST SCREEN LINE USED                    
         B     *+8                                                              
         NI    DISPFLAG,X'FF'-DISIOMAX                                          
         GOTO1 ASCRNCLR,DISLINE                                                 
*                                                                               
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN30              NO                                           
*                                                                               
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN20              YES                                          
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN20                                                           
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         CLI   IOERR,IOMAX                                                      
         BE    MAINX                                                            
         DC    H'0'                                                             
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         OC    OSUMM1-OPTVALSD(L'OSUMM1,RF),OSUMM1-OPTVALSD(RF)                 
         BNZ   MAIN20                                                           
         L     RE,ATSARREC                                                      
         CLI   TSARFMT-TSARRECD(RE),TSSRCITM SOURCE TOTAL?                      
         BNE   MAIN20                                                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    MAIN20                                                           
*                                                                               
         SR    RF,RF               BLANK LINE IF NOT SUMMARY OPTION             
         IC    RF,DISLINE                                                       
         LA    RF,1(RF)                                                         
         STC   RF,DISLINE                                                       
         B     MAIN20                                                           
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BZ    MAIN50              YES                                          
         LA    RF,BASKEYH                                                       
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAIN250             YES                                          
*                                                                               
MAIN60   BAS   RE,FILTKEY          APPLY KEY FILTERING                          
         BNE   MAIN220             RECORD REJECTED?                             
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET TRANSACTION RECORD                    
         BE    MAIN70                                                           
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         CLI   IOERR,IOMAX                                                      
         BE    MAINX                                                            
         DC    H'0'                                                             
*                                                                               
MAIN70   L     R3,AIO1                                                          
         USING TRNRECD,R3                                                       
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        CHECK TRANSACTION ELEMENT FOUND              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TRNSTAT,TRNSDR      BILL TRANSACTION (DEBIT)?                    
         BO    MAIN80                                                           
         CLC   SAVESRCE,TRNKULC    DIFFERENT BILL SOURCE?                       
         BNE   MAIN80                                                           
         CLC   SAVEBILL,TRNKREF    DIFFERENT BILL NUMBER?                       
         BNE   MAIN80                                                           
         CLC   SAVEDATE,TRNKDATE   DIFFERENT BILL DATE?                         
         BE    MAIN81                                                           
*                                                                               
MAIN80   NI    INTFLAG,X'FF'-NOTFSTQ SWITCH 'NOT 1ST TRAN FOR BILL' OFF         
*                                                                               
MAIN81   BAS   RE,FILTER           APPLY FILTERING TO TRANS RECORD              
         BE    MAIN83              GOOD RECORD                                  
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    ODUE,ODUE           OR FILTERING ON PAYMENT REFERENCE?           
         BZ    MAIN220                                                          
         DROP  RF                                                               
         TM    TRNSTAT,TRNSDR      BILL TRANSACTION (DEBIT)?                    
         BZ    MAIN220                                                          
         OI    INTFLAG,NOTFSTQ                                                  
         MVC   SAVEBILL,TRNKREF    SAVE BILL NUMBER                             
         MVC   SAVEDATE,TRNKDATE   SAVE BILL DATE                               
         MVC   SAVESRCE,TRNKULC    SAVE BILL SOURCE                             
         B     MAIN220             RECORD REJECTED                              
MAIN83   TM    CORDFLAG,CORDMODQ   IF CONTRA ORDERED MODE SECURITY              
         BO    MAIN85              ALREADY APPLIED                              
         GOTO1 AOFFTRN             APPLY SECURITY CHECK                         
         BNE   MAIN220                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
MAIN85   CLC   SAVESRCE,TRNKULC    DIFFERENT BILL SOURCE?                       
         BNE   MAIN90                                                           
         CLC   SAVEBILL,TRNKREF    DIFFERENT BILL NUMBER?                       
         BNE   MAIN90                                                           
         CLC   SAVEDATE,TRNKDATE   DIFFERENT BILL DATE?                         
         BNE   MAIN90                                                           
         TM    TRNSTAT,TRNSDR      DEBIT?                                       
         BZ    MAIN180                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OREVERSE-OPTVALSD(RF),C'O' SHOWING REVERSALS?                    
         BNE   MAIN180                                                          
*                                                                               
MAIN90   CP    OUTAMNT,=P'0'       ANY OUTSTANDING AMOUNT UNPAID?               
         BE    MAIN160                                                          
*                                                                               
MAIN100  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CP    OUTAMNT,=P'0'                                                    
         BNL   MAIN110                                                          
         CLI   ONEGATIV,C'N'       DO WE ONLY WANT +VE NUMBERS/ZERO?            
         BE    MAIN120                                                          
         B     MAIN130                                                          
MAIN110  CLI   ONEGATIV,C'O'       DO WE ONLY WANT -VE NUMBERS?                 
         BNE   MAIN130                                                          
         DROP  RF                                                               
MAIN120  ZAP   OUTAMNT,=P'0'       CLEAR OUTSTANDING AMOUNT                     
         TM    DISPFLAG,ALLREADQ   ALL READ?                                    
         BO    MAIN240                                                          
         B     MAIN160                                                          
*                                                                               
MAIN130  BAS   RE,ADDSRCE          DEAL WITH TOTAL AMOUNTS                      
         TM    DISPFLAG,ALLREADQ   ALL READ?                                    
         BO    MAIN136                                                          
         L     RF,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L' SHOW LOCAL CURRENCY?                    
         BNE   MAIN136                                                          
         CLC   CURRLAST,SPACES     LAST CURRENCY SET?                           
         BNH   MAIN135                                                          
         CLC   SAVESRCE,TRNKULC    DIFFERENT BILL SOURCE?                       
         BNE   MAIN135                                                          
         CLC   CURRLAST,TRANCURR                                                
         BE    MAIN136                                                          
         OI    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTAL LINE                  
         TM    AGEFLAG,AGSRCTQ     FIRST BILL FOR SOURCE?                       
         BZ    MAIN135                                                          
         OI    CURRFLAG,CURRSSTQ   SUPPRESS SOURCE TOTAL LINE                   
MAIN135  MVC   CURRLAST,TRANCURR                                                
MAIN136  BAS   RE,ADDTSDAT         DEAL WITH TSAR RECORD AMOUNTS                
         ZAP   OUTAMNT,=P'0'                                                    
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OSUMM1,OSUMM1       NO DETAIL IF SUMMARY OPTION                  
         BNZ   MAIN160                                                          
         DROP  RF                                                               
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN140                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN160                                                          
*                                                                               
MAIN140  GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAIN150             SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         B     *+8                                                              
MAIN150  OI    INTFLAG,SCRFULLQ                                                 
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN160  NI    INTFLAG,X'FF'-PAIDQ SWITCH PARTLY PAID FLAG OFF                  
         MVC   SAVEBILL,TRNKREF    SAVE BILL NUMBER                             
         MVC   SAVEDATE,TRNKDATE   SAVE BILL DATE                               
         CLC   SAVESRCE,SPACES                                                  
         BNH   MAIN170                                                          
         CLC   SAVESRCE,TRNKULC    HAVE WE GOT NEW SOURCE?                      
         BE    *+14                                                             
         BAS   RE,SRCETOTL         DEAL WITH SOURCE TOTALS                      
MAIN170  MVC   SAVESRCE,TRNKULC    SAVE NEW SOURCE                              
         TM    DISPFLAG,ALLREADQ   ALL TRANSACTIONS READ?                       
         BO    MAIN250                                                          
         TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BO    MAINX                                                            
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    MAINX                                                            
*                                                                               
MAIN180  TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    *+12                                                             
         BAS   RE,NEWTSDAT         BUILD TSAR RECORD FOR DATA LINE(S)           
         OI    INTFLAG,NOTFSTQ                                                  
         LA    RE,TRNAMNT                                                       
         L     RF,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L' SHOW LOCAL CURRENCY?                    
         BNE   MAIN200                                                          
*                                                                               
         LA    R1,TRNRFST          SEARCH FOR FOREIGN CURRENCY ELEMENT          
         USING AFCELD,R1                                                        
MAIN190  SR    R0,R0                                                            
         IC    R0,AFCLN                                                         
         AR    R1,R0                                                            
         CLI   AFCEL,EOR           END OF RECORD                                
         BE    MAIN200                                                          
         CLI   AFCEL,AFCELQ        ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BNE   MAIN190                                                          
         TM    AFCXSTAT,AFCXSMEM   MEMO POSTING?                                
         BO    MAIN190                                                          
         LA    RE,AFCAMNT          RE=A(FOREIGN CURRENCY AMOUNT)                
         DROP  R1                                                               
*                                                                               
*----------------------------------------------------------------------         
* HANDLE THE AMOUNTS                                                            
*----------------------------------------------------------------------         
MAIN200  TM    TRNSTAT,TRNSDR            DEBIT?                                 
         BZ    MAIN210                                                          
         TM    AGEFLAG,AGPAYLQ           (YES) PAYABLE LEDGER                   
         BO    MAIN212                                                          
MAIN208  AP    OUTAMNT,0(L'TRNAMNT,RE)   OUTSTANDING AMOUNT                     
         B     MAIN220                                                          
*                                                                               
MAIN210  TM    AGEFLAG,AGPAYLQ           (YES) PAYABLE LEDGER                   
         BO    MAIN208                                                          
MAIN212  SP    OUTAMNT,0(L'TRNAMNT,RE)   SUBTRACT ANY PAYMENT                   
         TM    AGEFLAG,AGPAYLQ           (YES) PAYABLE LEDGER                   
         BO    *+8                                                              
         OI    INTFLAG,PAIDQ             PARTLY PAID FLAG ON                    
         B     MAIN220                                                          
*----------------------------------------------------------------------         
*                                                                               
MAIN220  GOTO1 ANXTREC,NXTRMODE    RETURN NEXT RECORD                           
*        LA    R3,IOKEY            R3=A(RECORD KEY)                             
*        MVC   KEYSAVE,TRNKEY      SAVE THE KEY FOR SEQ RESTORE                 
*                                                                               
         BE    MAIN230                                                          
         CLI   IOERR,IOMAX         MAX IOS REACHED?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ATSARREC                                                      
         MVC   SAVETSAR,0(RE)                                                   
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN230  DS    0H                                                               
         LA    R3,IOKEY            R3=A(RECORD KEY)                             
         MVC   KEYSAVE,TRNKEY      SAVE THE KEY FOR SEQ RESTORE                 
         CLC   TRNKCULA,ACNTKEY    RECORD BELONGS TO CORRECT ACCOUNT?           
         BE    MAIN60                                                           
         OI    DISPFLAG,ALLREADQ                                                
         CP    OUTAMNT,=P'0'       ANY TSAR RECS STILL TO BE BUILT?             
         BNE   MAIN100                                                          
MAIN240  BAS   RE,SRCETOTL         DEAL WITH SOURCE TOTAL LINE                  
         B     MAIN250                                                          
*                                                                               
MAIN250  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   MAIN260                                                          
         LA    R0,3                                                             
         LA    RF,ACCBILL1                                                      
MAIN255  CP    0(L'ACCBILL1,RF),=P'0' DO WE HAVE AN ACCOUNT TOTAL?              
         BNE   MAIN260                                                          
         LA    RF,L'ACCBILL1(RF)                                                
         BCT   R0,MAIN255                                                       
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
MAIN260  TM    DISPFLAG,TOTDONEQ                                                
         BO    MAINX                                                            
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         OI    DISPFLAG,TOTDONEQ                                                
         OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   MAINX                                                            
         MVC   FVMSGNO,=AL2(AE$MCURR) MULTIPLE CURRENCY-SUMM NOT POSS           
*                                                                               
         B     ERRXIT                                                           
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),0                                 
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
         LA    R2,BASOPTH          R2=A(OPTION SCREEN HEADER)                   
         MVC   FVMSGNO,=AL2(EAAGEMO)                                            
*                                                                               
FSTD02   L     RE,AOPTVALS         RE=A(OPTION VALUES)                          
         USING OPTVALSD,RE                                                      
         CLI   OINCTYPE,C'N'       IF NUMERIC INCREMENT THEN                    
         BNE   *+12                                                             
         CLI   OAGEBY,C'M'         CANNOT AGE BY MOS                            
         BE    FSTDERR                                                          
         DROP  RE                                                               
*                                                                               
*----------------------------------------------------------------------         
* INITIALIZE VARIABLES                                                          
*----------------------------------------------------------------------         
         MVI   NXTRMODE,0          ALL RECORDS REQUIRED                         
         MVI   CURRFLAG,0          INIT CURENCY FLAG                            
         MVC   CURRLAST,SPACES     LAST CURRENCY CODE                           
         MVC   UNILDG,SPACES                                                    
         XC    AGETAB(L'AGETAB*3),AGETAB BILL AGEING TABLE                      
         MVI   AGEFLAG,0           CLEAR GENERAL FLAG                           
         ZAP   OUTAMNT,=P'0'       OUTSTANDING BILL AMOUNT                      
         ZAP   SRCEBIL1,=P'0'      CLEAR SOURCE TOTALS                          
         MVC   SRCETOTS+L'SRCEBIL1(L'SRCETOTS-L'SRCEBIL1),SRCETOTS              
         ZAP   ACCBILL1,=P'0'      CLEAR ACCOUNT TOTALS                         
         MVC   ACCTOTS+L'ACCBILL1(L'ACCTOTS-L'ACCBILL1),ACCTOTS                 
         MVC   SAVEBILL,SPACES     SAVED BILL NUMBER                            
         MVC   SAVEDATE,SPACES     SAVED BILL DATE                              
         MVC   SAVESRCE,SPACES     SAVED SOURCE DESCRIPTION                     
         MVC   CONTRA,SPACES       CLEAR CONTRA CODE                            
         MVI   CONTLEN,0           INPUT CONTRA CODE LENGTH                     
         MVI   NEGCONT,0           NEGATIVE CONTRA FILTER                       
         MVC   SVCACN,SPACES       AND CONTRA NAME                              
*                                                                               
*----------------------------------------------------------------------         
* CHECK CONTRA ACCOUNT FILTER                                                   
*----------------------------------------------------------------------         
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         USING FLDHDRD,R2                                                       
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        ANYTHING INPUT?                              
         BZ    FSTD10                                                           
         LA    RE,FLDDATA                                                       
         CLI   0(RE),NEGFILTR      NEGATIVE FILTER?                             
         BNE   FSTD06                                                           
         MVI   NEGCONT,NEGFILTR                                                 
         LA    RE,1(RE)                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SH    RF,=H'1'                                                         
         BZ    FSTDERR             NO ACCOUNT ENTERED?                          
FSTD06   STC   RF,CONTLEN          LENGTH OF CONTRA CODE INPUT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA(0),0(RE)                                                  
         GOTO1 ACNAME              ATTEMPT TO FIND CONTRA NAME                  
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   UNILDG,BASKEY                                                    
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
*                                                                               
*----------------------------------------------------------------------         
* CHECK LEDGER TYPE                                                             
*----------------------------------------------------------------------         
         L     RF,ALDGTAB                                                       
         USING LEDGTABD,RF                                                      
FSTD12   CLI   LEDGTYPE,EOT        END OF TABLE?                                
         BE    FSTD18                                                           
         CLC   LEDGTYPE,UNILDG+1   MATCH ON LEDGER?                             
         BE    FSTD14                                                           
         LA    RF,LEDGLNQ(RF)                                                   
         B     FSTD12                                                           
FSTD14   TM    LEDGATTR,LEDGCRD    CREDITOR/PAYABLE LEDGER?                     
         BZ    FSTD18                                                           
         OI    AGEFLAG,AGPAYLQ     (YES) PAYABLE LEDGER                         
         DROP  RF                                                               
*----------------------------------------------------------------------         
*                                                                               
FSTD18   GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
*                                 CHANGE COLUMN HEADINGS                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         CLI   FLDILEN,0                 MAKE SURE SOMETHING'S THERE            
         BE    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   FLDILEN,L'ACTKULA   ENSURE LENGTH NOT TOO LONG                   
         BH    FSTDERR                                                          
         GOTO1 AUNITLDG,UNILDG     READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(EAWRNGLV)                                           
         LA    RF,LEDGTLVA         RF=A(HIERARCHY LEVELS)                       
         LR    RE,RF               RE=A(HIERARCHY LEVELS)                       
         SR    R1,R1               R1=MINIMUM LENGTH OF ACCOUNT                 
         CLI   0(RF),X'0C'         FULL HIERARCHY?                              
         BE    *+12                                                             
         LA    RF,1(RF)            NO BUMP RF                                   
         B     *-12                                                             
         CR    RF,RE               IS IT JUST ONE LEVEL?                        
         BE    *+10                                                             
         BCTR  RF,0                NO THEN GET MIN LENGTH                       
         IC    R1,0(RF)                                                         
         ZIC   RE,FLDILEN          RE=L'(INPUT)                                 
         SHI   RE,2                SUBTRACT 2 FOR UNIT AND LEDGER               
         CR    RE,R1               IS INPUT LONG ENOUGH?                        
         BNH   FSTDERR             NOPE, WRONG LEVEL ACCOUNT                    
*                                                                               
         LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKULA,BASKEY                                                   
         OC    ACTKULA,SPACES                                                   
         MVC   FVMSGNO,=AL2(EAIFNTFN)                                           
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    FSTD20                                                           
         TM    IOERR,IOERNF                                                     
         BO    FSTDERR             LOW LEVEL ACCOUNT NOT FOUND                  
         DC    H'0'                                                             
*                                                                               
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC)                                           
         GOTO1 AOFFACC                                                          
         BNE   FSTDERR                                                          
         MVC   ACNTKEY,IOKEY                                                    
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD24                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
         B     FSTD26                                                           
*                                                                               
FSTD24   GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
         GOTO1 ADISACC                                                          
*        LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC                                         
         LA    R1,FLDDATA+L'MX@ACC-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
*                                                                               
FSTD26   L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
FSTD30   CLI   0(RF),EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),NAMELQ        NAME ELEMENT?                                
         BE    FSTD40                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD30                                                           
*                                                                               
         USING NAMELD,RF                                                        
FSTD40   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD62                                                           
         SR    RE,RE               GET ACCOUNT NAME                             
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),NAMEREC                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         CLC   SVCACN,SPACES       HAVE WE SPECIFIED FILTER CONTRA?             
         BE    FSTD50                                                           
         LA    R1,4(RE,R1)                                                      
         MVI   0(R1),C'/'                                                       
         MVC   2(L'MX@CTR,R1),MX@CTR                                            
         LA    R1,L'MX@CTR+1(R1)                                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
         LA    R1,2(R1)                                                         
         LA    RE,FLDDATA+L'ENQDAT1-1                                           
         SR    RE,R1                                                            
         CH    RE,=Y(L'SVCACN-1)                                                
         BNH   *+8                                                              
         LA    RE,L'SVCACN-1                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVCACN    DISPLAY AS MUCH OF NAME AS POSSIBLE            
         DROP  RF                                                               
*                                                                               
FSTD50   LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         LR    RF,R2                                                            
*                                                                               
         MVC   FLDDATA(L'MX@ENH06),MX@ENH06                                     
         CLC   SR,BASKEY                    IF U/L SR SHOW SOURCE HEAD          
         BE    *+10                                                             
         MVC   FLDDATA(L'MX@ENH08),MX@ENH08                                     
*                                                                               
         L     RE,AOPTVALS         RE=A(OPTION VALUES)                          
         CLI   OSHREC-OPTVALSD(RE),C'D' SHOW DUE DATE?                          
         BNE   FSTD60                                                           
         USING SCRHEAD,RF                                                       
         MVC   SCRHBD,SPACES                                                    
         MVC   SCRHBILL,MX@BILL                                                 
         MVC   SCRHDUE,MX@DUE                                                   
FSTD60   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*                                                                               
         MVC   FLDDATA(L'MX@ENH07),MX@ENH07                                     
         CLC   SR,BASKEY                    IF U/L SR SHOW SOURCE HEAD          
         BE    *+10                                                             
         MVC   FLDDATA(L'MX@ENH09),MX@ENH09                                     
*                                                                               
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
FSTD62   BAS   RE,AGEGAP                                                        
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD64                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         B     FSTD66                                                           
*                                                                               
FSTD64   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         GOTO1 ASCRNCLR,DISLINE                                                 
*                                                                               
FSTD66   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         GOTO1 AREADUP             READ UP FRONT FOR 2 CHAR OFF                 
         BNE   FSTDERR                                                          
*                                                                               
         LA    R2,BASOPTH          R2=A(OPTION SCREEN HEADER)                   
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAAGEMO)                                            
         L     RE,AOPTVALS         RE=A(OPTION VALUES)                          
         USING OPTVALSD,RE                                                      
*                                                                               
         CLI   OINCTYPE,C'N'       IF NUMERIC INCREMENT THEN                    
         BNE   *+12                                                             
         CLI   OAGEBY,C'M'         CANNOT AGE BY MOS                            
         BE    FSTDERR                                                          
*                               CHECK OPTIONS NOT VALID UNLESS SR               
         CLC   SR,BASKEY                                                        
         BE    FSTDX                                                            
         MVC   FVMSGNO,=AL2(AE$OPTNV)                                           
*                                                                               
*        CLI   OAGEBY,C'D'         CANNOT AGE BY DUE                            
*        BE    FSTDERR                                                          
*        OC    ODUE,ODUE           DUE DATE                                     
*        BNZ   FSTDERR                                                          
         OC    OSHREC,OSHREC       SHOW (D)UE DATE                              
         BNZ   FSTDERR                                                          
         OC    OPAYDATE,OPAYDATE   PAYMENT DATE/CHECK DATE                      
         BNZ   FSTDERR                                                          
         OC    ODDATE,ODDATE       DEPOSIT DATE                                 
         BNZ   FSTDERR                                                          
         DROP  RE                                                               
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FILTER RECORD KEY                                            *         
* ON ENTRY IOKEY CONTAINS RECORD KEY                                  *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING TRNRECD,R3                                                       
FILTKEY  NTR1                                                                   
         LA    R3,IOKEY                                                         
*                                                                               
         TM    TRNKSTA2,TRNSPEEL   TRANSACTION PEELED?                          
         BO    FILTKRJX                                                         
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BE    FILTKRJX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        CONTRA FILTER?                               
         BZ    FILTK5                                                           
*                                                                               
         CLC   TRNKULC,SPACES                                                   
         BE    FILTKRJX                                                         
*                                                                               
         LA    R4,TRNKULC                                                       
FILTK00  CLI   0(R4),C' '                                                       
         BNE   *+12                                                             
         AHI   R4,1                                                             
         B     FILTK00                                                          
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),CONTRA  MATCHED ON CONTRA?                               
         BE    FILTK1                                                           
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER                              
         BE    FILTK5                                                           
         B     FILTKRJX                                                         
FILTK1   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER                              
         BNE   FILTK5                                                           
         B     FILTKRJX                                                         
*                                                                               
FILTK5   L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
         TM    TRNKSTAT,TRNSDRFT   DRAFT?                                       
         BZ    FILTK10                                                          
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK15                                                          
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK15                                                          
         B     FILTKRJX                                                         
FILTK10  CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
FILTK15  OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILTK16                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK16  OC    OBILNO,OBILNO       BILL NUMBER FILTER?                          
         BZ    FILTK20                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OBILLN1,OBILVL1),(OBILLN2,OBILVL2),C        
               OBILFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK20  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FILTER TRANSACTIONS                                          *         
* ON ENTRY R3=A(TRANSACTION RECORD)                                   *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
FILTER   NTR1                                                                   
*                                                                               
         USING TRNRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLC   TRNKEY(TRNKEND-L'TRNKSBR),IOKEYSAV                               
         BE    *+12                                                             
         NI    INTFLAG,X'FF'-REJECTQ                                            
         B     *+12                                                             
         TM    INTFLAG,REJECTQ                                                  
         BO    FILTREJX                                                         
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT ON RECORD)                
         USING TRNELD,R4                                                        
*                                                                               
         MVI   OTHFLAG,0           INITIALISE OTHELD FLAG                       
         MVI   CHQFLAG,0           INITIALISE CHEQUE ELEMENT FLAG               
         MVI   DUEFLAG,0           INITIALISE DUE DATE ELEMENT FLAG             
         XC    AINVELD,AINVELD     INIT ADDRESS                                 
*                                                                               
FILT10   TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    FILT40                                                           
         OC    OTYPE,OTYPE         INPUT TYPE FILTER?                           
         BZ    FILT30                                                           
         SR    RF,RF                                                            
         IC    RF,OTYPLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,OTYPEVAL(0)                                                  
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         IC    RE,TRNTYPE                                                       
         CR    RE,RF                                                            
         BE    FILT20                                                           
         CLI   OTYPEFI,NEGFILTR                                                 
         BNE   FILTREJX                                                         
         B     FILT30                                                           
FILT20   CLI   OTYPEFI,NEGFILTR                                                 
         BE    FILTREJX                                                         
*                                                                               
FILT30   OC    ODUE,ODUE           OR FILTERING ON PAYMENT REFERENCE?           
         BZ    *+8                                                              
         OI    DUEFLAG,DUERQDQ     CHEQUE ELEMENT REQUIRED FLAG ON              
FILT40   OC    ODDATE,ODDATE       FILTERING ON DEPOSIT DATE?                   
         BNZ   FILT50                                                           
         OC    OPAYDATE,OPAYDATE   FILTERING ON PAYMENT DATE?                   
         BNZ   FILT50                                                           
         OC    OPAYREF,OPAYREF     OR FILTERING ON PAYMENT REFERENCE?           
         BZ    *+8                                                              
FILT50   OI    CHQFLAG,CHQRQDQ     CHEQUE ELEMENT REQUIRED FLAG ON              
         OC    OSUBREF,OSUBREF     FILTERING ON SUB REF?                        
         BZ    *+8                                                              
         OI    OTHFLAG,OTHRQDQ     OTHERS ELEMENT REQUIRED FLAG ON              
         TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    *+10                                                             
         MVC   FILTCURR,COMPCURR   SET TRANSACTION CURRENCY FROM AGENCY         
FILT60   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   0(R4),EOR           END OF RECORD?                               
         BE    FILT210                                                          
         CLI   0(R4),OTHELQ        OTHERS ELEMENT?                              
         BE    FILT110                                                          
         CLI   0(R4),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    FILT70                                                           
         CLI   0(R4),DUEELQ        DUE DATE ELEMENT?                            
         BE    FILT120                                                          
         CLI   0(R4),AFCELQ        ACOUNT FOREIGN CURRENCY ELEMENT?             
         BE    FILT130                                                          
         CLI   0(R4),RALELQ        RECEIVABLE ALLOCATION ELEMENT?               
         BE    FILT140                                                          
         CLI   0(R4),FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    FILT180                                                          
         CLI   0(R4),XPYELQ        EXTRA PAYMENT ELEMENT FOR INVOICE            
         BE    FILT200                                                          
         B     FILT60                                                           
*                                                                               
         USING TRSELD,R4           TRANSACTION STATUS ELEMENT                   
FILT70   OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILT80                                                           
         GOTO1 ADCOMP,DMCB,(L'TRSPMOS,TRSPMOS),OMOSST,OMOSEN,OMOSFI             
         BNE   FILTREJX                                                         
FILT80   TM    TRNRSTAT,TRNSREVS   REVERSAL?                                    
         BZ    FILT90                                                           
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT100                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT100                                                          
         OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS?                
         BZ    FILTRJX2                                                         
         OC    TRSRMOS,TRSRMOS     IF NO REV MOS ASSUME SAME AS TRAN            
         BZ    FILTRJX2                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTRJX2                                                         
         B     FILT100                                                          
FILT90   CLI   OREVERSE,C'O'                                                    
         BE    FILTRJX2                                                         
*                                                                               
FILT100  TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    FILT60                                                           
         OC    OACT,OACT          FILTERING ON DATE ADDED?                      
         BZ    FILT60                                                           
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTREJX                                                         
         B     FILT60                                                           
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4           OTHERS ELEMENT                               
FILT110  OI    OTHFLAG,OTHELEQ     OTHERS ELEMENT FOUND                         
         TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    FILT60                                                           
         GOTO1 ASCOMP,DMCB,OTHNUM,(OSUBRLN1,OSUBRVL1),(OSUBRLN2,OSUBRVLC        
               2),OSUBRFI                                                       
         BNE   FILTREJX                                                         
         B     FILT60                                                           
         DROP  R4                                                               
*                                                                               
         USING DUEELD,R4           DUE DATE ELEMENT                             
FILT120  OC    ODUE,ODUE           FILTERING ON TRANSACTION DUE DATE?           
         BZ    FILT60                                                           
*        TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
*        BO    FILT60                                                           
         OC    DUEDATE,DUEDATE     EMPTY DUE DATE?                              
         BZ    FILT60                                                           
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,TEMPDAT) DUE DATE                    
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),ODUEST,ODUEEN,ODUEFI             
         BNE   FILTREJX                                                         
         OI    DUEFLAG,DUEELEQ     HAS DUE DATE ELEMENT BEEN FOUND?             
         B     FILT60                                                           
         DROP  R4                                                               
*                                                                               
         USING AFCELD,R4           ACCOUNT FOREIGN CURRENCY ELEMENT             
FILT130  TM    INTFLAG,NOTFSTQ     FIRST TRANSACTION?                           
         BO    FILT60                                                           
         TM    AFCXSTAT,AFCXSMEM   MEMO?                                        
         BO    FILT60                                                           
         MVC   FILTCURR,AFCCURR    OVERRIDE TRANSACTION CURRENCY                
         B     FILT60                                                           
         DROP  R4                                                               
*                                                                               
         USING RALELD,R4                                                        
FILT140  CLI   RALTYPE,RALTALC                                                  
         BNE   FILT60                                                           
         OC    OPAYREF,OPAYREF     FILTERING ON PAYMENT REFERENCE?              
         BZ    FILT150                                                          
         GOTO1 ASCOMP,DMCB,RALAREF,(OPAYRLN1,OPAYRVL1),(OPAYRLN2,      X        
               OPAYRVL2),OPAYRFI                                                
         BNE   FILTREJX                                                         
*                                                                               
FILT150  OC    OPAYDATE,OPAYDATE   FILTERING ON PAYMENT DATE?                   
         BZ    FILT160                                                          
         GOTO1 ADCOMP,DMCB,(L'RALADAT,RALADAT),OPAYDST,OPAYDEN,OPAYDFI          
         BNE   FILTREJX                                                         
*                                                                               
FILT160  OC    ODDATE,ODDATE       FILTERING ON DEPOSIT DATE?                   
         BZ    FILT170                                                          
         GOTO1 ADCOMP,DMCB,(L'RALADEP,RALADEP),ODDATEST,ODDATEEN,      X        
               ODDATEFI                                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT170  OI    CHQFLAG,CHQELEQ     MATCHING CHQUE ELEMENT FOUND                 
         B     FILT60                                                           
         DROP  R4                                                               
*                                                                               
         USING FFTELD,R4           FREE FORM TEXT ELEMENT                       
FILT180  CLI   FFTTYPE,FFTTACUR    ASSOCIATED CURRENCY?                         
*        BNE   FILT60                                                           
         BNE   FILT190                                                          
         CLI   OVALUE,C'L'         DIFFO POSTING CARRIES AGY CURR ONLY          
         BE    FILTREJX                                                         
         MVC   FILTCURR,FFTDATA    OVERRIDE TRANSACTION CURRENCY                
         B     FILT60                                                           
*                                                                               
FILT190  CLI   FFTTYPE,FFTTINVN    IS THIS INVOICE TYPE FREE FORM               
         BNE   FILT60                                                           
FILT200  ST    R4,AINVELD          INVOICE NUMBER IN THIS ELEMENT               
         B     FILT60                                                           
*-----------------------------------                                            
* INVOICE NUMBER                                                                
*-----------------------------------                                            
FILT210  OC    OSERIAL,OSERIAL     CURRENCY CODE FILTER?                        
         BZ    FILT250                                                          
         XC    WORK,WORK                                                        
         XC    TEMP,TEMP                                                        
*        -----------------------------------                                    
*        PRODUCTION LONG INVOICE NUMBER                                         
*        -----------------------------------                                    
         USING FFTELD,R4                                                        
         ICM   R4,15,AINVELD        RE=A(FREE FORM TEXT ELEMENT)                
         BZ    FILT230                                                          
         CLI   FFTEL,FFTELQ         IS IT FREE FORM ELEM?                       
         BNE   FILT220              AROCESS THEN MUST                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FFTDLEN         DATA LENGTH                                 
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),FFTDATA                                                  
         LA    RF,WORK                                                          
         LA    R1,TEMP                                                          
*                                                                               
FILT215  CLI   0(R1),0              END OF DATA?                                
         BE    FILT240                                                          
         CLI   0(R1),C'-'           LOOK FOR DASHES                             
         BNE   *+12                                                             
         AHI   R1,1                 SKIP THIS CHARACTER                         
         B     FILT215                                                          
         MVC   0(1,RF),0(R1)        MOVE INTO WORK                              
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         B     FILT215                                                          
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
FILT220  MVC   WORK(L'XPYINV),XPYINV                                            
         B     *+10                                                             
FILT230  MVC   WORK(L'TRNKREF),TRNKREF                                          
FILT240  GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   +        
               OSERFI                                                           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
FILT250  OC    OCURRYVL,OCURRYVL   CURRENCY CODE FILTER?                        
         BZ    FILT270                                                          
         CLC   FILTCURR,OCURRYVL   MATCH ON CURRENCY CODE?                      
         BNE   FILT260                                                          
         CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BE    FILTREJX                                                         
         B     FILT270                                                          
FILT260  CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BNE   FILTREJX                                                         
*                                                                               
FILT270  TM    OTHFLAG,OTHRQDQ     IS OTHERS ELEMENT REQUIRED?                  
         BZ    FILT280                                                          
         TM    OTHFLAG,OTHELEQ     HAS OTHERS ELEMENT BEEN FOUND?               
         BZ    FILTREJX                                                         
*                                                                               
FILT280  LA    RF,TRNRFST          RF=A(TRANSACTION ELEMENT)                    
         TM    TRNSTAT-TRNELD(RF),TRNSDR      DEBIT?                            
         BO    FILT290                                                          
         TM    CHQFLAG,CHQRQDQ     IS CHEQUE ELEMENT REQUIRED?                  
         BZ    FILT290                                                          
         TM    CHQFLAG,CHQELEQ     HAS CHEQUE ELEMENT BEEN FOUND?               
         BZ    FILTREJX                                                         
*                                                                               
FILT290  TM    DUEFLAG,DUERQDQ     IS DUE DATE ELEMENT REQUIRED?                
         BZ    FILTX                                                            
         TM    DUEFLAG,DUEELEQ     HAS DUE DATE ELEMENT BEEN FOUND?             
         BO    FILTX                                                            
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODUEST,ODUEEN,ODUEFI           
         BNE   FILTREJX                                                         
*                                                                               
FILTX    CR    RB,RB                                                            
         B     XIT                                                              
FILTREJX TM    INTFLAG,NOTFSTQ                                                  
         BO    *+8                                                              
         OI    INTFLAG,REJECTQ                                                  
FILTRJX2 LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        NEW TSAR RECORD FOR SCREEN DATA ITEM,                        *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
NEWTSDAT NTR1                                                                   
         USING TRNRECD,R3                                                       
*                                                                               
         MVC   BILLDATE,TRNKDATE   BILL DATE                                    
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OSUMM1,OSUMM1       SUMMARY OPTION?                              
         BNZ   NEWTX                                                            
         DROP  RF                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         XC    TSDACDAT,TSDACDAT   ACTIVITY DATE                                
         XC    TSDUSDAT,TSDUSDAT   USED DATE                                    
         XC    TSDPEDAT,TSDPEDAT   PEELED DATE                                  
         XC    TSDDUDAT,TSDDUDAT   DUE DATE                                     
         XC    TSDINV#,TSDINV#     INVOICE NUMBER                               
         XC    BILLDUE,BILLDUE     PACKED DUE DATE FOR AGEING                   
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
*                                                                               
         MVC   TSDCONT,SPACES                                                   
         LA    RF,L'TRNKULC        SKIP PAST SPACES, AND THEN MOVE IN           
         LA    R1,TRNKULC                                                       
         CLI   0(R1),C' '                                                       
         BH    NEWT05                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         B     NEWTX                                                            
NEWT05   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSDCONT(0),0(R1)    CONTRA CODE                                  
*                                                                               
         MVC   TSDTREF,TRNKREF     TRANSACTION REFERENCE NUMBER                 
         MVC   TSDTDAT,TRNKDATE    TRANSACTION DATE                             
         MVC   TSDTRST,TRNRSTAT    TRANSACTION RECORD STATUS                    
         MVC   TSDTSBR,TRNKSBR     TRANSACTION SUB-REFERENCE NUMBER             
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT OF TRANS RECORD)          
         LA    R4,TSDRFST          R4=A(FIRST ELEMENT FOR TSAR RECORD)          
         USING TRNELD,R3                                                        
         MVC   TSDTEST,TRNSTAT     TRANSACTION ELEMENT STATUS                   
         MVC   TSDTTYPE,TRNTYPE    INPUT TYPE                                   
*                                                                               
NEWT10   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
NEWT20   CLI   0(R3),EOR           END OF RECORD?                               
         BE    NEWT80                                                           
         CLI   0(R3),OTHELQ        OTHERS ELEMENT?                              
         BE    NEWT30                                                           
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMENT?                
         BE    NEWT40                                                           
         CLI   0(R3),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    NEWT50                                                           
         CLI   0(R3),DUEELQ        DUE DATE ELEMENT?                            
         BE    NEWT55                                                           
         CLI   0(R3),AFCELQ        ACCOUNT FORIGN CURRENCY ELEMENT?             
         BE    NEWT57                                                           
         CLI   0(R3),APEELQ        ANALYSIS POINTER ELEMENT?                    
         BE    NEWT60                                                           
         CLI   0(R3),FFTELQ        FREE FORM ELEMENT FOR INVOICE NUMBER         
         BE    NEWT65                                                           
         CLI   0(R3),XPYELQ        X'46' EXTRA PAYMENT ELEMENT                  
         BE    NEWT67                                                           
         B     NEWT10                                                           
*                                                                               
         USING OTHELD,R3           OTHERS ELEMENT                               
NEWT30   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OXDETAIL,C'Y'       XTRA DETAIL REQUIRED?                        
         BNE   NEWT10                                                           
         DROP  RF                                                               
         SR    RF,RF                                                            
         IC    RF,OTHLN                                                         
         B     NEWT70                                                           
*                                                                               
         USING SCIELD,R3           ANALYSIS POINTER ELEMENT                     
NEWT40   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OXDETAIL,C'Y'       XTRA DETAIL OPTION?                          
         BNE   NEWT10                                                           
         DROP  RF                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         B     NEWT70                                                           
*                                                                               
         USING TRSELD,R3           TRANSACTION STATUS ELEMENT                   
NEWT50   MVC   TSDACDAT,TRSDATE    ACTIVITY DATE                                
         MVC   TSDUSDAT,TRSUDAT    USED DATE                                    
         MVC   TSDPEDAT,TRSPDAT    PEELED DATE                                  
         MVC   BILLMOS,TRSPMOS     MOS                                          
         B     NEWT10                                                           
*                                                                               
         USING DUEELD,R3           DUE DATE ELEMENT                             
NEWT55   MVC   TSDDUDAT,DUEDATE    DUE DATE                                     
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,BILLDUE) DUE DATE                    
         B     NEWT10                                                           
*                                                                               
         USING AFCELD,R3           ACCOUNT FOREIGN CURRENCY ELEMENT             
NEWT57   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OVALUE,0            SHOW FOREIGN CURRENCY VALUE?                 
         BNE   *+12                                                             
         CLI   OXDETAIL,C'Y'       SHOW XTRA DETAIL?                            
         BNE   NEWT10                                                           
         DROP  RF                                                               
         SR    RF,RF                                                            
         IC    RF,AFCLN                                                         
         B     NEWT70                                                           
*                                                                               
         USING APEELD,R3           ANALYSIS POINTER ELEMENT                     
NEWT60   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OATTRIB-OPTVALSD(RF),C'Y' ATTRIBUTE OPTION?                      
         BNE   NEWT10                                                           
         SR    RF,RF                                                            
         IC    RF,APELN                                                         
         B     NEWT70                                                           
*                                                                               
         USING FFTELD,R3                                                        
NEWT65   DS    0H                                                               
         CLI   FFTTYPE,FFTTINVN                                                 
         BNE   NEWT10                                                           
FILT750  SR    RF,RF                DATA LENGTH                                 
         ICM   RF,1,FFTDLEN         DATA LENGTH                                 
         BNP   NEWT10                                                           
*                                                                               
         LHI   RE,L'TSDINV#         GET LENGTH OF THE FIELD                     
         CR    RE,RF                                                            
         BNL   *+6                                                              
         LR    RF,RE                USE MAX LENGTH OF FIELD                     
*                                                                               
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSDINV#(0),FFTDATA                                               
         B     NEWT10                                                           
*                                                                               
         USING XPYELD,R3                                                        
NEWT67   DS    0H                                                               
         MVC   TSDINV#(L'XPYINV),XPYINV                                         
         B     NEWT10                                                           
*                                                                               
NEWT70   BCTR  RF,0                MOVE WHOLE OF ELEMENT ONTO TSAR REC          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     NEWT10                                                           
         DROP  R3                                                               
*                                                                               
NEWT80   MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         LA    R4,1(R4)                                                         
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4               RF=L'(TSAR RECORD)                           
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
NEWTX    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD REST OF SCREEN LINE DATA TO TSAR RECORD                  *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
ADDTSDAT NTR1                                                                   
         L     R3,AOPTVALS         R3=A(OPTION VALUES)                          
         USING OPTVALSD,R3                                                      
         OC    OSUMM1,OSUMM1       SUMMARY OPTION?                              
         BNZ   ADDT50                                                           
*                                                                               
         XC    AGEDATE,AGEDATE                                                  
         CLI   OAGEBY,C'M'         MONTHLY AGEING?                              
         BNE   *+14                                                             
         MVC   AGEDATE(L'BILLMOS),BILLMOS USE BILL MOS                          
         B     ADDT05                                                           
         CLI   OAGEBY,C'D'         MONTHLY AGEING?                              
         BNE   ADDT02                                                           
         OC    BILLDUE,BILLDUE     IF NO DUE DATE USE BILLDATE                  
         BZ    ADDT02                                                           
         MVC   AGEDATE,BILLDUE     USE BILL DUE DATE                            
         B     ADDT05                                                           
ADDT02   MVC   AGEDATE,BILLDATE    ASSUME AGE BY BILL DATE                      
*                                                                               
ADDT05   L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         ZAP   TSDAMT1,=P'0'                                                    
         ZAP   TSDAMT2,=P'0'                                                    
         ZAP   TSDAMT3,=P'0'                                                    
         LA    R0,3                R0=(NUMBER OF AGEING COLUMNS)                
         LA    RF,AGETAB           RF=A(AGEING TABLE)                           
         LA    R4,TSDAMT1          R4=A(1ST AGEING AMOUNT IN TSAR REC)          
         USING AGETABD,RF                                                       
ADDT10   CLI   OAGEBY,C'M'         MOS AGEING?                                  
         BNE   ADDT20                                                           
         CLC   AGEDATE(L'BILLMOS),AGESTART MOS LESS THAN AGEIN START?           
         BNL   ADDT40                                                           
         B     ADDT30                                                           
ADDT20   LA    RE,L'AGEDATE        RE=L'(BILL/DUE DATE PACKED YYMMDD)           
         CLI   OINCTYPE,0          DEFAULT IS MONTH                             
         BE    *+12                                                             
         CLI   OINCTYPE,C'M'       IF INCREMENT BY MONTH REDUCE LENGTH          
         BNE   *+8                                                              
         LA    RE,L'AGEDATE-1                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   AGEDATE(0),AGESTART BILL DATE LESS THAN AGEING START?            
         BNL   ADDT40                                                           
ADDT30   LA    R4,L'TSDAMT1(R4)     BUMP TO NEXT AGEING PERIOD                  
         LA    RF,AGELNQ(RF)                                                    
         BCT   R0,ADDT10                                                        
         DC    H'0'                 SHOULD HAVE BEEN FILTERED OUT               
         DROP  RF                                                               
ADDT40   ZAP   0(L'TSDAMT1,R4),OUTAMNT OUTSTANDING BILL AMOUNT                  
         TM    INTFLAG,PAIDQ        HAS BILL BEEN PARTLY PAID?                  
         BZ    *+8                                                              
         MVI   TSDPAID,C'*'                                                     
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         MVC   TSDLINES,LINSUSED                                                
         MVC   TOTCURR,TRANCURR    SAVE TOTAL CURRENCY                          
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX                                                
         BZ    ADDTERRX                                                         
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
ADDT50   OI    AGEFLAG,AGSRCTQ     SWITCH ON SOURCE TOT REQUIRED                
*                                                                               
ADDTX    CR    RB,RB                                                            
         B     XIT                                                              
ADDTERRX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         MVC   TRANCURR,COMPCURR   AGENCY CURRENCY                              
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R4                                                      
         CLI   TSARFMT,TSSRCITM    WORKCODE TOTAL ITEM?                         
         BE    FORM80                                                           
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FORM120                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
*                                                                               
         LA    R3,TSDRFST          R3=A(FIRST ELEMENT ON TSAR RECORD)           
         XC    ELEADDS(ELEADLN),ELEADDS CLEAR SAVED ADDRESSESS                  
FORM1    CLI   0(R3),EOR           EOR?                                         
         BE    FORM3A                                                           
         CLI   0(R3),OTHELQ        OTHERS ELEMENT?                              
         BNE   *+12                                                             
         ST    R3,AOTHELD          SAVE ADDRESS OF ELEMENT                      
         B     FORM3                                                            
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMEN?                 
         BNE   FORM2                                                            
         OC    ASCIELD,ASCIELD                                                  
         BNZ   FORM2                                                            
         ST    R3,ASCIELD          SAVE ADDRESS OF ELEMENT                      
         B     FORM3                                                            
FORM2    CLI   0(R3),AFCELQ        ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BNE   *+12                                                             
         ST    R3,AAFCELD          SAVE ADDRESS OF ELEMENT                      
         B     FORM3                                                            
         CLI   0(R3),APEELQ        ANALYSIS POINTER ELEMENT?                    
         BNE   *+12                                                             
         ST    R3,AAPEELD          SAVE ADDRESS OF ELEMENT                      
         B     FORM3                                                            
*                                                                               
FORM3    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FORM1                                                            
*                                                                               
FORM3A   MVC   SCR1CONT,TSDCONT    BILLING SOURCE                               
         SR    RF,RF                                                            
         ICM   RF,15,AAFCELD       ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BZ    FORM3B                                                           
         TM    AFCXSTAT-AFCELD(RF),AFCXSMEM MEMO ITEM?                          
         BO    FORM3B                                                           
         MVC   TRANCURR,AFCCURR-AFCELD(RF) GET TRANSACTION CURRENCY             
FORM3B   L     RF,AOPTVALS         R2=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BNE   FORM04                                                           
         LA    R1,L'SCR1CONT-L'TRANCURR-2                                       
         LA    R3,SCR1CONT(R1)                                                  
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-12                                                          
         MVI   1(R3),C' '          ENSURE GAP BETWEEN SOURCE AND CURR           
         GOTO1 AGETCURR,DMCB,TRANCURR                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   2(L'CURTSYMB,R3),CURTCUR-CURTABD(RF)                             
*                                                                               
FORM04   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OSHREC-OPTVALSD(RF),C'D' SHOW DUE DATE?                          
         BE    FORM05                                                           
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(17,SCR1TDAT) BILL DATE                 
         B     FORM06                                                           
FORM05   OC    TSDDUDAT,TSDDUDAT                                                
         BZ    FORM06                                                           
         GOTO1 VDATCON,DMCB,(2,TSDDUDAT),(17,SCR1TDAT) DUE DATE                 
FORM06   MVC   SCR1TREF,TSDTREF    BILL NUMBER                                  
*                                                                               
         GOTO1 FORMAMT,DMCB,SCR1AMT1,TSDAMT1,TRANCURR                           
*                                                                               
         MVC   SCR1PAID,TSDPAID                                                 
*                                                                               
FORM70   MVI   LINSUSED,1          NOTHING IN DESCRIPTION FIELD?                
         LA    R2,L'DUMLIN1(R2)                                                 
         BAS   RE,ATTRLNS          ATTRIBUTE LINES                              
         BAS   RE,XDETLNS          EXTRA DETAIL LINES                           
         B     FORMX                                                            
*                                                                               
         USING SCRSRCED,R2         DSECT FOR SOURCE TOTAL LINE                  
FORM80   SR    RF,RF                                                            
         ICM   RF,3,TSARLEN-TSARRECD(R4) RF=L'(TSAR RECORD)                     
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARSRCD,R4                                                      
         CLI   TSSLINES,X'FF'                                                   
         BE    FORMX                                                            
         MVC   SCRSCONT,TSSCONT     BILLING SOURCE                              
*                                                                               
         MVC   SCRSSTOT,MX@SRCTS    SOURCE TOTALS                               
         CLC   SR,BASKEY                                                        
         BE    *+10                                                             
         MVC   SCRSSTOT,MX@CONTS    CONTRA TOTALS                               
*                                                                               
         CH    RF,=Y((TSARDATA-TSARRECD)+TSSLN1Q) SHORT TSAR?                   
         BE    FORM90                                                           
         GOTO1 FORMAMT,DMCB,SCRSAMT1,TSSAMT1,TSSCURR                            
FORM90   MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM120  LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@ACCTS                                                
         GOTO1 FORMAMT,DMCB,SCRTAMT1,TSTAMT1,TSTCURR                            
         MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
*                                                                               
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
*                                                                               
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE?                                  
         BE    FGRM30                                                           
         CLI   TSARFMT,TSSRCITM    SUB TOTAL LINE?                              
         BE    FGRM25                                                           
         CLI   TSARFMT,TSITEM1                                                  
         BNE   FGRMX                                                            
*                                                                               
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM25   GOTO1 ADISGRD,DMCB,('DWN2ND',AGCTBL),UNILDG                            
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT 3 AMOUNT COLUMNS                                      *         
* NTRY   0(R1) A(FIRST DISPLAY COLUMN)                                *         
*        4(R1) A(FIRST AMOUNT ACCUMULATOR)                            *         
*        12(R1) A(CURRENCY CODE)                                      *         
*        R2=A(SCREEN LINE)                                            *         
***********************************************************************         
FORMAMT  NTR1                                                                   
*                                                                               
         USING SCRLIN1D,R2                                                      
         LM    R3,R4,0(R1)         R3=A(1ST DISP COL),R4=A(1ST ACCUM)           
         L     RF,8(R1)                                                         
         MVC   DUB(L'AFCCURR),0(RF)                                             
         LA    R0,4                R0=(NUMBER OF AGEING COLUMNS)                
         MVC   WORK(L'TSDAMT1*3),0(R4)                                          
         LA    R4,WORK                                                          
         ZAP   L'TSDAMT1*3(L'TSDAMT1,R4),=P'0' CLEAR TOTAL                      
*                                                                               
FORMA10  CP    0(L'TSDAMT1,R4),=P'0' AMOUNT OUTSTANDING?                        
         BE    FORMA90                                                          
         CH    R0,=H'1'            TOTAL COLUMN                                 
         BE    *+10                                                             
         AP    WORK+L'TSDAMT1*3(L'TSDAMT1),0(L'TSDAMT1,R4) ADD TOTAL            
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BNE   FORMA30                                                          
         GOTO1 AGETCURR,DMCB,DUB                                                
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         PACK  TEMP(8),NINES(L'SCR1AMT1-2) GET STRING OF NINES                  
         CP    TEMP(8),0(L'TSDAMT1,R4) IS AMOUNT TOO HIGH TO FIT?               
         BL    FORMA20                                                          
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(L'TSDAMT1,R4) IS AMOUNT TO LOW TO FIT?                 
         BH    FORMA20                                                          
         CURED (P6,(R4)),(L'SCR1AMT1,(R3)),(RF),MINUS=YES                       
         B     FORMA90                                                          
FORMA20  CURED (P6,(R4)),(L'SCR1AMT1,(R3)),(RF),MINUS=YES,DECS=ROUND            
         B     FORMA90                                                          
*                                                                               
FORMA30  PACK  TEMP(8),NINES(L'SCR1AMT1-2) GET STRING OF NINES                  
         CP    TEMP(8),0(L'TSDAMT1,R4) IS AMOUNT TOO HIGH TO FIT?               
         BL    FORMA40                                                          
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(L'TSDAMT1,R4) IS AMOUNT TO LOW TO FIT?                 
         BH    FORMA40                                                          
         CURED (P6,(R4)),(L'SCR1AMT1,(R3)),2,MINUS=YES                          
         B     FORMA90                                                          
FORMA40  CURED (P6,(R4)),(L'SCR1AMT1,(R3)),2,MINUS=YES,DECS=ROUND               
         B     FORMA90                                                          
*                                                                               
FORMA90  LA    R3,SCR1AMT2-SCR1AMT1(R3)                                         
         LA    R4,L'TSDAMT1(R4)                                                 
         BCT   R0,FORMA10                                                       
*                                                                               
FORMAX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT ATTRIBUTE LINE(S)                                     *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         SPACE 1                                                                
ATTRLNS  NTR1                                                                   
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         ICM   R4,15,AAPEELD       ANALYSIS POINTER ELEMENT?                    
         BZ    ATTRLX                                                           
         USING APEELD,R4                                                        
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    RF,TEMP                                                          
         MVC   0(L'MX@ATTR,RF),MX@ATTR                                          
         LA    RF,L'MX@ATTR-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         SR    R0,R0                                                            
         IC    R0,APENUM           NUMBER OF SUB ELEMENTS                       
         LA    R4,APENTRY          R4=A(FIRST SUB ELEMENT)                      
         USING APENTRY,R4                                                       
ATTRL10  SR    RE,RE                                                            
         IC    RE,APENLEN          RE=L'(SUB ELEMENT)                           
         SH    RE,=Y(APELN2Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),APENACT     GET ACCOUNT CODE                             
         LA    RF,2(RE,RF)         BUMP RF TO NEXT POSITION                     
         SR    RE,RE                                                            
         IC    RE,APENLEN                                                       
         AR    R4,RE               BUMP R4 TO NEXT SUB ELEMENT                  
         BCT   R0,ATTRL10                                                       
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         BZ    ATTRLX                                                           
         GOTO1 VCHOPPER,DMCB,((RF),TEMP),(L'DUMLIN1,SCANBLK),2                  
         L     RE,DMCB+8           R4=(NUMBER OF ATTRIBUTE LINES)               
         LTR   RE,RE                                                            
         BZ    ATTRLX                                                           
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BH    ATTRLX                                                           
         STC   RF,LINSUSED                                                      
         LA    R4,SCANBLK                                                       
         MVC   0(L'DUMLIN1,R2),0(R4)                                            
         LA    R4,L'DUMLIN1(R4)                                                 
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
ATTRLX   XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT EXTRA DETAIL LINE(S)                                  *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         SPACE 1                                                                
XDETLNS  NTR1                                                                   
         STCM  R2,15,ANXTLINE      SAVE ADDRESS OF NEXT FREE LINE               
         USING TSARDATD,R4                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL REQUIRED?                 
         BNE   XDETLX                                                           
         TM    TSDTEST,TRNSDR                                                   
         BZ    XDETLX                                                           
         LA    R0,UNSCANBK         CLEAR UNSCAN BLOCK                           
         LH    R1,=Y(MAXUNSCN*UNSCNLNQ)                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,UNSCANBK                                                      
         USING UNSCAND,R3                                                       
         CLI   TSDTTYPE,0          INPUT TYPE?                                  
         BE    XDETL10                                                          
         MVC   UNSCNLHS(L'MX@TYPE),MX@TYPE                                      
         SR    RF,RF                                                            
         IC    RF,TSDTTYPE                                                      
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL10  CLI   TWAOFFC,C'*'        SHOW TRANSACTION ELEMENT STATUS?             
         BNE   XDETL20                                                          
         MVC   UNSCNLHS(L'MX@STT),MX@STT                                        
         GOTO1 VHEXOUT,DMCB,TSDTEST,UNSCNRHS,1,=C'MIX'                          
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL20  ICM   R2,15,AOTHELD                                                    
         BZ    XDETL100                                                         
         USING OTHELD,R2                                                        
         CLC   OTHNUM(L'OTHNUM+L'OTHPROF),SPACES                                
         BE    XDETL100                                                         
         MVC   UNSCNLHS(L'MX@OTHER),MX@OTHER                                    
         SR    R1,R1                                                            
         IC    R1,OTHLN            R1=L'(ELEMENT)                               
         SH    R1,=Y(L'OTHEL+L'OTHLN)                                           
         LA    R2,OTHNUM           R2=A( BEGINNING OF DATA)                     
         DROP  R2                                                               
         CLI   0(R2),C' '                                                       
         BE    XDETL80                                                          
XDETL30  SR    RF,RF               RF=LENGTH FOR EXECUTED MOVE                  
         LA    RE,1(R2)            RE=LOCATION IN ELEMENT                       
         BCT   R1,*+8                                                           
         B     XDETL70             END OF ELEMENT - WRITE OUT STRING            
XDETL40  CLI   0(RE),C'A'          SEARCH FOR END OF STRING OR DATE             
         BL    XDETL60                                                          
XDETL50  LA    RE,1(RE)            KEEP ON GOING                                
         LA    RF,1(RF)                                                         
         BCT   R1,XDETL40                                                       
         B     XDETL70             END OF ELEMENT                               
XDETL60  CLI   0(RE),X'80'                                                      
         BH    XDETL70             SHOULD BE A PACKED DATE - STOP               
         CLI   0(RE),C' '                                                       
         BH    XDETL50             SPECIAL CHARACTER - KEEP ON GOING            
XDETL70  EX    RF,*+8              AT END - MOVE TO UNSCAN BLOCK                
         B     *+10                                                             
         MVC   UNSCNRHS(0),0(R2)                                                
         LA    R3,UNSCNLNQ(R3)                                                  
         SR    RF,RF                                                            
         LTR   R1,R1                                                            
         BZ    XDETL100            END OF ELEMENT                               
         LR    R2,RE                                                            
XDETL80  CLI   0(R2),C' '          SCAN FOR NEXT FIELD (IF ANY)                 
         BNH   XDETL90             SEARCH FARTHER                               
         CLI   0(R2),C'A'          IS THIS A STRING                             
         BNL   XDETL30             YES                                          
         CLI   0(R2),X'80'         OR A SPECIAL CHARACTER                       
         BL    XDETL30             YES                                          
         B     XDETL100            NO - MUST BE A DATE                          
XDETL90  LA    R2,1(R2)                                                         
         BCT   R1,XDETL80                                                       
*                                                                               
XDETL100 ICM   R2,15,ASCIELD       DISPLAY DISCOUNT                             
         BZ    XDETL180                                                         
         USING SCIELD,R2                                                        
XDETL110 CLI   SCITYPE,SCITCDSC                                                 
         BNE   XDETL120                                                         
         MVC   UNSCNLHS(L'MX@DISS),MX@DISS                                      
         CURED (P6,SCIAMNT),(L'UNSCNRHS,UNSCNRHS),2,ALIGN=LEFT,        X        
               MINUS=YES                                                        
         LA    R3,UNSCNLNQ(R3)                                                  
         B     XDETL130                                                         
*                                                                               
XDETL120 SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,SCILN                                                         
         AR    R2,RF                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    XDETL110                                                         
*                                                                               
XDETL130 ICM   R2,15,ASCIELD       DISPLAY REST OF SUBSIDIARY ITEMS?            
         USING SCIELD,R2                                                        
XDETL140 CLI   SCITYPE,SCITCDSC    ALREADY DONE DISCOUNT                        
         BE    XDETL170                                                         
         MVC   UNSCNLHS(L'SCITYPE),SCITYPE                                      
*        CLI   SCITYPE,SCITMILE                                                 
*        BNE   XDETL150                                                         
XDETL150 CURED (P6,SCIAMNT),(L'UNSCNRHS,UNSCNRHS),2,ALIGN=LEFT,        X        
               MINUS=YES                                                        
XDETL160 SR    RF,RF                                                            
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL170 SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,SCILN                                                         
         AR    R2,RF                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    XDETL140                                                         
*                                                                               
XDETL180 OC    TSDACDAT,TSDACDAT   ACTIVITY DATE?                               
         BZ    XDETL190                                                         
         MVC   UNSCNLHS(L'MX@DATE),MX@DATE                                      
         GOTO1 VDATCON,DMCB,(2,TSDACDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL190 OC    TSDUSDAT,TSDUSDAT   USED DATE?                                   
         BZ    XDETL200                                                         
         MVC   UNSCNLHS(L'MX@UDAT),MX@UDAT                                      
         GOTO1 VDATCON,DMCB,(2,TSDUSDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL200 OC    TSDPEDAT,TSDPEDAT   PEELED DATE?                                 
         BZ    XDETL210                                                         
         MVC   UNSCNLHS(L'MX@PELDT),MX@PELDT                                    
         GOTO1 VDATCON,DMCB,(2,TSDPEDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL210 ICM   R2,15,AAFCELD       ACOUNT FOREIGN CURRENCY ELEMENT?             
         BZ    *+10                                                             
         L     RF,AXDFCURR                                                      
         BASR  RE,RF               GET FOREIGN CURRENCY INFORMATION             
*                                                                               
         MVC   UNSCNLHS(L'MX@SEQ),MX@SEQ TRANS SUB-REFERENCE NUMBER             
         SR    RF,RF                                                            
         IC    RF,TSDTSBR                                                       
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
         ICM   R2,15,ANXTLINE      R2=A(NEXT DUMMY SCREEN LINE)                 
         GOTO1 AXDETDIS            DISPLAY ON DUMMY SCREEN LINES                
*                                                                               
XDETLX   XIT1  REGS=(R2)                                                        
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE BILLING SOURCE/CONTRA TOTALS                   *         
***********************************************************************         
SRCETOTL NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         LA    R0,3                                                             
         LA    RF,SRCEBIL1                                                      
         LA    R1,ACCBILL1                                                      
SRCET10  AP    0(L'ACCBILL1,R1),0(L'SRCEBIL1,RF) GET ACCOUNT TOTALS             
         CLI   OSUMM1,C'C'         CLIENT SUMMARY OPTION?                       
         BNE   *+10                                                             
         ZAP   0(L'SRCEBIL1,RF),=P'0'                                           
         LA    R1,L'ACCBILL1(R1)                                                
         LA    RF,L'SRCEBIL1(RF)                                                
         BCT   R0,SRCET10                                                       
         CLI   OSUMM1,C'C'                                                      
         BE    SRCETX                                                           
*                                                                               
         TM    AGEFLAG,AGSRCTQ     SOURCE TOTALS REQUIRED                       
         BZ    SRCETX                                                           
         NI    AGEFLAG,X'FF'-AGSRCTQ SWITCH OFF SOURCE TOT REQUIRED             
*                                                                               
SRCETAA  L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSSLN2Q)                                                   
         TM    CURRFLAG,CURRSSTQ   SUPRESS SOURCE TOTAL LINE?                   
         BZ    *+8                                                              
         LH    RF,=Y(TSSLN1Q)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR SOURCE TOTAL DATA)                 
         USING TSARSRCD,R3                                                      
         MVI   TSSFMT,TSSRCITM      BILLING SOURCE TSAR ITEM                    
*                                                                               
         MVC   TSSCONT,SPACES                                                   
         LA    RF,L'SAVESRCE       SKIP PAST SPACES, AND THEN MOVE IN           
         LA    R1,SAVESRCE                                                      
         CLI   0(R1),C' '                                                       
         BH    SRCET12                                                          
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         B     SRCET13                                                          
SRCET12  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSSCONT(0),0(R1)    CONTRA CODE                                  
*                                                                               
SRCET13  MVC   TSSCURR,TOTCURR      SAVE SOURCE CURRENCY                        
*                                                                               
         TM    CURRFLAG,CURRSSTQ   SUPRESS SOURCE TOTAL LINE?                   
         BO    SRCET15                                                          
         LA    R0,3                 GET SOURCE TOTALS                           
         LA    RE,SRCEBIL1                                                      
         LA    RF,TSSAMT1                                                       
         ZAP   0(L'TSSAMT1,RF),0(L'SRCEBIL1,RE)                                 
         LA    RE,L'SRCEBIL1(RE)                                                
         LA    RF,L'TSSAMT1(RF)                                                 
         BCT   R0,*-14                                                          
*                                                                               
SRCET15  TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         DISPLAY ON DUMMY SCREEN LINES                
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
*                                                                               
         NI    CURRFLAG,X'FF'-CURRSSTQ                                          
         CLI   TSSLINES,X'FF'                                                   
         BE    *+10                                                             
         MVC   TSSLINES,LINSUSED   GET NUMBER OF LINES USED                     
         GOTO1 ATSARADD                                                         
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX   IF MAX IO IT HAS BEEN ADDED                  
         BZ    SRCETERX                                                         
*                                                                               
         LA    R0,3                CLEAR SOURCE TOTALS                          
         LA    RF,SRCEBIL1                                                      
         ZAP   0(L'SRCEBIL1,RF),=P'0'                                           
         LA    RF,L'SRCEBIL1(RF)                                                
         BCT   R0,*-10                                                          
         MVC   TSLSTREC,TSCURRNO                                                
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    *+12                                                             
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    SRCET20                                                          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     SRCET20                                                          
         MVC   TSLSTLIN,TSCURRNO                                                
         OC    OSUMM1,OSUMM1                                                    
         BNZ   SRCET20                                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    SRCET20                                                          
         SR    RF,RF               BLANK LINE IF NOT SUMMARY OPTION             
         IC    RF,DISLINE                                                       
         LA    RF,1(RF)                                                         
         STC   RF,DISLINE                                                       
*                                                                               
SRCET20  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    SRCETERX                                                         
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    SRCETERX                                                         
         TM    DISPFLAG,DISIOMAX   COULD HAVE BEEN SET ON TSAR ADD              
         BO    SRCETERX                                                         
         TM    DISPFLAG,ALLREADQ   IF ALL REC READ NO POINT RESETTING           
         BO    SRCETX                                                           
*                                                                               
SRCETX   CR    RB,RB                                                            
         B     XIT                                                              
SRCETERX LTR  RB,RB                                                             
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
*                                                                               
         TM    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTAL LINE                  
         BO    TOTX                                                             
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLNQ)                                                    
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR ACCOUNT TOTAL DATA)                
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     ACCOUNT TOTAL ITEM                           
         MVC   TSTCURR,TOTCURR                                                  
         LA    R0,3                                                             
         LA    RE,TSTAMT1          RE=A(TSAR ACCOUNT TOTALS)                    
         LA    RF,ACCBILL1         RF=A(ACCOUNT TOTALS)                         
         ZAP   0(L'TSTAMT1,RE),0(L'ACCBILL1,RF)                                 
         LA    RE,L'TSTAMT1(RE)                                                 
         LA    RF,L'ACCBILL1(RF)                                                
         BCT   R0,*-14                                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         BAS   RE,FORMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
*                                                                               
         MVC   TSTLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    TOTX                                                             
*                                                                               
TOT50    GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY DATE HEADLINES AND BUILD DATE TABLE                  *         
* ON ENTRY RF=A(SCREEN LINE FOR FIRST HEADING)                        *         
***********************************************************************         
AGEGAP   NTR1                                                                   
*                                                                               
         LR    R2,RF                                                            
         USING SCRHEAD,R2                                                       
         L     R6,AOPTVALS         R6=A(OPTION VALUES)                          
         USING OPTVALSD,R6                                                      
         SR    RF,RF                                                            
         ICM   RF,3,OINCVL         HAS A DATE INTERVAL BEEN SPECIFIED?          
         BZ    *+10                                                             
         BCTR  RF,0                                                             
         B     *+8                                                              
         LA    RF,DEFINTQ          NO, USE DEFAULT INTERVAL                     
         LNR   RF,RF                                                            
         ST    RF,DATEINT          SAVE THE DATE INTERVAL                       
*                                                                               
         XC    STRTDATE,STRTDATE   PERIOD START DATE (IF ANY)                   
         XC    ENDDATE,ENDDATE     PERIOD END DATE/ TODAYS DATE                 
         MVC   GRDDTES(GRDDTEQ),SPACES                                          
*                                                                               
AGE02    CLI   OAGEBY,C'M'         AGE BY MOS?                                  
         BNE   AGE06                                                            
         OC    OMOS,OMOS           MOS FILTERING?                               
         BZ    AGE50               NO OPTIONS START=0 END=TODAY                 
         LA    R3,OMOS             R3=A(DATE OPTION)                            
         B     AGE10                                                            
AGE06    CLI   OAGEBY,C'D'         AGE BY DUE DATE?                             
         BNE   AGE08                                                            
         OC    ODUE,ODUE           DUE DATE FILTERING?                          
         BZ    AGE50               NO OPTIONS START=0 END=TODAY                 
         LA    R3,ODUE             R3=A(DATE OPTION)                            
         B     AGE10                                                            
AGE08    OC    ODATE,ODATE         ASSUME AGE BY BILL DATE                      
         BZ    AGE50                                                            
         LA    R3,ODATE            R3=A(DATE OPTION)                            
*                                                                               
         USING DATED,R3            GENERAL OPTION DATE DSECT                    
AGE10    CLI   DATEFI,NEGFILTR     NEGATIVE FILTERING?                          
         BE    AGE30                                                            
         OC    DATEEND,DATEEND     IF NO END VALUE START=VAL1 END=TODAY         
         BNZ   AGE20                                                            
         GOTO1 VDATCON,DMCB,(1,DATESTRT),(0,STRTDATE) USE START VALUE           
         B     AGE50               BUT USE TODAYS DATE FOR END DATE             
AGE20    GOTO1 VDATCON,DMCB,(1,DATEEND),(0,ENDDATE) USE END VALUE               
         OC    DATESTRT,DATESTRT                                                
         BZ    AGE60                                                            
         GOTO1 VDATCON,DMCB,(1,DATESTRT),(0,STRTDATE) USE START VALUE           
         B     AGE60                                                            
*                                                                               
AGE30    OC    DATESTRT,DATESTRT   IF NO START START=VAL2+1 END=TODAY           
         BNZ   AGE40                                                            
         GOTO1 VDATCON,DMCB,(1,DATEEND),(0,TEMP)                                
         LA    RF,1                                                             
         GOTO1 VADDAY,DMCB,TEMP,STRTDATE,(RF) USE DAY AFTER END DATE            
         B     AGE50                                                            
AGE40    OC    DATEEND,DATEEND     IF START START=0 END VAL1-1                  
         BNZ   AGE50                                                            
         GOTO1 VDATCON,DMCB,(1,DATESTRT),(0,TEMP)                               
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         GOTO1 VADDAY,DMCB,TEMP,ENDDATE,(RF) USE DAY BEFORE START DATE          
         B     AGE60                                                            
*                                                                               
AGE50    GOTO1 VDATCON,DMCB,(5,0),(0,ENDDATE) TODAYS DATE                       
         DROP  R3                                                               
*                                                                               
AGE60    MVC   INTEND,ENDDATE      END OF FIRST INTERVAL                        
         LA    R0,3                MAX NUMBER OF AGEING INTERVALS               
         LA    R3,AGETAB           R3=A(AGEING INTERVAL TABLE)                  
*                                                                               
         LA    R4,SCRHDAT1         R4=A(SCREEN POSITION)                        
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+8                                                              
         LA    R4,GRDDTE1          R4=A(GRIDS DATE HEADINGS)                    
*                                                                               
         USING AGETABD,R3                                                       
         CLI   OINCTYPE,0          DEFAULT IS MONTH INCREMENT                   
         BE    AGE90                                                            
         CLI   OINCTYPE,C'M'       INCREMENT SET AT MONTH?                      
         BE    AGE90                                                            
         CLI   OAGEBY,C'M'         MOS OPTION?                                  
         BNE   AGE140                                                           
AGE90    MVC   INTEND+4(2),=C'15'  OVERWRITE DAYS WITH MID MONTH                
         CH    R0,=H'1'            LAST COLUMN?                                 
         BNE   AGE110                                                           
         OC    STRTDATE,STRTDATE   IS THERE A START DATE?                       
         BZ    AGE100                                                           
         GOTO1 VDATCON,DMCB,(0,STRTDATE),(1,AGESTART)                           
         CLC   INTEND(2),STRTDATE  START SAME AS END FOR INTERVAL?              
         BE    AGE120                                                           
*                                                                               
         LA    RF,ENQDAT2H-ENQDAT1H(R4)                                         
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+8                                                              
         LA    RF,L'GRDDTE1(R4)                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(0,INTEND),(6,(RF)) NO, DISPLAY START DATE          
         B     AGE120                                                           
*                                                                               
AGE100   GOTO1 VDATCON,DMCB,(0,INTEND),(6,(R4))                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    AGE108                                                           
         LA    RF,L'GRDDTE1-1(R4)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'MX@ANDPR,RF),MX@ANDPR                                        
         B     AGEX                                                             
AGE108   MVC   ENQDAT2H-ENQDAT1H(L'MX@ANDPR,R4),MX@ANDPR AND PRIOR              
         B     AGEX                                                             
*                                                                               
AGE110   GOTO1 VDATCON,DMCB,(0,INTEND),(1,AGESTART)                             
*                                                                               
AGE120   GOTO1 VDATCON,DMCB,(1,AGESTART),(6,(R4))                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    AGE130                                                           
*                                                                               
AGE126   CLC   ENQDAT2H-ENQDAT1H(L'SCRHDAT1,R4),SPACES                          
         BNE   AGE130                                                           
         LA    RF,L'SCRHDAT1-1(R4)                                              
AGE128   CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R4               RF=L'(DATE HEADING) - 1                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ENQDAT2H-ENQDAT1H(0,R4),UNDERLIN                                 
AGE130   CLC   INTEND(4),STRTDATE                                               
         BNH   AGEX                                                             
*                                                                               
         L     RF,DATEINT                                                       
         GOTO1 VADDAY,DMCB,INTEND,INTSTART,(RF) GET PREVIOUS MONTH              
         MVC   INTEND,INTSTART                                                  
         LA    R3,AGELNQ(R3)                                                    
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         LA    R4,L'GRDDTES(R4)                                                 
         B     *+8                                                              
         LA    R4,SCRHDAT2-SCRHDAT1(R4)                                         
*                                                                               
         BCT   R0,AGE90                                                         
         B     AGEX                                                             
*                                                                               
AGE140   L     RF,DATEINT                                                       
         GOTO1 VADDAY,DMCB,INTEND,INTSTART,(RF)                                 
         LR    RF,R4                                                            
         CH    R0,=H'1'                                                         
         BNE   AGE150                                                           
         OC    STRTDATE,STRTDATE                                                
         BZ    AGE152                                                           
*                                                                               
AGE150   LA    RF,ENQDAT2H-ENQDAT1H(R4)                                         
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+8                                                              
         LA    RF,L'GRDDTE1(R4)                                                 
*                                                                               
AGE152   GOTO1 VDATCON,DMCB,(0,INTEND),(8,(RF))                                 
*                                                                               
         OC    STRTDATE,STRTDATE                                                
         BNZ   AGE160                                                           
         CH    R0,=H'1'                                                         
         BNE   AGE170                                                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    AGE158                                                           
         LA    RF,L'GRDDTE1-1(R4)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'MX@ANDPR,RF),MX@ANDPR                                        
         B     *+10                                                             
AGE158   MVC   ENQDAT2H-ENQDAT1H(L'MX@ANDPR,R4),MX@ANDPR                        
         B     AGE190                                                           
*                                                                               
AGE160   CLC   INTSTART,STRTDATE                                                
         BL    *+12                                                             
         CH    R0,=H'1'                                                         
         BNE   AGE170                                                           
         MVC   INTSTART,STRTDATE                                                
AGE170   GOTO1 VDATCON,DMCB,(0,INTSTART),(1,AGESTART)                           
         GOTO1 VDATCON,DMCB,(0,INTSTART),(8,(R4))                               
*                                                                               
         LA    RF,L'SCRHDAT1-1(R4)                                              
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+8                                                              
         LA    RF,L'GRDDTE1-1(R4)                                               
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLC   INTSTART,INTEND                                                  
         BNE   AGE180                                                           
         SR    RF,R4               RF=L'(DATE HEADING) - 1                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    AGE178                                                           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ENQDAT2H-ENQDAT1H(0,R4),UNDERLIN                                 
AGE178   CLI   DATEINT,0                                                        
         BE    *+12                                                             
         B     AGEX                                                             
AGE180   MVI   1(RF),C'-'                                                       
*                                                                               
AGE182   CLC   INTSTART,STRTDATE                                                
         BE    AGEX                                                             
*                                                                               
AGE190   LHI   RF,-1                                                            
         GOTO1 VADDAY,DMCB,INTSTART,INTEND,(RF) USE DAY AFTER END DATE          
         LA    R3,AGELNQ(R3)                                                    
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         LA    R4,L'GRDDTES(R4)                                                 
         B     *+8                                                              
         LA    R4,SCRHDAT2-SCRHDAT1(R4)                                         
*                                                                               
         BCT   R0,AGE140                                                        
*                                                                               
AGEX     B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ADD SOURCE TOTALS                                                   *         
***********************************************************************         
ADDSRCE  NTR1                                                                   
*                                                                               
         LA    R0,3                R0=(NUMBER OF AGEING COLUMNS)                
         LA    RF,AGETAB           RF=A(AGEING DATE TABLE)                      
         LA    R1,SRCEBIL1         R1=A(SOURCE TOTALS)                          
         USING AGETABD,RF                                                       
ADDS10   L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLI   OAGEBY,C'M'         MOS AGEING?                                  
         BNE   ADDS20                                                           
         CLC   BILLMOS,AGESTART                                                 
         BNL   ADDS30                                                           
         B     ADDS27                                                           
ADDS20   CLI   OAGEBY,C'D'         DUE DATE AGEING?                             
         BNE   ADDS25                                                           
         OC    BILLDUE,BILLDUE                                                  
         BZ    ADDS25              IF NO DUE DATE USE BILLING DATE              
         MVC   AGEDATE,BILLDUE                                                  
         B     *+10                                                             
ADDS25   MVC   AGEDATE,BILLDATE                                                 
         LA    R4,L'AGEDATE        R4=L'(BILL/DUE DATE PACKED YYMMDD)           
         CLI   OINCTYPE,0          DEFAULT IS MONTH                             
         BE    *+12                                                             
         CLI   OINCTYPE,C'M'       IF INCREMENT BY MONTH REDUCE LENGTH          
         BNE   *+8                                                              
         LA    R4,L'AGEDATE-1                                                   
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   AGEDATE(0),AGESTART BILL DATE LESS THAN AGEING START?            
         BNL   ADDS30                                                           
*                                                                               
ADDS27   LA    R1,L'SRCEBIL1(R1)   BUMP TO NEXT AGEING PERIOD                   
         LA    RF,AGELNQ(RF)                                                    
         BCT   R0,ADDS10                                                        
ADDS30   AP    0(L'SRCEBIL1,R1),OUTAMNT ADD TO AGEING COLUMN                    
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
UNDERLIN DC    C'-----------'                                                   
SI       DC    C'SI'                                                            
SR       DC    C'SR'                                                            
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH06,78                                                      
         DCDDL AC#ENH07,78                                                      
         DCDDL AC#ENH08,78                                                      
         DCDDL AC#ENH09,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#CTR,9                                                         
         DCDDL AC#DISS,7                                                        
         DCDDL AC#ATTR,9                                                        
         DCDDL AC#TYPE,2                                                        
         DCDDL AC#STT,2                                                         
         DCDDL AC#OTHER,2                                                       
         DCDDL AC#UDAT,2                                                        
         DCDDL AC#DATE,2                                                        
         DCDDL AC#PELDT,2                                                       
         DCDDL AC#SEQ,2                                                         
         DCDDL AC#ACCTS,15                                                      
         DCDDL AC#ANDPR,10                                                      
         DCDDL AC#SRCTS,15                                                      
         DCDDL AC#CONTS,15                                                      
         DCDDL AC#BIL,6                                                         
         DCDDL AC#DUE,4                                                         
         DCDDL AC#BLGSR,L'MX@BLGSR    BILLING SOURCE                            
         DCDDL AC#RSINO,L'MX@RSINO    BILL NUMBER                               
         DCDDL AC#RSBDT,L'MX@RSBDT    BILL DATE                                 
         DCDDL AC#RSDUD,L'MX@RSDUD    DUE DATE                                  
         DCDDL AC#ACTYD,L'MX@ACTYD    ACTIVITY DATE                             
         DCDDL AC#PTYPD,L'MX@PTYPD    PARTLY PAID                               
         DCDDL AC#TYPE1,L'MX@TYPE1    TRN TYPE                                  
         DCDDL AC#INVC2,L'MX@INVCN    INVOICE NUMBER                            
         DCDDL AC#TFOR,L'MX@TFOR      TOTAL FOR                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY MONTH AND OFFICE COLUMNS FOR GRIDS                   *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
         USING GCTBLD,R3                                                        
GSPECR   NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
         L     R4,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
*----------------------------------------------------------------------         
* AMOUNT COLUMN                                                                 
*----------------------------------------------------------------------         
         CLI   GCTCOID,GCAMTOT                                                  
         BNE   GSP60                                                            
*                                                                               
         CLI   TSDFMT,TSTOTITM                                                  
         BE    GSP50                                                            
         CLI   TSDFMT,TSSRCITM                                                  
         BE    GSP30                                                            
         ZAP   DUB,TSDAMT1                                                      
         AP    DUB,TSDAMT2                                                      
         AP    DUB,TSDAMT3                                                      
         CURED DUB,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                    
         LR    R1,R0                                                            
         B     GSPECX                                                           
*                                                                               
         USING TSARSRCD,R4                                                      
GSP30    ZAP   DUB,TSSAMT1                                                      
         AP    DUB,TSSAMT2                                                      
         AP    DUB,TSSAMT3                                                      
         CURED DUB,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                    
         LR    R1,R0                                                            
         B     GSPECX                                                           
*                                                                               
         USING TSARTOTD,R4                                                      
GSP50    ZAP   DUB,TSTAMT1                                                      
         AP    DUB,TSTAMT2                                                      
         AP    DUB,TSTAMT3                                                      
         CURED DUB,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                    
         LR    R1,R0                                                            
         B     GSPECX                                                           
*----------------------------------------------------------------------         
* CONTRA OR SOURCE COLUMN                                                       
*----------------------------------------------------------------------         
         USING TSARDATD,R4                                                      
GSP60    CLI   GCTCOID,GCCTR                                                    
         BE    *+12                                                             
         CLI   GCTCOID,GCBIS                                                    
         BNE   GSPECX                                                           
*                                                                               
         TM    GCTIND3,GCTI2ND                                                  
         BO    GSP65                                                            
         CLI   TSDFMT,TSTOTITM     TOTAL ITEM TYPE                              
         BE    GSP70                                                            
*                                                                               
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TSDCONT),TSDCONT                                        
         LHI   R1,L'TSDCONT+1                                                   
         B     GSPECX                                                           
*                                                                               
         USING TSARSRCD,R4                                                      
GSP65    MVI   TEMP,C'S'                                                        
         MVC   TEMP+1(L'MX@TFOR),MX@TFOR                                        
         MVC   TEMP+L'MX@TFOR+2(L'TSSCONT),TSSCONT                              
         LHI   R1,L'MX@TFOR+2+L'TSSCONT                                         
         B     GSPECX                                                           
*                                                                               
GSP70    MVI   TEMP,C'T'                                                        
         MVC   TEMP+1(L'MX@ACCTS),MX@ACCTS                                      
         LHI   R1,L'MX@ACCTS+1                                                  
         B     GSPECX                                                           
*                                                                               
GSPECX   J     XITR1                                                            
         LTORG                                                                  
         DROP  R4,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*      RECEIVABLE GRID COLUMN TABLE - COVERED BY GCTBLD               *         
***********************************************************************         
GCTBL    DS    0F                                                               
*_____________________________________________________________________          
GCTCTR   DC    AL1(GCTRLQ,GCCTR,L'LC@CTRA,0)       CONTRA A/C                   
         DC    AL2(LC@CTRA-WORKD,AGSPECR-OVERWRKD)                              
         DC    AL1(GCTITOT+GCTIXLDG+GCTIROUT+GCTIRTOT,0,0,GCTI1ST)              
         DC    AL1(0,0),AL2(AGSPECR-OVERWRKD)                                   
         DC    C'SR'                                                            
GCTRLQ   EQU   *-GCTCTR                                                         
*                                                                               
GCTCTR2  DC    AL1(GCTR2LQ,GCCTR,0,0)              CONTRA SUB-TOTAL             
         DC    AL2(0,AGSPECR-OVERWRKD)                                          
         DC    AL1(GCTIXLDG+GCTIROUT,0,0,GCTI2ND)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR'                                                            
GCTR2LQ  EQU   *-GCTCTR2                                                        
*                                                                               
GCTBIS   DC    AL1(GBISLQ,GCBIS,L'MX@BLGSR,0)          BILLING SOURCE           
         DC    AL2(MX@BLGSR-OVERWRKD,AGSPECR-OVERWRKD)                          
         DC    AL1(GCTITOT+GCTIOVER+GCTIROUT+GCTIRTOT,0,0,GCTI1ST)              
         DC    AL1(0,0),AL2(AGSPECR-OVERWRKD)                                   
         DC    C'SR'                                                            
GBISLQ   EQU   *-GCTBIS                                                         
*                                                                               
GCTBIS2  DC    AL1(GBIS2LQ,GCBIS,0,L'TSDCONT)          SOURCE SUB-TOTAL         
         DC    AL2(0,AGSPECR-OVERWRKD)                                          
         DC    AL1(GCTIROUT,0,0,GCTI2ND)                                        
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR'                                                            
GBIS2LQ  EQU   *-GCTBIS2                                                        
*_____________________________________________________________________          
GCTREF   DC    AL1(GREFLQ,GCREF,L'LC@REF,L'TSDTREF)     REF#                    
         DC    AL2(LC@REF-WORKD,TSDTREF-TSARDATD)                               
         DC    AL1(GCTIXLDG,0,0,GCTI1ST)                                        
         DC    AL1(0,0,0,0)                                                     
         DC    C'SR'                                                            
GREFLQ   EQU   *-GCTREF                                                         
*                                                                               
GCTBIN   DC    AL1(GBINLQ,GCBIN,L'MX@RSINO,L'TSDTREF)   BILL NUMBER             
         DC    AL2(MX@RSINO-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,GCTI1ST)                                        
         DC    AL1(0,0,0,0)                                                     
         DC    C'SR'                                                            
GBINLQ   EQU   *-GCTBIN                                                         
*_____________________________________________________________________          
GCTDATE  DC    AL1(GDATLQ,GCDATE,L'LC@DATE,1)           BILL DATE               
         DC    AL2(LC@DATE-WORKD,TSDTDAT-TSARDATD)                              
         DC    AL1(0,0,GCTFDAT+GCTFRGHT,GCTI1ST)                                
         DC    AL1(0,0,0,0)                                                     
GDATLQ   EQU   *-GCTDATE                                                        
*____________________________________________________________________           
GCTAM1   DC    AL1(GAM1LQ,GCAM1,L'GRDDTES,L'TSDAMT1)   TRANS AMNT 1             
         DC    AL2(GRDDTE1-OVERWRKD,TSDAMT1-TSARDATD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI1ST)                 
         DC    AL1(0,L'TSTAMT1),AL2(TSTAMT1-TSARTOTD)                           
GAM1LQ   EQU   *-GCTAM1                                                         
*                                                                               
GCTAM12  DC    AL1(GAM12LQ,GCAM1,L'GRDDTES,L'TSSAMT1)  TRANS AMNT 1             
         DC    AL2(GRDDTE1-OVERWRKD,TSSAMT1-TSARSRCD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI2ND)                 
         DC    AL1(0,L'TSTAMT1),AL2(TSTAMT1-TSARTOTD)                           
GAM12LQ  EQU   *-GCTAM12                                                        
*____________________________________________________________________           
GCTAM2   DC    AL1(GAM2LQ,GCAM2,L'GRDDTES,L'TSDAMT2)   TRANS AMNT 2             
         DC    AL2(GRDDTE2-OVERWRKD,TSDAMT2-TSARDATD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI1ST)                 
         DC    AL1(0,L'TSTAMT2),AL2(TSTAMT2-TSARTOTD)                           
GAM2LQ   EQU   *-GCTAM2                                                         
*                                                                               
GCTAM22  DC    AL1(GAM22LQ,GCAM2,L'GRDDTES,L'TSSAMT2)  TRANS AMNT 2             
         DC    AL2(GRDDTE2-OVERWRKD,TSSAMT2-TSARSRCD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI2ND)                 
         DC    AL1(0,L'TSTAMT2),AL2(TSTAMT2-TSARTOTD)                           
GAM22LQ  EQU   *-GCTAM22                                                        
*____________________________________________________________________           
GCTAM3   DC    AL1(GAM3LQ,GCAM3,L'GRDDTES,L'TSDAMT3)   TRANS AMNT 3             
         DC    AL2(GRDDTE3-OVERWRKD,TSDAMT3-TSARDATD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI1ST)                 
         DC    AL1(0,L'TSTAMT3),AL2(TSTAMT3-TSARTOTD)                           
GAM3LQ   EQU   *-GCTAM3                                                         
*                                                                               
GCTAM32  DC    AL1(GAM32LQ,GCAM3,L'GRDDTES,L'TSSAMT3)  TRANS AMNT 3             
         DC    AL2(GRDDTE3-OVERWRKD,TSSAMT3-TSARSRCD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTI2ND)                 
         DC    AL1(0,L'TSTAMT3),AL2(TSTAMT3-TSARTOTD)                           
GAM32LQ  EQU   *-GCTAM32                                                        
*____________________________________________________________________           
GCTAMT   DC    AL1(GAMTLQ,GCAMTOT,L'LC@TOTAL,0)        TOTAL                    
         DC    AL2(LC@TOTAL-WORKD,AGSPECR-OVERWRKD)                             
         DC    AL1(GCTITOT+GCTIROUT+GCTIRTOT,0)                                 
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(AGSPECR-OVERWRKD)                                   
GAMTLQ   EQU   *-GCTAMT                                                         
*                                                                               
GCTPP    DC    AL1(GPPLQ,GCPP,L'MX@PTYPD,L'TSDPAID)    PARTLY PAID              
         DC    AL2(MX@PTYPD-OVERWRKD,TSDPAID-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,GCTI1ST)                                        
         DC    AL1(0,0,0,0)                                                     
GPPLQ    EQU   *-GCTPP                                                          
*                                                                               
GCTACTD  DC    AL1(GACTLQ,GCACTD,L'MX@ACTYD,2)         ACTIVITY DATE            
         DC    AL2(MX@ACTYD-OVERWRKD,TSDACDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,GCTI1ST)                         
         DC    AL1(0,0,0,0)                                                     
GACTLQ   EQU   *-GCTACTD                                                        
*                                                                               
GCTDUED  DC    AL1(GDUELQ,GCDUED,L'MX@RSDUD,2)         DUE DATE                 
         DC    AL2(MX@RSDUD-OVERWRKD,TSDDUDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,GCTI1ST)                         
         DC    AL1(0,0,0,0)                                                     
GDUELQ   EQU   *-GCTDUED                                                        
*                                                                               
GCTTRNT  DC    AL1(GTRNTLQ,GCTRT,L'MX@TYPE1,1)         TRN TYPE                 
         DC    AL2(MX@TYPE1-OVERWRKD,TSDTTYPE-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFRGHT,GCTI1ST)                   
         DC    AL1(0,0),AL2(0)                                                  
GTRNTLQ  EQU   *-GCTTRNT                                                        
*        -----------------------------------                                    
GCTINV#  DC    AL1(GCTINVQ,GCINVN,L'MX@INVCN,L'TSDINV#) INVOICE                 
         DC    AL2(MX@INVCN-OVERWRKD,TSDINV#-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,GCTI1ST)                                        
         DC    AL1(0,0,0,0)                                                     
GCTINVQ  EQU   *-GCTINV#                                                        
*                                                                               
GCTINV2  DC    AL1(GINV2LQ,GCINVN,L'MX@INVCN,L'TSDTREF) INVOICE                 
         DC    AL2(MX@INVCN-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,GCTI1ST)                                        
         DC    AL1(0,0),AL2(0)                                                  
GINV2LQ  EQU   *-GCTINV2                                                        
*        -----------------------------------                                    
*                                                                               
GCTTST   DC    AL1(GTSTLQ,GCTST,L'LC@STT,L'TSDTEST)    TRN STAT                 
         DC    AL2(LC@STT-WORKD,TSDTRST-TSARDATD)                               
         DC    AL1(0,GCTIHEX+GCTIDDS,0,GCTI1ST)                                 
         DC    AL1(0,0),AL2(0)                                                  
GTSTLQ   EQU   *-GCTTST                                                         
*                                                                               
GCTSQ    DC    AL1(GSQLQ,GCSQ,L'MX@SEQ,1)              SEQUENCE NUMBER          
         DC    AL2(MX@SEQ-OVERWRKD,TSDTSBR-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIBIN+GCTIDDS,GCTFNUM+GCTFRGHT,GCTI1ST)           
         DC    AL1(0,0),AL2(0)                                                  
GSQLQ    EQU   *-GCTSQ                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
* GRID COLUMN EQUATES                                                           
*---------------------------------------------------------------------          
GCCTR    EQU   1           CONTRA A/C                                           
GCBIS    EQU   2           BILLING SOURCE                                       
GCREF    EQU   3           REF#                                                 
GCBIN    EQU   4           BILL NUMBER                                          
GCDATE   EQU   5           BILL DATE                                            
GCAM1    EQU   6           AMOUNT 1                                             
GCAM2    EQU   7           AMOUNT 2                                             
GCAM3    EQU   8           AMOUNT 3                                             
GCAMTOT  EQU   9           TOTAL AMOUNT                                         
GCPP     EQU   10          PARTLY PAID                                          
GCACTD   EQU   11          ACTIVITY DATE                                        
GCDUED   EQU   12          DUE DATE                                             
GCTRT    EQU   13          TRANSACTION TYPE                                     
GCINVN   EQU   14          INVOICE NUMBER LONG                                  
GCTST    EQU   80          TRANSACTION STATUS                                   
GCSQ     EQU   81          SEQUENCE NUMBER                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
OVERWRKD DSECT                                                                  
*                                                                               
PARAMREF DS    CL(L'TRNKREF)       PAYEMNT/CHEQUE NUMBER                        
PARAMDAT DS    CL(L'TRNKDATE)      PAYMENT/CHEQUE DATE                          
BILLDATE DS    CL(L'TRNKDATE)      BILL DATE                                    
BILLDUE  DS    PL3                 DUE DATE                                     
BILLMOS  DS    CL(L'TRNKSMOS)      BILL MOS                                     
AGEDATE  DS    PL3                 'AGEING' BY DATE                             
DATEINT  DS    F                   NUMBER OF DAYS PER INTERVAL                  
INTSTART DS    CL6                 START DATE FOR AGEING INTERVAL               
INTEND   DS    CL6                 END DATE FOR AGEING INTERVAL                 
AINVELD  DS    A                   SAVED ADDRESS FOR INVOICE NUMBER             
*                                                                               
GRDDTES  DS    0CL20               DATE HEADINGS FOR GRIDS                      
GRDDTE1  DS    CL10                                                             
         DS    CL10                                                             
GRDDTE2  DS    CL10                                                             
         DS    CL10                                                             
GRDDTE3  DS    CL10                                                             
         DS    CL10                                                             
GRDDTEQ  EQU   *-GRDDTES                                                        
*                                                                               
ANXTLINE DS    A                   ADDRESS OF NEXT DUMMY LINE                   
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGSPECR  DS    A                   GRID SPECIAL ROUTINE                         
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
OTHFLAG  DS    XL1                 OTHELD FLAG                                  
OTHELEQ  EQU   X'01'               OTHELD ELEMENT FOUND                         
OTHRQDQ  EQU   X'02'               OTHELD ELEMENT REQUIRED                      
CHQFLAG  DS    XL1                 CHEQUE ELEMENT FLAG                          
CHQELEQ  EQU   X'01'               CHEQUE ELEMENT FOUND                         
CHQRQDQ  EQU   X'02'               CHEQUE ELEMENT REQUIRED                      
DUEFLAG  DS    XL1                 DUE DATE ELEMENT FLAG                        
DUEELEQ  EQU   X'01'               DUE DATE ELEMENT FOUND                       
DUERQDQ  EQU   X'02'               DUE DATE ELEMENT REQUIRED                    
INTFLAG  DS    X                   GENERAL INTERNAL FLAG                        
SCRFULLQ EQU   X'80'               SCREEN FULL                                  
REJECTQ  EQU   X'40'               REJECT TRANSACTION                           
PAIDQ    EQU   X'20'               BILL HAS RECEIVED PAYMENT(S)                 
NOTFSTQ  EQU   X'10'               NOT FIRST DEBIT FOR THIS BILL                
FILTCURR DS    CL(L'AFCCURR)       FILTER TRANSACTION CURRENCY                  
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
TOTCURR  DS    CL(L'AFCCURR)       TOTALS CURRENCY                              
*                                                                               
DSMIX    DS    0C                                                               
MX@ENH06 DS    CL(L'ENQDAT1)                                                    
MX@ENH07 DS    CL(L'ENQDAT1)                                                    
MX@ENH08 DS    CL(L'ENQDAT1)                                                    
MX@ENH09 DS    CL(L'ENQDAT1)                                                    
MX@ACC   DS    CL9                                                              
MX@CTR   DS    CL9                                                              
MX@DISS  DS    CL7                                                              
MX@ATTR  DS    CL9                                                              
MX@TYPE  DS    CL2,CL1                                                          
MX@STT   DS    CL2,CL1                                                          
MX@OTHER DS    CL2,CL1                                                          
MX@UDAT  DS    CL2,CL1                                                          
MX@DATE  DS    CL2,CL1                                                          
MX@PELDT DS    CL2,CL1                                                          
MX@SEQ   DS    CL2,CL1                                                          
MX@ACCTS DS    CL15                                                             
MX@ANDPR DS    CL10                                                             
MX@SRCTS DS    CL15                                                             
MX@CONTS DS    CL15                                                             
MX@BILL  DS    CL6                                                              
MX@DUE   DS    CL4                                                              
MX@BLGSR DS    CL14                BILLING SOURCE                               
MX@RSINO DS    CL11                BILL NUMBER                                  
MX@RSBDT DS    CL9                 BILL DATE                                    
MX@RSDUD DS    CL8                 DUE DATE                                     
MX@ACTYD DS    CL13                ACTIVITY DATE                                
MX@PTYPD DS    CL11                PARTLY PAID                                  
MX@TYPE1 DS    CL8                 TRN TYPE                                     
MX@INVCN DS    CL14                INVOICE #                                    
MX@TFOR  DS    CL9                 TOTAL FOR                                    
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SCREEN LINES                                                                  
***********************************************************************         
SCRHEAD  DSECT                     COVER DATE HEADLINES                         
         DS    CL23                                                             
SCRHBD   DS    0CL11               BILL/DUE DATE COLUMN OVERRIDE                
SCRHBILL DS    CL6                 BILL                                         
         DS    CL1                                                              
SCRHDUE  DS    CL4                 DUE                                          
         DS    CL7                                                              
SCRHDAT1 DS    CL10                DATE 1                                       
         DS    CL2                                                              
SCRHDAT2 DS    CL10                DATE 2                                       
         DS    CL2                                                              
SCRHDAT3 DS    CL10                DATE 3                                       
*                                                                               
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CONT DS    CL(L'TRNKULC)       BILLING SOURCE/CONTRAA                       
         DS    CL1                                                              
SCR1TREF DS    CL6                 BILL NUMBER                                  
         DS    CL1                                                              
SCR1TDAT DS    CL8                 BILL DATE                                    
SCR1AMT1 DS    CL11                AMOUNT FOR INTERVAL 1                        
         DS    CL1                                                              
SCR1AMT2 DS    CL11                AMOUNT FOR INTERVAL 2                        
         DS    CL1                                                              
SCR1AMT3 DS    CL11                AMOUNT FOR INTERVAL 3                        
         DS    CL1                                                              
SCR1TOT  DS    CL11                AMOUNT                                       
SCR1PAID DS    CL1                 PARTLY PAID?                                 
SCR1LNQ  EQU   *-SCRLIN1D                                                       
*                                                                               
SCRSRCED DSECT                     COVER SCREEN SOURCE TOTAL LINE               
SCRSCONT DS    CL(L'TRNKULC)       CONTRA                                       
         DS    CL1                                                              
SCRSSTOT DS    CL(L'MX@SRCTS)      SOURCE TOTALS                                
SCRSAMT1 DS    CL11                SOURCE AMOUNT 1                              
         DS    CL1                                                              
SCRSAMT2 DS    CL11                SOURCE AMOUNT 2                              
         DS    CL1                                                              
SCRSAMT3 DS    CL11                SOURCE AMOUNT 3                              
         DS    CL1                                                              
SCRSTOT  DS    CL11                SOURCE TOTAL                                 
*                                                                               
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE                      
         DS    CL15                                                             
SCRTOTAL DS    CL(L'MX@ACCTS)                                                   
SCRTAMT1 DS    CL11                ACCOUNT TOTAL AMOUNT FOR PERIOD 1            
         DS    CL1                                                              
SCRTAMT2 DS    CL11                ACCOUNT TOTAL AMOUNT FOR PERIOD 2            
         DS    CL1                                                              
SCRTAMT3 DS    CL11                ACCOUNT TOTAL AMOUNT FOR PERIOD 3            
         DS    CL1                                                              
SCRTTOT  DS    CL11                ACCOUNT TOTAL FOR ALL PERIODS                
*                                                                               
***********************************************************************         
* TSAR RECORD. NOTE THIS RECORD MAY HAVE ELEMENTS ATTACHED                      
* POSSIBLE GENFILE ELEMENTS INCLUDE                                             
* OTHELD 'OTHERS' ELEMENT                                                       
* APEELD 'ANALYSIS POINTER' ELEMENT                                             
* SCIELD 'SUBSIDIARY CASH' ELEMENT                                              
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDCONT  DS    CL(L'TRNKULC)       BILLING SOURCE/CONTRA                        
TSDTDAT  DS    PL(L'TRNKDATE)      BILL DATE                                    
TSDTREF  DS    CL(L'TRNKREF)       BILL NUMBER                                  
TSDAMT1  DS    PL(L'TRNAMNT)       TRANSACTION AMOUNT INTERVAL 1                
TSDAMT2  DS    PL(L'TRNAMNT)       TRANSACTION AMOUNT INTERVAL 2                
TSDAMT3  DS    PL(L'TRNAMNT)       TRANSACTION AMOUNT INTERVAL 3                
TSDTRST  DS    XL(L'TRNRSTAT)      TRANSACTION RECORD STATUS                    
TSDTEST  DS    XL(L'TRNSTAT)       TRANSACTION ELEMENT STATUS                   
TSDTSBR  DS    XL(L'TRNKSBR)       TRANSACTION SUB-REF NUMBER                   
TSDTTYPE DS    CL(L'TRNTYPE)       INPUT TYPE                                   
TSDACDAT DS    XL(L'TRSDATE)       ACTIVITY DATE                                
TSDUSDAT DS    XL(L'TRSUDAT)       USED DATE                                    
TSDPEDAT DS    XL(L'TRSPDAT)       PEELED DATE                                  
TSDDUDAT DS    XL(L'DUEDATE)       DUE DATE                                     
TSDCURR  DS    XL(L'AFCCURR)       CURRENCY CODE                                
TSDINV#  DS    CL20                LONG INVOICE NUMBER                          
TSDPAID  DS    X                   PART PAID INDICATOR                          
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARSRCD DSECT                     COVER SCREEN LINE 1                          
TSSLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSSFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSSRCITM EQU   2                   WORKCODE TOTAL ITEM TYPE                     
TSSCONT  DS    CL(L'TRNKULC)       BILLING SOURCECONTRA                         
TSSCURR  DS    PL(L'AFCCURR)       CURRENCY CODE                                
TSSLN1Q  EQU   *-TSARSRCD                                                       
TSSAMT1  DS    PL(L'TRNAMNT)       AMOUNT1                                      
TSSAMT2  DS    PL(L'TRNAMNT)       AMOUNT2                                      
TSSAMT3  DS    PL(L'TRNAMNT)       AMOUNT3                                      
TSSLN2Q  EQU   *-TSARSRCD                                                       
*                                                                               
TSARTOTD DSECT                     TOTAL LINE                                   
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSTOTITM EQU   3                   TOTAL ITEM TYPE                              
TSTAMT1  DS    PL(L'TRNAMNT)       AMOUNT1                                      
TSTAMT2  DS    PL(L'TRNAMNT)       AMOUNT2                                      
TSTAMT3  DS    PL(L'TRNAMNT)       AMOUNT3                                      
TSTCURR  DS    PL(L'AFCCURR)       CURRENCY CODE                                
TSTLNQ   EQU   *-TSARTOTD                                                       
*                                                                               
***********************************************************************         
* GENERAL DATE OPTION                                                 *         
***********************************************************************         
DATED    DSECT                     COVER GENERAL DATE OPTION                    
DATEFI   DS    X                   NEGATIVE FILTER                              
DATESTRT DS    XL3                 START DATE                                   
DATEEND  DS    XL3                 END DATE                                     
*                                                                               
***********************************************************************         
* AGE TABLE                                                           *         
***********************************************************************         
AGETABD  DSECT                     DSECT FOR AGE INTERVAL TABLE                 
AGESTART DS    XL3                 START OF INTERVAL                            
AGELNQ   EQU   *-AGETABD                                                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SAVED STORAGE                                                                 
***********************************************************************         
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
*                                                                               
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
UNILDG   DS    CL2                 UNIT AND LEDGER                              
AGEFLAG  DS    X                   RECV FLAG                                    
AGSRCTQ  EQU   X'80'               SOURCE TOTALS REQUIRED                       
AGPAYLQ  EQU   X'40'               PAYABLE LEDGER                               
SAVEBILL DS    CL(L'TRNKREF)       BILL NUMBER                                  
SAVESRCE DS    CL(L'TRNKULC)       BILLING SOURCE/CONTRA                        
SAVEDATE DS    CL(L'TRNKDATE)      BILLING DATE                                 
AGETAB   DS    3XL(3)              3*(END DATE) AGEING PERIODS                  
ENDDATE  DS    CL6                 END DATE OF PERIOD                           
STRTDATE DS    CL6                 START DATE OF PERIOD                         
DRVALS   DS    0PL6                CREDITOR VALUES                              
OUTAMNT  DS    PL6                 OUTSTANDING BILL AMOUNT                      
ACCTOTS  DS    0XL(L'ACCBILL1*3)                                                
ACCBILL1 DS    PL6                 BILLING ACCOUNT TOTAL 1                      
ACCBILL2 DS    PL6                 BILLING ACCOUNT TOTAL 2                      
ACCBILL3 DS    PL6                 BILLING ACCOUNT TOTAL 3                      
SRCETOTS DS    0XL(L'SRCEBIL1*3)                                                
SRCEBIL1 DS    PL6                 BILLING SOURCE TOTAL 1                       
SRCEBIL2 DS    PL6                 BILLING SOURCE TOTAL 2                       
SRCEBIL3 DS    PL6                 BILLING SOURCE TOTAL 3                       
DRVALLNQ EQU   *-DRVALS                                                         
SAVETSAR DS    XL(TSDLENQ)                                                      
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACENQ08   12/22/17'                                      
         END                                                                    
