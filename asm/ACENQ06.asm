*          DATA SET ACENQ06    AT LEVEL 024 AS OF 05/17/18                      
*PHASE T62006A                                                                  
*INCLUDE VATICAN                                                                
T62006   TITLE 'ACCOUNT ENQUIRY - JOB DETAIL'                                   
*                                                                               
* VSAX   024  ADDED PAY AMOUNT, DATE AND REF TO JD SCREEN    SPEC-20297         
*                                                                               
T62006   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ENQ6**,R5,R6,R7,CLEAR=YES,RR=RE                              
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE               SET UP GRID COLUMN TABLE                     
         ST    RF,AGCTBL                                                        
         L     RF,=A(GC2BL)                                                     
         AR    RF,RE               SET UP SUBTOTAL GRID COLUMN TABLE            
         ST    RF,AGC2BL                                                        
         L     RF,=A(GRDSP)                                                     
         AR    RF,RE               SET UP GRID COLUMN TABLE                     
         ST    RF,AGRDSP                                                        
*                                                                               
         LA    RF,PRVTAB                                                        
         ST    RF,APRVTAB                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
         GOTO1 VDICTATE,DMCB,C'L   ',DCCAP,DSCAP                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERXIT                                                            
         TM    DISPFLAG,NORECQ     NO TRANSACTIONS ON ACCOUNT?                  
         BO    MAINX                                                            
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN90                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE    GRIDS PROCESSING FINISHED?                   
         BO    MAINXGX                                                          
*                                                                               
         TM    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY?                       
         BZ    *+12                                                             
         TM    OVRSTAT,OVRREVQ     REVERSAL?                                    
         BZ    MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN30              NO                                           
*                                                                               
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN20              YES                                          
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     MAIN110                                                          
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BZ    MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERXIT                                                            
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
MAIN60   MVC   KEYSAVE,IOKEY       SEQ RESTORE                                  
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN70                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN70   LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING RECORD)              
         USING TRNRECD,R3                                                       
         CLC   TRNKCULA,IOKEYSAV                                                
         BNE   MAIN120                                                          
*                                                                               
MAIN90   LA    R3,IOKEY                                                         
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BH    MAIN94                                                           
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAIN60                                                           
         BRAS  RE,GETCTRN                                                       
         BNE   MAINX                                                            
         B     MAIN60                                                           
*                                                                               
MAIN94   BAS   RE,FILTKEY          APPLY FILTERING TO TRANS KEY                 
         BNE   MAIN60              DO WE WANT TO KEEP THIS RECORD?              
         MVC   DADDRESS,TRNKDA     DISK ADDRESS                                 
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN95                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN95   L     R3,AIO1             R3=A(IOAREA1 CONTAINING RECORD)              
         BAS   RE,FILTER           APPLY FILTERING TO TRANS RECORD              
         BNE   MAIN60              DO WE WANT TO KEEP THIS RECORD?              
         CLC   SAVEWC,SPACES                                                    
         BH    *+14                                                             
         MVC   SAVEWC,TRNKWORK                                                  
         B     MAIN96                                                           
         CLC   SAVEWC,TRNKWORK                                                  
         BE    MAIN100                                                          
         MVC   NEXTWC,TRNKWORK                                                  
         BAS   RE,WORKTOT          DEAL WITH WORK CODE TOTALS                   
         BNE   MAINX                                                            
*                                                                               
MAIN96   OI    JDTFLAG,JDTFWCQ     SET FIRST TRANSACTION FOR WC                 
**NOP    TM    PCDRIVEN,PCGRIDQ                                                 
*        BZ    MAIN100                                                          
*        BRAS  RE,GETWCN           GET THE WORKCODE NAME                        
*        BNE   ERXIT                                                            
*                                                                               
MAIN100  BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE(S)           
         MVC   KEYSAVE,TRNKEY      SEQ RESTORE                                  
         BNE   ERXIT                                                            
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN110                                                          
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN40                                                           
*                                                                               
MAIN110  GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN20                                                           
*                                                                               
MAIN120  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   MAIN130                                                          
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         TM    OVRSTAT,OVRREVQ     DOES ACCOUNT CONTAIN REVERSALS?              
         BNO   MAINX                                                            
         B     *+8                                                              
MAIN130  BAS   RE,WORKTOT          DEAL WITH WORK CODE TOTALS                   
         OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         B     MAINX                                                            
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),0                                 
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         ZAP   TRNTOT,=P'0'        TRANSACTIONS TOTAL                           
         ZAP   HRSTOT,=P'0'        HOURS TOTAL                                  
         ZAP   WTRNTOT,=P'0'       WORK CODE TRANS TOTAL                        
         ZAP   WHRSTOT,=P'0'       WORK CODE HOURS TOTAL                        
         ZAP   INTTOT,=P'0'        INTERNAL TOTAL                               
         ZAP   EXTTOT,=P'0'        EXTERNAL TOTAL                               
         MVC   CURCNTR,SPACES      CURRENT CONTRA NAME                          
         MVI   CURRFLAG,0          INIT CURRENCY FLAG                           
         MVC   CURRLAST,SPACES     LAST CURRENCY CODE                           
         MVI   JDTFLAG,0           INIT JOB DETAIL FLAG                         
         MVI   COMBVATR,EOTFF      INTIT COMBINED VAT RATE                      
         MVC   SAVEWC,SPACES       SAVED WC                                     
*                                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     RF,0(R1)                                                         
         USING FACTSD,RF           R1=A(SYSTEM DEFINITION BLOCK)                
         MVC   SECALPHA,FATAGYSC   SET SECURITY AGENCY ALPHA                    
         DROP  RF                                                               
*                                                                               
         LA    RF,PREVBTOT         PREV BILLING TOTALS                          
         LA    R0,PREVBNMQ         NUMBER OF PREV BILL ACCUMULATORS             
         ZAP   0(L'PREVBTOT,RF),=P'0'                                           
         LA    RF,L'PREVBTOT(RF)                                                
         BCT   R0,*-10                                                          
*                                                                               
         MVC   CONTRA,SPACES       CLEAR CONTRA CODE                            
         MVI   CONTLEN,0           INPUT CONTRA CODE LENGTH                     
         MVI   NEGCONT,0           NEGATIVE CONTRA FILTER                       
*                                                                               
         MVC   SVCACN,SPACES       AND CONTRA NAME                              
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         USING FLDHDRD,R2                                                       
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        ANYTHING INPUT?                              
         BZ    FSTD10                                                           
         LA    RE,FLDDATA                                                       
         CLI   0(RE),NEGFILTR    NEGATIVE FILTER?                               
         BNE   FSTD05                                                           
         MVI   NEGCONT,NEGFILTR                                                 
         LA    RE,1(RE)                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SH    RF,=H'1'                                                         
         BZ    FSTDERR             NO ACCOUNT ENTERED?                          
FSTD05   STC   RF,CONTLEN          LENGTH OF CONTRA CODE INPUT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA(0),0(RE)                                                  
         GOTO1 ACNAME              ATTEMPT TO FIND CONTRA NAME                  
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,SPROUL,ACOMFACS,(0,0)             
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN        R4=L'(KEY FIELD INPUT)                       
         BZ    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   FLDILEN,L'ACTKULA   ENSURE LENGTH NOT TOO LONG                   
         BH    FSTDERR                                                          
         GOTO1 AUNITLDG,SPROUNIT   READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    FSTDX                                                            
         MVC   FVMSGNO,=AL2(EAWRNGLV)                                           
         LA    RF,LEDGTLVA         RF=A(HIERARCHY LEVELS)                       
         LR    RE,RF               RE=A(HIERARCHY LEVELS)                       
         SR    R1,R1               R1=MINIMUM LENGTH OF ACCOUNT                 
         CLI   0(RF),X'0C'         FULL HIERARCHY?                              
         BE    *+12                                                             
         LA    RF,1(RF)            NOPE BUMP RF                                 
         B     *-12                                                             
         CR    RF,RE               YEP IS IT JUST ONE LEVEL?                    
         BE    *+10                                                             
         BCTR  RF,0                NO THEN GET MIN LENGTH                       
         IC    R1,0(RF)                                                         
         LR    RE,R4               RE=L'(INPUT)                                 
         CR    RE,R1               IS INPUT LONG ENOUGH?                        
         BNH   FSTDERR             NOPE, WRONG LEVEL ACCOUNT                    
         BCTR  R4,0                                                             
         LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
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
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     FSTDX                                                            
         TM    IOERR,IOERNF                                                     
         BO    FSTDERR             LOW LEVEL ACCOUNT NOT FOUND                  
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
         LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    FSTD23                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
         B     FSTD25                                                           
*                                                                               
FSTD23   GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC                                         
         LA    R1,FLDDATA+L'MX@ACC-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
FSTD25   L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
         MVC   ACCNAM,SPACES                                                    
FSTD30   CLI   0(RF),EOR           END OF RECORD?                               
         BE    FSTD48                                                           
         CLI   0(RF),NAMELQ        NAME ELEMENT?                                
         BE    FSTD40                                                           
         CLI   0(RF),JOBELQ        JOB ELEMENT?                                 
         BE    FSTD45                                                           
*                                                                               
FSTD35   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD30                                                           
*                                                                               
         USING NAMELD,RF                                                        
FSTD40   SR    RE,RE               GET ACCOUNT NAME                             
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAM(0),NAMEREC  ACCOUNT NAME                                  
         LA    RE,1(RE)                                                         
         STC   RE,ACCNAMLN                                                      
         B     FSTD35                                                           
*                                                                               
         USING JOBELD,RF           JOB ELEMENT                                  
FSTD45   MVC   JOBSTAT,JOBSTA1     JOB STATUS                                   
         B     FSTD35                                                           
*                                                                               
FSTD48   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD50                                                           
         MVC   2(L'ACCNAM,R1),ACCNAM                                            
         OI    FLDOIND,FOUTTRN                                                  
         SR    RE,RE                                                            
         IC    RE,ACCNAMLN                                                      
         LA    R1,3(RE,R1)                                                      
*                                                                               
         TM    JOBSTAT,JOBSXJOB                                                 
         BNO   FSTD49                                                           
         MVI   0(R1),C'/'                                                       
         MVC   2(L'MX@XJOB,R1),MX@XJOB                                          
         LA    R1,L'MX@XJOB+1(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
*                                                                               
FSTD49   CLC   SVCACN,SPACES       HAVE WE SPECIFIED FILTER CONTRA?             
         BE    FSTD50                                                           
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
         CHI   RE,L'SVCACN-1                                                    
         BNH   *+8                                                              
         LA    RE,L'SVCACN-1                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVCACN    DISPLAY AS MUCH OF NAME AS POSSIBLE            
         DROP  RF                                                               
*                                                                               
FSTD50   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD65                                                           
         LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         MVC   FLDDATA(L'MX@ENH16),MX@ENH16                                     
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         LA    R3,FLDDATA                                                       
         USING SCRLIN1D,R3                                                      
         MVC   SCR1USED,SCR1USED+1       MAKE ROOM FOR WIDER AMT COL            
         MVI   SCR1USED+1,C' '                                                  
         CLI   OVALUE,C'L'               LOCAL CURRENCY?                        
         BNE   FSTD51                                                           
         MVC   SCR1FUSD,SCR1USED         MAKE ROOM FOR WIDER AMT COL            
         MVI   SCR1USED,C' '                                                    
         B     FSTD52                                                           
FSTD51   CLI   OVALUE,C'B'               BOTH LOCAL AND AGENCY CURR?            
         BNE   FSTD52                                                           
         OI    JDTFLAG,JDTCOLMQ          SET COLUMN OVERRIDE                    
         MVC   SCR1DESC,SPACES                                                  
         MVC   SCR1DESC(L'MX@LCLCU),MX@LCLCU LOCAL CURRENCY AMOUNT              
         LA    R1,L'MX@LCLCU-1                                                  
         LA    RE,SCR1FDSC                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
*                                                                               
FSTD52   OI    FLDATB,FATBHIGH                                                  
         OI    FLDOIND,FOUTTRN                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         LA    R3,ENQDAT2H-ENQDAT1H(R3)                                         
         MVC   FLDDATA(L'MX@ENH17),MX@ENH17                                     
         MVC   SCR1USED,SCR1USED+1       MAKE ROOM FOR WIDER AMT COL            
         MVI   SCR1USED+1,C' '                                                  
         CLI   OVALUE,C'L'               LOCAL CURRENCY?                        
         BNE   FSTD55                                                           
         MVC   SCR1FUSD,SCR1USED         MAKE ROOM FOR WIDER AMT COL            
         MVI   SCR1USED,C' '                                                    
         B     FSTD60                                                           
FSTD55   CLI   OVALUE,C'B'               BOTH LOCAL AND AGENCY CURR?            
         BNE   FSTD60                                                           
         MVC   SCR1DESC,SPACES                                                  
         LA    RE,ENQDAT2H-ENQDAT1H(RE)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),UNDERLIN                                                 
*                                                                               
FSTD60   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         DROP  R3,RF                                                            
*                                                                               
FSTD65   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD66                                                           
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD67                                                           
*                                                                               
FSTD66   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD67   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
         DROP  R2                                                               
*                                                                               
         LA    R3,IOKEY            READ FIRST CONTRA RECORD                     
         USING TRNRECD,R3                                                       
         MVC   TRNKCCPY,MYCO                                                    
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
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
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     FSTDX                                                            
*                                                                               
FSTD80   LA    R3,IOKEY            R3=A(RECORD IO IOAREA 1)                     
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
         OI    DISPFLAG,NORECQ     NO RECORDS ON JOB                            
         B     FSTDX                                                            
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   FSTDX                                                            
         CLI   OALLTRAN,C'Y'       SHOW ALL TRANSACTIONS?                       
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
         BNE   FSTD110                                                          
         CLI   PREVS,C'Y'          PROFILE TO SHOW REVERSALS                    
         BNE   *+8                                                              
         MVI   OREVERSE,C'Y'                                                    
*                                                                               
FSTD110  CLI   ODETAIL,0           IF NO OPTION OVERRIDE USE PROFILE            
         BNE   FSTDX                                                            
         MVC   ODETAIL,PDETAIL                                                  
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
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
         TM    TRNKSTAT,TRNSREVS   REVERSAL?                                    
         BNO   *+8                                                              
         OI    OVRSTAT,OVRREVQ     ACCOUNT HAS REVERSED TRANSACTIONS            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,CONTLEN        RE=L'(CONTRA LEN)                            
         BZ    FILTK07                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),CONTRA   MATCHED ON CONTRA?                           
         BNE   FILTK06                                                          
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    FILTKRJX                                                         
         B     FILTK07                                                          
FILTK06  CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BNE   FILTKRJX                                                         
*                                                                               
FILTK07  L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         OC    OWCODE,OWCODE       FILTERING ON WORKCODE?                       
         BZ    FILTK20                                                          
         CLC   OWCODEVL,TRNKWORK                                                
         BE    FILTK10                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILTKRJX                                                         
         B     FILTK20                                                          
FILTK10  CLI   OWCODEFI,NEGFILTR                                                
         BE    FILTKRJX                                                         
*                                                                               
FILTK20  CLC   TRNKWORK,ORDER      ORDER RECORD?                                
         BE    FILTK50                                                          
         TM    TRNKSTAT,TRNSDRFT   DRAFT?                                       
         BNO   FILTK50                                                          
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK60                                                          
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK60                                                          
         B     FILTREJX                                                         
FILTK50  CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
         TM    TRNKSTA2,TRNSPEEL   PEELED?                                      
         BNO   FILTK55                                                          
         CLI   OPEELED,C'Y'                                                     
         BE    FILTK55A                                                         
         CLI   OPEELED,C'O'                                                     
         BE    FILTK55A                                                         
         B     FILTREJX                                                         
FILTK55  CLI   OPEELED,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
FILTK55A TM    TRNKSTA2,TRNSEXCL   CONTRA'D?                                    
         BNO   FILTK56                                                          
         CLI   OCONT,C'Y'                                                       
         BE    FILTK60                                                          
         CLI   OCONT,C'O'                                                       
         BE    FILTK60                                                          
         B     FILTREJX                                                         
FILTK56  CLI   OCONT,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILTK60  OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILTK70                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK70  CLC   TRNKWORK,ORDER      ORDER?                                       
         BNE   FILTK80                                                          
         OC    OORDNO,OORDNO       ORDER FILTER?                                
         BZ    FILTK80                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),C        
               OORDFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK80  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK90                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK90  OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR   RB,RB                                                            
         B     XIT                                                              
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
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         L     R3,AIO1             R3=A(ACCMST RECORD)                          
         USING TRNRECD,R3                                                       
*                                                                               
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         USING TRNELD,R4                                                        
         ICM   R4,15,ATRNELD       R4=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRANCURR,COMPCURR   SET TRANSACTION CURRENCY FROM AGENCY         
         ZAP   TRANAMNT,TRNAMNT    AGENCY TRANSACTION AMOUNT                    
*                                                                               
         TM    TRNSTAT,TRNSAUTH    AUTHORISED?                                  
         BNO   FILT130                                                          
         CLI   OAUTH,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT130  CLI   OAUTH,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT150  TM    TRNSTAT,TRNSDR      DEBIT/CREDIT?                                
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
FILT165  TM    TRNSTAT,TRNSHOLD    HELD?                                        
         BNO   FILT170                                                          
         CLI   OHELD,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT170  CLI   OHELD,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
         TM    TRNSTAT,TRNSURG     URGENT TRANSACTION?                          
         BNO   FILT175                                                          
         CLI   OURGENT,C'N'                                                     
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT175  CLI   OURGENT,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
         OC    OBATCH,OBATCH       BATCH REF FILTER?                            
         BZ    FILT180                                                          
         GOTO1 ASCOMP,DMCB,TRNBTCH,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT180  SR    RF,RF               INPUT TYPE FILTER?                           
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
         CP    TRNAMNT,=P'0'               AND ZERO DOLLAR                      
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
FILT220  CLC   TRNKWORK,ORDER      ORDER TRAN?                                  
         BE    FILT235                                                          
         OC    OORDNO,OORDNO                                                    
         BZ    FILT235                                                          
         OC    AFFNELD,AFFNELD     ORDER NUMBER REQUIRED                        
         BZ    FILTREJX                                                         
*                                                                               
FILT235  OC    OSUBREF,OSUBREF     FILTERING ON SUB REF?                        
         BZ    *+14                                                             
         OC    AOTHELD,AOTHELD     OTHERS ELEMENT REQD                          
         BZ    FILTREJX                                                         
*                                                                               
         OC    OTIME,OTIME         FILTERING ON TIME?                           
         BZ    *+14                                                             
         OC    APRTELD,APRTELD     PERSONNEL RATE REQUIRED FLAG                 
         BZ    FILTREJX                                                         
*                                                                               
         USING TRSELD,R4           TRANSACTION STATUS ELEMENT                   
         ICM   R4,15,ATRSELD                                                    
         BZ    FILT280                                                          
         OC    OACT,OACT          FILTERING ON ACTIVITY DATE?                   
         BZ    FILT250                                                          
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT250  OC    OUSED,OUSED        FILTERING ON USED DATE?                       
         BZ    FILT260                                                          
         OC    TRSUDAT,TRSUDAT                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPDAT) USED DATE                   
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OUSEST,OUSEEN,OUSEFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT260  CLI   OTTYPE,0           TIME TYPE FILTER?                             
         BE    FILT270                                                          
         CLI   OTTYPE,C'T'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTIME  TIME SHEET REGULAR? (US COSTING)              
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'M'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTMSS  TIME SHEET MISSING? (US COSTING)              
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'A'                                                      
         BNE   FILT270                                                          
         TM    TRSSTAT2,TRSSTADJ  TIME SHEET ADJUSTED? (US COSTING)             
         BNO   FILTREJX                                                         
*                                                                               
FILT270  TM    TRNRSTAT,TRNSREVS   REVERSAL?                                    
         BNO   FILT275                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT280                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT280                                                          
         OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS?                
         BZ    FILTREJX                                                         
         OC    TRSRMOS,TRSRMOS     IN NO REV MOS ASSUME SAME AS TRAN            
         BZ    FILTREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTREJX                                                         
         B     FILT280                                                          
FILT275  CLI   OREVERSE,C'O'                                                    
         BE    FILTREJX                                                         
         B     FILT280                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4           OTHERS ELEMENT                               
FILT280  ICM   R4,15,AOTHELD                                                    
         BZ    FILT285                                                          
         OC    OSUBREF,OSUBREF     FILTERING ON SUB REFERENCE                   
         BZ    FILT285                                                          
         GOTO1 ASCOMP,DMCB,OTHNUM,(OSUBRLN1,OSUBRVL1),(OSUBRLN2,OSUBRVLC        
               2),OSUBRFI                                                       
         BNE   FILTREJX                                                         
*                                                                               
         USING PRTELD,R4           PERSONNEL RATE ELEMENT                       
FILT285  ICM   R4,15,APRTELD                                                    
         BZ    FILT290                                                          
         CLI   OTIME,0             TIME FILTER?                                 
         BE    FILT290                                                          
*                                                                               
         CLI   OTIME,C'B'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME (US COSTING)                   
         BNO   FILTREJX                                                         
         CLI   OTIME,C'N'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME (US COSTING)               
         BNO   FILTREJX                                                         
         CLI   OTIME,C'R'                                                       
         BNE   FILT290                                                          
         TM    PRTSTAT,PRTSRTEQ    SPECIAL NON-BILLABLE TIME (US COST)          
         BNO   FILTREJX                                                         
         B     FILT290                                                          
*                                                                               
         USING FFNELD,R4                                                        
FILT290  ICM   R4,15,AFFNELD                                                    
         BZ    FILT295                                                          
         OC    OORDNO,OORDNO      ORD NUMBER FILTER?                            
         BZ    FILT295                                                          
         MVC   ORD,SPACES                                                       
         SR    RF,RF                                                            
         IC    RF,FFNLN                                                         
         SHI   RF,FFNLN1Q+1    ANYTHING ON IT?                                  
         BM    FILTREJX                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ORD(0),FFNONUM                                                   
         GOTO1 ASCOMP,DMCB,ORD,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),OORDC        
               FI                                                               
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
         USING AFCELD,R4           ACCOUNT FOREIGN CURRENCY ELEMENT             
FILT295  ICM   R4,15,AAFCELD                                                    
         BZ    FILT297                                                          
         TM    AFCXSTAT,AFCXSMEM   MEMO ITEM?                                   
         BO    FILT295A                                                         
         CLI   OMEMOAFC,C'O'       ONLY REQUIRE MEMO ITEMS?                     
         BE    FILTREJX                                                         
         B     FILT296                                                          
*                                                                               
FILT295A CLI   OMEMOAFC,C'Y'                                                    
         BE    FILT296                                                          
         CLI   OMEMOAFC,C'O'                                                    
         BNE   FILT300                                                          
*                                                                               
FILT296  MVC   TRANCURR,AFCCURR    OVERRIDE TRANSACTION CURRENCY                
         CLI   OVALUE,C'L'         SHOW LOCAL CURRENCY                          
         BNE   FILT300                                                          
         ZAP   TRANAMNT,AFCAMNT    LOCAL TRANSACTION AMOUNT                     
         B     FILT300                                                          
*                                                                               
FILT297  CLI   OMEMOAFC,C'O'       MEMO ITEMS ONLY?                             
         BE    FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
FILT300  TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BO    FILT370                                                          
         OC    OAMOUNT,OAMOUNT     FILTERING TRANSACTION AMOUNT?                
         BZ    FILT350                                                          
         CP    OAMTVL,TRANAMNT     FILTER AMOUNT=TRANS AMOUNT?                  
         BE    FILT340                                                          
         CLI   OAMTSIGN,0          HAS A '+' OR '-' BEEN SPECIFIED?             
         BNE   FILT310                                                          
         CLI   OAMTRNG,0           HAS A '>' OR '<' BEEN SPECIFIED?             
         BNE   FILT310                                                          
         ZAP   TEMPNUM,OAMTVL      SEE IF AMOUNT IS -VE EQUIVALENT              
         AP    TEMPNUM,TRANAMNT                                                 
         BZ    FILT340             YES IT IS                                    
         B     FILT330                                                          
FILT310  CLI   OAMTRNG,C'>'        MORE THAN SIGN?                              
         BNE   FILT320                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT340                                                          
         B     FILT330                                                          
FILT320  CLI   OAMTRNG,C'<'        LESS THAN SIGN?                              
         BNE   FILT330                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT340                                                          
FILT330  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BNE   FILTREJX                                                         
         B     FILT350                                                          
FILT340  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BE    FILTREJX                                                         
*                                                                               
FILT350  CP    TRANAMNT,=P'0'                                                   
         BNL   FILT360                                                          
         CLI   ONEGATIV,C'N'       DO WE ONLY WANT +VE NUMBERS/ZERO?            
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT360  CLI   ONEGATIV,C'O'       DO WE ONLY WANT -VE NUMBERS?                 
         BE    FILTREJX                                                         
*                                                                               
         OC    OCURRYVL,OCURRYVL   CURRENCY CODE FILTER?                        
         BZ    FILT370                                                          
         CLC   TRANCURR,OCURRYVL   MATCH ON CURRENCY CODE?                      
         BNE   FILT365                                                          
         CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BE    FILTREJX                                                         
         B     FILT370                                                          
FILT365  CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BNE   FILTREJX                                                         
*                                                                               
FILT370  CLC   TRNKWORK,ORDER     ORDER TRANSACTION?                            
         BNE   FILT380                                                          
*        BNE   FILTX                                                            
         OC    OACT,OACT          FILTERING ON ACTIVITY DATE?                   
         BNZ   FILTREJX                                                         
         OC    OUSED,OUSED        FILTERING ON USED DATE?                       
         BNZ   FILTREJX                                                         
         CLI   OREVERSE,C'O'      ONLY REVERSALS?                               
         BE    FILTREJX                                                         
*                                                                               
FILT380  DS    0H                                                               
*-----------------------------------                                            
* DUE DATE ELEMENT X'61'                                                        
*-----------------------------------                                            
*        USING DUEELD,R4                                                        
*        OC    ODUE,ODUE           FILTERING ON DUE DATE?                       
*        BZ    FILT385              NO                                          
*        ICM   R4,15,ADUEELD                                                    
*        BZ    FILT382                                                          
*        GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,TEMPDAT) DUE DATE                    
*        GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),ODUEST,ODUEEN,ODUEFI             
*        BNE   FILTREJX                                                         
*        B     FILT385                                                          
*ILT382  GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODUEST,ODUEEN,ODUEFI           
*        BNE   FILTREJX                                                         
*        DROP  R4                                                               
*                                                                               
*ILT385  DS    0H                                                               
         OC    OSERIAL,OSERIAL      LONG INVOICE # FILTER                       
         BZ    FILTX                                                            
         XC    WORK,WORK                                                        
*        -----------------------------------                                    
*        PRODUCTION LONG INVOICE NUMBER                                         
*        -----------------------------------                                    
         USING FFTELD,R4                                                        
         ICM   R4,15,AFFTELD        RE=A(FREE FORM TEXT ELEMENT)                
         BZ    FILT410                                                          
FILT390  CLI   FFTEL,0                                                          
         BE    FILT410                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTINVN    LONG INVOICE NUMBER                          
         BE    FILT400                                                          
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     FILT390                                                          
*                                                                               
FILT400  SR    RF,RF                DATA LENGTH                                 
         ICM   RF,1,FFTDLEN         DATA LENGTH                                 
         BNP   FILT410                                                          
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FFTDATA                                                  
         B     FILT420                                                          
FILT410  MVC   WORK(L'TRNKREF),TRNKREF                                          
*                                                                               
FILT420  GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   +        
               OSERFI                                                           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
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
*                                                                               
         USING TRNRECD,R3                                                       
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         MVI   INTFLAG,0                                                        
         MVC   TRANCURR,COMPCURR                                                
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         XC    TSDACDAT,TSDACDAT   ACTIVITY DATE                                
         XC    TSDUSDAT,TSDUSDAT   USED DATE                                    
         XC    TSDPEDAT,TSDPEDAT   PEELED DATE                                  
         XC    TSDUSER,TSDUSER     HEX USER-ID                                  
         MVC   TSDUSRID,SPACES     USER-ID                                      
         XC    TSDXPID,TSDXPID     HEX PID                                      
         MVC   TSDPIDN,SPACES      PERSON                                       
         ZAP   TSDTAMNT,=P'0'                                                   
         ZAP   TSDPAMNT,=P'0'      PAYMENT AMOUNT                               
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
         BNE   BLDT02                                                           
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   BLDT05              NO, SKIP                                     
         MVI   TSDALLOC,TSDALLBI   MARK AS BILLED                               
         OC    TRNUNBIL,TRNUNBIL   IS THERE AN UNBILL DATE?                     
         BZ    BLDT05              NO                                           
         MVI   TSDALLOC,TSDALLUN   YES, CHANGE TO UNBILLED                      
         B     BLDT05                                                           
*                                                                               
BLDT02   CLI   TSDALLOC,C' '                                                    
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
         BNO   *+12                                                             
         MVI   TSDALLOC,TSDALLPB   SET PART BILLED                              
         B     BLDT05                                                           
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   BLDT05              NO, SKIP                                     
         CP    PP$AALLO,=P'0'      ANYTHING ALLOCATED?                          
         BE    BLDT05                                                           
         MVI   TSDALLOC,TSDALLPA                                                
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
         CLI   0(R3),AFCELQ        ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BE    BLDT135                                                          
         CLI   0(R3),APEELQ        ANALYSIS POINTER ELEMENT?                    
         BE    BLDT160                                                          
         CLI   0(R3),FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    BLDT170                                                          
*        CLI   0(R3),DUEELQ        FREE FORM TEXT ELEMENT?                      
*        BE    BLDT165                                                          
         CLI   0(R3),PBIELQ        PST BILLED ELEMENT?                          
         BE    BLDT175                                                          
         CLI   0(R3),PTAELQ        77 BILLING ELEMENT                           
         BE    BLDT176                                                          
         CLI   0(R3),PIDELQ        D8 PERSON ID ELEMENT                         
         BE    BLDT177                                                          
         B     BLDT10                                                           
*                                                                               
         USING OTHELD,R3           OTHERS ELEMENT                               
BLDT30   L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   OXDETAIL-OPTVALSD(RF),C'Y'                                       
         BE    *+12                                                             
         TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,OTHLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING PRTELD,R3           PERSONNEL RATE ELEMENT                       
BLDT35   TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,PRTLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING FFNELD,R3           FREE FORM NUMBER ELEMENT                     
BLDT40   TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,FFNLN                                                         
         CHI   RF,FFNLN1Q      ANYTHING ON IT?                                  
         BE    BLDT10                                                           
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING RATELD,R3           GENERAL RATE ELEMENT                         
BLDT50   TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,RATLN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING PXDELD,R3           POSTING XFER DETAIL ELEMENT                  
BLDT60   TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,PXDLN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING VBIELD,R3           VAT BILLED ELEMENT                           
BLDT80   TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
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
         CLI   OXDETAIL-OPTVALSD(RF),C'Y'                                       
         BE    *+12                                                             
         TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         CLI   SCITYPE,SCITSJXP    AMOUNT POSTED TO EXPENSES?                   
         BNE   *+14                                                             
         AP    TSDTAMNT,SCIAMNT    EXPENSE AMOUNT                               
         B     BLDT10                                                           
         CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BNE   BLDT100                                                          
         L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   OVALUE-OPTVALSD(RF),C'B'                                         
         BE    BLDT110                                                          
         AP    WHRSTOT,SCIAMNT                                                  
         B     BLDT110                                                          
BLDT100  CLI   SCITYPE,SCITFORD    FULLY MATCHED ORDER?                         
         BE    BLDT110                                                          
         CLI   SCITYPE,SCITPORD    PART MATCHED ORDER?                          
         BE    BLDT110                                                          
         CLI   SCITYPE,SCITMILE    MILEAGE                                      
         BE    BLDT110                                                          
         CLI   SCITYPE,SCITCPAD    JOB TRX & REB CREDITOR PAY DETAILS           
         BNE   BLDT101                                                          
         ZAP   TSDPAMNT,SCIAMNT                                                 
         MVC   TSDPYREF,SCITCPNO                                                
         MVC   TSDPYDAT,SCITCPDT                                                
         J     BLDT110                                                          
BLDT101  L     RF,AIO1             RF=A(TRANSACTION RECORD)                     
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
BLDT115  TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,TPRLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING TRSELD,R3           TRANSACTION STATUS ELEMENT                   
BLDT120  MVC   TSDACDAT,TRSDATE    ACTIVITY DATE                                
         MVC   TSDUSDAT,TRSUDAT    USED DATE                                    
         MVC   TSDPEDAT,TRSPDAT    PEELED DATE                                  
         MVC   TSDSTAT2,TRSSTAT2   STATUS BYTE 2                                
         MVC   TSDSTAT3,TRSSTAT3   STATUS BYTE 3 AND                            
         MVC   TSDUSER,TRSUSER     USER ID FOR 99 ONLY                          
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
         USING AFCELD,R3           ACCOUNT FOREIGN CURRENCY ELEMENT             
BLDT135  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         TM    AFCXSTAT,AFCXSMEM   MEMO ITEM?                                   
         BNO   BLDT140                                                          
         CLI   OMEMOAFC,C'Y'                                                    
         BE    BLDT140                                                          
         CLI   OMEMOAFC,C'O'                                                    
         BNE   BLDT148                                                          
BLDT140  CLI   OVALUE,C'L'         SHOWING LOCAL COLUMN ONLY?                   
         BNE   *+14                                                             
         ZAP   TRANAMNT,AFCAMNT    LOCAL CURRENCY AMOUNT                        
         B     BLDT145                                                          
         CLI   OVALUE,C'B'         SHOW XTRA DETAIL?                            
         BNE   BLDT148                                                          
         TM    TSDTEST,TRNSDR      DEBIT?                                       
         BNO   *+14                                                             
         AP    WHRSTOT,AFCAMNT     LOCAL CURRENCY TOTAL                         
         B     BLDT145                                                          
         SP    WHRSTOT,AFCAMNT                                                  
BLDT145  MVC   TRANCURR,AFCCURR    GET LOCAL CURRENCY                           
         B     BLDT150                                                          
BLDT148  CLI   OXDETAIL,C'Y'       SHOW XTRA DETAIL?                            
         BNE   BLDT10                                                           
         DROP  RF                                                               
BLDT150  SR    RF,RF                                                            
         IC    RF,AFCLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING APEELD,R3           ANALYSIS POINTER ELEMENT                     
BLDT160  TM    PCDRIVEN,PCGRIDQ                                                 
         BO    BLDT162                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OATTRIB-OPTVALSD(RF),C'Y' ATTRIBUTE OPTION?                      
         BNE   BLDT10                                                           
BLDT162  SR    RF,RF                                                            
         IC    RF,APELN                                                         
         B     BLDT200                                                          
*                                                                               
*        USING DUEELD,R3                                                        
*LDT165  SR    RF,RF               DUE DATE                                     
*        IC    RF,DUELN                                                         
*        B     BLDT200                                                          
*                                                                               
         USING FFTELD,R3           FREE FORM TEXT ELEMENT                       
BLDT170  CLI   FFTTYPE,FFTTPNAM    PAYEE NAME?                                  
         BE    BLDT172                                                          
         CLI   FFTTYPE,FFTTINVN    INVOICE NUMBER?                              
         BNE   BLDT10                                                           
BLDT172  SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         B     BLDT200                                                          
*                                                                               
         USING PBIELD,R3           PST BILLED ELEMENT                           
BLDT175  TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,PBILN                                                         
         B     BLDT200             ADD ELEMENT TO TSAR RECORD                   
*                                                                               
         USING PTAELD,R3                                                        
BLDT176  CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   BLDT10              NO, SKIP                                     
         CLI   TSDALLOC,TSDALLBI   IS THIS BILLING?                             
         BNE   BLDT10              NO                                           
         TM    PTASTAT1,PTASREVS   IS THIS A REVERSAL?                          
         BZ    BLDT10              NO                                           
         MVI   TSDALLOC,TSDALLRB   YES, CHANGE TO REVERSAL                      
         B     BLDT10                                                           
*                                                                               
         USING PIDELD,R3                                                        
BLDT177  MVC   TSDXPID,PIDNO       SAVE THE PID NUMBER                          
         B     BLDT10                                                           
*----------------------------------------------------------------------         
* MOVE WHOLE OF ELEMENT ONTO TSAR REC                                           
*----------------------------------------------------------------------         
BLDT200  BCTR  RF,0                MOVE WHOLE OF ELEMENT ONTO TSAR REC          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     BLDT10                                                           
         DROP  R3                                                               
*----------------------------------------------------------------------         
*                                                                               
BLDT210  OC    TSDUSER,TSDUSER                                                  
         BZ    BLDT216                                                          
         MVC   KEYSAVE2,IOKEY                                                   
         LA    RF,IOKEY            GET THE ID RECORD                            
         USING CTIREC,RF                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    IDI RECORD "I"                               
         MVC   CTIKNUM,TSDUSER                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',IOKEY,AIO2                 
         BNE   BLDT216                                                          
         L     RF,AIO2                                                          
         CLC   IOKEY(L'CTIKEY),0(RF)                                            
         BNE   BLDT216                    NOT FOUND                             
*                                                                               
         L     RF,AIO2                                                          
         LA    RF,CTIDATA          R2=FIRST ELEMENT                             
         SR    R1,R1                                                            
BLDT212  CLI   0(RF),0             END OF RECORD?                               
         BE    BLDT216                                                          
         CLI   0(RF),CTDSCELQ      ID ELEMENT                                   
         BE    BLDT214                                                          
*                                                                               
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     BLDT212                                                          
*                                                                               
         USING CTDSCD,RF                                                        
BLDT214  MVC   TSDUSRID,CTDSC                                                   
*                                                                               
         MVC   IOKEY,KEYSAVE2      REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    BLDT216                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
*                                                                               
BLDT216  OC    TSDXPID,TSDXPID                                                  
         BZ    BLDT240                                                          
         CLI   TWAOFFC,C'*'                                                     
         BE    BLDT218                                                          
         CLC   TSDXPID,=X'1000'                                                 
         BNL   BLDT218                                                          
         MVC   TSDPIDN(3),=C'DDS'                                               
         B     BLDT240                                                          
*                                                                               
BLDT218  MVC   KEYSAVE2,IOKEY                                                   
         USING SA0REC,RF                                                        
         LA    RF,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    SA0KAGY,SECALPHA    USE SECURITY AGENCY IF PRESENT               
         BNZ   *+10                                                             
         MVC   SA0KAGY,TWAAGY      ELSE NATIVE AGENCY                           
         MVC   SA0KNUM,TSDXPID                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IOKEY,AIO2                   
         BNE   BLDT240             CAN'T FIND USERID RECORD                     
*                                                                               
         L     RF,AIO2                                                          
         LA    RF,SA0DATA                                                       
         XR    R0,R0                                                            
*                                                                               
         USING SAPALD,RF                                                        
BLDT220  CLI   SAPALEL,SAPALELQ                                                 
         BE    BLDT222                                                          
         CLI   SAPALEL,0                                                        
         BE    BLDT240                                                          
         IC    R0,SAPALLN                                                       
         AR    RF,R0                                                            
         B     BLDT220                                                          
*                                                                               
BLDT222  MVC   TSDPIDN,SAPALPID                                                 
         MVC   IOKEY,KEYSAVE2      REESTABLISH IO SEQUENCE                      
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    BLDT240                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
*                                                                               
BLDT240  CLC   CURRLAST,SPACES     CURRENCY NOT SET?                            
         BE    BLDT242                                                          
         CLC   CURRLAST,TRANCURR   MIXED CURRENCIES DISPLAYED?                  
         BE    BLDT244                                                          
         OI    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS                      
         TM    JDTFLAG,JDTFWCQ     FIRST TRANSACTION FOR WC?                    
         BO    BLDT242                                                          
         OI    CURRFLAG,CURRSSTQ   SUPPRESS SUB TOTS                            
*                                                                               
BLDT242  MVC   CURRLAST,TRANCURR                                                
*                                                                               
BLDT244  AP    TSDTAMNT,TRANAMNT  TRANSACTION AMOUNT LOCAL/AGENCY               
         TM    TSDTAMTY,TSDTMEMQ   MEMO EXPENSE                                 
         BO    BLDT248                                                          
         CLC   TSDWORK,ORDER                                                    
         BE    BLDT246                                                          
         TM    TSDTEST,TRNSDR                                                   
         BO    BLDT246                                                          
         SP    WTRNTOT,TSDTAMNT                                                 
         B     *+10                                                             
*                                                                               
BLDT246  AP    WTRNTOT,TSDTAMNT    TRANSACTIONS TOTAL                           
*                                                                               
BLDT248  TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BO    BLDT250                                                          
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNKEY(R3) R3=A(TRANSACTION ELEMNT)                   
         USING TRNELD,R3                                                        
         USING NARELD,R4                                                        
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SHI   RF,TRNLN1Q+1                                                     
         BM    BLDT250             NO NARRATIVE ON TRANSACTION                  
         MVI   NAREL,ELETSARQ      INTERNAL ELEMENT                             
         MVI   NARELTP,NARELQ      NARRATIVE ELEMENT                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NARREC(0),TRNNARR   GET NARRATIVE                                
         AHI   RF,NARLN1Q+1                                                     
         STC   RF,NARLN            ELEMENT LENGTH                               
         LA    R4,0(RF,R4)                                                      
*                                                                               
BLDT250  MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         CLI   TSDALLOC,C' '       DO WE HAVE AN ALLOCATION STATUS?             
         BH    BLDT260             YES                                          
         OC    TSDUSDAT,TSDUSDAT   USED DATE FILLED IT?                         
         BZ    *+8                 NO                                           
         MVI   TSDALLOC,TSDALLFB   YES, MUST BE FULLY BILLED                    
*                                                                               
BLDT260  LA    R4,1(R4)                                                         
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4               RF=L'(TSAR RECORD)                           
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+10                                                             
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
BLDTX    NI    JDTFLAG,X'FF'-JDTFWCQ SWITCH FIRST TRANS FOR WC OFF              
         CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R4                                                      
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FORM150                                                          
         CLI   TSARFMT,TSWRKITM    WORKCODE TOTAL ITEM?                         
         BE    FORM200                                                          
         CLI   TSARFMT,TSIXITM     INTERNAL EXTERNAL ITEM?                      
         BE    FORM360                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         GOTO1 ASETELE,TSDRFST     SET ELEMENT ADDRESSES                        
*                                                                               
FORM70   MVC   SCR1WORK,TSDWORK    WORK CODE                                    
         MVC   SCR1CONT,TSDCONT    CONTRA ACCOUNT                               
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(17,SCR1TDAT) TRAN DATE                 
         MVC   SCR1TREF,TSDTREF    TRANSACTION REFERENCE                        
         MVC   SCR1BREF,TSDBREF    BATCH REFERENCE                              
         LA    RF,SCR1USED         RF=A(DEFAULT USED COLUMN)                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY OVERRIDE?                
         BNE   *+8                                                              
         LA    RF,SCR1FUSD         RF=A(FOREIGN CURR FORMAT USED COL)           
         MVC   0(L'SCR1USED,RF),TSDALLOC                                        
*                                                                               
         TM    TSDTAMTY,TSDTORDQ+TSDTMEMQ ORDER OR MEMEO EXPENSE?               
         BZ    FORM100                                                          
         L     R1,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY DISPLAY?                 
         BE    FORM90                                                           
         PACK  TEMP(8),NINES(L'SCR1AMNT-4) GET STRING OF NINES                  
         CP    TEMP(8),TSDTAMNT IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORM80                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),TSDTAMNT IS AMOUNT TO LOW TO FIT?                        
         BH    FORM80                                                           
         CURED (P6,TSDTAMNT),(L'SCR1AMNT,SCR1AMNT),2,MINUS=YES,        X        
               BRACKET=YES                                                      
         B     FORM130                                                          
FORM80   CURED (P6,TSDTAMNT),(L'SCR1AMNT,SCR1AMNT),2,MINUS=YES,        X        
               BRACKET=YES,DECS=ROUND                                           
         B     FORM130                                                          
*                                                                               
FORM90   CURED (P6,TSDTAMNT),(L'SCR1FAMT,SCR1FAMT),2,MINUS=YES,        X        
               BRACKET=YES                                                      
         B     FORM130                                                          
*                                                                               
FORM100  L     R1,AOPTVALS         RF=A(OPTIONS BLOCK)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY DISPLAY?                 
         BNE   FORM110                                                          
         MVC   TRANCURR,COMPCURR                                                
         SR    R3,R3                                                            
         ICM   R3,15,AAFCELD       ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BZ    FORM105                                                          
         TM    AFCXSTAT-AFCELD(R3),AFCXSMEM MEMO ITEM?                          
         BNO   FORM104                                                          
         CLI   OMEMOAFC-OPTVALSD(RF),C'Y'                                       
         BE    FORM104                                                          
         CLI   OMEMOAFC-OPTVALSD(RF),C'O'                                       
         BNE   FORM105                                                          
FORM104  MVC   TRANCURR,AFCCURR-AFCELD(R3)                                      
FORM105  GOTO1 AGETCURR,DMCB,TRANCURR                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,TSDTAMNT),(L'SCR1FAMT,SCR1FAMT),(RF),MINUS=YES,     X        
               CURSYMB=YES                                                      
         B     FORM120                                                          
FORM110  CURED (P6,TSDTAMNT),(L'SCR1AMNT,SCR1AMNT),2,MINUS=YES                  
*                                                                               
FORM120  TM    TSDTEST,TRNSDR      DEBIT OR CREDIT?                             
         BO    *+14                                                             
         MVC   SCR1SIGN,MX@CR                                                   
         B     *+10                                                             
         MVC   SCR1SIGN,MX@DR                                                   
*                                                                               
FORM130  TM    JDTFLAG,JDTCOLMQ    COLUMN OVERRIDE?                             
         BNO   *+12                                                             
         BAS   RE,CLOVER           DISPLAY OVERRIDE COLUMN                      
         B     FORM140                                                          
         CLC   TSDWORK,PREVBILL                                                 
         BNE   *+12                                                             
         BAS   RE,PREVBLNS         PREV BILLED                                  
         B     *+12                                                             
         BAS   RE,STANDLNS         STANDARD DESCRIPTION LINES                   
         BAS   RE,STATLNS          STATUS LINES                                 
FORM140  CLI   LINSUSED,0          NOTHING IN DESCRIPTION FIELD?                
         BNE   *+12                                                             
         LA    R2,L'DUMLIN1(R2)                                                 
         MVI   LINSUSED,1                                                       
         BAS   RE,ATTRLNS          ATTRIBUTE LINES                              
         BAS   RE,XDETLNS          EXTRA DETAIL LINES                           
         B     FORMX                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM150  LA    R2,L'DUMLIN1(R2)                                                 
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@ACCT                                                 
         CP    TSTHTOT,=P'0'                                                    
         BE    *+10                                                             
         MVC   SCRTOTAL,MX@ACCTS                                                
         L     R1,AOPTVALS         R1=A(OPTION BLOCK)                           
         CLI   OVALUE-OPTVALSD(R1),C'L' SHOW LOCAL CURRENCY?                    
         BNE   FORM160                                                          
         GOTO1 AGETCURR,DMCB,CURRLAST                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P8,TSTTTOT),(L'SCRTFTO2,SCRTFTO2),(RF),MINUS=YES,      X        
               CURSYMB=YES                                                      
         B     FORM170                                                          
FORM160  CURED (P8,TSTTTOT),(L'SCRTTTOT,SCRTTTOT),2,MINUS=YES                   
*                                                                               
FORM170  L     R1,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(R1),C'B' LOCAL CURRENCY DISPLAY?                 
         BNE   FORM180                                                          
         GOTO1 AGETCURR,DMCB,CURRLAST                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P8,TSTHTOT),(L'SCRTHTOT,SCRTHTOT),(RF),MINUS=YES,      X        
               CURSYMB=YES                                                      
         B     FORM190                                                          
*                                                                               
FORM180  CP    TSTHTOT,=P'0'                                                    
         BE    FORM190                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@HOURS),MX@HOURS                                        
         LA    RF,TEMP+L'MX@HOURS-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P8,TSTHTOT),(15,(RF)),2,MINUS=YES,ALIGN=LEFT                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
FORM190  MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTE                            
         B     FORMX                                                            
*                                                                               
         USING SCRWORKD,R2         DSECT FOR WORKCODE TOTAL LINE                
FORM200  LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARWRKD,R4                                                      
         MVC   SCRWWORK,TSWWORK     WORK CODE                                   
         MVC   SCRWDESC,TSWDESC     WORKCODE DESCRIPTION                        
         MVC   SCRWWTOT,MX@WCTOT    WORKCODE TOTAL                              
FORM210  CLC   TSWWORK,ORDER                                                    
         BNE   FORM250                                                          
         L     R1,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY DISPLAY?                 
         BE    FORM220                                                          
         PACK  TEMP(16),NINES(L'SCRWAMNT-4) GET STRING OF NINES                 
         LA    RF,L'SCRWAMNT                                                    
         LA    R3,SCRWAMNT                                                      
         B     FORM230                                                          
FORM220  PACK  TEMP(16),NINES(L'SCRWFAMT-4) GET STRING OF NINES                 
         LA    RF,L'SCRWFAMT                                                    
         LA    R3,SCRWFAMT                                                      
*                                                                               
FORM230  CP    TEMP(16),TSWAMNT IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORM240                                                          
         MP    TEMP(16),=P'-1'                                                  
         CP    TEMP(16),TSWAMNT IS AMOUNT TO LOW TO FIT?                        
         BH    FORM240                                                          
         CURED (P6,TSWAMNT),((RF),(R3)),2,MINUS=YES,BRACKET=YES                 
         B     FORM270                                                          
FORM240  CURED (P6,TSWAMNT),((RF),(R3)),2,MINUS=YES,BRACKET=YES,       X        
               DECS=ROUND                                                       
         B     FORM270                                                          
*                                                                               
FORM250  L     R1,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY DISPLAY?                 
         BNE   FORM260                                                          
         GOTO1 AGETCURR,DMCB,CURRLAST                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,TSWAMNT),(L'SCRWFAMT,SCRWFAMT),(RF),MINUS=YES,      X        
               CURSYMB=YES                                                      
         B     FORM270                                                          
*                                                                               
FORM260  CURED (P6,TSWAMNT),(L'SCRWAMNT,SCRWAMNT),2,MINUS=YES                   
*                                                                               
FORM270  L     R1,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(R1),C'B' LOCAL CURRENCY DISPLAY?                 
         BNE   FORM280                                                          
         GOTO1 AGETCURR,DMCB,CURRLAST                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,TSWHOURS),(L'SCRWINFO,SCRWINFO),(RF),MINUS=YES,     X        
               CURSYMB=YES                                                      
         B     FORM350                                                          
*                                                                               
FORM280  CP    TSWHOURS,=P'0'                                                   
         BE    FORM290                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@HOURS),MX@HOURS                                        
         LA    RF,TEMP+L'MX@HOURS-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,TSWHOURS),(13,(RF)),2,MINUS=YES,ALIGN=LEFT                   
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
FORM290  CLC   TSWWORK,PREVBILL                                                 
         BNE   FORM350                                                          
         CP    PREVBCOM,=P'0'      ANY COMMISSION?                              
         BE    FORM300                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@CMN),MX@CMN                                            
         LA    RF,TEMP+L'MX@CMN-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,PREVBCOM),(10,(RF)),2,MINUS=YES,ALIGN=LEFT                   
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
FORM300  CP    PREVBDIS,=P'0'      ANY DISCOUNT?                                
         BE    FORM310                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@DISS),MX@DISS                                          
         LA    RF,TEMP+L'MX@DISS-1                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,PREVBDIS),(10,(RF)),2,MINUS=YES,ALIGN=LEFT                   
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
FORM310  CP    PREVBPAY,=P'0'      ANY PAYABLE?                                 
         BE    FORM320                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@PAYBL),MX@PAYBL                                        
         LA    RF,TEMP+L'MX@PAYBL-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,PREVBPAY),(16,(RF)),2,MINUS=YES,ALIGN=LEFT                   
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
FORM320  LA    R3,COMBVATR         R3=A(SAVED VAT RATES OLD AND NEW)            
         LA    R4,PREVBVAT         R4=A(VAT AMOUNTS)                            
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    FORM345                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    FORM345                                                          
FORM330  CP    0(L'NARVAMT1,R4),=P'0' HAVE WE GOT A VAT AMOUNT?                 
         BE    FORM340                                                          
         MVC   VATRATE,0(R3)                                                    
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
FORM340  LA    R3,2(R3)                                                         
         LA    R4,L'PREVBTOT(R4)                                                
         LA    RF,PREVBVAT+(L'PREVBTOT*MAXNVATQ)                                
         CR    R4,RF                                                            
         BL    FORM330                                                          
         B     FORM350                                                          
*                                                                               
FORM345  CP    0(L'NARVAMT1,R4),=P'0' HAVE WE GOT A VAT AMOUNT?                 
         BE    FORM347                                                          
         MVC   VATVL(VATVLL1Q),0(R3)                                            
         ZAP   VATAMNT,0(L'PREVBTOT,R4)                                         
         BRAS  RE,TAXFMT                                                        
FORM347  LA    R3,L'COMBVATR(R3)                                                
         LA    R4,L'PREVBTOT(R4)                                                
         LA    RF,PREVBVAT+(L'PREVBTOT*MAXNVATQ)                                
         CR    R4,RF                                                            
         BL    FORM345                                                          
*                                                                               
FORM350  CLI   LINSUSED,0                                                       
         BNE   *+8                                                              
         MVI   LINSUSED,1                                                       
         MVI   DISATRIB,HILIGHTQ                                                
         B     FORMX                                                            
*                                                                               
         USING SCRIXD,R2           DSECT FOR INT/EXT LINE                       
FORM360  LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARIXD,R4                                                       
         MVC   SCRIXINT,MX@INT                                                  
         PACK  TEMP(L'TSIITOT),NINES(L'SCRIXIT-2) GET STRING OF NINES           
         CP    TEMP(L'TSIITOT),TSIITOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM370                                                          
         MP    TEMP(L'TSIITOT),=P'-1'                                           
         CP    TEMP(L'TSIITOT),TSIITOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM370                                                          
         CURED (P8,TSIITOT),(L'SCRIXIT,SCRIXIT),2,MINUS=YES                     
         B     FORM380                                                          
FORM370  CURED (P8,TSIITOT),(L'SCRIXIT,SCRIXIT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM380  MVC   SCRIXEXT,MX@EXT                                                  
         PACK  TEMP(L'TSIETOT),NINES(L'SCRIXET-2) GET STRING OF NINES           
         CP    TEMP(L'TSIETOT),TSIETOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM390                                                          
         MP    TEMP(L'TSIETOT),=P'-1'                                           
         CP    TEMP(L'TSIETOT),TSIETOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM390                                                          
         CURED (P8,TSIETOT),(L'SCRIXET,SCRIXET),2,MINUS=YES                     
         B     FORM400                                                          
FORM390  CURED (P8,TSIETOT),(L'SCRIXET,SCRIXET),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM400  MVI   LINSUSED,1          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ                                                
         B     FORMX                                                            
*                                                                               
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R4                                                      
*                                                                               
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FGRM30                                                           
         CLI   TSARFMT,TSWRKITM    WORKCODE SUBTOTAL?                           
         BE    FGRM40                                                           
         CLI   TSARFMT,TSITEM1                                                  
         BNE   FGRMX                                                            
                                                                                
         MVC   TEMPCNTR,CURCNTR                                                 
**NOP    MVC   WCDESC,SVWCDESC                                                  
*                                                                               
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         GOTO1 ASETELE,TSDRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         CLC   TSDWORK,PREVBILL    BILLING (99'S)?                              
         BE    FGRM25              (YES) SHOW SECONDARY COLUMNS                 
         GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
         B     FGRMX                                                            
*                                                                               
FGRM25   BAS   RE,PREVBLNS         PREV BILLED                                  
         GOTO1 ADISGRD,DMCB,('DWN2ND',AGCTBL)                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         B     FGRMX                                                            
*                                                                               
FGRM40   GOTO1 ADISGRD,DMCB,('DWNSUB',AGC2BL)                                   
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
CLOVER   NTR1                                                                   
*                                                                               
         USING SCRLIN1D,R2                                                      
         USING AFCELD,R4                                                        
         ICM   R4,15,AAFCELD       ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BZ    CLOVX                                                            
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OVALUE,C'B'         BOTH LOCAL AND AGENCY VALUES?                
         BNE   CLOVX                                                            
         TM    AFCXSTAT,AFCXSMEM   MEMO ITEM?                                   
         BNO   CLO10                                                            
         CLI   OMEMOAFC,C'Y'                                                    
         BE    CLO10                                                            
         CLI   OMEMOAFC,C'O'                                                    
         BNE   CLOVX                                                            
         DROP  RF                                                               
CLO10    GOTO1 AGETCURR,DMCB,AFCCURR                                            
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED AFCAMNT,(L'SCR1DESC,SCR1DESC),(RF),MINUS=YES,CURSYMB=YES         
CLOVX    B     XIT                                                              
         DROP  R2,4                                                             
***********************************************************************         
*        FORMAT STANDARD LINES FOR PREV BILLED IN DESCRIPTION FIELD   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
PREVBLNS NTR1                                                                   
*                                                                               
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
         BRAS  RE,DESCINF             DESCRIPTION INFO DISPLAY                  
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
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
PREVB58  MVC   VATNAME,SPACES                                                   
         MVC   VATNAME(L'MX@VAT-1),MX@VAT GST                                   
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
         DROP  R2                                                               
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
*        FORMAT STANDARD LINES IN DESCRIPTION FIELD                   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         USING TSARDATD,R4                                                      
STANDLNS NTR1                                                                   
         MVC   BTYPE,TSDTTYPE      SAVE THE TYPE                                
*                                                                               
         ICM   R4,15,ANARELD       ANY NARRATIVE TO DISPLAY?                    
         BZ    STAND40                                                          
         USING NARELD,R4                                                        
         SR    RF,RF                                                            
         IC    RF,NARLN                                                         
         SHI   RF,NARLN1Q                                                       
         LA    R1,NARREC                                                        
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND40  ICM   R4,15,APXDELD       POSTING XFER DETAIL ELEMENT?                 
         BZ    STAND120                                                         
         USING PXDELD,R4                                                        
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@XFR),MX@XFR                                            
         LA    RF,TEMP+L'MX@XFR-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         LR    R0,RF                                                            
         GOTO1 VDATCON,DMCB,(X'81',PXDDATE),(17,(R0))                           
         SR    RF,RF                                                            
         IC    RF,4(R1)                                                         
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
         CLI   PXDTYPE,PXDTFROM                                                 
         BE    STAND50                                                          
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@TO2),MX@TO2                                            
         LA    RF,TEMP+L'MX@TO2-1                                               
         B     *+14                                                             
STAND50  MVC   TEMP(L'MX@FROM),MX@FROM                                          
         LA    RF,TEMP+L'MX@FROM-1                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'PXDFRTO-1,RF),PXDFRTOU                                       
         LA    RF,2+L'PXDFRTO-1(RF)                                             
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND120 CLI   AGYCTRY,CTRYGER     IF GERMANY DISP DISCOUNT/FREELANCE           
         BNE   STAND160                                                         
         ICM   R4,15,ASCIELD                                                    
         BZ    STAND160                                                         
         USING SCIELD,R4                                                        
STAND130 CLI   SCITYPE,SCITCDSC    CASH DISCOUNT?                               
         BE    STAND140                                                         
         CLI   SCITYPE,SCITFTAX    FREELANCER?                                  
         BNE   STAND150                                                         
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@FLTX),MX@FLTX                                          
         LA    RF,TEMP+L'MX@NET-1                                               
         B     *+14                                                             
STAND140 MVC   TEMP(L'MX@DISS),MX@DISS                                          
         LA    RF,TEMP+L'MX@DISS-1                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED SCIAMNT,(12,(RF)),2,ALIGN=LEFT,MINUS=YES                         
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND150 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    STAND130                                                         
*                                                                               
STAND160 ICM   R4,15,AFFNELD       DISPLAY ORDER NUMBER?                        
         BZ    STAND170                                                         
         USING FFNELD,R4                                                        
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'MX@ORDER),MX@ORDER ORDER                                  
         LA    RF,TEMP+L'MX@ORDER-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    RE,RE                                                            
         IC    RE,FFNLN                                                         
         SHI   RE,FFNLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),FFNUMBER                                                 
         LA    RF,2+1(RE,RF)                                                    
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND170 ICM   R4,15,AOTHELD       DISPLAY SUB REFERENCE?                       
         BZ    STAND180                                                         
         USING OTHELD,R4                                                        
         MVC   TEMP(L'MX@SUBR),MX@SUBR SUBREF                                   
         LA    RF,TEMP+L'MX@SUBR-1                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'OTHNUM,RF),OTHNUM                                            
         LA    RF,L'OTHNUM+2(RF)                                                
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND180 ICM   R4,15,ATPRELD       DISPLAY PRICE LIST NUMBER?                   
         BZ    STAND190                                                         
         USING TPRELD,R4                                                        
         MVC   TEMP(L'MX@PCLST),MX@PCLST                                        
         LA    RF,TEMP+L'MX@PCLST-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'TPRCOD,RF),TPRCOD                                            
         LA    RF,L'TPRCOD+2(RF)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         LA    RF,2(RF)                                                         
         MVC   0(L'TPRNUM,RF),TPRNUM                                            
         LA    RF,L'TPRNUM(RF)                                                  
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND190 ICM   R4,15,ASCIELD                                                    
         BZ    STAND290                                                         
         USING SCIELD,R4                                                        
STAND200 CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BNE   STAND210                                                         
         MVC   TEMP(L'MX@HOURS),MX@HOURS                                        
         LA    RF,TEMP+L'MX@HOURS-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED SCIAMNT,(10,(RF)),2,ALIGN=LEFT,MINUS=YES                         
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND210 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    STAND200                                                         
         B     STAND290                                                         
*                                                                               
STAND290 ICM   R4,15,AFFTELD       FREE FORM TEXT ELEMENT                       
         BZ    STAND300                                                         
         USING FFTELD,R4                                                        
STAND291 CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BNE   STAND300                                                         
         CLI   FFTTYPE,FFTTPNAM    PAYEE NAME?                                  
         BE    STAND295                                                         
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     STAND291                                                         
*                                                                               
STAND295 MVC   TEMP(L'MX@PAYEE),MX@PAYEE                                        
         LA    RF,TEMP+L'MX@PAYEE-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    R1,R1                                                            
         USING FFTELD,R4                                                        
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         LA    RE,FFTDATA                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(RE)       GET NAME                                     
         LA    RF,3(RF,R1)                                                      
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND300 ICM   R4,15,AUNPELD       A(UNIT PRICING ELEMENT)                      
         BZ    STAND310                                                         
         MVC   TEMP(L'MX@UNIT),MX@UNIT                                          
         LA    RF,TEMP+L'MX@UNIT-1                                              
*                                                                               
         CLI   BTYPE,BAT08Q        TYPE 8 WITH UNPELD = BRO                     
         BE    STAND305                                                         
*                                                                               
         USING UNPELD,R4                                                        
         TM    UNPSTAT,UNPSQTRH    QUARTER HOURS?                               
         BZ    STAND305            NO                                           
*                                                                               
         MVC   TEMP(L'MX@HOURS),MX@HOURS                                        
         LA    RF,TEMP+L'MX@HOURS-1                                             
*                                                                               
STAND305 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
*                                                                               
         CURED (P3,UNPUNIT),(6,0(RF)),0,ALIGN=LEFT                              
         ORG   *-2                                                              
         TM    UNPSTAT,UNPSQTRH    QUARTER HOURS?                               
         BZ    *+8                 NO                                           
         MVI   11(R1),2            YES, SET DECIMALS                            
         BASR  RE,RF                                                            
*                                                                               
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
         MVC   TEMP(L'MX@PRICE),MX@PRICE                                        
         LA    RF,TEMP+L'MX@PRICE-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P4,UNPRICE),(9,0(RF)),2,ALIGN=LEFT,MINUS=YES                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
         USING SCIELD,R4                                                        
STAND310 DS    0H                                                               
         ICM   R4,15,ASCIELD                                                    
         JZ    STAND410                                                         
*                                                                               
STAND315 CLI   SCIEL,SCIELQ        PAYMENT DETAILS?                             
         JNE   STAND325                                                         
         CLI   SCITYPE,SCITCPAD                                                 
         JNE   STAND325                                                         
*                                                                               
STAND320 CP    SCIAMNT,PZERO       SKIP IF ZERO                                 
         JE    STAND410                                                         
*                                                                               
         MVC   TEMP,SPACES         SHOW PAYMENTS=                               
         MVC   TEMP(L'MX@PYMNT),MX@PYMNT                                        
         LA    RF,TEMP+L'MX@PYMNT-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P6,SCIAMNT),(12,(RF)),2,ALIGN=LEFT,MINUS=YES                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
         MVC   TEMP,SPACES         SHOW PAYMENT REFERENCE =                     
         MVC   TEMP(L'MX@PAYR2),MX@PAYR2                                        
         LA    RF,TEMP+L'MX@PAYR2-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         MVC   0(L'SCITCPNO,RF),SCITCPNO                                        
         LA    RF,L'SCITCPNO(RF)                                                
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
         MVC   TEMP,SPACES         SHOW PAYMENT DATE =                          
         MVC   TEMP(L'MX@PAYD2),MX@PAYD2                                        
         LA    RF,TEMP+L'MX@PAYD2-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         LR    R0,RF                                                            
         GOTO1 VDATCON,DMCB,(2,SCITCPDT),(13,(R0))                              
         SR    RF,RF                                                            
         IC    RF,4(R1)                                                         
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STAND325 LLC   R0,SCILN                                                         
         AR    R4,R0                                                            
         CLI   SCIEL,0                                                          
         JE    STAND410                                                         
         J     STAND315                                                         
*                                                                               
STAND410 DS    0H                                                               
*                                                                               
*&&US*&& BAS   RE,TIMEINF          DISPLAY TIME INFO                            
*                                                                               
STANDX   XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT STATUS LINE(2) IN DESCRIPTION FIELD                   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         USING TSARDATD,R4                                                      
STATLNS  NTR1                                                                   
*                                                                               
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    R3,TEMP                                                          
         CLC   TSDWORK,ORDER                                                    
         BE    STAT10                                                           
         TM    TSDTRST,TRNSDRFT    DRAFT TRANSACTION?                           
         BNO   STAT10                                                           
         MVC   0(L'MX@DRAFT,R3),MX@DRAFT                                        
         LA    RF,L'MX@DRAFT-1(R3)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT10   TM    COMPSTA4,CPYSIREG   INVOICE REGISTER IN USE?                     
         BNO   STAT30                                                           
         TM    TSDTEST,TRNSAUTH    INVOICE AUTHORISED?                          
         BNO   STAT20                                                           
         MVC   0(L'MX@ATHED,R3),MX@ATHED                                        
         LA    RF,L'MX@ATHED-1(R3)                                              
         B     *+14                                                             
STAT20   MVC   0(L'MX@UATH,R3),MX@UATH                                          
         LA    RF,L'MX@UATH-1(R3)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT30   TM    TSDTEST,TRNSHOLD    TRANSACTION HELD?                            
         BNO   STAT50                                                           
         MVC   0(L'MX@HELD,R3),MX@HELD                                          
         LA    RF,L'MX@HELD-1(R3)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,1(RF)                                                         
STAT50   LA    RF,TEMP                                                          
         SR    R3,RF                                                            
         BZ    STATX                                                            
         LR    RF,R3                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
STATX    XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT ATTRIBUTE LINE(S)                                     *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
ATTRLNS  NTR1                                                                   
*                                                                               
         ICM   R4,15,AAPEELD       ANALYSIS POINTER ELEMENT?                    
         BZ    ATTRLX                                                           
         USING APEELD,R4                                                        
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    RF,TEMP                                                          
         MVC   0(L'MX@ATTRS,RF),MX@ATTRS                                        
         LA    RF,L'MX@ATTRS-1(RF)                                              
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
         SHI   RE,APELN2Q+1                                                     
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
XDETLNS  NTR1                                                                   
*                                                                               
         STCM  R2,15,ANXTLINE      SAVE ADDRESS OF NEXT FREE LINE               
         USING TSARDATD,R4                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL REQUIRED?                 
         BNE   XDETLX                                                           
         LA    R0,UNSCANBK         CLEAR UNSCAN BLOCK                           
         LHI   R1,MAXUNSCN*UNSCNLNQ                                             
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,UNSCANBK                                                      
         USING UNSCAND,R3                                                       
         XR    RF,RF                                                            
         ICM   RF,1,TSDTTYPE                                                    
         BZ    XDETL10                                                          
         MVC   UNSCNLHS(L'MX@TYPE),MX@TYPE                                      
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
         SHI   R1,L'OTHEL+L'OTHLN                                               
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
         IC    RF,TSDSBREF                                                      
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
         TM    TSDSTAT3,TRSSOON    SOON BILLING?                                
         BNO   XDETL220                                                         
         MVC   UNSCNLHS(L'MX@SOON),MX@SOON                                      
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL220 CLC   TSDUSRID,SPACES     USER-ID PASSED?                              
         BNH   XDETL230            NO                                           
         MVC   UNSCNLHS(L'TSDUSRID),TSDUSRID                                    
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL230 CLC   TSDPIDN,SPACES                                                   
         BNH   XDETL240                                                         
         MVC   UNSCNLHS(L'MX@PID),MX@PID     PID                                
         MVC   UNSCNRHS(L'TSDPIDN),TSDPIDN                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL240 CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   XDETL600                                                         
         MVC   UNSCNLHS(L'MX@DSCAD),MX@DSCAD TRANS DISK ADDRESS                 
         LA    RF,L'TRNKDA                                                      
         GOTO1 VHEXOUT,DMCB,TSDDA,UNSCNRHS,(RF),1,=C'MIX'                       
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL600 ICM   R2,15,ANXTLINE      R2=A(NEXT DUMMY SCREEN LINE)                 
         GOTO1 AXDETDIS            DISPLAY ON DUMMY SCREEN LINES                
*                                                                               
XDETLX   XIT1  REGS=(R2)                                                        
         DROP  R3,R4                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
*        FORMAT TIME INFORMATION USA ONLY                             *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
TIMEINF  NTR1                                                                   
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   TIMEX                                                            
         LA    RF,TEMP                                                          
         MVC   TEMP,SPACES                                                      
         SR    RE,RE                                                            
         ICM   R4,15,APRTELD       PERSONNEL RATE ELEMENT?                      
         BZ    TIME15                                                           
         USING PRTELD,R4                                                        
         CP    PRTHOUR,=P'0'                                                    
         BE    TIME05                                                           
         MVC   TEMP(L'MX@HOURS),MX@HOURS                                        
         LA    RF,TEMP+L'MX@HOURS-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         ST    RF,FULL                                                          
         CURED (P3,PRTHOUR),(12,(RF)),2,MINUS=YES,ALIGN=LEFT                    
         ICM   RF,15,FULL                                                       
         AR    RF,R0                                                            
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         LA    R1,TEMP                                                          
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
TIME05   SR    R3,R3                                                            
         MVC   TEMP,SPACES                                                      
         LA    RF,TEMP                                                          
         TM    PRTSTAT,PRTSBILQ+PRTSNOTQ+PRTSRTEQ                               
         BZ    TIME15                                                           
         MVC   0(L'MX@TIME,RF),MX@TIME                                          
         LA    RF,L'MX@TIME-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME?                               
         BNO   *+12                                                             
         MVI   2(RF),C'B'                                                       
         B     TIME10                                                           
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME?                           
         BNO   *+12                                                             
         MVI   2(RF),C'N'                                                       
         B     TIME10                                                           
         MVI   2(RF),C'R'          ASSUME SPECIAL NON-BILLABLE TIME             
*                                                                               
TIME10   LA    RF,4(RF)                                                         
TIME15   L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         TM    TSDSTAT2,TRSSTIME+TRSSTADJ+TRSSTMSS                              
         BZ    TIME20                                                           
         MVC   0(L'MX@TMTYP,RF),MX@TMTYP                                        
         LA    RF,L'MX@TMTYP-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         TM    TSDSTAT2,TRSSTIME   TIME SHEET REGULAR?                          
         BNO   *+12                                                             
         MVI   2(RF),C'T'                                                       
         B     TIME20                                                           
         TM    TSDSTAT2,TRSSTADJ   TIME SHEET ADJUSTMENT?                       
         BNO   *+12                                                             
         MVI   2(RF),C'A'                                                       
         B     TIME20                                                           
         MVI   2(RF),C'M'          ASSUME TIME SHEET MISSING                    
TIME20   LA    RF,3(RF)                                                         
         LA    RE,TEMP                                                          
         SR    RF,RE               RF=L'(OUTPUT)                                
         BZ    TIMEX                                                            
         LA    R1,TEMP             R1=A(OUTPUT)                                 
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
TIMEX    XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
*&&                                                                             
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
         LA    RF,L'SCR1DESC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY OVERRIDE?                
         BNE   *+8                                                              
         LA    RF,L'SCR1FDSC       SQUASH DESC FOR XTRA WIDE AMT COL            
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
*        DEAL WITH THE WORK CODE TOTALS                               *         
***********************************************************************         
WORKTOT  NTR1                                                                   
*                                                                               
         TM    CURRFLAG,CURRSSTQ   SUPPRESS SUB TOTALS?                         
         BNO   WRKT05                                                           
         ZAP   WTRNTOT,=P'0'       CLEAR TOTALS                                 
         ZAP   WHRSTOT,=P'0'                                                    
         NI    CURRFLAG,X'FF'-CURRSSTQ   CLEAR FLAG                             
         B     WRKTX                                                            
WRKT05   MVC   KEYSAVE2,IOKEY                                                   
         ZAP   WORKHOUR,=P'0'      HOURS FOR WORKCODE                           
         MVC   WCDESC,SPACES                                                    
         LA    R3,IOKEY            GET THE WORKCODE RECORD                      
         USING WCORECD,R3                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,MYCO                                                     
         MVC   WCOKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         MVC   WCOKWRK,SAVEWC                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    WRKT10                                                           
         TM    IOERR,IOMAX                                                      
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     WRKTERRX                                                         
         CLC   SAVEWC,PREVBILL                                                  
         BNE   *+10                                                             
         MVC   WCDESC(L'MX@BLG),MX@BLG                                          
         TM    IOERR,IOERNF                                                     
         BO    WRKT50                                                           
         DC    H'0'                                                             
*                                                                               
WRKT10   L     R3,AIO2             R3=A(WORKCODE RECORD)                        
         LA    R4,WCORFST                                                       
WRKT20   CLI   0(R4),EOR           END OF RECORD?                               
         BE    WRKT50                                                           
         CLI   0(R4),WCOELQ        WORK CODE ELEMENT?                           
         BE    WRKT30                                                           
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     WRKT20                                                           
*                                                                               
         USING WCOELD,R4                                                        
WRKT30   MVC   WCDESC,WCODESC    GET DESCRIPTION                                
         L     RF,AOPTVALS       RF=A(OPTION BLOCK)                             
         CLI   OVALUE-OPTVALSD(RF),C'B'                                         
         BE    WRKT50                                                           
         CLC   SAVEWC,PREVBILL                                                  
         BE    WRKT50                                                           
         CLI   AGYCTRY,CTRYGER                                                  
         BNE   WRKT40                                                           
         CLI   SAVEWC,C'Z'                                                      
         BH    WRKT50                                                           
         B     *+12                                                             
WRKT40   TM    WCOSTAT,WCOSHCOE    HOURS?                                       
         BNO   WRKT50                                                           
         AP    WORKHOUR,WHRSTOT    HOURS FOR WORKCODE                           
         AP    HRSTOT,WHRSTOT      HOURS FOR ACCOUNT                            
         DROP  R4                                                               
*                                                                               
WRKT50   CLC   SAVEWC,ORDER                                                     
         BE    WRKT140                                                          
*                                                                               
         L     RF,AOPTVALS       RF=A(OPTION BLOCK)                             
         CLI   OVALUE-OPTVALSD(RF),C'B'                                         
         BNE   WRKT60                                                           
         AP    WORKHOUR,WHRSTOT    HOURS FOR WORKCODE                           
         AP    HRSTOT,WHRSTOT      HOURS FOR ACCOUNT                            
WRKT60   AP    TRNTOT,WTRNTOT      ACCOUNT NET TOTAL                            
         CLI   AGYCTRY,CTRYGER     EXTERNAL/INTERNAL TOTALS FOR GERMANY         
         BNE   WRKT140                                                          
         CLI   SAVEWC,C'0'                                                      
         BL    *+14                                                             
         AP    EXTTOT,WTRNTOT                                                   
         B     WRKT140                                                          
         AP    INTTOT,WTRNTOT                                                   
*                                                                               
WRKT140  L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSWLNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARWRKD,R3                                                      
         MVC   TSWWORK,SAVEWC      WORK CODE                                    
         MVC   TSWDESC,WCDESC      WORKCODE DESCRIPTION                         
         MVI   TSWFMT,TSWRKITM     WORKCODE TOTAL ITEM TYPE                     
         ZAP   TSWAMNT,WTRNTOT     WORKCODE AMOUNT                              
         ZAP   TSWHOURS,WORKHOUR   HOURS FOR WORKCODE                           
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR                                                      
         MVC   TSWLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
*                                                                               
         GOTO1 ATSARADD                                                         
         BE    WRKT200                                                          
         TM    DISPFLAG,DISIOMAX   IF MAX IO IT HAS BEEN ADDED                  
         BNO   WRKTERRX                                                         
WRKT200  ZAP   WTRNTOT,=P'0'       CLEAR TOTALS                                 
         ZAP   WHRSTOT,=P'0'                                                    
         MVC   SAVEWC,NEXTWC                                                    
         MVC   TSLSTREC,TSCURRNO                                                
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    WRKT220                                                          
         MVI   DISATRIB,HILIGHTQ                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
WRKT220  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         CLC   TSWWORK,PREVBILL    PREVIOUS BILLING TOTAL?                      
         BE    *+8                                                              
         BAS   RE,INTEXT                                                        
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    WRKTERRX                                                         
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    WRKTERRX                                                         
         TM    DISPFLAG,DISIOMAX   COULD HAVE BEEN SET ON TSAR ADD              
         BO    WRKTERRX                                                         
         TM    DISPFLAG,ALLREADQ   IF ALL REC READ NO POINT RESETTING           
         BO    WRKTX                                                            
*                                                                               
         MVC   IOKEY,KEYSAVE2      REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    WRKTX                                                            
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     WRKTERRX                                                         
*                                                                               
WRKTX    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
WRKTERRX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH INTERNAL/EXTERNAL TOTALS (GERMANY ONLY)            *         
***********************************************************************         
INTEXT   NTR1                                                                   
*                                                                               
         CLI   AGYCTRY,CTRYGER     ONLY FOR GERMANY                             
         BNE   INTEXTX                                                          
         CLC   NEXTWC,PREVBILL     ONLY PRECEEDING PREV BILLING                 
         BNE   INTEXTX                                                          
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSILNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         MVC   TSARLINE,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARIXD,R3          INTERNAL/EXTERNAL DSECT                      
         MVI   TSIFMT,TSIXITM      INTERNAL/EXTERNAL ITEM                       
         ZAP   TSIITOT,INTTOT      INTERNAL TOTAL                               
         ZAP   TSIETOT,EXTTOT      EXTERMAL TOTAL                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR                                                      
         MVC   TSILINES,LINSUSED                                                
         GOTO1 ATSARADD                                                         
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    INTEXT50                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    INTEXT50                                                         
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    INTEXT50                                                         
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
INTEXT50 SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
INTEXTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         TM    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS?                     
         BO    TOTX                                                             
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
         ZAP   TSTTTOT,TRNTOT      TRANS TOTAL                                  
         ZAP   TSTHTOT,HRSTOT      HOURS TOTAL                                  
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR                                                      
         MVC   TSTLINES,LINSUSED                                                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    TOTX                                                             
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3                                                               
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
BAT08Q   EQU   8                                                                
BAT21Q   EQU   21                                                               
BAT22Q   EQU   22                                                               
BAT70Q   EQU   70                                                               
BAT71Q   EQU   71                                                               
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH16,78                                                      
         DCDDL AC#ENH17,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#XJOB,11                                                       
         DCDDL AC#DRAFT,9                                                       
         DCDDL AC#CTR,9                                                         
         DCDDL AC#NET,7                                                         
         DCDDL AC#DISS,7                                                        
         DCDDL AC#PAYBL,7                                                       
         DCDDL AC#FLTX,7                                                        
         DCDDL AC#ORDER,7                                                       
         DCDDL AC#SUBR,7                                                        
         DCDDL AC#ATTRS,L'MX@ATTRS                                              
         DCDDL AC#ATHED,9                                                       
         DCDDL AC#UATH,9                                                        
         DCDDL AC#HELD,9                                                        
         DCDDL AC#HOURS,5                                                       
         DCDDL AC#TIME,5                                                        
         DCDDL AC#TMTYP,5                                                       
         DCDDL AC#TYPE,2                                                        
         DCDDL AC#STT,2                                                         
         DCDDL AC#OTHER,2                                                       
         DCDDL AC#UDAT,2                                                        
         DCDDL AC#DATE,2                                                        
         DCDDL AC#PELDT,2                                                       
         DCDDL AC#CR,2                                                          
         DCDDL AC#DR,2                                                          
         DCDDL AC#SEQ,2                                                         
         DCDDL AC#PCLST,2                                                       
         DCDDL AC#ACCT,15                                                       
         DCDDL AC#ACCTS,15                                                      
         DCDDL AC#WCTOT,15                                                      
         DCDDL AC#INT,15                                                        
         DCDDL AC#EXT,15                                                        
         DCDDL AC#CMN,10                                                        
         DCDDL AC#VAT,5                                                         
         DCDDL AC#TO2,5                                                         
         DCDDL AC#FROM,5                                                        
         DCDDL AC#TYPE,5                                                        
         DCDDL AC#XFRD,12                                                       
         DCDDL AC#PAYEE,10                                                      
         DCDDL AC#LCLCU,19,R                                                    
         DCDDL AC#PRICE,5                                                       
         DCDDL AC#UNIT,5                                                        
         DCDDL AC#BLG,10                                                        
         DCDDL AC#DSCAD,3                                                       
         DCDDL AC#BLGTY,L'MX@BLGTY          BILLING TYPE                        
         DCDDL AC#DRCR,L'MX@DRCR            DR/CR                               
         DCDDL AC#TIMTP,L'MX@TIMTP          TIME TYPE                           
         DCDDL AC#RSBTD,L'MX@RSBTD          BILLING RUN DATE                    
         DCDDL AC#ACTDT,L'MX@ACTDT          ACTIVITY DATE                       
         DCDDL AC#RSPO,L'MX@RSPO            ORDER NUMBER                        
         DCDDL AC#MOA,L'MX@MOA              MOA                                 
         DCDDL AC#TRAND,L'MX@TRAND          TRANS DATE                          
         DCDDL AC#TRANR,L'MX@TRANR          TRANS REF                           
         DCDDL AC#TRANT,L'MX@TRANT          TRANS TYPE                          
         DCDDL AC#RSBRF,L'MX@RSBRF          BATCH REF                           
         DCDDL AC#RSBCM,L'MX@RSBCM          BILLED COMMISSION                   
         DCDDL AC#GRSBD,L'MX@GRSBD          GROSS BILLED                        
         DCDDL AC#RSTDT,L'MX@XFRD           TRANSFER DATE                       
         DCDDL AC#RSTAC,L'MX@XFRT           TRANSFER TO ACCOUNT                 
         DCDDL AC#RSTFC,L'MX@XFRF           TRANSFER FROM ACCOUNT               
         DCDDL AC#RSWNO,L'MX@RSWNO          WRITE-OFF #                         
         DCDDL AC#RSWDT,L'MX@RSWDT          WRITE-OFF DATE                      
         DCDDL AC#CTRAN,L'MX@CTRAN          CONTRA ACCOUNT NAME                 
*        DCDDL AC#RSDUD,L'MX@RSDUD          DUE DATE                            
         DCDDL AC#INVC2,L'MX@INVCN          INVOICE NUMBER                      
         DCDDL AC#SOON,L'MX@SOON            SOON                                
         DCDDL AC#USRID,L'MX@USRID          USER-ID                             
         DCDDL AC#CPIDN,L'MX@PID            PID                                 
         DCDDL AC#PYMNT,L'MX@PYMNT          PAYMENT                             
         DCDDL AC#PAYR2,L'MX@PAYR2          PAYMENT REFERENCE                   
         DCDDL AC#PAYD2,L'MX@PAYD2          PAYMENT DATE                        
         DCDDL AC#PAYAM,L'MX@PAYAM          PAYMENT AMOUNT                      
         DCDDL AC#PAYRF,L'MX@PAYRF          PAYMENT REF                         
         DCDDL AC#PAYDT,L'MX@PAYDT          PAYMENT DATE                        
         DC    AL1(EOT)                                                         
*                                                                               
DCCAP    DS    0X                                                               
         DCDDL AC#ONT,3                                                         
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#HST,3                                                         
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
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
*PRVTAB                                                                         
       ++INCLUDE ACPRVTABN                                                      
         EJECT                                                                  
**NOP                                                                           
*&&DO                                                                           
***********************************************************************         
*        DEAL WITH THE WORK CODE TOTALS                               *         
***********************************************************************         
GETWCN   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE2,IOKEY                                                   
         MVC   SVWCDESC,SPACES                                                  
         LA    R3,IOKEY            GET THE WORKCODE RECORD                      
         USING WCORECD,R3                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,MYCO                                                     
         MVC   WCOKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         MVC   WCOKWRK,SAVEWC                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    GWCN10                                                           
         TM    IOERR,IOMAX                                                      
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     GWCNERX                                                          
         CLC   SAVEWC,PREVBILL                                                  
         BNE   *+10                                                             
         MVC   SVWCDESC(L'MX@BLG),MX@BLG                                        
         TM    IOERR,IOERNF                                                     
         BO    GWCNX                                                            
         DC    H'0'                                                             
*                                                                               
GWCN10   L     R3,AIO2             R3=A(WORKCODE RECORD)                        
         LA    R4,WCORFST                                                       
GWCN20   CLI   0(R4),EOR           END OF RECORD?                               
         BE    GWCN50                                                           
         CLI   0(R4),WCOELQ        WORK CODE ELEMENT?                           
         BE    GWCN30                                                           
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R0,R4                                                            
         B     GWCN20                                                           
*                                                                               
         USING WCOELD,R4                                                        
GWCN30   MVC   SVWCDESC,WCODESC    GET DESCRIPTION                              
         DROP  R4                                                               
*                                                                               
GWCN50   MVC   IOKEY,KEYSAVE2      REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    GWCNX                                                            
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     GWCNERX                                                          
*                                                                               
GWCNX    J     OKXIT                                                            
GWCNERX  J     ERXIT                                                            
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*        DISPLAY DESCRIPTION INFORMATION                              *         
*          R3=A(NUMBER OF LINES USED SO FAR)                          *         
***********************************************************************         
         USING CHDRECD,R3                                                       
GETCTRN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING RECORD)              
         CLC   CHDKSPAC,SPACES     TRANSACTION RECORD?                          
         BNE   GETCTX                                                           
         OC    CHDKNULL,CHDKNULL                                                
         BNZ   GETCTX                                                           
         MVC   CURCNTR,SPACES                                                   
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    GETCT10                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETCTERX                                                         
*                                                                               
GETCT10  L     R3,AIO1                                                          
         LA    R3,CHDRFST                                                       
GETCT20  CLI   0(R3),EOR           END OF RECORD                                
         BE    GETCTX                                                           
         CLI   0(R3),CACELQ        CONTRA HEADER ELEMENT                        
         BE    GETCT30                                                          
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETCT20                                                          
*                                                                               
         USING CACELD,R3                                                        
GETCT30  SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SHI   R1,CACLN1Q                                                       
         BNP   GETCTX                                                           
         CHI   R1,L'CURCNTR                                                     
         BNH   *+8                                                              
         LHI   R1,L'CURCNTR                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURCNTR(0),CACNAME                                               
*                                                                               
GETCTX   J     OKXIT                                                            
GETCTERX J     ERXIT                                                            
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DESCRIPTION INFORMATION                              *         
* ON ENTRY R1=A(INFORMATION TO BE DISPLAYED)                          *         
*          R2=A(NEXT DUMMY LINE TO BE USED                            *         
*          R3=A(NUMBER OF LINES USED SO FAR)                          *         
*          RF=L'(INFORMATION TO BE DISPLAYED)                         *         
* ON EXIT  R2 AND R3 ARE UPDATED                                      *         
***********************************************************************         
         USING SCRLIN1D,R2                                                      
DESCINF  NTR1  BASE=*,LABEL=*                                                   
*                                 ********************************              
         TM    PCDRIVEN,PCGRIDQ    DO NOT DISPLAY LINES FOR GRIDS               
         BO    DESCIX             ********************************              
*                                                                               
         LR    R4,R1                                                            
         LA    R0,L'SCR1DESC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY OVERRIDE?                
         BNE   *+8                                                              
         LA    R0,L'SCR1FDSC       SQUASH DESC FOR XTRA WIDE AMT COL            
         GOTO1 VCHOPPER,DMCB,((RF),(R4)),((R0),SCANBLK),11                      
         L     RE,DMCB+8                                                        
         LTR   RE,RE               RE=(NUMBER OF NARRATIVE LINES)               
         BZ    DESCIX                                                           
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BH    DESCIX                                                           
         STC   RF,LINSUSED                                                      
         LA    R4,SCANBLK                                                       
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' LOCAL CURRENCY DISPLAY?                 
         BE    DESCI10                                                          
         MVC   SCR1DESC,0(R4)      WIDE NARRATIVE COLUMN DISPLAY                
         LA    R4,L'SCR1DESC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     DESCIX                                                           
*                                                                               
DESCI10  MVC   SCR1FDSC,0(R4)      FOREIGN CURRENCY- NARROW COLUMN              
         LA    R4,L'SCR1FDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
DESCIX   XIT1  REGS=(R2,R3)                                                     
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT TAX LINE                                                     *         
***********************************************************************         
TAXFMT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'VATNAME),VATNAME                                          
         LA    RF,TEMP+L'VATNAME-1                                              
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
         BRAS  RE,DESCINF          DESCRIPTION INFO DISPLAY                     
*                                                                               
TAXFMTX  XIT1  REGS=(R2)           EXIT WITHOUT RESTORING R2                    
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
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
*----------------------------------------------------------------------         
* DR OR CR?                                                                     
*----------------------------------------------------------------------         
         CLI   GCTCOID,GCDRCRQ         DR OR CR?                                
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
*-----------------------------------------------------------                    
* WORKCODE                                                                      
*-----------------------------------------------------------                    
         USING TSARWRKD,R4                                                      
GRDSP05  CLI   GCTCOID,GCWCN                                                    
         BNE   GRDSP10                                                          
*                                                                               
         MVC   TEMP(L'TSWWORK),TSWWORK                                          
         MVI   TEMP+L'TSWWORK,C'-'                                              
         MVC   TEMP+L'TSWWORK+1(L'TSWDESC),TSWDESC                              
         LHI   R1,L'TSWWORK+1+L'TSWDESC                                         
         B     GRDSPX                                                           
*-------------------------------------------------------------                  
* TIME TYPE                                                                     
*-------------------------------------------------------------                  
         USING TSARDATD,R4                                                      
         USING PRTELD,R2                                                        
GRDSP10  CLI   GCTCOID,GCTIMT      TIME TYPE?                                   
         BNE   GRDSP20             (NO) EXIT                                    
         ICM   R2,15,APRTELD      THEN PERSONNEL RATE REQUIRED                  
         BZ    GRDSPX                                                           
*                                                                               
         TM    PRTSTAT,PRTSBILQ+PRTSNOTQ+PRTSRTEQ                               
         BZ    GRDSPX                                                           
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME?                               
         BZ    *+12                                                             
         MVI   TEMP,C'B'                                                        
         B     GRDSPX                                                           
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME?                           
         BZ    *+12                                                             
         MVI   TEMP,C'N'                                                        
         B     GRDSPX                                                           
         MVI   TEMP,C'R'          ASSUME SPECIAL NON-BILLABLE TIME              
         B     GRDSPX                                                           
         DROP  R2,R4                                                            
*                                                                               
*-----------------------------------------------------------                    
* ANALYSIS POINTER ELEMENT                                                      
*-----------------------------------------------------------                    
         USING APEELD,R2                                                        
GRDSP20  CLI   GCTCOID,GCAPE                                                    
         BNE   GRDSP50             (NO) EXIT                                    
         ICM   R2,15,AAPEELD       ANALYSIS POINTER ELEMENT?                    
         BZ    GRDSPX                                                           
         MVI   TEMP,C' '             < USE TEMP AND WORK                        
         MVC   TEMP+1(L'TEMP-1),TEMP < TO BUILD ATTRIBUTE                       
         LA    RF,TEMP                                                          
         SR    R0,R0                                                            
         CLI   APELN,APELN1Q                                                    
         BNH   GRDSPX                                                           
         ICM   R0,1,APENUM         NUMBER OF SUB ELEMENTS                       
         BZ    GRDSPX                                                           
         LA    R5,APENTRY          R4=A(FIRST SUB ELEMENT)                      
         USING APENTRY,R5                                                       
GRDSP30  SR    RE,RE                                                            
         IC    RE,APENLEN          RE=L'(SUB ELEMENT)                           
         SHI   RE,APELN2Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),APENACT     GET ACCOUNT CODE                             
         LA    RF,2(RE,RF)         BUMP RF TO NEXT POSITION                     
         SR    RE,RE                                                            
         IC    RE,APENLEN                                                       
         AR    R5,RE               BUMP R4 TO NEXT SUB ELEMENT                  
         BCT   R0,GRDSP30                                                       
         LA    RE,TEMP                                                          
         SR    RF,RE                                                            
         BZ    GRDSPX                                                           
         LR    R1,RF                                                            
         B     GRDSPX                                                           
         DROP  R2                                                               
*-----------------------------------------------------------                    
* GST                                                                           
*-----------------------------------------------------------                    
         USING VBIELD,R2                                                        
GRDSP50  CLI   GCTCOID,GCGST                                                    
         BNE   GRDSP60                                                          
         ICM   R2,15,AVBIELD       R4=A(VAT BILLED ELEMENT)                     
         BZ    GRDSPX                                                           
         MVC   TEMPGST,SPACES                                                   
         LA    RF,TEMPGST                                                       
         ST    RF,ATMPGST                                                       
*                                                                               
GRDSP56  CLI   VBIEL,EOR           END OF RECORD?                               
         BE    GRDSP59                                                          
         CLI   VBIEL,VBIELQ        VAT BILLED ELEMENT?                          
         BE    GRDSP58                                                          
*                                                                               
GRDSP57  SR    RF,RF                                                            
         IC    RF,VBILN                                                         
         AR    R2,RF                                                            
         B     GRDSP56                                                          
*                                                                               
GRDSP58  MVC   VATNAME,SPACES                                                   
         MVC   VATNAME(L'MX@VAT-1),MX@VAT GST                                   
         MVC   VATRULE,VBITYPE     VAT CODE                                     
         MVC   VATRATE,VBIRATE     VAT RATE                                     
         MVC   VATINDS,VBIINDS     VAT INDICATORS                               
         ZAP   VATAMNT,VBIVAT      VAT AMOUNT                                   
         BRAS  RE,TAXFMT           FORMAT TAX LINE                              
*                                                                               
         L     RE,ATMPGST          FIND PLACE IN TEMPGST                        
         ICM   RF,15,FULL          GET LENGTH OF DATA TO MOVE                   
         SHI   RF,1                SUBTRACT ONE FOR MVC                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),TEMP        MOVE FROM TEMP TO TEMPGST                    
         AHI   RF,1                BUMP LENGTH BACK UP                          
         AR    RE,RF               FIND NEXT SPACE                              
         MVI   0(RE),C','          PUT IN A COMMA                               
         AHI   RE,1                                                             
         ST    RE,ATMPGST                                                       
         B     GRDSP57                                                          
*                                                                               
GRDSP59  MVC   TEMP,SPACES                                                      
         LA    RE,TEMPGST                                                       
         L     R1,ATMPGST                                                       
         SR    R1,RE                                                            
         SHI   R1,1                SUBTRACT 1 FOR COMMA                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),TEMPGST                                                  
         ST    R1,FULL             EXIT WITH LENGTH IN R1                       
         B     GRDSPX                                                           
         DROP  R2                                                               
*-----------------------------------------------------------                    
* PST                                                                           
*-----------------------------------------------------------                    
         USING PBIELD,R2                                                        
GRDSP60  CLI   GCTCOID,GCPST                                                    
         BNE   GRDSP80                                                          
         ICM   R2,15,APBIELD       R4=A(PST BILLED ELEMENT)                     
         BZ    GRDSPX                                                           
         MVC   TEMPGST,SPACES                                                   
         LA    RF,TEMPGST                                                       
         ST    RF,ATMPGST                                                       
*                                                                               
GRDSP62  CLI   PBIEL,EOR           END OF RECORD?                               
         BE    GRDSP72                                                          
         CLI   PBIEL,PBIELQ        PST BILLED ELEMENT?                          
         BE    GRDSP66                                                          
*                                                                               
GRDSP64  SR    RF,RF                                                            
         IC    RF,PBILN                                                         
         AR    R2,RF                                                            
         B     GRDSP62                                                          
*                                                                               
GRDSP66  MVC   VATNAME,SPACES                                                   
*                                                                               
         L     R1,APRVTAB                                                       
GRDSP68  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    GRDSP70                                                          
         CLC   PBIPRV,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,L'PRVTAB(R1)                                                  
         B     GRDSP68                                                          
*                                                                               
         MVC   WORK(2),LARF0       PASS DESCRIPTION BACK                        
         MVC   WORK+2(2),2(R1)                                                  
         EX    0,WORK                                                           
         MVC   VATNAME,0(RF)                                                    
*                                                                               
GRDSP70  MVC   VATRULE,PBITYPE     PST CODE                                     
         MVC   VATRATE,PBIRATE     PST RATE                                     
         MVC   VATINDS,PBIINDS     PST INDICATORS                               
         ZAP   VATAMNT,PBIPST      PST AMOUNT                                   
         BRAS  RE,TAXFMT           FORMAT TAX LINE                              
*                                                                               
         L     RE,ATMPGST          FIND PLACE IN TEMPGST                        
         ICM   RF,15,FULL          GET LENGTH OF DATA TO MOVE                   
         SHI   RF,1                SUBTRACT ONE FOR MVC                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),TEMP        MOVE FROM TEMP TO TEMPGST                    
         AHI   RF,1                BUMP LENGTH BACK UP                          
         AR    RE,RF               FIND NEXT SPACE                              
         MVI   0(RE),C','          PUT IN A COMMA                               
         AHI   RE,1                                                             
         ST    RE,ATMPGST                                                       
         B     GRDSP64                                                          
*                                                                               
GRDSP72  MVC   TEMP,SPACES                                                      
         LA    RE,TEMPGST                                                       
         L     R1,ATMPGST                                                       
         SR    R1,RE                                                            
         SHI   R1,1                SUBTRACT 1 FOR COMMA                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),TEMPGST                                                  
         ST    R1,FULL             EXIT WITH LENGTH IN R1                       
         B     GRDSPX                                                           
*                                                                               
*-----------------------------------------------------------                    
* UNITS                                                                         
*-----------------------------------------------------------                    
         USING UNPELD,R4                                                        
GRDSP80  CLI   GCTCOID,GCUNI                                                    
         BNE   GRDSPX                                                           
         ICM   R4,15,AUNPELD                                                    
         BZ    GRDSPX                                                           
         XC    TEMP,TEMP                                                        
         LA    RF,TEMP                                                          
         CURED (P3,UNPUNIT),(6,0(RF)),0,ALIGN=LEFT                              
         ORG   *-2                                                              
         TM    UNPSTAT,UNPSQTRH    QUARTER HOURS?                               
         BZ    *+8                 NO                                           
         MVI   11(R1),2            YES, SET DECIMALS                            
         BASR  RE,RF                                                            
         B     GRDSPX                                                           
*                                                                               
LARF0    LA    RF,0                                                             
         DROP  R2                                                               
*-------------------------------------------------------------                  
GRDSPX   J     XITR1                                                            
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        JOB DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD             *         
*        - THIS TABLE MUST MATCH BELOW TABLE.  DEFINED FOR ALL ROWS   *         
*          EXCEPT WORKCODE SUBTOTALS                                  *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTWCD   DC    AL1(GWCDLQ,GCWCD,L'LC@WC2,L'TSDWORK)      WORK CODE              
         DC    AL2(LC@WC2-WORKD,TSDWORK-TSARDATD)                               
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@ACCT),AL2(MX@ACCT-OVERWRKD)                           
GWCDLQ   EQU   *-GCTWCD                                                         
**NOP                                                                           
*CTWCN   DC    AL1(GWCNLQ,GCWCN,L'LC@WC2,L'WCDESC)       WORK CODE              
*        DC    AL2(LC@WC2-WORKD,WCDESC-OVERWRKD)                                
*        DC    AL1(0,GCTISPDA,0,0)                                              
*        DC    AL1(0,0),AL2(0)                                                  
*WCNLQ   EQU   *-GCTWCN                                                         
*                                                                               
GCTCON   DC    AL1(GCONLQ,GCCONTRA,L'LC@CTRA,L'TSDCONT)  CONTRA                 
         DC    AL2(LC@CTRA-WORKD,TSDCONT-TSARDATD)                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
GCONLQ   EQU   *-GCTCON                                                         
*                                                                               
GCTCNA   DC    AL1(GCNALQ,GCCNA,L'MX@CTRAN,L'TEMPCNTR)  CONTRA NAME             
         DC    AL2(MX@CTRAN-OVERWRKD,TEMPCNTR-OVERWRKD)                         
         DC    AL1(GCTIOVER,GCTISPDA,0,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
GCNALQ   EQU   *-GCTCNA                                                         
*                                                                               
GCTDATE  DC    AL1(GDATELQ,GCDATE,L'MX@TRAND,1)          TRANS DATE             
         DC    AL2(MX@TRAND-OVERWRKD,TSDTDAT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GDATELQ  EQU   *-GCTDATE                                                        
*                                                                               
GCTREF   DC    AL1(GREFLQ,GCREF,L'MX@TRANR,L'TSDTREF)    TRANS REF              
         DC    AL2(MX@TRANR-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GREFLQ   EQU   *-GCTREF                                                         
*                                                                               
*CTMOA   DC    AL1(GMOALQ,GCMOA,L'MX@MOA,1)              MOA                    
*        DC    AL2(MX@MOA-OVERWRKD,TSDTMOA-TSARDATD)                            
*        DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
*        DC    AL1(0,0),AL2(0)                                                  
*MOALQ   EQU   *-GCTMOA                                                         
*                                                                               
GCTORD   DC    AL1(GORDLQ,GCORD,L'MX@RSPO,L'FFNONUM)     ORDER NUMBER           
         DC    AL2(MX@RSPO-OVERWRKD),AL1(FFNELQ,FFNONUM-FFNELD)                 
         DC    AL1(GCTIELEM+GCTIOVER,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GORDLQ   EQU   *-GCTORD                                                         
*                                                                               
GCTAMNT  DC    AL1(GAMTLQ,GCAMNT,L'LC@AMT,L'TSDTAMNT)    AMOUNT                 
         DC    AL2(LC@AMT-WORKD,TSDTAMNT-TSARDATD)                              
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTTTOT),AL2(TSTTTOT-TSARTOTD)                           
GAMTLQ   EQU   *-GCTAMNT                                                        
*                                                                               
GCTDRCR  DC    AL1(GDRCRLQ,GCDRCRQ,L'MX@DRCR,L'TSDTAMNT) DR/CR                  
         DC    AL2(MX@DRCR-OVERWRKD,AGRDSP-OVERWRKD)                            
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GDRCRLQ  EQU   *-GCTDRCR                                                        
*                                                                               
GCTBREF  DC    AL1(GBREFLQ,GCBREF,L'MX@RSBRF,L'TSDBREF)  BATCH REF              
         DC    AL2(MX@RSBRF-OVERWRKD,TSDBREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GBREFLQ  EQU   *-GCTBREF                                                        
*                                                                               
GCTTRNTY DC    AL1(GTRNTYLQ,GCTRNTYP,L'MX@TRANT,1)       TRAN TYPE              
         DC    AL2(MX@TRANT-OVERWRKD,TSDTTYPE-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFCENT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GTRNTYLQ EQU   *-GCTTRNTY                                                       
*                                                                               
GCTACDTE DC    AL1(GACDLQ,GCACTDTE,L'MX@ACTDT,2)         ACTIVITY DATE          
         DC    AL2(MX@ACTDT-OVERWRKD,TSDACDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GACDLQ   EQU   *-GCTACDTE                                                       
*                                                                               
GCTBILD  DC    AL1(GBILDLQ,GCBILLED,1,L'TSDALLOC)        BILLED?                
         DC    AL2(UP@BATL-WORKD,TSDALLOC-TSARDATD)                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
GBILDLQ  EQU   *-GCTBILD                                                        
*                                                                               
GCTBILTY DC    AL1(GBILTYLQ,GCBILTY,L'MX@BLGTY,L'NARBTYPE) BILLING TYPE         
         DC    AL2(MX@BLGTY-OVERWRKD),AL1(ELETSARQ,NARLN1Q)                     
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0,0,GCTI2ND)                      
         DC    AL1(NARELQ,0),AL2(0)                                             
GBILTYLQ EQU   *-GCTBILTY                                                       
*                                                                               
GCTBRUN  DC    AL1(GBRUNLQ,GCBRUND,L'MX@RSBTD,2)       BILLING RUN DATE         
         DC    AL2(MX@RSBTD-OVERWRKD,TSDUSDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GBRUNLQ  EQU   *-GCTBRUN                                                        
*                                                                               
GCTCOMM  DC    AL1(GCOMMLQ,GCCOMM,L'MX@RSBCM,L'NARCAMT)  COMMISSION             
         DC    AL2(MX@RSBCM-OVERWRKD)                                           
         DC    AL1(ELETSARQ,NARLN1Q+(NARCAMT-NARINFOD))                         
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,GCTI2ND)                                    
         DC    AL1(NARELQ,0),AL2(0)                                             
GCOMMLQ  EQU   *-GCTCOMM                                                        
*                                                                               
GCTNETA  DC    AL1(GNETALQ,GCGRSBIL,L'MX@GRSBD,L'NARPAMT1) GROSS BILLED         
         DC    AL2(MX@GRSBD-OVERWRKD)                                           
         DC    AL1(ELETSARQ,NARLN1Q+(NARPAMT1-NARINFOD))                        
         DC    AL1(GCTIELEM+GCTIELST+GCTIOVER,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,GCTI2ND)                                    
         DC    AL1(NARELQ,0),AL2(0)                                             
GNETALQ  EQU   *-GCTNETA                                                        
*                                                                               
GCTHRS   DC    AL1(GHRSLQ,GCHOURS,L'MX@HOURS,L'PRTHOUR)  HOURS                  
         DC    AL2(MX@HOURS-OVERWRKD),AL1(PRTELQ,PRTHOUR-PRTELD)                
         DC    AL1(GCTIELEM+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GHRSLQ   EQU   *-GCTHRS                                                         
*                                                                               
GCTTIMT  DC    AL1(GTIMTLQ,GCTIMT,L'MX@TIMTP,0)          TIME TYPE              
         DC    AL2(MX@TIMTP-OVERWRKD),AL2(AGRDSP-OVERWRKD)                      
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GTIMTLQ  EQU   *-GCTTIMT                                                        
*&&DO                                                                           
GCTUNI   DC    AL1(GUNILQ,GCUNI,L'MX@UNIT,L'UNPUNIT)     UNIT                   
         DC    AL2(MX@UNIT-OVERWRKD),AL1(UNPELQ,UNPUNIT-UNPELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFNUM+GCTFRGHT,GCTINDEC)               
         DC    AL1(0,0,0,0)                                                     
GUNILQ   EQU   *-GCTUNI                                                         
*&&                                                                             
GCTUNI   DC    AL1(GUNILQ,GCUNI,L'MX@UNIT,0)             UNIT                   
         DC    AL2(MX@UNIT-OVERWRKD),AL2(AGRDSP-OVERWRKD)                       
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0,0,0)                                                     
GUNILQ   EQU   *-GCTUNI                                                         
*                                                                               
GCTUNP   DC    AL1(GUNPLQ,GCUNP,L'MX@PRICE,L'UNPRICE)    PRICE                  
         DC    AL2(MX@PRICE-OVERWRKD),AL1(UNPELQ,UNPRICE-UNPELD)                
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0,0,0)                                                     
GUNPLQ   EQU   *-GCTUNP                                                         
*                                                                               
GCTXFRD  DC    AL1(GXFRDLQ,GCXFRD,L'MX@XFRD,1)           TRANSFER DATE          
         DC    AL2(MX@XFRD-OVERWRKD),AL1(PXDELQ,PXDDATE-PXDELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1(0,0,0,0)                                                     
GXFRDLQ  EQU   *-GCTXFRD                                                        
*                                                                               
GCTXFRT  DC    AL1(GXFRTLQ,GCXFRT,L'MX@XFRT,L'PXDFRTO)   TRANSFER TO            
         DC    AL2(MX@XFRT-OVERWRKD),AL1(PXDELQ,PXDFRTO-PXDELD)                 
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(PXDTTO,0,0,0)                                                
GXFRTLQ  EQU   *-GCTXFRT                                                        
*                                                                               
GCTXFRF  DC    AL1(GXFRFLQ,GCXFRF,L'MX@XFRF,L'PXDFRTO)   TRANSFER FROM          
         DC    AL2(MX@XFRF-OVERWRKD),AL1(PXDELQ,PXDFRTO-PXDELD)                 
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(PXDTFROM,0,0,0)                                              
GXFRFLQ  EQU   *-GCTXFRF                                                        
*                                                                               
GCTNARR  DC    AL1(GNARRLQ,GCNARR,L'LC@RSNAR,0)          NARRATIVE              
         DC    AL2(LC@RSNAR-WORKD),AL1(ELETSARQ,NARLN1Q)                        
         DC    AL1(GCTIELEM+GCTIELST,0,GCTFSIZ,GCTIEVL+GCTI1ST)                 
         DC    AL1(NARELQ,0),AL2(0)                                             
GNARRLQ  EQU   *-GCTNARR                                                        
*                                                                               
GCTGST   DC    AL1(GGSTLQ,GCGST,L'MX@VAT,0)              GST                    
         DC    AL2(MX@VAT-OVERWRKD),AL2(AGRDSP-OVERWRKD)                        
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI2ND+GCTICAN)                       
         DC    AL1(0,0),AL2(0)                                                  
GGSTLQ   EQU   *-GCTGST                                                         
*                                                                               
GCTPST   DC    AL1(GPSTLQ,GCPST,L'AC@PST,0)              PST                    
         DC    AL2(AC@PST-OVERWRKD),AL2(AGRDSP-OVERWRKD)                        
         DC    AL1(GCTIOVER+GCTIROUT,0,0,GCTI2ND+GCTICAN)                       
         DC    AL1(0,0),AL2(0)                                                  
GPSTLQ   EQU   *-GCTPST                                                         
*                                                                               
GCTDRAFT DC    AL1(GDRAFLQ,GCDRAFT,L'MX@DRAFT,TRNSDRFT)  DRAFT                  
         DC    AL2(MX@DRAFT-OVERWRKD,TSDTRST-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GDRAFLQ  EQU   *-GCTDRAFT                                                       
*                                                                               
GCTHELD  DC    AL1(GHELDLQ,GCHELD,L'MX@HELD,TRNSHOLD)     HELD                  
         DC    AL2(MX@HELD-OVERWRKD,TSDTEST-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GHELDLQ  EQU   *-GCTHELD                                                        
*                                                                               
GCTSEQ   DC    AL1(GSEQLQ,GCSEQ,L'MX@SEQ,1)               SEQUENCE              
         DC    AL2(MX@SEQ-OVERWRKD,TSDSBREF-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFRGHT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GSEQLQ   EQU   *-GCTSEQ                                                         
*   -----------------------------------                                         
*CTDUEDT DC    AL1(GDUEDLQ,GCDUE,L'MX@RSDUD,2)           DUE DATE               
*        DC    AL2(MX@RSDUD-OVERWRKD),AL1(DUEELQ,DUEDATE-DUEELD)                
*        DC    AL1(GCTIOVER+GCTIELEM,0,GCTFDAT+GCTFRGHT,0)                      
*        DC    AL1(0,0),AL2(0)                                                  
*DUEDLQ  EQU   *-GCTDUEDT                                                       
*                                                                               
GCTINVN  DC    AL1(GINVNLQ,GCINVN,L'MX@INVCN,0)              INVOICE            
         DC    AL2(MX@INVCN-OVERWRKD),AL1(FFTELQ,FFTDATA-FFTELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,GCTIEVL)                      
         DC    AL1(FFTTINVN,0),AL2(0)                                           
GINVNLQ  EQU   *-GCTINVN                                                        
GCTINV3  DC    AL1(GINV3LQ,GCINVN,L'MX@INVCN,L'TSDTREF)                         
         DC    AL2(MX@INVCN-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GINV3LQ  EQU   *-GCTINV3                                                        
*                                                                               
GUSER    DC    AL1(GUSERLQ,GCUSRID,L'MX@USRID,L'TSDUSRID)    USER-ID            
         DC    AL2(MX@USRID-OVERWRKD,TSDUSRID-TSARDATD)                         
         DC    AL1(GCTIOVER,0,0,GCTISPC)                                        
         DC    AL1(0,0),AL2(0)                                                  
GUSERLQ  EQU   *-GUSER                                                          
*                                                                               
GSOON    DC    AL1(GSOONLQ,GCSOON,L'MX@SOON,TRSSOON)         SOON               
         DC    AL2(MX@SOON-OVERWRKD,TSDSTAT3-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,GCTISPC)                                  
         DC    AL1(0,0),AL2(0)                                                  
GSOONLQ  EQU   *-GSOON                                                          
*                                                                               
GPIDN    DC    AL1(GPIDNLQ,GCPIDN,L'MX@PID,L'TSDPIDN)       PID                 
         DC    AL2(MX@PID-OVERWRKD,TSDPIDN-TSARDATD)                            
         DC    AL1(GCTIOVER,0,0,GCTISPC)                                        
         DC    AL1(0,0),AL2(0)                                                  
GPIDNLQ  EQU   *-GPIDN                                                          
*                                                                               
*        -----------------------------------                                    
*                                                                               
*----------*                                                                    
* DDS ONLY *                                                                    
*----------*                                                                    
GCTPEEL  DC    AL1(GPEELLQ,GCPEELDT,L'UP@PELDS,2)      PEELED DATE              
         DC    AL2(UP@PELDS-WORKD,TSDPEDAT-TSARDATD)                            
         DC    AL1(0,GCTIDDS,GCTFDAT+GCTFRGHT,0)                                
         DC    AL1(0,0,0,0)                                                     
GPEELLQ  EQU   *-GCTPEEL                                                        
*                                                                               
GCTTRNST DC    AL1(GTRNSTLQ,GCTRNST,L'LC@STT,L'TSDTEST)   TRN STAT              
         DC    AL2(LC@STT-WORKD,TSDTEST-TSARDATD)                               
         DC    AL1(0,GCTIHEX+GCTIDDS,0,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
GTRNSTLQ EQU   *-GCTTRNST                                                       
*                                                                               
GCTDA    DC    AL1(GDALQ,GCDADDR,L'MX@DSCAD,L'TSDDA)      DISK ADDRESS          
         DC    AL2(MX@DSCAD-OVERWRKD,TSDDA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIHEX+GCTIDDS,0,0)                                
         DC    AL1(0,0),AL2(0)                                                  
GDALQ    EQU   *-GCTDA                                                          
*                                                                               
GPAYN    DC    AL1(GPAYNLQ,GCPAYN,L'MX@PAYAM,L'TSDPAMNT)  PAYMENT AMT           
         DC    AL2(MX@PAYAM-OVERWRKD,TSDPAMNT-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIDDS,GCTFNUM+GCTFRGHT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GPAYNLQ  EQU   *-GPAYN                                                          
*                                                                               
GCPYREF  DC    AL1(GPREFLQ,GCPREF,L'MX@PAYRF,L'TSDPYREF)   PAYMENT REF          
         DC    AL2(MX@PAYRF-OVERWRKD,TSDPYREF-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIDDS,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GPREFLQ  EQU   *-GCPYREF                                                        
*                                                                               
GCTPYDT  DC    AL1(GPYDTLQ,GCPAYDT,L'MX@PAYDT,L'TSDPYDAT)  PAYMENT DATE         
         DC    AL2(MX@PAYDT-OVERWRKD,TSDPYDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIDDS,GCTFDAT+GCTFRGHT,0)                         
         DC    AL1(0,0,0,0)                                                     
GPYDTLQ  EQU   *-GCTPYDT                                                        
*                                                                               
*---------------------------------------------------------------------          
*                                                                               
GCTAPEL  DC    AL1(GAPELLQ,GCAPE,L'MX@ATTRS,0)        ANALYSIS POINTER          
         DC    AL2(MX@ATTRS-OVERWRKD),AL2(AGRDSP-OVERWRKD)                      
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GAPELLQ  EQU   *-GCTAPEL                                                        
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        JOB DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD             *         
*        - THIS TABLE MUST MATCH ABOVE, BUT IS DEFINED FOR WORKCODE   *         
*          SUBTOTALS                                                  *         
***********************************************************************         
GC2BL    DS    0F                                                               
*                                                                               
         DC    AL1(GNARRLQ,GCWCN,0,0)                                           
         DC    AL2(0),AL2(AGRDSP-OVERWRKD)                                      
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL2(0),AL2(0)                                                    
*                                                                               
*        DC    AL1(GWCDLQ,GCWCN,L'LC@WC2,0)             WORK CODE               
*        DC    AL2(LC@WC2-WORKD,AGRDSP-OVERWRKD)                                
*        DC    AL1(GCTIROUT,0,0,0)                                              
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    AL1(GWCDLQ,GCWCD,L'LC@WC2,L'TSWWORK)     WORK CODE               
*        DC    AL2(LC@WC2-WORKD,TSWWORK-TSARWRKD)                               
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
**NOP                                                                           
*        DC    AL1(GWCNLQ,GCWCN,L'LC@WC2,L'WCDESC)       WORK CODE              
*        DC    AL2(LC@WC2-WORKD,WCDESC-OVERWRKD)                                
*        DC    AL1(0,GCTISPDA,0,0)                                              
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GCONLQ,GCCONTRA,0,L'MX@WCTOT)        CONTRA                  
         DC    AL2(0,MX@WCTOT-OVERWRKD)                                         
         DC    AL1(0,GCTISPDA,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GNARRLQ,GCNARR,0,0)                  NARRATIVE               
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,0)                                                     
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(GDATELQ,GCDATE,0,0)             DATE                         
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GREFLQ,GCREF,0,0)               REFERENCE                    
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    AL1(GMOALQ,GCMOA,0,0)               MOA                          
*        DC    AL2(0,0)                                                         
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GORDLQ,GCORD,0,0)               ORDER NUMBER                 
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GAMTLQ,GCAMNT,0,L'TSWAMNT)      AMOUNT                       
         DC    AL2(0,TSWAMNT-TSARWRKD)                                          
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GDRCRLQ,GCDRCRQ,0,0)            DR/CR                        
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GBREFLQ,GCBREF,0,0)             BATCH REFERENCE              
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GTRNTYLQ,GCTRNTYP,0,0)          TRN TYPE                     
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GACDLQ,GCACTDTE,0,0)            ACTIVITY DATE                
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GBILDLQ,GCBILLED,0,0)           BILLED?                      
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GBILTYLQ,GCBILTY,0,0)           BILLING TYPE                 
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,GCTI2ND)                                               
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(GBRUNLQ,GCBRUND,0,0)            BILLING RUN DATE             
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GCOMMLQ,GCCOMM,0,0)             COMMISSION                   
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,GCTI2ND)                                               
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(GNETALQ,GCGRSBIL,0,0)           NET AMOUNT                   
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,GCTI2ND)                                               
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(GHRSLQ,GCHOURS,0,0)             HOURS                        
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GTIMTLQ,GCTIMT,0,0)             TIME TYPE                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GUNILQ,GCUNI,0,0)               UNIT                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GUNPLQ,GCUNP,0,0)               PRICE                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GXFRDLQ,GCXFRD,0,0)             TRANSFER DATE                
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GXFRTLQ,GCXFRT,0,0)             TRANSFER TO                  
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GXFRFLQ,GCXFRF,0,0)             TRANSFER FROM                
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GNARRLQ,GCNARR,0,L'TSWDESC)     NARRATIVE                    
         DC    AL2(0),AL2(TSWDESC-TSARWRKD)                                     
         DC    AL1(0,0,0,0)                                                     
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(GGSTLQ,GCGST,0,0)               GST                          
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,GCTI2ND+GCTICAN)                                       
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPSTLQ,GCPST,0,0)               PST                          
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(0,0,0,GCTI2ND+GCTICAN)                                       
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPEELLQ,GCPEELDT,0,0)           PEELED DATE                  
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(GDRAFLQ,GCDRAFT,0,0)            DRAFT                        
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GHELDLQ,GCHELD,0,0)             HELD                         
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GSEQLQ,GCSEQ,0,0)               SEQUENCE                     
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GTRNSTLQ,GCTRNST,0,0)           TRN STAT                     
         DC    AL2(0,0)                                                         
         DC    AL1(0,GCTIDDS,0,0)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GDALQ,GCDADDR,0,0)              DISK ADDRESS                 
         DC    AL2(0,0)                                                         
         DC    AL1(0,GCTIDDS,0,0)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GAPELLQ,GCAPE,0,0)              ANALYSIS POINTER             
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    AL1(GDUEDLQ,GCDUE,0,0)              DUE DATE                     
*        DC    AL2(0,0)                                                         
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GINVNLQ,GCINVN,0,0)             INVOICE                      
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GUSERLQ,GCUSRID,0,0)            USER-ID                      
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,GCTISPC)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GSOONLQ,GCSOON,0,0)             SOON                         
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,GCTISPC)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPIDNLQ,GCPIDN,0,0)             PID                          
         DC    AL2(0,0)                                                         
         DC    AL1(0,0,0,GCTISPC)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPAYNLQ,GCPAYN,0,0)            PAYMENT AMOUNT                
         DC    AL2(0,0)                                                         
         DC    AL1(0,GCTIDDS,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPREFLQ,GCPREF,0,0)            PAYMENT REF                   
         DC    AL2(0,0)                                                         
         DC    AL1(0,GCTIDDS,0,0)                                               
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    AL1(GPYDTLQ,GCPAYDT,0,0)          PAYMENT DATE                   
         DC    AL2(0,0)                                                         
         DC    AL1(0,GCTIDDS,0,0)                                               
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
* JOB DETAIL GRID COLUMN EQUATES                                                
*---------------------------------------------------------------------          
GCWCD    EQU   91       WORK CODE                                               
GCWCN    EQU   92       WORK CODE DESCRIPTION                                   
GCCONTRA EQU   2        CONTRA                                                  
GCDATE   EQU   3        DATE                                                    
GCREF    EQU   4        REFERENCE                                               
GCBREF   EQU   5        BATCH REFERENCE                                         
GCAMNT   EQU   6        AMOUNT                                                  
GCDRCRQ  EQU   7        DR/CR                                                   
GCBILLED EQU   8        BILLED?                                                 
GCHOURS  EQU   9        HOURS                                                   
GCNARR   EQU   10       NARRATIVE                                               
GCBILTY  EQU   11       BILLING TYPE                                            
GCGRSBIL EQU   12       GROSS BILLED AMOUNT                                     
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
GCCNA    EQU   32       CONTRA NAME                                             
GCINVN   EQU   33       INVOICE NUMBER                                          
*GCDUE    EQU   34       DUE DATE                                               
GCSOON   EQU   35       SOON                                                    
GCUSRID  EQU   36       USER-ID                                                 
GCPIDN   EQU   37       PID                                                     
* DDS ONLY *                                                                    
GCPAYN   EQU   38       PAYMENT AMOUNT                                          
GCPREF   EQU   39       PAYMENT REF                                             
GCPAYDT  EQU   40       PAYMENT DATE                                            
GCTRNST  EQU   82       TRANSACTION STATUS                                      
GCDADDR  EQU   83       DISK ADDRESS                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
OVERWRKD DSECT                                                                  
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGC2BL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGRDSP   DS    A                   ADDRESS OF GRID SPECIAL COLUMN CALCS         
ATMPGST  DS    A                   ADDRESS OF GST/PST TEMP TABLE                
DADDRESS DS    CL(L'TRNKDA)        TRANSACTION DISK ADDRESS                     
ACCNAM   DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ACCNAMLN DS    X                   ACCOUNT NAME LENGTH                          
JOBSTAT  DS    XL(L'JOBSTA1)       JOB STATUS                                   
NEXTWC   DS    XL(L'TRNKWORK)      NEXT WORK CODE                               
VATVL    DS    0C                                                               
VATNAME  DS    XL6                 VAT TYPE NAME                                
VATRULE  DS    XL(L'VTCTYPE)       VAT TYPE                                     
VATRATE  DS    XL(L'RATRATE)       VAT RATE                                     
VATINDS  DS    XL1                 INDICATOR BYTE                               
VATIDC3  EQU   TAXIDC3             X'20' = RATE IS 3-DECIMAL PLACES             
VATVLL1Q EQU   *-VATVL                                                          
VATAMNT  DS    PL(L'VBIVAT)        VAT AMOUNT                                   
GSTDES   DS    CL20                GST DESCRIPTION                              
PSTDES   DS    CL20                PST DESCRIPTION                              
ANXTLINE DS    A                   ADDRESS OF NEXT DUMMY LINE                   
ORD      DS    PL(L'FFNONUM)       TEMP ORDER NUM                               
WCDESC   DS    CL(L'WCODESC)       WORKCODE DESCPRIPTION                        
WORKHOUR DS    PL(L'WHRSTOT)       HOURS FOR WORKCODE                           
TEMPNUM  DS    PL8                 TEMPORARY NUMBER STORAGE                     
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
INTFLAG  DS    X                   GENERAL INTERNAL FLAG                        
SCRFULLQ EQU   X'01'               SCREEN FULL                                  
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
TRANAMNT DS    CL(L'TRNAMNT)       TRANSACTION AMOUNT LOCAL/AGENCY              
TEMPCNTR DS    CL36                CURRENT CONTRA                               
TEMPGST  DS    CL(L'TEMP)          TEMPORARY GST/PST BUILD AREA                 
BTYPE    DS    CL(L'TSDTTYPE)      SAVE THE TYPE FOR STANDLNS                   
*                                                                               
*---------------------------------------------------------------------*         
* DATA DICTIONARY                                                               
*---------------------------------------------------------------------*         
DSMIX    DS    0C                                                               
MX@ENH16 DS    CL(L'ENQDAT1)                                                    
MX@ENH17 DS    CL(L'ENQDAT1)                                                    
MX@ACC   DS    CL9                                                              
MX@XJOB  DS    CL11                                                             
MX@DRAFT DS    CL9                                                              
MX@CTR   DS    CL9                                                              
MX@NET   DS    CL7                                                              
MX@DISS  DS    CL7                                                              
MX@PAYBL DS    CL7                                                              
MX@FLTX  DS    CL7                                                              
MX@ORDER DS    CL7                                                              
MX@SUBR  DS    CL7                                                              
MX@ATTRS DS    CL10                                                             
MX@ATHED DS    CL9                                                              
MX@UATH  DS    CL9                                                              
MX@HELD  DS    CL9                                                              
MX@HOURS DS    CL5                                                              
MX@TIME  DS    CL5                                                              
MX@TMTYP DS    CL5                                                              
MX@TYPE  DS    CL2,CL1                                                          
MX@STT   DS    CL2,CL1                                                          
MX@OTHER DS    CL2,CL1                                                          
MX@UDAT  DS    CL2,CL1                                                          
MX@DATE  DS    CL2,CL1                                                          
MX@PELDT DS    CL2,CL1                                                          
MX@CR    DS    CL2,CL1                                                          
MX@DR    DS    CL2,CL1                                                          
MX@SEQ   DS    CL2,CL1                                                          
MX@PCLST DS    CL2,CL1                                                          
MX@ACCT  DS    CL15                                                             
MX@ACCTS DS    CL15                                                             
MX@WCTOT DS    CL15                                                             
MX@INT   DS    CL15                                                             
MX@EXT   DS    CL15                                                             
MX@CMN   DS    CL10                                                             
MX@VAT   DS    CL5                                                              
MX@TO2   DS    CL5                                                              
MX@FROM  DS    CL5                                                              
MX@TYPE1 DS    CL5                                                              
MX@XFR   DS    CL12                                                             
MX@PAYEE DS    CL10                                                             
MX@LCLCU DS    CL19                                                             
MX@PRICE DS    CL5                                                              
MX@UNIT  DS    CL5                                                              
MX@BLG   DS    CL10                                                             
MX@DSCAD DS    CL3                                                              
MX@BLGTY DS    CL12                                                             
MX@DRCR  DS    CL3                                                              
MX@TIMTP DS    CL9                                                              
MX@RSBTD DS    CL16              BILLING RUN DATE                               
MX@ACTDT DS    CL13              ACTIVITY DATE                                  
MX@RSPO  DS    CL12              ORDER NUMBER                                   
MX@MOA   DS    CL3               MOA                                            
MX@TRAND DS    CL10              TRANS DATE                                     
MX@TRANR DS    CL9               TRANS REF                                      
MX@TRANT DS    CL10              TRANS TYPE                                     
MX@RSBRF DS    CL9               BATCH REF                                      
MX@RSBCM DS    CL17              COMMISSION AMOUNT                              
MX@GRSBD DS    CL12              GROSS BILLED                                   
MX@XFRD  DS    CL13              TRANSFER DATE                                  
MX@XFRT  DS    CL19              TRANSFER TO ACCOUNT                            
MX@XFRF  DS    CL21              TRANSFER FROM ACCOUNT                          
MX@RSWNO DS    CL11              WRITE-OFF #                                    
MX@RSWDT DS    CL14              WRITE-OFF DATE                                 
MX@CTRAN DS    CL19              CONTRA ACCOUNT NAME                            
*MX@RSDUD DS    CL8               DUE DATE                                      
MX@INVCN DS    CL14              INVOICE NUMBER                                 
MX@SOON  DS    CL4               SOON                                           
MX@USRID DS    CL7               USER-ID                                        
MX@PID   DS    CL3               PID                                            
MX@PYMNT DS    CL6              PAYMENT                                         
MX@PAYR2 DS    CL6              PAYMENT REFERENCE                               
MX@PAYD2 DS    CL7              PAYMENT DATE                                    
MX@PAYAM DS    CL14             PAYMENT AMOUNT                                  
MX@PAYRF DS    CL11             PAYMENT REF                                     
MX@PAYDT DS    CL12             PAYMENT DATE                                    
*                                                                               
DSCAP    DS    0C                                                               
AC@ONT   DS    CL3                                                              
AC@PST   DS    CL3                                                              
AC@QST   DS    CL3                                                              
AC@HST   DS    CL3                                                              
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
*                                                                               
PRATBLK  DS    XL(PR$LNQ)          PRORATA BLOCK                                
*                                                                               
***********************************************************************         
* SCREEN LINES                                                        *         
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1WORK DS    CL(L'TRNKWORK)      WORKCODE                                     
         DS    CL1                                                              
SCR1CONT DS    CL(L'TRNKULA)       CONTRA CODE                                  
         DS    CL1                                                              
SCR1TDAT DS    CL8                 TRANSACTION DATE                             
         DS    CL1                                                              
SCR1TREF DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL1                                                              
SCR1BREF DS    CL6                 BATCH REFERENCE                              
         DS    CL1                                                              
SCR1DESC DS    CL20                WIDE DESCRIPTION COLUMN                      
         DS    CL1                                                              
SCR1USED DS    CL1                 BILLED STATUS                                
SCR1AMNT DS    CL13                AMOUNT                                       
         ORG   SCR1DESC            FOREIGN CURR FORMAT (VALUE=LOCAL)            
SCR1FDSC DS    CL17                NARROW DESCRIPTION COLUMN                    
         DS    CL1                                                              
SCR1FUSD DS    CL1                 BILLED STATUS                                
SCR1FAMT DS    CL16                EXTRA WIDE AMOUNT COL FOR CURR CODE          
         ORG                                                                    
SCR1SIGN DS    CL2                 DR/CR                                        
         ORG                                                                    
SCR1LNQ  EQU   *-SCRLIN1D                                                       
*                                                                               
SCRWORKD DSECT                     COVER SCREEN ITEM LINE1                      
SCRWWORK DS    CL(L'TRNKWORK)      WORKCODE                                     
         DS    CL1                                                              
SCRWDESC DS    CL(L'WCODESC)       WORKCODE DESCRIPTION                         
         DS    CL1                                                              
SCRWWTOT DS    CL(L'MX@WCTOT)      TOTAL                                        
         DS    CL7                                                              
SCRWINFO DS    CL20                WORKCODE TOTAL INFO                          
         DS    CL1                                                              
SCRWAMNT DS    CL14                WORKCODE AMOUNT                              
*                                                                               
         ORG   SCRWINFO+17         FOREIGN CURR FORMAT (VALUE=LOCAL)            
SCRWFAMT DS    CL18                EXTRA WIDE AMOUNT COL FOR CURR CODE          
         ORG                                                                    
SCRWLNQ  EQU   *-SCRWORKD                                                       
*                                                                               
SCRIXD   DSECT                     INTERNAL/EXTERNAL SCREEN LINE                
         DS    CL3                                                              
SCRIXINT DS    CL(L'MX@INT)        INTERNAL INFO                                
         DS    CL11                                                             
SCRIXIT  DS    CL12                INTERNAL TOTAL                               
         DS    CL1                                                              
SCRIXEXT DS    CL(L'MX@EXT)        EXTERNAL INFO                                
         DS    CL7                                                              
SCRIXET  DS    CL12                EXTERNAL TOTAL                               
*                                                                               
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE                      
SCRTOTAL DS    CL(L'MX@ACCT)                                                    
         DS    CL26                                                             
SCRTHTOT DS    CL20                HOURS TOTAL                                  
SCRTTTOT DS    CL15                AGENCY TOTAL                                 
         ORG   SCRTHTOT+17                                                      
SCRTFTO2 DS    CL18                LOCAL/AGENCY CURRENCY TOTAL                  
         ORG                                                                    
SCRTLNQ  EQU   *-SCRTOT1D                                                       
*                                                                               
***********************************************************************         
* TSAR RECORD. NOTE THIS RECORD MAY HAVE ELEMENTS ATTACHED                      
* POSSIBLE GENFILE ELEMENTS INCLUDE                                             
* APEELD 'ANALYSIS POINTER' ELEMENT                                             
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
TSDSTAT3 DS    XL(L'TRSSTAT3)      TRANSACTION STATUS ELE STAT BYTE 3           
TSDUSER  DS    XL(L'TRSUSER)       USER-ID (HEX)                                
TSDUSRID DS    XL10                USER-ID (CONVERTED)                          
TSDXPID  DS    XL2                 HEX PID                                      
TSDPIDN  DS    CL8                 PERSONAL ID                                  
TSDPAMNT DS    PL(L'SCIAMNT)       PAYMENT AMOUNT                               
TSDPYDAT DS    XL(L'SCITCPDT)      PAYMENT DATE                                 
TSDPYREF DS    CL(L'SCITCPNO)      PAYMENT REF                                  
TSDALLOC DS    XL1                 ALLOCATION BYTE                              
TSDALLFB EQU   C'*'                FULLY ALLOCATED                              
TSDALLPB EQU   C'P'                PARTLY ALLOCATED                             
TSDALLPA EQU   C'A'                ALLOCATED                                    
TSDALLBI EQU   C'B'                THIS 99 IS BILLED                            
TSDALLUN EQU   C'U'                THIS 99 IS UNBILLED                          
TSDALLRB EQU   C'R'                THIS 99 IS A REVERSAL                        
TSDDA    DS    CL(L'TRNKDA)        DISK ADDRESS                                 
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARWRKD DSECT                     COVER SCREEN LINE 1                          
TSWLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSWFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSWRKITM EQU   3                   WORKCODE TOTAL ITEM TYPE                     
TSWWORK  DS    CL(L'TRNKWORK)      WORK CODE                                    
TSWDESC  DS    CL(L'WCODESC)       WORKCODE DESCRIPTION                         
TSWAMNT  DS    PL(L'TRNAMNT)       WORKCODE NET AMOUNT                          
TSWHOURS DS    PL(L'WHRSTOT)       TOTAL HOURS FOR WORKCODE                     
TSWLNQ   EQU   *-TSARWRKD                                                       
*                                                                               
TSARIXD  DSECT                     INTERNAL/EXTERNAL LINE                       
TSILINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSIFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSIXITM  EQU   4                   TOTAL ITEM TYPE                              
TSIITOT  DS    PL(L'INTTOT)        INTERNAL TOT                                 
TSIETOT  DS    PL(L'EXTTOT)        EXTERNAL TOT                                 
TSILNQ   EQU   *-TSARIXD                                                        
*                                                                               
TSARTOTD DSECT                     TOTAL LINE                                   
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTHTOT  DS    PL(L'HRSTOT)        HOURS AMOUNT                                 
TSTTTOT  DS    CL(L'TRNTOT)        TRANSACTION AMOUNT                           
TSTLNQ   EQU   *-TSARTOTD                                                       
*                                                                               
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
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
*                                                                               
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
APRVTAB  DS    A                   ADDRESS OF THE PROVINCE TABLE                
SAVEWC   DS    CL(L'TRNKWORK)      WORK CODE                                    
SECALPHA DS    CL2                                                              
**NOP                                                                           
*VWCDESC DS    CL(L'WCODESC)       WORKCODE DESCPRIPTION                        
JDTFLAG  DS    X                   GENERAL FLAG FOR OVERLAYS                    
JDTCOLMQ EQU   X'80'               COLUMN OVERRIDE                              
JDTFWCQ  EQU   X'40'               FIRST TRANSACTION FOR WC                     
COMBVATR DS    (MAXNVATQ+1)XL9     VATNAME VATTYPE VATRATE                      
CURCNTR  DS    CL36                CURRENT CONTRA                               
JDVALS   DS    0PL8                CREDITOR VALUES                              
TRNTOT   DS    PL8                 TRANSACTION TOTAL                            
HRSTOT   DS    PL8                 HOURS TOTAL                                  
EXTTOT   DS    PL8                 EXTERNAL TOTAL                               
INTTOT   DS    PL8                 INTERNAL TOTAL                               
JDVALLNQ EQU   *-JDVALS                                                         
WHRSTOT  DS    PL6                 HOURS TOTAL FOR WORKCODE                     
WTRNTOT  DS    PL6                 TRANSACTION TOTAL FOR WORKCODE               
PREVBTOT DS    0PL6                PREV BILL TOTALS                             
PREVBCOM DS    PL6                 PREV BILL COMMISSION                         
PREVBDIS DS    PL6                 PREV BILL DISCOUNT                           
PREVBPAY DS    PL6                 PREV BILL PAYABLE                            
PREVBVAT DS    (MAXNVATQ)PL6       PREV BILL VAT                                
PREVBLNQ EQU   *-PREVBTOT          LENGTH OF PREV BILLING ACCUMULATORS          
PREVBNMQ EQU   PREVBLNQ/L'PREVBTOT NUMBER OF PREV BILLING ACCUMULATORS          
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACENQ06   05/17/18'                                      
         END                                                                    
