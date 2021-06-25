*          DATA SET ACENQ11    AT LEVEL 008 AS OF 12/22/17                      
*PHASE T62011A                                                                  
T62011   TITLE 'ACCOUNT ENQUIRY - JOB BILLING ALLOCATION'                       
T62011   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ11**,R6,R7,CLEAR=YES,RR=RE                                
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         A     RE,=A(GCTBL)                                                     
         ST    RE,AGCTBL                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
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
         BNO   *+12                                                             
         TM    OVRSTAT,OVRREVQ     REVERSAL?                                    
         BNO   MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN30                                                           
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
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         B     MAIN110                                                          
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
MAIN55   MVC   KEYSAVE,IOKEY                                                    
MAIN60   GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN70                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN70   LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING RECORD)              
         USING TRNRECD,R3                                                       
         CLC   TRNKCULA,IOKEYSAV                                                
         BE    MAIN90                                                           
*                                                                               
MAIN80   OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   MAIN85                                                           
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         TM    OVRSTAT,OVRREVQ     REVERSALS FOUND?                             
         BNO   MAINX                                                            
         B     *+8                                                              
MAIN85   BAS   RE,WORKTOT          DEAL WITH WORK CODE TOTALS                   
         OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         B     MAINX                                                            
*                                                                               
MAIN90   LA    R3,IOKEY                                                         
*                                                                               
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BNH   MAIN55                                                           
         BAS   RE,FILTKEY          APPLY FILTERING TO KEY                       
         BNE   MAIN55              DO WE WANT TO KEEP THIS RECORD?              
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET ACCMST RECORD                         
         BE    MAIN95                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN95   L     R3,AIO1                                                          
         BAS   RE,FILTER           APPLY FILTERING TO TRANS RECORD              
         BNE   MAIN55              DO WE WANT TO KEEP THIS RECORD?              
         CLC   SAVEWC,SPACES                                                    
         BH    *+14                                                             
         MVC   SAVEWC,TRNKWORK                                                  
         B     MAIN100                                                          
         CLC   SAVEWC,TRNKWORK                                                  
         BNL   MAIN100                                                          
         MVC   NEXTWC,TRNKWORK                                                  
         OI    CURRFLAG,CURRSFSQ   SET FIRST FOR WC                             
         BAS   RE,WORKTOT          DEAL WITH WORK CODE TOTALS                   
         BNE   MAINX                                                            
MAIN100  MVC   KEYSAVE,TRNKEY      SAVE THE KEY FOR SEQ RESTORE                 
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE(S)           
         BNE   ERRXIT                                                           
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
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         ZAP   WTRNTOT,=P'0'       WORK CODE TRANS TOTAL                        
         ZAP   WALLTOT,=P'0'       WORK CODE ALLOC TOTAL                        
         ZAP   WBALTOT,=P'0'       WORK CODE BALANCE TOTAL                      
         ZAP   ITRNTOT,=P'0'       INVOICE TRANS TOTAL                          
         ZAP   IALLTOT,=P'0'       INVOICE ALLOC TOTAL                          
         ZAP   IBALTOT,=P'0'       INVOICE BALANCE TOTAL                        
         ZAP   TALLTOT,=P'0'       INVOICE ALLOC TOTAL                          
         ZAP   NETTOT,=P'0'        ACCOUNT NET TOTAL                            
         MVI   CURRFLAG,0          INIT CURRENCY FLAG                           
         MVC   CURRLAST,SPACES     LAST CURRENCY CODE                           
         MVC   SAVEWC,SPACES       SAVED WC                                     
         MVC   SVCACN,SPACES       AND CONTRA NAME                              
         MVC   CONTRA,SPACES                                                    
         MVC   UNILDG,SPACES                                                    
         MVI   NEGCONT,0           NEGATIVE CONTRA                              
         MVI   CONTLEN,0           CONTRA CODE LENGTH                           
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
         MVC   UNILDG,SPROUNIT                                                  
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
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC) CHECK OFFICE/SECURITY                     
         GOTO1 AOFFACC                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    FSTDX                                                            
         CLI   OFFLFLAG,OFFLSEC                                                 
         BE    FSTDERR                                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BNO   FSTD23                                                           
         GOTO1 ADISACC                                                          
         B     FSTD25                                                           
*                                                                               
FSTD23   GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
         GOTO1 ADISACC                                                          
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
         BO    FSTD60                                                           
         SR    RE,RE               GET ACCOUNT NAME                             
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),NAMEREC                                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD60                                                           
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
         MVC   FLDDATA(L'MX@ENH20),MX@ENH20                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH21),MX@ENH21                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
FSTD60   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD64                                                           
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD65                                                           
*                                                                               
FSTD64   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         B     FSTD66                                                           
                                                                                
*                                                                               
FSTD65   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
         DROP  R2                                                               
*                                                                               
FSTD66   LA    R3,IOKEY            READ FIRST CONTRA RECORD                     
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
         BE    FSTDX                                                            
         OI    DISPFLAG,NORECQ     NO RECORDS ON JOB                            
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER TRANSACTION KEYS                                      *         
* ON ENTRY R3=A(TRANSACTION KEY)                                      *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING TRNRECD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,1,CONTLEN        RE=L'(INPUT CONTRA CODE)                     
         BZ    FILTK05                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),CONTRA   MATCHED ON CONTRA?                           
         BE    FILTK04                                                          
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    FILTK05                                                          
         B     FILTKRJX                                                         
FILTK04  CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BNE   FILTK05                                                          
         B     FILTKRJX                                                         
*                                                                               
FILTK05  TM    TRNKSTAT,TRNSREVS   REVERSAL?                                    
         BNO   *+8                                                              
         OI    OVRSTAT,OVRREVQ     ACCOUNT HAS REVERSED TRANSACTIONS            
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
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
FILTK20  TM    TRNKSTAT,TRNSDRFT   DRAFT?                                       
         BNO   FILTK110                                                         
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK120                                                         
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK120                                                         
         B     FILTKRJX                                                         
FILTK110 CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
FILTK120 TM    TRNKSTA2,TRNSPEEL    PEELED?                                     
         BNO   FILTK130                                                         
         CLI   OPEELED,C'Y'                                                     
         BE    FLTK130A                                                         
         CLI   OPEELED,C'O'                                                     
         BE    FLTK130A                                                         
         B     FILTKRJX                                                         
FILTK130 CLI   OPEELED,C'O'                                                     
         BE    FILTKRJX                                                         
*                                                                               
FLTK130A TM    TRNKSTA2,TRNSEXCL   CONTRA'D?                                    
         BNO   FILTK131                                                         
         CLI   OCONT,C'Y'                                                       
         BE    FILTK140                                                         
         CLI   OCONT,C'O'                                                       
         BE    FILTK140                                                         
         B     FILTREJX                                                         
FILTK131 CLI   OCONT,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILTK140 CLC   TRNKWORK,ORDER      ORDER?                                       
         BNE   FILTK150                                                         
         OC    OORDNO,OORDNO       ORDER FILTER?                                
         BZ    FILTK150                                                         
         GOTO1 ASCOMP,DMCB,TRNKREF,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),C        
               OORDFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK150 OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILTK160                                                         
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
FILTK160 CLC   TRNKWORK,PREVBILL   PREVIOUS BILLING?                            
         BNE   FILTK165                                                         
         OC    OBILNO,OBILNO       BILL NUMBER FILTER?                          
         BZ    FILTK165                                                         
         GOTO1 ASCOMP,DMCB,TRNKREF,(OBILLN1,OBILVL1),(OBILLN2,OBILVL2),C        
               OBILFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK165 OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK170                                                         
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK170 OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
***********************************************************************         
*        FILTER ACCMST RECORD                                         *         
* ON ENTRY AIO1 CONTAINS ACCMST RECORD                                *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         L     R3,AIO1             R3=A(ACCMST RECORD)                          
         USING TRNRECD,R3                                                       
*                                                                               
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         CLC   TRNKWORK,PREVBILL   PREVIOUS BILLING?                            
         BE    FILT05                                                           
         CLI   OBILLED,0           BILLED FILTER?                               
         BNE   FILT04                                                           
         OC    OBILNO,OBILNO       BILL NUMBER FILTER?                          
         BNZ   FILT04                                                           
         CLI   OVALUE,0            LOCAL VALUE OPTION?                          
         BE    FILT05                                                           
FILT04   GOTO1 VPRORATA,DMCB,AIO1,0,ACOMFACS,0,PRATBLK,0                        
*                                                                               
         USING TRNELD,R4                                                        
FILT05   ICM   R4,15,ATRNELD       R4=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRANCURR,COMPCURR   SET TRANSACTION CURRENCY FROM AGENCY         
         ZAP   TRANAMNT,TRNAMNT    AGENCY TRANSACTION AMOUNT                    
*                                                                               
         USING AFCELD,RF           ACCOUNT FOREIGN CURRENCY ELEMENT             
         ICM   RF,15,AAFCELD                                                    
         BZ    FILT10                                                           
         MVC   TRANCURR,AFCCURR    OVERRIDE TRANSACTION CURRENCY                
         CLI   OVALUE,C'L'         SHOW LOCAL CURRENCY                          
         BNE   FILT10                                                           
         CLC   TRNKWORK,PREVBILL   PREVIOUS BILLING?                            
         BE    FILT07                                                           
         LA    RE,PRATBLK                                                       
         USING PRORATAD,RE                                                      
         TM    PG$STAT,PG$FULLB+PG$PARTB BILLED?                                
         BZ    FILT07                                                           
         CLC   AFCCURR,PG$BLCUR    INVOICE CURR MATCHES BILLING CURR?           
         BNE   FILT10                                                           
FILT07   ZAP   TRANAMNT,AFCAMNT    LOCAL TRANSACTION AMOUNT                     
         DROP  RE,RF                                                            
*                                                                               
FILT10   OC    OCURRYVL,OCURRYVL   CURRENCY CODE FILTER?                        
         BZ    FILT20                                                           
         CLC   TRANCURR,OCURRYVL   MATCH ON CURRENCY CODE?                      
         BNE   FILT15                                                           
         CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BE    FILTREJX                                                         
         B     FILTX                                                            
FILT15   CLI   OCURRYFI,NEGFILTR   NEGATIVE FILTER?                             
         BNE   FILTREJX                                                         
*                                                                               
FILT20   OC    OAMOUNT,OAMOUNT     FILTERING TRANSACTION AMOUNT?                
         BZ    FILT90                                                           
         CP    OAMTVL,TRANAMNT     FILTER AMOUNT=TRANS AMOUNT?                  
         BE    FILT80                                                           
         CLI   OAMTSIGN,0          HAS A '+' OR '-' BEEN SPECIFIED?             
         BNE   FILT30                                                           
         CLI   OAMTRNG,0           HAS A '>' OR '<' BEEN SPECIFIED?             
         BNE   FILT30                                                           
         ZAP   TEMPNUM,OAMTVL      SEE IF AMOUNT IS -VE EQUIVALENT              
         AP    TEMPNUM,TRANAMNT                                                 
         BZ    FILT80              YES IT IS                                    
         B     FILT60                                                           
FILT30   CLI   OAMTRNG,C'>'        MORE THAN SIGN?                              
         BNE   FILT50                                                           
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT80                                                           
         B     FILT60                                                           
FILT50   CLI   OAMTRNG,C'<'        LESS THAN SIGN?                              
         BNE   FILT60                                                           
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT80                                                           
FILT60   CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BNE   FILTREJX                                                         
         B     FILT90                                                           
FILT80   CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BE    FILTREJX                                                         
*                                                                               
FILT90   CP    TRANAMNT,=P'0'                                                   
         BNL   FILT100                                                          
         CLI   ONEGATIV,C'N'       DO WE ONLY WANT +VE NUMBERS/ZERO?            
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT100  CLI   ONEGATIV,C'O'       DO WE ONLY WANT -VE NUMBERS?                 
         BE    FILTREJX                                                         
*                                                                               
FILT120  TM    TRNSTAT,TRNSAUTH    AUTHORISED?                                  
         BNO   FILT130                                                          
         CLI   OAUTH,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT130  CLI   OAUTH,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT150  TM    TRNSTAT,TRNSDR      DEBIT/CREDIT?                                
         BNO   FILT160                                                          
         CLI   OCREDIT,C'O'        ONLY CREDITS?                                
         BE    FILTREJX                                                         
         CLI   ODEBIT,C'N'         NO  DEBITS?                                  
         BE    FILTREJX                                                         
         B     FILT165                                                          
FILT160  CLI   ODEBIT,C'O'         ONLY DEBITS?                                 
         BE    FILTREJX                                                         
         CLI   OCREDIT,C'N'        NO CREDITS?                                  
         BE    FILTREJX                                                         
*                                                                               
FILT165  OC    OBATCH,OBATCH       BATCH REF FILTER?                            
         BZ    FILT190                                                          
         GOTO1 ASCOMP,DMCB,TRNBTCH,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT190  SR    RF,RF               INPUT TYPE FILTER?                           
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
FILT210  CLC   TRNKWORK,ORDER      ORDER TRANSACTION?                           
         BE    FILT225                                                          
         OC    OORDNO,OORDNO       ORDER NUMBER FILTER?                         
         BZ    FILT220                                                          
         OC    AFFNELD,AFFNELD     FREE FORM NUMBER ELEMENT?                    
         BZ    FILTREJX                                                         
*                                                                               
FILT220  CLC   TRNKWORK,PREVBILL   PREVIOUS BILLING?                            
         BE    FILT230                                                          
FILT225  OC    OBILNO,OBILNO       BILL NUMBER FILTER?                          
         BZ    FILT230                                                          
         OC    ABNDELD,ABNDELD     BILL NUMBER/DATE ELEMENT?                    
         BNZ   FILT230                                                          
         OC    APTAELD,APTAELD     PRODUCTION TRANS ACTIVITY ELEMENT?           
         BZ    FILTREJX                                                         
*                                                                               
FILT230  OC    OSUBREF,OSUBREF     SUB REFERENCE FILTER?                        
         BZ    *+14                                                             
         OC    AOTHELD,AOTHELD     OTHERS ELEMENT?                              
         BZ    FILTREJX                                                         
*                                                                               
         USING TRSELD,R4           TRANSACTION STATUS ELEMENT                   
         ICM   R4,15,ATRSELD                                                    
         BZ    FILT260                                                          
         OC    OACT,OACT          FILTERING ON ACTIVITY DATE?                   
         BZ    FILT245                                                          
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPPDAT) ACTIVITY DATE              
         GOTO1 ADCOMP,DMCB,(L'TEMPPDAT,TEMPPDAT),OACTST,OACTEN,OACTFI           
         BNE   FILTREJX                                                         
*                                                                               
FILT245  OC    OUSED,OUSED        FILTERING ON USED DATE?                       
         BZ    FILT250                                                          
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPPDAT) USED DATE                  
         GOTO1 ADCOMP,DMCB,(L'TEMPPDAT,TEMPPDAT),OUSEST,OUSEEN,OUSEFI           
         BNE   FILTREJX                                                         
*                                                                               
FILT250  TM    TRNRSTAT,TRNSREVS   REVERSAL?                                    
         BNO   FILT255                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT260                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT260                                                          
         OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS?                
         BZ    FILTREJX                                                         
         OC    TRSRMOS,TRSRMOS     IF NO REV MOS ASSUME SAME AS TRAN            
         BZ    FILTREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTREJX                                                         
         B     FILT260                                                          
FILT255  CLI   OREVERSE,C'O'                                                    
         BE    FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4           OTHERS ELEMENT                               
FILT260  ICM   R4,15,AOTHELD                                                    
         BZ    FILT270                                                          
         OC    OSUBREF,OSUBREF     FILTERING ON SUB REFERENCE                   
         BZ    FILT270                                                          
         GOTO1 ASCOMP,DMCB,OTHNUM,(OSUBRLN1,OSUBRVL1),(OSUBRLN2,OSUBRVLC        
               2),OSUBRFI                                                       
         BNE   FILTREJX                                                         
*                                                                               
         USING FFNELD,R4          FREE FORM NUMBER ELEMENT                      
FILT270  ICM   R4,15,AFFNELD                                                    
         BZ    FILT290                                                          
         OC    OORDNO,OORDNO      ORD NUMBER FILTER?                            
         BZ    FILT290                                                          
         MVC   ORD,SPACES                                                       
         SR    RF,RF                                                            
         IC    RF,FFNLN                                                         
         SH    RF,=Y(FFNLN1Q+1)    ANYTHING ON IT?                              
         BM    FILTREJX                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ORD(0),FFNONUM                                                   
         GOTO1 ASCOMP,DMCB,ORD,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),    X        
               OORDFI                                                           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*                                                                               
FILT290  CLC   TRNKWORK,ORDER      ORDER TRANSACTION?                           
         BNE   FILT295                                                          
         OC    OACT,OACT           FILTERING ON ACTIVITY DATE?                  
         BNZ   FILTREJX                                                         
         OC    OUSED,OUSED         FILTERING ON USED DATE?                      
         BNZ   FILTREJX                                                         
         CLI   OREVERSE,C'O'       ONLY REVERSALS?                              
         BE    FILTREJX                                                         
*                                                                               
FILT295  CLC   TRNKWORK,PREVBILL   PREVIOUS BILLING?                            
         BE    FILTX                                                            
         CLI   OBILLED,0           BILLED FILTER?                               
         BNE   *+14                                                             
         OC    OBILNO,OBILNO       BILL NUMBER FILTER?                          
         BZ    FILTX                                                            
         LA    R4,PRATBLK                                                       
         USING PRORATAD,R4                                                      
         TM    PG$STAT,PG$FULLB+PG$PARTB BILLED?                                
         BZ    FILT320                                                          
         CLI   OBILLED,C'N'                                                     
         BE    FILTREJX                                                         
         OC    OBILNO,OBILNO       FILTERING ON BILL NUMBER?                    
         BZ    FILTX                                                            
         GOTO1 ASCOMP,DMCB,PG$LBLNO,(OBILLN1,OBILVL1),(OBILLN2,OBILVL2)X        
               ,OBILFI                                                          
         BNE   FILTREJX                                                         
         B     FILTX                                                            
FILT320  CLI   OBILLED,C'O'                                                     
         BE    FILTREJX                                                         
         OC    OBILNO,OBILNO       FILTERING ON BILL NUMBER?                    
         BNZ   FILTREJX                                                         
*                                                                               
FILTX    CR    RB,RB                                                            
         B     XIT                                                              
FILTREJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY AIO1 CONTAINS TRANSACTION RECORD                           *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
BLDTSDAT NTR1                                                                   
         L     R3,AIO1                                                          
         USING TRNRECD,R3                                                       
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         MVI   INTFLAG,0                                                        
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         XC    TSDACDAT,TSDACDAT   CLEAR ACTIVITY DATE                          
         XC    TSDUSDAT,TSDUSDAT   CLEAR USED DATE                              
         XC    TSDPEDAT,TSDPEDAT   CLEAR PEELED DATE                            
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         MVC   TSDWORK,TRNKWORK    WORK CODE                                    
         MVC   TSDCONT,TRNKULC     CONTRA CODE                                  
         MVC   TSDTREF,TRNKREF     TRANSACTION REFERENCE NUMBER                 
         MVC   TSDTDAT,TRNKDATE    TRANSACTION DATE                             
         MVC   TSDTRST,TRNRSTAT    TRANSACTION RECORD STATUS                    
         MVC   TSDSBREF,TRNKSBR    TRANSACTION SUB-REF NUMBER                   
*                                                                               
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT OF TRANS RECORD)          
         LA    R4,TSDRFST          R4=A(FIRST ELEMENT FOR TSAR RECORD)          
         USING TRNELD,R3                                                        
         MVC   TRANCURR,COMPCURR   AGENCY CURRENCY                              
         MVC   TSDBREF,TRNBTCH     BATCH REFERENCE                              
         MVC   TSDTEST,TRNSTAT     TRANSACTION ELEMENT STATUS                   
         ZAP   TSDTAMNT,TRNAMNT    TRANSACTION AMOUNT                           
         MVC   TSDTTYPE,TRNTYPE    INPUT TYPE                                   
*                                                                               
BLDT10   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
BLDT20   CLI   0(R3),EOR           END OF RECORD?                               
         BE    BLDT100                                                          
         CLI   0(R3),OTHELQ        OTHERS ELEMENT?                              
         BE    BLDT30                                                           
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMENT?                
         BE    BLDT50                                                           
         CLI   0(R3),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    BLDT60                                                           
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    BLDT70                                                           
         CLI   0(R3),APEELQ        ANALYSIS POINTER ELEMENT?                    
         BE    BLDT80                                                           
         CLI   0(R3),AFCELQ        ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BE    BLDT85                                                           
         B     BLDT10                                                           
*                                                                               
         USING OTHELD,R3           OTHERS ELEMENT                               
BLDT30   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL OPTION?                   
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,OTHLN                                                         
         B     BLDT90                                                           
*                                                                               
         USING SCIELD,R3           ANALYSIS POINTER ELEMENT                     
BLDT50   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL OPTION?                   
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         B     BLDT90                                                           
*                                                                               
         USING TRSELD,R3           TRANSACTION STATUS ELEMENT                   
BLDT60   MVC   TSDACDAT,TRSDATE    ACTIVITY DATE                                
         MVC   TSDUSDAT,TRSUDAT    USED DATE                                    
         MVC   TSDPEDAT,TRSPDAT    PEELED DATE                                  
         B     BLDT10                                                           
*                                                                               
         USING OAMELD,R3           ORDER AMOUNT ELEMENT                         
BLDT70   AP    TSDTAMNT,OAMAMNT    ADD ORDER AMOUNT                             
         SP    TSDTAMNT,OAMIVAL    SUBTRACT INVOICE TODATE AMOUNT               
         ZAP   INVAMT,OAMIVAL      INVOICED TODATE AMOUNT                       
         OI    TSDTAMTY,TSDTORDQ   ORDER AMOUNT INCLUDED                        
         B     BLDT10                                                           
*                                                                               
         USING APEELD,R3           ANALYSIS POINTER ELEMENT                     
BLDT80   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OATTRIB-OPTVALSD(RF),C'Y' ATTRIBUTE OPTION?                      
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,APELN                                                         
         B     BLDT90                                                           
*                                                                               
         USING AFCELD,R3           ACCOUNT FOREIGN CURRENCY ELEMENT             
BLDT85   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L' SHOW LOCAL CURRENCY                     
         BNE   BLDT86                                                           
         L     RE,AIO1                                                          
         CLC   PREVBILL,TRNKOFF-TRNRECD(RE) PREVIOUS BILLING?                   
         BE    BLDT85A                                                          
         LA    RE,PRATBLK          RE=A(PRORATA BLOCK)                          
         USING PRORATAD,RE                                                      
         TM    PG$STAT,PG$FULLB+PG$PARTB BILLED?                                
         BZ    BLDT85A                                                          
         CLC   AFCCURR,PG$BLCUR    INVOICE CURR MATCHED BILLING CURR?           
         BNE   BLDT86                                                           
BLDT85A  MVC   TRANCURR,AFCCURR    INVOICE CURRENCY                             
         ZAP   TSDTAMNT,AFCAMNT    LOCAL CURRENCY AMOUNT                        
BLDT86   CLI   OXDETAIL-OPTVALSD(RF),C'Y'                                       
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,AFCLN                                                         
*                                                                               
BLDT90   BCTR  RF,0                MOVE WHOLE OF ELEMENT ONTO TSAR REC          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     BLDT10                                                           
         DROP  R3,RE                                                            
*                                                                               
BLDT100  MVC   TSDCURR,TRANCURR    SET CURRENCY CODE                            
         CLC   CURRLAST,SPACES     CURRENCY NOT SET?                            
         BE    BLDT101                                                          
         CLC   CURRLAST,TRANCURR   MIXED CURRENCIES DISPLAYED?                  
         BE    BLDT102                                                          
         OI    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS                      
         TM    CURRFLAG,CURRSFSQ   FIRST TRANSACTION FOR WC?                    
         BO    BLDT101                                                          
         OI    CURRFLAG,CURRSSTQ   SUPPRESS SUB TOTS                            
BLDT101  MVC   CURRLAST,TRANCURR                                                
BLDT102  ZAP   TSDALAMT,=P'0'                                                   
         NI    CURRFLAG,X'FF'-CURRSFSQ SWITCH OFF FIRST FOR WC                  
         L     RF,AIO1                                                          
         CLC   PREVBILL,TRNKWORK-TRNRECD(RF)                                    
         BNE   *+14                                                             
         AP    WTRNTOT,TSDTAMNT    TRANSACTIONS NET WORKCODE TOTAL              
         B     BLDT170                                                          
         GOTO1 VPRORATA,DMCB,AIO1,0,ACOMFACS,0,PRATBLK,0                        
         LA    RF,PRATBLK                                                       
         USING PRORATAD,RF                                                      
         MVC   TSDBSTAT,PG$STAT    PRORATA STATUS BYTE                          
         TM    PG$STAT,PG$FULLB+PG$PARTB BILLED?                                
         BZ    BLDT111                                                          
         ZAP   TSDALAMT,PA$NETBL   NET BILLED (BILLED ALLOCATION)               
         CP    PB$NETBL,=P'0'      ANY LOCAL ALLOCATION?                        
         BE    BLDT110                                                          
         CLC   TRANCURR,PG$BLCUR   INVOICE CURR MATCHED BILLING CURR?           
         BNE   BLDT110                                                          
         L     R1,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L' SHOW LOCAL CURRENCY                     
         BNE   BLDT110                                                          
         ZAP   TSDALAMT,PB$NETBL   NET BILLED (BILLED ALLOCATION)               
BLDT110  MVC   TSDBREF,PG$LBLNO    LATEST BILL NUMBER                           
         GOTO1 VDATCON,DMCB,(2,PG$LBLDT),(1,TSDTDAT) LATEST BILL DATE           
         AP    WALLTOT,TSDALAMT    ALLOCATION WORKCODE TOTAL                    
BLDT111  AP    WTRNTOT,TSDTAMNT    TRANSACTIONS NET WORKCODE TOTAL              
         TM    TSDBSTAT,PG$FULLB   TRANSACTION FULLY BILLED?                    
         BO    BLDT170                                                          
         AP    WBALTOT,TSDTAMNT    WORKCODE BALANCE AMOUNT                      
         SP    WBALTOT,TSDALAMT                                                 
         DROP  RF                                                               
*                                                                               
BLDT170  MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         LA    R4,1(R4)                                                         
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4               RF=L'(TSAR RECORD)                           
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
         L     RF,AIO1                                                          
         CLC   PREVBILL,TRNKWORK-TRNRECD(RF)                                    
         BE    BLDT180                                                          
         L     RF,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY OPTION?                  
         BNE   BLDT180                                                          
         LA    RF,TSDCONT+L'TSDCONT-L'TRANCURR-3                                
         MVI   0(RF),C' '                                                       
         MVC   1(L'TRANCURR,RF),TRANCURR DISPLAY CURRENCY                       
*                                                                               
BLDT180  TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         MVC   TSDLINES,LINSUSED   NUMBER OF LINES USED                         
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    BLDT190                                                          
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   BLDT190             TSAR BLOCK IS FULL                           
         LTR   RB,RB                                                            
         B     BLDTX                                                            
BLDT190  MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         CR    RB,RB                                                            
*                                                                               
BLDTX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0                                                       
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R4                                                      
         CLI   TSARFMT,TSWRKITM    WORKCODE TOTAL ITEM?                         
         BE    FORM90                                                           
         CLI   TSARFMT,TSINVITM    INVOICE TOTAL ITEM?                          
         BE    FORM100                                                          
         CLI   TSARFMT,TSTOTITM    ACCOUNT TOTAL ITEM?                          
         BE    FORM110                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1WORK,TSDWORK    WORK CODE                                    
         MVC   SCR1CONT,TSDCONT    CONTRA ACCOUNT                               
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(17,SCR1TDAT) TRAN/BILL DATE            
         MVC   SCR1TREF,TSDTREF    TRANSACTION REFERENCE                        
         MVC   SCR1BREF,TSDBREF    BATCH REFERENCE/BILL NO                      
*                                                                               
         CLC   TSDWORK,PREVBILL    PREVIOUS BILLING NO BALANCE                  
         BNE   FORM10                                                           
         LA    R3,TSDTAMNT         R3=A(NET AMOUNT)                             
         L     RF,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY?                         
         BNE   FORM05                                                           
         GOTO1 AGETCURR,DMCB,TSDCURR                                            
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,(R3)),(L'SCR1LNET,SCR1LNET),(RF),MINUS=YES,         X        
               CURSYMB=YES                                                      
         B     FORM40                                                           
*                                                                               
FORM05   OI    FORMFLAG,DISZEROQ   DISPLAY ZEROS FLAG ON                        
         LA    RF,SCR1NET          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         B     FORM40                                                           
FORM10   MVC   TRANCURR,TSDCURR    SET CURRENCY CODE FOR AMOUNT DISP            
         TM    TSDTAMTY,TSDTORDQ   HAVE WE GOT AN ORDER TRANSACTION?            
         BNO   *+8                                                              
         OI    FORMFLAG,ORDERQ                                                  
         LA    R3,TSDALAMT         R3=A(ALLOCATION AMOUNT)                      
         LA    RF,SCR1ALL          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         OI    FORMFLAG,DISZEROQ   DISPLAY ZEROS FLAG ON                        
         LA    R3,TSDTAMNT         R3=A(NET AMOUNT)                             
         LA    RF,SCR1NET          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         ZAP   BALAMT,TSDTAMNT                                                  
         SP    BALAMT,TSDALAMT                                                  
         TM    TSDBSTAT,PG$FULLB   TRANSACTION FULLY BILLED?                    
         BNO   FORM20                                                           
         CP    BALAMT,=P'0'        OUTSTANDING BAL FROM XRATE DIFFO?            
         BE    FORM39                                                           
         OI    FORMFLAG,FLOATQ     SET FLOAT ON                                 
FORM20   LA    R3,BALAMT           R3=A(BALANCE AMOUNT)                         
         LA    RF,SCR1BAL          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
FORM39   NI    FORMFLAG,X'FF'-(ORDERQ+FLOATQ)                                   
*                                                                               
FORM40   GOTO1 ASETELE,TSDRFST                                                  
*                                                                               
FORM70   MVI   LINSUSED,1                                                       
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         LA    R2,L'DUMLIN1(R2)                                                 
         BAS   RE,ATTRLNS          ATTRIBUTE LINES                              
         BAS   RE,XDETLNS          EXTRA DETAIL LINES                           
         B     FORMX                                                            
*                                                                               
         USING SCRWORKD,R2         WORKCODE TOTAL LINE                          
FORM90   LR    RF,R4                                                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARWRKD,R4                                                      
         MVC   SCRWWORK,TSWWORK    WORK CODE                                    
         MVC   SCRWDESC,TSWDESC    WORKCODE DESCRIPTION                         
         SR    RE,RE                                                            
         ICM   RE,3,TSARLEN-TSARRECD(RF)                                        
         SH    RE,=Y(TSARDATA-TSARRECD)                                         
         CH    RE,=Y(TSWLEN1Q)     SHORT TSAR IF TOTALS SUPRESSED               
         BE    FORM95                                                           
         MVC   SCRWWTOT,MX@WCTOT   WORKCODE TOTAL                               
         CLC   TSWWORK,PREVBILL    PREVIOUS BILLING NO BALANCE                  
         BNE   FORM94                                                           
         LA    R3,TSWNET           R3=A(NET AMOUNT)                             
         L     RF,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY?                         
         BNE   FORM93                                                           
         GOTO1 AGETCURR,DMCB,TSWCURR                                            
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,(R3)),(L'SCRWLNET,SCRWLNET),(RF),MINUS=YES,         X        
               CURSYMB=YES                                                      
         B     FORM95                                                           
*                                                                               
FORM93   OI    FORMFLAG,DISZEROQ   DISPLAY ZERO FLAG ON                         
         LA    RF,SCRWNTOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         B     FORM95                                                           
*                                                                               
FORM94   MVC   TRANCURR,TSWCURR    CURRENCY CODE FOR AMOUNT DISP                
         CLC   TSWWORK,ORDER       HAVE WE GOT AN ORDER LINE?                   
         BNE   *+8                                                              
         OI    FORMFLAG,ORDERQ                                                  
         LA    R3,TSWALL           R3=A(ALLOCATION AMOUNT)                      
         LA    RF,SCRWATOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         OI    FORMFLAG,DISZEROQ   DISPLAY ZERO FLAG ON                         
         LA    R3,TSWNET           R3=A(NET AMOUNT)                             
         LA    RF,SCRWNTOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         ZAP   BALAMT,TSWBAL                                                    
         LA    R3,BALAMT           R3=A(BALANCE AMOUNT)                         
         LA    RF,SCRWBTOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
FORM95   NI    FORMFLAG,X'FF'-ORDERQ                                            
         MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
*                                                                               
         USING SCRINVD,R2          INVOICE TOTAL LINE                           
FORM100  LA    R2,L'DUMLIN1(R2)                                                 
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARINVD,R4                                                      
         MVC   TRANCURR,TSICURR    CURRENCY CODE                                
         MVC   SCRIITOT,MX@INVTO   INVOICE TOTAL                                
         LA    R3,TSIALL           R3=A(ALLOCATION AMOUNT)                      
         LA    RF,SCRIATOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         OI    FORMFLAG,DISZEROQ   DISPLAY ZERO FLAG ON                         
         LA    R3,TSINET           R3=A(NET AMOUNT)                             
         LA    RF,SCRINTOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         ZAP   BALAMT,TSIBAL                                                    
         LA    R3,BALAMT           R3=A(BALANCE AMOUNT)                         
         LA    RF,SCRIBTOT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
*                                                                               
         USING SCRTOTD,R2          ACCOUNT TOTAL LINE                           
FORM110  LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@ACCT     ACCOUNT TOTAL                               
         OI    FORMFLAG,DISZEROQ    DISPLAY ZERO                                
*        LA    R3,TSTALL            R3=A(ALLOC AMOUNT)                          
*        LA    RF,SCRTATOT          RF=A(LINE POSITION)                         
*        BAS   RE,FORAMT                                                        
         MVC   TRANCURR,TSTCURR     CURRENCY CODE IF LOCAL DISPLAY              
         LA    R3,TSTNET            R3=A(NET AMOUNT)                            
         LA    RF,SCRTNTOT          RF=A(LINE POSITION)                         
         BAS   RE,FORAMT                                                        
*        ZAP   BALAMT,TSTNET                                                    
*        SP    BALAMT,TSTALL                                                    
*        LA    R3,BALAMT           R3=A(BALANCE AMOUNT)                         
*        LA    RF,SCRTBTOT         RF=A(LINE POSITION)                          
*        BAS   RE,FORAMT                                                        
         NI    FORMFLAG,X'FF'-DISZEROQ                                          
         MVI   LINSUSED,1           NUMBER OF DUMMY SCREEN LINES USED           
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
*                                                                               
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
         SPACE 1                                                                
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0                                                       
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
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FGRM30                                                           
         CLI   TSARFMT,TSITEM1     ITEM 1 FORMAT                                
         BNE   FGRMX                                                            
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT AN AMOUNT                                             *         
* ON ENTRY R3=A(PACKED AMOUNT OF LENGTH 6)                            *         
*          RF=A(POSITION ON LINE WHERE AMOUNT WILL BE DISPLAYED)      *         
*          IF VAL=LOCAL THEN TRANCURR MUST CONTAIN CURR CODE          *         
***********************************************************************         
         SPACE 1                                                                
FORAMT   NTR1                                                                   
         LR    R2,RF                                                            
         TM    FORMFLAG,DISZEROQ   DISPLAYING ZEROS?                            
         BO    FORA40                                                           
         TM    FORMFLAG,ORDERQ     DISPLAYING AN ORDER AMOUNT?                  
         BNO   FORA20                                                           
         PACK  TEMP(8),NINES(L'SCR1NET-4) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA10                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA10                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,BRACKET=YES,ZERO=*        
               BLANK                                                            
         B     FORA35                                                           
FORA10   CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,DECS=ROUND,BRACKE*        
               T=YES,ZERO=BLANK                                                 
         B     FORA35                                                           
*                                                                               
FORA20   L     RE,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BNE   FORA28                                                           
         GOTO1 AGETCURR,DMCB,TRANCURR                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         PACK  TEMP(8),NINES(L'SCR1NET-2) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA25                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA25                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),(RF),MINUS=YES,ZERO=BLANK             
         B     FORA35                                                           
FORA25   CURED (P6,(R3)),(L'SCR1NET,(R2)),(RF),MINUS=YES,DECS=ROUND,   X        
               ZERO=BLANK                                                       
         B     FORA35                                                           
*                                                                               
FORA28   PACK  TEMP(8),NINES(L'SCR1NET-2) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA30                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA30                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,ZERO=BLANK                
         B     FORA35                                                           
FORA30   CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,DECS=ROUND,      X        
               ZERO=BLANK                                                       
*                                                                               
FORA35   TM    FORMFLAG,FLOATQ                                                  
         BNO   FORAX                                                            
         BCTR  R2,0                                                             
         LA    R0,L'SCR1NET                                                     
         CLI   1(R2),C' '                                                       
         BH    *+12                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         MVI   0(R2),C'*'                                                       
         B     FORAX                                                            
*                                                                               
FORA40   TM    FORMFLAG,ORDERQ                                                  
         BNO   FORA60                                                           
         PACK  TEMP(8),NINES(L'SCR1NET-4) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA10                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA50                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,BRACKET=YES               
         B     FORAX                                                            
FORA50   CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,DECS=ROUND,BRACKE*        
               T=YES                                                            
         B     FORAX                                                            
*                                                                               
FORA60   L     RE,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BNE   FORA68                                                           
         GOTO1 AGETCURR,DMCB,TRANCURR                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         PACK  TEMP(8),NINES(L'SCR1NET-2) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA65                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA65                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),(RF),MINUS=YES                        
         B     FORAX                                                            
FORA65   CURED (P6,(R3)),(L'SCR1NET,(R2)),(RF),MINUS=YES,DECS=ROUND             
         B     FORAX                                                            
*                                                                               
FORA68   PACK  TEMP(8),NINES(L'SCR1NET-2) GET STRING OF NINES                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA70                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA70                                                           
         CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES                           
         B     FORAX                                                            
FORA70   CURED (P6,(R3)),(L'SCR1NET,(R2)),2,MINUS=YES,DECS=ROUND                
         B     FORAX                                                            
*                                                                               
*                                                                               
FORAX    B     XIT                                                              
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
         CLI   OXDETAIL-OPTVALSD(RF),C'Y' XTRA DETAIL OPTION?                   
         BNE   XDETLX                                                           
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
         USING AFCELD,R2                                                        
XDETL210 ICM   R2,15,AAFCELD       ACOUNT FOREIGN CURRENCY ELEMENT?             
         BZ    XDETL215                                                         
         L     RF,AXDFCURR                                                      
         BASR  RE,RF               GET FOREIGN CURRENCY INFORMATION             
*                                                                               
XDETL215 MVC   UNSCNLHS(L'MX@SEQ),MX@SEQ TRANS SUB-REFERENCE NUMBER             
         SR    RF,RF                                                            
         IC    RF,TSDSBREF                                                      
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL220 ICM   R2,15,ANXTLINE      R2=A(NEXT DUMMY SCREEN LINE)                 
         GOTO1 AXDETDIS            DISPLAY ON DUMMY SCREEN LINES                
*                                                                               
XDETLX   XIT1  REGS=(R2)                                                        
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE WORK CODE TOTALS                               *         
***********************************************************************         
         SPACE 1                                                                
WORKTOT  NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSWLENQ)                                                   
         TM    CURRFLAG,CURRSSTQ   SUPPRESS WORKCODE TOTALS?                    
         BNO   *+12                                                             
         LH    RF,=Y(TSWLEN1Q)                                                  
         NI    CURRFLAG,X'FF'-CURRSSTQ                                          
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARWRKD,R3                                                      
         MVI   TSWFMT,TSWRKITM     WORKCODE TOTAL ITEM TYPE                     
         MVC   TSWWORK,SAVEWC      WORK CODE                                    
         MVC   TSWDESC,SPACES      WORKCODE DESCRIPTION                         
         MVC   TSWCURR,CURRLAST    WORKCODE CURRENCY                            
         LA    R2,IOKEY            GET THE WORKCODE RECORD                      
         USING WCORECD,R2                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,MYCO                                                     
         MVC   WCOKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         MVC   WCOKWRK,SAVEWC                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    WRKT10                                                           
         TM    IOERR,IOERNF                                                     
         BO    WRKT40                                                           
         CLI   IOERR,IOMAX                                                      
         BE    WRKT10                                                           
         DC    H'0'                                                             
*                                                                               
WRKT10   L     R2,AIO2             R2=A(WORKCODE RECORD)                        
         LA    R4,WCORFST                                                       
WRKT20   CLI   0(R4),EOR           END OF RECORD?                               
         BE    WRKT40                                                           
         CLI   0(R4),WCOELQ        WORK CODE ELEMENT?                           
         BE    WRKT30                                                           
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     WRKT20                                                           
*                                                                               
WRKT30   MVC   TSWDESC,WCODESC-WCOELD(R4) GET DESCRIPTION                       
*                                                                               
WRKT40   TM    CURRFLAG,CURRSSTQ   SUPPRESS SUB TOTAL?                          
         ZAP   TSWALL,WALLTOT      ALLOCATION FOR WORKCODE                      
         ZAP   TSWNET,WTRNTOT      NET AMOUNT FOR WORKCODE                      
         ZAP   TSWBAL,WBALTOT      BALANCE FOR WORKCODE                         
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSWLINES,LINSUSED   NUMBER OF LINES USED                         
*                                                                               
         GOTO1 ATSARADD                                                         
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    WRKT50                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
WRKT50   SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         CLC   SAVEWC,PREVBILL     PREVIOUS BILLING?                            
         BNE   *+14                                                             
         SP    NETTOT,WTRNTOT      SUBTRACT PREV BILLING                        
         B     WRKT70                                                           
         CLC   SAVEWC,ORDER        ORDERS NOT INCLUDED                          
         BE    WRKT60                                                           
         AP    NETTOT,WTRNTOT      ACCOUNT NET TOTAL                            
         AP    IALLTOT,WALLTOT     INVOICE ALLOCATION                           
         AP    ITRNTOT,WTRNTOT     INVOICE NET AMOUNT                           
         AP    IBALTOT,WBALTOT     INVOICE BAL AMOUNT                           
         AP    TALLTOT,WALLTOT     ACCOUNT ALLOCATION                           
WRKT60   CLC   NEXTWC,PREVBILL     IF NXT WC PREV BILL DISPLAY INV LINE         
         BE    WRKT65                                                           
         TM    DISPFLAG,ALLREADQ                                                
         BNO   WRKT70                                                           
         TM    INTFLAG,INVTOTQ     ALREADY DONE INV TOTAL LINE?                 
         BO    WRKT70                                                           
WRKT65   BAS   RE,INVCTOT          DEAL WITH INVOICE TOTAL LINE                 
WRKT70   ZAP   WTRNTOT,=P'0'       CLEAR TOTALS                                 
         ZAP   WALLTOT,=P'0'                                                    
         ZAP   WBALTOT,=P'0'                                                    
         MVC   SAVEWC,NEXTWC                                                    
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    WRKTERRX                                                         
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    WRKTERRX                                                         
         TM    DISPFLAG,ALLREADQ   IF ALL REC READ NO POINT RESETTING           
         BO    WRKTX                                                            
*                                                                               
         L     R3,AIO1                                                          
         OC    0(L'IOKEY,R3),0(R3) SEQUENCE RESTORE REQUIRED?                   
         BZ    WRKTX                                                            
         MVC   IOKEY,0(R3)         REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    WRKTX                                                            
         CLI   IOERR,IOMAX                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     WRKTERRX                                                         
*                                                                               
WRKTX    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
WRKTERRX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE INVOICE TOTAL                                  *         
***********************************************************************         
         SPACE 1                                                                
INVCTOT  NTR1                                                                   
         TM    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTAL LINE                  
         BO    INVTX                                                            
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         OI    INTFLAG,INVTOTQ     INVOICE TOTAL DONE                           
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSILENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARINVD,R3                                                      
         MVI   TSIFMT,TSINVITM     INVOICE TOTAL ITEM TYPE                      
*                                                                               
         MVC   TSICURR,CURRLAST    CURRENCY CODE                                
         ZAP   TSIALL,IALLTOT      INVOICE ALLOCATION AMOUNT                    
         ZAP   TSINET,ITRNTOT      INVOICE NET AMOUNT                           
         ZAP   TSIBAL,IBALTOT      INVOICE BAL AMOUNT                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSILINES,LINSUSED   NUMBER OF LINES USED                         
*                                                                               
         GOTO1 ATSARADD                                                         
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX   IF MAX IO IT HAS BEEN ADDED                  
         BNO   INVTERRX                                                         
*                                                                               
         ZAP   ITRNTOT,=P'0'       CLEAR TOTALS                                 
         ZAP   IALLTOT,=P'0'                                                    
         ZAP   IBALTOT,=P'0'                                                    
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         BO    INVT20                                                           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    INVT20                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
*                                                                               
INVT20   SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
INVTX    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
INVTERRX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         TM    CURRFLAG,CURRSRTQ   SUPPRESS TOTAL LINE?                         
         BO    TOTX                                                             
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     ACCOUNT TOTAL ITEM                           
*                                                                               
         MVC   TSTCURR,CURRLAST    CURRENCY CODE                                
         ZAP   TSTNET,NETTOT       ACCOUNT NET AMOUNT                           
         ZAP   TSTALL,TALLTOT      ACCOUNT NET AMOUNT                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSTLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD                                                         
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         BO    TOT10                                                            
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    TOT10                                                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL                                  
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
*                                                                               
TOT10    SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
TOTX     CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
SI       DC    C'SI'                                                            
PREVBILL DC    C'99'                                                            
ORDER    DC    C'**'                                                            
         EJECT                                                                  
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH20,78                                                      
         DCDDL AC#ENH21,78                                                      
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
         DCDDL AC#ACCT,15                                                       
         DCDDL AC#WCTOT,15                                                      
         DCDDL AC#INVTO,15                                                      
         DCDDL AC#RSALA,L'MX@RSALA     ALLOCATED AMOUNT                         
         DCDDL AC#NETAM,L'MX@NETAM     NET AMOUNT                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        JOB BILLING GRID COLUMN TABLE - COVERED BY GCTBLD            *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'LC@WC2,L'TSDWORK)     WORK CODE                  
         DC    AL2(LC@WC2-WORKD,TSDWORK-TSARDATD)                               
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@ACCT),AL2(MX@ACCT-OVERWRKD)                           
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'LC@CTRA,L'TSDCONT)     CONTRA                    
         DC    AL2(LC@CTRA-WORKD,TSDCONT-TSARDATD)                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'LC@DATE,1)             DATE                      
         DC    AL2(LC@DATE-WORKD,TSDTDAT-TSARDATD)                              
         DC    AL1(0,0,GCTFDAT+GCTFLEFT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'LC@REF,L'TSDTREF)      REFERENCE                 
         DC    AL2(LC@REF-WORKD,TSDTREF-TSARDATD)                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'UP@BATL,L'TSDBREF)     BATCH REFERENCE           
         DC    AL2(UP@BATL-WORKD,TSDBREF-TSARDATD)                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
GCT5LQ   EQU   *-GCTCOL5                                                        
*                                                                               
GCTCOL7  DC    AL1(GCT7LQ,07,L'MX@RSALA,L'TSDALAMT)  ALLOCATED AMOUNT           
         DC    AL2(MX@RSALA-OVERWRKD,TSDALAMT-TSARDATD)                         
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTALL),AL2(TSTALL-TSARTOTD)                             
GCT7LQ   EQU   *-GCTCOL7                                                        
*                                                                               
GCTCOL8  DC    AL1(GCT8LQ,08,L'MX@NETAM,L'TSDTAMNT)   NET AMOUNT                
         DC    AL2(MX@NETAM-OVERWRKD,TSDTAMNT-TSARDATD)                         
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTNET),AL2(TSTNET-TSARTOTD)                             
GCT8LQ   EQU   *-GCTCOL8                                                        
*                                                                               
GCTDD01  DC    AL1(GDD01LQ,81,L'UP@SUBRL,L'TSDSBREF)  TRANS SUB-REF             
         DC    AL2(UP@SUBRL-WORKD,TSDSBREF-TSARDATD)                            
         DC    AL1(0,GCTIBIN,GCTFNUM+GCTFRGHT,GCTIDDS)                          
         DC    AL1(0,0,0,0)                                                     
GDD01LQ  EQU   *-GCTDD01                                                        
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
OVERWRKD DSECT                                                                  
ANXTLINE DS    A                   ADDRESS OF NEXT DUMMY LINE                   
ALLAMT   DS    F                   ALLOCATED AMOUNT                             
BALAMT   DS    PL(L'TRNAMNT)       BALANCE AMOUNT                               
INVAMT   DS    PL(L'OAMIVAL)       INVOICED TODATE AMOUNT                       
NEXTWC   DS    XL(L'TRNKWORK)      NEXT WORK CODE                               
ORD      DS    PL(L'FFNONUM)       TEMP ORDER NUM                               
TEMPNUM  DS    PL8                 TEMPORARY NUMBER STORAGE                     
TEMPPDAT DS    PL3                 TEMPORARY PACKED DATE STORAGE                
FORMFLAG DS    X                   FORMAT FLAG                                  
DISZEROQ EQU   X'01'               DISPLAY ZERO                                 
ORDERQ   EQU   X'02'               ORDER                                        
FLOATQ   EQU   X'04'               USE FLOAT                                    
INTFLAG  DS    X                   GENERAL INTERNAL FLAG                        
SCRFULLQ EQU   X'01'               SCREEN FULL                                  
INVTOTQ  EQU   X'02'               INVOICE TOTAL DONE                           
TRANAMNT DS    CL(L'TRNAMNT)       AGENCY/LOCAL CURRENCY AMOUNT                 
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY CODE                    
*                                                                               
*---------------------------------------------------------------------*         
* DATA DICTIONARY                                                     *         
*---------------------------------------------------------------------*         
DSMIX    DS    0C                                                               
MX@ENH20 DS    CL(L'ENQDAT1)                                                    
MX@ENH21 DS    CL(L'ENQDAT1)                                                    
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
MX@ACCT  DS    CL15                                                             
MX@WCTOT DS    CL15                                                             
MX@INVTO DS    CL15                                                             
MX@RSALA DS    CL16                ALLOCATED AMOUNT                             
MX@NETAM DS    CL10                NET AMOUNT                                   
*                                                                               
PRATBLK  DS    CL(PR$LNQ)          PRORATA BLOCK                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SCREEN DSECT                                                                  
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1WORK DS    CL(L'TRNKWORK)      WORKCODE                                     
         DS    CL1                                                              
SCR1CONT DS    CL(L'TRNKULA)       CONTRA CODE                                  
         DS    CL1                                                              
SCR1TDAT DS    CL8                 TRANSACTION DATE/BILL DATE                   
         DS    CL1                                                              
SCR1TREF DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL1                                                              
SCR1BREF DS    CL6                 BATCH REFERENCE/BILL NUMBER                  
         DS    CL2                                                              
SCR1LNET DS    0CL23               PREV BILL LOCAL CURR NET AMOUNT              
SCR1ALL  DS    CL11                ALLOCATED AMOUNT                             
         DS    CL1                                                              
SCR1NET  DS    CL11                NET AMOUNT                                   
         DS    CL1                                                              
SCR1BAL  DS    CL11                BALANCE                                      
SCR1LNQ  EQU   *-SCRLIN1D                                                       
         SPACE 1                                                                
SCRWORKD DSECT                     COVER WORKCODE TOTAL SCREEN LINE             
SCRWWORK DS    CL(L'TRNKWORK)      WORKCODE                                     
         DS    CL1                                                              
SCRWDESC DS    CL(L'WCODESC)       WORKCODE DESCRIPTION                         
         DS    CL1                                                              
SCRWWTOT DS    CL(L'MX@WCTOT)      TOTAL                                        
         DS    CL8                                                              
SCRWLNET DS    0CL23               LOCAL CURRENCY PREV BILLING                  
SCRWATOT DS    CL11                ALLOCATION TOTAL                             
         DS    CL1                                                              
SCRWNTOT DS    CL11                NET TOTAL                                    
         DS    CL1                                                              
SCRWBTOT DS    CL11                BALANCE TOTAL                                
         SPACE 1                                                                
SCRINVD  DSECT                     COVER INVOICE TOTAL SCREEN LINE              
         DS    CL19                                                             
SCRIITOT DS    CL(L'MX@INVTO)      INVOICE TOTAL                                
         DS    CL8                                                              
SCRIATOT DS    CL11                ALLOCATION TOTAL                             
         DS    CL1                                                              
SCRINTOT DS    CL11                NET TOTAL                                    
         DS    CL1                                                              
SCRIBTOT DS    CL11                BALANCE TOTAL                                
         SPACE 1                                                                
SCRTOTD  DSECT                     COVER ACCOUNT TOTAL SCREEN LINE              
         DS    CL19                                                             
SCRTOTAL DS    CL(L'MX@ACCT)                                                    
         DS    CL8                                                              
SCRTATOT DS    CL11                ALLOCATION TOTAL                             
         DS    CL1                                                              
SCRTNTOT DS    CL11                ACCOUNT NET TOTAL                            
         DS    CL1                                                              
SCRTBTOT DS    CL11                BALANCE TOTAL                                
*                                                                               
**********************************************************************          
* TSAR RECORD. NOTE THIS RECORD MAY HAVE ELEMENTS ATTACHED           *          
* POSSIBLE GENFILE ELEMENTS INCLUDE                                  *          
* APEELD 'ANALYSIS POINTER' ELEMENT                                  *          
* OTHELD 'OTHERS' ELEMENT                                            *          
* SCIELD 'SUBSIDIARY CASH' ELEMENT                                   *          
**********************************************************************          
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDBSTAT DS    CL(L'PG$STAT)       BILLING STATUS BYTE                          
TSDWORK  DS    CL(L'TRNKWORK)      WORK CODE                                    
TSDCONT  DS    CL(L'TRNKULC)       CONTRA CODE                                  
TSDTDAT  DS    PL(L'TRNKDATE)      TRANSACTION DATE                             
TSDTREF  DS    CL(L'TRNKREF)       TRANSACTION REFERENCE                        
TSDBREF  DS    CL(L'TRNBTCH)       BATCH REFERENCE                              
TSDSBREF DS    CL(L'TRNKSBR)       TRANSACTION SUB-REFERENCE                    
TSDTAMNT DS    PL(L'TRNAMNT)       TRANSACTION AMOUNT                           
TSDTAMTY DS    XL1                 TRANSACTION AMOUNT TYPE                      
TSDTORDQ EQU   X'01'               ORDER AMOUNTS INCLUDED                       
TSDALAMT DS    PL6                 ALLOCATED BILL AMOUNT                        
TSDTRST  DS    XL(L'TRNRSTAT)      TRANSACTION RECORD STATUS                    
TSDTEST  DS    XL(L'TRNSTAT)       TRANSACTION ELEMENT STATUS                   
TSDTTYPE DS    CL(L'TRNTYPE)       INPUT TYPE                                   
TSDACDAT DS    XL(L'TRSDATE)       ACTIVITY DATE                                
TSDUSDAT DS    XL(L'TRSUDAT)       USED DATE                                    
TSDPEDAT DS    XL(L'TRSPDAT)       PEELED DATE                                  
TSDCURR  DS    XL(L'AFCCURR)       CURRENCY CODE                                
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARWRKD DSECT                     WORKCODE TOTAL TSAR RECORD                   
TSWLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSWFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSWRKITM EQU   2                   WORKCODE TOTAL ITEM TYPE                     
TSWWORK  DS    CL(L'TRNKWORK)      WORK CODE                                    
TSWDESC  DS    CL(L'WCODESC)       WORKCODE DESCRIPTION                         
TSWLEN1Q EQU   *-TSARWRKD                                                       
TSWCURR  DS    CL(L'AFCCURR)       WORKCODE CURRENCY                            
TSWALL   DS    PL6                 WORKCODE ALLOCATION AMOUNT                   
TSWNET   DS    PL6                 WORKCODE NET AMOUNT                          
TSWBAL   DS    PL6                 WORKCODE BALANCE AMOUNT                      
TSWLENQ  EQU   *-TSARWRKD                                                       
*                                                                               
TSARINVD DSECT                     INVOICE TOTAL TSAR RECORD                    
TSILINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSIFMT   DS    CL1                 ITEM FOMAT TYPE                              
TSINVITM EQU   3                   INVOICE TOTAL ITEM TYPE                      
TSICURR  DS    CL(L'AFCCURR)       CURRENCY CODE                                
TSIALL   DS    PL6                 INVOICE ALLOCATION AMOUNT                    
TSINET   DS    PL6                 INVOICE NET AMOUNT                           
TSIBAL   DS    PL6                 INVOICE BALANCE AMOUNT                       
TSILENQ  EQU   *-TSARINVD                                                       
*                                                                               
TSARTOTD DSECT                     ACCOUNT TOTAL TSAR RECORD                    
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   4                   TOTAL ITEM TYPE                              
TSTOTAL  DS    CL(L'MX@ACCT)       TOTAL                                        
TSTCURR  DS    CL(L'AFCCURR)       CURRENCY CODE                                
TSTNET   DS    PL6                 ACCOUNT NET TOTAL                            
TSTALL   DS    PL6                 INVOICE ALLOCATION AMOUNT                    
TSTLENQ  EQU   *-TSARTOTD                                                       
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
SAVEWC   DS    CL(L'TRNKWORK)      WORK CODE                                    
JBVALS   DS    0PL6                CREDITOR VALUES                              
WHRSTOT  DS    PL6                 HOURS TOTAL FOR WORKCODE                     
WTRNTOT  DS    PL6                 TRANSACTION TOTAL FOR WORKCODE               
WALLTOT  DS    PL6                 ALLOCATION TOTAL FOR WORKCODE                
WBALTOT  DS    PL6                 BALANCE TOTAL FOR WORKCODE                   
ITRNTOT  DS    PL6                 INVOICE TRANSACTION TOTAL                    
IALLTOT  DS    PL6                 INVOICE ALLOCATION TOTAL                     
IBALTOT  DS    PL6                 INVOICE BALANCE TOTAL                        
NETTOT   DS    PL6                 NET TOTAL                                    
TALLTOT  DS    PL6                 INVOICE ALLOCATION TOTAL                     
UNILDG   DS    CL2                 SAVED UNIT AND LEDGER                        
JBVALLNQ EQU   *-JBVALS                                                         
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACENQ11   12/22/17'                                      
         END                                                                    
