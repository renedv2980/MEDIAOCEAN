*          DATA SET ACENQ0C    AT LEVEL 006 AS OF 09/20/07                      
*PHASE T6200CA                                                                  
T6200C   TITLE 'ACCOUNT ENQUIRY - ORDER DETAIL'                                 
T6200C   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQC**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,RELO                                                          
*                                                                               
         L     RF,=A(GCTBL)        SET UP GRID COLUMN TABLE                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL                                                        
*                                                                               
         L     RF,=A(GRDSP)        SET UP GRID COLUMN TABLE                     
         AR    RF,RE                                                            
         ST    RF,AGRDSP                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
         NI    DISPFLG2,X'FF'-DISWCOVF                                          
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN105                                                          
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
         TM    DISPFLAG,NORECQ     NO RECORDS?                                  
         BO    MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN50              YES                                          
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN40              YES                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   ODELETED-OPTVALSD(RF),C'Y' DO WE NEED DELETED RECORDS?           
         BE    *+12                                                             
         CLI   ODELETED-OPTVALSD(RF),C'O' DO WE NEED DELETED RECORDS?           
         BNE   MAIN20                                                           
         GOTO1 AIO,IORDD+IOACCDIR+IO1                                           
         BE    MAIN40                                                           
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
         TM    IOERR,IOEDEL           RECORD DELETED?                           
         BO    MAIN40                                                           
         DC    H'0'                                                             
MAIN20   GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN40                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
MAIN40   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN60              NO                                           
*                                                                               
MAIN50   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         B     MAIN150                                                          
*                                                                               
MAIN60   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN70              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN70   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
MAIN80   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   ODELETED-OPTVALSD(RF),C'Y' DO WE NEED DELETED RECORDS?           
         BE    *+12                                                             
         CLI   ODELETED-OPTVALSD(RF),C'O' DO WE NEED DELETED RECORDS?           
         BNE   MAIN90                                                           
         GOTO1 AIO,IOSQD+IOACCDIR+IO1                                           
         BE    MAIN105                                                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
         TM    IOERR,IOEDEL           RECORD DELETED?                           
         BO    MAIN105                                                          
         DC    H'0'                                                             
MAIN90   GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN105                MAX IOS REACHED?                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN105  LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING ORDER REC)           
         USING TRNRECD,R3                                                       
         CLC   TRNKCULA,IOKEYSAV                                                
         BNE   MAIN160                                                          
         SR    RF,RF               ARE WE FILTERING ON CONTRA ACC?              
         ICM   RF,1,CONTLEN                                                     
         BZ    MAIN130                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CONTRA(0),TRNKULC   MATCH ON CONTRA ULACC?                       
         BE    MAIN110                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CONTRA(0),TRNKULC+2 MATCH ON CONTRA ACCOUNT?                     
         BE    MAIN110                                                          
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BNE   MAIN80                                                           
         B     *+12                                                             
MAIN110  CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    MAIN80                                                           
*                                                                               
MAIN130  CLC   TRNKDATE,SPACES     TRANSACTION?                                 
         BH    MAIN140                                                          
         OC    CHDKNULL-CHDKEY(L'CHDKNULL,R3),CHDKNULL-CHDKEY(R3)               
         BNZ   MAIN80                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET CONTRA NAME RECORD                    
         BE    MAIN135                MAX IOS REACHED?                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN135  BAS   RE,SUPPNAME        GET SUPPLIER CODE AND NAME                    
         B     MAIN80                                                           
*                                                                               
MAIN140  CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT                           
         BNE   *+14                                                             
         CLC   TRNKWORK,ORDER                                                   
         BH    MAIN160                                                          
         BAS   RE,FILTKEY          APPLY FILTERING TO INDEX RECORD              
         BNE   MAIN80              DO WE WANT TO KEEP THIS RECORD?              
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET CONTRA NAME RECORD                    
         BE    MAIN141                MAX IOS REACHED?                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN141  L     R3,AIO1                                                          
         CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT                           
         BE    MAIN145                                                          
         GOTO1 AOFFTRN                                                          
         BNE   MAIN80                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
MAIN145  BAS   RE,FILTER           APPLY FILTERING TO ORDER RECORD              
         BNE   MAIN80              DO WE WANT TO KEEP THIS RECORD?              
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         BNE   ERRXIT                                                           
         MVC   KEYSAVE,ORDKEY-ORDRECD(R3) SAVE THE KEY FOR SEQ RESTORE          
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN150                                                          
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN60                                                           
*                                                                               
MAIN150  GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN40                                                           
*                                                                               
MAIN160  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
         TM    DISPFLAG,ALLREADQ                                                
         BO    *+12                                                             
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
*                                                                               
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
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
         SPACE 1                                                                
FSTDIS   NTR1                                                                   
         ZAP   ORDTOT,=P'0'        ORDER AMIOUNT TOTAL                          
         ZAP   INVTOT,=P'0'        INVOICED TODATE TOTAL                        
         MVC   CONTRA,SPACES                                                    
         MVI   CONTLEN,0           LENGTH OF INPUT                              
         MVI   NEGCONT,0           NEGATIVE FILTER                              
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
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
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
         GOTO1 AUNITLDG,FLDDATA    READ UNIT/LEDGER RECORDS                     
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
         SH    RE,=Y(L'ACTKUNT+L'ACTKLDG) SUBTRACT UNIT LEDGER LENGTH           
         CR    RE,R1               IS INPUT LON ENOUGH?                         
         BNH   FSTDERR             NOPE, WRONG LEVEL ACCOUNT                    
         BCTR  R4,0                                                             
         LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FLDDATA                                               
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
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BZ    FSTD23                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
         B     FSTD25                                                           
*                                                                               
FSTD23   GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         OI    FLDOIND,FOUTTRN                                                  
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC                                         
         LA    R1,FLDDATA+L'MX@ACC-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
FSTD25   MVC   ACCNAM,SPACES                                                    
         L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
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
         SH    RE,=Y(NAMLN1Q+1)                                                 
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
*                                                                               
FSTD49   LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         MVC   FLDDATA(L'MX@ENH7),MX@ENH7                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH8),MX@ENH8                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
FSTD50   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD60                                                           
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD65                                                           
*                                                                               
FSTD60   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD65   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        GET SUPPLIER NAME THEN RESTORE FOR SEQUENTIAL READ          *          
* ON ENTRY R3=A(CONTRA ACCOUNT RECORD)                               *          
*          IOAREA1 CONTAINS CONTRA RECORD                            *          
**********************************************************************          
         SPACE 1                                                                
SUPPNAME NTR1                                                                   
         L     R3,AIO1                                                          
         USING CHDRECD,R3                                                       
         LA    R4,CHDRFST                                                       
         MVC   SUPCODE,SPACES      CLEAR SUPPLIER CODE                          
         MVC   SUPNAME,SPACES      AND SUPPLIER NAME                            
         MVC   SUPCODE,CHDKCACT    SUPPLIER CODE                                
SUPPN20  CLI   0(R4),EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),CACELQ        NAME ELEMENT?                                
         BE    SUPPN30                                                          
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SUPPN20                                                          
*                                                                               
         USING CACELD,R4           NAME ELEMENT                                 
SUPPN30  SR    RE,RE                                                            
         IC    RE,CACLN                                                         
         SH    RE,=Y(CACLN1Q+1)                                                 
         BM    SUPPNX                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SUPNAME(0),CACNAME                                               
*                                                                               
SUPPNX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER TRANSACTION INDEX RECORDS (ACCDIR)                    *         
* ON ENTRY AIO1  CONTAINS TRANSACTION INDEX RECORD                    *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING TRNRECD,R3                                                       
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         TM    TRNKSTAT,TRNSDELT   IS RECORD DELETED                            
         BNO   FILTK10                                                          
         CLI   ODELETED,C'N'                                                    
         BE    FILTKRJX                                                         
         B     *+12                                                             
FILTK10  CLI   ODELETED,C'O'                                                    
         BE    FILTKRJX                                                         
*                                                                               
         OC    OORDNO,OORDNO       ORDER FILTER?                                
         BZ    FILTK20                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OORDLN1,OORDVL1),(OORDLN2,OORDVL2),C        
               OORDFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK20  OC    OREF,OREF           REF FILTER?                                  
         BZ    FILTK30                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILTK30  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK40                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK40  OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
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
*        FILTER CONTRA TRANSACTIONS                                   *         
* ON ENTRY R3=A(CONTRA ACCOUNT RECORD)                                *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         USING TRNRECD,R3                                                       
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT ON RECORD)                
         USING TRNELD,R4                                                        
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLC   TRNKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT PRODUCTION?              
         BE    *+14                                                             
         CLC   TRNANAL,ORDER       ORDER TRANSACTION?                           
         BNE   FILTREJX                                                         
*                                                                               
FILT90   OC    OWCODE,OWCODE       WORK CODE FILTER?                            
         BZ    FILTX                                                            
FILT100  CLI   0(R4),EOR           END OF RECORD                                
         BE    FILTREJX                                                         
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    FILT110                                                          
*                                                                               
FILT105  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FILT100                                                          
*                                                                               
FILT110  SR    RF,RF                                                            
         IC    RF,OWCODELN                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   OWCODEVL(0),OAMWORK-OAMELD(R4)                                   
         BNE   FILT120                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BE    FILT105                                                          
         B     FILTX                                                            
FILT120  CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILT105                                                          
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
* ON ENTRY R3=A(AIO AREA CONTAINING CONTRA TRANSACTION RECORD)        *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
BLDTSDAT NTR1                                                                   
         USING TRNRECD,R3                                                       
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         MVC   TSDSUPP,SUPCODE     SUPPLIER CODE                                
         MVC   TSDNAME,SUPNAME     SUPPLIER NAME                                
         MVC   TSDONUM,TRNKREF     ORDER NUMBER                                 
         MVC   TSDODATE,TRNKDATE                                                
         LA    R4,TRNRFST          R4=A(FIRST ELEMENT ON ORD REC)               
         LA    R3,TSDRFST          R3=A(FIRST ELEMENT ON TSAR REC)              
         SR    R0,R0                                                            
BLDT10   CLI   0(R4),EOR           END OF RECORD?                               
         BE    BLDT80                                                           
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    BLDT30                                                           
*                                                                               
BLDT20   SR    RF,RF               BUMP TO NEXT RECORD                          
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     BLDT10                                                           
*                                                                               
         USING OAMELD,R4                                                        
BLDT30   AH    R0,=H'1'                                                         
         AP    ORDTOT,OAMAMNT      ORDER AMOUNT                                 
         AP    INVTOT,OAMIVAL      INVOICE TOTAL                                
*        SR    RF,RF                                                            
*        IC    RF,1(R4)                                                         
*        BCTR  RF,0                                                             
*        EX    RF,*+8                                                           
*        B     *+10                                                             
*        MVC   0(0,R3),0(R4)           PUT ELEMENT ON TSAR RECORD               
         MVC   0(OAMLN1Q,R3),0(R4)       PUT ELEMENT ON TSAR RECORD             
         MVI   1(R3),OAMLN1Q                                                    
         LA    R3,OAMLN1Q(R3)                                                   
*        LA    R3,1(RF,R3)                                                      
         B     BLDT20                                                           
         DROP  R4                                                               
*                                                                               
BLDT80   MVI   0(R3),EOR                                                        
         LA    R3,1(R3)                                                         
         LA    RF,TSDRFST                                                       
         SR    R3,RF                                                            
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         AR    R3,RF                                                            
         L     RE,ATSARREC                                                      
         STCM  R3,3,TSARLEN-TSARRECD(RE)                                        
         CH    R0,=H'1'                                                         
         BNH   *+8                                                              
         OI    TSDSTATB,TSDMWCQ    SET MULTIPLE WORK CODES                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+10                                                             
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         CR    RB,RB                                                            
*                                                                               
BLDTX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         ZAP   ORDAMT,=P'0'                                                     
         ZAP   INVAMT,=P'0'                                                     
         ZAP   INVNUM,=P'0'                                                     
         L     R0,ADUMLINE         CLEAR TSAR RECORD                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R3                                                      
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FORM150                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         MVC   SCR1SUPP,TSDSUPP    JOB NUMBER                                   
         MVC   TMPNBLK,SPACES                                                   
         GOTO1 VCHOPPER,DMCB,(L'TSDNAME,TSDNAME),(L'TMPNB1,TMPNBLK),2           
         MVC   SCR1NAME,TMPNB1     LINE 1 OF JOB NAME                           
         MVC   SCR1ONUM,TSDONUM    ORDER NUMBER                                 
         GOTO1 VDATCON,DMCB,(1,TSDODATE),(17,SCR1ODAT) ORDER DATE               
         MVI   LINSUSED,0                                                       
         LA    R3,TSDRFST                                                       
FORM01   CLI   0(R3),EOR           END OF RECORD?                               
         BE    FORM09                                                           
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    FORM03                                                           
*                                                                               
FORM02   SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     FORM01                                                           
*                                                                               
         USING OAMELD,R3           ORDER AMOUNT ELEMENT                         
FORM03   AP    ORDAMT,OAMAMNT      ORDER AMOUNT                                 
         AP    INVAMT,OAMIVAL      INVOICE AMOUNT                               
         AP    INVNUM,OAMINUM      SAVE NUMBER OF INVOICES                      
         MVC   SCR1OWC,OAMWORK     ORDER WORKCODE                               
         PACK  TEMP(L'OAMAMNT),NINES(L'SCR1OAMT-2) GET STRING OF NINES          
         CP    TEMP(L'OAMAMNT),OAMAMNT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM04                                                           
         MP    TEMP(L'OAMAMNT),=P'-1'                                           
         CP    TEMP(L'OAMAMNT),OAMAMNT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM04                                                           
         CURED (P6,OAMAMNT),(L'SCR1OAMT,SCR1OAMT),2,MINUS=YES                   
         B     FORM05                                                           
FORM04   CURED (P6,OAMAMNT),(L'SCR1OAMT,SCR1OAMT),2,MINUS=YES,         X        
               DECS=ROUND                                                       
*                                                                               
FORM05   PACK  TEMP(L'OAMIVAL),NINES(L'SCR1IAMT-2) GET STRING OF NINES          
         CP    TEMP(L'OAMIVAL),OAMIVAL IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM06                                                           
         MP    TEMP(L'OAMIVAL),=P'-1'                                           
         CP    TEMP(L'OAMIVAL),OAMIVAL IS AMOUNT TO LOW TO FIT?                 
         BH    FORM06                                                           
         CURED (P6,OAMIVAL),(L'SCR1IAMT,SCR1IAMT),2,ZERO=BLANK,        X        
               MINUS=YES                                                        
         B     FORM07                                                           
FORM06   CURED (P6,OAMIVAL),(L'SCR1IAMT,SCR1IAMT),2,ZERO=BLANK,        X        
               MINUS=YES,DECS=ROUND                                             
*                                                                               
FORM07   TM    TSDSTATB,TSDMWCQ    MULTIPLE WORKCODES?                          
         BO    FORM08                                                           
         CURED (P6,OAMINUM),(L'SCR1INUM,SCR1INUM),0,ZERO=BLANK                  
FORM08   SR    RF,RF                                                            
         IC    RF,LINSUSED         BUMP UP THE NUMBER OF LINES USED             
         LA    RF,1(RF)                                                         
         STC   RF,LINSUSED         SAVE                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         CLI   LINSUSED,MAXWCS                                                  
         BL    *+12                                                             
         OI    DISPFLG2,DISWCOVF   TOO MANY WORKCODES TO DISPLAY                
         B     FORMX                                                            
*                                                                               
         CLI   LINSUSED,2                                                       
         BNE   FORM02                                                           
         CLC   TMPNB2,SPACES                                                    
         BE    FORM02                                                           
         MVC   SCR2NAME-SCRLIN2D(L'SCR2NAME,R2),TMPNB2                          
         B     FORM02                                                           
*                                                                               
FORM09   CLI   LINSUSED,1                                                       
         BH    FORM10                                                           
         CLC   TMPNB2,SPACES                                                    
         BE    FORMX                                                            
         MVC   SCR2NAME-SCRLIN2D(L'SCR2NAME,R2),TMPNB2                          
         MVI   LINSUSED,2                                                       
         B     FORMX                                                            
*                                                                               
FORM10   TM    TSDSTATB,TSDMWCQ    MULTIPLE WORK CODES?                         
         BNO   FORMX                                                            
         MVC   SCR1ORDT,SPACES                                                  
         MVC   SCR1ORDT(L'MX@ORDT),MX@ORDT                                      
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         LA    RF,1(RF)                                                         
         STC   RF,LINSUSED                                                      
         PACK  TEMP(L'ORDAMT),NINES(L'SCR1OAMT-2) GET STRING OF NINES           
         CP    TEMP(L'ORDAMT),ORDAMT IS AMOUNT TOO HIGH TO FIT?                 
         BL    FORM110                                                          
         MP    TEMP(L'ORDAMT),=P'-1'                                            
         CP    TEMP(L'ORDAMT),ORDAMT IS AMOUNT TO LOW TO FIT?                   
         BH    FORM110                                                          
         CURED (P6,ORDAMT),(L'SCR1OAMT,SCR1OAMT),2,MINUS=YES                    
         B     FORM120                                                          
FORM110  CURED (P6,ORDAMT),(L'SCR1OAMT,SCR1OAMT),2,MINUS=YES,          X        
               DECS=ROUND                                                       
*                                                                               
FORM120  PACK  TEMP(L'INVAMT),NINES(L'SCR1IAMT-2) GET STRING OF NINES           
         CP    TEMP(L'INVAMT),INVAMT IS AMOUNT TOO HIGH TO FIT?                 
         BL    FORM130                                                          
         MP    TEMP(L'INVAMT),=P'-1'                                            
         CP    TEMP(L'INVAMT),INVAMT IS AMOUNT TO LOW TO FIT?                   
         BH    FORM130                                                          
         CURED (P6,INVAMT),(L'SCR1IAMT,SCR1IAMT),2,MINUS=YES                    
         B     FORM140                                                          
FORM130  CURED (P6,INVAMT),(L'SCR1IAMT,SCR1IAMT),2,MINUS=YES,          X        
               DECS=ROUND                                                       
*                                                                               
FORM140  CURED (P6,INVNUM),(L'SCR1INUM,SCR1INUM),0,ZERO=BLANK                   
         B     FORMX                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
         USING TSARRECD,R3                                                      
FORM150  LA    R2,L'DUMLIN1(R2)    BLANK LINE                                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@TOTAL                                                
         PACK  TEMP(L'TSTOTOT),NINES(L'SCROTOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FROM20                                                           
         MP    TEMP(L'TSTOTOT),=P'-1'                                           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TO LOW TO FIT?                 
         BH    FROM20                                                           
         CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,MINUS=YES                     
         B     FROM30                                                           
FROM20   CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FROM30   PACK  TEMP(L'TSTITOT),NINES(L'SCRITOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTITOT),TSTITOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FROM40                                                           
         MP    TEMP(L'TSTITOT),=P'-1'                                           
         CP    TEMP(L'TSTITOT),TSTITOT IS AMOUNT TO LOW TO FIT?                 
         BH    FROM40                                                           
         CURED (P6,TSTITOT),(L'SCRITOT,SCRITOT),2,MINUS=YES                     
         B     FORM50                                                           
FROM40   CURED (P6,TSTITOT),(L'SCRITOT,SCRITOT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM50   MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
         DROP  R2,R3,R4                                                         
*                                                                               
FORMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         ZAP   ORDAMT,=P'0'                                                     
         ZAP   INVAMT,=P'0'                                                     
         ZAP   INVNUM,=P'0'                                                     
         L     R0,ADUMLINE         CLEAR TSAR RECORD                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC         R3=A(TSAR RECORD )                           
*                                                                               
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB       DISPLAY DUMMY SCREEN LINES               
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM                                                 
         BE    FGRM30                                                           
         CLI   TSARFMT,TSITEM1                                                  
         BNE   FGRMX                                                            
*                                                                               
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
*                                                                               
*        L     R0,ADUMLINE         CLEAR TSAR RECORD                            
*        LH    R1,=Y(DUMLINLN)                                                  
*        SR    RE,RE                                                            
*        LA    RF,X'40'                                                         
*        SLL   RF,24                                                            
*        MVCL  R0,RE                                                            
*        L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
*        USING SCRTOT1D,R2                                                      
*                                                                               
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
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         MVC   TSTOTAL,MX@TOTAL    TOTAL                                        
         ZAP   TSTOTOT,ORDTOT      ORDER TOTAL                                  
         ZAP   TSTITOT,INVTOT      INVOICE TOTAL                                
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
         GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ORDER    DC    C'**'               ORDER RECORD                                 
NINES    DC    C'99999999999999999999'                                          
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH7,78                                                       
         DCDDL AC#ENH8,78                                                       
         DCDDL AC#ACC,9                                                         
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#ORDT,15                                                       
         DCDDL AC#XJOB,11                                                       
*                                                                               
         DCDDL AC#RSVNC,L'MX@RSVNC                VENDOR CODE                   
         DCDDL AC#RSVNN,L'MX@RSVNN                VENDOR NAME                   
         DCDDL AC#RSPO,L'MX@RSPO                  ORDER NUMBER                  
         DCDDL AC#RSPOD,L'MX@RSPOD                ORDER DATE                    
         DCDDL AC#WC2,L'MX@WC2                    W/C                           
         DCDDL AC#ORDAM,L'MX@ORDAM                ORDER AMOUNT                  
         DCDDL AC#INVCD,L'MX@INVCD                INVOICED AMOUNT               
         DCDDL AC#INVT2,L'MX@INVT2                TOTAL INVOICED                
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*      ORDER DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD             *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'MX@RSVNC,L'TSDSUPP)    VENDOR CODE               
         DC    AL2(MX@RSVNC-OVERWRKD,TSDSUPP-TSARDATD)                          
         DC    AL1(GCTITOT+GCTIOVER,0,0,0)                                      
         DC    AL1(0,L'MX@TOTAL)                                                
         DC    AL2(MX@TOTAL-OVERWRKD)                                           
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'MX@RSVNN,L'TSDNAME)    VENDOR NAME               
         DC    AL2(MX@RSVNN-OVERWRKD,TSDNAME-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL2(0,0,0,0)                                                     
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'MX@RSPO,L'TSDONUM)     ORDER NUMBER              
         DC    AL2(MX@RSPO-OVERWRKD,TSDONUM-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0,0,0)                                                     
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'MX@RSPOD,1)            ORDER DATE                
         DC    AL2(MX@RSPOD-OVERWRKD,TSDODATE-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'LC@WC2,0)              WORK CODE                 
         DC    AL2(LC@WC2-WORKD,AGRDSP-OVERWRKD)                                
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT5LQ   EQU   *-GCTCOL5                                                        
GCTWCQ   EQU   5                                                                
*                                                                               
GCTCOL6  DC    AL1(GCT6LQ,06,L'MX@ORDAM,0)            ORDER AMOUNT              
         DC    AL2(MX@ORDAM-OVERWRKD,AGRDSP-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,GCTFNUM,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT6LQ   EQU   *-GCTCOL6                                                        
GCTOAQ   EQU   6                                                                
*                                                                               
GCTCOL7  DC    AL1(GCT7LQ,07,L'MX@INVCD,0)            INVOICED AMOUNT           
         DC    AL2(MX@INVCD-OVERWRKD,AGRDSP-OVERWRKD)                           
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFNUM,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT7LQ   EQU   *-GCTCOL7                                                        
GCTIVAQ  EQU   7                                                                
*                                                                               
GCTCOL8  DC    AL1(GCT8LQ,08,L'MX@ORDT,L'ORDAMT)      ORDER TOTAL               
         DC    AL2(MX@ORDT-OVERWRKD,ORDAMT-OVERWRKD)                            
         DC    AL1(GCTIOVER+GCTITOT,GCTISPDA,GCTFNUM+GCTFRGHT,0)                
         DC    AL1(0,L'TSTOTOT),AL2(TSTOTOT-TSARTOTD)                           
GCT8LQ   EQU   *-GCTCOL8                                                        
*                                                                               
GCTCOL9  DC    AL1(GCT9LQ,09,L'MX@INVT2,L'INVAMT)     TOTAL INVOICED            
         DC    AL2(MX@INVT2-OVERWRKD,INVAMT-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTITOT,GCTISPDA,GCTFNUM+GCTFRGHT,0)                
         DC    AL1(0,L'TSTITOT),AL2(TSTITOT-TSARTOTD)                           
GCT9LQ   EQU   *-GCTCOL9                                                        
*                                                                               
GCTCOL10 DC    AL1(GCT10LQ,10,8,L'INVNUM)             # OF INVOICES             
         DC    AL2(MX@INVCD-OVERWRKD,INVNUM-OVERWRKD)                           
         DC    AL1(GCTIOVER,GCTISPDA,GCTFNUM+GCTFRGHT,GCTINDEC)                 
         DC    AL1(0,0),AL2(0)                                                  
GCT10LQ  EQU   *-GCTCOL10                                                       
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
GRDSP    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
*                                                                               
         L     R4,ATSARREC             R2=A(TSAR RECORD)                        
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         LA    R2,TSDRFST                                                       
         LA    R5,TEMP                                                          
GRDSP10  CLI   0(R2),EOR           END OF RECORD?                               
         BE    GRDSP75                                                          
         CLI   0(R2),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    GRDSP20                                                          
GRDSP15  SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     GRDSP10                                                          
*                                                                               
         USING OAMELD,R2           ORDER AMOUNT ELEMENT                         
*-------------------*                                                           
* CHECK COLUMN TYPE *                                                           
*-------------------*                                                           
GRDSP20  CLI   GCTCOID,GCTWCQ       WORKCODES                                   
         BE    GRDSP25                                                          
         CLI   GCTCOID,GCTOAQ       ORDER AMOUNTS                               
         BE    GRDSP30                                                          
         CLI   GCTCOID,GCTIVAQ      INVOICE AMOUNT                              
         BE    GRDSP40                                                          
         B     GRDSPERX                                                         
*                                                                               
*------------*                                                                  
* WORK CODES *                                                                  
*------------*                                                                  
GRDSP25  MVC   0(L'OAMWORK,R5),OAMWORK     ORDER WORKCODE                       
         LA    R5,L'OAMWORK(R5)                                                 
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         B     GRDSP50                                                          
*                                                                               
*---------------*                                                               
* ORDER AMOUNTS *                                                               
*---------------*                                                               
GRDSP30  AP    ORDAMT,OAMAMNT      ORDER AMOUNT                                 
         ZAP   DUB,OAMAMNT                                                      
         B     GRDSP45                                                          
*                                                                               
*-----------------*                                                             
* INVOICE AMOUNTS *                                                             
*-----------------*                                                             
GRDSP40  AP    INVAMT,OAMIVAL      INVOICE AMOUNT                               
         AP    INVNUM,OAMINUM      SAVE NUMBER OF INVOICES                      
         ZAP   DUB,OAMIVAL                                                      
         B     GRDSP45                                                          
*                                                                               
*----------------------------------------------------*                          
GRDSP45  CURED DUB,(20,(R5)),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                    
         AR    R5,R0                                                            
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         B     GRDSP50                                                          
*----------------------------------------------------*                          
*                                                                               
GRDSP50  TM    TSDSTATB,TSDMWCQ    MULTIPLE WORKCODES?                          
         BO    GRDSP15                                                          
*                                                                               
GRDSP75  LA    RE,TEMP                                                          
         SR    R5,RE                                                            
         BNP   GRDSPERX                                                         
         LR    R1,R5                                                            
*                                                                               
GRDSPX   J     XITR1                                                            
*                                                                               
GRDSPERX MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
         J     XITR1                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,R4,RB                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
OVERWRKD DSECT                                                                  
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGRDSP   DS    A                   SPECIAL GRID ROUTINES                        
ACCNAM   DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ACCNAMLN DS    X                   ACCOUNT NAME LENGTH                          
JOBSTAT  DS    XL(L'JOBSTA1)       JOB STATUS                                   
TMPNBLK  DS    0CL40               TEMPORARY JOB NAME BLOCK                     
TMPNB1   DS    CL20                TEMP JOB NAME LINE 1                         
TMPNB2   DS    CL20                TEMP JOB NAME LINE 2                         
ORDAMT   DS    PL6                 TOTAL ORDER AMOUNT                           
INVAMT   DS    PL6                 TOTAL INVOICE AMOUNT                         
INVNUM   DS    PL6                 NUMBER OF INVOICES                           
*                                                                               
**********************************************************************          
* DATA DICTIONARY                                                               
**********************************************************************          
DSMIX    DS    0C                                                               
MX@ENH7  DS    CL78                                                             
MX@ENH8  DS    CL78                                                             
MX@ACC   DS    CL9                                                              
MX@TOTAL DS    CL9                                                              
MX@ORDT  DS    CL15                                                             
MX@XJOB  DS    CL11                                                             
MX@RSVNC DS    CL11                VENDOR CODE                                  
MX@RSVNN DS    CL11                VENDOR NAME                                  
MX@RSPO  DS    CL12                ORDER NUMBER                                 
MX@RSPOD DS    CL10                ORDER DATE                                   
MX@WC2   DS    CL3                 W/C                                          
MX@ORDAM DS    CL12                ORDER AMOUNT                                 
MX@INVCD DS    CL15                INVOICED AMOUNT                              
MX@INVT2 DS    CL13                TOTAL INVOICED                               
*                                                                               
**********************************************************************          
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1SUPP DS    CL(L'TRNKCACT)      SUPPLIER CODE                                
         DS    CL1                                                              
SCR1NAME DS    CL20                SUPPLIER NAME (PART 1)                       
         DS    CL1                                                              
SCR1ORDT DS    0CL15               ORDER TOTAL                                  
SCR1ONUM DS    CL6                 ORDER NUMBER                                 
         DS    CL1                                                              
SCR1ODAT DS    CL8                 ORDER DATE                                   
         DS    CL1                                                              
SCR1OWC  DS    CL2                 ORDER WORKCODE                               
         DS    CL1                                                              
SCR1OAMT DS    CL10                ORDER AMOUNT                                 
         DS    CL1                                                              
SCR1IAMT DS    CL10                INVOICED AMOUNT                              
         DS    CL1                                                              
SCR1INUM DS    CL3                 NUMBER OF INVOICES                           
*                                                                               
SCRLIN2D DSECT                     COVER SCREEN ITEM LINE2                      
         DS    CL(L'TRNKCACT+1)                                                 
SCR2NAME DS    CL20                SUPPLIER NAME (PART 2)                       
*                                                                               
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
         DS    CL34                                                             
SCRTOTAL DS    CL(L'MX@TOTAL)      TOTAL                                        
         DS    CL9                                                              
SCROTOT  DS    CL11                ORDER TOTAL                                  
         DS    CL1                                                              
SCRITOT  DS    CL11                INVOICED TOTAL                               
*                                                                               
***********************************************************************         
* TSAR DATA                                                           *         
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDSUPP  DS    CL(L'TRNKCACT)      SUPPLIER CODE                                
TSDNAME  DS    CL36                SUPPLIER NAME                                
TSDONUM  DS    CL6                 ORDER NUMBER                                 
TSDODATE DS    XL3                 ORDER DATE                                   
TSDSTATB DS    XL1                 STATUS BYTE                                  
TSDMWCQ  EQU   X'80'               MULTIPLE WORKCODES ON ORDER                  
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTOTAL  DS    CL(L'MX@TOTAL)      TOTAL                                        
TSTOTOT  DS    PL(L'ORDTOT)        ORDER AMOUNT                                 
TSTITOT  DS    PL(L'INVTOT)        INVOICE AMOUNT                               
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
UNILDG   DS    CL2                 UNIT AND LEDGER                              
SUPCODE  DS    CL(L'TRNKCACT)      SUPPLIER ACCOUNT                             
SUPNAME  DS    CL(L'NAMEREC)       SUPPLIER NAME                                
ODVALS   DS    0PL6                CREDITOR VALUES                              
ORDTOT   DS    PL6                 ORDER TOTAL                                  
INVTOT   DS    PL6                 INVOICE TOTAL                                
ODVALLNQ EQU   *-ODVALS                                                         
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACENQ0C   09/20/07'                                      
         END                                                                    
