*          DATA SET ACENQ12    AT LEVEL 008 AS OF 07/23/13                      
*PHASE T62012C                                                                  
T62012   TITLE 'ACCOUNT ENQUIRY - ESTIMATED TIME'                               
T62012   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ12**,R7,CLEAR=YES,RR=RE                                   
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         A     RE,=A(GCTBL)        SET UP GRID COLUMN TABLE                     
         ST    RE,AGCTBL                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,NORECQ     NO RECORDS FOUND?                            
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN60                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE    GRIDS PROCESSING FINISHED?                   
         BO    MAINXGX                                                          
*                                                                               
         TM    DISPFLAG,ALLREADQ   IF ALL RECORDS READ, USE TSAR                
         BO    MAIN30                                                           
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN20                                                           
         TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BO    MAIN20                                                           
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
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT ONTO DUMMY LINES FOR GRIDS            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX                                                            
         MVC   TSLSTLIN,TSCURRNO   KEEP TRACK OF LAST TSAR REC USED             
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO       BUMP UP THE CURRENT TSAR NUMBER              
         B     MAIN20                                                           
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN50                                                           
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
*                                                                               
MAIN60   TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BO    MAIN66                                                           
         LA    R3,IOKEY            R3=A(IOAREA1)                                
         USING TRNRECD,R3                                                       
         CLC   TRNKDATE,SPACES     HAVE WE GOT A TRANSACTION RECORD?            
         BNH   MAIN150                                                          
         BAS   RE,FILTKEY          APPLY FILTERING                              
         BNE   MAIN150                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN65                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         MVC   KEYSAVE,IOKEYSAV    SAVE KEY FOR RESTORE                         
         B     MAINX                                                            
MAIN65   L     R3,AIO1                                                          
         CLC   SAVEWC,TRNKWORK     HAVE WE GOT A DIFFO WORKCODE?                
         BE    MAIN130                                                          
MAIN66   CP    HRSTOT,=P'0'        ANY ACTUAL HOURS?                            
         BE    MAIN80                                                           
*                                                                               
         TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BO    *+10                                                             
         MVC   KEYSAVE,TRNKEY      SAVE CURRENT KEY IN CASE OF RESTORE          
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         BNE   ERRXIT                                                           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN70              YES                                          
         SR    RF,RF               INCREMENT CURRENT TSAR REC NUMBER            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN80                                                           
*                                                                               
MAIN70   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                                                             
         OI    INTFLAG,SCRFULLQ    SCREEN FULL FLAG ON                          
         B     MAIN80                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN80   LA    R0,MAXWCQ           R0=(MAX NUMBER OF WORKCODES)                 
         LA    R2,WCLIST           R2=A(WORK CODE LIST)                         
         USING WCD,R2                                                           
         TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BNO   MAIN90                                                           
MAIN85   OC    WCCODE,WCCODE       END OF WORKCODE LIST?                        
         BZ    MAIN190                                                          
         CP    WCESTAMT,=P'0'                                                   
         BNE   MAIN100                                                          
         LA    R2,WCLNQ(R2)        BU,P TO NEXT ENTRY                           
         BCT   R0,MAIN85                                                        
         B     MAIN190             NO MATCH, READ HIGH FOR NEXT WC              
*                                                                               
MAIN90   CLC   TRNKWORK,WCCODE     MATCH ON TRANSACTION WORKCODE?               
         BE    MAIN100                                                          
         OC    WCCODE,WCCODE       END OF WORKCODE LIST?                        
         BZ    MAIN115                                                          
         LA    R2,WCLNQ(R2)        BUMP TO NEXT ENTRY                           
         BCT   R0,MAIN90                                                        
         B     MAIN115             NO MATCH, READ HIGH FOR NEXT WC              
*                                                                               
MAIN100  BAS   RE,GETWC            MATCH FOUND, READ WORKCODE RECORD            
         BE    MAIN120                                                          
MAIN110  ZAP   WCESTAMT,=P'0'      SET WORKCODE ESTIMATE AMOUNT TO ZERO         
         TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BO    MAIN80                                                           
MAIN115  LA    R3,IOKEY            WORKCODE REJECTED, READ HIGH FOR NXT         
         MVI   TRNKCCPY,X'FF'                                                   
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    MAIN160                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYSAVE,IOKEYSAV    SAVE KEY FOR RESTORE                         
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN120  TM    DISPFLAG,DISIOMAX   MAX IO REACHED?                              
         BNO   *+14                                                             
         MVC   KEYSAVE,TRNKEY                                                   
         B     MAINX                                                            
         ZAP   BUDTOT,WCESTAMT     GET ESTIMATED HOURS FOR WORKCODE             
         MVC   SAVEWC,WCCODE       UPDATE SAVED WORKCODE                        
         ZAP   WCESTAMT,=P'0'      SET WORKCODE ESTIMATE AMOUNT TO ZERO         
         TM    INTFLAG,SCRFULLQ    SCREEN FULL FLAG ON?                         
         BO    MAINX                                                            
         TM    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ?                
         BO    MAIN66                                                           
         DROP  R2                                                               
*                                                                               
MAIN130  LA    RF,TRNRFST          RF=A(TRANSACTION ELEMENT)                    
         USING SCIELD,RF                                                        
MAIN140  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
*                                                                               
         CLI   SCIEL,EOR           END OF RECORD?                               
         BE    MAIN150                                                          
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH INFO?                        
         BNE   MAIN140                                                          
*                                                                               
         CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BNE   MAIN150                                                          
         AP    HRSTOT,SCIAMNT      ACTUAL HOURS                                 
         DROP  RF                                                               
*                                                                               
MAIN150  GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN160                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   MAIN160                                                          
         MVC   KEYSAVE,TRNKEY                                                   
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN160  L     R3,AIO1             R3=A(IOAREA1 CONTAINING RECORD)              
         USING TRNRECD,R3                                                       
         CLC   TRNKCULA,IOKEYSAV   SAME JOB?                                    
         BE    MAIN60                                                           
MAIN170  OI    ETIMFLAG,ETALLTQ    ALL TRANSACTION RECORDS READ                 
         B     MAIN66                                                           
*                                                                               
*        CP    HRSTOT,=P'0'        ANY ACTUAL HOURS?                            
*        BNE   *+14                                                             
*        CP    BUDTOT,=P'0'        OR BUDGETED HOURS?                           
*        BE    MAIN190                                                          
*        BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
*        BNE   ERRXIT                                                           
*        CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
*        BNL   MAIN180             YES                                          
*        SR    RF,RF               UPDATE TSAR RECORD COUNTER                   
*        ICM   RF,3,TSCURRNO                                                    
*        LA    RF,1(RF)                                                         
*        STCM  RF,3,TSCURRNO                                                    
*        B     MAIN190                                                          
*                                                                               
*AIN180  GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
*        BE    *+12                                                             
*        OI    INTFLAG,SCRFULLQ    SCREEN FULL FLAG ON                          
*        B     *+10                                                             
*        MVC   TSLSTLIN,TSCURRNO   LAST TSAR REC DISPLAYED                      
*        SR    RF,RF                                                            
*        ICM   RF,3,TSCURRNO       INCREMENT CURRENT TSAR NUMBER                
*        LA    RF,1(RF)                                                         
*        STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN190  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
         OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         B     MAINX                                                            
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
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
         MVI   ETIMFLAG,0          ESTIMATED TIME FLAG                          
         ZAP   BUDTOT,=P'0'        BUDGETED HOURS TOTAL                         
         ZAP   HRSTOT,=P'0'        ACTUAL HOURS TOTAL                           
         ZAP   ESTTOT,=P'0'        PRESENT ESTIMATE TOTAL                       
         ZAP   ACTTOT,=P'0'        ACTUAL CHARGES TOTAL                         
         MVC   CONTRA,SPACES       CONTRA ACCOUNT CODE                          
         MVI   CONTLEN,0           CONTRA LENGTH                                
         MVI   NEGCONT,0           NEGATIVE CONTRA FILTER                       
         MVC   SVCACN,SPACES       CONTRA ACCOUNT NAME                          
         MVC   SAVEWC,SPACES       SAVED WORKCODE                               
         MVC   UNILDG,SPACES                                                    
*                                                                               
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
         MVC   UNILDG,SPROUNIT     SAVE UNIT/LEDGER                             
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    FSTDX                                                            
         MVC   FVMSGNO,=AL2(EAWRNGLV)                                           
         LA    RF,LEDGTLVA         RF=A(HIERARCHY LEVELS)                       
         LR    RE,RF               RE=A(HIERARCHY LEVELS)                       
         SR    R1,R1               R1=MINIMUM LENGTH OF ACCOUNT                 
         CLI   0(RF),X'0C'         FULL HIERARCHY?                              
         BE    *+12                                                             
         LA    RF,1(RF)            NO BUMP RF                                   
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
         BNO   *+6                                                              
         DC    H'0'                                                             
         MVC   KEYSAVE,IOKEYSAV                                                 
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BNO   FSTD23                                                           
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
FSTD50   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH24),MX@ENH24                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH25),MX@ENH25                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
FSTD60   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD63                                                           
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD65                                                           
*                                                                               
FSTD63   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         B     *+10                                                             
*                                                                               
FSTD65   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         BAS   RE,RDOPT                                                         
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    FSTDX                                                            
         BAS   RE,LOOKUP           GET ESTIMATES                                
*                                                                               
         MVC   IOKEY,KEYSAVE       READ FOR START OF SEQ                        
         LA    R3,IOKEY                                                         
         USING TRNRECD,R3                                                       
FSTD70   MVI   TRNKCCPY,X'FF'                                                   
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    FSTD80                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     FSTDX                                                            
*                                                                               
FSTD80   LA    R3,IOKEY            R3=A(RECORD IO IOAREA 1)                     
         CLC   TRNKEY(L'TRNKCULA),IOKEYSAV                                      
         BE    FSTDX                                                            
         OI    DISPFLAG,NORECQ     NO RECORDS FOUND ON JOB                      
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        READ WORKCODE RECORD (USING AIOAREA 2)                       *         
* ON ENTRY R3=A(IOAREA1 CONTAINING TRANSACTION RECORD)                *         
* ON EXIT  CC EQUAL-     ACTION COMPLETED OK                          *         
*          CC NOT EQUAL- WORKCODE REJECTED                            *         
***********************************************************************         
         SPACE 1                                                                
GETWC    NTR1                                                                   
         LA    R3,IOKEY            GET THE WORKCODE RECORD                      
         USING WCORECD,R3                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,MYCO                                                     
         MVC   WCOKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         MVC   WCOKWRK,WCCODE-WCD(R2)                                           
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    GETWC10                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     GETWCX                                                           
*                                                                               
GETWC10  L     R3,AIO2             R3=A(WORKCODE RECORD)                        
         LA    R4,WCORFST                                                       
GETWC20  CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETWCRJX                                                         
         CLI   0(R4),WCOELQ        WORK CODE ELEMENT?                           
         BE    GETWC30                                                          
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R0,R4                                                            
         B     GETWC20                                                          
*                                                                               
         USING WCOELD,R4                                                        
GETWC30  L     R3,AIO1                                                          
         MVC   IOKEY,0(R3)         REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    GETWC40                                                          
         CLI   IOERR,IOMAX                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
GETWC40  TM    WCOSTAT,WCOSHCOE    IS ESTIMATE FOR HOURS?                       
         BNO   GETWCRJX                                                         
         MVC   SAVEWCDS,WCODESC    GET DESCRIPTION                              
         DROP  R4                                                               
*                                                                               
GETWCX   CR    RB,RB                                                            
         BE    *+6                                                              
GETWCRJX LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FILTER JOBS                                                  *         
* ON ENTRY R3=A(TRANSACTION INDEX RECORD) ACCDIR                      *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING TRNRECD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,1,CONTLEN                                                     
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
FILTK05  L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
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
FILTK20  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK30                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,      X        
               ODATEFI                                                          
         BNE   FILTKRJX                                                         
*                                                                               
FILTK30  OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR  RB,RB                                                             
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
BLDTSDAT NTR1                                                                   
*                                                                               
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
*                                                                               
         MVC   TSDWC,SAVEWC        WORKCODE                                     
         MVC   TSDWCDES,SAVEWCDS   WORKCODE DESCRIPTION                         
         ZAP   TSDHEST,BUDTOT      ESTIMATED HOURS                              
         ZAP   TSDHACT,HRSTOT      ACTUAL HOURS                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BNE   BLDTERRX                                                         
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         AP    ESTTOT,BUDTOT       JOB ESTIMATE TOTAL                           
         AP    ACTTOT,HRSTOT       JOB ACTUAL TOTAL                             
         ZAP   BUDTOT,=P'0'        CLEAR WORKCODE TOTALS                        
         ZAP   HRSTOT,=P'0'                                                     
         MVC   SAVEWCDS,SPACES     CLEAR WORKCODE DESCRIPTION                   
         CR    RB,RB                                                            
*                                                                               
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
BLDTERRX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
         CLI   TSARFMT-TSARRECD(R4),TSTOTITM TOTAL LINE ITEM?                   
         BE    FORM10                                                           
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1WC,TSDWC        WORKCODE                                     
         MVC   SCR1DESC,TSDWCDES   WORKCODE DESCRIPTION                         
         LA    R3,TSDHEST          R3=A(ESTIMATED HOURS AMOUNT)                 
         LA    RF,SCR1HEST         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         LA    R3,TSDHACT          R3=A(ACTUAL HOURS AMOUNT)                    
         LA    RF,SCR1HACT         RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         ZAP   BALAMT,TSDHEST                                                   
         SP    BALAMT,TSDHACT                                                   
         LA    R3,BALAMT           R3=A(BALANCE AMOUNT)                         
         LA    RF,SCR1BAL          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         B     FORMX                                                            
         DROP  R2,R4                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM10   LA    R2,L'DUMLIN1(R2)    BLANK LINE                                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@TOTAL   TOTAL                                        
         LA    R3,TSTETOT          R3=A(ESTIMATED HOURS TOT FOR JOB)            
         LA    RF,SCRETOT          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         LA    R3,TSTATOT          R3=A(ACTUAL HOURS TOT FOR JOB)               
         LA    RF,SCRATOT          RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
         ZAP   BALAMT,TSTETOT                                                   
         SP    BALAMT,TSTATOT                                                   
         LA    R3,BALAMT           R3=A(BALANCE AMOUNT FOR JOB)                 
         LA    RF,SCRBAL           RF=A(LINE POSITION)                          
         BAS   RE,FORAMT                                                        
FORM160  MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMX                                                            
         DROP  R2,R4                                                            
*                                                                               
FORMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
         SPACE 1                                                                
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         USING TSARRECD,R4                                                      
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
*                                                                               
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB                                                
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
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
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT AN AMOUNT                                             *         
* ON ENTRY R3=A(PACKED AMOUNT OF LENGTH 6)                            *         
*          RF=A(POSITION ON LINE WHERE AMOUNT WILL BE DISPLAYED)      *         
***********************************************************************         
         SPACE 1                                                                
FORAMT   NTR1                                                                   
         PACK  TEMP(8),NINES(L'SCR1HEST-2) GET STRING OF NINES                  
         CP    TEMP(8),0(6,R3)  IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORA10                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),0(6,R3)  IS AMOUNT TO LOW TO FIT?                        
         BH    FORA10                                                           
         CURED (P6,(R3)),(L'SCR1HEST,(RF)),2,MINUS=YES                          
         B     FORAX                                                            
FORA10   CURED (P6,(R3)),(L'SCR1HEST,(RF)),2,MINUS=YES,DECS=ROUND               
*                                                                               
FORAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC         R3=A(TSAR RECORD AREA)                       
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLNQ)                                                    
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTETOT,ESTTOT      ESTIMATE TOTAL                               
         ZAP   TSTATOT,ACTTOT      ACTUAL TOTAL                                 
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    TOTX                                                             
*                                                                               
TOT50    GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO   LAST TSAR DISPLAYED                          
TOTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CALL GETOPT FOR JOBBER CALL                                  *         
***********************************************************************         
         SPACE 1                                                                
RDOPT    NTR1                                                                   
         LA    R3,IOKEY                                                         
         USING CPYRECD,R3                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,MYCO                                                     
         GOTO1 AIO,IOREAD+IOACCMST+IO3                                          
         BE    RDOP10                                                           
         TM    IOERR,IOMAX         HAVE WE EXCEEDED MAX IO'S?                   
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   YES                                          
         B     RDOPX                                                            
*                                                                               
RDOP10   GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO3                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         MVC   GOADM,VDATAMGR                                                   
         L     RE,AOPTBUFF                                                      
         ST    RE,GOABUFF                                                       
         LA    RE,L'OPTBUFF                                                     
         ST    RE,GOLBUFF                                                       
         L     RF,AIO3             RF=A(COMPANY RECORD)                         
         ST    RF,GOACOMP                                                       
         XC    GOAKEY,GOAKEY       CLEAR SEQUENCE RESET                         
         L     R4,AIO1                                                          
         ST    R4,GOAKEY                                                        
*        ST    R4,GOAJOB                                                        
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,LEDGTLVA                                                      
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELPRO,SPACES                                                  
         IC    R1,LEDGTLVB                                                      
         SR    R1,RF                                                            
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELJOB,SPACES                                                  
         IC    R1,LEDGTLVC                                                      
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R4)                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYWC,C'N'                                                     
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
*                                                                               
RDOPX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CALL JOBBER FOR ORIGINAL ESTIMATE                            *         
***********************************************************************         
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         LA    R0,WCLIST           CLEAR TSAR RECORD                            
         LH    R1,=Y(MAXWCQ*L'WCLIST)                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ACOLIST                                                       
         OC    0(L'COLIST,RF),0(RF)                                             
         BNZ   LOOK10                                                           
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
*                                                                               
LOOK10   L     R5,AJOBLOCK                                                      
         USING JBLOCKD,R5                                                       
         LR    RE,R5                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO1                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
         MVC   JBAJOB,AIO1                                                      
         XC    JBAKEY,JBAKEY       CLEAR SEQUENCE RESET                         
         L     R1,AIO1                                                          
         ST    R1,JBAKEY                                                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         L     R1,AGOBLOCK                                                      
         ST    R1,JBAGOBLK                                                      
         MVC   JBGETOPT,VGETOPT                                                 
         L     RE,AIO2                                                          
         ST    RE,JBAIO                                                         
         LR    RF,RE                                                            
         L     RF,ACOLTAB                                                       
         ST    RF,JBACOLTB                                                      
         LA    RF,L'COLTAB                                                      
         ST    RF,JBLCOLTB                                                      
         L     RF,AOPTVTAB                                                      
         ST    RF,JBAOPVTB                                                      
         LA    RF,L'OPTVTAB                                                     
         ST    RF,JBLOPVTB                                                      
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,LOOKFLDH                                                      
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WCLIST,WCLIST       CLEAR WORKCODE LIST                          
         LA    RE,WCLIST           RE=A(WORKCODE LIST)                          
         USING WCD,RE                                                           
         SR    R1,R1               R0=NUMBER OF WORKCODES)                      
         L     RF,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    LOOK50                                                           
         USING JBCOLD,RF                                                        
         SR    R0,R0                                                            
         ICM   R0,3,JBNROWS        NUMBER OF ESTIMATE ROWS                      
         BZ    LOOK40                                                           
LOOK20   CLI   JBCOLTYP,JBCOLTWC   WORKCODE ROWS ONLY                           
         BNE   LOOK30                                                           
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         OC    OWCODE,OWCODE       FILTERING ON WORKCODE?                       
         BZ    LOOK25                                                           
         CLC   OWCODEVL,JBCOLWC                                                 
         BE    LOOK24                                                           
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   LOOK30                                                           
         B     LOOK25                                                           
LOOK24   CLI   OWCODEFI,NEGFILTR                                                
         BE    LOOK30                                                           
         DROP  R2                                                               
LOOK25   MVC   WCCODE,JBCOLWC      GET WORKCODE                                 
         ZAP   WCESTAMT,JBCOLVAL   GET ESTIMATE AMOUNT                          
         LA    RE,WCLNQ(RE)        BUMP TO NEXT FREE POSITION                   
         LA    R1,1(R1)            INCREMENT WORKCODE COUNT                     
LOOK30   AH    RF,JBLCOL           BUMP TO NEXT JOBBER ROW                      
         BCT   R0,LOOK20                                                        
         CH    R1,=Y(MAXWCQ)                                                    
         BNH   LOOK40                                                           
         DC    H'0'                WORKCODE TABLE NOT BIG ENOUGH                
*                                                                               
LOOK40   GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIO1                                     
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R5,RF                                                            
*                                                                               
         USING MJETABD,RF                                                       
LOOK50   CLI   MJETTYP,MJETTEQ     ARE WE AT END?                               
         BE    LOOK40                                                           
         CLI   MJETTYP,MJETTWQ     WORKCODE ROWS ONLY                           
         BNE   LOOK80                                                           
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   LOOK80                                                           
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         OC    OWCODE,OWCODE       FILTERING ON WORKCODE?                       
         BZ    LOOK70              NO                                           
         CLC   OWCODEVL,MJETWCD                                                 
         BE    LOOK60                                                           
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   LOOK80                                                           
         B     LOOK70                                                           
*                                                                               
LOOK60   CLI   OWCODEFI,NEGFILTR                                                
         BE    LOOK80                                                           
         DROP  R2                                                               
*                                                                               
LOOK70   MVC   WCCODE,MJETWCD      GET WORKCODE                                 
         ZAP   WCESTAMT,MJETVAL    GET ESTIMATE AMOUNT                          
         LA    RE,WCLNQ(RE)        BUMP TO NEXT FREE POSITION                   
         LA    R1,1(R1)            INCREMENT WORKCODE COUNT                     
         CH    R1,=Y(MAXWCQ)                                                    
         BNH   *+6                                                              
         DC    H'0'                WORKCODE TABLE NOT BIG ENOUGH                
*                                                                               
LOOK80   XR    R0,R0                                                            
         IC    R0,MJETLEN                                                       
         AR    RF,R0                                                            
         B     LOOK50                                                           
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
LOOKFLDH DC    AL1(8+L'LOOKFLD)                                                 
         DC    XL4'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL2'00'                                                          
LOOKFLD  DC    C'OE'                                                            
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH24,78                                                      
         DCDDL AC#ENH25,78                                                      
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#ACC,9                                                         
         DCDDL AC#CTR,9                                                         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*    ESTIMATED TIME GRID COLUMN TABLE - COVERED BY GCTBLD             *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'UP@WC,L'TSDWC)        WORK CODE                  
         DC    AL2(UP@WC-WORKD,TSDWC-TSARDATD)                                  
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'UP@WC,L'TSDWCDES)     WORK CODE NAME             
         DC    AL2(UP@WC-WORKD,TSDWCDES-TSARDATD)                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'UP@ESTML,L'TSDHEST)   TIME ESTIMATE              
         DC    AL2(UP@ESTML-WORKD,TSDHEST-TSARDATD)                             
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTETOT),AL2(TSTETOT-TSARTOTD)                           
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'UP@HOURS,L'TSDHACT)   ACTUAL HOURS               
         DC    AL2(UP@HOURS-WORKD,TSDHACT-TSARDATD)                             
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTATOT),AL2(TSTATOT-TSARTOTD)                           
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
OVERWRKD DSECT                                                                  
INTFLAG  DS    X     INTERNAL FLAG                                              
SCRFULLQ EQU   1     SCREEN IS FULL                                             
BALAMT   DS    PL6   BALANCE AMOUNT                                             
         SPACE 1                                                                
DSMIX    DS    0C                                                               
MX@ENH24 DS    CL78                                                             
MX@ENH25 DS    CL78                                                             
MX@TOTAL DS    CL9                                                              
MX@ACC   DS    CL9                                                              
MX@CTR   DS    CL9                                                              
         EJECT                                                                  
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1WC   DS    CL(L'TRNKWORK)      WORK CODE                                    
         DS    CL1                                                              
SCR1DESC DS    CL(L'WCODESC)       WORKCODE DESCRIPTION                         
         DS    CL12                                                             
SCR1HEST DS    CL13                ESTIMATED HOURS                              
         DS    CL1                                                              
SCR1HACT DS    CL13                ACTUAL HOURS                                 
         DS    CL2                                                              
SCR1BAL  DS    CL13                BALANCE                                      
         SPACE 1                                                                
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@TOTAL)      TOTAL                                        
         DS    CL21                                                             
SCRETOT  DS    CL13                ESTIMATE HOURS JOB TOTAL                     
         DS    CL1                                                              
SCRATOT  DS    CL13                ACTUAL HOURS JOB TOTAL                       
         DS    CL2                                                              
SCRBAL   DS    CL13                JOB BALANCE                                  
         SPACE 1                                                                
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDWC    DS    CL(L'TRNKWORK)      WORK CODE                                    
TSDWCDES DS    CL(L'WCODESC)       WORK CODE DESCRIPTION                        
TSDHEST  DS    PL(L'JBCOLVAL)      ESTIMATED HOURS                              
TSDHACT  DS    PL(L'SCIAMNT)       ACTUAL HOURS                                 
TSDLENQ  EQU   *-TSARDATD                                                       
         SPACE 1                                                                
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTETOT  DS    PL(L'JBCOLVAL)      ESTIMATE TOTAL                               
TSTATOT  DS    PL(L'SCIAMNT)       ACTUAL TOTAL                                 
TSTLNQ   EQU   *-TSARTOTD                                                       
         SPACE 1                                                                
WCD      DSECT COVER WORKCODE LIST                                              
WCCODE   DS    CL(L'TRNKWORK)      WORKCODE                                     
WCESTAMT DS    CL(L'JBCOLVAL)      WORKCODE ESTIMATE AMOUNT (HOURS)             
WCLNQ    EQU   *-WCD                                                            
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
UNILDG   DS    CL2                 UNIT AND LEDGER                              
ETIMFLAG DS    X                   ESTIMATED TIME FLAG                          
ETALLTQ  EQU   X'80'               ALL TRANSACTION RECORDS READ                 
SAVEWC   DS    CL(L'TRNKWORK)      WORK CODE                                    
SAVEWCDS DS    CL(L'WCODESC)       WORK CODE DESCRIPTION                        
WCLIST   DS    (MAXWCQ)XL(L'WCOKWRK+L'JBCOLVAL)                                 
ETVALS   DS    0PL8                CREDITOR VALUES                              
HRSTOT   DS    PL8                 HOURS TOTAL                                  
BUDTOT   DS    PL8                 BUDGET TOTAL                                 
ACTTOT   DS    PL8                 ACTUAL CHARGES TOTAL                         
ETVALLNQ EQU   *-ETVALS                                                         
ESTTOT   DS    PL6                 ESTIMATE TOTAL                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACENQ12   07/23/13'                                      
         END                                                                    
