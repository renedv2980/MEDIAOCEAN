*          DATA SET ACENQ05    AT LEVEL 014 AS OF 08/02/17                      
*PHASE T62005A                                                                  
T62005   TITLE 'ACCOUNT FIS - LIST'                                             
T62005   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ENQ05**,R7,R6,CLEAR=YES,RR=RE                                
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL                                                        
*                                                                               
         L     RF,=A(DISCUS)                                                    
         AR    RF,RE                                                            
         ST    RF,ADISCUS                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN20                                                           
         BAS   RE,FSTDIS           PERFORM FIRST DISPLAY FUNCTIONS              
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S BEEN REACHED?                       
         BO    MAINX                                                            
         TM    DISPFLAG,NORECQ     NO RECORDS FOUND?                            
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN50                                                           
*                                                                               
MAIN20   TM    OVRSTAT,OVRGDONE    HAVE WE ALREADY FINISHED GRIDS?              
         BO    MAINXGX             NO                                           
*                                                                               
MAIN22   CLC   TSCURRNO,TSLSTREC   HAVE WE ALREADY GOT RECORD IN TSAR?          
         BH    MAIN30              NO                                           
*                                                                               
         GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    *+12                . NO                                         
         BAS   RE,FGRMTSAR         . YES, FORMAT FOR GRIDS                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAIN170             IS SCREEN FULL?                              
*                                                                               
         MVC   TSLSTLIN,TSCURRNO   KEEP TRACK OF LAST TSAR REC USED             
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO       BUMP UP THE CURRENT TSAR NUMBER              
         B     MAIN22                                                           
*                                                                               
MAIN30   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN40                                                           
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN40   TM    LISTFLAG,OUTSTNDQ   ANYTHING OUTSTANDING IN 'BALTAB'?            
         BNO   MAIN45                                                           
         BAS   RE,BLDDIS           BUILD AND DISPLAY ACCOUNT TSAR RECS          
         BNE   MAIN170                                                          
*                                                                               
MAIN45   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX                                                            
*                                                                               
MAIN50   GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET AN ACC DIRECTORY RECORD              
         BE    MAIN60                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN60   LA    R3,IOKEY            R3=A(ACCOUNT KEY)                            
         USING ACTRECD,R3                                                       
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEYSAV                                  
         BNE   MAIN150             SAME COMPANY/UNIT/LEDGER?                    
         MVC   KEYSAVE,IOKEY       SAVE KEY FOR SEQUENCE RESTORE                
         CLC   CACKOFF-CACRECD(L'CACKCULC+L'CACKOFF,R3),SPACES                  
         BH    MAIN120                                                          
*                                                                               
         LA    RE,L'ACTKACT        RE=L'(ACCOUNT CODE)                          
         LA    RF,ACTKACT-1(RE)                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-12                                                          
         LA    R1,1                R1=(LEVEL OF CURRENT ACCOUNT)                
         LA    RF,LEDGTLVA         RF=A(LEDGER STRUCTURE)                       
*                                                                               
MAIN70   CLI   0(RF),L'ACTKACT     END OF LEDGER TABLE?                         
         BE    MAIN80                                                           
         CLM   RE,1,0(RF)                                                       
         BNH   MAIN80                                                           
         LA    R1,1(R1)            BUMP UP LEVEL COUNTER                        
         LA    RF,L'LEDGTLVA(RF)   BUMP UP LEDGER TABLE                         
         B     MAIN70                                                           
*                                                                               
MAIN80   STC   R1,ACCLEV           SAVE LEVEL OF CURRENT ACCOUNT                
*                                                                               
         BAS   RE,FILTKEY          FILTER ACCOUNT KEY                           
         BNE   MAIN120                                                          
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OLEVEL,OLEVEL       ARE WE FILTERING ON ACCOUNT LEVEL?           
         BZ    MAIN90                                                           
         GOTO1 ASCOMP,DMCB,ACCLEV,(L'OLEVVL1,OLEVVL1),(L'OLEVVL2,OLEVVLX        
               2),OLEVFI                                                        
         BE    MAIN90                                                           
         DROP  RF                                                               
         CLC   ACCLEV,LDGLEVS      IF LOW LEVEL ACCOUNT GET DETAILS             
         BNE   MAIN120                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET AN ACCOUNT RECORD                     
         BE    MAIN81                                                           
         CLI   IOERR,IOMAX         MAX IO?                                      
         BE    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN81   L     R3,AIO1             R3=A(IOAREA1)                                
         GOTO1 AOFFACC             OFFICE CHECK ACCOUNT                         
         BNE   MAIN120                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAIN140                                                          
         BAS   RE,OFFFILT          OFFICE FILTER                                
         BNE   MAIN120             FILTER OUT                                   
*                                                                               
         BAS   RE,GETDET           GET ACCOUNT DETAILS                          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         B     MAIN120                                                          
*                                                                               
MAIN90   GOTO1 AIO,IOGET+IOACCMST+IO1 GET AN ACCOUNT RECORD                     
         BE    MAIN91                                                           
         CLI   IOERR,IOMAX         MAX IO?                                      
         BE    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN91   L     R3,AIO1                                                          
         CLC   ACCLEV,LDGLEVS      LOW LEVEL ACCOUNT CHECK OFFICE               
         BNE   MAIN92                                                           
         GOTO1 AOFFACC             OFFICE CHECK ACCOUNT                         
         BNE   MAIN120                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAIN140                                                          
         BAS   RE,OFFFILT          OFFICE FILTER                                
         BNE   MAIN120             FILTER OUT                                   
*                                                                               
MAIN92   CLC   SVLOWACC,SPACES     HAVE WE GOT LOWEST DISPLAY LEVEL?            
         BE    MAIN100                                                          
         SR    RF,RF                                                            
         IC    RF,LOWLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),SVLOWACC IS CURR REC IN SAME GROUP AS PREV?           
         BE    MAIN100                                                          
         MVC   KEYSAVE,ACTKEY      SAVE KEY IN CASE OF RESTORE                  
         BAS   RE,BLDDIS           BUILD/DISPLAY ACCOUNT TSAR REC(S)            
         BNE   MAIN170                                                          
*                                                                               
MAIN100  BAS   RE,GETDET           GET ACCOUNT DETAILS                          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         CLC   ACCLEV,LDGLEVS      LOWEST LEVEL?                                
         BE    MAIN110                                                          
         CLC   LOWLEV,ACCLEV       LOWEST DISPLAY LEVEL?                        
         BNE   MAIN105                                                          
         CLC   LOWLEV,LDGLEVS      LOWEST LEVEL ON LEDGER?                      
         BE    MAIN110             YES,GO AND DISPLAY ACCOUNT                   
         MVC   SVLOWACC,ACTKACT    SAVE LOWEST DISP LEV ACCOUNT CODE            
         MVC   SAVEACCN,ACCNAME    SAVE LOWEST DISP LEV ACCOUNT NAME            
         B     MAIN130                                                          
*                                                                               
MAIN105  BAS   RE,SVACCD           SAVE HIGHER LEVEL ACCOUNTS                   
         MVC   KEYSAVE,ACTKEY      SAVE CURRENT KEY IN CASE OF RESTORE          
         MVI   KEYSAVE+ACTKACT+L'ACTKACT-ACTRECD,X'FF'                          
         B     MAIN130                                                          
*                                                                               
MAIN110  MVC   KEYSAVE,ACTKEY      SAVE CURRENT KEY IN CASE OF RESTORE          
         MVI   KEYSAVE+ACTKACT+L'ACTKACT-ACTRECD,X'FF'                          
         BAS   RE,BLDDIS           DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAIN170                                                          
*                                                                               
MAIN120  CLC   ACCLEV,LDGLEVS      LOWEST LEVEL ON LEDGER?                      
         BNE   *+12                                                             
         MVI   IOKEY+ACTKACT+L'ACTKACT-ACTRECD,X'FF' READ HIGH FOR NEXT         
         B     MAIN50                                                           
*                                                                               
MAIN130  GOTO1 AIO,IOSEQ+IOACCDIR+IO1 READ SEQ FOR NEXT RECORD                  
         BE    MAIN60                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN140  MVC   KEYSAVE,ACTKEY                                                   
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN150  CLC   SVLOWACC,SPACES     ACCOUNT DETAILS OUTSTANDING?                 
         BE    MAIN155                                                          
         BAS   RE,BLDDIS           BULID/DISPLAY ACCOUNT TSAR REC(S)            
         BNE   MAIN170                                                          
         B     MAIN160                                                          
*                                                                               
MAIN155  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED?                       
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
*                                                                               
MAIN160  OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+10                                                             
         MVC   ENQDATL,SPACES      CLEAR TOTAL CARRIED FORWARD LINE             
         CLC   TSLSTLIN,TSCURRNO   IF TOTAL ALREADY DISPLAYED EXIT              
         BE    MAINX                                                            
*                                                                               
MAIN170  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+8                  YES                                         
         BAS   RE,TOTCFW           DISPLAY TOTALS CARRIED FORWARD               
         B     MAINX                                                            
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),BASKEY                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         MVC   SAVEACCN,SPACES                                                  
         XC    SVLOWOAC,SVLOWOAC   CLEAR LOW LEVEL ACTIVITY DATE                
         XC    SVLOWOPE,SVLOWOPE   CLEAR LOW LEVEL PEELED DATE                  
         XC    SVLOWOOP,SVLOWOOP   CLEAR LOW LEVEL OPENED DATE                  
         XC    OFFDONE,OFFDONE     INITIALISE VARIABLES                         
         MVI   LISTFLAG,0          INITIALISE LIST FLAG                         
*                                                                               
         LA    RF,LIVALS               LIVALS - LIST BUCKETS                    
         LHI   RE,(LIVALLNQ/L'LIVALS)   NUMBER OF BUCKETS                       
         ZAP   0(L'LIVALS,RF),=P'0'     ZAP TO ZERO                             
         LA    RF,L'LIVALS(RF)          NEXT BUCKET                             
         BCT   RE,*-10                  ZAP THEM ALL                            
*                                                                               
         MVC   SVLOWACC,SPACES                                                  
         MVC   KEYSAVE2,SPACES                                                  
         LA    RF,HILEV                                                         
         XC    0(SVHLNQ*SVHMAXQ,RF),0(RF) INIT HIGHER LEV ACC AREA              
         L     RF,ATIA             CLEAR BALANCE TABLE                          
         LA    RF,BALTAB-TIAD(RF)                                               
         LR    R0,RF                                                            
         LR    R2,RF                                                            
         LH    R1,=Y(MAXOFFQ*BALLNQ)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         USING BALTABD,R2          1ST ENTRY ALWAYS USED FOR ACC(S) TOT         
         ZAP   BALDR,=P'0'                                                      
         ZAP   BALCR,=P'0'                                                      
         ZAP   BALBFWD,=P'0'                                                    
         DROP  R2                                                               
*                                                                               
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         ST    R2,FVADDR                                                        
         USING FLDHDRD,R2                                                       
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CLI   FLDILEN,0           INPUT NOT ALLOWED IN THIS FIELD              
         BNE   ERRXIT                                                           
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(X'10',0)              
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OMEDIA,OMEDIA                                                    
         BZ    *+14                                                             
         CLC   SPROUL,BASKEY                                                    
         BNE   FSTDERR                                                          
         CLI   OBFWD,0             DEFAULT IS BF=Y                              
         BNE   *+8                                                              
         MVI   OBFWD,C'D'          USE 'D' TO INDICATE WE SET IT                
         DROP  RF                                                               
*                                                                               
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
*                                                                               
         BAS   RE,ACCESSOK         ACCESS TO LIST ACTION ALLOWED?               
         BNE   FSTDERR                                                          
*                                                                               
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN        R4=(L'START KEY)                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACTKULA(0),FLDDATA                                               
         CLI   ACTKACT,C' '                                                     
         BNE   *+8                                                              
         MVI   ACTKACT+L'ACTKACT,X'FF'                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    FSTD05               YES                                         
*                                                                               
         LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
FSTD05   GOTO1 ASETFIS                                                          
         GOTO1 ADISMOS,DMCB,(L'MX@MOA,MX@MOA),(L'MX@BFW,MX@BFW)                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    FSTD17               YES                                         
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH41),MX@ENH41                                     
*                                                                               
FSTD15   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH42),MX@ENH42                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*                                                                               
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD18                                                           
*                                                                               
FSTD17   GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD18   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         SR    R2,R2                                                            
         IC    R2,LDGLEVS          R2=(LOWEST DISPLAY LEVEL)                    
         STC   R2,LOWLEV                                                        
*                                                                               
         L     R3,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,R3                                                      
         OC    OLEVEL,OLEVEL       LOWEST LEVEL OVERRIDE?                       
         BZ    FSTD30                                                           
*                                                                               
FSTD20   STC   R2,LOWLEV                                                        
         GOTO1 ASCOMP,DMCB,LOWLEV,(L'OLEVVL1,OLEVVL1),(L'OLEVVL2,OLEVVL*        
               2),OLEVFI                                                        
         BE    FSTD30              IS THIS LEVEL IN REQUEST RANGE?              
         BCT   R2,FSTD20                                                        
         DROP  R3                                                               
         OI    DISPFLAG,NORECQ     NO LEDGER LEVELS IN REQUEST RANGE            
         B     FSTD35                                                           
*                                                                               
FSTD30   LA    R2,LEDGTLVA-1(R2)                                                
         MVC   LOWLEN,0(R2)        MAX LENGTH OF LOWEST LEV DISP ACC            
*-----------------------------------------                                      
* HOW TO GET OFFICE DETAIL                                                      
*-----------------------------------------                                      
         USING OPTVALSD,RE                                                      
FSTD35   L     RE,AOPTVALS         R2=A(OPTION VALUES)                          
*                                                                               
         CLC   UNITLEDG,SPROUL                                                  
         BE    FSTDX                                                            
         CLI   OOFFDET,C'Y'        OFFICE DETAIL NEEDED?                        
         BE    FSTD40                                                           
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER?                               
         BNZ   FSTD40               YES, NEED OFFICE DETAIL                     
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    FSTD40               YES, NEED OFFICE DETAIL                     
         CLI   TERMACCS,C'$'                                                    
         BNE   FSTDX                YES, NEED OFFICE DETAIL                     
*                                                                               
FSTD40   TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICE COMPANY?                       
         BZ    FSTDX                                                            
*                                                                               
         OI    LISTFLAG,NEEDOFFQ                                                
*                                                                               
         CLI   BASKEY,C'S'         SUBSIDIARY LEDGER?                           
         BE    *+12                                                             
         CLI   BASKEY,C'G'         GENERAL LEDGER?                              
         BNE   FSTDX                                                            
         OI    LISTFLAG,ROFFBUKQ   READ OFFICE/CONTRA BUCKETS                   
         DROP  RE                                                               
*                                                                               
FSTDX    J     OKXIT                                                            
FSTDERR  J     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*        FILTER OUT ACCOUNT BASED ON OFFICE FILTER                    *         
*        ON ENTRY R3=A(IOAREA 1 CONTAINING ACCOUNT RECORD)            *         
*        ON ENTRY RECOFFC MAY BE SET IN OFFACC (OFFPOS=PROD,FN)       *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
OFFFILT  NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
*                                                                               
         OC    OOFFPOS,OOFFPOS     ANY OFFICE FILTER ?                          
         BZ    OFFFX               . NO, EXIT                                   
         CLI   LDGTOFFP,LDGONONE   ANY OFFICE?                                  
         BE    OFFFX               . NO,EXIT                                    
*                                                                               
         LHI   R4,1                LENGTH-1 FOR 2 CHAR OFFICE                   
         TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICES?                              
         BO    *+8                                                              
         LHI   R4,0                LENGTH-1 FOR 1 CHAR OFFICE                   
*                                                                               
         MVC   TEMPOFF,RECOFFC                                                  
*                                                                               
         CLI   LDGTOFFP,LDGOFLT1   OFFICE POSITION IN FILTER                    
         BNL   OFFF25                                                           
         CLI   LDGTOFFP,LDGOPROF   PRODUCTION PROFILE  (PPRGAOF                 
         BE    OFFF25                                                           
*                                                                               
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,X'FF'-LDGOKEY2                                             
         CLI   BYTE1,LDGOKEY                                                    
         BH    OFFFX                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BYTE1       OFFICE POSITION IN ACCOUNT                        
         AHI   RF,2           3-1 (ADD 3 FOR C/U/L MINUS TO GET DISP)           
         AR    RF,R3          DISP TO OFFICE + ACCOUNT REC                      
         EX    R4,*+8         1 OR 2 CHAR OFFICE LENGTH                         
         B     *+10                                                             
         MVC   TEMPOFF(0),0(RF)    PULL IN OFFICE FROM ACCOUNT                  
*                                                                               
OFFF25   TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICES?                              
         BO    OFFF30                                                           
         XC    TEMPOFFL(4),TEMPOFFL                                             
         MVI   TEMPOFFL+1,1                                                     
         MVC   TEMPOFFL+2(L'OOFFPVL),OOFFPVL                                    
         B     OFFF40                                                           
*                                                                               
         USING OFFALD,R1                                                        
OFFF30   L     R1,AOFFBLK          OFFICES ALLOWED UNDER LIMITED ACCESS         
         LA    RE,TEMPOFFL                                                      
         ST    RE,OFFAREQL                                                      
         MVC   OFFAOFFC,OOFFPVL    OFFICE CODE FOR SECURITY TEST                
         MVI   OFFAACT,OFFAREQ     CREATE OFFICE LIST                           
         GOTO1 VOFFAL                                                           
         BE    OFFF40                                                           
         DROP  R1                                                               
*                                                                               
OFFF40   CLI   TEMPOFF,C' '        OFFICE A SPACE ?                             
         BE    OFFF55              . YES, TEST FOR NEGATIVE FILTER              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,TEMPOFFL       NUMBER OF OFFICES IN FILTER LIST             
         BZ    OFFFX               NO OFFICES, SKIP                             
         LA    RF,TEMPOFFL+2       FIRST OFFICE IN LIST                         
*                                                                               
OFFF50   CLC   0(2,RF),SPACES      SPACES?                                      
         BE    OFFF52              . YES, SKIP                                  
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   TEMPOFF(0),0(RF)    IS THIS OFFICE THE ONE WE WANT ?             
         BE    OFFF60              YES, FOUND A MATCH                           
OFFF52   LA    RF,2(,RF)           NO, NEXT OFFICE                              
         BCT   R0,OFFF50           TEST NEXT OFFICE                             
*                                                                               
OFFF55   CLI   OOFFPFI,NEGFILTR    NO MATCH FOUND, NEGATIVE FILTER ?            
         BNE   OFFFERX             . NO, REJECT                                 
         B     OFFFX               . YES, OKAY                                  
*                                                                               
OFFF60   CLI   OOFFPFI,NEGFILTR    FOUND A MATCH, NEGATIVE FILTER ?             
         BE    OFFFERX             . YES, REJECT                                
*                                                                               
OFFFX    J     OKXIT                                                            
OFFFERX  J     ERRXIT                                                           
         DROP  R2                                                               
***********************************************************************         
*        CHECK ACCESS TO LIST SCREEN IS ALLOWED                       *         
***********************************************************************         
ACCESSOK NTR1                                                                   
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ULVRT)   UNIT/LEDGER NOT VALID                   
         OC    TWALEN,TWALEN       NEW SECURITY?                                
         BNZ   ACCOK10                                                          
         CLI   LDGTOFFP,LDGONONE   NO OFFICE                                    
         BE    *+12                                                             
         CLI   LDGTOFFP,LDGOTRAN   OFFICE IN TRANSACTION                        
         BNE   *+12                                                             
         TM    TERMAUTH,X'08'                                                   
         BNO   ACCOKERR                                                         
         B     ACCOK30                                                          
*                                                                               
ACCOK10  MVC   FVMSGNO,=AL2(EASECLOC) SECURITY LOCKOUT                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    ACCOK30                                                          
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    ACCOK30                                                          
         OC    TWALEN,TWALEN       NEW SECURITY?                                
         BZ    ACCOK30                                                          
         CLC   ONEP,BASKEY         CHECK IF ALLOWED TO SEE HOURS RATE           
         BE    ACCOK20                                                          
         CLC   ONER,BASKEY                                                      
         BE    ACCOK20                                                          
         CLC   ONEJ,BASKEY                                                      
         BNE   ACCOK30                                                          
ACCOK20  L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECD),=AL1(1)                           
         BL    ACCOKERR                                                         
*                                                                               
ACCOK30  CLI   LDGTOFFP,LDGOTRAN   OFFICE IN TRANSACTION?                       
         BNE   ACCOKX                                                           
         TM    COMPSTA4,CPYSOFF2   TWO CHARACTER OFFICE?                        
         BO    ACCOKX                                                           
*                                                                               
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OOFFDET,C'Y'        OFFICE DETAILS REQUIRED?                     
         BE    ACCOKERR                                                         
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER?                               
         BNZ   ACCOKERR                                                         
         DROP  RF                                                               
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   ACCOKX                                                           
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    ACCOKERR                                                         
         CLI   TERMACCS,C'$'                                                    
         BE    ACCOKERR                                                         
*                                                                               
ACCOKX   J     OKXIT                                                            
ACCOKERR J     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCOUNT KEY                                           *         
* ON ENTRY R3=A(ACCOUNT KEY)                                          *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
         USING ACTRECD,R3                                                       
FILTKEY  NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
*                                                                               
         CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT PRODUCTION LDG?           
         BNE   FILTK12                                                          
*                                                                               
         OC    OMEDIA,OMEDIA       MEDIA FILTER?                                
         BZ    FILTK12                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LEDGTLVB                                                      
         LA    RF,ACTKACT(RF)      RF=A(MEDIA CODE)                             
*                                                                               
         CLC   OMEDIAVL,0(RF)      MATCH ON FILTER?                             
         BNE   FILTK10                                                          
         CLI   OMEDIAFI,NEGFILTR                                                
         BE    FILTKRJX                                                         
         B     FILTK12                                                          
*                                                                               
FILTK10  CLI   OMEDIAFI,NEGFILTR                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK12  TM    ACTKSTAT,ACTSCLOS   IS ACCOUNT CLOSED?                           
         BO    FILTK14             YES                                          
         CLI   OCLOSED,C'O'        DO WE ONLY WANT CLOSED ACCOUNTS?             
         BE    FILTKRJX                                                         
         B     *+12                                                             
*                                                                               
FILTK14  CLI   OCLOSED,C'N'        DO WE ONLY WANT OPEN ACCOUNTS?               
         BE    FILTKRJX            YES                                          
*                                                                               
         TM    ACTKSTAT,ACTSLOCK   ACCOUNT IS LOCKED?                           
         BO    FILTK16             YES                                          
         CLI   OLOCKED,C'O'        DO WE ONLY WANT LOCKED ACCOUNTS?             
         BE    FILTKRJX            YES                                          
         B     *+12                                                             
*                                                                               
FILTK16  CLI   OLOCKED,C'N'        DO WE ONLY WANT UNLOCKED ACCOUNTS?           
         BE    FILTKRJX            YES                                          
*                                                                               
         CLC   ACCLEV,LDGLEVS      LOW LEVEL ACCOUNT?                           
         BE    FILTKX                                                           
         CLC   ACCLEV,LOWLEV       LOWEST DISPLAY LEVEL?                        
         BE    FILTKX                                                           
         OC    OALL,OALL                                                        
         BNZ   FILTKRJX                                                         
         OC    OCREDIT,OCREDIT                                                  
         BNZ   FILTKRJX                                                         
         OC    ODEBIT,ODEBIT                                                    
         BNZ   FILTKRJX                                                         
         OC    OOUT,OOUT                                                        
         BNZ   FILTKRJX                                                         
         OC    OMEDIA,OMEDIA                                                    
         BNZ   FILTKRJX                                                         
*                                                                               
FILTKX   J     OKXIT                                                            
FILTKRJX J     ERRXIT              REJECT                                       
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT DETAILS                                                 *         
*        ON ENTRY AIO1 = ACCOUNT RECORD                               *         
***********************************************************************         
GETDET   NTR1                                                                   
*                                                                               
         L     R3,AIO1                                                          
         USING ACTRECD,R3                                                       
         L     R5,AOPTVALS         R5=A(OPTION VALUES)                          
         USING OPTVALSD,R5                                                      
         LA    R4,ACTRFST          R4=A(FIRST ELEMENT ON RECORD)                
         XC    ACCACDAT,ACCACDAT                                                
         XC    OACDAT,OACDAT                                                    
         XC    OPEDAT,OPEDAT                                                    
         XC    OOPDAT,OOPDAT                                                    
*                                                                               
GETD10   CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETD70                                                           
         CLI   0(R4),NAMELQ        NAME ELEMENT?                                
         BE    GETD30                                                           
         CLI   0(R4),JOBELQ        JOB ELEMENT?                                 
         BE    GETD35                                                           
         CLI   0(R4),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    GETD40                                                           
         CLI   0(R4),ABLELQ        ACCOUNT BALANCE ELEMENT?                     
         BE    GETD50                                                           
         CLI   0(R4),APOELQ        ACCOUNT PEEL-OFF ELEMENT?                    
         BE    GETD60                                                           
*                                                                               
GETD20   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETD10                                                           
*-----------------------------------------                                      
* NAME ELEMENT                                                                  
*-----------------------------------------                                      
         USING NAMELD,R4                                                        
GETD30   MVC   ACCNAME,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAME(0),NAMEREC  GET NAME                                     
         B     GETD20                                                           
         DROP  R4                                                               
*-----------------------------------------                                      
* JOB ELEMENT                                                                   
*-----------------------------------------                                      
         USING JOBELD,R4                                                        
GETD35   MVC   JOBST,JOBSTA1       SAVE JOB STATUS                              
         CLI   JOBLN,JOBLN2Q                                                    
         BL    GETD20                                                           
         MVC   OOPDAT,JOBODATE     YES, GET THE OPEN DATE                       
         B     GETD20                                                           
         DROP  R4                                                               
*-----------------------------------------                                      
* RECORD STATUS ELEMENT                                                         
*-----------------------------------------                                      
         USING RSTELD,R4                                                        
GETD40   MVC   OACDAT,RSTTDATE     LAST TRANSACTION DATE                        
         MVC   OPEDAT,RSTBDATE     LAST PEEL DATE                               
         MVC   ACCACDAT,RSTTDATE   ACTIVITY DATE                                
         MVC   CLOSEDAT,RSTBDATE   WE NEED LAST PEEL OFF FOR BUCKET DET         
         B     GETD20                                                           
         DROP  R4                                                               
*-----------------------------------------                                      
* ACCOUNT BALANCE ELEMENT                                                       
*-----------------------------------------                                      
         USING ABLELD,R4           ACCOUNT BALANCE ELEMENT                      
GETD50   TM    LISTFLAG,NEEDOFFQ   OFFICE INFO NEEDED?                          
         BO    GETD20              GET INFO LATER                               
         ZAP   BALFRWD,ABLFRWD     GET AMOUNTS OF FIRST BALANCE ELEMENT         
         ZAP   DEBTOT,ABLDR                                                     
         ZAP   CRETOT,ABLCR                                                     
         B     GETD20                                                           
         DROP  R4                                                               
*-----------------------------------------                                      
* ACCOUNT PEEL-OFF ELEMENT                                                      
*-----------------------------------------                                      
         USING APOELD,R4                                                        
GETD60   OC    APOPLDT,APOPLDT     HAS ACCOUNT BEEN PEELED?                     
         BNZ   GETD20                                                           
         XC    CLOSEDAT,CLOSEDAT   IF NOT PEELED, CLEAR CLOSED DATE             
         B     GETD20                                                           
         DROP  R4                                                               
*-----------------------------------------                                      
GETD70   CLC   SVLOWOAC,OACDAT     SAVE LATEST DATE                             
         BH    *+10                                                             
         MVC   SVLOWOAC,OACDAT                                                  
         CLC   SVLOWOOP,OOPDAT     SAVE LATEST DATE                             
         BH    *+10                                                             
         MVC   SVLOWOOP,OOPDAT                                                  
         CLC   SVLOWOPE,OPEDAT     SAVE LATEST DATE                             
         BH    *+10                                                             
         MVC   SVLOWOPE,OPEDAT                                                  
         CLC   ACCLEV,LDGLEVS      LOW LEVEL ACCOUNT?                           
         BNE   GETD90                                                           
*-----------------------------------------                                      
* ADJUST BALANCE FOR OFFICE/MONTH FILTERS                                       
*-----------------------------------------                                      
         TM    LISTFLAG,NEEDOFFQ             OFFICE INFO NEEDED?                
         BZ    GETD75                                                           
         BAS   RE,NOFFDET                    GET OFFICE DETAILS                 
         B     GETD90                        ALL DONE                           
*                                                                               
GETD75   OC    OMOS,OMOS                     IF NO MOS FILTER THEN              
         BZ    GETD80                        USE ONLY BAL ELEMENT               
         BAS   RE,BUCKDET                    ADJUST BALANCE W/ BUCKETS          
         B     GETD90                                                           
*                                                                               
GETD80   L     R2,ATIA             R2=A(TIA)                                    
         LA    R2,BALTAB-TIAD(R2)  R2=A(BALANCE TABLE)                          
         USING BALTABD,R2                                                       
         AP    BALBFWD,BALFRWD     GET TOTAL FOR ACC(S) FOR 1ST ENTRY           
         AP    BALDR,DEBTOT                                                     
         AP    BALCR,CRETOT                                                     
         MVC   BALOFF,NULLOFF      SET NULL OFFICE IE ACCOUNT TOTAL             
         DROP  R2                                                               
*                                                                               
GETD90   J     OKXIT                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        GET NEW OFFICE BALANCE DETAILS (IOAREA 2 IS USED)            *         
*        SEQUENCE OF IOAREA1 IS NOT RESTORED                          *         
* ON ENTRY IOAREA 1 CONTAINING LOW LEVEL ACCOUNT                      *         
*          'OFFDONE' = NUMBER OF FULLY PROCESSED OFFICES IF RE-ENTER  *         
*          'KEYSAVE2'= KEY OF LAST RECORD OF PARTLY PROCESSED OFFICE  *         
*                      OR SPACES IF LAST OFFICE COMPLETELY PROCESSED  *         
* ON EXIT CC EQUAL MEANS ALL OFFICES PROCESSED AND BALANCE ADJUSTED   *         
*            MAX IO MAY HAVE BEEN REACHED.                            *         
*         CC UNEQUAL MEANS ERROR EXIT.                                *         
***********************************************************************         
         USING OFARECD,R3                                                       
         USING OPTVALSD,R5                                                      
NOFFDET  NTR1                                                                   
         L     R5,AOPTVALS                R5=A(OPTION VALUES)                   
*                                                                               
         MVC   KEYSAVE,IOKEY              SAVE KEY OF LOW LEVEL ACCOUNT         
         CLC   KEYSAVE2,SPACES            MAXIO W/ OUTSTANDING RECS             
         BE    NOFF10                                                           
         MVC   IOKEY,KEYSAVE2                                                   
         GOTO1 AIO,IOREAD+IOACCDIR+IO2    RE-READ RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
*-----------------------------------------                                      
* DECIDE WHICH OFFICE LIST WE NEED                                              
*-----------------------------------------                                      
NOFF10   OC    OOFFICVL,OOFFICVL          OFFICE LIST OVERRIDE?                 
         BZ    *+12                       . NO                                  
         LA    R4,OOFFICLS                . YES, USE WANTED OFFICES             
         B     NOFF12                                                           
         L     R4,AOFFBLK                 LIMITED ACCESS                        
         CLI   LDGTOFFP,LDGOTRAN          SECURITY OFFICE IN TRANSACTS?         
         BE    *+8                        . YES                                 
         L     R4,AOFFBLK2                . NO, USE ALL OFFICES                 
         LA    R4,OFFAWORK-OFFALD(,R4)                                          
*-----------------------------------------                                      
* NEXT OFFICE TO PROCESS                                                        
*-----------------------------------------                                      
NOFF12   LA    R4,2(R4)                   BUMP PAST NUMBER OF OFFICES           
         SR    RF,RF                                                            
         ICM   RF,3,OFFDONE               RF=(# OF OFFICES PROCESSED)           
         MHI   RF,L'OFAKOFF                                                     
         LA    R4,0(RF,R4)                R4=A(NEXT OFFICE TO PROCESS)          
         CLC   KEYSAVE2,SPACES            ALREADY HAVE THE NEXT REC?            
         BE    NOFF20                                                           
         MVC   KEYSAVE2,SPACES                                                  
         LA    R3,IOKEY                                                         
         CLC   OFAKEY+TRNKCULC-TRNRECD(L'TRNKCULC),SPACES                       
*        CLI   OFAKOFF+TRNKCACT-TRNRECD,C' '                                    
         BE    NOFF40                     OFFICE ACCOUNT RECORD?                
         B     NOFF92                     PROCESS NEXT CNTRA ACNT REC           
*        B     NOFF170                    OFF TRN REC OR CNTRA NME REC?         
*                                                                               
NOFF20   TM    DISPFLAG,DISIOMAX          MAXIO THEN NO MORE OFF ACCS           
         BO    NOFFX                                                            
*                                                                               
         OC    OOFFICVL,OOFFICVL          OFFICE LIST OVERRIDE?                 
         BZ    *+12                       . NO                                  
         LA    RF,OOFFICLS                . YES, USE WANTED OFFICES             
         B     NOFF22                                                           
         L     RF,AOFFBLK                 LIMITED ACCESS                        
         CLI   LDGTOFFP,LDGOTRAN          SECURITY OFF IN TRANS?                
         BE    *+8                        . YES                                 
         L     RF,AOFFBLK2                . NO, USE ALL OFFICES                 
         LA    RF,OFFAWORK-OFFALD(,RF)                                          
*                                                                               
NOFF22   CLC   OFFDONE,0(RF)              FINISHED?                             
         BL    NOFF30                                                           
*                                                                               
         MVC   IOKEY,KEYSAVE              RESET LOW LEVEL ACCOUNT KEY           
         XC    OFFDONE,OFFDONE            RESET PROCESSED OFF COUNTER           
         B     NOFFX                                                            
*                                                                               
NOFF30   CLC   0(L'TRNKOFF,R4),SPACES                                           
         BNE   NOFF35                                                           
         LA    R4,L'TRNKOFF(R4)           BUMP TO NEXT OFFICE                   
         ICM   RE,3,OFFDONE               RF=(# OF OFFICES PROCESSED)           
         LA    RE,1(RE)                                                         
         STCM  RE,3,OFFDONE                                                     
         B     NOFF20                                                           
*-----------------------------------------                                      
* GET OFFICE ACCOUNT RECORD                                                     
*-----------------------------------------                                      
NOFF35   LA    R3,IOKEY                   R3=A(IO KEY)                          
         MVC   OFAKOFF,0(R4)              READ FOR RELEVANT OFF ACC REC         
         CLC   OFAKOFF,SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    NOFF40                                                           
         TM    IOERR,IOMAX                TOO MANY IOS?                         
         BO    NOFF200                                                          
         TM    IOERR,IOERNF               RECORD NOT FOUND?                     
         BO    NOFF190                                                          
         DC    H'0'                                                             
NOFF40   GOTO1 AIO,IOGET+IOACCMST+IO2     GET OFFICE ACCOUNT RECORD             
         BE    NOFF50                                                           
         TM    IOERR,IOMAX                TOO MANY IOS?                         
         BO    NOFF200                                                          
         DC    H'0'                                                             
*                                                                               
NOFF50   L     R3,AIO2                    R3=A(OFFICE ACCOUNT RECORD)           
         LA    RF,OFARFST                 RF=A(FIRST ELEMENT)                   
         USING ABLELD,RF                                                        
NOFF60   CLI   ABLEL,EOR                  END OF RECORD?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ABLEL,ABLELQ               ACCOUNT BALANCE ELEMENT?              
         BE    NOFF70                                                           
         SR    R0,R0                      BUMP TO NEXT ELEMENT                  
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     NOFF60                                                           
NOFF70   ZAP   BALFRWD,ABLFRWD            GET BALANCE FOR OFFICE                
         ZAP   DEBTOT,ABLDR                                                     
         ZAP   CRETOT,ABLCR                                                     
         DROP  RF                                                               
*-----------------------------------------                                      
* ADJUST OFFICE BALANCES                                                        
*-----------------------------------------                                      
         OC    OMOS,OMOS                  IF MOS FILTER MAY NEED TRANS          
         BZ    NOFF160                                                          
         TM    LISTFLAG,ROFFBUKQ          ADJUST BAL W/ OFF/CNTRA?              
         BO    NOFF90                     . NO                                  
         BAS   RE,ISBALOK                 DOES BAL REQUIRE ADJUSTING?           
         BE    NOFF160                                                          
*-----------------------------------------                                      
* ADJUST USING TRANSACTIONS                                                     
*-----------------------------------------                                      
         USING TRNRECD,R3                                                       
NOFF75   GOTO1 AIO,IOSEQ+IOACCDIR+IO2                                           
         BE    NOFF80                     NO ERROR                              
         TM    IOERR,IOMAX                TOO MANY IOS?                         
         BO    *+6                                                              
         DC    H'0'                       DIE IF ERROR NOT MAX IO               
         OI    DISPFLAG,DISIOMAX                                                
         B     NOFF200                                                          
*                                                                               
NOFF80   LA    R3,IOKEY                                                         
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV   SAME OFFICE?                  
         BNE   NOFF160                            . NO                          
         CLC   TRNKDATE,SPACES            TRANSACTION RECORD?                   
         BNH   NOFF75                     . NO, KEEP READING                    
*                                                                               
         TM    TRNKSTA,TRNSDRFT           IF THIS A DRAFT?                      
         BO    NOFF75                     . YES                                 
         TM    TRNKSTA2,TRNSPEEL          TRANSACTION PEELED?                   
         BO    NOFF75                     . YES                                 
*                                                                               
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BE    NOFF75                                                           
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+14                       NO ERRORS                             
         TM    IOERR,IOMAX                TOO MANY IOS?                         
         BO    NOFF200                                                          
         DC    H'0'                       DIE IF ERROR NOT MAX IO               
*                                                                               
         L     R3,AIO2                    R3=A(TRANSACTION RECORD)              
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         CP    TRNAMNT,=P'0'                                                    
         BE    NOFF75                     SKIP IF ZERO AMOUNT                   
         LA    RE,DEBTOT                                                        
         TM    TRNSTAT,TRNSDR             SUBTRACT FROM CURRENT BALANCE         
         BO    *+8                                                              
         LA    RE,CRETOT                                                        
         SP    0(L'CRETOT,RE),TRNAMNT                                           
*                                                                               
         CLI   OMOSFI,NEGFILTR                                                  
         BNE   *+14                                                             
         ZAP   BALFRWD,=P'0'              NO BALANCE BROUGHT FORWARD            
         B     NOFF75                                                           
*                                                                               
         CLC   TRNRSMOS,OMOSST                                                  
         BNL   NOFF75                                                           
*                                                                               
         CLC   TRNRSMOS,COMPFIN           LOWER THAN COMP FISCAL YEAR?          
         BL    NOFF75                                                           
*                                                                               
         TM    TRNSTAT,TRNSDR             DEBIT?                                
         BO    *+14                       . YES                                 
         SP    BALFRWD,TRNAMNT            ADJUST BBF                            
         B     NOFF75                                                           
         AP    BALFRWD,TRNAMNT                                                  
         B     NOFF75                                                           
         DROP  RF                                                               
*-----------------------------------------                                      
* ADJUST USING OFFICE-CONTRA BUCKETS                                            
*-----------------------------------------                                      
NOFF90   AP    BALFRWD,DEBTOT             PREPARE BBF FOR ADJUSTMENT            
         SP    BALFRWD,CRETOT                                                   
         ZAP   DEBTOT,=P'0'                                                     
         ZAP   CRETOT,=P'0'                                                     
*                                                                               
         USING CACRECD,R3                                                       
NOFF92   GOTO1 AIO,IOSEQ+IOACCDIR+IO2                                           
         BE    NOFF96                     NO ERROR                              
         TM    IOERR,IOMAX                TOO MANY IOS?                         
         BO    *+6                                                              
         DC    H'0'                       DIE IF ERROR NOT MAX IO               
NOFF94   OI    DISPFLAG,DISIOMAX                                                
         B     NOFF200                                                          
NOFF96   LA    R3,IOKEY                                                         
         CLC   CACKCULA(CACKCULC-CACKCULA),IOKEYSAV  SAME ACC/OFFICE            
         BNE   NOFF160                                                          
         CLI   CACKBTYP,C' '                                                    
         BNE   NOFF92                                                           
         CLC   CHDKNULL-CHDKEY(L'CHDKNULL,R3),SPACES KEY SPACE FILLED?          
         BNE   NOFF92                                                           
         CLC   CACKSPAC,SPACES            TRANSACTION?                          
         BH    NOFF92                     . YES, GET NEXT                       
*                                                                               
NOFF100  GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+14                                                             
         TM    IOERR,IOMAX                                                      
         BO    NOFF94                                                           
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                    R3=A(RECORD)                          
         LA    R3,CACRFST                                                       
NOFF110  CLI   0(R3),EOR                  END OF RECORD?                        
         BE    NOFF92                                                           
         CLI   0(R3),BUKELQ               BUCKET ELEMENT?                       
         BE    NOFF120                                                          
         CLI   0(R3),PBKELQ               PRIOR BUCKET ELEMENT?                 
         BE    NOFF140                                                          
NOFF111  SR    RF,RF                      BUMP TO NEXT ELEMENT                  
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     NOFF110                                                          
*-----------------------------------------                                      
* BUCKET ELEMENT X'45'                                                          
*-----------------------------------------                                      
         USING BUKELD,R3                                                        
NOFF120  GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    NOFF124                    WITHIN MONTH RANGE                    
         CLI   OMOSFI,NEGFILTR            WANT EVERYTHING BUT?                  
         BE    NOFF126                    REMOVE                                
         CLC   BUKMOS,OMOSEN              MONTH PAST WANTED RANGE?              
         BH    NOFF126                    REMOVE                                
         CLC   BUKMOS,COMPFIN             LOWER THAN COMP FISCAL YEAR?          
         BL    NOFF126                                                          
         B     NOFF111                                                          
NOFF124  AP    DEBTOT,BUKDR               ADD TO CURRENT BALANCE AND            
         AP    CRETOT,BUKCR                                                     
NOFF126  SP    BALFRWD,BUKDR              REMOVE FROM BBF                       
         AP    BALFRWD,BUKCR                                                    
         B     NOFF111                                                          
*-----------------------------------------                                      
* PREVIOUS BUCKET ELEMENT X'55'                                                 
*-----------------------------------------                                      
         USING PBKELD,R3                                                        
NOFF140  GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    NOFF144                    WITHIN MONTH RANGE                    
         CLI   OMOSFI,NEGFILTR            WANT EVERYTHING BUT?                  
         BE    NOFF146                    REMOVE                                
         CLC   PBKHI,OMOSEN               MONTH PAST WANTED RANGE?              
         BH    NOFF146                    REMOVE                                
         CLC   PBKHI,COMPFIN              LOWER THAN COMP FISCAL YEAR?          
         BL    NOFF146                                                          
         B     NOFF111                                                          
NOFF144  AP    DEBTOT,PBKDR               ADD TO CURRENT BALANCE AND            
         AP    CRETOT,PBKCR                                                     
NOFF146  SP    BALFRWD,PBKDR              REMOVE FROM BBF                       
         AP    BALFRWD,PBKCR                                                    
         B     NOFF111                                                          
*-----------------------------------------                                      
* CORRECT BALTAB ENTRY WITH BALANCES                                            
*-----------------------------------------                                      
NOFF160  CLI   OBFWD,C'N'                 DON'T SHOW BALANCE FORWARD?           
         BNE   *+10                                                             
         ZAP   BALFRWD,=P'0'                                                    
*                                                                               
NOFF170  L     R2,ATIA                    R2=A(TIA)                             
         LA    R2,BALTAB-TIAD(R2)         R2=A(BALANCE TABLE)                   
         USING BALTABD,R2                                                       
         AP    BALBFWD,BALFRWD            GET TOTAL FOR ACCOUNT                 
         AP    BALDR,DEBTOT               FOR FIRST ENTRY                       
         AP    BALCR,CRETOT                                                     
         MVC   BALOFF,NULLOFF             SET NULL OFFICE IN ACC TOTAL          
*                                                                               
         CLI   OOFFDET,C'Y'               DO WE REQUIRE OFFICE DETAIL?          
         BNE   NOFF190                                                          
NOFF175  CLI   BALOFF,EOT                 END OF BALANCE TABLE?                 
         BE    NOFF180                                                          
         CLC   BALOFF,0(R4)               MATCH ON OFFICE?                      
         BE    NOFF185                                                          
         LA    R2,BALLNQ(R2)              BUMP UP BALANCE TABLE                 
         B     NOFF175                                                          
*                                                                               
NOFF180  ZAP   BALBFWD,BALFRWD            1ST ENTRY FOR THIS OFF TOTAL          
         ZAP   BALDR,DEBTOT                                                     
         ZAP   BALCR,CRETOT                                                     
         MVC   BALOFF,0(R4)               GET OFFICE                            
         B     NOFF190                                                          
NOFF185  AP    BALBFWD,BALFRWD            2ND OR LATER NTRY FOR OFF TOT         
         AP    BALDR,DEBTOT                                                     
         AP    BALCR,CRETOT                                                     
*                                                                               
NOFF190  SR    RF,RF                                                            
         ICM   RF,3,OFFDONE               RF=(# OF OFFICES PROCESSED)           
         LA    RF,1(RF)                                                         
         STCM  RF,3,OFFDONE                                                     
         LA    R4,L'OFAKOFF(R4)           R4=A(NEXT OFFICE TO PROCESS)          
         LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   OFAKCULA-OFAKEY(L'OFAKCULA,R3),KEYSAVE                           
         B     NOFF20                                                           
*                                                                               
NOFF200  TM    IOERR,IOERNF               RECORD NOT FOUND?                     
         BO    *+10                                                             
         MVC   KEYSAVE2,IOKEY             SAVE KEY                              
         MVC   IOKEY,KEYSAVE                                                    
         OI    DISPFLAG,DISIOMAX          MAX IO'S REACHED                      
         B     NOFFERX                                                          
*                                                                               
NOFFX    J     OKXIT                                                            
NOFFERX  J     ERRXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF IT IS NECESSARY TO ADJUST THE OFFICE BALANCE BY READING    *         
* TRANSACTIONS                                                        *         
*        ON ENTRY AIO2 = OFFICE ACCOUNT RECORD                        *         
*        ON EXIT CC EQUAL MEANS BALANCE IS OK                         *         
*                CC UNEQUAL MEANS BALANCE NEEDS ADJUSTING             *         
***********************************************************************         
ISBALOK  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS                                                      
         USING OPTVALSD,R2                                                      
         L     R3,AIO2                                                          
         USING OFARECD,R3                                                       
         GOTO1 ADCOMP,DMCB,(L'OFARHMOS,OFARHMOS),OMOSST,OMOSEN,OMOSFI           
         BH    ISBALNO                    HIGH POSTING MOA>END MOA FILT         
         CLC   COMPFIN,OFARLMOS           FISCAL START>LOW POSTING MOA?         
         BH    ISBALNO                    . YES                                 
         CLI   OMOSFI,NEGFILTR            NEGATIVE FILTER?                      
         BE    ISBAL10                    . YES                                 
         CLC   OFARCMOS,OMOSST            STRT DTE FLTR=LST PEELED DTE?         
         BNE   ISBALNO                    . NO                                  
         B     ISBALYES                   . YES                                 
*                                                                               
ISBAL10  MVC   WORK,OMOSEN                                                      
         MVI   WORK+2,X'15'                                                     
         GOTO1 VDATCON,DMCB,(1,WORK),(0,WORK+6)                                 
         LA    RF,30                                                            
         GOTO1 VADDAY,DMCB,WORK+6,WORK,(RF)                                     
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK+6)                                 
         CLC   OFARCMOS,WORK+6            STRT DTE FLTR=LST PEELED DTE?         
         BNE   ISBALNO                    . NO                                  
         B     ISBALYES                                                         
*                                                                               
         OC    OFARCMOS,OFARCMOS          OFF LAST CLOSED MOS PRESENT?          
         BNZ   ISBALNO                    . YES                                 
         B     ISBALYES                   . NO                                  
*                                                                               
ISBALYES J     OKXIT                                                            
ISBALNO  J     ERRXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET BUCKET DETAILS (IOAREA 2 IS USED)                               *         
*        SEQUENCE OF IOAREA1 IS NOT RESTORED                          *         
*        ON ENTRY R3=A(LOW LEVEL ACCOUNT)                             *         
*             'KEYSAVE2'= KEY OF LAST RECORD READ IF REENTRY          *         
*        ON EXIT CC EQUAL MEANS ALL BUCKETS PROCESSED                 *         
*                CC UNEQUAL MEANS MAXIO ERROR                         *         
***********************************************************************         
BUCKDET  NTR1                                                                   
*                                                                               
         USING CACRECD,R3                                                       
         L     R5,AOPTVALS         R5=A(OPTION VALUES)                          
         USING OPTVALSD,R5                                                      
         MVC   KEYSAVE,IOKEY       SAVE LOW LEVEL ACCOUNT KEY                   
         CLC   KEYSAVE2,SPACES     IS THIS FIRST TIME FOR ACCOUNT?              
         BE    BUCK10                                                           
         MVC   IOKEY,KEYSAVE2      RE-READ LAST RECORD                          
         MVC   KEYSAVE2,SPACES                                                  
         B     BUCK20                                                           
*                                                                               
BUCK10   AP    BALFRWD,DEBTOT                                                   
         SP    BALFRWD,CRETOT                                                   
         ZAP   DEBTOT,=P'0'                                                     
         ZAP   CRETOT,=P'0'                                                     
*                                                                               
BUCK15   LA    R3,IOKEY                                                         
         MVI   CACKSPAC,X'FF'                                                   
BUCK20   GOTO1 AIO,IOHIGH+IOACCDIR+IO2 GET CONTRA RECORD                        
         BE    BUCK30                                                           
BUCK22   TM    IOERR,IOMAX         MAX IO?                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         CLC   CACKCULA,IOKEYSAV   SAME ACCOUNT?                                
         BE    BUCK80                                                           
         B     BUCK90                                                           
*                                                                               
BUCK30   CLC   CACKCULA,IOKEYSAV   SAME ACCOUNT?                                
         BNE   BUCK90                                                           
         CLC   SPROUL,BASKEY       CHECK PRODCTION?                             
         BE    *+14                NO                                           
         CLC   CHDKOFF-CHDKEY(L'CHDKOFF,R3),SPACES   NO OFFICE?                 
         BNE   BUCK40                                                           
         CLI   CACKBTYP,C' '                                                    
         BNE   BUCK40                                                           
         CLC   CHDKNULL-CHDKEY(L'CHDKNULL,R3),SPACES KEY SPACE FILLED?          
         BE    BUCK45                                                           
BUCK40   GOTO1 AIO,IOSEQ+IOACCDIR+IO2 NAME CONTRA SO GET NEXT RECORD            
         BE    BUCK30                                                           
         B     BUCK22                                                           
*                                                                               
BUCK45   GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+14                                                             
         TM    IOERR,IOMAX                                                      
         BO    BUCK80                                                           
         DC    H'0'                                                             
         L     R3,AIO2             R3=A(RECORD)                                 
         LA    R3,CACRFST                                                       
BUCK50   CLI   0(R3),EOR           END OF RECORD?                               
         BE    BUCK15                                                           
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    BUCK60                                                           
         CLI   0(R3),PBKELQ        PRIOR BUCKET ELEMENT?                        
         BE    BUCK70                                                           
BUCK55   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     BUCK50                                                           
*-----------------------------------------                                      
* BUCKET ELEMENT X'45'                                                          
*-----------------------------------------                                      
         USING BUKELD,R3                                                        
BUCK60   GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    BUCK64               WITHIN MONTH RANGE                          
         CLI   OMOSFI,NEGFILTR     WANT EVERYTHING BUT?                         
         BE    BUCK66               REMOVE                                      
         CLC   BUKMOS,OMOSEN       MONTH PAST WANTED RANGE?                     
         BH    BUCK66               REMOVE                                      
         CLC   BUKMOS,COMPFIN      LOWER THAN COMPANY FISCAL YEAR?              
         BL    BUCK66                                                           
         B     BUCK55                                                           
BUCK64   AP    DEBTOT,BUKDR        ADD TO CURRENT BALANCE AND                   
         AP    CRETOT,BUKCR                                                     
BUCK66   SP    BALFRWD,BUKDR       REMOVE FROM BBF                              
         AP    BALFRWD,BUKCR                                                    
         B     BUCK55                                                           
*-----------------------------------------                                      
* PREVIOUS BUCKET ELEMENT X'55'                                                 
*-----------------------------------------                                      
         USING PBKELD,R3                                                        
BUCK70   GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    BUCK74               WITHIN MONTH RANGE                          
         CLI   OMOSFI,NEGFILTR     WANT EVERYTHING BUT?                         
         BE    BUCK76               REMOVE                                      
         CLC   PBKHI,OMOSEN        MONTH PAST WANTED RANGE?                     
         BH    BUCK76               REMOVE                                      
         CLC   PBKHI,COMPFIN       LOWER THAN COMPANY FISCAL YEAR?              
         BL    BUCK76                                                           
         B     BUCK55                                                           
BUCK74   AP    DEBTOT,PBKDR        ADD TO CURRENT BALANCE AND                   
         AP    CRETOT,PBKCR                                                     
BUCK76   SP    BALFRWD,PBKDR       REMOVE FROM BBF                              
         AP    BALFRWD,PBKCR                                                    
         B     BUCK55                                                           
*-----------------------------------------                                      
BUCK80   MVC   KEYSAVE2,IOKEY      SAVE KEY                                     
         MVC   IOKEY,KEYSAVE                                                    
         OI    DISPFLAG,DISIOMAX   MAX IO'S REACHED                             
         B     BUCKERRX                                                         
*                                                                               
BUCK90   L     R2,ATIA             R2=A(TIA)                                    
         LA    R2,BALTAB-TIAD(R2)  R2=A(BALANCE TABLE)                          
         USING BALTABD,R2                                                       
         AP    BALBFWD,BALFRWD                                                  
         AP    BALDR,DEBTOT                                                     
         AP    BALCR,CRETOT                                                     
         MVC   BALOFF,NULLOFF                                                   
*                                                                               
         CLI   OBFWD,C'N'          DON'T SHOW BALANCE FORWARD?                  
         BNE   *+10                                                             
         ZAP   BALBFWD,=P'0'                                                    
*                                                                               
         MVC   IOKEY,KEYSAVE       RESTORE LOW LEVEL ACCOUNT KEY                
         ZAP   BALFRWD,=P'0'                                                    
         ZAP   DEBTOT,=P'0'                                                     
         ZAP   CRETOT,=P'0'                                                     
*                                                                               
BUCKX    J     OKXIT                                                            
BUCKERRX J     ERRXIT                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TSAR RECORDS AND DISPLAY IF REQUIRED                   *                
* ON ENTRY IF FIRST BALTAB OFFICE IS BINARY ZEROS THEN NOT LOWEST     *         
*          DISPLAY LEVEL                                              *         
* ON EXIT  'OUTSTNDQ' SET ON IF OUSTANDING 'BALTAB' ENTRIES AND       *         
*          'OFFDONE' SET TO NUMBER OF PROCESSED 'BALTAB' ENTRIES      *         
***********************************************************************         
BLDDIS   NTR1                                                                   
*                                                                               
         TM    LISTFLAG,OUTSTNDQ   RE-ENTER OR FIRST FOR ACCOUNT?               
         BO    BLDD05                                                           
*                                                                               
         BAS   RE,BLDHILEV         BUILD TSAR RECS FOR HIGHER LEVELS            
         BNE   BLDDERRX                                                         
*                                                                               
         BAS   RE,FILTAMT          APPLY AMOUNT FILTERS                         
         BNE   BLDD40              NO ENTRIES LEFT IN 'BALTAB' TABLE?           
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OACT,OACT          FILTERING ON ACTIVITY DATE?                   
         BZ    BLDD05                                                           
         CLC   SVLOWACC,SPACES     SAVED LOWEST DISPLAY LEV DETAILS             
         BE    BLDD01                                                           
         GOTO1 ADCOMP,DMCB,(L'SVLOWOAC,SVLOWOAC),OACTST,OACTEN,OACTFI           
         BNE   BLDD40                                                           
         B     BLDD05                                                           
*                                                                               
BLDD01   GOTO1 ADCOMP,DMCB,(L'ACCACDAT,ACCACDAT),OACTST,OACTEN,OACTFI           
         BNE   BLDD40                                                           
         DROP  RF                                                               
BLDD05   NI    LISTFLAG,X'FF'-OUTSTNDQ                                          
         SR    R1,R1                                                            
         ICM   R1,3,OFFDONE                                                     
         MH    R1,=Y(BALLNQ)       DISPLACEMENT FOR NEXT ENTRY                  
         L     R3,ATIA                                                          
         LA    R3,BALTAB-TIAD(R1,R3) R3=A(NEXT TABLE ENTRY)                     
         USING BALTABD,R3                                                       
BLDD10   CLC   BALOFF,REJOFF       HAS ENTRY BEEN REJECTED?                     
         BE    BLDD30                                                           
*                                                                               
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         BNE   BLDDERRX                                                         
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   BLDD20              YES                                          
         SR    RF,RF               INCREMENT CURRENT TSAR REC NUMBER            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     BLDD30                                                           
*                                                                               
BLDD20   TM    INTFLAG,SCRFULLQ    MAY BE FULL FROM HIGHER LEVELS               
         BO    BLDD30                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                STILL ROOM ON SCREEN?                        
         OI    INTFLAG,SCRFULLQ                                                 
         B     BLDD30                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BLDD30   LA    R3,BALLNQ(R3)       BUMP TO NEXT ENTRY                           
         CLI   BALOFF,EOT          END OF TABLE?                                
         BE    BLDD40                                                           
         OI    LISTFLAG,OUTSTNDQ   SET OUTSTANDING ENTRIES FLAG ON              
         SR    R1,R1                                                            
         ICM   R1,3,OFFDONE                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,OFFDONE        UPDATE ENTRY COUNT                           
         B     BLDD50                                                           
*                                                                               
BLDD40   LA    RF,HILEV                                                         
         XC    0(SVHLNQ*SVHMAXQ,RF),0(RF) INIT HIGHER LEV ACC AREA              
         L     R3,ATIA             CLEAR BALANCE TABLE                          
         LA    R3,BALTAB-TIAD(R3)                                               
         LR    R0,R3                                                            
         LH    R1,=Y(MAXOFFQ+1*BALLNQ)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   BALDR,=P'0'                                                      
         ZAP   BALCR,=P'0'                                                      
         ZAP   BALBFWD,=P'0'                                                    
         MVI   JOBST,0             INIT JOB STATUS                              
*                                                                               
BLDD50   TM    DISPFLAG,DISIOMAX   MAX IO?                                      
         BO    BLDDERRX                                                         
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    BLDDERRX                                                         
         TM    LISTFLAG,OUTSTNDQ   OUTSTANDING 'BALTAB' ENTRIES?                
         BZ    *+12                                                             
         NI    LISTFLAG,X'FF'-OUTSTNDQ                                          
         B     BLDD10                                                           
         XC    OFFDONE,OFFDONE                                                  
*                                                                               
BLDDX    J     OKXIT                                                            
BLDDERRX J     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*        FILTER AMOUNTS ON ACCOUNTS USING 'BALTAB' TABLE              *         
*        IF DISPLAYING OFFICE DETAIL MAIN ACCOUNT MUST ALWAYS BE      *         
*        KEPT IF 1 OR MORE OFFICE ACCOUNTS PASS THE FILTERING         *         
* ON EXIT  CC IS SET TO EQUAL IF TABLE CONTAINS AT LEAST ON GOOD NTRY *         
*          CC IS SET TO UNEQUAL IF ALL ENTRYS REJECTED                *         
***********************************************************************         
FILTAMT  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         NI    LISTFLAG,X'FF'-KEEPQ                                             
         L     R3,ATIA             R3=A(TIA)                                    
         LA    R3,BALTAB-TIAD(R3)  R3=A(BALANCE TABLE)                          
         LR    R0,R3               SAVE A(START OF TABLE) IN R0                 
         USING BALTABD,R3                                                       
         CLI   BALOFF,EOT          NO AMOUNTS FOR THIS LEVEL?                   
         BNE   *+12                                                             
         CLI   OOUT,C'O'           ONLY OUTSTANDING BALANCES?                   
         BE    FILTARJX                                                         
         LR    R4,R3               R4=A(TOTAL FOR ACCOUNT)                      
         CLI   BALLNQ(R3),EOT      LAST ENTRY IN TABLE?                         
         BE    *+12                                                             
         LA    R3,BALLNQ(R3)                                                    
         B     *-12                                                             
*                                                                               
FILTA10  CLC   BALBFWD,SPACES                                                   
         BE    FILTA60                                                          
         CLI   OALL,C'Z'           ONLY ACCS BALFWD=ZERO & CURBAL=ZERO          
         BNE   FILTA20                                                          
         CP    BALBFWD,=P'0'       BALANCE BROUGHT FORWARD IS ZERO?             
         BNE   FILTA40                                                          
         CP    BALDR,=P'0'         CURRENT DEBITS ARE ZERO?                     
         BNE   FILTA40                                                          
         CP    BALCR,=P'0'         CURRENT CREDITS ARE ZERO?                    
         BNE   FILTA40                                                          
*                                                                               
FILTA20  CLI   OCREDIT,C'Z'        ONLY ACCS WITH NO CURRENT CREDITS            
         BNE   *+14                                                             
         CP    BALCR,=P'0'                                                      
         BNE   FILTA40                                                          
*                                                                               
         CLI   ODEBIT,C'Z'         ONLY ACCS WITH NO CURRENT DEBITS             
         BNE   *+14                                                             
         CP    BALDR,=P'0'                                                      
         BNE   FILTA40                                                          
*                                                                               
         CLI   OOUT,C'O'           ONLY ACCS WITH OUTSTANDING CURR BAL          
         BNE   FILTA30                                                          
         ZAP   BALANCE,BALBFWD                                                  
         AP    BALANCE,BALDR                                                    
         SP    BALANCE,BALCR                                                    
         CP    BALANCE,=P'0'                                                    
         BE    FILTA40                                                          
*                                                                               
FILTA30  CLI   BALOFF,EOT          ZERO ACCOUNT?                                
         BE    FILTAX                                                           
         CLC   BALOFF,NULLOFF      IF NULL OFFICE WE HAVE FINISHED              
         BE    FILTAX                                                           
         OI    LISTFLAG,KEEPQ      OTHERWISE 'AT LEAST 1' FLAG ON               
         B     FILTA60                                                          
FILTA40  CLC   BALOFF,NULLOFF      NULL OFFICE?                                 
         BNE   FILTA50                                                          
         TM    LISTFLAG,KEEPQ      KEEP FLAG ON?                                
         BO    FILTAX                                                           
         B     FILTARJX                                                         
FILTA50  SP    BALBFWD-BALTABD(L'BALBFWD,R4),BALBFWD                            
         SP    BALDR-BALTABD(L'BALDR,R4),BALDR                                  
         SP    BALCR-BALTABD(L'BALCR,R4),BALCR                                  
         MVC   BALOFF,REJOFF       OFFICE REJECTED                              
*                                                                               
FILTA60  SH    R3,=Y(BALLNQ)       BUMP BACK UP TABLE                           
         CR    R3,R0                                                            
         BNL   FILTA10                                                          
*                                                                               
FILTAX   J     OKXIT                                                            
FILTARJX J     ERRXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORDS FOR HIGHER LEVEL ACCOUNTS                 *         
* ON EXIT  CC EQUAL-     TSAR RECORDS ADDED OK                        *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
BLDHILEV NTR1                                                                   
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         LA    R4,TSARDATA         R4=A(TSAR DATA)                              
         USING TSARDATD,R4                                                      
         LH    RF,=Y(TSDLEN1Q)     LENGTH OF REC FOR HIGHER LEVS                
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         ZAP   TSDDR,=P'0'         DEBITS                                       
         ZAP   TSDCR,=P'0'         CREDITS                                      
         ZAP   TSDBLFWD,=P'0'      BALANCE BROUGHT FORWARD                      
         ZAP   TSDBFWCF,BALBFCF    BALANCE CARRIED FORWARD                      
         ZAP   TSDDRCF,DEBCF       DEBITS CARRIED FORWARD                       
         ZAP   TSDCRCF,CREDCF      CREDITS CARRIED FORWARD                      
*                                                                               
         LA    R3,HILEV                                                         
         USING SVHIGHLD,R3                                                      
         LA    R0,SVHMAXQ                                                       
BLDHI10  OC    0(SVHLNQ,R3),0(R3)  ENTRY FOUND?                                 
         BZ    BLDHI20                                                          
         MVC   TSARKYNO,TSCURRNO   RECORD NUMBER                                
         MVC   TSDCODE,SVHCODE     ACCOUNT CODE                                 
         MVC   TSDNAME,SVHNAME     ACCOUNT NAME                                 
         MVC   TSDACDAT,SVHACDAT   ACTIVITY DATE                                
         MVC   TSDPEDAT,SVHPEDAT   PEELED DATE                                  
         MVC   TSDOPDAT,SVHOPDAT   OPENED DATE                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    *+12                 NO                                          
         BAS   RE,FGRMTSAR          YES, FORMAT FOR GRIDS                       
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
         TM    INTFLAG,SCRFULLQ    ATTEMPT TO DISPLAY?                          
         BO    BLDHI15              NO                                          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                SCREEN FULL?                                 
         OI    INTFLAG,SCRFULLQ                                                 
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
BLDHI15  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BLDHI20  LA    R3,SVHLNQ(R3)                                                    
         BCT   R0,BLDHI10                                                       
         DROP  R2,R3,R4                                                         
*                                                                               
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDHIERX                                                         
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    BLDHIERX                                                         
*                                                                               
BLDHIX   J     OKXIT                                                            
BLDHIERX J     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(BALTAB ENTRY)                                                   
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         USING BALTABD,R3                                                       
BLDTSDAT NTR1                                                                   
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         LA    R4,TSARDATA         R4=A(TSAR DATA)                              
         USING TSARDATD,R4                                                      
         LHI   RF,TSDLEN1Q         LENGTH OF REC FOR HIGHER LEVS                
         CLI   BALOFF,EOT          BALTAB EMPTY MEANS HIGH LEVEL                
         BE    BLDT20                                                           
         CLC   BALOFF,UNKNOWN      UNKNOWN BALANCES (MAINA ACCOUNT)             
         BE    BLDT10                                                           
         CLC   BALBFWD,SPACES      UNKNOWN BALANCES (OFFICE ACCOUNT)            
         BE    BLDT10                                                           
         CP    BALDR,=P'0'         IF ZERO BAL TREAT AS HIGH LEVEL              
         BNE   BLDT10                                                           
         CP    BALCR,=P'0'                                                      
         BNE   BLDT10                                                           
         CP    BALBFWD,=P'0'                                                    
         BE    BLDT20                                                           
BLDT10   LH    RF,=Y(TSDLEN2Q)     LENGTH OF REC FOR AMOUNTS                    
         MVI   TSDTYPE,TSDDETQ     AMOUNT DETAILS TYPE                          
         CLC   BALOFF,NULLOFF      ACCOUNT TOTAL?                               
         BE    BLDT20                                                           
         MVI   TSDTYPE,TSDUNKQ     UNKNOWN BALANCES                             
         CLC   BALOFF,UNKNOWN                                                   
         BE    BLDT20                                                           
         CLC   BALBFWD,SPACES                                                   
         BE    BLDT20                                                           
         MVI   TSDTYPE,TSDOFFQ     AMOUNT DETAILS TYPE                          
BLDT20   AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   RECORD NUMBER                                
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
*                                                                               
         CLI   BALOFF,EOT          END OF TABLE                                 
         BE    BLDT25                                                           
         CLC   BALOFF,UNKNOWN      UNKNOWN BALANCES?                            
         BE    BLDT25                                                           
         CLC   BALOFF,NULLOFF      NULL OFFICE (ACCOUNT TOTAL)                  
         BNE   BLDT40                                                           
*                                                                               
BLDT25   CLC   SVLOWACC,SPACES     SAVED LOWEST DISPLAY LEV DETAILS             
         BE    BLDT30                                                           
         MVC   TSDNAME,SAVEACCN    LOW ACCOUNT CODE                             
         MVC   TSDACDAT,SVLOWOAC   LOW ACCOUNT ACTIVITY DATE                    
         MVC   TSDPEDAT,SVLOWOPE   LOW ACCOUNT PEELED DATE                      
         MVC   TSDOPDAT,SVLOWOOP   LOW ACCOUNT OPENED DATE                      
         MVC   TSDCODE,SVLOWACC    ACCOUNT CODE                                 
         B     BLDT50                                                           
*                                                                               
         USING ACTRECD,RF                                                       
BLDT30   L     RF,AIO1                                                          
         MVC   TSDCODE,ACTKACT     ACCOUNT CODE                                 
         MVC   TSDNAME,ACCNAME     ACCOUNT NAME                                 
         MVC   TSDJOBST,JOBST      SET JOB STATUS                               
         MVC   TSDACDAT,OACDAT     ACTIVITY DATE                                
         MVC   TSDPEDAT,OPEDAT     PEELED DATE                                  
         MVC   TSDOPDAT,OOPDAT     OPENEDDATE                                   
         B     BLDT50                                                           
         DROP  RF                                                               
*                                                                               
BLDT40   MVC   TSDNAME(L'MX@OFF),MX@OFF      OFFICE KEYWORD                     
         LA    RF,TSDNAME+L'MX@OFF-1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'BALOFF,RF),BALOFF OFFICE CODE                                
         XC    TSDACDAT,SVLOWOAC   NO DATE RELEVANT                             
         XC    TSDPEDAT,SVLOWOPE   PEELED DATE                                  
         XC    TSDOPDAT,SVLOWOOP   OPENED DATE                                  
*                                                                               
BLDT50   CLI   BALOFF,EOT                                                       
         BE    BLDT60                                                           
         CLC   BALOFF,UNKNOWN                                                   
         BE    BLDT60                                                           
         CLC   BALBFWD,SPACES                                                   
         BE    BLDT60                                                           
         ZAP   TSDDR,BALDR         DEBITS                                       
         ZAP   TSDCR,BALCR         CREDITS                                      
         ZAP   TSDBLFWD,BALBFWD    BALANCE BROUGHT FORWARD                      
*                                                                               
         USING OPTVALSD,RF                                                      
BLDT60   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         MVI   TSDBSTAR,C' '                                                    
*                                                                               
         TM    LISTFLAG,NEEDOFFQ                                                
         BZ    BLDT65                                                           
         TM    LISTFLAG,ROFFBUKQ   READ OFFICE/CONTRA BUCKETS                   
         BO    BLDT65                                                           
*                                                                               
         OC    CLOSEDAT,CLOSEDAT   ANY CLOSE DATE?                              
         BZ    BLDT65              NO                                           
         OC    OMOS,OMOS           ANY FILTERING?                               
         BZ    BLDT65              NO                                           
         CLC   OMOSST,CLOSEDAT     YES, CHECK AGAINST PEEL DATE                 
         BNL   BLDT65              PAST CLOSE                                   
         MVI   TSDBSTAR,C'*'                                                    
         DROP  RF                                                               
*                                                                               
BLDT65   XC    CLOSEDAT,CLOSEDAT   CLEAR FOR NEXT ITEM                          
         ZAP   TSDBFWCF,BALBFCF    BALANCE CARRIED FORWARD                      
         ZAP   TSDDRCF,DEBCF       DEBITS CARRIED FORWARD                       
         ZAP   TSDCRCF,CREDCF      CREDITS CARRIED FORWARD                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    *+12                 NO                                          
         BAS   RE,FGRMTSAR          YES, FORMAT FOR GRIDS                       
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDTERRX                                                         
*                                                                               
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         MVC   SVLOWACC,SPACES     CLEAR LOW LEVEL ACCOUNT CODE                 
         XC    SVLOWOAC,SVLOWOAC   CLEAR LOW LEVEL ACTIVITY DATE                
         XC    SVLOWOPE,SVLOWOPE   CLEAR LOW LEVEL PEELED DATE                  
         XC    SVLOWOOP,SVLOWOOP   CLEAR LOW LEVEL OPENED DATE                  
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTERRX                                                         
         DROP  R3,R2                                                            
*                                                                               
BLDTX    J     OKXIT                                                            
BLDTERRX J     ERRXIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE ACCOUNT DETAILS                                                *         
*        ON ENTRY AIO1 = ACCOUNT RECORD                               *         
*                 ACCNAME CONTAINS ACCOUNT NAME                       *         
*                 OACDAT  CONTAINS ACTIVITY DATE                      *         
*                 OPEDAT  CONTAINS PEELED DATE                        *         
*                 OOPDAT  CONTAINS OPEN DATE                          *         
***********************************************************************         
SVACCD   NTR1                                                                   
*                                                                               
         LA    RF,HILEV                                                         
         USING SVHIGHLD,RF                                                      
         CLI   ACCLEV,1                                                         
         BNE   *+10                                                             
         XC    0(SVHLNQ*SVHMAXQ,RF),0(RF)  INIT HIGHER LEV ACC AREA             
         SR    RE,RE                                                            
         IC    RE,ACCLEV                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(SVHLNQ)                                                    
         LA    RF,0(RF,RE)                                                      
         L     R1,AIO1                     R1=A(ACCOUNT RECORD)                 
         MVC   SVHCODE,ACTKACT-ACTRECD(R1) ACCOUNT CODE                         
         MVC   SVHNAME,ACCNAME             ACCOUNT NAME                         
         MVC   SVHACDAT,OACDAT             ACTIVITY DATE                        
         MVC   SVHPEDAT,OPEDAT             PEELED DATE                          
         MVC   SVHOPDAT,OOPDAT             OPENED DATE                          
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                        *         
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
*                                                                               
         USING OPTVALSD,R5                                                      
         L     R5,AOPTVALS         R3=A(OPTION VALUES)                          
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
         CLI   TSARFMT-TSARRECD(R4),TSTOTITM TOTAL LINE ITEM?                   
         BE    FORM40                                                           
         USING SCRLIN1D,R2         ACCOUNT DATA LINE                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1CODE,TSDCODE    ACCOUNT CODE                                 
*                                                                               
         CLI   TSDTYPE,TSDUNKQ     UNKNOWN BALANCES?                            
         BNE   FORM04                                                           
         MVC   WORK,STARS                                                       
*                                                                               
         LA    R1,L'SCR1BFWD-1                                                  
         LA    R3,L'MX@UNKWN-1                                                  
         LA    RE,MX@UNKWN(R3)                                                  
         LA    RF,WORK                                                          
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCT   R3,*-12                                                          
         DC    H'0'                                                             
*                                                                               
         LA    RE,2(R3)                                                         
         CR    RE,R1                                                            
         BH    FORM03                                                           
         SR    R1,R3                                                            
         SRA   R1,1                                                             
         LA    RF,0(R1,RF)                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),MX@UNKWN                                                 
*                                                                               
FORM03   MVC   SCR1BFWD,WORK                                                    
         MVC   SCR1DR,WORK                                                      
         MVC   SCR1CR,WORK                                                      
         B     FORM10                                                           
*                                                                               
FORM04   CLI   TSDTYPE,TSDOFFQ     OFFICE AMOUNT TYPE?                          
         BE    FORM05                                                           
         CLI   TSDTYPE,TSDDETQ     AMOUNT DETAILS TYPE?                         
         BNE   FORM10                                                           
         L     RF,AOPTVALS                                                      
         CLI   OOFFDET-OPTVALSD(RF),C'Y'                                        
         BNE   FORM05                                                           
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
*                                                                               
FORM05   CURED (P8,TSDDR),(L'SCR1DR,SCR1DR),2,MINUS=YES,DECS=ROUND,ZERO*        
               =BLANK                                                           
         CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES,DECS=ROUND,ZERO*        
               =BLANK                                                           
         ZAP   BALANCE,TSDDR                                                    
         SP    BALANCE,TSDCR                                                    
*                                                                               
         CLI   OBFWD,C'N'          PRINTING BALANCE FORWARD?                    
         BNE   *+10                YES                                          
         ZAP   TSDBLFWD,=P'0'                                                   
         AP    BALANCE,TSDBLFWD    AND AND PRINT BF                             
         CURED (P8,TSDBLFWD),(L'SCR1BFWD,SCR1BFWD),2,MINUS=YES,DECS=ROU*        
               ND,ZERO=BLANK                                                    
*                                                                               
         CURED (P8,BALANCE),(L'SCR1BAL,SCR1BAL),2,MINUS=YES,DECS=ROUND,*        
               ZERO=BLANK                                                       
*                                                                               
FORM10   CLI   TSDBSTAR,C'*'                                                    
         BNE   *+10                                                             
         MVC   SCR1BFWD+L'SCR1BFWD(1),TSDBSTAR                                  
         MVC   TEMP(L'TSDNAME),TSDNAME                                          
         LA    RF,L'TSDNAME                                                     
         TM    TSDJOBST,JOBSXJOB   EXPENSE JOB?                                 
         BNO   FORM15                                                           
         LA    RE,TEMP-1(RF)                                                    
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         MVI   2(RE),C'/'                                                       
         MVC   3(3,RE),MX@EXP EXPENSE                                           
         LA    RF,3+3(RF)                                                       
*                                                                               
FORM15   GOTO1 VCHOPPER,DMCB,((RF),TEMP),(L'TMPNB1,TMPNBLK),3                   
         LA    RF,SCR1NAME                                                      
         LA    RE,TMPNB1                                                        
         SR    R1,R1                                                            
         LA    R0,3                MAX NUMBER OF LINES                          
*                                                                               
FORM20   CLC   0(L'TMPNB1,RE),SPACES                                            
         BE    FORM30                                                           
         MVC   0(L'SCR1NAME,RF),0(RE)                                           
         LA    RF,L'DUMLIN1(RF)                                                 
         LA    RE,L'TMPNB1(RE)                                                  
         LA    R1,1(R1)                                                         
         BCT   R0,FORM20                                                        
*                                                                               
FORM30   STC   R1,LINSUSED                                                      
         ZAP   BALBFCF,TSDBFWCF    CARRIED FORWARD BALANCES                     
         ZAP   DEBCF,TSDDRCF                                                    
         ZAP   CREDCF,TSDCRCF                                                   
         CLI   TSDTYPE,TSDDETQ     AMOUNT DETAIL TYPE?                          
         BNE   FORMX                                                            
         AP    BALBFCF,TSDBLFWD    UPDATE CARRIED FORWARD BALANCES              
         AP    DEBCF,TSDDR                                                      
         AP    CREDCF,TSDCR                                                     
         B     FORMX                                                            
         DROP  R2,R4                                                            
*                                                                               
         USING SCRTOT1D,R2         TOTAL LINE                                   
FORM40   LA    R2,L'DUMLIN1(R2)                                                 
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@TOTAL   TOTAL                                        
         CURED (P8,TSTBLFWD),(L'SCRTBFWD,SCRTBFWD),2,MINUS=YES,DECS=ROU*        
               ND,ZERO=BLANK                                                    
         CURED (P8,TSTDR),(L'SCRTDR,SCRTDR),2,MINUS=YES,DECS=ROUND,ZERO*        
               =BLANK                                                           
         CURED (P8,TSTCR),(L'SCRTCR,SCRTCR),2,MINUS=YES,DECS=ROUND,ZERO*        
               =BLANK                                                           
         ZAP   BALANCE,TSTBLFWD BALANCE BROUGHT FORWARDS                        
         AP    BALANCE,TSTDR                                                    
         SP    BALANCE,TSTCR                                                    
         CURED (P8,BALANCE),(L'SCRTBAL,SCRTBAL),2,MINUS=YES,DECS=ROUND,*        
               ZERO=BLANK                                                       
*                                                                               
FORM50   MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
*                                                                               
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
FGRM01   MVI   LINSUSED,0               NUMBER OF LINES USED                    
         MVI   DISATRIB,0               DISPLAY ATTRIBUTES                      
*                                                                               
         L     R0,ADUMLINE              CLEAR DUMMY SCREEN LINES                
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,ATSARREC                                                      
*                                                                               
         TM    OVRSTAT,OVRGINIT         ALREADY INITIALIZED?                    
         BO    FGRM10                   (YES)                                   
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),BASKEY                           
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         B     FGRM01                                                           
*                                                                               
FGRM10   CLI   TSARFMT,TSTOTITM         TOTAL LINE ITEM?                        
         BNE   FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),BASKEY                            
         B     FGRMX                                                            
*                                                                               
FGRM20   LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
         MVC   TMPNBLK(10),SPACES                                               
         TM    TSDJOBST,JOBSXJOB            EXPENSE JOB?                        
         BZ    *+14                                                             
         MVC   TMPNBLK(L'MX@EXP),MX@EXP                                         
         B     FGRM25                                                           
         TM    TSDJOBST,JOBSTUD             STUDIO JOB?                         
         BZ    *+14                                                             
         MVC   TMPNBLK(L'MX@STDIO),MX@STDIO                                     
         B     FGRM25                                                           
         MVC   TMPNBLK(L'MX@BLB),MX@BLB       BILLABLE                          
*                                                                               
FGRM25   TM    INTFLAG,SCRFULLQ    ATTEMPT TO DISPLAY?                          
         BZ    FGRM30               NO                                          
         GOTO1 ADISGRD,DMCB,('DWNDNB',AGCTBL),BASKEY                            
         B     FGRMX                                                            
FGRM30   GOTO1 ADISGRD,DMCB,(0,AGCTBL),BASKEY                                   
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TOTAL CARRIED FORWARD LINE                                  *         
***********************************************************************         
TOTCFW   NTR1                                                                   
         GOTO1 ASCRNCLR,DISLINE    CLEAR SCREEN BELOW CURRENT LINE              
         LA    R2,ENQDATL          R2=A(LAST SCREEN LINE)                       
         USING SCRCFWDD,R2                                                      
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         CLI   TSTFMT-TSARTOTD(R4),TSTOTITM ALL RECORDS READ                    
         BE    TOTCF20                                                          
*                                                                               
         USING TSARDATD,R4                                                      
TOTCF10  MVC   SCRCTCF,MX@TCF      WE ARE SHOWING TOTAL CARRIED FORWARD         
         ZAP   TEMPDR,TSDDRCF      DEBITS                                       
         ZAP   TEMPCR,TSDCRCF      CREDITS                                      
         ZAP   TEMPBFWD,TSDBFWCF   BALANCE BROUGHT FORWARDS                     
         B     TOTCF30                                                          
*                                                                               
         USING TSARTOTD,R4                                                      
TOTCF20  MVC   SCRCTCF(L'MX@TOTAL),MX@TOTAL WE ARE SHOWING A TOTAL FIG.         
         ZAP   TEMPDR,TSTDR        DEBITS                                       
         ZAP   TEMPCR,TSTCR        CREDITS                                      
         ZAP   TEMPBFWD,TSTBLFWD   BALANCE BROUGHT FORWARDS                     
         DROP  R4                                                               
*                                                                               
TOTCF30  CURED (P8,TEMPBFWD),(L'SCRCBFWD,SCRCBFWD),2,MINUS=YES,DECS=ROU*        
               ND,ZERO=BLANK                                                    
         CURED (P8,TEMPDR),(L'SCRCDR,SCRCDR),2,MINUS=YES,DECS=ROUND,ZER*        
               O=BLANK                                                          
         CURED (P8,TEMPCR),(L'SCRCCR,SCRCCR),2,MINUS=YES,DECS=ROUND,ZER*        
               O=BLANK                                                          
*                                                                               
         ZAP   BALANCE,TEMPBFWD    BALANCE BROUGHT FORWARD                      
         AP    BALANCE,TEMPDR                                                   
         SP    BALANCE,TEMPCR      BALANCE AMOUNT                               
*                                                                               
         CURED (P8,BALANCE),(L'SCRCBAL,SCRCBAL),2,MINUS=YES,DECS=ROUND,*        
               ZERO=BLANK                                                       
*                                                                               
         LA    R2,ENQDATLH                                                      
         OI    FLDOIND-FLDHDRD(R2),FOUTTRN                                      
         OI    FLDATB-FLDHDRD(R2),FATBHIGH                                      
         SR    RF,RF                                                            
         IC    RF,DISEND                                                        
         LA    RF,2(RF)                                                         
         STC   RF,DISLINE          UPDATE DISPLAY END LINE COUNT                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DEAL WITH THE TOTAL LINE                                            *         
***********************************************************************         
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
*                                                                               
TOT10    MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTDR,DEBCF                                                      
         ZAP   TSTCR,CREDCF                                                     
         ZAP   TSTBLFWD,BALBFCF                                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    *+12                 NO                                          
         BAS   RE,FGRMTSAR          YES, FORMAT FOR GRIDS                       
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
TOT20    GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   TOTX                                                             
         MVC   TSLSTLIN,TSCURRNO   LAST TSAR DISPLAYED                          
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         J     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
STARS    DC    C'********************'                                          
NULLOFF  DC    X'FFFF'             NO OFFICE/ACCOUNT TOTAL                      
REJOFF   DC    X'FFFE'             REJECTED OFFICE                              
UNKNOWN  DC    X'FFFD'             UNKNOWN AMOUNTS                              
ONEP     DC    C'1P'                                                            
ONER     DC    C'1R'                                                            
ONEJ     DC    C'1J'                                                            
SINCUL   DC    C'SI'                                                            
GPROUL   DC    C'GP'                                                            
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH41,78                                                      
         DCDDL AC#ENH42,78                                                      
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#TCF,25                                                        
         DCDDL AC#OFF,10                                                        
         DCDDL AC#EXP,L'MX@EXP                                                  
         DCDDL AC#MOA,3                                                         
         DCDDL AC#BALBF,3                                                       
         DCDDL AC#UNKWN,7                                                       
         DCDDL AC#PELED,8,C                                                     
         DCDDL AC#OPND,8,C                                                      
         DCDDL AC#LSTAC,14                                                      
         DCDDL AC#NAME,L'MX@NAME           NAME                                 
         DCDDL AC#BAL,L'MX@BAL             BALANCE                              
         DCDDL AC#BALBF,L'MX@BALBF         BAL B/F                              
         DCDDL AC#JOB,L'MX@JOB             JOB                                  
         DCDDL AC#TYPE1,L'MX@TYPE1         TYPE                                 
         DCDDL AC#STDIO,L'MX@STDIO         STUDIO                               
         DCDDL AC#BLB,L'MX@BLB             BILLABLE                             
         DC    AL1(EOT)                                                         
***********************************************************************         
*        CUSTOMIZED DISPLAY ROUTINE                                   *         
*         R3 = ADDRESS OF GRID TAB ENTRY                                        
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
DISCUS   NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
         L     R4,ATSARREC         R2=A(TSAR RECORD)                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TSARLEN         LENGTH OF TSAR RECORD                       
         SHI   RF,TSARDATA-TSARRECD SUBTRACT OVERHEAD                           
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4           YES, GET AMOUNTS                           
*                                                                               
         CLI   TSDFMT,TSTOTITM      TOTAL LINE ITEM?                            
         BE    DISC30                                                           
*                                                                               
         CHI   RF,TSDLEN1Q          ARE AMOUNTS THERE?                          
         BNH   DISCX                 NO, THEN SPACE                             
*                                                                               
         CLI   GCTCOID,GCTBBFQ      BBF                                         
         BNE   DISC10                                                           
         ZAP   BALANCE,TSDBLFWD                                                 
         ZAP   BALBFCF,TSDBFWCF                                                 
         CLI   TSDTYPE,TSDDETQ      AMOUNT DETAIL TYPE?                         
         BNE   DISC100                                                          
         AP    BALBFCF,TSDBLFWD                                                 
         B     DISC100                                                          
*                                                                               
DISC10   CLI   GCTCOID,GCTDRQ       DEBITS                                      
         BNE   DISC20                                                           
         ZAP   BALANCE,TSDDR                                                    
         ZAP   DEBCF,TSDDRCF                                                    
         CLI   TSDTYPE,TSDDETQ      AMOUNT DETAIL TYPE?                         
         BNE   DISC100                                                          
         AP    DEBCF,TSDDR                                                      
         B     DISC100                                                          
*                                                                               
DISC20   CLI   GCTCOID,GCTCRQ       CREDITS                                     
         BNE   DISC30                                                           
         ZAP   BALANCE,TSDCR                                                    
         ZAP   CREDCF,TSDCRCF                                                   
         CLI   TSDTYPE,TSDDETQ      AMOUNT DETAIL TYPE?                         
         BNE   DISC100                                                          
         AP    CREDCF,TSDCR                                                     
         B     DISC100                                                          
*                                                                               
DISC30   CLI   GCTCOID,GCTBALQ      BALANCE                                     
         BNE   DISCX                                                            
         CLI   TSDFMT,TSTOTITM      TOTAL LINE ITEM?                            
         BE    DISC40                                                           
         ZAP   BALANCE,TSDBLFWD                                                 
         AP    BALANCE,TSDDR                                                    
         SP    BALANCE,TSDCR                                                    
         B     DISC100                                                          
*                                                                               
         USING TSARTOTD,R4           YES, GET AMOUNTS                           
DISC40   ZAP   BALANCE,TSTBLFWD                                                 
         AP    BALANCE,TSTDR                                                    
         SP    BALANCE,TSTCR                                                    
         B     DISC100                                                          
*                                                                               
DISC100  CURED BALANCE,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                
         LR    R1,R0                                                            
         J     XITR1                                                            
*                                                                               
DISCX    LHI   R1,1                                                             
         MVI   TEMP,C' '                                                        
         J     XITR1                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
***********************************************************************         
*        DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD                           
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,99,L'LC@ACC,L'TSDCODE)     ACCOUNT                    
         DC    AL2(LC@ACC-WORKD,TSDCODE-TSARDATD)                               
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'MX@NAME,L'TSDNAME)    NAME                       
         DC    AL2(MX@NAME-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'MX@BALBF,0)               BBF                    
         DC    AL2(MX@BALBF-OVERWRKD,ADISCUS-OVERWRKD)                          
         DC    AL1(GCTITOT+GCTIROUT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)              
         DC    AL1(0,L'TSTBLFWD),AL2(TSTBLFWD-TSARTOTD)                         
GCT3LQ   EQU   *-GCTCOL3                                                        
GCTBBFQ  EQU   3                                                                
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'LC@DRS,0)               DEBITS                   
         DC    AL2(LC@DRS-WORKD,ADISCUS-OVERWRKD)                               
         DC    AL1(GCTITOT+GCTIROUT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTDR),AL2(TSTDR-TSARTOTD)                               
GCT4LQ   EQU   *-GCTCOL4                                                        
GCTDRQ   EQU   4                                                                
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'LC@CRS,0)               CREDITS                  
         DC    AL2(LC@CRS-WORKD,ADISCUS-OVERWRKD)                               
         DC    AL1(GCTITOT+GCTIROUT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTCR),AL2(TSTCR-TSARTOTD)                               
GCT5LQ   EQU   *-GCTCOL5                                                        
GCTCRQ   EQU   5                                                                
*                                                                               
GCTCOL6  DC    AL1(GCT6LQ,06,L'MX@BAL,0)              BALANCE                   
         DC    AL2(MX@BAL-OVERWRKD,ADISCUS-OVERWRKD)                            
         DC    AL1(GCTITOT+GCTIROUT+GCTIRTOT+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(ADISCUS-OVERWRKD)                                   
GCT6LQ   EQU   *-GCTCOL6                                                        
GCTBALQ  EQU   6                                                                
*                                                                               
GCTCOL7  DC    AL1(GCT7LQ,07,L'MX@LSTAC,1)           ACTIVITY DATE              
         DC    AL2(MX@LSTAC-OVERWRKD,TSDACDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT7LQ   EQU   *-GCTCOL7                                                        
*                                                                               
GCTCOL8  DC    AL1(GCT8LQ,08,L'MX@PELED,1)             PEELED DATE              
         DC    AL2(MX@PELED-OVERWRKD,TSDPEDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT8LQ   EQU   *-GCTCOL8                                                        
*                                                                               
GCTCOL9  DC    AL1(GCT9LQ,09,L'MX@OPND,1)              OPENED DATE              
         DC    AL2(MX@OPND-OVERWRKD,TSDOPDAT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SJ'                                                            
GCT9LQ   EQU   *-GCTCOL9                                                        
*                                                                               
GCTCOL10 DC    AL1(GCT10LQ,10,L'MX@JOBTY,10)            JOB TYPE                
         DC    AL2(MX@JOBTY-OVERWRKD,TMPNBLK-OVERWRKD)                          
         DC    AL1(GCTIOVER,GCTISPDA,0,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SJ'                                                            
GCT10LQ  EQU   *-GCTCOL10                                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
**********************************************************************          
         EJECT                                                                  
OVERWRKD DSECT                                                                  
ADISCUS  DS    A                   DISPLAY CUSTOM GRID COLUMNS                  
TMPNBLK  DS    0CL51               TEMPORARY JOB NAME BLOCK                     
TMPNB1   DS    CL17                TEMP JOB NAME LINE 1                         
TMPNB2   DS    CL17                TEMP JOB NAME LINE 2                         
TMPNB3   DS    CL17                TEMP JOB NAME LINE 3                         
ACCNAME  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ACCACDAT DS    PL(L'RSTTDATE)      ACTIVITY DATE                                
OACDAT   DS    PL(L'RSTTDATE)      ACTIVITY DATE                                
OPEDAT   DS    PL(L'RSTTDATE)      PEELED DATE                                  
OOPDAT   DS    PL(L'RSTTDATE)      OPENED DATE                                  
JOBST    DS    XL(L'JOBSTA1)       JOB STATUS BYTE                              
TEMPBFWD DS    PL8                 TEMP BFWD                                    
TEMPDR   DS    PL8                 TEMP DR                                      
TEMPCR   DS    PL8                 TEMP CR                                      
BALANCE  DS    PL8                 BALANCE AMOUNT                               
ACCLEV   DS    X                   CURRENT ACCOUNT LEVEL                        
INTFLAG  DS    X                   INTERNAL FLAG                                
SCRFULLQ EQU   X'01'               . SCREEN IS FULL                             
*                                                                               
TEMPOFF  DS    CL2                 TEMP OFFICE                                  
TEMPOFFL DS    XL512               TEMP OFFICE LIST                             
*                                                                               
DSMIX    DS    0C                                                               
MX@ENH41 DS    CL78                                                             
MX@ENH42 DS    CL78                                                             
MX@TOTAL DS    CL9                                                              
MX@TCF   DS    CL25                                                             
MX@OFF   DS    CL10                                                             
MX@EXP   DS    CL7                                                              
MX@MOA   DS    CL3                                                              
MX@BFW   DS    CL3                                                              
MX@UNKWN DS    CL7                                                              
MX@PELED DS    CL8                                                              
MX@OPND  DS    CL8                                                              
MX@LSTAC DS    CL14                LAST ACTIVITY                                
MX@NAME  DS    CL4                 NAME                                         
MX@BAL   DS    CL8                 BALANCE                                      
MX@BALBF DS    CL7                 BAL B/F                                      
MX@JOBTY DS    0CL8                JOB TYPE                                     
MX@JOB   DS    CL4                 JOB                                          
MX@TYPE1 DS    CL4                 TYPE                                         
MX@STDIO DS    CL6                 STUDIO                                       
MX@BLB   DS    CL8                 BILLABLE                                     
*                                                                               
         EJECT                                                                  
**********************************************************************          
* SCREEN DSECT                                                                  
**********************************************************************          
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CODE DS    CL(L'ACTKACT)       ACCOUNT CODE                                 
         DS    CL1                                                              
SCR1NAME DS    CL17                ACCOUNT NAME                                 
SCR1BFWD DS    CL12                BAL BFWD                                     
SCR1DR   DS    CL12                DEBITS                                       
SCR1CR   DS    CL12                CREDITS                                      
SCR1BAL  DS    CL12                BALANCE                                      
*                                                                               
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@TOTAL)      TOTAL                                        
         DS    CL21                                                             
SCRTBFWD DS    CL12                BAL BFWD                                     
SCRTDR   DS    CL12                DEBITS                                       
SCRTCR   DS    CL12                CREDITS                                      
SCRTBAL  DS    CL12                BALANCE                                      
*                                                                               
SCRCFWDD DSECT                     COVER SCREEN CFWD LINE                       
SCRCTCF  DS    CL(L'MX@TCF)        TOTALS CARRIED FORWARD                       
         DS    CL5                                                              
SCRCBFWD DS    CL12                BAL BFWD                                     
SCRCDR   DS    CL12                DEBITS                                       
SCRCCR   DS    CL12                CREDITS                                      
SCRCBAL  DS    CL12                BALANCE                                      
*                                                                               
**********************************************************************          
* TSAR RECORD DSECTS                                                            
**********************************************************************          
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDTYPE  DS    CL1                                                              
TSDDETQ  EQU   1                   ACCOUNT DETAIL                               
TSDOFFQ  EQU   2                   OFFICE DETAIL                                
TSDUNKQ  EQU   3                   UNKNOWN BALANCES                             
TSDCODE  DS    CL(L'ACTKACT)       ACCOUNT CODE                                 
TSDNAME  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
TSDJOBST DS    CL(L'JOBSTA1)       JOB STATUS                                   
TSDACDAT DS    CL(L'RSTTDATE)      ACTIVITY DATE                                
TSDPEDAT DS    CL(L'RSTTDATE)      PEELED DATE                                  
TSDOPDAT DS    CL(L'RSTTDATE)      OPENED DATE                                  
TSDBFWCF DS    PL(L'ABLFRWD)       BAL BFWD CARRIED FORWARD                     
TSDDRCF  DS    PL(L'ABLDR)         DEBITS CARRIED FORWARD                       
TSDCRCF  DS    PL(L'ABLCR)         CREDITS CARRIED FORWARD                      
TSDBSTAR DS    CL1                 ASTERISK FOR PEELED DATA                     
TSDLEN1Q EQU   *-TSARDATD                                                       
TSDDR    DS    CL(L'ABLDR)         DEBITS                                       
TSDCR    DS    CL(L'ABLCR)         CREDITS                                      
TSDBLFWD DS    CL(L'ABLFRWD)       BALANCE BROUGHT FORWARDS                     
TSDLEN2Q EQU   *-TSARDATD                                                       
*                                                                               
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTDR    DS    CL(L'ABLDR)         DEBITS                                       
TSTCR    DS    CL(L'ABLCR)         CREDITS                                      
TSTBLFWD DS    CL(L'ABLFRWD)       BALANCE BROUGHT FORWARDS                     
TSTLNQ   EQU   *-TSARTOTD                                                       
*                                                                               
**********************************************************************          
* DSECTS                                                                        
**********************************************************************          
MTHD     DSECT                     BUCKET OR PRIOR BUCKET DATA                  
MTHMOS   DS    PL2                                                              
MTHDR    DS    PL8                                                              
MTHCR    DS    PL8                                                              
*                                                                               
SVHIGHLD DSECT                     SAVE AREA FOR HIGH LEVEL ACCOUNTS            
SVHCODE  DS    CL(L'ACTKACT)       ACCOUNT CODE                                 
SVHNAME  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
SVHACDAT DS    CL(L'RSTTDATE)      ACTIVITY DATE/PEELED DATE                    
SVHPEDAT DS    CL(L'RSTTDATE)      PEELED DATE                                  
SVHOPDAT DS    CL(L'RSTTDATE)      OPENED DATE                                  
SVHLNQ   EQU   *-SVHIGHLD                                                       
SVHMAXQ  EQU   3                   MAX NUM OF ACCS HIGHER THAN LOWEST           
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
**********************************************************************          
* SAVED STORAGE                                                                 
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                   RELOCATABLE                                  
AGCTBL   DS    A                   GRID COLUMN TABLE                            
LISTFLAG DS    X                   LIST FLAG                                    
OUTSTNDQ EQU   X'80'               . OUTSTNDNG BALTAB ENTRIES FOR TSAR          
KEEPQ    EQU   X'40'               . MUST KEEP ACCOUNT RECORD                   
UNKNOWNQ EQU   X'20'               . UNKNOWN BALANCE                            
NEEDOFFQ EQU   X'10'               . NEED OFFICE INFORMATION                    
ROFFBUKQ EQU   X'08'               . READ OFFICE/CONTRA BUCKETS                 
LISGRINQ EQU   X'01'               . GRIDS INIT                                 
CLOSEDAT DS    XL(L'RSTBDATE)      LAST CLOSED MOS                              
OFFDONE  DS    XL2                 NUMBER OF OFFICES PROCESSED                  
SVLOWACC DS    CL(L'ACTKULA)       SAVED LOW LEVEL ACC CODE                     
SVLOWOAC DS    CL(L'RSTTDATE)      LOW LEV ACTIVE DATE                          
SVLOWOPE DS    CL(L'RSTTDATE)      LOW LEV PEELED DATE                          
SVLOWOOP DS    CL(L'RSTTDATE)      LOW LEV OPENED DATE                          
LOWLEV   DS    X                   LOWEST LEVEL TO BE DISPLAYED (LIST)          
LOWLEN   DS    X                   LOWEST LENGTH TO BE DISPLAYED                
HILEV    DS    XL(SVHLNQ*SVHMAXQ)  HIGHER LEV ACC AREA                          
*                                                                               
LIVALS   DS    0PL8          CREDITOR VALUES                                    
BALFRWD  DS    PL8             ->  BALANCE BROUGHT FORWARD                      
DEBTOT   DS    PL8             ->  TOTAL DEBITS                                 
CRETOT   DS    PL8             ->  TOTAL CREDITS                                
BALBFCF  DS    PL8           ->    BALANCE BFWD CARRIED FWD                     
DEBCF    DS    PL8           ->    DEBITS CARRIED FORWARD                       
CREDCF   DS    PL8           ->    CREDITS CARRIED FORWARD                      
LIVALLNQ EQU   *-LIVALS                                                         
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACENQ05   08/02/17'                                      
         END                                                                    
