*          DATA SET ACENQ09    AT LEVEL 017 AS OF 01/30/14                      
*PHASE T62009C                                                                  
*INCLUDE BINSRCH                                                                
T62009   TITLE 'ACCOUNT ENQUIRY-STATEMENT/JOB STATEMENT/DDS STATEMENT'          
***********************************************************************         
*        STATEMENT- ALL LEDGERS EXCEPT 'SJ' PRESENT BALANCE AND       *         
*                   ACCOUNT TOTAL GENERATED FROM BALANCE ELEMENT      *         
*                   UNLESS DATE FILTERS APPLIED                       *         
*                                                                     *         
*        DDS STMNT- ALL LEDGERS EXCEPT 'SJ' PRESENT BALANCE AND       *         
*                   ACCOUNT TOTAL GENERATED FROM TRANSACTIONS         *         
*                                                                     *         
*        JOB STMNT- ONLY SJ LEDGER PRESENT BALANCE AND                *         
*                   ACCOUNT TOTAL GENERATED FROM TRANSACTIONS         *         
*                                                                     *         
*        STMNT OFF- ALL LEDGERS EXCEPT 'SJ' PRESENT BALANCE AND       *         
*                   ACCOUNT TOTAL GENERATED FROM TRANSACTIONS         *         
***********************************************************************         
T62009   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ENQ9**,R7,R6,CLEAR=YES,RR=RE                                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*---------------------------------------------------------------------*         
* SET GRID ADDRESSES                                                  *         
*---------------------------------------------------------------------*         
         L     RF,=A(GRITOT)                                                    
         AR    RF,RE                                                            
         ST    RF,AGRITOT                                                       
*                                                                               
         L     RF,=A(GRIDAT)                                                    
         AR    RF,RE                                                            
         ST    RF,AGRIDAT                                                       
*                                                                               
         L     RF,=A(GCTBL)                                                     
         CLI   SVACTNUM,ACTNSTOF   TYPE = SF?                                   
         BE    *+8                                                              
         L     RF,=A(GC2BL)                                                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL                                                        
*---------------------------------------------------------------------*         
* DATA DICTIONARY                                                     *         
*---------------------------------------------------------------------*         
MAIN08   GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
         ZAP   DISPLDDR,=P'0'      DISPLAYED DEBITS                             
         ZAP   DISPLDCR,=P'0'      DISPLAYED CREDITS                            
*---------------------------------------------------------------------*         
* FIRST FOR DISPLAY                                                   *         
*---------------------------------------------------------------------*         
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN11              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BE    MAIN10                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         B     ERRXIT                                                           
MAIN10   OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN20                                                           
*-----------------------------------                                            
* 2ND OR LATER DISPLAY                                                          
*-----------------------------------                                            
MAIN11   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
                                                                                
         TM    IOMAXSW,IOMRRDUP+IOMRRD25 LAST TIME READUP HIT MAX IO'S?         
         BZ    MAIN16                    . NO, SKIP                             
         GOTO1 AREADUP                   CALL READUP                            
         BNE   MAINX                     EXIT                                   
         B     MAIN70                                                           
*        CLC   IOKEY,SPACES                                                     
*        BE    MAIN70                                                           
*                                                                               
MAIN16   TM    DISPFLAG,NORECQ     ANY RECORDS?                                 
         BO    MAINX                                                            
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN25              YES                                          
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN30              YES                                          
*                                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
         USING OPTVALSD,RF                                                      
MAIN20   L     RF,AOPTVALS                                                      
         CLI   ODDS,C'Y'           DDS STATEMENT OPTION?                        
         BE    *+12                                                             
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BNE   MAIN25              NO                                           
         BAS   RE,TSARNOW          READ TSAR UP FRONT                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         TM    DISPFLAG,NORECQ                                                  
         BO    MAINX                                                            
         NI    OVRSTAT,X'FF'-OVRGDONE     NEED TO DISPLAY STILL                 
         DROP  RF                                                               
*                                                                               
MAIN25   ZAP   DEBITS,=P'0'        CONTRA DEBITS                                
         ZAP   CREDITS,=P'0'       CONTRA CREDITS                               
*---------------------------------------------------------------------*         
* REDISPLAY TSAR RECORDS                                              *         
*---------------------------------------------------------------------*         
         CLC   TSCURRNO,TSLSTREC   HAVE WE ALREADY GOT RECORD IN TSAR?          
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                                                             
         OI    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         B     MAIN190                                                          
*                                                                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         AHI   RF,1                                                             
         STCM  RF,3,TSCURRNO                                                    
         AP    DISPLDDR,DEBITS     DISPLAYED DEBITS                             
         AP    DISPLDCR,CREDITS    DISPLAYED CREDITS                            
         B     MAIN190             DISPLAY BALANCE/OTHERS INFO                  
*---------------------------------------------------------------------*         
* PROCESS THE ACCOUNT                                                           
*---------------------------------------------------------------------*         
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN50              NO                                           
         LA    R2,BASKEYH          TSAR BLOCK FULL                              
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BZ    MAIN56              YES                                          
         BAS   RE,DISBAL                                                        
         B     MAINX                                                            
*                                                                               
MAIN56   BAS   RE,GETODET                                                       
         BAS   RE,GETDET           GET DETAILS FROM RECORD                      
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
MAIN60   MVC   KEYSAVE2,IOKEY                                                   
         GOTO1 ANXTREC,NXTRMODE    GET NEXT RECORD                              
         BE    MAIN70                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         DC    H'0'                                                             
*                                                                               
MAIN70   LA    R3,IOKEY                                                         
         USING CHDRECD,R3                                                       
         MVC   KEYSAVE,CHDKEY                                                   
         CLC   CHDKCULA,IOKEYSAV   SAME ACCOUNT?                                
         BE    MAIN82              YES                                          
*                                                                               
         OI    DISPFLAG,ALLREADQ   NO, FINISHED READING                         
*                                                                               
         OC    TSLSTREC,TSLSTREC   HAVE WE ANY TSAR RECORDS?                    
         BNZ   MAIN88                                                           
         TM    DETFLAG,DETFOUND                                                 
         BO    MAIN88                                                           
*                                                                               
MAIN81   OI    DISPFLAG,NORECQ     NO RECORDS FOUND FOR ACCOUNT                 
         B     MAIN150                                                          
*                                                                               
MAIN82   BAS   RE,CKCONFLT         CHECK FOR CONTRA FILTER                      
         BNE   MAIN60                                                           
*                                                                               
MAIN84   CLC   CHDKCULC,KEYSAVE2+CHDKCULC-CHDKEY REC FOR CURR CONTRA?           
         BE    MAIN50                                                           
*                                                                               
MAIN88   CP    DEBITS,=P'0'        IF ZERO DEBITS ON THE CONTRA                 
         BNE   MAIN90                                                           
         CP    CREDITS,=P'0'       AND ZERO CREDITS ON THE CONTRA               
         BE    MAIN130             REJECT THE RECORD                            
*                                                                               
MAIN90   BAS   RE,BLDTSDAT         ELSE BUILD TSAR REC AND FORMAT DATA          
         BNE   ERRXIT                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    MAIN100                                                          
         OI    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         B     MAIN118                                                          
*                                                                               
MAIN100  MVC   TSLSTLIN,TSCURRNO   SAVE LAST TSAR REC                           
         AP    DISPLDDR,DEBITS     DISPLAYED DEBITS                             
         AP    DISPLDCR,CREDITS    DISPLAYED CREDITS                            
*                                                                               
MAIN118  AP    DEBTOT,DEBITS       DEBITS                                       
         AP    CRETOT,CREDITS      CREDITS                                      
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO       INCREMENT CURRUNT TSAR NUMBER                
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN130  CLC   CONTOFF,CHDKOFF                                                  
         BE    MAIN150                                                          
         USING OPTVALSD,R5                                                      
         L     R5,AOPTVALS                                                      
         OC    ENDBBF,ENDBBF                                                    
         BZ    *+12                                                             
         DROP  R5                                                               
         TM    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         BO    MAIN140                                                          
         CP    ODRTOT,=P'0'        OFFICE DEBIT TOTAL?                          
         BNE   MAIN140                                                          
         CP    OCRTOT,=P'0'        OFFICE CREDIT TOTAL?                         
         BE    MAIN150                                                          
*                                                                               
MAIN140  BAS   RE,BLDOFTOT         BUILD OFFICE TOTALS                          
         BNE   MAIN200                                                          
*                                                                               
         CLI   SVACTNUM,ACTNSTOF                                                
         BNE   *+12                                                             
         TM    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         BO    MAIN149                                                          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                                                             
         OI    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO   SAVE LAST TSAR REC                           
*                                                                               
MAIN149  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO       INCREMENT CURRENT TSAR NUMBER                
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN150  TM    DISPFLAG,ALLREADQ                                                
         BZ    MAIN190                                                          
         BAS   RE,BLDTOT                                                        
         BNE   MAIN200                                                          
*                                                                               
         CLI   SVACTNUM,ACTNSTOF   SF                                           
         BNE   *+12                                                             
         TM    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         BO    MAIN159                                                          
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISLPLAY DUMMY SCREEN LINES                  
         BE    *+12                                                             
         OI    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         B     *+10                                                             
         MVC   TSLSTLIN,TSCURRNO   SAVE LAST TSAR REC                           
*                                                                               
MAIN159  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO       INCREMENT CURRENT TSAR NUMBER                
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
MAIN190  BAS   RE,DISBAL           DISPLAY BALANCE/OTHERS INFO                  
*                                                                               
MAIN200  TM    FORMFLAG,SCRFULLQ   SCREEN IS FULL                               
         BZ    MAIN25                                                           
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* FIRST FOR DISPLAY FUNCTIONS                                         *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*-----------------------------------------                                      
* INITIALIZE FIELDS AND BUFFERS                                                 
*-----------------------------------------                                      
         ZAP   DEBITS,=P'0'        CONTRA DEBITS                                
         ZAP   CREDITS,=P'0'       CONTRA CREDITS                               
         ZAP   DEBTOT,=P'0'        ACCOUNT DEBIT TOTALS                         
         ZAP   CRETOT,=P'0'        ACCOUNT CREDIT TOTALS                        
         ZAP   BALFRWD,=P'0'       BALANCE BROUGHT FORWARD                      
         ZAP   BALTOT,=P'0'        BALANCE                                      
         ZAP   ODRTOT,=P'0'        OFFICE DEBIT TOTALS                          
         ZAP   OCRTOT,=P'0'        OFFICE CREDIT TOTALS                         
         ZAP   OBFTOT,=P'0'        OFFICE BALANCE BROUGHT FORWARD               
         MVI   FORMFLAG,0                                                       
         MVI   DETFLAG,0           INIT DETAIL FLAG                             
         MVI   STATFLAG,0                                                       
         MVI   NXTRMODE,0          ALL RECORDS REQUIRED                         
         XC    TEMPBLKN,TEMPBLKN   NUMBER OF ENTRIES IN BLOCK                   
         MVI   SCRLLADJ,5          SCROLL ADJUST FOR BAL/TOTAL LINE             
*                                                                               
         MVC   TEMPCOL1,SPACES                                                  
         MVC   TEMPCOL1(L'LC@WC2),LC@WC2            WORKCODE                    
         L     RE,AOPTVALS                                                      
         CLI   OSUMM1-OPTVALSD(RE),0                                            
         BNE   *+10                                                             
         MVC   TEMPCOL1(L'LC@CTRA),LC@CTRA         CONTRA ACCOUNT               
*                                                                               
         L     R0,ATEMPBLK         CLEAR TEMP BLOCK                             
         LHI   R1,MAXTEMP                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*-----------------------------------------                                      
* POSSIBLE SEARCH                                                               
*-----------------------------------------                                      
         CLI   SVACTNUM,ACTNJBST          TYPE = JS?                            
         BE    FSTD04                     . YES                                 
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
         B     FSTD05                                                           
FSTD04   GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,SPROUL,ACOMFACS,(0,0)             
*-----------------------------------------                                      
* GET UNIT/LEDGER AND ACCOUNTS NAMES                                            
*-----------------------------------------                                      
FSTD05   LA    R2,BASKEYH                 R2=A(KEY FIELD)                       
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
*                                                                               
FSTD07   GOTO1 AGETACC,UNITLEDG                                                 
         ORG   *-2                                                              
         CLI   SVACTNUM,ACTNJBST                                                
         BE    *+8                                                              
         LA    R1,0                                                             
         BASR  RE,RF                                                            
         BNE   FSTDERR                                                          
*                                                                               
         USING OPTVALSD,R5                                                      
         L     R5,AOPTVALS                                                      
         CLI   OBFWD,0             DEFAULT IS BF=Y                              
         BNE   *+8                                                              
         MVI   OBFWD,C'D'          USE 'D' TO INDICATE WE SET IT                
*---------------------------                                                    
* DETERMINE HOW TO GET DATA                                                     
*---------------------------                                                    
         CLI   SVACTNUM,ACTNJBST   TYPE=JS?                                     
         BE    FSTD32              YES                                          
         CLI   ODRAFT,C'Y'         DRAFTS?                                      
         BE    FSTD20              . YES                                        
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    FSTD20              . YES                                        
         CLI   SVACTNUM,ACTNSTMT   TYPE=ST?                                     
         BNE   FSTD20              NO                                           
*                                                                               
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER?                               
         BNZ   FSTD20              YES                                          
         CLI   LDGTOFFP,LDGOTRAN   OFFICE IN TRANSACTIONS?                      
         BNE   FSTD25              NO                                           
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    FSTD20              YES                                          
         CLI   TERMACCS,C'$'       LIMIT ACCESS?                                
         BNE   FSTD25              YES                                          
*                                                                               
FSTD20   TM    COMPSTA4,CPYSOFF2        2 CHAR OFFICE?                          
         BZ    FSTD30                   NO, EXIT                                
         CLI   UNITLEDG,C'S'                                                    
         BE    *+12                                                             
         CLI   UNITLEDG,C'G'                                                    
         BNE   FSTD30                                                           
         OI    STATFLAG,STOFBUK                                                 
         B     FSTD30                                                           
*                                                                               
FSTD25   OI    NXTRMODE,NXTRCACQ   READ CONTRA ACCOUNTS ONLY                    
*-------------------------------------------                                    
* SET FISCAL YEAR, DISPLAY MONTH OF SERVICE                                     
*-------------------------------------------                                    
FSTD30   GOTO1 ASETFIS                                                          
*-----------------------------                                                  
* DISPLAY UNIT/LEDGER ACCOUNT                                                   
*-----------------------------                                                  
FSTD32   LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD34                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
         B     FSTD36                                                           
FSTD34   GOTO1 ADISUL                   DISPLAY UNIT AND LEDGER NAMES           
*------------------------                                                       
* PROCESS ACCOUNT RECORD                                                        
*------------------------                                                       
FSTD36   L     R3,AIO1                                                          
         USING ACTRECD,R3                                                       
         LA    RF,ACTRFST                                                       
         MVC   ACCNAM,SPACES                                                    
*                                                                               
FSTD40   CLI   0(RF),EOR           END OF RECORD?                               
         BE    FSTD70                                                           
         CLI   0(RF),NAMELQ        NAME ELEMENT?                                
         BE    FSTD50                                                           
         CLI   0(RF),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    FSTD55                                                           
         CLI   0(RF),JOBELQ        JOB ELEMENT?                                 
         BE    FSTD60                                                           
FSTD45   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD40                                                           
*                                                                               
*--------------                                                                 
* NAME ELEMENT                                                                  
*--------------                                                                 
         USING NAMELD,RF                                                        
FSTD50   SR    RE,RE               SAVE ACCOUNT NAME                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAM(0),NAMEREC                                                
         LA    RE,1(RE)                                                         
         STC   RE,ACCNAMLN                                                      
         B     FSTD45                                                           
         DROP  RF                                                               
*                                                                               
*----------------                                                               
* STATUS ELEMENT                                                                
*----------------                                                               
         USING RSTELD,RF                                                        
FSTD55   MVC   CLOSEDAT,RSTBDATE   SAVE PEEL DATE                               
         B     FSTD45                                                           
         DROP  RF                                                               
*                                                                               
*-------------                                                                  
* JOB ELEMENT                                                                   
*-------------                                                                  
         USING JOBELD,RF                                                        
FSTD60   MVC   JOBSTAT,JOBSTA1     SAVE JOB STATUS                              
         B     FSTD45                                                           
         DROP  RF                                                               
*                                                                               
*---------------------------                                                    
* SET HEADINGS FOR NON-GRID                                                     
*---------------------------                                                    
FSTD70   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD97                                                           
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC                                         
         LA    R1,FLDDATA+L'MX@ACC-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
         MVC   2(L'ACCNAM,R1),ACCNAM                                            
         OI    FLDOIND,FOUTTRN                                                  
         SR    RE,RE                                                            
         IC    RE,ACCNAMLN                                                      
         LA    R1,3(RE,R1)                                                      
*                                                                               
         TM    JOBSTAT,JOBSXJOB                                                 
         BNO   FSTD75                                                           
         MVI   0(R1),C'/'                                                       
         MVC   2(L'MX@XJOB,R1),MX@XJOB                                          
         LA    R1,L'MX@XJOB+1(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
*                                                                               
FSTD75   CLC   SVCACN,SPACES       HAVE WE SPECIFIED STARTING CONTRA?           
         BE    FSTD80              NO                                           
         MVI   0(R1),C'/'                                                       
         MVC   2(L'MX@STCON,R1),MX@STCON                                        
         LA    R1,L'MX@STCON+1(R1)                                              
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
         MVC   0(0,R1),SVCACN      DISPLAY AS MUCH OF NAME AS POSS              
         LA    R1,1(RE,R1)                                                      
*                                                                               
FSTD80   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         CLI   SVACTNUM,ACTNSTOF   TYPE = SF?                                   
         BE    FSTD90              YES                                          
         LA    RF,FLDDATA                                                       
         MVC   FLDDATA(L'MX@ENH11),MX@ENH11                                     
         CLI   OSUMM1,0                                                         
         BE    FSTD85                                                           
         MVC   SCR1CON1-SCRLIN1D(L'SCR1CON1,RF),HYPHENS                         
         MVC   SCR1CON1+9-SCRLIN1D(L'MX@WC,RF),MX@WC                            
*                                                                               
FSTD85   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH12),MX@ENH12                                     
         B     FSTD95                                                           
*                                                                               
FSTD90   MVC   FLDDATA(L'MX@ENH30),MX@ENH30                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH31),MX@ENH31                                     
*                                                                               
FSTD95   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*-----------                                                                    
* FINISH UP                                                                     
*-----------                                                                    
FSTD97   GOTO1 ADISMOS,DMCB,(L'MX@MOA,MX@MOA),(L'MX@BFW,MX@BFW)                 
*                                                                               
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    FSTD100                                                          
         ZAP   BALFRWD,=P'0'                                                    
*                                                                               
FSTD100  LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD102                                                          
         GOTO1 ASCRNDIM,DMCB,(2,(R2))                                           
         B     FSTD104                                                          
FSTD102  LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
FSTD104  L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTD105  CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    FSTD110                                                          
*                                                                               
         CLI   OBUKTYPE,0                                                       
         BNE   FSTD108                                                          
         CLI   OHOURS,C'Y'                                                      
         BNE   *+12                                                             
         MVI   OBUKTYPE,C'H'                                                    
         B     FSTD108                                                          
         CLC   =C'1C',BASKEY         IF UNIT/LEDGER 1C                          
         BNE   FSTD108                                                          
         TM    COMPSTA5,CPYSNCST   AND NEW COST AGENCY                          
         BNO   FSTD108                                                          
         MVI   OBUKTYPE,C'1'       DEFAULT IS METHOD 1                          
FSTD108  OI    OBUKTYPE,C' '                                                    
         OI    OPAYTYPE,C' '                                                    
         TM    SECFFLAG,SECFRATE   NOT ALLOWED TO SEE RATE?                     
         BNO   FSTD109                                                          
         CLI   OBUKTYPE,C' '                                                    
         BH    FSTD109                                                          
         MVI   OBUKTYPE,C'H'       SET TO HOURS                                 
*                                                                               
FSTD109  BAS   RE,GETBALA                                                       
*                                                                               
FSTD110  GOTO1 AREADUP             READ AHEAD AND RETURN                        
         BNE   FSTDERR                                                          
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
* READ ALL OFFICE RECORDS UP FRONT FOR BALANCE BROUGHT FORWARD.       *         
* THIS IS ONLY NEEDED IF GETTING PASSED RECS IN CONTRA SEQUENCE.  THE *         
* CONTROLLER DOES NOT PASS OFFICE RECS WHEN READING IN CONTRA SEQ.    *         
* ON ENTRY AIOAREA1 CONTAINS ACCOUNT RECORD                           *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING OPTVALSD,R5                                                      
GETBALA  NTR1                                                                   
         TM    STATFLAG,STOFBUK         READ CONTRA OFFICE BUCKETS?             
         BO    GETBAX                   . YES                                   
         TM    NXTRMODE,NXTRCACQ        READ CONTRA ACCOUNTS ONLY?              
         BO    GETBAX                   . YES, EXIT                             
         TM    CORDFLAG,CORDMODQ        CONTRA ORDERED MODE?                    
         BZ    GETBAX                   . NO                                    
         TM    COMPSTA4,CPYSOFF2        2 CHAR OFFICE?                          
         BZ    GETBAX                   . NO, EXIT                              
*                                                                               
         L     R5,AOPTVALS                                                      
         MVC   ACNTKEY,IOKEY            SAVE THE ACCOUNT KEY                    
*                                                                               
         OC    ENDBBF,ENDBBF            BBF WANTED?                             
         BZ    GETBAXR                  . NO, EXIT                              
*----------------------------------                                             
* DECIDE WHICH OFFICE LIST WE NEED                                              
*----------------------------------                                             
         OC    OOFFICVL,OOFFICVL       OFFICE LIST OVERRIDE?                    
         BZ    *+12                    . NO                                     
         LA    R4,OOFFICLS             . YES, USE WANTED OFFICES                
         B     GETBA5                                                           
         L     R4,AOFFBLK              LIMITTED ACCESS                          
         CLI   LDGTOFFP,LDGOTRAN       SECURITY OFFICE IN TRANSACTIONS?         
         BE    *+8                     . YES                                    
         L     R4,AOFFBLK2             . NO, USE ALL OFFICES                    
         LA    R4,OFFAWORK-OFFALD(,R4)                                          
*--------------------------------                                               
* PROCESS OFFICE BALANCE RECORDS                                                
*--------------------------------                                               
GETBA5   LA    R3,IOKEY                                                         
         MVC   TWAOFWK#,=H'1'           START OF OFF LIST                       
GETBA10  CLC   TWAOFWK#,0(R4)           END OF OFFICE LIST?                     
         BH    GETBAXR                  . YES, DONE                             
         SR    R1,R1                                                            
         LH    R1,TWAOFWK#                                                      
         SLL   R1,1                     SHIFT NO OF OFFICES FOR INDEX           
         LA    RF,0(R1,R4)                                                      
         SRL   R1,1                                                             
         LA    R1,1(,R1)                                                        
         STH   R1,TWAOFWK#              BUMP TO NEXT                            
         CLC   0(L'TRNKOFF,RF),SPACES   SPACES MEANS FILLER OFFICE              
         BE    GETBA10                                                          
         MVC   TRNKCULA,ACNTKEY         SET ACCOUNT CODE                        
         MVC   TRNKOFF,0(RF)            SET  OFFICE CODE                        
*                                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1  GET OFFICE RECORD                       
         BE    GETBA20                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETBAXR                                                          
         B     GETBA10                  PROBLEM WITH OFF CODE GET NXT           
*                                                                               
GETBA20  BAS   RE,GETDET                GETDET WILL GET OFFICE BBF INFO         
         B     GETBA10                                                          
*                                                                               
GETBAXR  MVC   IOKEY,ACNTKEY            RESTORE ACCOUNT KEY                     
GETBAX   B     XIT                                                              
         DROP  R3,R5                                                            
*                                                                               
***********************************************************************         
*        READ ALL RECORDS UP FRONT (FOR USE WITH DDS OPTION)          *         
* ON ENTRY AIOAREA1 CONTAINS CONTRA HEADER RECORD                     *         
***********************************************************************         
TSARNOW  NTR1                                                                   
*                                                                               
         TM    DISPFLAG,ALLREADQ       ALL RECORDS READ?                        
         BO    TSARN18                                                          
*                                                                               
TSARN02  BAS   RE,GETODET                                                       
         BAS   RE,GETDET               GET DETAILS FROM RECORD                  
         TM    DISPFLAG,DISIOMAX                                                
         BO    TSARNX                                                           
*                                                                               
TSARN04  MVC   KEYSAVE2,IOKEY                                                   
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1  READ NEXT RECORD                         
         BE    TSARN06                                                          
         TM    IOERR,IOMAX             MAX IOS REACHED?                         
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     TSARNX                                                           
*                                                                               
TSARN06  LA    R3,IOKEY                                                         
         USING CHDRECD,R3                                                       
         MVC   KEYSAVE,CHDKEY      SAVE THE KEY FOR SEQ RESTORE                 
         CLC   CHDKCULA,IOKEYSAV   DOES REC BELONG TO CURRENT ACCOUNT?          
         BE    *+12                                                             
         OI    DISPFLAG,ALLREADQ       FINISHED READING RECORDS                 
         B     TSARN12                                                          
*                                                                               
         BAS   RE,CKCONFLT             CHECK FOR CONTRA FILTER                  
         BNE   TSARN04                                                          
*                                                                               
TSARN08  CLI   SVACTNUM,ACTNJBST       TYPE = JS?                               
         BNE   TSARN10                  NO                                      
         CLC   CHDKOFF(L'CHDKOFF+L'CHDKCULC),IOKEYSAV+CHDKOFF-CHDKEY            
         BE    TSARN02                                                          
         B     TSARN12                                                          
*                                                                               
TSARN10  CLC   CHDKCULC,IOKEYSAV+CHDKCULC-CHDKEY REC FOR CURR CONTRA?           
         BE    TSARN02                                                          
*                                                                               
TSARN12  CP    DEBITS,=P'0'        IF NO ZERO DEBITS ON THE CONTRA              
         BNE   *+14                                                             
         CP    CREDITS,=P'0'       AND ZERO CREDITS ON THE CONTRA               
         BE    TSARN16             REJECT THE RECORD                            
*                                                                               
         L     RE,AOPTVALS                                                      
         CLI   OSUMM1-OPTVALSD(RE),0                                            
         BE    *+8                                                              
         BAS   RE,GETWC                                                         
*                                                                               
         LA    R5,WORK                 R5=A(TABLE RECORD TO BE ADDED)           
         USING TEMPD,R5                                                         
         MVC   TEMPCONT,CONTCODE       CONTRA CODE                              
         MVC   TEMPNAME,CONTNAME       CONTRA NAME                              
         ZAP   TEMPDR,DEBITS                                                    
         ZAP   TEMPCR,CREDITS                                                   
         AP    DEBTOT,DEBITS           TOTAL DEBITS                             
         AP    CRETOT,CREDITS          TOTAL CREDITS                            
         LA    R2,MAXTEMP              R2=(MAX NUMBER OF TABLE RECORDS)         
         LA    R3,TEMPLNQ              R3=L'(TABLE RECORD)                      
         SR    R4,R4                                                            
         ICM   R4,3,TEMPBLKN         R4=(NUMBER OF TABLE RECORDS SOFAR)         
         TM    STATFLAG,STFULLQ        IS 'TEMPBLK' FULL?                       
         BNO   TSARN13                  NO                                      
         GOTO1 =V(BINSRCH),DMCB,(0,(R5)),ATEMPBLK,(R4),(R3),(0,L'TEMPCO*        
               NT),(R2),RR=RB                                                   
         B     TSARN14                                                          
TSARN13  GOTO1 =V(BINSRCH),DMCB,(1,(R5)),ATEMPBLK,(R4),(R3),(0,L'TEMPCO*        
               NT),(R2),RR=RB                                                   
         SR    RF,RF                                                            
         ICM   RF,7,DMCB+1         RF=A(TABLE ENTRY/ZERO IF TABLE FULL)         
         BNZ   *+12                                                             
         OI    STATFLAG,STFULLQ    'TEMPBLK' NOT BIG ENOUGH                     
         B     TSARN16                                                          
*                                                                               
TSARN14  CLI   DMCB,1              RECORD NOT FOUND (POSSIBLY ADDED)?           
         BE    TSARN15             YES                                          
         SR    RF,RF               NO, ADD TO EXISTING ENTRY                    
         ICM   RF,7,DMCB+1         RF=A(EXISTING ENTRY WITH SAME KEY)           
         AP    TEMPDR-TEMPD(L'TEMPDR,RF),TEMPDR ADD DEBITS TO ENTRY             
         AP    TEMPCR-TEMPD(L'TEMPCR,RF),TEMPCR ADD CREDITS TO ENTRY            
         B     TSARN16                                                          
*                                                                               
TSARN15  TM    STATFLAG,STFULLQ    IS 'TEMPBLK' FULL                            
         BO    TSARN16                                                          
         SR    RF,RF                                                            
         ICM   RF,15,DMCB+8                                                     
         STCM  RF,3,TEMPBLKN       NUMBER OF ENTRIES IN TABLE                   
*                                                                               
TSARN16  ZAP   DEBITS,=P'0'        INIT DEBITS AND CREDITS FOR CONTRA           
         ZAP   CREDITS,=P'0'                                                    
         MVC   CONTNAME,SPACES                                                  
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BNO   TSARN02                                                          
         OC    TEMPBLKN,TEMPBLKN   HAVE WE ANY TSAR RECORDS?                    
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS FOUND FOR ACCOUNT                 
         B     TSARNX                                                           
         DROP  R3                                                               
*                                                                               
TSARN18  SR    R4,R4                                                            
         ICM   R4,3,TEMPBLKN       R4=(NUM OF ENTRIES IN TEMPBLK)               
         CH    R4,TSLSTREC         IF SAME AS NUM IN TSAR THEN XIT              
         BE    TSARNX                                                           
         L     R5,ATEMPBLK         R5=A(TEMP BLOCK)                             
         SR    RF,RF                                                            
         ICM   RF,3,TSLSTREC       RF=(NO OF RECS IN TSAR)                      
         SR    R4,RF               R4=(NO OF RECS STILL TO PUT IN TSAR)         
         MH    RF,=Y(TEMPLNQ)                                                   
         LA    R5,0(RF,R5)         R5=A(NEXT TABLE ENTRY FOR TSAR)              
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSARDLNQ)                                                  
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         LA    R2,TSARDATA         R2=A(TSAR DATA)                              
*                                                                               
         USING TSARDATD,R2                                                      
TSARN20  MVC   TSARKYNO,TSCURRNO                                                
         MVC   TSDCONT,TEMPCONT    CONTRA CODE                                  
         MVC   TSDNAME,TEMPNAME    CONTRA NAME                                  
         ZAP   TSDDR,TEMPDR        DEBITS FOR CONTRA                            
         ZAP   TSDCR,TEMPCR        CREDITS FOR CONTRA                           
         MVI   TSDLINES,1          NUMBER OF SCREEN LINES USED BY REC           
         MVI   TSDFMT,TSDITM1      ITEM FORMAT TYPE                             
         GOTO1 ATSARADD            ADD RECORD                                   
         BNE   TSARN22                                                          
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         LA    R5,TEMPLNQ(R5)      BUMP R5 TO NEXT TABLE ENTRY                  
         BCT   R4,TSARN20                                                       
*                                                                               
TSARN22  TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BZ    TSARN30              NO                                          
         BAS   RE,BLDTOT                                                        
*                                                                               
TSARN30  MVC   TSCURRNO,=H'1'      SET CURRENT TSAR TO FIRST ONE                
*                                                                               
TSARNX   B     XIT                                                              
         DROP  R2,R3,R5                                                         
*                                                                               
***********************************************************************         
*        GET DETAILS FROM OFFICE/CONTRA BUCKETS IN AIO1               *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING OPTVALSD,R5                                                      
GETODET  NTR1                                                                   
         LA    R3,IOKEY            R3=KEY RECORD                                
         L     R5,AOPTVALS         R5=A(OPTION VALUES)                          
*                                                                               
         TM    STATFLAG,STOFBUK    READ CONTRA OFFICE BUCKETS?                  
         BZ    GETOX               . NO                                         
         CLC   TRNKOFF,SPACES      OFFICE?                                      
         BE    GETOX               . NO                                         
         CLC   TRNKCULC,SPACES     CONTRA ACCOUNT?                              
         BNH   GETOX               . NO                                         
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BH    GETO250             . YES                                        
         OC    CHDKNULL-CHDRECD(L'CHDKNULL,R3),CHDKNULL-CHDRECD(R3)             
         BZ    GETO025             CONTRA NAME RECORD?                          
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETOX               . YES, SKIP                                  
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BE    GETO150             CONTRA HISTORY RECORD                        
         B     GETOX               NOT A CONTRA HISTORY RECORD                  
*                                                                               
*---------------------------------------------------------------------*         
* GET DETAIL FROM CONTRA NAME                                                   
*---------------------------------------------------------------------*         
GETO025  CLI   OSUMM1,0                                                         
         BNE   GETOX                                                            
         GOTO1 AOFFOFF             CHECK FOR FILTERING                          
         BNE   GETOX                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1 READ NEXT RECORD                          
         BE    GETO030                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETOX                                                            
*                                                                               
GETO030  L     R3,AIO1                                                          
         USING CHDRECD,R3                                                       
         LA    R4,CHDRFST                                                       
         USING CACELD,R4                                                        
GETO035  CLI   CACEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CACEL,CACELQ        CONTRA ACCOUNT ELEMENT?                      
         BE    GETO040                                                          
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETO035                                                          
*                                                                               
GETO040  MVC   CONTOFF,CHDKOFF     GET OFFICE CODE                              
         MVC   CONTCODE,SPACES     CLEAR CONTRA ACCOUNT CODE                    
         LA    RE,CACCNTU                                                       
         LA    RF,L'CACCNTU+L'CACCNTL+L'CACCNTA-1                               
         CLI   CACCNTU,C' '                                                     
         BH    *+12                                                             
         LA    RE,CACCNTA                                                       
         LA    RF,L'CACCNTA-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTCODE(0),0(RE)   CONTRA CODE                                  
         MVC   CONTNAME,SPACES     CLEAR NAME                                   
         SR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    GETOX                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTNAME(0),CACNAME CONTRA ACCOUNT NAME                          
         B     GETOX                                                            
         DROP  R3,R4                                                            
*                                                                               
*---------------------------------------------------------------------*         
* GET DETAIL FROM CONTRA HISTORY RECORD                                         
*---------------------------------------------------------------------*         
* BUCKET TYPE, COSTING ACCOUNT *                                                
*------------------------------*                                                
         USING CACRECD,R3                                                       
GETO150  CLC   =C'1C',BASKEY       COSTING?                                     
         BNE   GETO152              NO                                          
         CLI   OBUKTYPE,C'1'       METHOD=?                                     
         BL    GETO152                                                          
         CLI   OBUKTYPE,C'9'       METHOD=?                                     
         BH    GETO152                                                          
         CLC   =C'14',CACKCUNT     1-9 VALID FOR 14, 15 AND 16 ONLY             
         BE    GETO152                                                          
         CLC   =C'15',CACKCUNT                                                  
         BE    GETO152                                                          
         CLC   =C'16',CACKCUNT                                                  
         BNE   GETO154                                                          
*                                                                               
GETO152  CLC   OBUKTYPE,CACKBTYP   DOES BUCKET MATCH?                           
         BNE   GETOX               NO, GET NEXT                                 
         CLI   OPAYTYPE,C' '       YES, ANY PAYTYPE?                            
         BE    *+14                NO                                           
         CLC   OPAYTYPE,CACKSTYP   YES, DOES IT MATCH SUBTYPE?                  
         BNE   GETOX               NO, GET NEXT                                 
*----------------------------------                                             
GETO154  GOTO1 AOFFOFF             CHECK FOR FILTERING                          
         BNE   GETOX                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1 READ RECORD                               
         BE    GETO155                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETOX                                                            
*                                                                               
GETO155  L     R3,AIO1                                                          
         MVC   CONTOFF,CACKOFF-CACRECD(R3)   GET OFFICE CODE                    
*                                                                               
         LA    R3,CACRFST          R3=A(FIRST ELEMENT ON RECORD)                
GETO160  CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETOX                                                            
         CLI   0(R3),PBKELQ        PREVIOUS BUCKET ELEMENT?                     
         BE    GETO180                                                          
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    GETO195                                                          
GETO175  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETO160                                                          
*                                                                               
*--------------------------------*                                              
* PREVIOUS BUCKET ELEMENT- X'55' *                                              
*--------------------------------*                                              
         USING PBKELD,R3                                                        
GETO180  OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETO190                                                          
         GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    GETO190                                                          
*                                                                               
         CLI   OMOSFI,NEGFILTR                                                  
         BE    GETO175                                                          
*                                                                               
         CLC   PBKHI,OMOSST                                                     
         BNL   GETO175                                                          
*                                                                               
         CLC   PBKHI,COMPFIN       YES, THEN NO BBF OUTSIDE FIN YEAR            
         BL    GETO175                                                          
*                                                                               
         CLI   SVACTNUM,ACTNSTOF   ADD TO BBF (BALFRWD) FOR ST AND JS           
         BNE   GETO188                                                          
         OI    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         AP    OBFTOT,PBKDR                                                     
         SP    OBFTOT,PBKCR                                                     
         B     GETO202                                                          
GETO188  AP    BALFRWD,PBKDR                                                    
         SP    BALFRWD,PBKCR                                                    
         B     GETO202                                                          
GETO190  AP    DEBITS,PBKDR        ADD TO DEBITS FOR CONTRA ACCOUNT             
         AP    CREDITS,PBKCR       ADD TO CREDITS FOR CONTRA ACCOUNT            
         B     GETO202                                                          
         DROP  R3                                                               
*                                                                               
*-----------------------*                                                       
* BUCKET ELEMENT- X'45' *                                                       
*-----------------------*                                                       
         USING BUKELD,R3                                                        
GETO195  OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETO200                                                          
         GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    GETO200                                                          
*                                                                               
         CLI   OMOSFI,NEGFILTR                                                  
         BE    GETO175                                                          
*                                                                               
         CLC   BUKMOS,OMOSST                                                    
         BNL   GETO175                                                          
*                                                                               
         CLC   BUKMOS,COMPFIN      YES, THEN NO BBF OUTSIDE FIN YEAR            
         BL    GETO175                                                          
*                                                                               
         CLI   SVACTNUM,ACTNSTOF   ADD TO BBF (BALFRWD) FOR ST AND JS           
         BNE   GETO198                                                          
         OI    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         AP    OBFTOT,BUKDR                                                     
         SP    OBFTOT,BUKCR                                                     
         B     GETO202                                                          
GETO198  AP    BALFRWD,BUKDR                                                    
         SP    BALFRWD,BUKCR                                                    
         B     GETO202                                                          
GETO200  AP    DEBITS,BUKDR        ADD TO DEBITS FOR CONTRA ACCOUNT             
         AP    CREDITS,BUKCR       ADD TO CREDITS FOR CONTRA ACCOUNT            
*                                                                               
GETO202  OI    DETFLAG,DETFOUND                                                 
         B     GETO175                                                          
*                                                                               
*---------------------------------------------------------------------*         
         USING TRNRECD,R3                                                       
GETO250  TM    TRNKSTAT,TRNSDRFT   DRAFT TRANSACTION?                           
         BZ    GETOX               . NO                                         
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    GETD050             . YES                                        
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETD050             . YES                                        
*                                                                               
GETOX    B     XIT                                                              
         DROP  R3,R5                                                            
*                                                                               
***********************************************************************         
*        GET DETAILS FROM AIO1                                        *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING OPTVALSD,R5                                                      
GETDET   NTR1                                                                   
         LA    R3,IOKEY            R3 = KEY RECORD                              
         L     R5,AOPTVALS         R5=A(OPTION VALUES)                          
*                                                                               
         TM    STATFLAG,STOFBUK    GET DETAIL FROM OFFICE BUCKETS?              
         BO    GETDTX                                                           
         TM    NXTRMODE,NXTRCACQ   READ CONTRA ACCOUNTS ONLY?                   
         BZ    GETD010              YES                                         
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BH    GETDTX               YES                                         
         CLC   TRNKCULC,SPACES     ACCOUNT/OFFICE RECORD?                       
         BNH   GETDTX               NO                                          
         CLC   TRNKOFF,SPACES      OFFICE REC?                                  
         BNE   GETDTX              YES                                          
         OC    CHDKNULL-CHDRECD(L'CHDKNULL,R3),CHDKNULL-CHDRECD(R3)             
         BZ    GETD025             CONTRA NAME RECORD?                          
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BE    GETD140             CONTRA HISTORY RECORD                        
         B     GETDTX                                                           
*                                                                               
GETD010  CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BH    GETD050             YES                                          
         CLC   TRNKCULC,SPACES     ACCOUNT/OFFICE REC?                          
         BH    GETD020             NO                                           
         CLC   TRNKOFF,SPACES      OFFICE REC?                                  
         BH    GETD110             YES                                          
         B     GETDTX                                                           
*                                                                               
GETD020  OC    CHDKNULL-CHDRECD(L'CHDKNULL,R3),CHDKNULL-CHDRECD(R3)             
         BNZ   GETDTX              CONTRA NAME RECORD?                          
         DROP  R3                  NO                                           
*                                                                               
*-----------------------------*                                                 
* GET DETAIL FROM CONTRA NAME *                                                 
*-----------------------------*                                                 
GETD025  CLI   OSUMM1,0                                                         
         BNE   GETDTX                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1 READ NEXT RECORD                          
         BE    GETD030                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETDTX                                                           
*                                                                               
GETD030  L     R3,AIO1                                                          
         USING CHDRECD,R3                                                       
         LA    R4,CHDRFST                                                       
*                                                                               
         USING CACELD,R4                                                        
GETD035  CLI   CACEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CACEL,CACELQ        CONTRA ACCOUNT ELEMENT?                      
         BE    GETD040                                                          
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETD035                                                          
*                                                                               
GETD040  MVC   CONTOFF,CHDKOFF     GET OFFICE CODE                              
         MVC   CONTCODE,SPACES     CLEAR CONTRA ACCOUNT CODE                    
         LA    RE,CACCNTU                                                       
         LA    RF,L'CACCNTU+L'CACCNTL+L'CACCNTA-1                               
         CLI   CACCNTU,C' '                                                     
         BH    *+12                                                             
         LA    RE,CACCNTA                                                       
         LA    RF,L'CACCNTA-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTCODE(0),0(RE)   CONTRA CODE                                  
         MVC   CONTNAME,SPACES     CLEAR NAME                                   
         SR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    GETDTX                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTNAME(0),CACNAME CONTRA ACCOUNT NAME                          
         B     GETDTX                                                           
         DROP  R3,R4                                                            
*                                                                               
*------------------------------------*                                          
* GET DETAIL FROM TRANSACTION RECORD *                                          
*------------------------------------*                                          
         USING TRNRECD,R3                                                       
GETD050  BAS   RE,FILTKEY          FILTER TRANSACTION KEY                       
         BNE   GETDTX              REJECT THE RECORD                            
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE RECORD                            
         BE    GETD060                                                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETDTX                                                           
*                                                                               
GETD060  L     R3,AIO1                                                          
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    GETD070             YES                                          
         TM    CORDFLAG,CORDMODQ   IF CONTRA ORDERED MODE SECURITY              
         BO    GETD070             ALREADY APPLIED                              
*                                                                               
         GOTO1 AOFFTRN             TRANSACTION SECURITY                         
         BNE   GETDTX                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETDTX                                                           
*                                                                               
GETD070  LA    R4,TRNRFST                                                       
*                                                                               
         USING TRNELD,R4                                                        
GETD072  CLI   TRNEL,EOR           END OF RECORD?                               
         BE    GETDTX                                                           
         CLI   TRNEL,TRNELQ        TRANSACTION ELEMENT?                         
         BE    GETD080                                                          
         CLI   TRNEL,SCIELQ        SUBSIDIARY ELEMENT?                          
         BE    GETD100                                                          
*                                                                               
GETD075  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETD072                                                          
*                                                                               
GETD080  OI    DETFLAG,DETFOUND                                                 
*                                                                               
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    GETD095             YES                                          
*                                                                               
         OC    OMOS,OMOS           MOA FILTER?                                  
         BZ    GETD095             NO                                           
         TM    STATFLAG,BBFTRN                                                  
         BZ    GETD095                                                          
*                                                                               
         CLI   SVACTNUM,ACTNSTOF   ADD TO BBF (BALFRWD) FOR ST AND JS           
         BE    GETD090                                                          
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+14                                                             
         AP    BALFRWD,TRNAMNT                                                  
         B     GETD075                                                          
         SP    BALFRWD,TRNAMNT                                                  
         B     GETD075                                                          
*                                                                               
GETD090  OI    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         TM    TRNSTAT,TRNSDR      ADD TO BBF (OBFTOT) FOR SF                   
         BNO   *+14                                                             
         AP    OBFTOT,TRNAMNT                                                   
         B     GETD075                                                          
         SP    OBFTOT,TRNAMNT                                                   
         B     GETD075                                                          
*                                                                               
GETD095  TM    TRNSTAT,TRNSDR      DEBIT OR CREDIT?                             
         BNO   *+14                                                             
         AP    DEBITS,TRNAMNT      ADD TO DEBITS FOR CONTRA ACCOUNT             
         B     GETD075                                                          
         AP    CREDITS,TRNAMNT     ADD TO CREDITS FOR CONTRA ACCOUNT            
         B     GETD075                                                          
*                                                                               
         USING SCIELD,R4           SUBSIDIARY CASH ELEMENT                      
GETD100  CLI   SCITYPE,SCITSJXP    AMOUNT POSTED TO EXPENSES?                   
         BNE   GETD075             NO                                           
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BNE   GETD075             NO                                           
         LA    RF,TRNRFST                                                       
         CLI   0(RF),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DETFLAG,DETFOUND                                                 
*                                                                               
         TM    TRNSTAT-TRNELD(RF),TRNSDR                                        
         BNO   *+14                                                             
         AP    DEBITS,SCIAMNT      ADD TO TOTAL FOR CONTRA ACCOUNT              
         B     *+10                                                             
         AP    CREDITS,SCIAMNT                                                  
         B     GETD075                                                          
*                                                                               
*-------------------------------*                                               
* GET DETAIL FROM OFFICE RECORD *                                               
*-------------------------------*                                               
GETD110  CLI   CONTLEN,0                                                        
         BE    GETDTX                                                           
         CLC   CLOSEDAT,COMPFIN    BBF BEFORE FINANCIAL YEAR?                   
         BL    GETDTX              YES                                          
         GOTO1 AOFFOFF            CHECK OFFICE FILTERING                        
         BNE   GETDTX                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE RECORD                            
         BE    GETD120                                                          
         TM    IOERR,IOMAX            MAX IOS REACHED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETDTX                                                           
*                                                                               
GETD120  L     R3,AIO1             SHOULD BE OFFICE RECORD                      
         MVC   CONTOFF,OFAKOFF-OFARECD(R3)   GET OFFICE CODE                    
*                                                                               
         LA    R3,TRNRFST          ADDRESS FIRST ELEMENT                        
GETD125  CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETDTX              YES                                          
         CLI   0(R3),ABLELQ        BALANCE ELEMENT?                             
         BE    GETD130             YES                                          
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETD125                                                          
*                                                                               
         USING ABLELD,R3                                                        
GETD130  CLI   OMOSFI,NEGFILTR     NEGATIVE FILTER?                             
         BE    GETD135             YES                                          
         CLC   CLOSEDAT,OMOSST     BALANCE BEFORE START MOA?                    
         BL    *+8                 YES                                          
GETD135  OI    STATFLAG,SHOWAST                                                 
         CLI   SVACTNUM,ACTNSTOF   ADD TO BBF (BALFRWD) FOR ST AND JS           
         BE    GETD138                                                          
         AP    BALFRWD,ABLFRWD                                                  
         B     GETDTX              GET NEXT                                     
GETD138  AP    OBFTOT,ABLFRWD      ADD TO BBF (OBFTOT) FOR SF                   
         OI    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         B     GETDTX                                                           
         DROP  R3                                                               
*                                                                               
*---------------------------------------*                                       
* GET DETAIL FROM CONTRA HISTORY RECORD *                                       
*---------------------------------------*                                       
* BUCKET TYPE, COSTING ACCOUNT *                                                
*------------------------------*                                                
         USING CACRECD,R3                                                       
GETD140  CLC   =C'1C',BASKEY       COSTING?                                     
         BNE   GETD146              NO                                          
         CLI   OBUKTYPE,C'1'       METHOD=?                                     
         BL    GETD146                                                          
         CLI   OBUKTYPE,C'9'       METHOD=?                                     
         BH    GETD146                                                          
         CLC   =C'14',CACKCUNT     1-9 VALID FOR 14, 15 AND 16 ONLY             
         BE    GETD146                                                          
         CLC   =C'15',CACKCUNT                                                  
         BE    GETD146                                                          
         CLC   =C'16',CACKCUNT                                                  
         BNE   GETD148                                                          
*                                                                               
GETD146  CLC   OBUKTYPE,CACKBTYP   DOES BUCKET MATCH?                           
         BNE   GETDTX              NO, GET NEXT                                 
         CLI   OPAYTYPE,C' '       YES, ANY PAYTYPE?                            
         BE    *+14                NO                                           
         CLC   OPAYTYPE,CACKSTYP   YES, DOES IT MATCH SUBTYPE?                  
         BNE   GETDTX              NO, GET NEXT                                 
         DROP  R3                                                               
*----------------------------------------------------------------------         
*                                                                               
         USING CACRECD,R4                                                       
GETD148  GOTO1 AIO,IOGET+IOACCMST+IO1 READ RECORD                               
         BE    GETD155                                                          
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     GETDTX                                                           
*                                                                               
GETD155  L     R4,AIO1                                                          
         LA    R3,CACRFST          R3=A(FIRST ELEMENT ON RECORD)                
*                                                                               
GETD160  CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETDTX                                                           
         CLI   0(R3),PBKELQ        PREVIOUS BUCKET ELEMENT?                     
         BE    GETD180                                                          
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    GETD195                                                          
*                                                                               
GETD175  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETD160                                                          
*                                                                               
*--------------------------------*                                              
* PREVIOUS BUCKET ELEMENT- X'55' *                                              
*--------------------------------*                                              
         USING PBKELD,R3                                                        
GETD180  OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETD190                                                          
         GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    GETD190                                                          
*                                                                               
         CLI   OMOSFI,NEGFILTR                                                  
         BE    GETD175                                                          
         CLC   PBKHI,OMOSST                                                     
         BNL   GETD175                                                          
         CLC   PBKHI,COMPFIN       BBF BEFORE FINANCIAL YEAR?                   
         BL    GETD175             YES                                          
         OI    DETFLAG,DETFOUND                                                 
         AP    BALFRWD,PBKDR                                                    
         SP    BALFRWD,PBKCR                                                    
         B     GETD175                                                          
*                                                                               
GETD190  OI    DETFLAG,DETFOUND                                                 
         AP    DEBITS,PBKDR        ADD TO DEBITS FOR CONTRA ACCOUNT             
         AP    CREDITS,PBKCR       ADD TO CREDITS FOR CONTRA ACCOUNT            
         B     GETD175                                                          
         DROP  R3                                                               
*                                                                               
*-----------------------*                                                       
* BUCKET ELEMENT- X'45' *                                                       
*-----------------------*                                                       
         USING BUKELD,R3                                                        
GETD195  OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETD200                                                          
         GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    GETD200                                                          
*                                                                               
         CLI   OMOSFI,NEGFILTR                                                  
         BE    GETD175                                                          
         CLC   BUKMOS,OMOSST                                                    
         BNL   GETD175                                                          
         CLC   BUKMOS,COMPFIN      BBF BEFORE FINANCIAL YEAR?                   
         BL    GETD175             YES                                          
         OI    DETFLAG,DETFOUND                                                 
         AP    BALFRWD,BUKDR                                                    
         SP    BALFRWD,BUKCR                                                    
         B     GETD175                                                          
*                                                                               
GETD200  OI    DETFLAG,DETFOUND                                                 
         AP    DEBITS,BUKDR        ADD TO DEBITS FOR CONTRA ACCOUNT             
         AP    CREDITS,BUKCR       ADD TO CREDITS FOR CONTRA ACCOUNT            
         B     GETD175                                                          
*                                                                               
*---------------------------------------------------------------------*         
GETDTX   B     XIT                                                              
         DROP  R3,R4,R5                                                         
*                                                                               
***********************************************************************         
*        READ WORKCODE RECORD                                         *         
***********************************************************************         
         USING WCORECD,R3                                                       
GETWC    NTR1                                                                   
         MVC   CONTCODE(L'TRNKWORK),TRNKWORK+IOKEYSAV-TRNRECD                   
         LA    R3,IOKEY            GET THE WORKCODE RECORD                      
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,MYCO                                                     
         MVC   WCOKUNT(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         MVC   WCOKWRK,CONTCODE                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    GETW02                                                           
         TM    IOERR,IOERNF                                                     
         BO    GETW08                                                           
         TM    IOERR,IOMAX         RECORD NOT FOUND                             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
*                                                                               
GETW02   L     R3,AIO2             R3=A(WORKCODE RECORD)                        
         LA    R4,WCORFST                                                       
*                                                                               
GETW04   CLI   0(R4),EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),WCOELQ        WORK CODE ELEMENT?                           
         BE    GETW06                                                           
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R0,R4                                                            
         B     GETW04                                                           
*                                                                               
         USING WCOELD,R4                                                        
GETW06   MVC   CONTNAME(L'WCODESC),WCODESC-WCOELD(R4) WC DESCRIPTION            
*                                                                               
GETW08   MVC   IOKEY,KEYSAVE       REESTABLISH SEQ                              
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    GETWCX                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
*                                                                               
GETWCX   B     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
***********************************************************************         
*        FILTER TRANSACTION INDEX RECORD (ACCDIR)                     *         
* ON ENTRY R3=A(TRANSACTION RECORD)                                   *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
         USING TRNRECD,R3                                                       
FILTKEY  NTR1                                                                   
         L     R2,AOPTVALS                                                      
         CLC   TRNKDATE,SPACES                                                  
         BNH   FILTKRJX                                                         
*                                                                               
         NI    STATFLAG,X'FF'-BBFTRN     TRAN NEEDS TO GO IN BBF                
*                                                                               
         TM    TRNKSTA2,TRNSPEEL   PEELED TRANSACTION?                          
         BO    FILTKRJX            YES, REJECT IT                               
         TM    TRNKSTAT,TRNSDRFT   DRAFT TRANSACTION?                           
         BNO   FILTK02             NO                                           
         CLI   ODRAFT,C'Y'         YES, INCLUDE DRAFTS?                         
         BE    FILTK03             YES, KEEP IT                                 
         CLI   ODRAFT,C'O'         NO, DRAFTS ONLY?                             
         BE    FILTK03             YES, KEEP IT                                 
         B     FILTKRJX            NO, REJECT                                   
FILTK02  CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    FILTKRJX            YES, REJECT THIS ONE                         
*                                                                               
FILTK03  CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BNE   FILTK06             NO, SKIP THIS                                
         OC    OWCODE,OWCODE       WORKCODE FILTERING?                          
         BZ    FILTK06             NO                                           
*                                                                               
         SR    RF,RF               YES, FILTER WORKCODE                         
         IC    RF,OWCODELN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   OWCODEVL(0),TRNKWORK                                             
         BE    FILTK04                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILTKRJX                                                         
         B     FILTK06                                                          
*                                                                               
FILTK04  CLI   OWCODEFI,NEGFILTR                                                
         BE    FILTKRJX                                                         
*                                                                               
FILTK06  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK08             NO                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK08  CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    FILTK10             YES                                          
         CLC   TRNKSMOS,COMPFIN    NO, BEFORE FISCAL START?                     
         BL    FILTKRJX            YES, REJECT                                  
*                                                                               
FILTK10  OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILTKX              NO, DONE                                     
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BE    FILTKX                                                           
         CLI   OMOSFI,NEGFILTR     NEGATIVE FILTER?                             
         BE    FILTKRJX                                                         
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    FILTKRJX            YES                                          
         OC    ENDBBF,ENDBBF       SHOWING BAL BFWD?                            
         BZ    FILTKRJX            . NO                                         
         CLC   TRNKSMOS,OMOSST                                                  
         BNL   FILTKRJX                                                         
         CLC   TRNKSMOS,COMPFIN    YES, THEN NO BBF OUTSIDE FIN YEAR            
         BL    FILTKRJX                                                         
         OI    STATFLAG,BBFTRN     TRAN NEEDS TO GO IN BBF                      
*                                                                               
FILTKX   CR    RB,RB               KEEP RECORD                                  
         B     XIT                                                              
*                                                                               
FILTKRJX LTR   RB,RB               REJECT RECORD                                
         B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
*        ON ENTRY STORAGE CONTAINS INFO                               *         
*          CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
BLDTSDAT NTR1                                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         LH    RF,=Y(TSARDLNQ)                                                  
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVI   TSDFMT,TSDITM1      SCREEN DATA ITEM 1                           
         MVC   TSDOFF,CONTOFF      OFFICE CODE FOR 2 CHAR OFFICE                
         MVC   TSDCONT,CONTCODE    CONTRA CODE                                  
         MVC   TSDNAME,CONTNAME    CONTRA NAME                                  
         ZAP   TSDDR,DEBITS                                                     
         ZAP   TSDCR,CREDITS                                                    
*                                                                               
         CLI   SVACTNUM,ACTNSTOF                                                
         BNE   BLDT02                                                           
         AP    ODRTOT,TSDDR                                                     
         AP    OCRTOT,TSDCR                                                     
*                                                                               
BLDT02   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
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
         CR    RB,RB                                                            
BLDTX    B     XIT                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*        BUILD OFFICE TOTAL ITEM IN TSAR                              *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         USING OPTVALSD,R5                                                      
BLDOFTOT NTR1                                                                   
         L     R5,AOPTVALS                                                      
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         LH    RF,=Y(TSAROLNQ)                                                  
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVI   TSDFMT,TSOFFITM     SCREEN DATA ITEM 2                           
         MVC   TSDOFF,CONTOFF      OFFICE CODE FOR 2 CHAR OFFICE                
         MVC   TSDCONT(L'TSDCONT+L'TSDNAME),SPACES                              
         MVC   TSDNAME(L'TSDOFF),TSDOFF                                         
         MVC   TSDNAME+L'TSDOFF+1(L'MX@TOTAL),MX@TOTAL                          
         ZAP   TSOBF,OBFTOT                                                     
         ZAP   TSDDR,ODRTOT                                                     
         ZAP   TSDCR,OCRTOT                                                     
         AP    BALFRWD,TSOBF                                                    
*                                                                               
         OC    ENDBBF,ENDBBF                                                    
         BZ    BLDO10                                                           
         MVC   TSDNAME+10(L'MX@BFW),MX@BFW                                      
         MVI   TSDNAME+10+L'MX@BFW,C'='                                         
         LA    R3,TSDNAME+10+L'MX@BFW+1                                         
         CURED TSOBF,(12,(R3)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES                  
*                                                                               
BLDO10   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
         ZAP   OBFTOT,=P'0'        BALANCE BFWD                                 
         NI    STATFLAG,X'FF'-OFFBBFT                                           
         ZAP   ODRTOT,=P'0'        DEBITS                                       
         ZAP   OCRTOT,=P'0'        CREDITS                                      
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+10                                                             
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDOFX                                                           
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
         CR    RB,RB                                                            
BLDOFX   B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        BUILD TOTAL LINE FOR TSAR                                    *         
***********************************************************************         
BLDTOT   NTR1                                                                   
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
         STCM  RF,3,TSARLEN        LENGTH OF TSAR RECORD                        
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
*                                                                               
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,DISTOT                                                        
*                                                                               
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         BNE   BLDTOTX                                                          
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
BLDTOTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR SCAN BLOCK                             
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         L     R4,ATSARREC                                                      
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
*                                                                               
         CLI   TSDFMT,TSOFFITM     OFFICE TOTAL ITEM?                           
         BE    FORMT14                                                          
         CLI   TSDFMT,TSDITM1      DETAIL ITEM?                                 
         BE    FORMT01                                                          
         CLI   TSDFMT,TSTOTITM                                                  
         BNE   FORMTX                                                           
         BAS   RE,DISTOT                                                        
         B     FORMTX                                                           
*                                                                               
FORMT01  PACK  TEMP(L'TSDDR),NINES(L'SCR1DR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT02                                                          
         MP    TEMP(L'TSDDR),=P'-1'                                             
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT02                                                          
         CURED (P8,TSDDR),(L'SCR1DR,SCR1DR),2,MINUS=YES                         
         B     FORMT04                                                          
FORMT02  CURED (P8,TSDDR),(L'SCR1DR,SCR1DR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT04  PACK  TEMP(L'TSDCR),NINES(L'SCR1CR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT06                                                          
         MP    TEMP(L'TSDCR),=P'-1'                                             
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT06                                                          
         CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES                         
         B     FORMT08                                                          
FORMT06  CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT08  MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         CLI   SVACTNUM,ACTNSTOF   STATEMENT BY OFFICE                          
         BE    FORMT10                                                          
         MVC   SCR1CON1,TSDCONT    CONTRA CODE                                  
         MVC   SCR1NAM1,TSDNAME         CONTRA NAME                             
         B     FORMT12                                                          
*                                                                               
FORMT10  MVC   SCR1OFF,TSDOFF      OFFICE CODE                                  
         MVC   SCR1CON2,TSDCONT    CONTRA CODE                                  
         MVC   TEMP,SPACES                                                      
         GOTO1 VCHOPPER,DMCB,(L'TSDNAME,TSDNAME),(L'SCR1NAM2,TEMP),2            
         MVC   SCR1NAM2,TEMP       GET FIRST NAME LINE                          
         CLC   TEMP+L'SCR1NAM2(L'SCR1NAM2),SPACES                               
         BNH   FORMT12                                                          
         LA    R2,L'DUMLIN1(R2)                                                 
         MVC   SCR1NAM2,TEMP+L'SCR1NAM2 GET SECOND NAME LINE                    
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
*                                                                               
FORMT12  ZAP   DEBITS,TSDDR                                                     
         ZAP   CREDITS,TSDCR                                                    
         B     FORMTX                                                           
*                                                                               
FORMT14  LA    RF,SCR1NAM2         RF=(NAME FIELD USED FOR OFF TOT)             
         MVC   0(L'MX@OFFC,RF),MX@OFFC OFFICE                                   
         LA    RF,L'MX@OFFC-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'TSDOFF,RF),TSDOFF DISPLAY OFFICE CODE                        
*                                                                               
         L     RE,AOPTVALS                                                      
         USING OPTVALSD,RE                                                      
*                                                                               
         OC    ENDBBF,ENDBBF                                                    
         BZ    FORMT16                                                          
         TM    STATFLAG,SHOWAST         BBF FROM PEELED?                        
         BO    *+14                     YES, CONTINUE                           
         OC    OMOSST,OMOSST            NO, START DATE?                         
         BZ    FORMT16                  NO, ZERO BBF NOT NEEDED                 
         DROP  RE                                                               
*                                                                               
         MVC   5(L'MX@BALB9,RF),MX@BALB9 BAL B/FWD                              
         LA    RF,L'MX@BALB9-1+5(RF)                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CURED (P8,TSOBF),(16,2(RF)),2,MINUS=YES,ALIGN=LEFT                     
*                                                                               
         TM    STATFLAG,SHOWAST                                                 
         BNO   FORMT16                                                          
         LA    RF,SCR1NAM2+L'SCR1NAM2-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'*'                                                       
*                                                                               
FORMT16  PACK  TEMP(L'TSDDR),NINES(L'SCR1DR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT18                                                          
         MP    TEMP(L'TSDDR),=P'-1'                                             
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT18                                                          
         CURED (P8,TSDDR),(L'SCR1DR,SCR1DR),2,MINUS=YES                         
         B     FORMT20                                                          
FORMT18  CURED (P8,TSDDR),(L'SCR1DR,SCR1DR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT20  PACK  TEMP(L'TSDCR),NINES(L'SCR1CR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT22                                                          
         MP    TEMP(L'TSDCR),=P'-1'                                             
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT22                                                          
         CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES                         
         B     FORMT24                                                          
FORMT22  CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT24  MVI   LINSUSED,1                                                       
         MVI   DISATRIB,HILIGHTQ                                                
*                                                                               
FORMTX   B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR SCAN BLOCK                             
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ATSARREC                                                      
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         TM    DETFLAG,DETGRINQ                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB          DISPLAY DUMMY SCREEN LINES            
         OI    DETFLAG,DETGRINQ                                                 
*                                                                               
         CLI   SVACTNUM,ACTNJBST          TYPE = JS?                            
         BNE   FGRM10                     . NO                                  
         SR    R1,R1                      FORCE DISLINE FOR JS                  
         IC    R1,GRXTLNS                                                       
         AHI   R1,7                       DATA STARTS SEVEN LINES DOWN          
         L     RE,AOPTVALS                                                      
         CLI   OSUMM1-OPTVALSD(RE),0      WORKCODE SUMMARY?                     
         BE    *+8                        . NO                                  
         AHI   R1,-1                      . YES, THEN SIX LINES                 
         STC   R1,DISLINE                                                       
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSDFMT,TSTOTITM            TOTAL ITEM TYPE                       
         BE    FGRM30                                                           
         GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY PRESENT BALANCE LINE AND OTHERS LINE                 *         
***********************************************************************         
DISBAL   NTR1                                                                   
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    DISBX                                                            
*                                                                               
         NI    FORMFLAG,X'FF'-(OTHERLNQ+FLTDISPQ)                               
         TM    STATFLAG,STSUPPT    SUPPRESS PER BAL TOT?                        
         BZ    *+8                 NO                                           
         OI    FORMFLAG,FLTDISPQ   LIMITED BALANCE DISPLAY DUE TO FILT          
*                                                                               
         TM    STATFLAG,OFFBBFT    OFFICE BAL FRWD?                             
         BO    DISB06                                                           
         CP    ODRTOT,=P'0'        OFFICE DEBIT TOTALS?                         
         BNE   DISB06                                                           
         CP    OCRTOT,=P'0'        OFFICE CREDIT TOTALS?                        
         BNE   DISB06                                                           
         TM    DISPFLAG,ALLREADQ   HAVE ALL RECORD BEEN READ?                   
         BZ    DISB06                                                           
         TM    DISPFLAG,NORECQ                                                  
         BO    DISB04                                                           
         CLC   TSCURRNO,TSLSTREC   ARE WE DEALING WITH LAST RECORD?             
         BL    DISB06                                                           
DISB04   OI    FORMFLAG,NOMOREQ    YES, SWITCH 'NO MORE' ON                     
         TM    FORMFLAG,STARTQ     LAST REC ON 1ST SCREEN FORGET OTHER          
         BO    DISB08                                                           
DISB06   TM    FORMFLAG,FLTDISPQ                                                
         BO    DISB08                                                           
         OI    FORMFLAG,OTHERLNQ   'OTHERS' LINE TO BE DISPLAYED                
*                                                                               
DISB08   LA    R2,ENQDATLH        R2=A(LAST LINE)                               
         SHI   R2,(ENQDAT2H-ENQDAT1H)                                           
         USING FLDHDRD,R2                                                       
         LA    R3,FLDDATA         R3=A(NEXT TO LAST)                            
         USING SCRBAL1D,R3                                                      
         TM    FORMFLAG,FLTDISPQ                                                
         BO    DISB22                                                           
*                                                                               
*---------------------*                                                         
* DISPLAY OTHERS LINE *                                                         
*---------------------*                                                         
         TM    FORMFLAG,OTHERLNQ  ARE WE DISPLAYING 'OTHERS' LINE?              
         BNO   DISB22                                                           
         NI    FORMFLAG,X'FF'-OTHERLNQ SWITCH OFF 'OTHERS' LINE FLAG            
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   SCRBDES1,MX@OTHRS   OTHERS                                       
         ZAP   DIFFO,DEBTOT                                                     
         SP    DIFFO,DISPLDDR      OTHER DEBITS                                 
*                                                                               
         PACK  TEMP(L'DIFFO),NINES(L'SCRBDB-2) GET STRING OF NINES              
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TOO HIGH TO FIT?                   
         BL    DISB14                                                           
         MP    TEMP(L'DIFFO),=P'-1'                                             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TO LOW TO FIT?                     
         BH    DISB14                                                           
         CURED (P8,DIFFO),(L'SCRBDB,SCRBDB),2,MINUS=YES                         
         B     DISB16                                                           
DISB14   CURED (P8,DIFFO),(L'SCRBDB,SCRBDB),2,MINUS=YES,DECS=ROUND              
*                                                                               
DISB16   ZAP   DIFFO,CRETOT                                                     
         SP    DIFFO,DISPLDCR      OTHER CREDITS                                
         PACK  TEMP(L'DIFFO),NINES(L'SCRBCR-2) GET STRING OF NINES              
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TOO HIGH TO FIT?                   
         BL    DISB18                                                           
         MP    TEMP(L'DIFFO),=P'-1'                                             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TO LOW TO FIT?                     
         BH    DISB18                                                           
         CURED (P8,DIFFO),(L'SCRBCR,SCRBCR),2,MINUS=YES                         
         B     DISB20                                                           
DISB18   CURED (P8,DIFFO),(L'SCRBCR,SCRBCR),2,MINUS=YES,DECS=ROUND              
*                                                                               
DISB20   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R3,ENQDAT2H-ENQDAT1H(R3) BUMP R3 TO NEXT LINE                    
         LA    R2,ENQDAT2H-ENQDAT1H(R2) BUMP R2 TO NEXT LINE                    
*                                                                               
DISB22   MVC   FLDDATA(L'ENQDAT1),SPACES                                        
*                                                                               
         CLI   SVACTNUM,ACTNJBST     JOB STATEMENT?                             
         BNE   DISB24                                                           
         ZAP   BALTOT,DEBTOT                                                    
         SP    BALTOT,CRETOT                                                    
         MVC   SCRBDES1,MX@ACCDF   ACCOUNT DIFFERENCE                           
         B     DISB30                                                           
*                                                                               
DISB24   TM    FORMFLAG,FLTDISPQ                                                
         BO    DISB42                                                           
         TM    DISPFLAG,ALLREADQ                                                
         BZ    DISB26                                                           
         CLC   TSCURRNO,TSLSTREC                                                
         BNL   DISB42                                                           
*                                                                               
DISB26   MVC   SCRBDES1,MX@PRSB    PRESENT BALANCE                              
         ZAP   BALTOT,DEBTOT                                                    
         SP    BALTOT,CRETOT                                                    
*                                                                               
DISB30   PACK  TEMP(L'BALTOT),NINES(L'SCRBBAL-2) GET STRING OF NINES            
         CP    TEMP(L'BALTOT),BALTOT IS AMOUNT TOO HIGH TO FIT?                 
         BL    DISB32                                                           
         MP    TEMP(L'BALTOT),=P'-1'                                            
         CP    TEMP(L'BALTOT),BALTOT IS AMOUNT TO LOW TO FIT?                   
         BH    DISB32                                                           
         CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES                      
         B     DISB34                                                           
DISB32   CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES,DECS=ROUND           
*                                                                               
DISB34   PACK  TEMP(L'DEBTOT),NINES(L'SCRBDB-2) GET STRING OF NINES             
         CP    TEMP(L'DEBTOT),DEBTOT IS AMOUNT TOO HIGH TO FIT?                 
         BL    DISB36                                                           
         MP    TEMP(L'DEBTOT),=P'-1'                                            
         CP    TEMP(L'DEBTOT),DEBTOT IS AMOUNT TO LOW TO FIT?                   
         BH    DISB36                                                           
         CURED (P8,DEBTOT),(L'SCRBDB,SCRBDB),2,MINUS=YES                        
         B     DISB38                                                           
DISB36   CURED (P8,DEBTOT),(L'SCRBDB,SCRBDB),2,MINUS=YES,DECS=ROUND             
*                                                                               
DISB38   PACK  TEMP(L'CRETOT),NINES(L'SCRBCR-2) GET STRING OF NINES             
         CP    TEMP(L'CRETOT),CRETOT IS AMOUNT TOO HIGH TO FIT?                 
         BL    DISB40                                                           
         MP    TEMP(L'CRETOT),=P'-1'                                            
         CP    TEMP(L'CRETOT),CRETOT IS AMOUNT TO LOW TO FIT?                   
         BH    DISB40                                                           
         CURED (P8,CRETOT),(L'SCRBCR,SCRBCR),2,MINUS=YES                        
         B     DISB42                                                           
DISB40   CURED (P8,CRETOT),(L'SCRBCR,SCRBCR),2,MINUS=YES,DECS=ROUND             
*                                                                               
DISB42   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
DISBX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY PRESENT BALANCE/TOTAL LINE AND OTHERS LINE           *         
***********************************************************************         
DISTOT   NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0             NUMBER OF LINES DISPLAYED                 
         MVI   DISATRIB,0             DISPLAY ATTRIBUTES                        
         L     R0,ADUMLINE            CLEAR SCAN BLOCK                          
         LHI   R1,L'DUMLIN1*NDUMLINE                                            
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE            R2=A(FIRST DUMMY SCREEN LINE)             
         USING SCRBAL1D,R2                                                      
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
*--------------------------*                                                    
* HANDLE MOA RANGE BALANCE *                                                    
*--------------------------*                                                    
         ZAP   BALTOT,DEBTOT                                                    
         SP    BALTOT,CRETOT                                                    
         MVC   SCRBDES1(L'MX@PRSB),MX@PRSB        TOTAL                         
*                                                                               
         PACK  TEMP(L'BALTOT),NINES(L'SCRBBAL-2) GET STRING OF NINES            
         CP    TEMP(L'BALTOT),BALTOT            AMOUNT TOO HIGH TO FIT?         
         BL    DIST12                                                           
         MP    TEMP(L'BALTOT),=P'-1'                                            
         CP    TEMP(L'BALTOT),BALTOT            AMOUNT TO LOW TO FIT?           
         BH    DIST12                                                           
         CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES                      
         B     DIST14                                                           
DIST12   CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES,DECS=ROUND           
*                                                                               
DIST14   PACK  TEMP(L'DEBTOT),NINES(L'SCRBDB-2) GET STRING OF NINES             
         CP    TEMP(L'DEBTOT),DEBTOT            AMOUNT TOO HIGH TO FIT?         
         BL    DIST16                                                           
         MP    TEMP(L'DEBTOT),=P'-1'                                            
         CP    TEMP(L'DEBTOT),DEBTOT            AMOUNT TO LOW TO FIT?           
         BH    DIST16                                                           
         CURED (P8,DEBTOT),(L'SCRBDB,SCRBDB),2,MINUS=YES                        
         B     DIST18                                                           
DIST16   CURED (P8,DEBTOT),(L'SCRBDB,SCRBDB),2,MINUS=YES,DECS=ROUND             
*                                                                               
DIST18   PACK  TEMP(L'CRETOT),NINES(L'SCRBCR-2) GET STRING OF NINES             
         CP    TEMP(L'CRETOT),CRETOT            AMOUNT TOO HIGH TO FIT?         
         BL    DIST20                                                           
         MP    TEMP(L'CRETOT),=P'-1'                                            
         CP    TEMP(L'CRETOT),CRETOT            AMOUNT TO LOW TO FIT?           
         BH    DIST20                                                           
         CURED (P8,CRETOT),(L'SCRBCR,SCRBCR),2,MINUS=YES                        
         B     DIST25                                                           
DIST20   CURED (P8,CRETOT),(L'SCRBCR,SCRBCR),2,MINUS=YES,DECS=ROUND             
*                                                                               
DIST25   LA    R2,(2*L'DUMLIN1)(R2)                                             
         MVI   LINSUSED,3                                                       
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    DISTX               NO                                           
*                                                                               
*--------------------------------*                                              
* HANDLE BALANCE BROUGHT FORWARD *                                              
*--------------------------------*                                              
         L     RE,AOPTVALS                                                      
         USING OPTVALSD,RE                                                      
         OC    ENDBBF,ENDBBF                                                    
         BZ    DIST50                                                           
         TM    STATFLAG,SHOWAST       BBF FROM PEELED?                          
         BO    *+14                    YES, CONTINUE                            
         OC    OMOSST,OMOSST           NO, START DATE?                          
         BZ    DIST50                  NO, ZERO BBF NOT NEEDED                  
*                                                                               
         DROP  RE                                                               
         MVC   SCRBDES1(L'MX@BALBF),MX@BALBF  BALANCE BROUGHT FORWARD           
*                                                                               
         ZAP   BALTOT,BALFRWD                                                   
         PACK  TEMP(L'BALTOT),NINES(L'SCRBBAL-2) GET STRING OF NINES            
         CP    TEMP(L'BALTOT),BALTOT            AMOUNT TOO HIGH TO FIT?         
         BL    DIST30                                                           
         MP    TEMP(L'BALTOT),=P'-1'                                            
         CP    TEMP(L'BALTOT),BALTOT            AMOUNT TO LOW TO FIT?           
         BH    DIST30                                                           
         CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES                      
         B     DIST32                                                           
DIST30   CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES,DECS=ROUND           
*                                                                               
DIST32   LA    R2,(2*L'DUMLIN1)(R2)                                             
         SR    R0,R0                                                            
         IC    R0,LINSUSED                                                      
         AHI   R0,2                                                             
         STC   R0,LINSUSED                                                      
*                                                                               
*---------------*                                                               
* TOTAL BALANCE *                                                               
*---------------*                                                               
DIST40   AP    BALTOT,DEBTOT                                                    
         SP    BALTOT,CRETOT                                                    
*                                                                               
DIST50   MVC   SCRBDES1,MX@ACCDF            ACCOUNT DIFFERENCE                  
*                                                                               
         PACK  TEMP(L'BALTOT),NINES(L'SCRBBAL-2) GET STRING OF NINES            
         CP    TEMP(L'BALTOT),BALTOT        AMOUNT TOO HIGH TO FIT?             
         BL    DIST54                                                           
         MP    TEMP(L'BALTOT),=P'-1'                                            
         CP    TEMP(L'BALTOT),BALTOT        AMOUNT TO LOW TO FIT?               
         BH    DIST54                                                           
         CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES                      
         B     DIST56                                                           
DIST54   CURED (P8,BALTOT),(L'SCRBBAL,SCRBBAL),2,MINUS=YES,DECS=ROUND           
*                                                                               
DIST56   SR    R0,R0                                                            
         IC    R0,LINSUSED                                                      
         AHI   R0,1                                                             
         STC   R0,LINSUSED                                                      
*                                                                               
DISTX    B     XIT                                                              
         DROP  R2                                                               
*                                                                               
*********************************************************************           
*        CHECK CONTRA FILTER                                                    
*********************************************************************           
         USING CHDRECD,R3                                                       
CKCONFLT NTR1                      CHECK CONTRA FILTER                          
*                                                                               
         LA    R3,IOKEY            R3=A(IO AREA CONTAINING RECORD)              
         CLI   CONTLEN,0                                                        
         BE    OKXIT                                                            
         MVC   WORK,SPACES                                                      
         LA    RF,L'CHDKULC        RF=MAX LENGTH OF CONTRA ACCOUNT              
         LA    RE,CHDKULC          RE=POSITION IN CONTRA ACCOUNT                
         CLI   0(RE),C' '          IF SPACE THEN BUMP                           
         BH    CKCF10                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     CKCF20                                                           
CKCF10   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       PULL IN CONTRA AFTER SPACES                  
         SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        LENGTH OF CONTRA CODE                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),CONTRA      MATCHES CONTRA?                              
         BNE   CKCF20                                                           
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    ERRXIT                                                           
         B     OKXIT                                                            
CKCF20   CLI   NEGCONT,NEGFILTR       NEGATIVE FILTER?                          
         BNE   ERRXIT                 FILTER OUT                                
         B     OKXIT                  GOOD                                      
         DROP  R3                                                               
*                                                                               
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
*                                                                               
***********************************************************************         
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
NINES    DC    C'99999999999999999999'                                          
HYPHENS  DC    C'--------------------'                                          
ONEP     DC    C'1P'                                                            
ONER     DC    C'1R'                                                            
ONEJ     DC    C'1J'                                                            
SINCUL   DC    C'SI'                                                            
GPROUL   DC    C'GP'                                                            
*                                                                               
MAXTEMP  EQU   (L'TEMPBLK/TEMPLNQ)                                              
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH11,78                                                      
         DCDDL AC#ENH12,78                                                      
         DCDDL AC#ENH30,78                                                      
         DCDDL AC#ENH31,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#XJOB,11                                                       
         DCDDL AC#STCON,14                                                      
         DCDDL AC#BALBF,15                                                      
         DCDDL AC#PRSB,15                                                       
         DCDDL AC#OTHRS,15                                                      
         DCDDL AC#ACCDF,15                                                      
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#MOA,3                                                         
         DCDDL AC#OFFC,6                                                        
         DCDDL AC#BALBF,9                                                       
         DCDDL AC#BALBF,3                                                       
         DCDDL AC#WC,2                                                          
         DCDDL AC#DFRNC,10                                                      
         DCDDL AC#NAME,L'MX@NAME                                                
         DC    AL1(EOT)                                                         
*                                                                               
         DROP  RB,R7,R6                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CALCULATE DATA FOR GRIDS                                     *         
*         ON INPUT:                                                   *         
*             R3 = GRID TABLE                                         *         
***********************************************************************         
         USING GCTBLD,R3                                                        
GRIDAT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)         R8=A(LOCAL WORKING STORAGE)                 
         L     R4,ATSARREC                                                      
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA          R4=A(TSAR DATA)                             
         USING TSARDATD,R4                                                      
         SR    R1,R1                                                            
*---------------                                                                
* OFFICE COLUMN                                                                 
*---------------                                                                
         CLI   GCTCOID,GCOFFQ       OFFICE?                                     
         BNE   GRID20               . NO                                        
         MVI   TEMP,C' '            . YES, THEN FIRST IN ROW                    
         CLI   TSDFMT,TSOFFITM      OFFICE TOTAL (SUB-TOTAL LINE)               
         BNE   GRIDA5               . NO                                        
         MVI   TEMP,C'S'                                                        
         MVC   TEMP+L'TSDOFF+2(L'MX@TOTAL),MX@TOTAL                             
         AHI   R1,L'MX@TOTAL+1                                                  
GRIDA5   MVC   TEMP+1(L'TSDOFF),TSDOFF                                          
         AHI   R1,L'TSDOFF+1                                                    
         B     GRIDX                                                            
*-------------------                                                            
* DIFFERENCE COLUMN                                                             
*-------------------                                                            
GRID20   CLI   GCTCOID,GCDIFQ      DIFFERENCE?                                  
         BNE   GRIDX                 NO                                         
         ZAP   DIFFO,TSDDR                                                      
         SP    DIFFO,TSDCR                                                      
         CURED (P8,DIFFO),(16,TEMP),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES             
         LR    R1,R0                                                            
         B     GRIDX                                                            
*-------------------                                                            
GRIDX    J     XITR1                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R4,R3                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CALCULATE TOTALS FOR GRIDS                                   *         
*         ON INPUT:                                                   *         
*             R3 = GRID TABLE                                         *         
***********************************************************************         
         USING GCTBLD,R3                                                        
GRITOT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
*                                                                               
         XC    TEMP,TEMP                                                        
*                                                                               
         CLI   GCTCOID,GCOFFQ       OFFICE?                                     
         BNE   GRIT05                NO                                         
         MVI   TEMP,C'T'                                                        
         MVC   TEMP+1(L'MX@TOTAL),MX@TOTAL                                      
         LHI   R0,L'MX@TOTAL+1                                                  
         B     GRITX                                                            
*                                                                               
GRIT05   CLI   GCTCOID,GCDRQ       DEBIT COLUMN?                                
         BNE   GRIT10               NO                                          
         CURED (P8,DEBTOT),(16,TEMP),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES            
         B     GRITX                                                            
*                                                                               
GRIT10   CLI   GCTCOID,GCCRQ       CREDIT COLUMN?                               
         BNE   GRIT20               NO                                          
         CURED (P8,CRETOT),(16,TEMP),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES            
         B     GRITX                                                            
*                                                                               
GRIT20   CLI   GCTCOID,GCDIFQ      DIFFERENCE?                                  
         BNE   GRIT30                NO                                         
         ZAP   DIFFO,DEBTOT                                                     
         L     RE,AOPTVALS                                                      
         USING OPTVALSD,RE                                                      
         OC    ENDBBF,ENDBBF                                                    
         BZ    *+10                                                             
         AP    DIFFO,BALFRWD                                                    
         SP    DIFFO,CRETOT                                                     
         CURED (P8,DIFFO),(16,TEMP),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES             
         B     GRITX                                                            
*                                                                               
GRIT30   CLI   GCTCOID,GCNAMEQ    NAME?                                         
         BNE   GRIX                 NO                                          
*                                                                               
         L     RE,AOPTVALS                                                      
         USING OPTVALSD,RE                                                      
         CLI   SVACTNUM,ACTNJBST   TYPE = JS?                                   
         BE    GRIX                                                             
         OC    ENDBBF,ENDBBF                                                    
         BZ    GRIX                                                             
         TM    STATFLAG,SHOWAST       BBF FROM PEELED?                          
         BO    *+14                    YES, CONTINUE                            
         OC    OMOSST,OMOSST           NO, START DATE?                          
         BZ    GRIX                    NO, ZERO BBF NOT NEEDED                  
         DROP  RE                                                               
*                                                                               
         LA    R4,TEMP                                                          
         MVC   0(L'MX@BALB9,R4),MX@BALB9                                        
         LA    R4,L'MX@BALB9(R4)                                                
         MVI   0(R4),C'='                                                       
         LA    R4,1(R4)                                                         
         ZAP   DUB,BALFRWD                                                      
         CURED (P8,DUB),(16,(R4)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES               
         AR    R0,R4                                                            
         LA    R1,TEMP                                                          
         SR    R0,R1                                                            
         BNP   GRIX                                                             
*                                                                               
GRITX    LR    R1,R0                                                            
         J     XITR1                                                            
*----------------------------------                                             
GRIX     LHI   R1,1                                                             
         MVI   TEMP,C' '                                                        
         J     XITR1                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
*            SF GRID COLUMN TABLE - COVERED BY GCTBLD                 *         
***********************************************************************         
GCTBL    DS    0F                                                               
GCTOFF   DC    AL1(GOFFLQ,GCOFFQ,L'MX@OFFC,0)            OFFICE                 
         DC    AL2(MX@OFFC-OVERWRKD,AGRIDAT-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTITOT+GCTIROUT+GCTIRTOT,0,0,0)                    
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
GOFFLQ   EQU   *-GCTOFF                                                         
*                                                                               
GCTCON   DC    AL1(GCONLQ,GCCON,L'LC@CTRA,L'TSDCONT)     CONTRA ACCOUNT         
         DC    AL2(LC@CTRA-WORKD,TSDCONT-TSARDATD)                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
GCONLQ   EQU   *-GCTCON                                                         
*                                                                               
GCTNAME  DC    AL1(GNAMELQ,GCNAMEQ,L'MX@NAME,L'TSDNAME)  NAME                   
         DC    AL2(MX@NAME-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER+GCTITOT+GCTIRTOT,0,0,0)                             
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
GNAMELQ  EQU   *-GCTNAME                                                        
*                                                                               
GCTDR    DC    AL1(GDRLQ,GCDRQ,L'LC@DRS,L'TSDDR)         DEBITS                 
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(GCTITOT+GCTIRTOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
GDRLQ    EQU   *-GCTDR                                                          
*                                                                               
GCTCR    DC    AL1(GCRLQ,GCCRQ,L'LC@CRS,L'TSDCR)         CREDITS                
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(GCTITOT+GCTIRTOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
GCRLQ    EQU   *-GCTCR                                                          
*                                                                               
GCTDIF   DC    AL1(GDIFLQ,GCDIFQ,L'MX@DFRNC,0)           DIFFERENCE             
         DC    AL2(MX@DFRNC-OVERWRKD,AGRIDAT-OVERWRKD)                          
         DC    AL1(GCTITOT+GCTIRTOT+GCTIROUT+GCTIOVER+GCTIXLDG,0)               
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
         DC    C'SJ'                                                            
GDIFLQ   EQU   *-GCTDIF                                                         
*                                                                               
         DC    AL1(EOT)                                                         
*----------------------------------------------------------------------         
*            ST, JS GRID COLUMN TABLE - COVERED BY GCTBLD                       
*----------------------------------------------------------------------         
GC2BL    DS    0F                                                               
         DC    AL1(GCONLQ,GCCON2,L'TEMPCOL1,L'TSDCONT)   CONTRA/WRKCDE          
         DC    AL2(TEMPCOL1-OVERWRKD,TSDCONT-TSARDATD)                          
         DC    AL1(GCTIOVER+GCTITOT,0,0,0)                                      
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
*                                                                               
         DC    AL1(GNAMELQ,GCNAMEQ,L'MX@NAME,L'TSDNAME)  NAME                   
         DC    AL2(MX@NAME-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER+GCTITOT+GCTIRTOT,0,0,0)                             
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
*                                                                               
         DC    AL1(GDRLQ,GCDRQ,L'LC@DRS,L'TSDDR)         DEBITS                 
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(GCTITOT+GCTIRTOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
*                                                                               
         DC    AL1(GCRLQ,GCCRQ,L'LC@CRS,L'TSDCR)         CREDITS                
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(GCTITOT+GCTIRTOT,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
*                                                                               
GCTDIF2  DC    AL1(GDIF2LQ,GCDIFQ,L'MX@DFRNC,0)          DIFFERENCE             
         DC    AL2(MX@DFRNC-OVERWRKD,AGRIDAT-OVERWRKD)                          
         DC    AL1(GCTITOT+GCTIRTOT+GCTIROUT+GCTIOVER+GCTIXLDG,0)               
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
         DC    C'SJ'                                                            
GDIF2LQ  EQU   *-GCTDIF2                                                        
*                                                                               
GCTPB    DC    AL1(GPBLQ,GCDIFQ,L'MX@PRSB,0)         PRESENT BALANCE            
         DC    AL2(MX@PRSB-OVERWRKD,AGRIDAT-OVERWRKD)                           
         DC    AL1(GCTITOT+GCTIRTOT+GCTIROUT+GCTIOVER,0)                        
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(AGRITOT-OVERWRKD)                                   
         DC    C'SJ'                                                            
GPBLQ    EQU   *-GCTPB                                                          
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
*---------------------------------------------------------------------          
* GRID COLUMN EQUATES                                                           
*---------------------------------------------------------------------          
GCOFFQ   EQU   1      OFFICE                                                    
GCCON    EQU   2      CONTRA CODE                                               
GCCON2   EQU   92     CONTRA CODE                                               
GCNAMEQ  EQU   3      NAME                                                      
GCDRQ    EQU   4      DEBITS                                                    
GCCRQ    EQU   5      CREDITS                                                   
GCDIFQ   EQU   6      DIFFERENCE                                                
         EJECT                                                                  
*********************************************************************           
OVERWRKD DSECT                                                                  
AGRITOT  DS    A                   GRID TOTAL ROUTINE                           
AGRIDAT  DS    A                   GRID DATA ROUTINE                            
AGCTBL   DS    A                   GRID TABLE                                   
*                                                                               
ACCNAM   DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ACCNAMLN DS    X                   ACCOUNT NAME LENGTH                          
JOBSTAT  DS    XL(L'JOBSTA1)       JOB STATUS                                   
DISPLDDR DS    PL8                 TOTAL DEBS DISPLAYED ON CURR SCREEN          
DISPLDCR DS    PL8                 TOTAL CREDS DISPLAYED ON CURR SCREEN         
DIFFO    DS    PL8                                                              
*                                                                               
TEMPCOL1 DS    CL15                WORKCODE                                     
*                                                                               
FORMFLAG DS    X                   DISPLAY FORMAT FLAG                          
OTHERLNQ EQU   X'80'               . 'OTHERS' LINE TO BE DISPLAYED              
NOMOREQ  EQU   X'40'               . NO MORE INFO TO DISPLAY                    
STARTQ   EQU   X'20'               . DISPLAY IS FIRST FOR ACCOUNT               
FLTDISPQ EQU   X'08'               . BAL DISPLAY ON LAST SCRREN ONLY            
SCRFULLQ EQU   X'04'               . SCREEN FULL                                
*                                                                               
DSMIX    DS    0C                                                               
MX@ENH11 DS    CL78                                                             
MX@ENH12 DS    CL78                                                             
MX@ENH30 DS    CL78                                                             
MX@ENH31 DS    CL78                                                             
MX@ACC   DS    CL9                                                              
MX@XJOB  DS    CL11                                                             
MX@STCON DS    CL14                                                             
MX@BALBF DS    CL15                                                             
MX@PRSB  DS    CL15                                                             
MX@OTHRS DS    CL15                                                             
MX@ACCDF DS    CL15                                                             
MX@TOTAL DS    CL9                                                              
MX@MOA   DS    CL3                                                              
MX@OFFC  DS    CL6                                                              
MX@BALB9 DS    CL9                                                              
MX@BFW   DS    CL3                                                              
MX@WC    DS    CL2,CL1                                                          
MX@DFRNC DS    CL10                                                             
MX@NAME  DS    CL4                                                              
*                                                                               
*********************************************************************           
* SCREEN ITEM LINE1                                                             
*********************************************************************           
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CON1 DS    CL(L'CHDKULA)       CONTRA ACCOUNT CODE                          
         DS    CL1                                                              
SCR1NAM1 DS    CL36                CONTRA ACCOUNT NAME                          
         DS    CL2                                                              
         ORG   SCR1CON1            STATEMENT BY OFFICE FORMAT                   
*                                                                               
SCR1OFF  DS    CL(L'TRNKOFF)       OFFICE CODE                                  
         DS    CL1                                                              
SCR1CON2 DS    CL(L'CHDKULA)       CONTRA ACCOUNTR CODE                         
         DS    CL1                                                              
SCR1NAM2 DS    CL34                CONTRA ACCOUNT NAME                          
         DS    CL1                                                              
*                                                                               
SCR1DR   DS    CL12                DEBITS                                       
         DS    CL1                                                              
SCR1CR   DS    CL12                CREDITS                                      
*                                                                               
*********************************************************************           
* SCREEN BALANCE LINE 1                                                         
*********************************************************************           
SCRBAL1D DSECT                     COVER SCREEN BALANCE LINE 1                  
         DS    CL15                                                             
SCRBDES1 DS    CL15                DESCRIPTION FOR BALANCE                      
         DS    CL1                                                              
SCRBBAL  DS    CL13                BALANCE                                      
         DS    CL2                                                              
SCRBDES2 DS    CL6                 DESCRIPTION FOR DEBITS/CREDITS               
         DS    CL1                                                              
SCRBDB   DS    CL12                DEBITS                                       
         DS    CL1                                                              
SCRBCR   DS    CL12                CREDITS                                      
*                                                                               
*********************************************************************           
* TSAR DATA ITEM                                                                
*********************************************************************           
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSDITM1  EQU   1                   ITEM 1 FORMAT                                
TSOFFITM EQU   2                   OFFICE ITEM 1 FORMAT                         
TSDCONT  DS    CL(L'CHDKULA)       CONTRA CODE                                  
TSDNAME  DS    CL36                CONTRA NAME                                  
TSDOFF   DS    CL(L'TRNKOFF)       OFFICE CODE (2 CHAR OFFICE ONLY)             
TSDDR    DS    PL8                 DEBITS                                       
TSDCR    DS    PL8                 CREDITS                                      
TSARDLNQ EQU   *-TSARDATD                                                       
TSOBF    DS    PL8                 BALANCE BROUGHT FORWARD                      
TSOIND   DS    XL1                 OFFICE INDICATOR                             
TSBBFBQ  EQU   X'80'               BBF FROM BALANCE ELEM AFTER ST DATE          
TSAROLNQ EQU   *-TSARDATD                                                       
*                                                                               
*--------------------------*                                                    
* SUBTOTAL, BBF, AND TOTAL *                                                    
*--------------------------*                                                    
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSSUBITM EQU   3                   SUB TOTAL ITEM                               
TSBBFITM EQU   4                   BALANCE BROUGHT FORWARD ITEM                 
TSTOTITM EQU   5                   TOTAL ITEM TYPE                              
TSTCONT  DS    CL(L'TRNKULA)       SPACES                                       
TSTNAME  DS    CL(L'NAMEREC)       SUB/BBF                                      
TSTDR    DS    PL8                 DEBITS                                       
TSTCR    DS    PL8                 CREDITS                                      
TSTDIF   DS    PL8                 DIFFERENCE                                   
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
*-----------------------------------*                                           
* TEMP ENTRIES FOR READING UP FRONT *                                           
*-----------------------------------*                                           
TEMPD    DSECT                     TEMP ENTRIES FOR READING UP FRONT            
TEMPCONT DS    CL(L'CHDKULA)       CONTRA CODE NAME                             
TEMPNAME DS    CL36                CONTRA NAME                                  
TEMPDR   DS    PL6                 DEBITS                                       
TEMPCR   DS    PL6                 CREDITS                                      
TEMPLNQ  EQU   *-TEMPD                                                          
*                                                                               
*********************************************************************           
       ++INCLUDE ACENQWORK                                                      
*                                                                               
*********************************************************************           
* OVERLAY SAVE AREA                                                             
*********************************************************************           
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
*                                                                               
DETFLAG  DS    X                                                                
DETGRINQ EQU   X'80'               . SCREEN INITIALIZED FOR GRIDS               
DETFOUND EQU   X'08'               . DETAILS FOUND                              
*                                                                               
STATFLAG DS    X                   STATEMENT FLAG                               
STOFBUK  EQU   X'80'               . USE OFFICE/CONTRA BUCKETS                  
BBFTRN   EQU   X'40'               . ADD TRANS TO BBF                           
SHOWAST  EQU   X'20'               . START DATE OVERLAPS PEELED DATE            
OFFBBFT  EQU   X'08'               . OFFICE BBF?                                
TOTALBBF EQU   X'04'               . TOTAL AND BBF ONLY TO DISPLAY              
STFULLQ  EQU   X'02'               . NO MORE ROOM FOR 'UP FRONT' READ           
STSUPPT  EQU   X'01'               . SUPPRESS PRESENT BAL TOTALS                
*                                                                               
CONTOFF  DS    CL(L'CHDKOFF)       OFFICE CODE                                  
CONTCODE DS    CL(L'CHDKULA)       CONTRA CODE                                  
CONTNAME DS    CL(L'NAMEREC)       CONTRA NAME                                  
*                                                                               
STVALS   DS    0PL8                CREDITOR VALUES                              
DEBTOT   DS    PL8                 TOTAL DEBITS                                 
CRETOT   DS    PL8                 TOTAL CREDITS                                
DEBITS   DS    PL8                 INVOICE DEBITS                               
CREDITS  DS    PL8                 INVOICE CREDITS                              
ODRTOT   DS    PL8                 OFFICE DEBIT TOTALS                          
OCRTOT   DS    PL8                 OFFICE CREDIT TOTALS                         
OBFTOT   DS    PL8                 OFFICE BALANCE BROUGHT FORWARD               
BALFRWD  DS    PL8                 BALANCE BROUGHT FORWARD                      
BALTOT   DS    PL8                 PRESENT BALANCE                              
STVALLNQ EQU   *-STVALS                                                         
*                                                                               
CLOSEDAT DS    XL(L'OFAKCMOS)      LAST CLOSED MOS                              
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
*                                                                               
*********************************************************************           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACENQ09   01/30/14'                                      
         END                                                                    
