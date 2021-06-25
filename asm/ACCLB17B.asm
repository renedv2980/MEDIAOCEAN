*          DATA SET ACCLB17B   AT LEVEL 071 AS OF 12/22/99                      
*PHASE T62117B                                                                  
CLB17    TITLE '- BILL PROGRAM - FEE ADJUSTMENT'                                
CLB17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB17**,R8,R7,CLEAR=YES,RR=RE                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING FWORKD,RC                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
*                                                                               
         SRL   RF,24                                                            
         LTR   RF,RF               TEST RETURN FROM NTRSES                      
         BZ    INI01                                                            
         LA    RE,BASACTH                                                       
         ST    RE,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         B     EXIT                                                             
*                                                                               
INI01    TM    BCINDS2,BCINTRS     TEST FIRST TIME                              
         BNO   INI02                                                            
         MVI   SMODE,SMODENEW      ASSUME ADDING NEW RECORD                     
         SR    RE,RE                                                            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   SRECACT,0(RE)                                                    
         CLI   SACT,ACTFLI         TEST PREVIOUS ACTION IS FEE-LIST             
         BNE   INI02                                                            
         XR    RF,RF               YES - TEST SELECTED EXISTING FEE             
         ICM   RF,3,CSSELCUR                                                    
         A     RF,AOVERSEL                                                      
         CLC   =Y(UC@SEL-TWAD),0(RF)                                            
         BNE   INI02                                                            
         MVI   SMODE,SMODESEL                                                   
*                                                                               
INI02    LA    R2,FWORKD                                                        
         LA    R3,FWORKL                                                        
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         MVC   FDEFCRUL,=C'SI'     SET DEFAULT CREDIT LEDGER TO SI              
         CLI   CUCTRY,CTRYGER                                                   
         BNE   *+10                                                             
         MVC   FDEFCRUL,=C'SK'     SET TO SK FOR GERMANY                        
*                                                                               
*        TM    BCINDS2,BCIXITS                                                  
*        BO    INI04                                                            
         CLC   TWASCRN,CSSCRN                                                   
         BE    VAL08                                                            
         GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*&&US                                                                           
         OI    FEEPERH+(FVATRB-FVIHDR),FVAPROT  NO PERSON/RATE/HOURS            
         OI    FEERATH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    FEEHRSH+(FVATRB-FVIHDR),FVAPROT                                  
         XC    FEEPERT,FEEPERT                                                  
         OI    FEEPERTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    FEERATT,FEERATT                                                  
         OI    FEERATTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    FEEHRST,FEEHRST                                                  
         OI    FEEHRSTH+(FVOIND-FVIHDR),FVOXMT                                  
*&&                                                                             
         B     INI04                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY CLIENT PRODUCT AND JOB                                      *         
***********************************************************************         
         SPACE 1                                                                
INI04    MVC   FEECLIC,BCCLICOD    CLIENT CODE                                  
         MVC   FEECLIN,BCCLINAM    CLIENT NAME                                  
*        TM    BCINDS2,BCIXITS                                                  
*        BO    *+10                                                             
         XC    SFESAV,SFESAV       CLEAR SAVED VALUES                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    R1,BCPROCOD(RE)                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FEEPROC(0),0(R1)    PRODUCT CODE                                 
         MVC   FEEPRON,BCPRONAM    PRODUCT NAME                                 
*                                                                               
         IC    RE,BCPROLEN                                                      
         LA    R1,BCJOBCOD(RE)                                                  
         IC    RF,BCJOBLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FEEJOBC(0),0(R1)    JOB CODE                                     
         MVC   FEEJOBN,BCJOBNAM    JOB NAME                                     
*                                                                               
         MVC   FEECURR,CSBILCUR                                                 
*                                                                               
         CLI   SMODE,SMODENEW                                                   
         BE    INI40                                                            
         MVC   SFEDA,TLDA                                                       
         MVC   IODAOVER,SFEDA      READ FEEADJ TRANSACTION SELECTED             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         PUSH  USING                                                            
         USING TRNRECD,RF                                                       
         USING TRNELD,TRNRFST                                                   
         CLI   TRNTYPE,169         MAKE SURE GOT CORRECT RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                  CONVERT FOR DISPLAY IF NECESSARY             
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   INI12                                                            
         GOTO1 VTOBACCO,BOPARM,('TOBAACVS',TOBCUR),AIO3,ACOM,0,0                
         OI    LSINDS2,LSTOBACC                                                 
*                                  CREDIT ACCOUNT                               
INI12    L     R4,AIO3                                                          
         USING TRNRECD,R4                                                       
         USING TRNELD,TRNRFST                                                   
         MVC   FEECRAC(L'TRNKCACT),TRNKCACT                                     
         CLC   FDEFCRUL,TRNKULC                                                 
         BE    *+14                                                             
         MVI   FEECRAC,C'*'                                                     
         MVC   FEECRAC+1(L'TRNKULC),TRNKULC                                     
         OI    FEECRACH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    FEECRACH+(FVATRB-FVIHDR),FVAPROT                                 
I        USING ACTRECD,IOKEY                                                    
         MVC   I.ACTKEY,BCSPACES                                                
         MVC   I.ACTKCULA,TRNKCULC                                              
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         MVC   FEECRAN,ACNAME                                                   
         OI    FEECRANH+(FVOIND-FVIHDR),FVOXMT                                  
*                                  WORKCODE                                     
         MVC   FEEWC,TRNKWORK                                                   
         OI    FEEWCH+(FVOIND-FVIHDR),FVOXMT                                    
         OI    FEEWCH+(FVATRB-FVIHDR),FVAPROT                                   
         GOTO1 AGETWRK,FEEWC                                                    
         BNE   EXITN                                                            
         MVC   FEEWCN,BOWORK1                                                   
         OI    FEEWCNH+(FVOIND-FVIHDR),FVOXMT                                   
*                                  REFERENCE                                    
         MVC   FEEREF,TRNKREF                                                   
         OI    FEEREFH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    FEEREFH+(FVATRB-FVIHDR),FVAPROT                                  
*                                  DATE                                         
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,FEEDATE)                         
         OI    FEEDATEH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    FEEDATEH+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
         MVC   FEECMBL,BCSPACES    COMMISSIONABLE                               
         MVC   FEECMBL(L'BC@YES),BC@YES                                         
         TM    TRNSTAT,TRNSNOCM                                                 
         BNO   *+16                                                             
         MVC   FEECMBL,BCSPACES                                                 
         MVC   FEECMBL(L'BC@NO),BC@NO                                           
*                                                                               
         MVC   FEEALOC,BCSPACES    ALLOCATED                                    
         MVC   FEEALOC(L'BC@NO),BC@NO                                           
         LA    RE,TRNELD                                                        
         USING PTAELD,RE                                                        
         SR    R0,R0                                                            
INI14    IC    R0,PTALN                                                         
         AR    RE,R0                                                            
         CLI   PTAEL,0                                                          
         BE    INI20                                                            
         CLI   PTAEL,PTAELQ                                                     
         BNE   INI14                                                            
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BZ    INI14                                                            
         CLI   PTATYPE,PTATRAL     REGULAR ALLOCATION                           
         BNE   INI14                                                            
         CP    PTANET,BCPZERO      TEST ALLOCATION AMOUNT                       
         BE    INI20                                                            
         MVC   FEEALOC,BCSPACES                                                 
         MVC   FEEALOC(L'BC@YES),BC@YES                                         
*                                  TRANSACTION AMOUNT                           
*NI10    DS    0H                                                               
*        GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('TPRELQ',TRNRECD),0                 
*        CLI   BOPARM+12,0                                                      
*        BNE   INI20A                                                           
*        L     R6,BOPARM+12                                                     
         USING TPRELD,R6                                                        
*NIL4    CURED (P6,TPRNUMP),(14,FEEALNT),2,ALIGN=LEFT,MINUS=YES,                
*              ZERO=NOBLANK,DMCB=BODMCB                                         
*        LA    R3,FEEALNT+15                                                    
*        LA    R2,15                                                            
*NIL6    CLI   0(R3),X'41'                                                      
*        BH    INIL8                                                            
*        BCTR  R3,0                                                             
*        BCT   R2,INPL6                                                         
*NIL8    MVI   1(R3),C','                                                       
*        MVC   2(4,R3),TPRCOD                                                   
*        B     NEE                                                              
         DROP  R6                                                               
INI20    CURED TRNAMNT,(14,FEEALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,     X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEALNTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         OI    FEEFOFH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('FFTELQ',TRNRECD),         X        
               (L'FFTTYPE,=AL1(FFTTOFFC))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI22                                                            
         L     RE,BOPARM+12                                                     
         MVC   FEEFOF,FFTDATA-FFTELD(RE)                                        
*                                                                               
INI22    TM    FEEPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    INI28               NO PERSON/RATE/HOURS                         
         OI    FEEPERH+(FVIIND-FVIHDR),FVIVAL                                   
*                                                                               
         OI    FEEPERH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATPERS))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI24                                                            
         L     RE,BOPARM+12                                                     
         MVC   FEEPER,SPAAACT-SPAELD(RE)                                        
*                                                                               
INI24    OI    FEERATH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITCRAT))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI26                                                            
         L     RE,BOPARM+12                                                     
         ZAP   BODUB1,SCIAMNT-SCIELD(L'SCIAMNT,RE)                              
         CURED BODUB1,(14,FEERAT),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
*                                                                               
INI26    OI    FEEHRSH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITSJHR))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI28                                                            
         L     RE,BOPARM+12                                                     
         ZAP   BODUB1,SCIAMNT-SCIELD(L'SCIAMNT,RE)                              
         CURED BODUB1,(14,FEEHRS),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
*                                                                               
INI28    MVC   BOWORK1,BCSPACES                                                 
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)                                                 
         BNP   *+14                                                             
         EX    RE,*+4                                                           
         MVC   BOWORK1(0),TRNNARR                                               
         MVC   FEENAR1,BOWORK1                                                  
         MVC   FEENAR2,BOWORK1+(L'FEENAR1)                                      
         OI    FEENAR1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    FEENAR2H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         LA    R2,FEECMBLH                                                      
         ST    R2,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         B     EXIT                                                             
*                                                                               
INI40    LA    R2,FEECRACH                                                      
         ST    R2,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ITEM DETAILS                                               *         
***********************************************************************         
         SPACE 1                                                                
VAL08    LA    R1,FEECRACH         TEST ANY SCREEN INPUT                        
         XR    RF,RF                                                            
VAL10    ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    VAL12                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BZ    VAL16                                                            
         BXH   R1,RF,VAL10                                                      
VAL12    L     RF,AINP             KEEP CURSOR & MESSAGE AS THEY WERE           
         USING TIOBD,RF                                                         
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RF                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     EXIT                                                             
*                                                                               
VAL16    MVI   IKEYINP,C'N'        SET NO KEY CHANGE                            
         CLI   SMODE,SMODESEL      TEST SELECTED RECORD                         
         BE    VAL44               YES - KEY IS PROTECTED                       
*                                                                               
         MVI   FVMINL,1            WORKCODE - REQUIRED INPUT                    
         GOTO1 AFVAL,FEEWCH                                                     
         BE    VAL17                                                            
         CLI   FEECRACH+(FVILEN-FVIHDR),0                                       
         BNE   EXIT                GIVE FEEWC ERROR                             
         CLI   P#DINCAR,C' '       TEST DEFAULT INCOME ACCOUNT RULE             
         BNH   VAL24               LET FVAL GIVE FEECRAC ERROR                  
         CLI   P#DINCAR,C'N'       TEST NO DEFAULT                              
         BE    VAL24               LET FVAL GIVE FEECRAC ERROR                  
         B     EXIT                EXIT CC NEQ                                  
*                                                                               
VAL17    TM    FVIIND,FVIVAL                                                    
         BO    VAL20                                                            
         CLC   FVIFLD(2),=C'99'                                                 
         BE    *+14                                                             
         CLC   FVIFLD(2),=C'**'                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
         OI    FEEWCH+(FVOIND-FVIHDR),FVOXMT                                    
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         GOTO1 AGETWRK,FVIFLD                                                   
         BNE   EXITN                                                            
         CLI   CUCTRY2,CTRYGER     TEST GERMANY                                 
         BNE   VAL18                                                            
         CLI   FVIFLD,C'Z'         TEST INTERNAL W/C (1ST CHAR ALPHA)           
         BNH   VAL18                                                            
         MVC   FVMSGNO,=AL2(AE$MUIWC)                                           
         B     EXITN                                                            
VAL18    MVC   SFEWC,FVIFLD                                                     
         MVC   FEEWCN,BOWORK1                                                   
         OI    FEEWCNH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VAL20    L     R1,AIO3             PASS JOB/WORKCODE IN IO3                     
         USING TRNRECD,R1                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,SFEWC      READ WORKCODE LEVEL OPTIONS                  
         DROP  R1                                                               
         GOTO1 AGETOPT,BODMCB,AIO3                                              
*                                                                               
         OC    FEECRAC,BCSPACES                                                 
         CLC   FEECRAC,BCSPACES    TEST CREDIT ACCOUNT PRESENT                  
         BNH   VAL22                                                            
         CLC   FEECRAC,SCRNCRAC    TEST AGAINST ANY RESOLVED A/C                
         BNE   VAL24                                                            
         TM    FEECRACH+(FVIIND-FVIHDR),FVITHIS                                 
         BO    VAL24               IGNORE PROFILE IF INPUT THIS TIME            
VAL22    CLI   P#DINCAR,C' '       TEST DEFAULT INCOME ACCOUNT RULE             
         BNH   VAL24               LET FVAL GIVE AN ERROR                       
         CLI   P#DINCAR,C'N'       TEST NO DEFAULT                              
         BE    VAL24               LET FVAL GIVE AN ERROR                       
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
B        USING ACTRECD,BOWORK1                                                  
         MVC   B.ACTKEY,BCSPACES                                                
         MVC   B.ACTKCULA,GOINCAC-GOBBLOCK(RF)                                  
         CLI   P#DINCAR,C'B'       TEST LEDGER AS SET BY GETOPT                 
         BE    *+10                                                             
         MVC   B.ACTKLDG,P#DINCAR  SET LEDGER DIRECTLY FROM PROFILE             
         CLC   B.ACTKLDG,FDEFCRUL+L'ACTKLDG  TEST DEFAULT LEDGER                
         BNE   *+14                                                             
         MVC   FEECRAC,B.ACTKACT   EXTRACT JUST SPACE-PADDED ACCOUNT            
         B     *+14                                                             
         MVC   FEECRAC,B.ACTKCULA  EXTRACT U/L/ACCOUNT CODE                     
         MVI   FEECRAC,C'*'        INDICATE NOT DEFAULT SK LEDGER               
         MVC   SCRNCRAC,FEECRAC    SAVE SCREEN CREDIT ACCOUNT                   
         DROP  B                                                                
VAL24    MVI   FVMINL,1            CREDIT ACCOUNT - REQUIRED INPUT              
         GOTO1 AFVAL,FEECRACH                                                   
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL30                                                            
         NI    SFEIND1,FF-(SFEIORCR)                                            
         OI    FEECRACH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         MVC   I.ACTKEY,BCSPACES                                                
         MVC   I.ACTKCPY,CUABIN                                                 
         MVC   I.ACTKUNT(L'FDEFCRUL),FDEFCRUL                                   
         MVC   I.ACTKACT,FVIFLD                                                 
         CLI   FVIFLD,C'*'         INDICATES LEDGER OVERRIDDEN                  
         BNE   VAL28                                                            
         LA    R1,OVLDGS                                                        
         LA    R0,OVLDGSN                                                       
         CLC   0(OVLDGSL,R1),FVIFLD+1                                           
         BE    VAL26                                                            
         LA    R1,OVLDGSL(R1)                                                   
         BCT   R0,*-14                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITN                                                            
VAL26    MVC   I.ACTKULA,FVIFLD+1                                               
         OI    SFEIND1,SFEIORCR    CREDIT ACCOUNT LEDGER OVERRIDDEN             
*                                                                               
VAL28    GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         CLI   CUCTRY,CTRYGER                                                   
         BNE   VAL29                                                            
         CLC   =C'SI',ACCODE+1     DISALLOW SI IN GERMANY                       
         BNE   VAL29                                                            
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITN                                                            
VAL29    TM    ACBSTAT,ACBSABAL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         MVC   FEECRAN,ACNAME                                                   
         MVC   SFECRAC,ACCODE+1                                                 
         MVC   SFECRAN,ACNAME                                                   
         OI    FEECRANH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
VAL30    MVI   FVMINL,1            REFERENCE - REQUIRED INPUT                   
         GOTO1 AFVAL,FEEREFH                                                    
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL40                                                            
         OI    FEEREFH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         MVC   SFEREF,FVIFLD                                                    
*                                                                               
VAL40    MVI   FVMINL,1            DATE - REQUIRED INPUT                        
         GOTO1 AFVAL,FEEDATEH                                                   
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL44                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         GOTO1 AVALDAT                                                          
         BNE   EXITN                                                            
         MVC   SFEDATE,BCWORK+2                                                 
*                                                                               
VAL44    LA    R2,BC@NO            COMMISSIONABLE?                              
         NI    SFEIND1,FF-(SFEICMBL)                                            
         GOTO1 AFVAL,FEECMBLH                                                   
         BNE   VAL48                                                            
         IC    R1,FVXLEN                                                        
         LA    R2,BC@NO                                                         
         EX    R1,*+8                                                           
         BE    VAL48                                                            
         CLC   FVIFLD(0),0(R2)                                                  
         LA    R2,BC@YES                                                        
         EX    R1,*+8                                                           
         BE    VAL46                                                            
         CLC   FVIFLD(0),0(R2)                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
VAL46    OI    SFEIND1,SFEICMBL                                                 
VAL48    MVC   FEECMBL,BCSPACES                                                 
         MVC   FEECMBL(L'BC@YES),0(R2)                                          
         OI    FEECMBLH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         LA    R2,BC@NO            ALLOCATED?                                   
         NI    SFEIND1,FF-(SFEIALOC)                                            
         GOTO1 AFVAL,FEEALOCH                                                   
         BNE   VAL52                                                            
         IC    R1,FVXLEN                                                        
         LA    R2,BC@NO                                                         
         EX    R1,*+8                                                           
         BE    VAL52                                                            
         CLC   FVIFLD(0),0(R2)                                                  
         LA    R2,BC@YES                                                        
         EX    R1,*+8                                                           
         BE    VAL50                                                            
         CLC   FVIFLD(0),0(R2)                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
*                                                                               
VAL50    OI    SFEIND1,SFEIALOC                                                 
VAL52    MVC   FEEALOC,BCSPACES                                                 
         MVC   FEEALOC(L'BC@YES),0(R2)                                          
         OI    FEEALOCH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         ZAP   SFENET,BCPZERO      NET                                          
         ZAP   SFECOM,BCPZERO      COMMISSION                                   
         XC    FEEALLN,FEEALLN                                                  
                                                                                
*******                                                                         
         MVI   FVMINL,1            NET IS REQUIRED INPUT                        
         GOTO1 AFVAL,FEEALNTH                                                   
         BNE   EXITN                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
*                                                                               
         LA    R2,FEEALNTH                                                      
         XR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         LA    R6,BOELEM                                                        
         GOTO1 VSCANNER,BODMCB,(R2),(3,0(R6)),C',=  '                           
         CLI   BODMCB+4,0                                                       
         BE    EXITN                                                            
         CLI   BODMCB+4,2                                                       
         BH    EXITN                                                            
         BL    IN10AA              SINGLE ITEM INPUT - NO PRICELIST             
         CLI   0(R6),X'0C'                                                      
         BH    EXITN                                                            
         XR    R3,R3                                                            
         IC    R3,0(R6)                                                         
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(R3))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN                                                            
         ZAP   SFEPLAMT,BODMCB+4(8)                                             
         BZ    EXITN                                                            
*                                                                               
         LA    R6,32(R6)           LOCATE TO PRICE-LIST NO                      
         CLI   0(R6),X'01'                                                      
         BL    EXITN               LENGTH 1-4                                   
         CLI   0(R6),X'04'                                                      
         BH    EXITN                                                            
         BNE   INPL2                                                            
         MVC   SFEPLNO,12(R6)      IF 4 CHARACTER INPUT JUST MOVE IN            
         B     INPL4                                                            
*                                                                               
INPL2    TM    2(R6),X'80'         IF LESS THAN 4 CHARACTERS                    
         BNO   EXITN               NUMBERS MUST BE NUMERIC                      
         EDIT  (B4,4(R6)),(4,SFEPLNO),FILL=0,WRK=BOWORK1,DUB=BODUB1             
*                                                                               
INPL4    CURED SFEPLAMT,(14,FEEALLN),CSCURBIL,ALIGN=LEFT,MINUS=YES,    X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         GOTO1 PRLROUT                                                          
         BNE   EXITN                                                            
         LA    RF,FEEALLN+15                                                    
         LA    RE,15                                                            
INPL6    CLI   0(RF),X'41'                                                      
         BH    INPL8                                                            
         BCTR  RF,0                                                             
         BCT   RE,INPL6                                                         
INPL8    MVI   2(RF),C'*'                                                       
         CURED BODUB1,(14,4(RF)),2,ALIGN=LEFT,DMCB=BODMCB                       
         ZAP   REFE,BODUB1                                                      
         MP    REFE,SFEPLAMT                                                    
         SRP   REFE,64-2,5                                                      
         OC    REFE+8(2),REFE+8                                                 
         BZ    *+12                                                             
         MVI   FVMSGNO,1      INVRESUL                                          
         B     EXITN                                                            
         ZAP   SFENET,REFE+10(6)                                                
         LA    RF,FEEALLN+30                                                    
         LA    RE,30                                                            
INPL10   CLI   0(RF),X'41'                                                      
         BH    INPL12                                                           
         BCTR  RF,0                                                             
         BCT   RE,INPL10                                                        
                                                                                
INPL12   MVI   2(RF),C'='                                                       
         CURED (P8,SFENET),(14,4(RF)),2,ALIGN=LEFT,DMCB=BODMCB                  
         OI    FEEALLNH+(FVOIND-FVIHDR),FVOXMT                                  
         B     IN10A                                                            
*                                                                               
******                                                                          
IN10AA   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SFENET,BODMCB+4(8)                                               
         BZ    EXITN               AMOUNT MUST BE NON-ZERO                      
         CURED SFENET,(14,FEEALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEALNTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
IN10A    MVC   SFEFOF,BCSPACES                                                  
         MVI   FVMINL,0            NOT REQUIRED INPUT                           
         GOTO1 AFVAL,FEEFOFH                                                    
         BL    VAL60                                                            
         TM    SFEIND1,SFEIORCR    IS CREDIT ACCOUNT LEDGER OVERRIDDEN?         
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NAFLE)                                           
         B     EXITN                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   SFEFOF(0),FVIFLD                                                 
         GOTO1 AVALOFF,BOPARM,(X'80',SFEFOF)                                    
         BNE   EXITN                                                            
*                                                                               
VAL60    TM    FEEPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VAL70                                                            
         CLI   P#PERSJC,C'Y'       TEST USE PERSON AS SJ CONTRA                 
         BNE   VAL62                                                            
         TM    FEEPERH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    *+8                                                              
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
VAL62    MVC   SFEPER,BCSPACES     PERSON                                       
         XC    FEEPERN,FEEPERN                                                  
         OI    FEEPERNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    FEEHRS,FEEHRS                                                    
         OI    FEEHRSH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,FEEPERH                                                    
         BL    VAL64                                                            
         MVC   I.ACTKEY,BCSPACES                                                
         MVC   I.ACTKCPY,CUABIN                                                 
         MVI   I.ACTKUNT,C'1'      PERSON IN 1P                                 
         MVI   I.ACTKLDG,C'P'                                                   
         CLI   CUCTRY2,CTRYGER     TEST GERMAN AGENCY                           
         BE    *+12                                                             
         TM    BCCPYST7,CPYSTMSY   OR TMS USER                                  
         BZ    *+8                                                              
         MVI   I.ACTKLDG,C'R'      PERSON IN 1R                                 
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   I.ACTKACT(0),FVIFLD                                              
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         TM    ACBSTAT,ACBSABAL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         OI    FEEPERH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   SFEPER,ACCODE+1                                                  
         MVC   FEEPERN,ACNAME                                                   
         OI    FEEPERH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    FEEPERNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   FVMINL,1            RATE NOW REQUIRED                            
         B     *+8                                                              
*                                                                               
VAL64    MVI   FVMINL,0            RATE NOT ALLOWED WITHOUT PERSON              
         ZAP   SFERAT,BCPZERO      RATE                                         
         ZAP   SFEHRS,BCPZERO      HOURS                                        
         GOTO1 AFVAL,FEERATH                                                    
         BH    EXITN                                                            
         BL    VAL70                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         CLC   SFEPER,BCSPACES                                                  
         BNH   EXITN                                                            
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SFERAT,BODMCB+4(8)                                               
         BZ    EXITN               AMOUNT MUST BE NON-ZERO                      
         CURED SFERAT,(14,FEERAT),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEERATH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         ZAP   BODUB1,SFENET       NET AMOUNT                                   
         ZAP   BODUB2,SFERAT       RATE                                         
         ZAP   BOPL81(16),BODUB1   CALCULATE HOURS=NET/RATE                     
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),BODUB2                                                
         SRP   BOPL81,64-2,5                                                    
         ZAP   SFEHRS,BOPL81                                                    
         CURED SFEHRS,(14,FEEHRS),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEHRSH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VAL70    MVC   SFENAR,BCSPACES                                                  
         MVI   SFENARL,0                                                        
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,FEENAR1H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL72                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFENAR(0),FVIFLD                                                 
         LA    RF,1(RF)                                                         
         STC   RF,SFENARL          SET NARRATIVE LENGTH                         
VAL72    MVI   FVMINL,0                                                         
         GOTO1 AFVAL,FEENAR2H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BE    VAL80                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFENAR+L'FEENAR1(0),FVIFLD                                       
         LA    RF,L'FEENAR1+1(RF)                                               
         STC   RF,SFENARL                                                       
*                                                                               
VAL80    DS    0H                                                               
         CLI   IKEYINP,C'Y'        TEST IF RECORD ALREADY THERE                 
         BNE   CHA10               YES - READ AND UPDATE IT                     
         B     ADD10               NO - ADD NEW RECORD                          
         EJECT                                                                  
*********                                                                       
PRLROUT  NTR1                      VALIDATE PRICE-LIST                          
         MVC   FVMSGNO,58          ELEMENT NOT ON FILE                          
         MVC   IOKEY,BCSPACES                                                   
         LA    R1,IOKEY                                                         
         USING PRLRECD,R1                                                       
         MVI   PRLKTYP,PRLKTYPQ   PRICE-LIST KEY                                
         MVC   PRLKCPY,CUABIN                                                   
         MVC   PRLKNUM,SFEPLNO                                                  
         DROP  R1                                                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AIO,IOHIGH+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEYSAV(16),IOKEY                                               
         BNE   EXITN                                                            
         L     R3,AIO2                                                          
         USING PRLRECD,R3                                                       
         LA    R3,PRLRFST                                                       
PRL000   CLI   0(R3),0                                                          
         BE    PRLEXIT                                                          
         CLI   0(R3),FFNELQ                                                     
         BE    PRL020                                                           
         CLI   0(R3),SCIELQ                                                     
         BE    PRL030                                                           
PRL010   ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     PRL000                                                           
PRL020   PACK  BODUB1,2(10,R3)                                                  
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   PRL010                                                           
         MVC   BOWORK1(L'CSCPYCUR),CSCPYCUR                                     
         MVC   BOWORK1+L'CSCPYCUR(L'BCCPYSEC),BCCPYSEC                          
         GOTO1 VCASHVAL,BOPARM,(X'80',BODUB1),(X'28',0),BOWORK1                 
         ZAP   BODUB1,12(8,R1)                                                  
         B     PRL010                                                           
         USING SCIELD,R3                                                        
PRL030   CLI   SCITYPE,SCITCOKE                                                 
         BNE   PRL010                                                           
         ZAP   BODUB1,SCIAMNT                                                   
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   PRLEXIT                                                          
         CLI   SCILN,SCILN2Q                                                    
         BNE   PRL040                                                           
         ZAP   BODUB1,SCIADMN                                                   
         B     PRLEXIT                                                          
*                                                                               
PRL040   MVC   BOWORK1(L'CSCPYCUR),CSCPYCUR                                     
         MVC   BOWORK1+L'CSCPYCUR(L'BCCPYSEC),BCCPYSEC                          
         GOTO1 VCASHVAL,BOPARM,(X'80',BODUB1),(X'28',0),BOWORK1                 
         ZAP   BODUB1,12(8,R1)                                                  
         B     PRLEXIT                                                          
*                                                                               
PRLEXIT  CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
***********************************************************************         
* ADD DRAFT TRANSACTION RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
ADD10    DS    0H                                                               
         L     R4,AIO3             BUILD RECORD IN IO3                          
         USING TRNRECD,R4                                                       
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,SFEWC                                                   
         MVC   TRNKCCPY,CUABIN                                                  
         MVC   TRNKULC,SFECRAC     CONTRA IS CREDIT A/C                         
         CLI   P#PERSJC,C'Y'       TEST USING PERSON AS SJ CONTRA A/C           
         BNE   ADD12                                                            
         CLC   SFEPER,BCSPACES     TEST PERSON SUPPLIED                         
         BNH   ADD12                                                            
         MVC   TRNKULC,SFEPER      REPLACE CONTRA WITH PERSON                   
ADD12    MVC   TRNKDATE,SFEDATE                                                 
         MVC   TRNKREF,SFEREF                                                   
*                                                                               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,SFEDATE                                                  
         MVC   TRNREF,SFEREF                                                    
         MVI   TRNTYPE,169         ADD WITH SPECIAL TRANSACTION TYPE            
         MVI   TRNSUB,X'E0'        AND START WITH A HIGH SUB-REFERENCE          
         MVC   TRNMOS,BCTMON       SET TODAYS MOA                               
         MVC   TRNBREF,BCSPACES    SET NULL BATCH REFERENCE                     
         MVI   TRNSTAT,TRNSDR+TRNSAUTH                                          
         MVC   TRNANAL,SFEWC                                                    
         ZAP   TRNAMNT,BCPZERO                                                  
         SR    RE,RE                                                            
         ICM   RE,1,SFENARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SFENAR                                                
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    RF,TRNEL(RE)                                                     
         MVI   0(RF),0             SET E-O-R                                    
         SR    RF,R4                                                            
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   *+8                                                              
         OI    LSINDS2,LSTOBACC                                                 
*                                                                               
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         L     R4,AADTBLK                                                       
         USING ADDTRND,R4                                                       
         GOTO1 AINIADT                                                          
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         MVC   TRNCACNM,SFECRAN    CREDIT ACCOUNT NAME                          
         GOTO1 VADDTRN,ADDTRND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND                                                  
         L     RE,AIO3                                                          
         AH    RE,=Y(2000-4)                                                    
         MVC   SFEDA,0(RE)         SAVE DISK ADDRESS OF TRANSACTION             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INSERT NEW ITEM IN THE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
INS10    CLI   SACT,ACTFLI         ONLY INSERT IF CAME FROM INTLIST             
         BNE   ADDXIT                                                           
*                                                                               
         XC    FIOKEY,FIOKEY                                                    
         L     RE,AIO3                                                          
F        USING TRNRECD,FIOKEY                                                   
         MVC   F.TRNKEY,0(RE)                                                   
         MVC   F.TRNKSTA,TRNRSTA-TRNKEY(RE)                                     
         MVC   F.TRNKDA,SFEDA                                                   
         DROP  F                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',FIOKEY),AIO3,LSPRATA                        
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                                                             
         STC   RE,TLKSES                                                        
F        USING TLSTD,FLST                                                       
         MVC   F.TLKEY,TLKEY                                                    
         MVC   F.TLKSEQ,BCEFFS     GET RECORD AFTER INSERTION                   
         GOTO1 ATSARIO,BOPARM,('TSARDH',F.TLSTD)                                
         BL    *+10                END-OF-FILE                                  
         BH    INS12                                                            
         DC    H'0'                THIS KEY SHOULD NOT EXIST                    
         MVC   F.TLNUM,CSHIRECN                                                 
         B     INS14                                                            
INS12    SR    RE,RE                                                            
         ICM   RE,3,F.TLNUM                                                     
         BCT   RE,*+4                                                           
         B     INS16                                                            
         STCM  RE,3,F.TLNUM                                                     
*                                                                               
INS14    GOTO1 ATSARIO,BOPARM,('TSAGET',F.TLSTD)  GET 'BEFORE' RECORD           
         CLC   F.TLKEY(TLKSEQ-TLKEY),TLKEY                                      
         BNE   *+10                                                             
         MVC   TLKSEQ,F.TLKSEQ                                                  
         ICM   RE,3,TLKSEQ         INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLKSEQ                                                      
INS16    MVC   TLRLEN,LSMIXLST+(MIXLRECL-MIXLST)                                
         MVC   TLDA,SFEDA                                                       
         L     RF,AIO3                                                          
         MVC   TLSTA,TRNRSTA-TRNRECD(RF)                                        
         MVC   TLRECACT,SRECACT                                                 
         GOTO1 ATSARIO,TSAADD                                                   
*                                                                               
ADDXIT   MVC   IODAOVER,BCJOBDA    GET JOB RECORD                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TLXSTAT,TLXSFEEA                                                 
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,0,LSPRATA                        
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AI$RECAD)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,FEECRACH         POSITION CURSOR TO CREDIT ACCOUNT            
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE THE ITEM CURRENTLY DISPLAYED                                 *         
***********************************************************************         
         SPACE 1                                                                
CHA10    MVC   IODAOVER,SFEDA      SET RECORD DISK ADDRESS                      
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3                                                          
         USING TRNRECD,R4                                                       
         CLI   TRNRFST,TRNELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TRNRFST+(TRNTYPE-TRNELD),169                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    FIND1,FF-(FIND1DEL)                                              
         TM    TRNRSTAT,TRNSDELT   TEST DELETED (USER PF'D TO INT/LIST)         
         BZ    *+12                                                             
         NI    TRNRSTAT,FF-TRNSDELT                                             
         OI    FIND1,FIND1DEL      SET FLAG TO UNDELETE DIRECTORY               
*                                                                               
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   CHA12                                                            
         GOTO1 VTOBACCO,BOPARM,('TOBAACVS',TOBCUR),AIO3,ACOM,0,0                
         OI    LSINDS2,LSTOBACC                                                 
*                                                                               
         USING TRNELD,R3                                                        
CHA12    XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         SR    RE,RE                                                            
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
         ICM   RE,1,SFENARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SFENAR                                                
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ENSURE THIS IS FIRST ELEMENT                 
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
*                                                                               
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FIND1,FIND1DEL      TEST RECORD HAS BEEN UNDELETED               
         BZ    CHA20                                                            
D        USING TRNRECD,IOKEY                                                    
         MVC   D.TRNKEY,TRNKEY     READ AND UNDELETE DIRECTORY                  
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BNE   *+6                                                              
         DC    H'0'                DIRECTORY MUST BE DELETED                    
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                DIE IF ANY OTHER ERROR                       
         NI    D.TRNKSTAT,FF-TRNSDELT                                           
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  D                                                                
*                                                                               
CHA20    MVC   IODAOVER,BCJOBDA    GET JOB RECORD                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TLXSTAT,TLXSFEEA                                                 
         LA    RF,LSPRATAS                                                      
         TM    FIND1,FIND1DEL                                                   
         BZ    *+6                                                              
         XR    RF,RF                                                            
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,(RF),LSPRATA                     
*                                                                               
         TM    FIND1,FIND1DEL      TEST RECORD HAS BEEN UNDELETED               
         BZ    CHA30                                                            
         L     R3,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING ASTELD,R3                                                        
         XR    RF,RF                                                            
CHA22    CLI   ASTEL,0                                                          
         BE    CHA30                                                            
         CLI   ASTEL,ASTELQ                                                     
         BE    CHA24                                                            
         IC    RF,1(R3)                                                         
         BXH   R3,RF,CHA22                                                      
*                                                                               
CHA24    ICM   RE,7,ASTDRAFT       INCREASE JOB DRAFTS COUNT                    
         LA    RE,1(RE)            (USUALLY DONE BY ADDTRN)                     
         STCM  RE,7,ASTDRAFT                                                    
         DROP  R3                                                               
*                                                                               
CHA30    GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$RECCH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,FEECRACH         POSITION CURSOR TO CREDIT ACCOUNT            
         TM    FEECRACH+(FVATRB-FVIHDR),FVAPROT                                 
         BNO   *+8                                                              
         LA    R1,FEECMBLH         POSITION CURSOR TO COMMISSIONABLE            
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTION PTAEL AND DISPLAY AMOUNTS                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
         USING TRNELD,R3                                                        
UPDTRN   NTR1  ,                                                                
         LA    R3,TRNRFST          R3=A(TRNEL)                                  
         CP    SFEHRS,MAXHOURS     TEST HOURS INSIDE LIMITS                     
         BH    *+14                                                             
         CP    SFEHRS,MINHOURS                                                  
         BNL   UPDT02                                                           
         LA    RE,FEEHRSH                                                       
         STCM  RE,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AE$IVHRS)                                           
         B     EXITN                                                            
*                                                                               
UPDT02   GOTO1 AGETOPT,BODMCB,AIO3                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPDT04                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPDT04                                                           
         LA    R0,CSEXCVAL                                                      
UPDT04   GOTO1 VPRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         LA    RE,LSPRATAS  (CALLING APORATA COULD MESS THIS UP)                
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   TRNAMNT,SFENET                                                   
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV   TEST CONVERTED FOR 2ND CURRENCY              
         BZ    UPDT20                                                           
         MVC   BOWORK1(L'TOBCUR),TOBCUR                                         
         TM    LSINDS2,LSTOBACC                                                 
         BZ    *+16                                                             
         MVC   BOWORK1(L'TOBCURO),TOBCURO                                       
         MVC   BOWORK1+L'TOBCURO(L'TOBCURI),TOBCURI                             
         GOTO1 VCASHVAL,BOPARM,(X'80',SFENET),(X'28',0),BOWORK1                 
         ZAP   BODUB1,12(8,R1)                                                  
         LA    R2,TRNELD                                                        
         USING OCAELD,R2                                                        
         XR    RF,RF                                                            
UPDT06   CLI   OCAEL,0                                                          
         BE    UPDT12                                                           
         CLI   OCAEL,OCAELQ                                                     
         BE    *+12                                                             
         IC    RF,OCALN                                                         
         BXH   R2,RF,UPDT06                                                     
*                                                                               
         CLI   OCANUM,0            OCAELD ON RECORD - TEST OKAY                 
         BE    UPDT10                                                           
         CLI   OCANUM,OCAIDSEC                                                  
         BE    UPDT10                                                           
         CLI   OCANTYPE,QTRNAMNT                                                
         BNE   UPDT10                                                           
         ZAP   OCANCASH,BODUB1                                                  
         TM    LSINDS2,LSTOBACC    ENSURE THINGS AS THEY SHOULD BE              
         BZ    UPDT08                                                           
         TM    OCAINDS,OCAIDSEC                                                 
         BO    UPDT20                                                           
         DC    H'0'                                                             
UPDT08   TM    OCAINDS,OCAIDSEC                                                 
         BZ    UPDT20                                                           
         DC    H'0'                                                             
UPDT10   GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('OCAELQ',TRNRECD),0                 
*                                                                               
         PUSH  USING                                                            
         USING OCAELD,BOELEM                                                    
UPDT12   MVI   OCAEL,OCAELQ                                                     
         MVI   OCALN,OCALN1Q+OCANTRYL                                           
         MVI   OCAINDS,1                                                        
         TM    LSINDS2,LSTOBACC                                                 
         BZ    *+8                                                              
         OI    OCAINDS,OCAIDSEC                                                 
         MVI   OCANTYPE,QTRNAMNT                                                
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH,BODUB1                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO3,OCAELD                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*&&                                                                             
UPDT20   OI    TRNSTAT,TRNSNOCM    ASSUME NON-COMMISSIONABLE                    
         TM    SFEIND1,SFEICMBL                                                 
         BZ    *+8                                                              
         NI    TRNSTAT,FF-(TRNSNOCM)                                            
         LA    RF,TRNELD                                                        
         USING FFTELD,RF                                                        
         SR    R0,R0                                                            
UPDT22   IC    R0,FFTLN                                                         
         AR    RF,R0                                                            
         CLI   FFTEL,0                                                          
         BE    UPDT24                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   UPDT22                                                           
         CLI   FFTTYPE,FFTTOFFC    TEST OFFICE ELEMENT FOUND/REQUIRED           
         BNE   UPDT22                                                           
         MVC   FFTDATA(L'SFEFOF),SFEFOF                                         
         CLC   FFTDATA(L'SFEFOF),BCSPACES                                       
         BH    UPDT26                                                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',TRNRECD),         X        
               (L'FFTTYPE,=AL1(FFTTOFFC))                                       
         B     UPDT26                                                           
*                                                                               
UPDT24   CLC   SFEFOF,BCSPACES     TEST ELEMENT REQUIRED                        
         BNH   UPDT26                                                           
         LA    RF,BOELEM                                                        
         XC    FFTEL(FFTLN1Q),FFTEL                                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'SFEFOF                                 
         MVI   FFTTYPE,FFTTOFFC                                                 
         MVI   FFTDLEN,L'SFEFOF                                                 
         MVC   FFTDATA(L'SFEFOF),SFEFOF                                         
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDT26   DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TPRELQ',TRNRECD),0,0,0             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TPRELD,RF                                                        
*        CLC   SFEPLAMT,=P'0'                                                   
         OC    SFEPLAMT,SFEPLAMT                                                
         BZ    UPDT30                                                           
         LA    RF,BOELEM                                                        
         XC    TPREL(TPRLN2Q),TPREL                                             
         MVI   TPREL,TPRELQ        PRICE-LIST ELEMENT                           
         MVC   TPRCOD,SFEPLNO                                                   
         ZAP   TPRNUMP,SFEPLAMT                                                 
         MVI   TPRLN,TPRLN2Q                                                    
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    UPDT30                                                           
         DC    H'0'                                                             
*                                                                               
UPDT30   LA    RF,TRNELD                                                        
         USING SPAELD,RF                                                        
         SR    R0,R0                                                            
UPDT32   IC    R0,SPALN                                                         
         AR    RF,R0                                                            
         CLI   SPAEL,0                                                          
         BE    UPDT42                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   UPDT32                                                           
         CLI   SPATYPE,SPATPERS    TEST PERSON FOUND/REQUIRED                   
         BNE   UPDT32                                                           
         MVC   SPAAULA,SFEPER      REFRESH PERSON                               
         CLC   SPAAULA,BCSPACES    TEST PERSON STILL PRESENT                    
         BNH   UPDT40                                                           
         MVI   BOBYTE1,0           CLEAR INDICATOR                              
UPDT33   LA    RF,TRNELD           REFRESH RATE/HOURS SCIELS                    
         USING SCIELD,RF                                                        
N        USING SCIELD,BOELEM                                                    
UPDT34   IC    R0,SCILN                                                         
         AR    RF,R0                                                            
         CLI   SCIEL,0                                                          
         BE    UPDT38                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   UPDT34                                                           
*&&UK                                                                           
         CLI   SCITYPE,SCITCRAT    TEST RATE ELEMENT                            
         BNE   UPDT36                                                           
         TM    BOBYTE1,SCIRATOK                                                 
         BO    UPDT34                                                           
         OI    BOBYTE1,SCIRATOK                                                 
         MVC   BOELEM(SCILN2Q),0(RF)                                            
         ZAP   N.SCIAMNT,SFERAT      REFRESH THE RATE                           
         GOTO1 ATOBCHA,BOPARM,TRNRECD,(RF),N.SCIELD                             
         B     UPDT33                                                           
*                                                                               
UPDT36   CLI   SCITYPE,SCITSJHR                                                 
         BNE   UPDT34                                                           
         TM    BOBYTE1,SCIHRSOK                                                 
         BO    UPDT34                                                           
         OI    BOBYTE1,SCIHRSOK                                                 
         MVC   BOELEM(SCILN2Q),0(RF)                                            
         ZAP   N.SCIAMNT,SFEHRS      REFRESH THE HOURS                          
         GOTO1 ATOBCHA,BOPARM,TRNRECD,(RF),N.SCIELD                             
*&&                                                                             
UPDT38   TM    BOBYTE1,SCIRATOK+SCIHRSOK                                        
         BO    *+6                                                              
         DC    H'0'                MISSING RATE AND/OR HOURS SCIEL              
         B     UPDT50                                                           
         DROP  N                                                                
*                                                                               
UPDT40   GOTO1 CLRELS,BOPARM,TRNRECD                                            
         B     UPDT50                                                           
*                                                                               
UPDT42   GOTO1 CLRELS,BOPARM,TRNRECD                                            
         CLC   SFEPER,BCSPACES     TEST ELEMENT REQUIRED                        
         BNH   UPDT50                                                           
         USING SPAELD,RF                                                        
         LA    RF,BOELEM                                                        
         CLI   P#PERSJC,C'Y'       TEST USING PERSON AS SJ CONTRA A/C           
         BE    UPDT44                                                           
         XC    SPAEL(SPALNQ),SPAEL                                              
         MVI   SPAEL,X'45'         ENSURE ELEMENT GOES AFTER TRNEL              
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATPERS                                                 
         MVC   SPAAULA,SFEPER                                                   
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,16(R1)           RF=A(SPAEL) AFTER HELLO                      
         MVI   SPAEL,SPAELQ        NOW SET CORRECT ELEMENT CODE                 
         B     UPDT46                                                           
*                                                                               
         USING APEELD,RF           PERSON IS CONTRA A/C, SAVE SK/SI A/C         
UPDT44   XC    APEEL(APELN1Q+APELN2Q+L'APENACT),APEEL                           
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q+APELN2Q+L'APENACT                                  
         MVI   APENUM,1            ONE ACCOUNT                                  
         MVI   APENLEN,APELN2Q+L'APENACT                                        
         MVI   APENSTAT,0                                                       
         MVC   APENACT,SFECRAC                                                  
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
UPDT46   DS    0H                                                               
*&&UK                                                                           
         USING SCIELD,RF                                                        
         LA    RF,BOELEM                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCRAT                                                 
         ZAP   SCIAMNT,SFERAT                                                   
         GOTO1 ATOBCHA,BODMCB,TRNRECD,0,BOELEM                                  
         LA    RF,BOELEM                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJHR                                                 
         ZAP   SCIAMNT,SFEHRS                                                   
         GOTO1 ATOBCHA,BODMCB,TRNRECD,0,BOELEM                                  
         DROP  RF                                                               
*&&                                                                             
UPDT50   NI    TRNRSTA2,FF-TRNSBILP  NO BILL ACTIVITY PENDING                   
         USING TRXELD,R1                                                        
         LA    R1,TRNRFST                                                       
         SR    R0,R0                                                            
UPDT52   IC    R0,TRXLN                                                         
         AR    R1,R0                                                            
         CLI   TRXEL,0                                                          
         BE    UPDT54                                                           
         CLI   TRXEL,TRXELQ                                                     
         BNE   UPDT52                                                           
         NI    TRXSTA2,FF-TRNSBILP                                              
*                                                                               
UPDT54   CLI   SACT,ACTJLI         TEST IF CAME FROM JOB LIST                   
         BNE   UPDT55                                                           
         XC    FIOKEY,FIOKEY       SET UP TSAR RECORD FOR ALLTRN                
         L     RE,AIO3                                                          
F        USING TRNRECD,FIOKEY                                                   
         MVC   F.TRNKEY,0(RE)                                                   
         MVC   F.TRNKSTA,TRNRSTA-TRNKEY(RE)                                     
         MVC   F.TRNKDA,SFEDA                                                   
         DROP  F                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',FIOKEY),AIO3,LSPRATA                        
*                                                                               
UPDT55   ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         NI    TLXSTAT,FF-TLXSRVAL                                              
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         TM    SFEIND1,SFEIALOC    TEST ALLOCATED                               
         BNO   UPDT60                                                           
         ZAP   BODUB1,SFENET                                                    
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         BE    *+16                                                             
         LA    RE,FEEALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
UPDT60   TM    SFEIND1,SFEIALOC    TEST ALLOCATED                               
         BZ    UPDT62                                                           
         TM    SFEIND1,SFEICMBL    TEST COMMISSIONABLE                          
         BZ    UPDT62                                                           
         L     RF,AGOPBLK          CALCULATE COMMISSION                         
         ZAP   BOPL81(16),SFENET                                                
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   SFECOM,BOPL81(16)                                                
         ZAP   BODUB1,SFECOM                                                    
         B     *+10                                                             
UPDT62   ZAP   BODUB1,BCPZERO                                                   
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               0,(1,BODUB1)                                                     
         BE    UPDT70                                                           
         LA    RE,FEEALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
UPDT70   ZAP   BODUB1,SFENET                                                    
         CURED BODUB1,(14,FEEALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEALNTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPDT72                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPDT72                                                           
         LA    R0,CSEXCVAL                                                      
UPDT72   GOTO1 APRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
*                                                                               
         TM    LSINDS2,LSTOBACC                                                 
         BZ    UPDTRNX                                                          
         GOTO1 VTOBACCO,BODMCB,('TOBAACVB',TOBCUR),AIO3,ACOM,0,0                
         NI    LSINDS2,FF-LSTOBACC                                              
*                                                                               
UPDTRNX  B     EXITY                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* RUUTINE TO CLEAR RATE/TIME ELEMENTS ETC FROM RECORD                 *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
CLRELS   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATPERS))                                       
         GOTO1 (RF),(R1),(C'D',ACCMST),('APEELQ',TRNRECD),0                     
*                                                                               
CELS01   XR    RF,RF                                                            
         LA    R3,TRNRFST                                                       
         USING SCIELD,R3                                                        
CELS02   CLI   SCIEL,0                                                          
         BE    CLRELSX                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   CELS08                                                           
*&&UK                                                                           
         CLI   SCITYPE,SCITSJHR                                                 
         BE    *+12                                                             
         CLI   SCITYPE,SCITCRAT                                                 
         BNE   CELS08                                                           
         GOTO1 ATOBCHA,BOPARM,TRNRECD,SCIELD,0                                  
         B     CELS01                                                           
*&&                                                                             
CELS08   IC    RF,SCILN                                                         
         BXH   R3,RF,CELS02                                                     
*                                                                               
CLRELSX  B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL/CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
OVLDGS   DS    0X                  OVERRIDE CREDIT ACCOUNT LEDGERS              
         DC    C'SK'                                                            
OVLDGSL  EQU   *-OVLDGS                                                         
*                                  FURTHER OVERRIDE LEDGERS HERE                
         DC    C'SI'                                                            
OVLDGSN  EQU   (*-OVLDGS)/OVLDGSL                                               
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
FF       EQU   X'FF'                                                            
MAXHOURS DC    PL6'32700'                                                       
MINHOURS DC    PL6'-32700'                                                      
*                                                                               
SCIRATOK EQU   X'80'               RATE SCIEL FOUND AND REFRESHED               
SCIHRSOK EQU   X'40'               HOURS SCIEL FOUND AND REFRESHED              
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
FWORKD   DSECT                                                                  
FLST     DS    XL(L'TLST)                                                       
FSLSTCUR DS    XL(LSTTABL)                                                      
IKEYINP  DS    CL1                                                              
FIOKEY   DS    XL(L'IOKEY)                                                      
FIND1    DS    XL1                                                              
FIND1DEL EQU   X'80'                                                            
FDEFCRUL DS    CL2                 DEFAULT CREDIT ACCOUNT LEDGER                
FWORKL   EQU   *-FWORKD                                                         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORKB                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF0BD                                                      
         ORG   OSVALS                                                           
SFESAV   DS    0XL(SFESAVEL)                                                    
SFESAVE  DS    0X                                                               
SFECRAC  DS    CL14                CREDIT ACCOUNT CODE                          
SFECRAN  DS    CL36                CREDIT ACCOUNT NAME                          
SFEWC    DS    CL2                 WORKCODE                                     
SFEREF   DS    CL6                 REFERENCE                                    
SFEDATE  DS    PL3                 DATE                                         
SFENET   DS    PL8                 NET ALLOCATION                               
SFECOM   DS    PL8                 COMM ALLOCATION                              
PRLUSED  DS    CL1                 IF PRICE-LIST IS USED X'FF'                  
SFEPLNO  DS    CL4                 PRICELIST NUMBER                             
SFEPLAMT DS    PL6                 MULTIPLICATOR 2DP * AMNT(PRICELIST)          
SFENARL  DS    CL1                                                              
SFENAR   DS    CL(L'FEENAR1+L'FEENAR2)                                          
SFEDA    DS    CL4                 LAST INSERTED TRANSACTION D/A                
SFEFOF   DS    XL2                 FINANCIAL OFFICE                             
SFEPER   DS    CL14                PERSON ACCOUNT CODE (IN 1P)                  
SFERAT   DS    PL8                 RATE                                         
SFEHRS   DS    PL8                 HOURS                                        
SFEIND1  DS    XL1                 INDICATOR - 1                                
SFEICMBL EQU   X'80'               ITEM IS COMMISSIONABLE                       
SFEIALOC EQU   X'40'               ITEM IS ALLOCATED                            
SFEIORCR EQU   X'20'               CREDIT ACCOUNT LEDGER OVERRIDDEN             
SCRNCRAC DS    XL(L'FEECRAC)       SCREEN CREDIT ACCOUNT CODE                   
SFESAVEL EQU   *-SFESAVE                                                        
SRECACT  DS    0XL2                PREVIOS RECORD ACTION                        
SREC     DS    XL1                                                              
SACT     DS    XL1                                                              
SMODE    DS    XL1                 MODE                                         
SMODENEW EQU   C'N'                ENTERED TO ADD NEW RECORD                    
SMODESEL EQU   C'S'                ENTERED TO CHANGE SELECTED RECORD            
REFE     DS    PL16                                                             
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071ACCLB17B  12/22/99'                                      
         END                                                                    
