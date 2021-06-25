*          DATA SET ACCLB17    AT LEVEL 035 AS OF 08/16/00                      
*PHASE T62117A                                                                  
CLB17    TITLE '- BILL PROGRAM - FEE ADJUSTMENT'                                
CLB17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB17**,R8,R7,R6,CLEAR=YES,RR=RE                              
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
         SR    RE,RE                                                            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   SRECACT,0(RE)       SET PREVIOUS RECORD/ACTION                   
         SR    RF,RF                                                            
         ICM   RF,3,CSSELCUR       SET LAST SUBACTION                           
         A     RF,AOVERSEL                                                      
         MVI   SSUBACT,SSELQ                                                    
         CLC   =Y(UC@SEL-TWAD),0(RF)                                            
         BE    INI02                                                            
         MVI   SSUBACT,SADDQ                                                    
INI02    LA    R2,FWORKD                                                        
         LA    R3,FWORKL                                                        
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
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
         CLI   SACT,ACTFLI         TET IF PREVIOUS ACTION WAS FEELIST           
         BNE   INI40                                                            
         CLI   SSUBACT,SADDQ       TEST SUBACTION WAS ADD                       
         BE    INI40               YES - JUST LIKE WE CAME FROM SETUP           
         MVC   IODAOVER,TLDA       READ FEEADJ TRANSACTION SELECTED             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  CREDIT ACCOUNT                               
         USING TRNRECD,R4                                                       
         USING TRNELD,TRNRFST                                                   
         L     R4,AIO3                                                          
         MVC   FEECRAC(L'TRNKCACT),TRNKCACT                                     
         CLC   =C'SK',TRNKULC                                                   
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
INI06    IC    R0,PTALN                                                         
         AR    RE,R0                                                            
         CLI   PTAEL,0                                                          
         BE    INI10                                                            
         CLI   PTAEL,PTAELQ                                                     
         BNE   INI06                                                            
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BZ    INI06                                                            
         CLI   PTATYPE,PTATRAL     REGULAR ALLOCATION                           
         BNE   INI06                                                            
         CP    PTANET,BCPZERO      TEST ALLOCATION AMOUNT                       
         BE    INI10                                                            
         MVC   FEEALOC,BCSPACES                                                 
         MVC   FEEALOC(L'BC@YES),BC@YES                                         
*                                  TRANSACTION AMOUNT                           
INI10    CURED TRNAMNT,(14,FEEALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,     X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEALNTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         OI    FEEFOFH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('FFTELQ',TRNRECD),         X        
               (L'FFTTYPE,=AL1(FFTTOFFC))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI12                                                            
         L     RE,BOPARM+12                                                     
         MVC   FEEFOF,FFTDATA-FFTELD(RE)                                        
*                                                                               
INI12    TM    FEEPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    INI18               NO PERSON/RATE/HOURS                         
*                                                                               
         OI    FEEPERH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATPERS))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI14                                                            
         L     RE,BOPARM+12                                                     
         MVC   FEEPER,SPAAACT-SPAELD(RE)                                        
*                                                                               
INI14    OI    FEERATH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITCRAT))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI16                                                            
         L     RE,BOPARM+12                                                     
         ZAP   BODUB1,SCIAMNT-SCIELD(L'SCIAMNT,RE)                              
         CURED BODUB1,(14,FEERAT),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
*                                                                               
INI16    OI    FEEHRSH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITSJHR))                                       
         CLI   BOPARM+12,0                                                      
         BNE   INI18                                                            
         L     RE,BOPARM+12                                                     
         ZAP   BODUB1,SCIAMNT-SCIELD(L'SCIAMNT,RE)                              
         CURED BODUB1,(14,FEEHRS),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
*                                                                               
INI18    MVC   BOWORK1,BCSPACES                                                 
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
         BZ    VAL14                                                            
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
VAL14    MVI   IKEYINP,C'N'        SET NO KEY CHANGE                            
         CLI   SACT,ACTSET         TEST PREVIOUS ACTION WAS SETUP               
         BE    *+12                                                             
         CLI   SSUBACT,SSELQ       TEST SUBACTION WAS SELECT                    
         BE    VAL44               YES - KEY IS PROTECTED                       
*                                                                               
         MVI   FVMINL,1            WORKCODE - REQUIRED INPUT                    
         GOTO1 AFVAL,FEEWCH                                                     
         BE    VAL16                                                            
         CLI   FEECRACH+(FVILEN-FVIHDR),0                                       
         BNE   EXIT                GIVE FEEWC ERROR                             
         CLI   P#DINCAR,C' '       TEST DEFAULT INCOME ACCOUNT RULE             
         BNH   VAL24               LET FVAL GIVE FEECRAC ERROR                  
         CLI   P#DINCAR,C'N'       TEST NO DEFAULT                              
         BE    VAL24               LET FVAL GIVE FEECRAC ERROR                  
         B     EXIT                EXIT CC NEQ                                  
*                                                                               
VAL16    TM    FVIIND,FVIVAL                                                    
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
         CLI   B.ACTKLDG,C'K'      TEST DEFAULT LEDGER                          
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
         MVI   I.ACTKUNT,C'S'      DEFAULT UNIT/LEDGER IS SK                    
         MVI   I.ACTKLDG,C'K'                                                   
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
         TM    ACBSTAT,ACBSABAL                                                 
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
VAL50    OI    SFEIND1,SFEIALOC                                                 
VAL52    MVC   FEEALOC,BCSPACES                                                 
         MVC   FEEALOC(L'BC@YES),0(R2)                                          
         OI    FEEALOCH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         ZAP   SFENET,BCPZERO      NET                                          
         ZAP   SFECOM,BCPZERO      COMMISSION                                   
*                                                                               
         MVI   FVMINL,1            NET IS REQUIRED INPUT                        
         GOTO1 AFVAL,FEEALNTH                                                   
         BNE   EXITN                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
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
         MVC   SFEFOF,BCSPACES                                                  
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
         ZAP   BOPL81(16),SFENET   CALCULATE HOURS=NET/RATE                     
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),SFERAT                                                
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
         BE    ADD10                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SFENAR+L'FEENAR1(0),FVIFLD                                       
         LA    RF,L'FEENAR1+1(RF)                                               
         STC   RF,SFENARL                                                       
         B     ADD10                                                            
         EJECT                                                                  
***********************************************************************         
* ADD DRAFT TRANSACTION RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
ADD10    CLI   IKEYINP,C'Y'        TEST IF RECORD ALREADY THERE                 
         BNE   CHA10               YES - READ AND UPDATE IT                     
         L     R4,AIO3             BUILD RECORD IN IO3                          
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
         ZAP   TRNAMNT,SFENET                                                   
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
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
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
INS10    CLI   SACT,ACTSET         NEVER INSERT IF CAME FROM SETUP              
         BE    ADDXIT                                                           
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
         USING TRNRECD,R4                                                       
CHA10    MVC   IODAOVER,SFEDA      SET RECORD DISK ADDRESS                      
         CLI   SACT,ACTSET         TEST PREVIOUS ACTION WAS SETUP               
         BE    *+10                YES - D/A WAS SAVED                          
         MVC   IODAOVER,TLDA       ELSE USE LIST RECORD D/A                     
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    FIND1,FF-(FIND1DEL)                                              
         L     R4,AIO3                                                          
         TM    TRNRSTAT,TRNSDELT   TEST DELETED (USER PF'D TO INT/LIST)         
         BZ    *+12                                                             
         NI    TRNRSTAT,FF-TRNSDELT                                             
         OI    FIND1,FIND1DEL      SET FLAG TO UNDELETE DIRECTORY               
         USING TRNELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
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
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FIND1,FIND1DEL      TEST RECORD HAS BEEN UNDELETED               
         BZ    CHA12                                                            
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
CHA12    MVC   IODAOVER,BCJOBDA    GET JOB RECORD                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TLXSTAT,TLXSFEEA                                                 
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,LSPRATAS,LSPRATA                 
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
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
UPDTRN   NTR1                                                                   
         CP    SFEHRS,MAXHOURS     TEST HOURS INSIDE LIMITS                     
         BH    *+14                                                             
         CP    SFEHRS,MINHOURS                                                  
         BNL   UPDT01                                                           
         LA    RE,FEEHRSH                                                       
         STCM  RE,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AE$IVHRS)                                           
         B     EXITN                                                            
*                                                                               
UPDT01   LA    R3,TRNRFST          R3=A(TRNEL)                                  
         OI    TRNSTAT,TRNSNOCM    ASSUME NON-COMMISSIONABLE                    
         TM    SFEIND1,SFEICMBL                                                 
         BZ    *+8                                                              
         NI    TRNSTAT,FF-(TRNSNOCM)                                            
         LA    RF,TRNELD                                                        
         USING FFTELD,RF                                                        
         SR    R0,R0                                                            
UPDT02   IC    R0,FFTLN                                                         
         AR    RF,R0                                                            
         CLI   FFTEL,0                                                          
         BE    UPDT04                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   UPDT02                                                           
         CLI   FFTTYPE,FFTTOFFC    TEST OFFICE ELEMENT FOUND/REQUIRED           
         BNE   UPDT02                                                           
         MVC   FFTDATA(L'SFEFOF),SFEFOF                                         
         CLC   FFTDATA(L'SFEFOF),BCSPACES                                       
         BH    UPDT06                                                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',TRNRECD),         X        
               (L'FFTTYPE,=AL1(FFTTOFFC))                                       
         B     UPDT06                                                           
*                                                                               
UPDT04   CLC   SFEFOF,BCSPACES     TEST ELEMENT REQUIRED                        
         BNH   UPDT06                                                           
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
UPDT06   GOTO1 AGETOPT,BODMCB,AIO3                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA                
         LA    RE,LSPRATAS                                                      
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         ZAP   TRNAMNT,SFENET                                                   
*                                                                               
UPDT10   LA    RF,TRNELD                                                        
         USING SPAELD,RF                                                        
         SR    R0,R0                                                            
UPDT12   IC    R0,SPALN                                                         
         AR    RF,R0                                                            
         CLI   SPAEL,0                                                          
         BE    UPDT22                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   UPDT12                                                           
         CLI   SPATYPE,SPATPERS    TEST PERSON FOUND/REQUIRED                   
         BNE   UPDT12                                                           
         MVC   SPAAULA,SFEPER      REFRESH PERSON                               
         CLC   SPAAULA,BCSPACES    TEST PERSON STILL PRESENT                    
         BNH   UPDT20                                                           
         LA    RF,TRNELD           REFRESH RATE/HOURS SCIELS                    
         USING SCIELD,RF                                                        
         MVI   BOBYTE1,0           CLEAR INDICATOR                              
UPDT14   IC    R0,SCILN                                                         
         AR    RF,R0                                                            
         CLI   SCIEL,0                                                          
         BE    UPDT18                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   UPDT14                                                           
         CLI   SCITYPE,SCITCRAT    TEST RATE ELEMENT                            
         BNE   UPDT16                                                           
         ZAP   SCIAMNT,SFERAT      REFRESH THE RATE                             
         OI    BOBYTE1,SCIRATOK                                                 
         B     UPDT14                                                           
UPDT16   CLI   SCITYPE,SCITSJHR                                                 
         BNE   UPDT14                                                           
         ZAP   SCIAMNT,SFEHRS                                                   
         OI    BOBYTE1,SCIHRSOK                                                 
UPDT18   TM    BOBYTE1,SCIRATOK+SCIHRSOK                                        
         BO    UPDT30                                                           
         DC    H'0'                MISSING RATE AND/OR HOURS SCIEL              
*                                                                               
UPDT20   GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATPERS))                                       
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITCRAT))                                       
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('SCIELQ',TRNRECD),         X        
               (L'SCITYPE,=AL1(SCITSJHR))                                       
         B     UPDT30                                                           
*                                                                               
         USING SPAELD,RF                                                        
UPDT22   CLC   SFEPER,BCSPACES     TEST ELEMENT REQUIRED                        
         BNH   UPDT30                                                           
         LA    RF,BOELEM                                                        
         CLI   P#PERSJC,C'Y'       TEST USING PERSON AS SJ CONTRA A/C           
         BE    UPDT24                                                           
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
         B     UPDT26                                                           
*                                                                               
         USING APEELD,RF           PERSON IS CONTRA A/C, SAVE SK/SI A/C         
UPDT24   XC    APEEL(APELN1Q+APELN2Q+L'APENACT),APEEL                           
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
*                                                                               
         USING SCIELD,RF                                                        
UPDT26   LA    RF,BOELEM                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCRAT                                                 
         ZAP   SCIAMNT,SFERAT                                                   
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,BOELEM                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJHR                                                 
         ZAP   SCIAMNT,SFEHRS                                                   
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    UPDT30                                                           
         DC    H'0'                                                             
*                                                                               
UPDT30   NI    TRNRSTA2,FF-TRNSBILP  NO BILL ACTIVITY PENDING                   
         USING TRXELD,R1                                                        
         LA    R1,TRNRFST                                                       
         SR    R0,R0                                                            
UPDT32   IC    R0,TRXLN                                                         
         AR    R1,R0                                                            
         CLI   TRXEL,0                                                          
         BE    UPDT34                                                           
         CLI   TRXEL,TRXELQ                                                     
         BNE   UPDT32                                                           
         NI    TRXSTA2,FF-TRNSBILP                                              
*                                                                               
UPDT34   ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         TM    SFEIND1,SFEIALOC    TEST ALLOCATED                               
         BNO   UPDT36                                                           
         ZAP   BODUB1,SFENET                                                    
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         BE    *+16                                                             
         LA    RE,FEEALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
UPDT36   TM    SFEIND1,SFEIALOC    TEST ALLOCATED                               
         BZ    UPDT38                                                           
         TM    SFEIND1,SFEICMBL    TEST COMMISSIONABLE                          
         BZ    UPDT38                                                           
         L     RF,AGOPBLK          CALCULATE COMMISSION                         
         ZAP   BOPL81(16),SFENET                                                
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   SFECOM,BOPL81(16)                                                
         ZAP   BODUB1,SFECOM                                                    
         B     *+10                                                             
UPDT38   ZAP   BODUB1,BCPZERO                                                   
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               0,(1,BODUB1)                                                     
         BE    UPDT60                                                           
         LA    RE,FEEALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
UPDT60   CURED SFENET,(14,FEEALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    FEEALNTH+(FVOIND-FVIHDR),FVOXMT                                  
         B     EXITY                                                            
*                                                                               
         DROP  R3,R4                                                            
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
         DC    C'SI'                                                            
OVLDGSL  EQU   *-OVLDGS                                                         
*                                  FURTHER OVERRIDE LEDGERS HERE                
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
FWORKL   EQU   *-FWORKD                                                         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORKS                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF0D                                                       
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
SRECACT  DS    0XL2                PREVIOUS RECORD ACTION                       
SREC     DS    XL1                                                              
SACT     DS    XL1                                                              
SSUBACT  DS    XL1                 LAST SUBACTION (IF SACT=ACTFLI)              
SADDQ    EQU   C'A'                ADD                                          
SSELQ    EQU   C'S'                SELECT                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACCLB17   08/16/00'                                      
         END                                                                    
